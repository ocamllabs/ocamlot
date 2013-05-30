(*
 * Copyright (c) 2013 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(* queue-centric work dispatch so workers don't have to manage invariants *)
module Fifo = struct
  type 'a t = 'a Lwt_sequence.t (* left=front, right=back *)
  type 'a slot = 'a Lwt_sequence.node

  let create = Lwt_sequence.create
  let take = Lwt_sequence.take_opt_l
  let put = Lwt_sequence.add_r
end
module Event = Lwt_condition

type instruction = Opam of Opam_task.t
type worker_type = {
  os : Host.os;
  arch : Host.arch;
}
type worker_id = int
type task_id = int
type event =
  | Advertized of worker_type
  | Refused of worker_id * string
  | Deferred of string
  | Started of worker_id
  | Checked_in
  | Failed of string
  | Timed_out
  | Cancelled of string
  | Completed of Result.t
type log = (Time.t * event) list
type task = {
  task_id : task_id;
  instruction : instruction;
  log : log;
  timeout : float;
  task_update : task -> event -> task;
  task_completion : (task * Result.t) Event.t;
  task_logging : log Event.t;
  task_queue: queue;
}
and worker = {
  worker_type : worker_type;
  worker_id : worker_id;
  worker_offer : task -> unit;
  (*
  worker_evict : unit;
  worker_cancel : unit;
  *)
  mutable queues : (queue * worker Fifo.slot) list;
}
and command =
  | Post of task
  | Join of worker
  | Leave of worker Fifo.slot
and queue = {
  queue_type : worker_type;
  posted : task Fifo.t;
  running : (task, worker) Hashtbl.t;
  workers : worker Fifo.t;
  commands : command Fifo.t;
  queue_modification : queue Event.t;
}
type t = (worker_type, queue) Hashtbl.t

let offer_timeout_s = 1. (* compare gettimeofday and sleep *)
let exec_timeout_s = 240. (* 4m *)

let make () = Hashtbl.create 4

let mint_id mint = let id = !mint in mint := id + 1; id

let worker_mint = ref 0
let new_worker_id () = mint_id worker_mint

let task_mint = ref 0
let new_task_id () = mint_id task_mint

let worker_type_of_opam_task t =
  {os=Opam_task.(t.target.os); arch=Opam_task.(t.target.arch)}

let worker_type_of_instruction = function
  | Opam opam_task -> worker_type_of_opam_task opam_task

let string_of_worker_type {os; arch} =
  Printf.sprintf "[OS: %s ARCH: %s]"
    (Host.string_of_os os) (Host.string_of_arch arch)

let queue_modified queue =
  Event.broadcast queue.queue_modification queue
(*
let rec monitor_queue q () =
    Lwt.(async (fun () -> catch
      (fun () ->
        match Fifo.get q.commands with
          | Some (Post task) ->

          | Some (Join worker) ->

          | Some (Leave worker_slot) ->
              let worker = Fifo.get worker_slot in
              Fifo.remove worker_slot;
              queue_modified q;
          | None -> fail (Failure "command stream closed")
      )
      (fun exn -> Printf.eprintf
      )
    ));

*)
let queue_of_type queue t =
  try Hashtbl.find queue t
  with Not_found ->
    let q = {queue_type=t;
             posted=Fifo.create ();
             running=Hashtbl.create 10;
             workers=Fifo.create ();
             commands=Fifo.create ();
             queue_modification=Event.create ();
            } in
    Hashtbl.replace queue t q;
(*    Lwt.async (monitor_queue q);*)
    q

let log_task event task =
  let task = {task with log=(Time.now (), event)::task.log} in
  Event.broadcast task.task_logging task.log;
  task
(*
let schedule_work q ?timeout:exec_timeout_s instruction =
  let worker_type = worker_type_of_instruction instruction in
  let task_queue = queue_type worker_type in
  let task_completion = Event.create () in
  let task_logging = Event.create () in
  let task_update task event =

  in
  let task = {
    task_id=new_task_id ();
    instruction;
    log=[];
    timeout;
    task_update;
    task_completion;
    task_logging;
    task_queue;
  } in
  let task = {instruction; status=Advertized} in
  let node = Lwt_sequence.add_r task queue.posted in
  Printf.eprintf "Scheduled \"%s\"\n%!" (string_of_instruction instruction);
  queue_modified queue
*)
let worker_on_task task =
  let queue = task.task_queue in
  Hashtbl.find queue.running task

let start_task task =
  let {worker_id} = worker_on_task task in
  task.task_update task (Started worker_id)

let refuse_task task why =
  let {worker_id} = worker_on_task task in
  task.task_update task (Refused (worker_id, why))

let checkin_task task =
  task.task_update task Checked_in

let fail_task task why =
  task.task_update task (Failed why)

let complete_task task result =
  task.task_update task (Completed result)
(*
let is_task_engaged = function
  | {log=[]
        |(_,   Advertized _
             | Refused (_,_)
             | Deferred _)::_} -> false
  | {log=(_,   Started _
             | Checked_in

             | Failed _
  | Timed_out
  | Completed of result
*)
let defer_task queue task message =
  let task = log_task (Deferred message) task in
  let worker_type = worker_type_of_instruction task.instruction in
  Printf.eprintf "%s %s\n%!"
    (string_of_worker_type worker_type) message;
  ignore (Fifo.put task queue.posted);
  queue_modified queue
(*
let rec supervise worker task stream =

*)(*
let rec offer queue refusals node =
  let task = Lwt_sequence.get node in
  match Lwt_sequence.take_opt_l queue.workers with
    | None ->
        defer_task task "workers unavailable";

    | Some worker ->
        let first_worker = match first_worker with
          | None -> worker.worker_id | Some id -> id
        in
        let stream, push = Lwt_stream.create () in
        worker.worker_notify task push;
        Lwt.(pick [
          Lwt_unix.sleep offer_timeout_s
          >>= begin fun () -> (* TODO: send eviction notice *)
            List.iter (fun (q,node) ->
              Lwt_sequence.remove node;
              queue_modified q;
            ) worker.queues;
            worker.queues <- [];
            Printf.eprintf "Worker %d timed out on %s/%s offer and was evicted\n%!"
              worker.worker_id worker.worker_type.arch worker.worker_type.os;
            return (Some refusals)
          end;
          Lwt_stream.get stream
          >>= begin function
            | None ->
                worker.queues <- List.remove_assoc queue worker.queues;
                (*let worker_node = Lwt_sequence.add_r worker queue.workers in
                worker.queues <- (queue,worker_node)::queues;*)
                return (Some (worker::refusals))
            | Some ({status=Running _ | Completed} as task) ->
                Lwt_sequence.remove node;
                queue_modified queue;
                return None
          end
        ]) >>= function
          | Some refusals -> offer queue refusals node
          | None ->
  *)

let register_worker ~queue ~arch ~os =
  let worker_type = {os; arch} in
  let queues = [queue_of_type queue worker_type] in
  let worker_id = new_worker_id () in
  {worker_type; worker_id; worker_offer=(fun _ -> ()); (* TODO: FIXME *)
   queues=[]; (* TODO: FIXME insert, mutate *)
  }
(*
let run worker = Lwt.(begin
  pick (List.map (fun q -> Lwt_condition.wait q.new_work_event) worker.queues)
  >>= Task.(function
    | {task=OpamBuild {opam_task; opam_repo}; ack} ->
        Lwt_mvar.put ack true
        >>= fun () ->

  )
end)
*)
