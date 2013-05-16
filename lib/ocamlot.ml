open Sexplib.Std

module Uri = struct
  include Uri
  let t_of_sexp sexp =
    of_string (string_of_sexp sexp)
  let sexp_of_t uri = sexp_of_string (to_string uri)
end

module Body    = Cohttp_lwt_unix.Body
module Request = Cohttp_lwt_unix.Request
module Server  = Cohttp_lwt_unix.Server
module Cookie  = Cohttp.Cookie
module Header  = Cohttp.Header

type engagement = Time.t
type history = engagement list

type worker_id = int

type worker_message =
  | Refuse of string
  | Accept
  | Check_in
  | Fail of string
  | Complete of Result.t
with sexp

type worker_action =
  | Hibernate
type worker = {
  engagement : engagement;
  cookie : string;
  worker_id : worker_id;
  worker_host : Host.t;
  last_request : Time.t;
}
type worker_resource = (worker, worker_action) Resource.t

type event =
  | Advertized of Host.t
  | Refused of worker_id * string
  | Started of worker_id
  | Checked_in of worker_id
  | Failed of worker_id * string
  | Timed_out of worker_id * Time.duration
  | Cancelled of string
  | Completed of worker_id * Result.t

type log = (Time.t * event) list
type job = Opam of Opam_task.t with sexp
type task = {
  log : log;
  host : Host.t;
  job : job;
}
type task_offer = Uri.t * job with sexp

type task_action =
  | Worker of worker_id * worker_message
  | Time_out of worker_id * Time.duration
  | Cancel of string
type task_resource = (task * requeue, task_action) Resource.t
and requeue = Requeue of (task_resource -> unit)

type goal_action =
  | New_task of task_resource
  | Update_task of Uri.t * task_action
  | Update_subgoal of Uri.t * goal_action

type goal = {
  slug : string;
  title : string;
  descr : string;
  subgoals : (goal, goal_action) Resource.index;
  completed : task Resource.archive;
  tasks : (task * requeue, task_action) Resource.index;
  queue : task_resource Lwt_stream.t;
  enqueue : task_resource -> unit;
}
and goal_resource = (goal, goal_action) Resource.t

type t_action =
  | New_worker of worker_resource
  | Update_worker of Uri.t * worker_action
  | New_goal of goal_resource
  | Update_goal of Uri.t * goal_action
type t = {
  goals : (goal, goal_action) Resource.index;
  outstanding : task_resource Lwt_stream.t;
  workers : (worker, worker_action) Resource.index;
  idle : task_resource Lwt.u Lwt_sequence.t;
}
type t_resource = (t, t_action) Resource.t

let engagement = Time.now ()
let sessions = Hashtbl.create 10 (* cookie -> worker URI *)
let title = "ocamlot"

let mint_id mint () = let id = !mint in incr mint; id
let worker_mint = ref 0
let new_worker_id = mint_id worker_mint

let string_of_job = function
  | Opam opam_task -> "opam => "^(Opam_task.to_string opam_task)

(* TODO: DO *)
let html_escape s = s

let string_of_event = Printf.(function
  | Advertized host -> sprintf "advertized for %s host" (Host.to_string host)
  | Refused (worker_id, reason) -> sprintf
      "refused by worker %d because '%s'" worker_id (html_escape reason)
  | Started worker_id -> sprintf "started by worker %d" worker_id
  | Checked_in worker_id -> sprintf "checked-in by worker %d" worker_id
  | Failed (worker_id, reason) -> sprintf
      "failed by worker %d because '%s'" worker_id (html_escape reason)
  | Timed_out (worker_id, duration) -> sprintf
      "timed-out worker %d after %s" worker_id (Time.duration_to_string duration)
  | Cancelled reason -> sprintf
      "cancelled because '%s'" reason
  | Completed (worker_id, result) -> sprintf
      "completed by worker %d with result: %s" worker_id
    Result.(string_of_status (get_status result))
)

(* TODO: better search, yes it's linear right now *)
let find_task t host =
  let rec pull rql = match Lwt_stream.get_available_up_to 1 t.outstanding with
    | [] -> (List.rev rql), None
    | tr::_ ->
        let (task, Requeue rq) = Resource.content tr in
        if task.host = host (* TODO: 1st class pattern match *)
        then (List.rev rql), Some tr
        else pull ((tr, rq)::rql)
  in
  let rql, task_opt = pull [] in
  List.iter (fun (tr, rq) -> rq tr) rql;
  task_opt

(* TODO: DO *)
let update_worker worker action = worker
let worker_renderer =
  let render_html event =
    let page worker = Printf.sprintf
      "<html><head><title>Knight %d : %s</title></head><body><h1>Knight %d</h1><p>Last request: %s</p><p>%s</p></body></html>"
      worker.worker_id title worker.worker_id
      (Time.to_string worker.last_request)
      (Host.to_string worker.worker_host)
    in Resource.(match event with
      | Create (worker, r) -> page worker
      | Update (worker_action, r) -> page (content r)
    ) in
  let r = Hashtbl.create 1 in
  Hashtbl.replace r (`text `html) render_html;
  r

let lift_worker_to_t = Resource.(function
  | Create (_, r) -> New_worker r
  | Update (d, r) -> Update_worker (uri r, d)
)

let new_worker t_resource worker_host =
  let t = Resource.content t_resource in
  let cookie = Util.(hex_str_of_string (randomish_string 20)) in
  let worker_id = new_worker_id () in
  let worker = {
    engagement; cookie; worker_id; worker_host;
    last_request = Time.now ();
  } in
  let worker_resource = Resource.index t.workers
    worker update_worker worker_renderer
  in
  let () = Hashtbl.replace sessions cookie (Resource.uri worker_resource) in
  Resource.bubble worker_resource t_resource lift_worker_to_t;
  worker_resource

let update_task (task,rq) action =
  let now = Time.now () in
  match action with
    | Worker (wid, Refuse reason) ->
        ({ task with log=(now, Refused (wid, reason))::task.log },rq)
    | Worker (wid, Accept) ->
        ({ task with log=(now, Started wid)::task.log },rq)
    | Worker (wid, Check_in) ->
        ({ task with log=(now, Checked_in wid)::task.log },rq)
    | Worker (wid, Fail reason) ->
        ({ task with log=(now, Failed (wid, reason))::task.log },rq)
    | Worker (wid, Complete result) ->
        ({ task with log=(now, Completed (wid, result))::task.log },rq)
    | Time_out (wid, duration) ->
        ({ task with log=(now, Timed_out (wid, duration))::task.log },rq)
    | Cancel reason ->
        ({ task with log=(now, Cancelled reason)::task.log },rq)
let task_renderer =
  let render_html event =
    let log_event (time, event) =
      Printf.sprintf "<li>%s at %s</li>"
        (string_of_event event) (Time.to_string time)
    in
    let page { log; job } =
      let job_descr = string_of_job job in
      let (time, event) = List.hd log in
      Printf.sprintf
        "<html><head><title>%s : %s</title></head><body><h1>%s</h1><div id='update'>%s</div><div id='status'>%s</div>%s<ul>%s</ul></body></html>"
        job_descr title job_descr
        (Time.to_string time)
        (string_of_event event)
        (match event with
          | Completed (wid, result) ->
              Printf.sprintf "<div id='result'>%s</div>"
                Result.(to_html result)
          | _ -> "")
        (String.concat "\n" (List.map log_event log))
    in Resource.(match event with
      | Create ((task, _), r) -> page task
      | Update (_, r) -> page (fst (content r))
    ) in
  let r = Hashtbl.create 1 in
  Hashtbl.replace r (`text `html) render_html;
  r

let host_of_job = function
  | Opam opam_task -> Opam_task.(opam_task.target.host)

let lift_task_to_goal = Resource.(function
  | Create (_, r) -> New_task r
  | Update (d, r) -> Update_task (uri r, d)
)

let queue_job goal_resource job =
  let goal = Resource.content goal_resource in
  let host = host_of_job job in
  let task = {
    log = [Time.now (), Advertized host];
    host;
    job;
  } in
  let task_resource = Resource.index goal.tasks
    (task, Requeue goal.enqueue) update_task task_renderer
  in
  Resource.bubble task_resource goal_resource lift_task_to_goal;
  task_resource

let update_goal goal action = (match action with
  | New_task tr -> goal.enqueue tr
  | Update_task (uri, Worker (wid, Accept))
  | Update_task (uri, Worker (wid, Check_in)) -> ()
  | Update_task (uri, Worker (wid, (Refuse reason))) ->
      let tr = Resource.remove uri goal.tasks in
      goal.enqueue tr
  | Update_task (uri, Worker (wid, (Fail reason))) ->
      let tr = Resource.remove uri goal.tasks in
      goal.enqueue tr
  | Update_task (uri, Worker (wid, (Complete result))) ->
      let tr = Resource.remove uri goal.tasks in
      Resource.archive goal.completed fst tr
  | Update_task (uri, Time_out (wid, duration)) ->
      let tr = Resource.remove uri goal.tasks in
      goal.enqueue tr
  | Update_task (uri, Cancel reason) ->
      let tr = Resource.remove uri goal.tasks in
      Resource.archive goal.completed fst tr
  | Update_subgoal (uri, subgoal_action) -> ()
); goal

let goal_renderer =
  let render_html event =
    let task tr =
      let task = fst (Resource.content tr) in
      let tm, status = List.hd task.log in
      Printf.sprintf "<li>%s (%s) : %s</li>"
        (string_of_event status) (Time.to_string tm) (string_of_job task.job)
    in
    let page goal = Printf.sprintf
      "<html><head><title>%s : %s</title></head><body><h1>%s</h1><p>%s</p><ul>%s</ul></body></html>"
      goal.title title goal.title goal.descr
      (String.concat "\n" (List.rev_map task (Resource.to_list goal.tasks)))
    in Resource.(match event with
      | Create (goal, r) -> page goal
      | Update (goal_action, r) -> page (content r)
    ) in
  let r = Hashtbl.create 1 in
  Hashtbl.replace r (`text `html) render_html;
  r

let lift_goal_to_t = Resource.(function
  | Create (_, r) -> New_goal r
  | Update (d, r) -> Update_goal (uri r, d)
)

let new_goal t_resource goal =
  let t = Resource.content t_resource in
  let goal_resource = Resource.index t.goals goal update_goal goal_renderer in
  Resource.bubble goal_resource t_resource lift_goal_to_t;
  goal_resource

let update_index index action = (match action with
  | New_worker _ -> index
  | Update_worker (_,_) -> index (* TODO: retire *)
  | New_goal gr ->
      let goal = Resource.content gr in
      { index with
        outstanding = Lwt_stream.choose [index.outstanding; goal.queue];
  }
  | Update_goal (_, _) -> index (* TODO: aggregate? *)
)

let index_renderer =
  let render_html event =
    let goal gr =
      let goal = Resource.content gr in
      Printf.sprintf
        "<li><a href='%s'>%s</a></li>"
        (Uri.to_string (Resource.uri gr))
        goal.title
    in
    let worker wr =
      let worker = Resource.content wr in
      Printf.sprintf
        "<li><a href='%s'>#%d %s</a></li>"
        (Uri.to_string (Resource.uri wr))
        worker.worker_id
        (Host.to_string worker.worker_host)
    in
    let page t = Printf.sprintf
      "<html><head><title>%s</title></head><body><h1>%s</h1><ul>%s</ul><ul>%s</ul><p>Idle workers: %d</p></body></html>"
      title title
      (String.concat "\n" (List.rev_map goal (Resource.to_list t.goals)))
      (String.concat "\n" (List.rev_map worker (Resource.to_list t.workers)))
      (Lwt_sequence.length t.idle)
    in Resource.(match event with
      | Create (t, r) -> page t
      | Update (_, r) -> page (content r)
    ) in
  let r = Hashtbl.create 1 in
  Hashtbl.replace r (`text `html) render_html;
  r

let make ~base =
  let queue, enqueue = Lwt_stream.create () in
  let slug = "integration" in
  let generate_goal_uri goal =
    Uri.(resolve "" base (of_string goal.slug))
  in
  let generate_subgoal_uri subgoal =
    Uri.(resolve "" base (of_string (slug^"/"^subgoal.slug)))
  in
  let task_mint = ref 0 in
  let new_task_id = mint_id task_mint in
  let generate_task_uri task =
    Uri.(resolve "" base
           (of_string (Printf.sprintf "%s/task/%d" slug (new_task_id ()))))
  in
  let generate_worker_uri worker =
    Uri.(resolve "" base
           (of_string (Printf.sprintf "worker/%d" worker.worker_id)))
  in
  let integration = {
    slug;
    title="GitHub OPAM Repository Integration";
    descr="The GitHub OPAM Repository Integration goal subscribes to GitHub"
    ^" events, scans GitHub, and runs package build tasks.";
    subgoals=Resource.create_index generate_subgoal_uri [];
    completed=Resource.create_archive [];
    tasks=Resource.create_index generate_task_uri [];
    queue;
    enqueue=(fun task_resource -> enqueue (Some task_resource));
  } in
  let goals = Resource.create_index generate_goal_uri [] in
  let t = {
    goals;
    outstanding = integration.queue; (* TODO: choose *)
    workers = Resource.create_index generate_worker_uri [];
    idle = Lwt_sequence.create ();
  } in
  let t_resource = Resource.create base t update_index index_renderer in
  let goal_resource = new_goal t_resource integration in
  t_resource

let worker_listener service_fn ~root ~host ~port t_resource =
  let routes = Re.(seq [str root; eos]) in
  let worker_id_cookie = "worker_id" in
  let offer_task ~headers task_resource =
    let (task,_) = Resource.content task_resource in
    let uri = Resource.uri task_resource in
    let body = string_of_sexp (sexp_of_task_offer (uri,task.job)) in
    let open Lwt in
    Server.respond_string ~headers ~status:`OK ~body ()
    >>= Http_server.some_response
  in
  let handler conn_id ?body req = Lwt.(
    if Request.params req <> ["queue",[]]
    then return None
    else let req_headers = Request.headers req in
         let cookies = Cookie.Cookie_hdr.extract req_headers in
         let headers = Header.init () in
         let t = Resource.content t_resource in
         (try
            let ident = List.assoc worker_id_cookie cookies in
            let uri = Hashtbl.find sessions ident in
            return (headers, Resource.find t.workers uri)
          with Not_found -> begin
            Body.string_of_body body
            >>= fun body ->
            let host = Host.t_of_sexp (sexp_of_string body) in
            let wr = new_worker t_resource host in
            let worker = Resource.content wr in
            let open Cookie.Set_cookie_hdr in
            let cookie = make (worker_id_cookie,
                               worker.cookie) in
            let k, v = Cookie.Set_cookie_hdr.serialize cookie in
            let headers = Header.add headers k v in
            return (headers, wr)
          end)
       >>= fun (headers, wr) ->
         let host = (Resource.content wr).worker_host in
         match find_task t host with
           | None ->
               let t = Resource.content t_resource in
               add_task_r t.idle
               >>= offer_task ~headers
           | Some task_resource -> offer_task ~headers task_resource
  ) in
  service_fn
    ~routes
    ~handler
    ~startup:[]
