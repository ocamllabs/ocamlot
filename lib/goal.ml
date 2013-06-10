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

open Lwt
open Ocamlot

let task_subpath = "task"

let rec update_goal_subgoal goal = function
  | New_task _
  | Update_task (_,_) -> goal
  | New_subgoal _ ->
      { goal with stream=Lwt_stream.choose
          (goal.queue::(List.map (fun g -> (Resource.content g).stream)
                          (Resource.index_to_list goal.subgoals))) }
  | Update_subgoal (_,subgoal_event) ->
      update_goal_subgoal goal subgoal_event

let update_goal ~on_complete goal = function
  | New_task tr -> goal.enqueue tr; goal
  | Update_task (uri, Worker (wid, Accept))
  | Update_task (uri, Worker (wid, Check_in)) -> goal
  | Update_task (uri, Time_out (wid, _))
  | Update_task (uri, Worker (wid, (Refuse _)))
  | Update_task (uri, Worker (wid, (Fail_task _))) ->
      goal.enqueue (Resource.find goal.tasks uri);
      goal
  | Update_task (uri, Worker (_, (Complete _)))
  | Update_task (uri, Cancel _) ->
      let tr = Resource.remove uri goal.tasks in
      Resource.archive goal.completed fst tr;
      async (on_complete tr);
      goal
  | New_subgoal gr -> update_goal_subgoal goal (New_subgoal gr)
  | Update_subgoal (_, subgoal_event) -> update_goal_subgoal goal subgoal_event

module Table = struct
  type 'a t = (string, (string, 'a) Hashtbl.t) Hashtbl.t

  let create rowfn colfn els =
    let t = Hashtbl.create 10 in
    List.iter
      (fun el -> match rowfn el with
        | Some row ->
            let cols =
              try Hashtbl.find t row
              with Not_found -> Hashtbl.create 10
            in
            Hashtbl.replace t row cols;
            Hashtbl.add cols (colfn el) el
        | None -> ()
      ) els;
    t

  let cell_count tbl = Hashtbl.fold
    (fun _ v a ->
      a + (Hashtbl.length v)
    ) tbl 0

  let columns tbl =
    let col_set = Hashtbl.create 10 in
    Hashtbl.iter (fun _ ct ->
      Hashtbl.iter (fun c _ -> Hashtbl.replace col_set c ()) ct
    ) tbl;
    Hashtbl.fold (fun c () l -> c::l) col_set []

  let rows tbl =
    let cols = columns tbl in
    List.sort (fun (x,_) (y,_) -> String.compare x y)
      (Hashtbl.fold (fun r ct l ->
        (r, List.map (Hashtbl.find_all ct) cols)::l
       ) tbl [])
end

let goal_renderer parent_title parent_uri =
  let render_html event =
    let render_tasks_table trl =
      let opam_task_of_tr tr = match Resource.content tr with
        | { job = Opam ot } -> ot
      in
      let target_colfn tr = Opam_task.(match opam_task_of_tr tr with
        | { target } -> string_of_target target) in
      let last_event task = snd (List.hd task.log) in
      let simple_opam_tasks, other_tasks = List.partition (fun tr ->
        match opam_task_of_tr tr with
          | { Opam_task.packages = [ _ ] } -> true
          | _ -> false) trl
      in
      let tbl = Table.create
        Opam_task.(fun tr ->
          match opam_task_of_tr tr with
            | { packages = pkg::_ } -> Some pkg
            | { packages = [] } -> None)
        target_colfn
        simple_opam_tasks
      in
      let done_tbl = Table.create
        (fun tr ->
          let packages = (opam_task_of_tr tr).Opam_task.packages in
          match last_event (Resource.content tr), packages with
            | (Completed (_,_) | Started _ | Checked_in _), pkg::_ -> Some pkg
            | _ -> None)
        target_colfn
        simple_opam_tasks
      in
      let task_cell trl =
        let cell_link tr =
          let task = Resource.content tr in
          let task_state = match last_event task with
            | Completed (_, { Result.status = Result.Passed }) -> "PASSED"
            | Completed (_, { Result.status = Result.Failed }) -> "FAILED"
            | Started _ | Checked_in _ -> "pending"
            | _ -> "queued"
          in
          Printf.sprintf "<a href='%s'>%s</a>"
            (Uri.to_string (Resource.uri tr))
            task_state
        in
        "<td>"^(match trl with
          | [] -> ""
          | [tr] -> cell_link tr
          | trl -> "<ul>"^(List.fold_left (fun s tr ->
            s^"<li>"^(cell_link tr)^"</li>\n"
          ) "" trl)^"</ul>"
        )^"</td>"
      in
      let render_table tbl =
        if Table.cell_count tbl > 0
        then "<table><tr><th></th>"^
          (String.concat "\n"
             (List.map (fun h -> "<th>"^h^"</th>") (Table.columns tbl)))^"</tr>"^
          (String.concat "\n"
             (List.map (fun (rl,r) ->
               "<tr><th>"^rl^"</th>"^(String.concat "\n"
                                        (List.map task_cell r))^"</tr>")
                (Table.rows tbl)))
          ^"</table>"
        else ""
      in
      (render_table done_tbl)
      ^(render_table tbl)
      ^(
        if List.length other_tasks > 0
        then "<ul>"^
          (String.concat "\n"
             (List.map (fun tr ->
               let task = Resource.content tr in
               let tm, status = List.hd task.log in
               Printf.sprintf "<li><a href='%s'>%s (%s) : %s</a></li>"
                 (Uri.to_string (Resource.uri tr))
                 (string_of_event status)
                 (Time.to_string tm)
                 (string_of_job task.job)
              ) other_tasks)
          )^"</ul>"
        else ""
       )
    in
    let render_subgoal gr =
      let goal = Resource.content gr in
      Printf.sprintf "<li><a href='%s'>%s</a></li>"
        (Uri.to_string (Resource.uri gr))
        goal.title
    in
    let page goal = Printf.sprintf
      "<html><head><link rel='stylesheet' type='text/css' href='%s'/><title>%s : %s</title></head><body><h1>%s</h1><p>%s</p>%s<ul>%s</ul><p><a href='%s'>%s</a></body></html>"
      Ocamlot.style
      goal.title title goal.title goal.descr
      (render_tasks_table
         (List.rev_append
            Resource.(archive_to_list
                        (create_archive
                           (List.map (fun r -> (r,fst))
                              (index_to_list goal.tasks))))
            (Resource.archive_to_list goal.completed)))
      (String.concat "\n" (List.rev_map render_subgoal
                             (Resource.index_to_list goal.subgoals)))
      (Uri.to_string parent_uri) parent_title
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

let lift_subgoal_to_goal = Resource.(function
  | Create (_, r) -> New_subgoal r
  | Update (d, r) -> Update_subgoal (uri r, d)
)

let max_task_record_id task_records =
  List.fold_left (fun x (id,_) -> max (int_of_string id) x) 0 task_records

let read_tasks path =
  let ok p = Lwt_unix.(access p [F_OK;R_OK]) in
  let task_path = Filename.concat path task_subpath in
  if Sys.file_exists task_path
  then
    let listing_stream = Lwt_unix.files_of_directory task_path in
    Lwt_stream.fold (fun filename l ->
      if filename <> "." && filename <> ".." then filename::l else l
    ) listing_stream []
     >>= Lwt_list.fold_left_s (fun l tid ->
       let f = Filename.(concat task_path tid) in
       catch (fun () ->
         ok f
         >>= fun () ->
         Lwt_io.(with_file ~mode:input f Lwt_io.read)
         >>= fun buf ->
         let sexp = Sexplib.Sexp.of_string buf in
         return ((tid,task_of_sexp sexp)::l))
         (fun exn -> return l)
     ) []
  else return []

let write_task dir tr =
  let uri = Resource.uri tr in
  let uri_str = Uri.to_string uri in
  let task_id = List.hd (List.rev Re_str.(split (regexp_string "/") uri_str)) in
  let (task,_) = Resource.content tr in
  (match List.hd task.Ocamlot.log with
    | (_,Completed (_,r)) -> return r
    | _ -> fail (Failure "Goal.write_task on uncompleted task")
  )
  >>= fun result ->
  let message = Printf.sprintf "%s %s\n\n%s"
    Result.(string_of_status (get_status result))
    (string_of_job task.job)
    (Uri.path uri) in
  let sexp = sexp_of_task task in
  let buf = Sexplib.Sexp.to_string sexp in
  let filename = Filename.(concat (concat dir task_subpath) task_id) in
  catch (fun () ->
    Lwt_io.(with_file ~mode:output filename (fun oc -> Lwt_io.write oc buf))
    >>= fun () ->
    Repo.add ~path:filename
    >>= fun dir ->
    Repo.commit ~dir ~message
    (*>>= fun dir ->
      Repo.push ~dir*)
    >>= fun _ ->
    return ()
  ) (Repo.die "Goal.write_task")

module TaskSet = Set.Make(struct
  type t = Opam_task.t
  let compare = compare
end)
let missing_tasks user repo targets task_records =
  let set = List.fold_left (fun set (_,{ job=Opam task }) ->
    TaskSet.add task set) TaskSet.empty task_records in
  let branch = "master" in
  let repo_url = Uri.of_string
    (Printf.sprintf "git://github.com/%s/%s.git" user repo) in
  let work_dir = Filename.(concat (get_temp_dir_name ()) "ocamlotd") in
  let repo = Repo.({
    repo = { url = Uri.of_string ""; repo_url = URL repo_url; };
    reference = Ref branch;
    label = Printf.sprintf "%s/%s:%s" user repo branch;
  }) in
  catch (fun () ->
    Opam_repo.packages_of_repo work_dir repo
    >>= fun packages ->
    let tasks = Opam_task.(tasks_of_packages targets Build [repo] packages) in
    return (List.filter (fun task -> not (TaskSet.mem task set)) tasks)
  ) (Repo.die "Goal.missing_tasks")

let new_goal t_resource goal on_complete =
  let t = Resource.content t_resource in
  let goal_resource = Resource.index t.goals
    goal (update_goal ~on_complete)
    (goal_renderer Ocamlot.title (Resource.uri t_resource)) in
  Resource.bubble goal_resource t_resource lift_goal_to_t;
  goal_resource

let new_subgoal goal_resource subgoal =
  let goal = Resource.content goal_resource in
  let subgoal_resource = Resource.index goal.subgoals
    subgoal (update_goal ~on_complete:(fun _ () -> return ()))
    (goal_renderer goal.Ocamlot.title (Resource.uri goal_resource)) in
  Resource.bubble subgoal_resource goal_resource lift_subgoal_to_goal;
  subgoal_resource

let generate_subgoal_uri base subgoal =
  Uri.(resolve "" base (of_string subgoal.slug))

let generate_task_uri base genfn task =
  Uri.(resolve "" base
         (of_string (Printf.sprintf "%s/%d" task_subpath (genfn ()))))

let subresource_base uri =
  let path = Uri.path uri in
  if path.[String.length path - 1] = '/' then uri
  else Uri.with_path uri (path^"/")

let base_of_resource_slug resource slug =
  let base = subresource_base (Resource.uri resource) in
  Uri.(resolve "" base (of_string (slug^"/")))

let make_integration t_resource ~title ~descr ~slug ~min_id ~goal_state_path =
  let base = base_of_resource_slug t_resource slug in
  let queue, enqueue = Lwt_stream.create () in
  let task_mint = ref min_id in
  let new_task_id = mint_id task_mint in
  let integration = {
    slug;
    title;
    descr;
    subgoals=Resource.create_index (generate_subgoal_uri base);
    completed=Resource.create_archive [];
    tasks=Resource.create_index (generate_task_uri base new_task_id);
    queue;
    enqueue=(fun task_resource -> enqueue (Some task_resource));
    stream=queue;
  } in
  Util.mkdir_p (Filename.concat goal_state_path task_subpath) 0o700;
  new_goal t_resource integration
    (fun tr () -> write_task goal_state_path tr)

let make_pull integration_gr ~title ~descr ~slug =
  let base = base_of_resource_slug integration_gr slug in
  let queue, enqueue = Lwt_stream.create () in
  let task_mint = ref 0 in
  let new_task_id = mint_id task_mint in
  let pull_goal = {
    slug;
    title;
    descr;
    subgoals=Resource.create_index (generate_subgoal_uri base);
    completed=Resource.create_archive [];
    tasks=Resource.create_index (generate_task_uri base new_task_id);
    queue;
    enqueue=(fun task_resource -> enqueue (Some task_resource));
    stream=queue;
  } in
  new_subgoal integration_gr pull_goal
