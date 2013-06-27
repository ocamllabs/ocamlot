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

type task_status =
  | Pending | Queued
  | Pass
  | Fail of Result.analysis list

let state_branch = "paleolithic-01"
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

let class_of_category = Result.(function
  | Broken -> "fail"
  | Errorwarn -> "errwarn"
  | Incompat -> "incompatible"
  | Dependency -> "dependency"
  | Fixable -> "meta"
  | Transient -> "transient"
  | System -> "system"
  | Ext_dep -> "extdep"
)

let goal_renderer parent_title parent_uri =
  let render_html event =
    let render_tasks_table trl =
      let opam_task_of_tr tr = match Resource.content tr with
        | { job = Opam ot } -> ot
      in
      let target_colfn tr = Opam_task.(match opam_task_of_tr tr with
        | { target } -> string_of_target target) in
      let last_event task = List.hd task.log in
      let simple_opam_tasks, other_tasks = List.partition (fun tr ->
        match opam_task_of_tr tr with
          | { Opam_task.packages = [ _ ] } -> true
          | _ -> false) trl
      in
      let cat_of_errs = function
        | [] -> Result.Broken
        | errs ->
            Result.(worst_of_categories (List.map category_of_analysis errs))
      in
      let task_state task = Result.(match last_event task with
        | (_, Completed (_, { status = Passed _ })) -> Pass
        | (_, Completed (_, { status = Failed (reason,_)})) -> Fail reason
        | (_, Started _) | (_, Checked_in _) -> Pending
        | _ -> Queued
      ) in
      let task_cell trl =
        let cell_classes trl =
          match List.fold_left (fun ostate tr ->
            match ostate, task_state (Resource.content tr) with
              | Fail x, (Pending | Queued | Pass)
              | (Pending | Queued | Pass), Fail x -> Fail x
              | Fail x, Fail y when x=y -> Fail x
              | Fail x, Fail y -> Fail (x @ y)
              | Pending, _ | _, Pending -> Pending
              | Queued, _ | _, Queued -> Queued
              | Pass, Pass -> Pass
          ) Pass trl with
            | Pass -> "pass"
            | Fail errs -> class_of_category (cat_of_errs errs)
            | Pending -> "pending"
            | Queued -> "queued"
        in
        let cell_link tr =
          let task = Resource.content tr in
          let task_state_str, attrs = match task_state task with
            | Pass -> "PASS", []
            | Fail errs ->
                Result.string_of_category (cat_of_errs errs),
              ["title", Result.string_of_analysis_list errs]
            | Pending -> "pending", []
            | Queued -> "queued", []
          in
          <:html<<a href=$uri:Resource.uri tr$
                    $alist:attrs$>$str:task_state_str$</a>&>>
        in
        let classes, content = match trl with
          | [] -> "", None
          | [tr] -> cell_classes [tr], Some (cell_link tr)
          | trl -> cell_classes trl, Some (Html.ul (List.map cell_link trl))
        in
        <:html<<td class=$str:classes$>$opt:content$</td>&>>
      in
      let tbl = Html.Table.(
        render ~row_anchor_prefix:"all"
          (create
             Opam_task.(fun tr ->
               match opam_task_of_tr tr with
                 | { packages = pkg::_ } -> Some pkg
                 | { packages = [] } -> None)
             target_colfn
             simple_opam_tasks) task_cell)
      in
      let latest_tasks (_,r) (_,r') =
        let cmp_time m tr =
          let t_0 = fst (last_event (Resource.content tr)) in
          if Time.(is_later (elapsed m t_0)) then t_0 else m in
        let r_tmax = List.fold_left cmp_time Time.min (List.flatten r) in
        let r'_tmax = List.fold_left cmp_time Time.min (List.flatten r') in
        if Time.(is_later (elapsed r'_tmax r_tmax)) then -1
        else if Time.(is_later (elapsed r_tmax r'_tmax)) then 1
        else 0
      in
      let done_tbl = Html.Table.(
        render ~row_anchor_prefix:"latest"
          (create ~sortfn:latest_tasks
             (fun tr ->
               let packages = (opam_task_of_tr tr).Opam_task.packages in
               match last_event (Resource.content tr), packages with
                 | (_, (Completed (_,_) | Started _ | Checked_in _)), pkg::_ ->
                     Some pkg
                 | _ -> None)
             target_colfn
             simple_opam_tasks) task_cell)
      in
      let other_task_list = Html.ul (List.map (fun tr ->
        let task = Resource.content tr in
        let tm, status = List.hd task.log in
        <:html<
        <a href="$uri:Resource.uri tr$">
          $str:string_of_event status$ ($str:Time.to_string tm$)
          : $str:string_of_job task.job$
        </a>&>>
      ) other_tasks) in
      (*       <h3 id="dep-severity">Dependency Error Severity</h3>
               $dep_tbl$
      *)
      <:html<
      <h3 id="latest-tasks">Latest Results</h3>
      $done_tbl$
      <h3 id="all-tasks">All Tasks</h3>
      $tbl$
      $if List.length other_tasks > 0
       then other_task_list
       else []
      $>>
    in
    let render_subgoal gr =
      let goal = Resource.content gr in
      <:html<<a href="$uri:Resource.uri gr$">$str:goal.title$</a>&>>
    in
    let page goal = Html.(
      to_string
        (page ~title:goal.title
         <:html<
           <p>$goal.descr$</p>
           $render_tasks_table
           (List.rev_append
              Resource.(archive_to_list
                          (create_archive
                             (List.map (fun r -> (r,fst))
                                (index_to_list goal.tasks))))
              (Resource.archive_to_list goal.completed))$
           $ul (List.rev_map render_subgoal
                  (Resource.index_to_list goal.subgoals))$
           <p><a href="$uri:parent_uri$">$str:parent_title$</a></p>
         >>))
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

let filter_map_tasks f = List.fold_left (fun lst (tid, old_task) ->
  match f old_task with
    | None -> lst
    | Some x -> (tid, x)::lst
) []

let commit_task dir tr =
  let uri = Resource.uri tr in
  let uri_str = Uri.to_string uri in
  let task_id = List.hd (List.rev Re_str.(split (regexp_string "/") uri_str)) in
  let (task,_) = Resource.content tr in
  (match List.hd task.Ocamlot.log with
    | (_,Completed (_,r)) -> return r
    | _ -> fail (Failure "Goal.commit_task on uncompleted task")
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
    (* TODO: handle exceptions *)
    Lwt_mutex.with_lock Ocamlot.git_state_lock (fun () ->
      Lwt_io.(with_file ~mode:output filename (fun oc -> Lwt_io.write oc buf))
      >>= fun () ->
      Repo.add ~path:filename
      >>= fun dir ->
      Repo.commit ~dir ~message
    )
    >>= fun _ ->
    return ()
  ) (Result.die "Goal.commit_task")

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
  ) (Result.die "Goal.missing_tasks")

let new_goal t_resource goal on_complete =
  let t = Resource.content t_resource in
  let goal_resource = Resource.index t.goals
    goal (update_goal ~on_complete)
    (goal_renderer Html.site_title (Resource.uri t_resource)) in
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
    (fun tr () ->
      commit_task goal_state_path tr
      >>= fun () ->
      catch (fun () ->
        Repo.push ~dir:goal_state_path ~branch:state_branch
        >>= fun _ -> return ()
      ) (fun _ -> (* the remote is probably down or flaky *)
        Printf.eprintf "Ignoring git push failure!\n%!";
        return ()
      )
    )

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
