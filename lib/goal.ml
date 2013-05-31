open Ocamlot

let rec update_goal_subgoal goal = function
  | New_task _
  | Update_task (_,_) -> goal
  | New_subgoal _ ->
      { goal with stream=Lwt_stream.choose
          (goal.queue::(List.map (fun g -> (Resource.content g).stream)
                          (Resource.to_list goal.subgoals))) }
  | Update_subgoal (_,subgoal_event) ->
      update_goal_subgoal goal subgoal_event

let update_goal goal = function
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
      Resource.archive goal.completed fst (Resource.find goal.tasks uri);
      goal
  | New_subgoal gr -> update_goal_subgoal goal (New_subgoal gr)
  | Update_subgoal (_, subgoal_event) -> update_goal_subgoal goal subgoal_event

let goal_renderer parent_title parent_uri =
  let render_html event =
    let render_task tr =
      let task = fst (Resource.content tr) in
      let tm, status = List.hd task.log in
      Printf.sprintf "<li><a href='%s'>%s (%s) : %s</a></li>"
        (Uri.to_string (Resource.uri tr))
        (string_of_event status) (Time.to_string tm) (string_of_job task.job)
    in
    let render_subgoal gr =
      let goal = Resource.content gr in
      Printf.sprintf "<li><a href='%s'>%s</a></li>"
        (Uri.to_string (Resource.uri gr))
        goal.title
    in
    let page goal = Printf.sprintf
      "<html><head><link rel='stylesheet' type='text/css' href='%s'/><title>%s : %s</title></head><body><h1>%s</h1><p>%s</p><ul>%s</ul><ul>%s</ul><p><a href='%s'>%s</a></body></html>"
      Ocamlot.style
      goal.title title goal.title goal.descr
      (String.concat "\n" (List.rev_map render_task
                             (Resource.to_list goal.tasks)))
      (String.concat "\n" (List.rev_map render_subgoal
                             (Resource.to_list goal.subgoals)))
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

let new_goal t_resource goal =
  let t = Resource.content t_resource in
  let goal_resource = Resource.index t.goals
    goal update_goal
    (goal_renderer Ocamlot.title (Resource.uri t_resource)) in
  Resource.bubble goal_resource t_resource lift_goal_to_t;
  goal_resource

let new_subgoal goal_resource subgoal =
  let goal = Resource.content goal_resource in
  let subgoal_resource = Resource.index goal.subgoals
    subgoal update_goal
    (goal_renderer goal.Ocamlot.title (Resource.uri goal_resource)) in
  Resource.bubble subgoal_resource goal_resource lift_subgoal_to_goal;
  subgoal_resource

let generate_subgoal_uri base subgoal =
  Uri.(resolve "" base (of_string subgoal.slug))

let generate_task_uri base genfn task =
  Uri.(resolve "" base
         (of_string (Printf.sprintf "task/%d" (genfn ()))))

let base_of_resource_slug resource slug =
  let base = Resource.uri resource in
  Uri.(resolve "" base (of_string (slug^"/")))

let make_integration t_resource ~title ~descr ~slug =
  let base = base_of_resource_slug t_resource slug in
  let queue, enqueue = Lwt_stream.create () in
  let task_mint = ref 0 in
  let new_task_id = mint_id task_mint in
  let integration = {
    slug;
    title;
    descr;
    subgoals=Resource.create_index (generate_subgoal_uri base) [];
    completed=Resource.create_archive [];
    tasks=Resource.create_index (generate_task_uri base new_task_id) [];
    queue;
    enqueue=(fun task_resource -> enqueue (Some task_resource));
    stream=queue;
  } in
  new_goal t_resource integration

let make_pull integration_gr ~title ~descr ~slug =
  let base = base_of_resource_slug integration_gr slug in
  let queue, enqueue = Lwt_stream.create () in
  let task_mint = ref 0 in
  let new_task_id = mint_id task_mint in
  let pull_goal = {
    slug;
    title;
    descr;
    subgoals=Resource.create_index (generate_subgoal_uri base) [];
    completed=Resource.create_archive [];
    tasks=Resource.create_index (generate_task_uri base new_task_id) [];
    queue;
    enqueue=(fun task_resource -> enqueue (Some task_resource));
    stream=queue;
  } in
  new_subgoal integration_gr pull_goal
