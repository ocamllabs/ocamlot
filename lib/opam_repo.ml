open Sexplib.Std

open Repo

type pull_part = Base | Head
exception WTFGitHub of string
exception GitError of Result.output

let branch_of_pull part pull = Github_t.(
  let pull_id = pull.pull_number in
  let ref_base = "refs/pull/"^(string_of_int pull_id)^"/" in
  let base = match pull.pull_base.branch_repo with
    | None -> raise (WTFGitHub
                       (Printf.sprintf "pull %d lacks a base repo" pull_id))
    | Some repo -> repo
  in
  let repo_url = Repo.URL (Uri.of_string base.repo_clone_url) in
  match part with
    | Head -> let gitref = ref_base^"head" in Repo.({
      repo={ url=Uri.of_string ""; repo_url };
      reference=Repo.Commit (gitref, pull.pull_head.branch_sha);
      label=base.repo_full_name^":"^gitref;
    })
    | Base -> let gitref = ref_base^"base" in Repo.({
      repo={ url=Uri.of_string ""; repo_url };
      reference=Repo.Commit (gitref, pull.pull_base.branch_sha);
      label=base.repo_full_name^":"^gitref;
    })
)

let diff_of_pull pull = [
  branch_of_pull Head pull;
  branch_of_pull Base pull;
]

let pkg_change_re component = Re.(compile (seq [
  str "packages/"; group (rep1 (non_greedy any)); component
]))

let pkg_descr = Re.(str "/descr")
let pkg_descr_re = pkg_change_re pkg_descr
let pkg_semantic = Re.(alt [
  str "/opam";
  str "/url";
  seq [str "/files/"; non_greedy (rep1 any); str ".install"; eos];
])
let pkg_sem_re = pkg_change_re pkg_semantic

(* TODO: log inference *)
let try_infer_packages merge_dir =
  let mod_files = ref [] in
  try
    OpamFilename.in_dir merge_dir (fun () ->
      mod_files := List.filter (fun filename ->
        not (Re.execp pkg_descr_re filename)
      ) (OpamSystem.read_command_output
           [ "git" ; "diff" ; "--name-only" ; "HEAD~1" ; "HEAD" ]);
      let packages = OpamPackage.Name.(Set.of_list (List.map (fun filename ->
        of_string Re.(get (exec pkg_sem_re filename) 1)
      ) !mod_files)) in
      Continue (OpamPackage.Name.Set.elements packages)
    )
  with
    | Not_found ->
        let non_package_updates = List.filter (fun filename ->
          not (Re.execp pkg_sem_re filename)
        ) !mod_files in
        Terminate Result.({
          err=Printf.sprintf "Pull request modifies non-packages:\n%s\n"
            (String.concat "\n" non_package_updates);
          out="";
          info="";
        })
    | OpamSystem.Process_error e -> terminate_of_process_error e

(* TODO: asynchronous *)
let packages_of_diff prefix work_dir diff =
  let prefix = prefix^"-merge" in
  let tmp_name = make_temp_dir ~root_dir:work_dir ~prefix in
  let merge_name = Filename.concat tmp_name "opam-repository" in
  Unix.mkdir merge_name 0o700;
  match begin
    try_collapse ~name:merge_name diff
    >>= try_infer_packages
    >>= fun packages ->
    Continue (List.rev_map OpamPackage.Name.to_string packages)
  end with
    | Continue packages -> packages
    | Terminate output ->
        let open Printf in
        eprintf "ERROR in merging diff stack and inferring build tasks\n%!";
        raise (GitError output)
