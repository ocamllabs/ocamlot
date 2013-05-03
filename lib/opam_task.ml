module Client = OpamClient.SafeAPI

type compiler = {
  c_version : string;
  c_build   : string;
}

type target = {
  arch      : Host.arch;
  os        : Host.os;
  compiler  : compiler;
(*
  packages  : package list;
  depends   : package Set.t;
  extdepends: string;
*)
}

type action = Check | Build (*| Test | Benchmark*)

type t = {
  base : Repo.git Repo.branch;
  head : Repo.git Repo.branch;
  target : target;
  action : action;
}

type 'a process = Continue of 'a | Terminate of Result.output

let (>>=) process f = match process with
  | Continue env -> f env
  | Terminate out -> Terminate out

let initialize_opam ~jobs =
  let repo_name = OpamRepositoryName.default in
  let repo_address = OpamRepository.default_address in
  let repo_kind = `http in
  let repo_priority = 0 in
  let repository = OpamTypes.({
    repo_name; repo_address; repo_kind; repo_priority;
  }) in
  Client.init repository OpamCompiler.system ~jobs
    `sh (OpamFilename.of_string "") `no

(* clone opam (initialize if necessary) and switch to target compiler *)
let clone_opam ~jobs root tmp compiler =
  let switch = OpamSwitch.of_string (compiler.c_version^compiler.c_build) in
  let master_name = Filename.concat root "ocamlot.opam.master" in
  OpamGlobals.root_dir := master_name;
  let opam_root = OpamPath.default () in
  let config_f = OpamPath.config opam_root in
  (if OpamFilename.exists config_f
   then Client.update []
   else initialize_opam ~jobs);
  Client.SWITCH.switch ~quiet:false ~warning:true switch;
  let clone_name = Filename.concat tmp "opam-install" in
  let clone_dir = OpamFilename.Dir.of_string clone_name in
  let master_dir = OpamFilename.Dir.of_string master_name in
  OpamFilename.copy_dir ~src:master_dir ~dst:clone_dir;
  OpamGlobals.root_dir := clone_name;
  OpamState.install_conf_ocaml_config clone_dir switch;
  Printf.eprintf "OCAMLOT cloned opam\n%!";
  clone_dir

let terminate_of_process_error e = OpamProcess.(
  List.iter (fun l -> Printf.eprintf "ERR: %s\n" l) e.r_stderr;
  Terminate Result.({
    err=String.concat "\n" e.r_stderr;
    out=String.concat "\n" e.r_stdout;
    info=e.r_info;
  })
)

let try_merge ~merge_name ~base ~head =
  let merge_dir = OpamFilename.Dir.of_string merge_name in
  let base_url = Uri.to_string Repo.(base.repo.repo_url) in
  let head_url = Uri.to_string Repo.(head.repo.repo_url) in
  try
    OpamFilename.in_dir merge_dir Repo.(fun () -> OpamSystem.commands [
      [ "git" ; "clone" ; base_url ; "." ];
      [ "git" ; "checkout" ; base.name ];
      [ "git" ; "fetch" ; head_url ; head.name];
      [ "git" ; "merge" ; "--no-edit" ; "FETCH_HEAD" ];
    ]);
    Printf.eprintf "OCAMLOT repo merge %s onto %s\n%!"
      head.Repo.label base.Repo.label;
    Continue merge_dir
  with
    | OpamSystem.Process_error e -> terminate_of_process_error e

(* TODO: log inference *)
let pkg_semantic = Re.(alt [str "/opam"; str "/url"])
let pkg_descr = Re.(str "/descr")
let pkg_change_re component = Re.(compile (seq [
  str "packages/"; group (rep1 (non_greedy any)); component
]))
let pkg_sem_re = pkg_change_re pkg_semantic
let pkg_descr_re = pkg_change_re pkg_descr
let try_infer_packages merge_dir =
  let mod_files = ref [] in
  try
    OpamFilename.in_dir merge_dir Repo.(fun () ->
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

let try_install packages =
  try
    OpamGlobals.yes := true;
    Client.install (OpamPackage.Name.Set.of_list packages);
    Continue ()
  with
    | OpamGlobals.Exit code -> OpamProcess.( (* TODO: capture errors *)
      Terminate Result.({
        err=Printf.sprintf "Exit with OPAM code %d\n" code;
        out="";
        info="";
      })
    )

let run ?jobs prefix root_dir {base; head; target} =
  let start = Time.now () in
  let jobs = match jobs with None -> 1 | Some j -> j in
  (* merge repositories *)
  let tmp_name = Util.make_fresh_dir ~root_dir
    ("ocamlot."^prefix^"."^(Time.date_to_string start)^".") in
  let () = Printf.eprintf "OCAMLOT temp_dir %s made\n%!" tmp_name in
  let merge_name = Filename.concat tmp_name "opam-repository" in
  Unix.mkdir merge_name 0o700;
  match begin
    try_merge ~merge_name ~base ~head
    >>= fun merge_dir ->
    (* initialize opam if necessary and alias target compiler *)
    let clone_dir = clone_opam ~jobs root_dir tmp_name  target.compiler in
    (* add merged repository to opam *)
    Client.REPOSITORY.add
      (OpamRepositoryName.of_string merge_name)
      `local merge_dir ~priority:None;
    Printf.eprintf "OCAMLOT repo merge added\n%!";
    Continue merge_dir
    >>= try_infer_packages
    >>= fun packages ->
    Printf.eprintf "OCAMLOT testing %s\n%!"
      (String.concat " " (List.map OpamPackage.Name.to_string packages));
    Continue packages
    >>= try_install
    >>= fun () ->
    Printf.eprintf "OCAMLOT test packages built and installed\n%!";
    Continue ()
  end with
    | Continue () ->
        (* report success *)
        let duration = Time.(elapsed start (now ())) in
        (* TODO: capture stdio *)
        Result.({ status=`Passed; duration; output={err="";out="";info=""} })
    | Terminate output ->
        let duration = Time.(elapsed start (now ())) in
        Result.({ status=`Failed; duration; output })
