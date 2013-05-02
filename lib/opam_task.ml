module Client = OpamClient.SafeAPI

type package = {
  p_name   : string;
  p_version: string;
}

type compiler = {
  c_version : string;
  c_build   : string;
}

type target = {
  arch      : Host.arch;
  os        : Host.os;
  compiler  : compiler;
  packages  : package list;
(*
  depends   : package Set.t;
  extdepends: string;
*)
}

type action = Check | Build (*| Test | Benchmark*)

type t = {
  trunk : Repo.git Repo.branch;
  branch : Repo.git Repo.branch;
  target : target;
  action : action;
}

type 'a process = Continue of 'a | Terminate of Result.output
exception Onward

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
  (if not (OpamFilename.exists config_f) then initialize_opam ~jobs);
  Client.SWITCH.switch ~quiet:false ~warning:true switch;
  let clone_name = Filename.concat tmp "opam-install" in
  let clone_dir = OpamFilename.Dir.of_string clone_name in
  let master_dir = OpamFilename.Dir.of_string master_name in
  OpamFilename.copy_dir ~src:master_dir ~dst:clone_dir;
  OpamGlobals.root_dir := clone_name;
  OpamState.install_conf_ocaml_config clone_dir switch;
  Printf.eprintf "OCAMLOT cloned opam\n%!";
  clone_dir

let try_merge ~merge_name ~trunk ~branch =
  let merge_dir = OpamFilename.Dir.of_string merge_name in
  let trunk_url = Uri.to_string Repo.(trunk.branch_repo.repo_url) in
  let branch_url = Uri.to_string Repo.(branch.branch_repo.repo_url) in
  try
    OpamFilename.in_dir merge_dir Repo.(fun () -> OpamSystem.commands [
      [ "git" ; "clone" ; trunk_url ; "." ];
      [ "git" ; "checkout" ; trunk.branch_name ];
      [ "git" ; "fetch" ; branch_url ; branch.branch_name];
      [ "git" ; "merge" ; "--no-edit" ; "FETCH_HEAD" ];
    ]); raise Onward
  with
    | Onward -> Continue merge_dir
    | OpamSystem.Process_error e -> OpamProcess.(
      List.iter (fun l -> Printf.eprintf "ERR: %s\n" l) e.r_stderr;
      Terminate Result.({
        err=String.concat "\n" e.r_stderr;
        out=String.concat "\n" e.r_stdout;
        info=e.r_info;
      })
    )

let try_install target () =
  try
    (* attempt package installation to test *)
    OpamGlobals.yes := true;
    Client.install
      (OpamPackage.Name.Set.of_list
         (List.map (fun p ->
           OpamPackage.Name.of_string (p.p_name^"."^p.p_version)
          ) target.packages));
    raise Onward
  with
    | Onward -> Continue ()
    | OpamGlobals.Exit code -> OpamProcess.( (* TODO: capture errors *)
      Terminate Result.({
        err=Printf.sprintf "Exit with OPAM code %d\n" code;
        out="";
        info="";
      })
    )

let run ?jobs root_dir {trunk; branch; target} =
  let start = Time.now () in
  let jobs = match jobs with None -> 1 | Some j -> j in
  (* merge repositories *)
  let tmp_name = Util.make_fresh_dir ~root_dir
    ("ocamlot."^(Time.date_to_string start)^".") in
  let () = Printf.eprintf "OCAMLOT temp_dir %s made\n%!" tmp_name in
  let merge_name = Filename.concat tmp_name "opam-repository" in
  Unix.mkdir merge_name 0o700;
  match begin
    try_merge ~merge_name ~trunk ~branch
    >>= fun merge_dir ->
    let () = Printf.eprintf "OCAMLOT repo merge done\n%!" in
    (* initialize opam if necessary and alias target compiler *)
    let clone_dir = clone_opam ~jobs root_dir tmp_name  target.compiler in
    (* add merged repository to opam *)
    Client.REPOSITORY.add
      (OpamRepositoryName.of_string merge_name)
      `local merge_dir ~priority:None;
    Printf.eprintf "OCAMLOT repo merge added\n%!";
    Continue ()
    >>= try_install target
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
