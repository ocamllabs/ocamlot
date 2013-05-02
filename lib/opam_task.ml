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

let initialize () =
  let repo_name = OpamRepositoryName.default in
  let repo_address = OpamRepository.default_address in
  let repo_kind = `http in
  let repo_priority = 0 in
  let repository = OpamTypes.({
    repo_name; repo_address; repo_kind; repo_priority;
  }) in
  Client.init repository OpamCompiler.system ~jobs:1
    `sh (OpamFilename.of_string "") `no

(* clone (or initialize and clone) opam and switch to target compiler *)
let clone root tmp compiler =
  let master_name = Filename.concat root "ocamlot.opam.master" in
  OpamGlobals.root_dir := master_name;
  let opam_root = OpamPath.default () in
  let config_f = OpamPath.config opam_root in
  (if OpamFilename.exists config_f
   then Client.update []
   else initialize ());
  Client.SWITCH.switch ~quiet:false ~warning:true
    (OpamSwitch.of_string (compiler.c_version^compiler.c_build));
  let clone_name = Filename.concat tmp "opam-install" in
  let clone_dir = OpamFilename.Dir.of_string clone_name in
  let master_dir = OpamFilename.Dir.of_string master_name in
  OpamFilename.copy_dir ~src:master_dir ~dst:clone_dir;
  OpamGlobals.root_dir := clone_name;
  clone_dir

let run root {trunk; branch; target} =
  let start = Time.now () in
  (* merge repositories *)
  let tmp_name = Util.make_temp_dir ~temp_dir:root "ocamlot.opam." "" in
  let merge_name = Filename.concat tmp_name "opam-repository" in
  Unix.mkdir merge_name 0o700;
  let merge_dir = OpamFilename.Dir.of_string merge_name in
  let trunk_url = Uri.to_string Repo.(trunk.branch_repo.repo_url) in
  let branch_url = Uri.to_string Repo.(branch.branch_repo.repo_url) in
  match (try
           OpamFilename.in_dir merge_dir Repo.(fun () -> OpamSystem.commands [
             [ "git" ; "clone" ; trunk_url ; "." ];
             [ "git" ; "checkout" ; trunk.branch_name ];
             [ "git" ; "fetch" ; branch_url ; branch.branch_name];
             [ "git" ; "merge" ; "--no-edit" ; "FETCH_HEAD" ];
           ]); None
    with OpamSystem.Process_error e -> OpamProcess.(
      List.iter (fun l -> Printf.eprintf "ERR: %s\n" l) e.r_stderr;
      Some Result.({err=String.concat "\n" e.r_stderr;
                    out=String.concat "\n" e.r_stdout;
                    info=e.r_info;
                   })
    )
  ) with
    | Some error -> Result.Error error
    | None ->
        (* switch to target compiler and clone (or initialize) opam *)
        let clone_dir = clone root tmp_name target.compiler in
        (* add merged repository to opam *)
        Client.REPOSITORY.add
          (OpamRepositoryName.of_string merge_name)
          `local merge_dir ~priority:None;
        (* update opam *)
        Client.update [];
        (* attempt package installation to test *)
        OpamGlobals.yes := true;
        Client.install (OpamPackage.Name.Set.of_list
                          (List.map (fun p ->
                            OpamPackage.Name.of_string (p.p_name^"."^p.p_version)
                           ) target.packages));
        (* report success *)
        Result.OK Time.(elapsed start (now ()))
