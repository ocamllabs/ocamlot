open Sexplib.Std

module Client = OpamClient.SafeAPI
open Repo

type compiler = {
  c_version : string;
  c_build   : string;
} with sexp

type target = {
  host : Host.t;
  compiler  : compiler;
(*
  packages  : package list;
  depends   : package Set.t;
  extdepends: string;
*)
} with sexp

type action = Check | Build (* | Test | Benchmark*) with sexp

type t = {
  diff : diff;
  packages : string list;
  target : target;
  action : action;
} with sexp

let string_of_action = function
  | Check -> "check"
  | Build -> "build"

let string_of_target { host; compiler } =
  Printf.sprintf "%s on %s"
    (compiler.c_version^compiler.c_build)
    (Host.to_string host)

let to_string { diff; packages; target; action } =
  Printf.sprintf "[%s %s %s with %s]"
    (string_of_action action)
    (String.concat "," packages)
    (string_of_diff diff)
    (string_of_target target)

let initialize_opam ~jobs =
  let repository = OpamRepository.default () in
  Client.init repository OpamCompiler.system ~jobs
    `sh (OpamFilename.of_string "") `no

(* clone opam (initialize if necessary) and switch to target compiler *)
let clone_opam ~jobs root tmp compiler =
  let switch = OpamSwitch.of_string (compiler.c_version^compiler.c_build) in
  let master_name = Filename.concat root "ocamlot.opam.master" in
  OpamGlobals.root_dir := master_name;
  OpamGlobals.keep_build_dir := true;
  let opam_root = OpamPath.default () in
  let config_f = OpamPath.config opam_root in
  (if OpamFilename.exists config_f
   then Client.update []
   else initialize_opam ~jobs);
  Client.SWITCH.switch ~quiet:false ~warning:true switch;
  let build_dir = OpamPath.Switch.build_ocaml opam_root switch in
  (if OpamFilename.exists_dir build_dir
   then OpamSystem.command [ "rm"; "-rf"; OpamFilename.Dir.to_string build_dir ]);
  let clone_name = Filename.concat tmp "opam-install" in
  let clone_dir = OpamFilename.Dir.of_string clone_name in
  let master_dir = OpamFilename.Dir.of_string master_name in
  OpamFilename.copy_dir ~src:master_dir ~dst:clone_dir;
  OpamGlobals.root_dir := clone_name;
  OpamState.install_conf_ocaml_config clone_dir switch;
  Printf.eprintf "OCAMLOT cloned opam\n%!";
  clone_dir

let try_install tmp pkgs =
  (* TODO: fix this terrible hack *)
  let env = OpamState.(
    Array.of_list
      (List.map (fun (k,v) -> k^"="^v)
         (("OCAMLRUNPARAM","b")
          ::("HOME",tmp)
          ::(get_opam_env (load_env_state "ocamlot-try-install"))))
  ) in
  let () = Array.iter print_endline env in
  let r = OpamProcess.run ~name:(Filename.concat tmp "install") ~env
    "opam" ("install" :: "--yes" :: pkgs) in
  Continue (pkgs, r)
(* Maybe once we tame opam's stdout/stderr...
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
*)

let run ?jobs prefix root_dir {action; diff; packages; target} =
  let start = Time.now () in
  let jobs = match jobs with None -> 1 | Some j -> j in
  let tmp_name = make_temp_dir ~root_dir ~prefix in
  let merge_name = Filename.concat tmp_name "opam-repository" in
  Unix.mkdir merge_name 0o700;
  let set_opam_repo merge_dir =
    (* add merged repository to opam *)
    Client.REPOSITORY.add
      (OpamRepositoryName.of_string merge_name)
      `local merge_dir ~priority:None;
    (* remove default repository from opam *)
    Client.REPOSITORY.remove (OpamRepositoryName.of_string "default");
    Printf.eprintf "OCAMLOT repo merge added\n%!";
  in
  match begin
    (* initialize opam if necessary and alias target compiler *)
    let clone_dir = clone_opam ~jobs root_dir tmp_name target.compiler in
    (* merge repositories *)
    try_collapse ~name:merge_name diff
    >>= fun merge_dir ->
    set_opam_repo merge_dir;
    Printf.eprintf "OCAMLOT building %s\n%!" (String.concat " " packages);
    Continue packages
    >>= try_install tmp_name
  end with
    | Continue (pkgs, result) ->
        let duration = Time.(elapsed start (now ())) in

        let facts = Printf.sprintf "OCAMLOT \"opam install %s\" exited %d in %fs"
          (String.concat " " pkgs)
          result.OpamProcess.r_code
          result.OpamProcess.r_duration in
        let status = if result.OpamProcess.r_code = 0
          then begin (* clean up opam-install *)
            OpamSystem.command [ "rm"; "-rf";
                                 Filename.concat tmp_name "opam-install" ];
            Result.Passed
          end else Result.Failed
        in
        Result.({ status; duration;
                  output={
                    err=String.concat "\n"
                      (facts::""::result.OpamProcess.r_stderr);
                    out=String.concat "\n" result.OpamProcess.r_stdout;
                    info=result.OpamProcess.r_info};
                })
    | Terminate output ->
        let duration = Time.(elapsed start (now ())) in
        (* clean up opam-install *)
        OpamSystem.command [ "rm"; "-rf";
                             Filename.concat tmp_name "opam-install" ];

        Result.({ status=Failed; duration; output })

let tasks_of_packages targets action diff packages =
  List.fold_left (fun tasks package ->
    List.rev_append (List.rev_map (fun target -> {
      diff;
      packages=[package];
      target;
      action;
    }) targets) tasks
  ) [] packages
