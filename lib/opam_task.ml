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

type action = Check | Build (*| Test | Benchmark*) with sexp
type diff = { base : git branch; head : git branch } with sexp
type packages =
  | Diff of diff * git branch option
  | List of string list * diff option
with sexp

type t = {
  packages : packages;
  target : target;
  action : action;
} with sexp

let string_of_action = function
  | Check -> "check"
  | Build -> "build"

let string_of_diff { base; head } = head.label^" onto "^base.label
let string_of_packages = function
  | Diff (diff, None) -> string_of_diff diff
  | Diff (diff, Some overlay) ->
      overlay.label^" onto "^(string_of_diff diff)
  | List (pkglst, None) -> String.concat "," pkglst
  | List (pkglst, Some diff) ->
      (String.concat "," pkglst)^" from "^(string_of_diff diff)

let string_of_target { host; compiler } =
  Printf.sprintf "%s on %s"
    (compiler.c_version^compiler.c_build)
    (Host.to_string host)

let to_string { packages; target; action } =
  Printf.sprintf "[%s %s with %s]"
    (string_of_action action)
    (string_of_packages packages)
    (string_of_target target)

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

(* TODO: log inference *)
let pkg_semantic = Re.(alt [
  str "/opam";
  str "/url";
  seq [str "/files/"; non_greedy (rep1 any); str ".install"; eos];
])
let pkg_descr = Re.(str "/descr")
let pkg_change_re component = Re.(compile (seq [
  str "packages/"; group (rep1 (non_greedy any)); component
]))
let pkg_sem_re = pkg_change_re pkg_semantic
let pkg_descr_re = pkg_change_re pkg_descr
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

let run ?jobs prefix root_dir {action; packages; target} =
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
    begin match packages with
      | Diff ({ base; head }, overlay) ->
          clone_repo ~name:merge_name ~branch:base
          >>= fun dir ->
          try_merge ~dir ~base ~head
          >>= fun dir ->
          begin match overlay with
            | None -> Continue dir
            | Some overlay -> try_merge ~dir ~base ~head:overlay
          end
          >>= fun merge_dir ->
          set_opam_repo merge_dir;
          Continue merge_dir
          >>= try_infer_packages
      | List (pkgs, Some { base; head }) ->
          clone_repo ~name:merge_name ~branch:base
          >>= fun dir ->
          try_merge ~dir ~base ~head
          >>= fun dir ->
          set_opam_repo dir;
          Continue (List.map OpamPackage.Name.of_string pkgs)
      | List (pkgs, None) ->
          Continue (List.map OpamPackage.Name.of_string pkgs)
    end
    >>= fun packages ->
    let pkgs = List.map OpamPackage.Name.to_string packages in
    Printf.eprintf "OCAMLOT building %s\n%!" (String.concat " " pkgs);
    Continue pkgs
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
