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
type diff = { base : Repo.git Repo.branch; head : Repo.git Repo.branch }
type packages =
  | Diff of diff * Repo.git Repo.branch option
  | List of string list * diff option

type t = {
  packages : packages;
  target : target;
  action : action;
}

type 'a process = Continue of 'a | Terminate of Result.output

let (>>=) process f = match process with
  | Continue env -> f env
  | Terminate out -> Terminate out

let string_of_action = function
  | Check -> "check"
  | Build -> "build"

let string_of_diff { base; head } = Repo.(head.label^" onto "^base.label)
let string_of_packages = function
  | Diff (diff, None) -> string_of_diff diff
  | Diff (diff, Some overlay) ->
      Repo.(overlay.label^" onto "^(string_of_diff diff))
  | List (pkglst, None) -> String.concat "," pkglst
  | List (pkglst, Some diff) ->
      (String.concat "," pkglst)^" from "^(string_of_diff diff)

let string_of_target { arch; os; compiler } =
  Printf.sprintf "%s on %s (%s)"
    (compiler.c_version^compiler.c_build)
    (Host.string_of_os os)
    (Host.string_of_arch arch)

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

let terminate_of_process_error e = OpamProcess.(
  List.iter (fun l -> Printf.eprintf "ERR: %s\n" l) e.r_stderr;
  Terminate Result.({
    err=String.concat "\n" e.r_stderr;
    out=String.concat "\n" e.r_stdout;
    info=e.r_info;
  })
)

let clone_repo ~name ~branch =
  let dir = OpamFilename.Dir.of_string name in
  let url = Uri.to_string Repo.(branch.repo.repo_url) in
  let git_ref, ref_cmd = Repo.(match branch.name with
    | Ref r -> r, []
    | Commit (r, sha) -> r, [[ "git" ; "update-ref" ; r ; sha ]]
  ) in
  try
    OpamFilename.in_dir dir Repo.(fun () -> OpamSystem.commands ([
      [ "git" ; "clone" ; url ; "." ];
    ]@ref_cmd@[
      [ "git" ; "checkout" ; git_ref ];
    ]));
    Printf.eprintf "OCAMLOT repo clone %s\n%!" branch.Repo.label;
    Continue dir
  with OpamSystem.Process_error e -> terminate_of_process_error e

let try_merge ~merge_name ~base ~head =
  let merge_dir = OpamFilename.Dir.of_string merge_name in
  let head_url = Uri.to_string Repo.(head.repo.repo_url) in
  try (* TODO: update-ref ? *)
    OpamFilename.in_dir merge_dir Repo.(fun () -> OpamSystem.commands ([
      [ "git" ; "fetch" ; head_url ;
        match head.name with Ref h -> h^":"^h | Commit (h,_) -> h^":"^h ];
      [ "git" ; "merge" ; "--no-edit" ;
        match head.name with Ref h -> h | Commit (h,_) -> h ];
    ]));
    Printf.eprintf "OCAMLOT repo merge %s onto %s\n%!"
      head.Repo.label base.Repo.label;
    Continue merge_dir
  with
    | OpamSystem.Process_error e -> terminate_of_process_error e

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
  (* merge repositories *)
  let tmp_name = Util.make_fresh_dir ~root_dir
    ("ocamlot."^prefix^"."^(Time.date_to_string start)^".") in
  let () = Printf.eprintf "OCAMLOT temp_dir %s made\n%!" tmp_name in
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
    begin match packages with
      | Diff ({ base; head }, overlay) ->
          clone_repo ~name:merge_name ~branch:base
          >>= fun merge_dir ->
          try_merge ~merge_name ~base ~head
          >>= fun merge_dir ->
          begin match overlay with
            | None -> Continue merge_dir
            | Some overlay -> try_merge ~merge_name ~base ~head:overlay
          end
          >>= fun merge_dir ->
          set_opam_repo merge_dir;
          Continue merge_dir
          >>= try_infer_packages
      | List (pkgs, Some { base; head }) ->
          clone_repo ~name:merge_name ~branch:base
          >>= fun merge_dir ->
          try_merge ~merge_name ~base ~head
          >>= fun merge_dir ->
          set_opam_repo merge_dir;
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
            OpamSystem.command [ "rm"; "-rf"; Filename.concat tmp_name "opam-install" ];
            `Passed
          end else `Failed
        in
        Result.({ status; duration;
                  output={
                    err=String.concat "\n" (facts::""::result.OpamProcess.r_stderr);
                    out=String.concat "\n" result.OpamProcess.r_stdout;
                    info=result.OpamProcess.r_info};
                })
    | Terminate output ->
        let duration = Time.(elapsed start (now ())) in
        (* clean up opam-install *)
        OpamSystem.command [ "rm"; "-rf"; Filename.concat tmp_name "opam-install" ];

        Result.({ status=`Failed; duration; output })
