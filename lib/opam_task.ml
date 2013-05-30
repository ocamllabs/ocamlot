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

open Sexplib.Std
open Lwt
module Client = OpamClient.SafeAPI

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
  diff : Repo.diff;
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
    (Repo.string_of_diff diff)
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
  Repo.run_command ~env ~cwd:tmp ("opam" :: "install" :: "--yes" :: pkgs)
  >>= fun r -> return (pkgs, r)

let run ?jobs prefix root_dir {action; diff; packages; target} =
  let start = Time.now () in
  let jobs = match jobs with None -> 1 | Some j -> j in
  let tmp_name = Repo.make_temp_dir ~root_dir ~prefix in
  let merge_name = Filename.concat tmp_name "opam-repository" in
  Unix.mkdir merge_name 0o700;
  let set_opam_repo merge_dir =
    let merge_dir = OpamFilename.Dir.of_string merge_dir in
    (* add merged repository to opam *)
    Client.REPOSITORY.add
      (OpamRepositoryName.of_string merge_name)
      `local merge_dir ~priority:None;
    (* remove default repository from opam *)
    Client.REPOSITORY.remove (OpamRepositoryName.of_string "default");
    Printf.eprintf "OCAMLOT repo merge added\n%!";
  in
  (* initialize opam if necessary and alias target compiler *)
  let clone_dir = clone_opam ~jobs root_dir tmp_name target.compiler in
  (* merge repositories *)
  catch (fun () ->
    Repo.try_collapse ~dir:merge_name diff
    >>= fun merge_dir ->
    set_opam_repo merge_dir;
    Printf.eprintf "OCAMLOT building %s\n%!" (String.concat " " packages);
    try_install tmp_name packages
    >>= fun (pkgs, result) ->
    let duration = Time.(elapsed start (now ())) in

    let facts = Printf.sprintf "OCAMLOT \"opam install %s\" succeeded in %s\n"
      (String.concat " " pkgs)
      (Time.duration_to_string duration) in
    Repo.run_command ~cwd:tmp_name [ "rm"; "-rf"; "opam-install" ]
    >>= fun _ ->
    return Result.({ status=Passed;
                     duration;
                     output={
                       err=facts^result.Repo.r_stderr;
                       out=result.Repo.r_stdout;
                       info=""};
                   })
  ) (fun exn ->
    let duration = Time.(elapsed start (now ())) in
    let err,out = Repo.(match exn with
      | ProcessError (Unix.WEXITED code, r) ->
          Printf.sprintf "OCAMLOT \"%s %s\" failed (%d) in %s\n"
            r.r_cmd (String.concat " " r.r_args) code
            (Time.duration_to_string duration)^r.r_stderr, r.r_stdout
      | ProcessError (Unix.WSTOPPED signum, r)
      | ProcessError (Unix.WSIGNALED signum, r) ->
          Printf.sprintf "OCAMLOT \"%s %s\" terminated by signal %d in %s\n"
            r.r_cmd (String.concat " " r.r_args) signum
            (Time.duration_to_string duration)^r.r_stderr, r.r_stdout
      | exn ->
          Printf.sprintf "OCAMLOT opam task terminated by \"%s\" in %s\n"
            (Printexc.to_string exn)
            (Time.duration_to_string duration), ""
    ) in
    (* clean up opam-install *)
    Repo.run_command ~cwd:tmp_name [ "rm"; "-rf"; "opam-install" ]
    >>= fun _ ->
    return Result.({ status=Failed; duration;
                     output={ err; out; info=""; };
                   })
  )

let tasks_of_packages targets action diff packages =
  List.fold_left (fun tasks package ->
    List.rev_append (List.rev_map (fun target -> {
      diff;
      packages=[package];
      target;
      action;
    }) targets) tasks
  ) [] packages
