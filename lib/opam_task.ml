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
(*module Client = OpamClient.SafeAPI*)

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

exception TargetError of target * (compiler * string) list

let ocamlc = "ocamlc"

let string_of_action = function
  | Check -> "check"
  | Build -> "build"

let string_of_compiler { c_version; c_build } = c_version^c_build

let string_of_target { host; compiler } =
  Printf.sprintf "%s on %s"
    (string_of_compiler compiler)
    (Host.to_string host)

let to_string { diff; packages; target; action } =
  Printf.sprintf "[%s %s %s with %s]"
    (string_of_action action)
    (String.concat "," packages)
    (Repo.string_of_diff diff)
    (string_of_target target)

let make_env alist = Array.of_list (List.map (fun (k,v) -> k^"="^v) alist)
let basic_env ?(path=[]) home opam_dir =
  let path = match path with [] -> "" | path -> (String.concat ":" path)^":" in
  [
    "OCAMLRUNPARAM","b";
    "HOME",home;
    "OPAMROOT",opam_dir;
    "PATH",path^(Unix.getenv "PATH");
  ]

let ocamlc_path ?env cwd =
  let env = match env with None -> None | Some e -> Some (make_env e) in
  Repo.run_command ?env ~cwd
    [ "which" ; ocamlc ]
  >>= fun { Repo.r_stdout } ->
  let which_ocamlc = Util.strip r_stdout in
  return which_ocamlc

let version_patt = Re.(seq [
  bos;
  group (seq [rep1 digit; char '.'; rep1 digit; char '.'; rep1 digit]);
  group (rep any);
  eos;
])
let version_re = Re.compile version_patt

let compiler_of_path exec =
  catch (fun () ->
    Repo.run_command ~cwd:(Filename.dirname exec)
      [ exec ; "-version" ]
    >>= fun { Repo.r_stdout } ->
    let vstr = Util.strip r_stdout in
    Printf.eprintf "Got %s for %s\n%!" vstr exec;
    let matches = Re.(get_all (exec version_re vstr)) in
    return { c_version = matches.(1); c_build = matches.(2) }
  ) (fun exn ->
    Printf.eprintf "%s -version failed with:\n%s\n%!"
      exec (Printexc.to_string exn);
    fail exn
  )

let list_compilers root subpath =
  let ok p = Lwt_unix.(access p [F_OK;X_OK]) in
  let listing_stream = Lwt_unix.files_of_directory root in
  Lwt_stream.fold (fun filename l ->
    if filename <> "." && filename <> ".." then filename::l else l
  ) listing_stream []
  >>= Lwt_list.fold_left_s (fun l cdir ->
    let f = Filename.(concat (concat root cdir) subpath) in
    catch (fun () ->
      ok f
      >>= fun () -> ok (Filename.concat f ocamlc)
      >>= fun () -> return (f::l))
      (fun exn -> return l)
  ) []
  >>= Lwt_list.rev_map_p (fun p ->
    compiler_of_path (Filename.concat p ocamlc)
    >>= fun compiler -> return (compiler, p)
  )

let opam_config_env_extractor =
  Re.(compile (rep (seq [
    bol;
    group (rep1 (compl [set "="]));
    char '=';
    group (rep (compl [set ";"]));
    char ';'; non_greedy (rep any); eol;
  ])))
let opam_env ~path home opam_dir =
  let rec extract_env s pos lst =
    match Re.(get_all (exec ~pos opam_config_env_extractor s)) with
      | [|"";"";""|] -> lst
      | [|m; k; v|] -> extract_env s (pos + (String.length m) + 1) ((k,v)::lst)
      | _ -> lst
  in
  let basic = basic_env ~path home opam_dir in
  let env = make_env basic in
  Repo.run_command ~env ~cwd:home
    [ "opam" ; "config" ; "env" ]
  >>= fun { Repo.r_stdout } ->
  let env = extract_env r_stdout 0 [] in
  return ((List.remove_assoc "PATH" basic)@env)

let initialize_opam ~env ~cwd ~jobs =
  Repo.run_command ~env ~cwd
    [ "opam"; "init"; "--no-setup"; "-j"; string_of_int jobs ]
  >>= fun _ -> return ()

let add_opam_repository ~env ~cwd name dir =
  Repo.run_command ~env ~cwd
    [ "opam"; "repository"; "add"; name; dir ]

let remove_opam_repository ~env ~cwd name =
  Repo.run_command ~env ~cwd
    [ "opam"; "repository"; "remove"; name ]

(* clone opam (initialize if necessary) and switch to target compiler *)
(*let clone_opam ~jobs root tmp compiler =
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
*)

let try_install env tmp pkgs =
  let () = Array.iter print_endline env in
  Repo.run_command ~env ~cwd:tmp ("opam" :: "install" :: "--yes" :: pkgs)
  >>= fun r -> return (pkgs, r)

let run ?jobs prefix root_dir ocaml_dir {action; diff; packages; target} =
  let start = Time.now () in
  let jobs = match jobs with None -> 1 | Some j -> j in
  let tmp_name = Repo.make_temp_dir ~root_dir ~prefix in
  let opam_root = Filename.concat tmp_name "opam-install" in
  let { c_version } = target.compiler in

  let clean_up () =
    Repo.run_commands ~cwd:tmp_name [
      [ "rm"; "-rf"; "opam-install" ];
      [ "rm"; "-rf"; "opam-repository" ];
    ] >>= fun _ -> return ()
  in

  let opam_fail info exn =
    let duration = Time.(elapsed start (now ())) in
    let err,out = Repo.process_error
      (Printf.sprintf "After %s Opam_task.run"
         (Time.duration_to_string duration))
      exn in
    clean_up ()
    >>= fun () ->
    return Result.({ status=Failed; duration;
                     output={ err; out; info; };
                   })
  in

  catch (fun () ->
    ocamlc_path ~env:(basic_env tmp_name opam_root) ocaml_dir
    >>= compiler_of_path
    >>= fun compiler -> begin
      if (compiler.c_version = c_version)
      then return []
      else
        (list_compilers ocaml_dir "bin")
         >>= fun compilers -> begin
           try
             return [snd (List.find (fun ({ c_version=v },_) -> v=c_version)
                            compilers)
                    ]
           with Not_found ->
             fail (TargetError (target, compilers))
         end
    end
    >>= fun path ->
    let env = make_env (basic_env ~path tmp_name opam_root) in
    let merge_name = Filename.concat tmp_name "opam-repository" in
    Unix.mkdir merge_name 0o700;
    let set_opam_repo env merge_name =
      add_opam_repository ~env ~cwd:merge_name merge_name merge_name
      >>= fun _ ->
      remove_opam_repository ~env ~cwd:merge_name "default"
      >>= fun _ ->
      Printf.eprintf "OCAMLOT repo merge added\n%!";
      return merge_name
    in
    initialize_opam ~env ~cwd:tmp_name ~jobs
    >>= fun () ->
    opam_env ~path tmp_name opam_root
    >>= fun env ->
    ocamlc_path ~env ocaml_dir
    >>= compiler_of_path
    >>= fun compiler ->
    let info = "Compiler: "^(string_of_compiler compiler)^"\n" in
    let info = List.fold_left (fun s (k,v) -> s^k^" = "^v^"\n") info env in

    let env = make_env env in

    (*
    Repo.run_commands ~env ~cwd:tmp_name [
      [ "which" ; "ocamlfind" ];
      [ "strace" ; "-f" ; "ocamlfind" ; "ocamlc" ; "-where" ];
    ]
    >>= fun [{Repo.r_stdout=wof};{Repo.r_stdout;r_stderr}] ->
    let info =
      info^"\n$ which ocamlfind\n"^wof^"\n"
      ^r_stdout^"\n\n"^r_stderr
    in
    *)

    catch (fun () ->
      Repo.try_collapse ~dir:merge_name diff
      >>= set_opam_repo env
      >>= fun merge_dir ->
      Printf.eprintf "OCAMLOT building %s\n%!" (String.concat " " packages);
      try_install env tmp_name packages
      >>= fun (pkgs, result) ->
      let duration = Time.(elapsed start (now ())) in

      let facts = Printf.sprintf "OCAMLOT \"opam install %s\" succeeded in %s\n"
        (String.concat " " pkgs)
        (Time.duration_to_string duration) in
      clean_up ()
      >>= fun () ->
      return Result.({ status=Passed;
                       duration;
                       output={
                         err=facts^result.Repo.r_stderr;
                         out=result.Repo.r_stdout;
                         info};
                     })
    ) (opam_fail info)
  ) (opam_fail "")

let tasks_of_packages targets action diff packages =
  List.fold_left (fun tasks package ->
    List.rev_append (List.rev_map (fun target -> {
      diff;
      packages=[package];
      target;
      action;
    }) targets) tasks
  ) [] packages
