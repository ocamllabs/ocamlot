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

let bin_subpath = "bin"
let ocamlc = "ocamlc"

let ocamlfind_not_found = "#!/bin/sh\n\n"
  ^"echo \"This isn't the ocamlfind you're looking for.\"\n"
  ^"echo \"ocamlfind: Package \\`ocamlfind' not found\"\n"
  ^"exit 1\n"

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

let which cmd ?env cwd =
  let env = match env with None -> None | Some e -> Some (make_env e) in
  Repo.run_command ?env ~cwd
    [ "which" ; cmd ]
  >>= fun { Repo.r_stdout } -> return (Util.strip r_stdout)

let ocamlc_path = which ocamlc

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
  Re.(compile (seq [
    bol;
    group (rep1 (compl [set "="]));
    char '=';
    opt (char '"');
    group (rep (compl [set ";\""]));
    opt (char '"');
    char ';'; non_greedy (rep any); eol;
  ]))
let opam_env ?(debug=false) ~path home opam_dir =
  let rec extract_env s pos lst =
    match begin
      try Some Re.(get_all_ofs (exec ~pos opam_config_env_extractor s))
      with Not_found -> None
    end with None -> lst | Some ofs -> begin
      match Array.map (fun (a,z) -> String.sub s a (z-a)) ofs with
        | [|m; k; v|] -> extract_env s (snd ofs.(0)) ((k,v)::lst)
        | _ -> lst
    end
  in
  let basic = basic_env ~path home opam_dir in
  let env = make_env basic in
  Repo.run_command ~env ~cwd:home
    [ "opam" ; "config" ; "env" ]
  >>= fun { Repo.r_stdout } ->
  let env = extract_env r_stdout 0 [] in
  let path = Re_str.(split (regexp_string ":") (List.assoc "PATH" env)) in
  let path = (List.hd path)::home::(List.tl path) in
  let env = ("PATH", String.concat ":" path)::(List.remove_assoc "PATH" env) in
  let env = if debug then ("OPAMKEEPBUILDDIR","1")::env else env in
  return ((List.remove_assoc "OPAMROOT" (List.remove_assoc "PATH" basic))@env)

let initialize_opam ~env ~cwd ~jobs =
  Repo.run_command ~env ~cwd
    [ "opam"; "init"; "--no-setup"; "-j"; string_of_int jobs ]

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

let try_install ?(debug=false) env tmp pkgs =
  if debug then Array.iter print_endline env;
  Repo.run_command ~env ~cwd:tmp
    ("opam" :: "install" :: "--verbose" :: "--yes" :: pkgs)
  >>= fun r -> return (pkgs, r)

let run ?(debug=false) ?jobs prefix root_dir ocaml_dir
    {action; diff; packages; target} =
  let start = Time.now () in
  let jobs = match jobs with None -> 1 | Some j -> j in
  let tmp_name = Repo.make_temp_dir ~root_dir ~prefix in
  let opam_root = Filename.concat tmp_name "opam-install" in
  let { c_version } = target.compiler in

  let clean_up () =
    if debug then return ()
    else Repo.run_commands ~cwd:tmp_name [
      [ "rm"; "-rf"; "opam-install" ];
      [ "rm"; "-rf"; "opam-repository" ];
      [ "rm"; "-f";  "ocamlfind" ];
    ] >>= fun _ -> return ()
  in

  let opam_fail info exn =
    let duration = Time.(elapsed start (now ())) in
    clean_up ()
    >>= fun () ->
    let error = Result.error_of_exn exn in
    return Result.({ status=Failed (analyze error, error); duration; info; })
  in

  catch (fun () ->
    list_compilers ocaml_dir bin_subpath
    >>= fun compilers -> begin
      try
        return [snd (List.find (fun ({ c_version=v },_) -> v=c_version)
                       compilers)
               ]
      with Not_found -> begin
        (* maybe this task is being run ad hoc (e.g. cli) *)
        ocamlc_path
          ~env:(basic_env ~path:[ocaml_dir] tmp_name opam_root)
          ocaml_dir
        >>= compiler_of_path
        >>= fun compiler ->
        if (compiler.c_version = c_version)
        then return []
        else fail (TargetError (target, compilers))
      end
    end
    >>= fun path ->
    let env = make_env (basic_env ~path tmp_name opam_root) in
    let merge_name = Filename.concat tmp_name "opam-repository" in
    Unix.mkdir merge_name 0o700;
    let set_opam_repo env merge_name =
      add_opam_repository ~env ~cwd:merge_name "ocamlot" merge_name
      >>= fun _ ->
      remove_opam_repository ~env ~cwd:merge_name "default"
      >>= fun _ ->
      Printf.eprintf "OCAMLOT repo merge added\n%!";
      return merge_name
    in
    Lwt_io.(with_file ~perm:0o700 ~mode:output
              (Filename.concat tmp_name "ocamlfind")
              (fun oc -> write oc ocamlfind_not_found))
    >>= fun () ->
    initialize_opam ~env ~cwd:tmp_name ~jobs
    >>= fun _r ->
    opam_env ~debug ~path tmp_name opam_root
    >>= fun env ->
    Repo.run_command ~env:(make_env env) ~cwd:tmp_name
      [ "opam" ; "--git-version" ; ]
    >>= fun { Repo.r_stdout } ->
    let opam_version = Util.strip r_stdout in
    ocamlc_path ~env ocaml_dir
    >>= compiler_of_path
    >>= fun compiler ->
    let info = "Compiler: "^(string_of_compiler compiler)^"\n" in
    let info = info^"OPAM: "^opam_version^"\n" in
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
      try_install ~debug env tmp_name packages
      >>= fun (pkgs, result) ->
      let duration = Time.(elapsed start (now ())) in
      clean_up ()
      >>= fun () ->
      return Result.({ status=Passed result; duration; info; })
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
