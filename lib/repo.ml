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

module Uri = struct
  include Uri
  let t_of_sexp sexp =
    of_string (Sexplib.Std.string_of_sexp sexp)
  let sexp_of_t uri = Sexplib.Std.sexp_of_string (to_string uri)
end

type git =
  | SSH of Uri.t * Uri.t
  | URL of Uri.t
with sexp

type 'a t = {
  url : Uri.t;
  repo_url : 'a;
} with sexp

type sha = string with sexp

type reference =
  | Ref of string
  | Commit of string * sha
  | Copy of string * reference
with sexp

type 'a branch = {
  repo : 'a t;
  label : string;
  reference : reference;
} with sexp

type diff = git branch list with sexp

type r = {
  r_cmd : string;
  r_args : string list;
  r_env : string array;
  r_cwd : string;
  r_duration : Time.duration;
  r_stdout : string;
  r_stderr : string;
}

exception ProcessError of Unix.process_status * r

let rec string_of_diff = function
  | [] -> ""
  | b::[] -> b.label
  | b::bs -> b.label^" onto "^(string_of_diff bs)

let string_of_git = function
  | SSH (host, path) -> (Uri.to_string host)^":"^(Uri.to_string path)
  | URL url -> Uri.to_string url

let string_of_reference = function Ref h | Commit (h,_) | Copy (h,_) -> h

let run_command ?(env=[||]) ~cwd cmd_args =
  let pgsz = 1 lsl 12 in
  let cmd = List.hd cmd_args in
  let args = List.tl cmd_args in
  let time = Time.now () in
  Unix.chdir cwd;
  let cmd_args = Array.of_list cmd_args in
  Lwt_process.with_process_full ~env ("", cmd_args) (fun process ->
    let stdout = Lwt_io.read_lines process#stdout in
    let stderr = Lwt_io.read_lines process#stderr in
    let outbuf = Buffer.create pgsz in
    let errbuf = Buffer.create pgsz in
    let write_line buf s =
      Buffer.add_string buf s;
      Buffer.add_char buf '\n';
    in
    let read () =
      List.iter (write_line outbuf) (Lwt_stream.get_available stdout);
      List.iter (write_line errbuf) (Lwt_stream.get_available stderr);
    in
    let rec stream () = match process#state with
      | Lwt_process.Running -> read (); Lwt_unix.yield () >>= stream
      | Lwt_process.Exited status ->
          read ();
          (* tail of pipe without trailing newline *)
          Lwt_io.read process#stdout
          >>= fun s ->
          Buffer.add_string outbuf s;
          Lwt_io.read process#stderr
          >>= fun s ->
          Buffer.add_string errbuf s;
          return status
    in stream ()
    >>= fun status ->
    let r_duration = Time.(elapsed time (now ())) in
    let r = {
      r_cmd = cmd;
      r_args = args;
      r_env = env;
      r_cwd = cwd;
      r_duration;
      r_stdout = Buffer.contents outbuf;
      r_stderr = Buffer.contents errbuf;
    } in match status with
      | Unix.WEXITED 0 -> return r
      | _ -> fail (ProcessError (status,r))
  )

let run_commands ?(env=[||]) ~cwd =
  Lwt_list.map_s (run_command ~env ~cwd)

let rec update_ref_cmd = function
  | Ref r -> r, []
  | Commit (r, sha) -> r, [[ "git" ; "update-ref" ; r ; sha ]]
  | Copy (a, b) ->
      let b, cmd = update_ref_cmd b in
      a, cmd@[[ "git" ; "update-ref" ; a ; b ]]

let update_refs ~dir refs =
  run_commands ~cwd:dir (List.fold_left (fun cmds r ->
    (snd (update_ref_cmd r))@cmds
  ) [] refs)
  >>= fun _ -> return dir

let base_reference_of_diff ~dir ref_prefix diff =
  run_command ~cwd:dir ([
    "git" ; "show-branch" ; "--merge-base" ;
  ] @ (List.map (fun branch -> string_of_reference branch.reference) diff))
  >>= fun { r_stdout } ->
  return (Commit (ref_prefix^"merge-base", Util.strip r_stdout))

let clone_repo ~dir ~commit =
  let url = string_of_git commit.repo.repo_url in
  let git_ref, ref_cmd = update_ref_cmd commit.reference in
  run_commands ~cwd:dir ([
    [ "git" ; "clone" ; url ; "." ];
    [ "git" ; "config" ; "--local" ; "--add" ;
      "user.name" ; "ocamlot" ];
    [ "git" ; "config" ; "--local" ; "--add" ;
      "user.email" ; "infrastructure@lists.ocaml.org" ];
  ] @ ref_cmd @ [
    [ "git" ; "checkout" ; git_ref ];
  ])
  >>= fun _ ->
  Printf.eprintf "OCAMLOT repo clone %s\n%!" commit.label;
  return dir

let make_temp_dir ~root_dir ~prefix =
  let tmp_name = Util.make_fresh_dir ~root_dir
    ("ocamlot."^prefix^"."^Time.(date_to_string (now ()))^".") in
  Printf.eprintf "OCAMLOT temp_dir %s made\n%!" tmp_name;
  tmp_name

let fetch_refspec ~dir ~url ~refspec =
  run_command ~cwd:dir [
    "git" ; "fetch" ; string_of_git url ; refspec ;
  ]
  >>= fun _ ->
  Printf.eprintf "OCAMLOT fetch refspec %s into %s\n%!"
    refspec dir;
  return dir

let push_refspec ~dir ~url ~refspec =
  run_command ~cwd:dir [
    "git" ; "push" ; string_of_git url ; refspec ;
  ]
  >>= fun _ ->
  Printf.eprintf "OCAMLOT push refspec %s onto %s\n%!"
    refspec (string_of_git url);
  return dir

let try_merge ~dir ~base ~head =
  let ref_str = string_of_reference head.reference in
  let refspec = ref_str ^ ":" ^ ref_str in
  fetch_refspec ~dir ~url:head.repo.repo_url ~refspec
  >>= fun dir ->
  (* TODO: update-ref ? *)
  run_command ~cwd:dir [
    "git" ; "merge" ; "--no-edit" ; string_of_reference head.reference;
  ]
  >>= fun _ ->
  Printf.eprintf "OCAMLOT repo merge %s onto %s\n%!"
    head.label base.label;
  return dir

let try_collapse ~dir = function
  | [] -> return dir
  | bl -> begin
    let diffl = List.rev bl in
    let base = List.hd diffl in
    let rec merge diffl dir =
      match diffl with
      | [] -> return dir
      | head::bs -> try_merge ~dir ~base ~head >>= (merge bs)
    in
    clone_repo ~dir ~commit:base
    >>= (merge (List.tl diffl))
  end

let add ~path =
  let cwd = Filename.dirname path in
  run_command ~cwd [
    "git" ; "add" ; path ;
  ]
  >>= fun _ -> return cwd

let commit ~dir ~message =
  run_command ~cwd:dir [
    "git" ; "commit" ; "-m" ; message ;
  ]
  >>= fun _ -> return dir

let push ~dir =
  run_command ~cwd:dir [
    "git" ; "push" ;
  ]
  >>= fun _ -> return dir

let process_error site = function
  | ProcessError (Unix.WEXITED code, r) ->
      Printf.sprintf "%s\nOCAMLOT %s \"%s %s\" failed (%d) in %s\n"
        r.r_stderr site r.r_cmd (String.concat " " r.r_args) code
        (Time.duration_to_string r.r_duration), r.r_stdout
  | ProcessError (Unix.WSTOPPED signum, r)
  | ProcessError (Unix.WSIGNALED signum, r) ->
      Printf.sprintf "%s\nOCAMLOT %s \"%s %s\" terminated by signal %d in %s\n"
        r.r_stderr site r.r_cmd (String.concat " " r.r_args) signum
        (Time.duration_to_string r.r_duration), r.r_stdout
  | exn ->
      Printf.sprintf "OCAMLOT %s terminated by \"%s\"\n%s\n"
        site (Printexc.to_string exn)
        (if Printexc.backtrace_status ()
         then "Backtrace:\n"^(Printexc.get_backtrace ())
         else "No backtrace available."), ""

let die site exn =
  let err, out = process_error site exn in
  Printf.eprintf "stdout: %s\nstderr: %s\n%!" out err;
  exit 1
