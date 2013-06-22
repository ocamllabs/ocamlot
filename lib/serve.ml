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

open Printf
open Lwt

type policy = {
  listen : bool;
  comprehensive : bool;
  targets : Opam_task.target list;
}

let targets = Opam_task.(Host.([
(*  { host = { os = Linux;
             arch = X86_64; };
    compiler = { c_version = "3.12.1";
                 c_build = ""; }; }; *)
  { host = { os = Linux;
             arch = X86_64; };
    compiler = { c_version = "4.00.1";
                 c_build = ""; }; };
  { host = { os = Linux;
             arch = X86_64; };
    compiler = { c_version = "4.01.0";
                 c_build = ""; }; };
  { host = { os = Linux;
             arch = X86_64; };
    compiler = { c_version = "4.02.0";
                 c_build = ""; }; };
]))

let watch_list = [
(*  "ocamlot", "opam-repository"; *)
(*  ("ocamlot-dev", "opam-repository"), {
    listen=true; comprehensive=false; targets; }; *)
  ("OCamlPro", "opam-repository"), {
    listen=false; comprehensive=true; targets; };
]

let state_path = Filename.concat (Unix.getcwd ()) Config.state_path

let forever base port =
  let host = match Uri.host base with None -> "" | Some host -> host in
  let ocamlot = Ocamlot.make ~base in

  let browser_listener = Ocamlot.browser_listener
    (Http_server.service "Browser Request Listener")
    ~base ocamlot in

  let worker_listener = Ocamlot.worker_listener
    (Http_server.service "Worker Task Queue Listener")
    ~base ocamlot in

  let ocamlot_server_later =
    Github_listener.make_listener ocamlot
    >>= fun gh_listener ->
    let gh_event_service = Github_listener.service gh_listener
      (Http_server.service "GitHub Listener") in

    let http_server = Http_server.make_server host port in

    Lwt_list.map_p (fun ((user, repo), policy) ->
      let name = user^"/"^repo in
      let slug = "github/"^name in
      let goal_state_path = Filename.concat state_path slug in

      Goal.read_tasks goal_state_path
      >>= fun repo_trs ->
      let descr = <:html<
        The goal is to monitor and test the
        <a href="$str:"https://github.com/"^name$">$str:name$</a>
        package repository.
      >> in
      let resource = Goal.make_integration ocamlot
        ~title:slug ~descr ~slug
        ~min_id:(Goal.max_task_record_id repo_trs)
        ~goal_state_path
      in

      let goal = Resource.content resource in
      let base = Goal.subresource_base (Resource.uri resource) in

      (if policy.comprehensive
       then Goal.missing_tasks user repo policy.targets repo_trs
       else return [])
      >>= fun tasks ->
      List.iter (function
        | { Opam_task.packages = pkg::_ } as task ->
            let href = Printf.sprintf
              "https://github.com/%s/blob/master/packages/%s/opam"
              name pkg
            in ignore (Ocamlot.queue_job
                         resource
                         (Ocamlot.Opam task)
                         (Uri.of_string href))
        | _ -> ()
      ) tasks;

      List.iter (fun (tid,task) ->
        let uri = Uri.resolve "" base
          (Uri.of_string (Filename.concat Goal.task_subpath tid)) in
        let tr = Resource.create uri (task,()) (fun t _ -> t)
          (Ocamlot.task_renderer resource) in
        Resource.archive goal.Ocamlot.completed fst tr;
        Ocamlot.register_resource (Resource.content ocamlot) tr;
      ) repo_trs;

      return ((user, repo), policy, resource)
    ) watch_list
    >>= fun watch_list ->

    let startup = List.map
      (fun ((user, repo), policy, resource) ->
        catch (fun () ->
          if policy.listen
          then Github_listener.attach gh_listener ~user ~repo
            policy.targets resource
          else return ()
        ) (function
          | Github_listener.TokenMissing (jar,name) ->
              eprintf "Jar path: %s\n%!"
                (Github_cookie_jar.jar_path jar);
              eprintf "GITHUB TOKEN MISSING: %s\nQuitting\n%!" name;
              exit 1
          | exn ->
              eprintf "GH Repo Attachment Error: %s\nQuitting\n%!"
                (Printexc.to_string exn);
              exit 1
        )
      ) watch_list in

    let gh_http_server = Http_server.register_service http_server
      (gh_event_service ~startup) in
    let gh_event_server = Http_server.register_service
      gh_http_server browser_listener in
    return (Http_server.register_service
              gh_event_server worker_listener)
  in

  (* TODO: This causes Github Cookie Jar reads to fail after attempting to
           deserialize an empty string rather than the cookie file contents.

  let directory = Unix.getcwd () in
  eprintf "Daemonizing in %s\n%!" directory;
  Lwt_daemon.daemonize
    ~stdout:`Keep ~stderr:`Keep ~directory
    ();*)
  Lwt_unix.run begin
    ocamlot_server_later
    >>= fun ocamlot_server ->
    Http_server.(run ocamlot_server)
  end
