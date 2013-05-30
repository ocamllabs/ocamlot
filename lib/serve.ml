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

let watch_list = [
  "ocamlot", "opam-repository";
  "ocamlot-dev", "opam-repository";
]

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
    let gh_http_server = Http_server.register_service http_server
      Github_listener.(
        gh_event_service
          ~startup:(List.map
                      (fun (user, repo) ->
                        catch (fun () ->
                          attach gh_listener ~user ~repo
                        ) (function
                          | TokenMissing (jar,name) ->
                              eprintf "Jar path: %s\n%!"
                                (Github_cookie_jar.jar_path jar);
                              eprintf "GITHUB TOKEN MISSING: %s\nQuitting\n%!" name;
                              exit 1
                          | exn ->
                              eprintf "GH Repo Attachment Error: %s\mQuitting\n%!"
                                (Printexc.to_string exn);
                              exit 1
                        )
                      ) watch_list)) in
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
