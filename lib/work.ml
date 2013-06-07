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

(* This module implements the client projection of the worker protocol *)
open Lwt
open Cohttp

module Body = Cohttp_lwt_body
module Client = Cohttp_lwt_unix.Client
module Response = Cohttp_lwt_unix.Response

type env = { headers : Header.t ; work_dir : string }

exception ProtocolError of Ocamlot.worker_message

let serialize sexp = Body.body_of_string (Sexplib.Sexp.to_string sexp)
let message mesg = serialize (Ocamlot.sexp_of_worker_message mesg)

let print_result (Ocamlot.Opam task) = Result.(function
  | { status=Failed; duration; output } ->
      Printf.eprintf "%s\n%!" output.err;
      Printf.eprintf "OCAMLOT %s FAILED in %s\n%!"
        (Opam_task.to_string task)
        (Time.duration_to_string duration)
  | { status=Passed; duration; output } ->
      Printf.eprintf "OCAMLOT %s PASSED in %s\n%!"
        (Opam_task.to_string task)
        (Time.duration_to_string duration)
)

let execute ~jobs prefix work_dir = function
  | Ocamlot.Opam opam_task ->
      Opam_task.run ~jobs prefix work_dir opam_task

let complete_task ~continue ~env uri task result =
  let body = message (Ocamlot.Complete result) in
  let headers = env.headers in
  Client.post ~headers ?body uri
  >>= function
    | Some (resp,_) ->
        let status = Response.status resp in
        if status = `No_content
        then continue ~env
        else begin
          Printf.eprintf "OCAMLOT worker didn't get Completion response: %s; quitting\n"
          (Code.string_of_status status);
          return ()
        end
    | None ->
        Printf.eprintf "OCAMLOT worker didn't get Completion response\n";
        return ()

let rec check_in_task ~env uri =
  Lwt_unix.sleep (0.8*.Ocamlot.worker_timeout)
  >>= fun () ->
  Printf.eprintf "OCAMLOT CHECKIN\n%!";
  let body = message Ocamlot.Check_in in
  let headers = env.headers in
  Client.post ~headers ?body uri
  >>= function
    | Some (resp, _) ->
        let status = Response.status resp in
        if status = `No_content
        then check_in_task ~env uri
        else begin
          Printf.eprintf "OCAMLOT worker didn't get Check-in confirmation: %s; quitting\n"
            (Code.string_of_status status);
          fail (ProtocolError Ocamlot.Check_in)
        end
    | None ->
        Printf.eprintf "OCAMLOT worker didn't get Check-in response\n";
        fail (ProtocolError Ocamlot.Check_in)

let execute_task ~continue ~env uri task =
  pick [
    check_in_task ~env uri;
    execute ~jobs:3 "work" env.work_dir task;
  ]
  >>= complete_task ~continue ~env uri task

let accept_task_offer ~continue ~env (uri,task) =
  let body = message Ocamlot.Accept in
  let headers = env.headers in
  Client.post ~headers ?body uri
  >>= function
    | Some (resp, _) ->
        let status = Response.status resp in
        if status = `No_content
        then execute_task ~continue ~env uri task
        else begin
          Printf.eprintf "OCAMLOT worker didn't get Acceptance confirmation: %s; quitting\n"
            (Code.string_of_status status);
          return () end
    | None -> (* TODO: connection closed without response? *)
        Printf.eprintf "OCAMLOT worker didn't get Acceptance response\n";
        return ()

let request_task ~continue ~env worker_env uri =
  let body = serialize (Ocamlot.sexp_of_worker_env worker_env) in
  let headers = env.headers in
  Client.post ~headers ?body uri
  >>= function
    | Some (resp, body) ->
        (* TODO: check response validity *)
        Body.string_of_body body
        >>= fun s ->
        Printf.eprintf "%s\n%!" s;
        let sexp = Sexplib.Sexp.of_string s in
        let task_offer = Ocamlot.task_offer_of_sexp sexp in
        let resphdrs = Response.headers resp in
        let cookies = Cookie.Set_cookie_hdr.extract resphdrs in
        let headers = if List.mem_assoc Ocamlot.worker_id_cookie cookies
          then Header.of_list [
            Cookie.Cookie_hdr.serialize
              (List.map (fun (_,c) -> Cookie.Set_cookie_hdr.binding c) cookies)
          ]
          else headers in
        accept_task_offer ~continue ~env:{env with headers} task_offer
    | None -> (* TODO: connection closed without response? *)
        Printf.eprintf "OCAMLOT worker didn't get a response: quitting\n";
        return ()

let forever work_dir ocaml_dir uri =
  let url = Uri.resolve "" uri (Uri.of_string "?queue") in
  let host = Host.detect () in
  Opam_task.list_compilers ocaml_dir ""
  >>= fun compilers ->
  let rec work ~env =
    request_task ~continue:work ~env (host, (List.map fst compilers)) url
  in work { headers = Header.init (); work_dir }
