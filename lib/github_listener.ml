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

open Cohttp
open Cohttp_lwt_unix

module S = Http_server
module Jar = Github_cookie_jar

type user = string
type repo = string
type repo_id = user * repo

type t = {
  t : Ocamlot.t_resource;
  registry : (string, Github_hook.endpoint) Hashtbl.t;
  jar : Jar.t;
}

exception TokenMissing of (Jar.t * string)

let github_ua = "ocamlot <mailto:sheets@alum.mit.edu>"
let github = Github.API.set_user_agent github_ua

let path_seg = Re.(rep1 (compl [char '/']))
let notify_re =
  Re.(seq [str "github/"; group path_seg; char '/'; group path_seg])
let notify_query = "notify"

let work_dir = Filename.(concat (get_temp_dir_name ()) "ocamlotd")
let () = Util.mkdir_p work_dir 0o700

let github_error_str ~user ~repo =
  sprintf "GitHub connection for %s/%s failed:" user repo

let notification_handler user repo conn_id ?body req =
  (* push, pull req, pull req comment, status *)
  let body = sprintf "Got event for %s/%s\n" user repo in
  Server.respond_string ~status:`OK ~body ()
  >>= S.some_response

(* TODO: packages_of_diff worker task, commit status checking *)
let scan targets endpoint gh_repo_resource =
  let {Github_hook.github; user; repo} = endpoint in
  Github.(Monad.(run Github_t.(
    github >> Pull.for_repo ~user ~repo ())))
  >>= Lwt_list.iter_p (fun pull ->
    let diff = Opam_repo.diff_of_pull pull in
    let pull_number = pull.Github_t.pull_number in
    let prefix = string_of_int pull_number in
    let gh_repo_goal = Resource.content gh_repo_resource in
    let pull_goal = Goal.make_pull gh_repo_resource
      ~title:(sprintf "%s Pull Request %d" gh_repo_goal.Ocamlot.title pull_number)
      ~descr:(sprintf "Check %s <a href='%s'>pull request %d</a>."
                gh_repo_goal.Ocamlot.slug pull.Github_t.pull_html_url pull_number)
      ~slug:("pull/"^prefix)
    in
    catch (fun () ->
      Opam_repo.packages_of_diff prefix work_dir diff
      >>= fun packages ->
      List.iter (Printf.eprintf "PACKAGE %s\n%!") packages;
      List.iter (fun task ->
        let href = Printf.sprintf
          "https://github.com/%s/%s/blob/master/packages/%s/opam"
          user repo (List.hd packages)
        in ignore Ocamlot.(queue_job
                             pull_goal
                             (Opam task)
                             (Uri.of_string href))
      ) Opam_task.(tasks_of_packages targets Build diff packages);
      return ()
    ) (Repo.die (Printf.sprintf "Github_listener.scan \"%s\""
                   (Uri.to_string (Resource.uri pull_goal))))
  )

let attach listener ~user ~repo targets gh_repo_resource =
  let name = user^"/"^repo in
  let uri = Resource.uri gh_repo_resource in
  Jar.get listener.jar ~name
  >>= function
    | None -> fail (TokenMissing (listener.jar, name))
    | Some auth ->
        let github = Github.(Monad.(
          github >> API.set_token (Token.of_string auth.Github_t.auth_token)))
        in
        let callback_url = Uri.resolve "" uri
          (Uri.of_string ("?"^notify_query)) in
        eprintf "Connecting GitHub to callback %s\n%!"
          (Uri.to_string callback_url);
        Github_hook.connect github
          listener.registry
          callback_url
          ((user,repo), notification_handler user repo)
        >>= fun endpoint -> Github_hook.(match endpoint with
          | {status=Indicated} ->
              eprintf "%s wedged prerequest\n%!"
                (github_error_str ~user ~repo);
              return ()
          | {status=Pending} ->
              eprintf "%s wedged pending\n%!"
                (github_error_str ~user ~repo);
              return ()
          | {status=Timeout} ->
              eprintf "%s handshake timeout\n%!"
                (github_error_str ~user ~repo);
              return ()
          | {status=Unauthorized} ->
              eprintf "%s authorization failed\n%!"
                (github_error_str ~user ~repo);
              return ()
          | {status=Connected} ->
              scan targets endpoint gh_repo_resource
        )

let make_listener t =
  Jar.init ()
  >>= fun jar -> return {
    t; registry = Hashtbl.create 10; jar;
  }

let service {t; registry} service_fn =
  let base = Resource.uri t in
  let root = Uri.path base in
  let routes = Re.(seq [str root; notify_re]) in
  let handler conn_id ?body req =
    let uri = Request.uri req in
    if Uri.query uri <> [notify_query,[]] then return None
    else let path = Uri.path uri in
         try
           let endpoint = Hashtbl.find registry path in
           Github_hook.(endpoint.handler conn_id ?body req)
         with Not_found -> return None
  in
  service_fn
    ~routes
    ~handler
