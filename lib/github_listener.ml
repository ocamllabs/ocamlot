open Printf

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
}

exception TokenMissing of string

let github_ua = "ocamlot <mailto:sheets@alum.mit.edu>"
let github = Github.API.set_user_agent github_ua

let path_seg = Re.(rep1 (compl [char '/']))
let notify_re =
  Re.(seq [group path_seg; char '/'; group path_seg])
let notify_query = "notify"

let github_error_str ~user ~repo =
  sprintf "GitHub connection for %s/%s failed:" user repo

let notification_handler user repo conn_id ?body req =
  let body = sprintf "Got event for %s/%s\n" user repo in
  Lwt.(Server.respond_string ~status:`OK ~body ()
       >>= S.some_response)

let attach listener ~user ~repo =
  let name = user^"/"^repo in
  let goal_resource = Ocamlot.make_integration_goal listener.t
    ~title:(sprintf "Test GitHub Repository %s" name)
    ~descr:(sprintf "The goal is to monitor and test the repository <a href='https://github.com/%s'>%s</a>." name name)
    ~slug:name
  in
  let uri = Resource.uri goal_resource in Lwt.(
    Jar.get ~name
    >>= function
      | None -> fail (TokenMissing name)
      | Some auth ->
          Github_hook.connect Github.(
            Monad.(
              github
              >> API.set_token (Token.of_string auth.Github_t.auth_token)
            ))
            listener.registry
            (Uri.resolve "" uri (Uri.of_string ("?"^notify_query)))
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
                return ()
          )
  )

let make_listener t = {
  t; registry = Hashtbl.create 10;
}

let service {t; registry} service_fn =
  let base = Resource.uri t in
  let root = Uri.path base in
  let routes = Re.(seq [str root; char '/'; notify_re]) in
  let handler conn_id ?body req = Lwt.(
    if Request.params req <> ["notify",[]] then return None
    else let path = Request.path req in
         try
           let endpoint = Hashtbl.find registry path in
           Github_hook.(endpoint.handler conn_id ?body req)
         with Not_found -> return None
  ) in
  let open Lwt in
  service_fn
    ~routes
    ~handler
