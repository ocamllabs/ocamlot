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
  Re.(seq [str "github/"; group path_seg; char '/'; group path_seg])
let notify_query = "notify"
let targets = Opam_task.(Host.([
  { host = { os = Linux;
             arch = X86_64; };
    compiler = { c_version = "4.00.1";
                 c_build = ""; }; };
]))

let work_dir = Filename.(concat (get_temp_dir_name ()) "ocamlotd")
let () = OpamSystem.mkdir work_dir

let github_error_str ~user ~repo =
  sprintf "GitHub connection for %s/%s failed:" user repo

let notification_handler user repo conn_id ?body req =
  (* push, pull req, pull req comment, status *)
  let body = sprintf "Got event for %s/%s\n" user repo in
  Lwt.(Server.respond_string ~status:`OK ~body ()
       >>= S.some_response)

(* TODO: packages_of_diff task, status checking *)
let scan endpoint goal_resource = Lwt.(
  Github.(Monad.(run Github_t.(
    let {Github_hook.github; user; repo} = endpoint in
    github >> Pull.for_repo ~user ~repo ())))
  >>= Lwt_list.iter_p (fun pull ->
    let diff = Opam_repo.diff_of_pull pull in
    let prefix = string_of_int pull.Github_t.pull_number in
    Opam_repo.packages_of_diff prefix work_dir diff
    >>= fun packages ->
    List.iter (fun task ->
      ignore Ocamlot.(queue_job goal_resource (Opam task))
    ) Opam_task.(tasks_of_packages targets Build diff packages);
    return ()
  )
)

let attach listener ~user ~repo =
  let name = user^"/"^repo in
  let goal_resource = Ocamlot.make_integration_goal listener.t
    ~title:(sprintf "Test Package Repository %s" name)
    ~descr:(sprintf "The goal is to monitor and test the <a href='https://github.com/%s'>%s</a> package repository." name name)
    ~slug:("github/"^name)
  in
  let uri = Resource.uri goal_resource in Lwt.(
    Jar.get ~name
    >>= function
      | None -> (*TODO: why doesn't this exception show up? *)
          eprintf "GITHUB TOKEN MISSING: %s\n%!" name;
          fail (TokenMissing name)
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
                scan endpoint goal_resource
          )
  )

let make_listener t = {
  t; registry = Hashtbl.create 10;
}

let service {t; registry} service_fn =
  let base = Resource.uri t in
  let root = Uri.path base in
  let routes = Re.(seq [str root; notify_re]) in
  let handler conn_id ?body req = Lwt.(
    let uri = Request.uri req in
    if Uri.query uri <> [notify_query,[]] then return None
    else let path = Uri.path uri in
         try
           let endpoint = Hashtbl.find registry path in
           Github_hook.(endpoint.handler conn_id ?body req)
         with Not_found -> return None
  ) in
  let open Lwt in
  service_fn
    ~routes
    ~handler
