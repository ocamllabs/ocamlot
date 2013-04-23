open Printf

open Cohttp
open Cohttp_lwt_unix

module S = Http_server

type user = string
type repo = string
type repo_id = user * repo

let github_ua = "ocamlot <mailto:sheets@alum.mit.edu>"
let github = Github.API.set_user_agent github_ua

let path_seg = Re.(rep1 (compl [char '/']))
let notify_re =
  Re.(seq [group path_seg; char '/'; group path_seg])
let notify_uriref (user, repo) = sprintf "%s/%s?notify" user repo

let watch_list = ["ocamlot","opam-repository"]
let registry = Hashtbl.create 10

let make_listener service_fn root host port =
  let base = Uri.make ~scheme:"http" ~host ~port ~path:("/"^root^"/") () in
  let routes = Re.(seq [str root; char '/'; notify_re]) in
  let path_match = Re.compile routes in
  let path_repo_id req =
    let path = Request.path req in
    let params = Re.(get_all (exec path_match path)) in
    let user = params.(0) in
    let repo = params.(1) in
    path, (user, repo)
  in
  let notification_handler conn_id ?body req =
    let path, (user, repo) = path_repo_id req in
    let body = sprintf "Got event for %s/%s\n" user repo in
    Lwt.(Server.respond_string ~status:`OK ~body ()
         >>= S.some_response)
  in
  let handler conn_id ?body req = Lwt.(
    if Request.params req <> ["notify",[]] then return None
    else let path = Request.path req in
         try
           let endpoint = Hashtbl.find registry path in
           Github_hook.(endpoint.handler conn_id ?body req)
         with Not_found -> return None
  ) in
  let token_kp = ["github"; "token"] in
  let open Lwt in
  service_fn
    ~routes
    ~handler
    ~startup:(List.map (fun watch ->
      catch begin
        fun () -> Persist.get ~access:`Private token_kp
        >>= begin function
          | Some (_, _, token) -> return token
          | None -> (Github.(Monad.(run (
            github
            >> Token.create
              ~note:github_ua ~note_url:(Uri.to_string base)
              ~client_id:Config.github_app_id
              ~client_secret:Config.github_app_secret
              ~user:Config.github_user
              ~pass:Config.github_pass ()
            >>= fun auth -> return (Token.of_auth auth))))
          ) >>= fun token ->
              Persist.put token_kp token
            >>= fun _ -> return token
        end
        >>= fun token ->
          let github = Github.(Monad.(github >> API.set_token token)) in
          let url = Uri.resolve "http" base
            (Uri.of_string (notify_uriref watch)) in
          Github_hook.connect github registry url (watch,notification_handler)
        >>= fun endpoint -> return () (* TODO: SYNC *)
        end
        begin fun exn ->
          eprintf "GitHub connection failed: %s\n%!" (Printexc.to_string exn);
          return ()
        end
    ) watch_list)
