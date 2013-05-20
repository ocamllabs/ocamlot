open Config

let watch_list = ["ocamlot", "opam-repository"]

let base = Uri.make ~scheme:"http" ~host ~port ~path:"/" ()

let ocamlot = Ocamlot.make ~base

let browser_listener = Ocamlot.browser_listener
  (Http_server.service "Browser Request Listener")
  ~root:"/" ~base ocamlot

let worker_listener = Ocamlot.worker_listener
  (Http_server.service "Worker Task Queue Listener")
  ~root:"/" ~host ~port ocamlot

let gh_listener = Github_listener.make_listener ocamlot
let gh_event_service = Github_listener.service gh_listener
  (Http_server.service "GitHub Listener")

let http_server = Http_server.make_server host port
let gh_http_server = Http_server.register_service http_server
  Github_listener.(gh_event_service
                     ~startup:(List.map
                                 (fun (user, repo) ->
                                   attach gh_listener ~user ~repo)
                                 watch_list))
let gh_event_server = Http_server.register_service
  gh_http_server browser_listener
let ocamlot_server = Http_server.register_service
  gh_event_server worker_listener
;;
Lwt_unix.run (Lwt_list.iter_p (fun x -> x) [
  Http_server.(run ocamlot_server);
])
