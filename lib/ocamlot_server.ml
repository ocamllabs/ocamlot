open Config

let base = Uri.make ~scheme:"http" ~host ~port ()

let ocamlot = Ocamlot.make ~base

(*
let browser_listener = Ocamlot.browser_listener
  (Http_server.service "Browser State Listener")
  ~root:"" ~host ~port ocamlot
*)
let worker_listener = Ocamlot.worker_listener
  (Http_server.service "Worker Task Queue Listener")
  ~root:"" ~host ~port ocamlot

let gh_listener = Github_listener.make_listener
  (Http_server.service "GitHub Listener")
  ~root:"github" ~host ~port ocamlot
let http_server = Http_server.make_server host port
let gh_http_server = Http_server.register_service http_server gh_listener
(*let gh_event_server = Http_server.register_service http_server browser_listener*)
let ocamlot_server = Http_server.register_service http_server worker_listener
;;
Lwt_unix.run (Lwt_list.iter_p (fun x -> x) [
  Http_server.(run ocamlot_server);
])
