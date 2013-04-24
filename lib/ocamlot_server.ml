open Config

let queue = Ocamlot.make_queue ()
let worker = Ocamlot.make_worker ()

let gh_listener = Github_listener.make_listener
  (Http_server.service "GitHub Listener")
  ~root:"github" ~host ~port ~queue
let http_server = Http_server.make_server host port
;;
Lwt_unix.run (Lwt_list.iter_p (fun x -> x) [
  Http_server.(run (register_service http_server gh_listener));
  Ocamlot.(run worker);
])
