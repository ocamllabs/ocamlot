open Config
open Http_server

let gh_listener = Github_listener.make_listener
  (service "GitHub Listener")
  ~root:"github" ~host ~port
let http_server = Http_server.make_server host port
;;
Lwt_unix.run Http_server.(run (register_service http_server gh_listener))
