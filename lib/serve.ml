let watch_list = [
  "ocamlot", "opam-repository";
  "ocamlot-dev", "opam-repository";
]

let daemon base port =
  let host = match Uri.host base with None -> "" | Some host -> host in
  let ocamlot = Ocamlot.make ~base in

  let browser_listener = Ocamlot.browser_listener
    (Http_server.service "Browser Request Listener")
    ~base ocamlot in

  let worker_listener = Ocamlot.worker_listener
    (Http_server.service "Worker Task Queue Listener")
    ~base ocamlot in

  let gh_listener = Github_listener.make_listener ocamlot in
  let gh_event_service = Github_listener.service gh_listener
    (Http_server.service "GitHub Listener") in

  let http_server = Http_server.make_server host port in
  let gh_http_server = Http_server.register_service http_server
    Github_listener.(gh_event_service
                       ~startup:(List.map
                                   (fun (user, repo) ->
                                     attach gh_listener ~user ~repo)
                                   watch_list)) in
  let gh_event_server = Http_server.register_service
    gh_http_server browser_listener in
  let ocamlot_server = Http_server.register_service
    gh_event_server worker_listener in

  Lwt_daemon.daemonize
    ~stdout:`Keep ~stderr:`Keep
    ~directory:(Unix.getcwd ())
    ();
  Lwt_unix.run (Lwt_list.iter_p (fun x -> x) [
    Http_server.(run ocamlot_server);
  ])
