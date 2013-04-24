type endpoint_type = Github of Github_hook.endpoint
type repo = {
  url : Uri.t;
  git_url : Uri.t;
  tasks : Task.t list;
  endpoint : endpoint_type;
}
type queue = unit
type worker = unit

let make_queue () = ()

let make_worker () = ()

let run () = Lwt.return ()
