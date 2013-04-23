type user = string
type repo = string
type repo_id = user * repo

val github : unit Github.Monad.t
val watch_list : repo_id list
val make_listener :
  (routes:Re.t ->
   handler:Http_server.response option Lwt.t Http_server.handler ->
   startup:unit Lwt.t list -> 'a) ->
  root:string -> host:string -> port:int -> 'a
