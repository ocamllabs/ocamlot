type t

exception TokenMissing of (Github_cookie_jar.t * string)

val github : unit Github.Monad.t

val make_listener : Ocamlot.t_resource -> t Lwt.t

val attach : t -> user:string -> repo:string -> unit Lwt.t

val service :
  t ->
  (routes:Re.t ->
   handler:Http_server.response option Lwt.t Http_server.handler -> 'a) -> 'a
