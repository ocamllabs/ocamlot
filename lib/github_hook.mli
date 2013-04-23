type status = Indicated | Unauthorized | Pending | Connected
type endpoint = {
  id : int;
  url : Uri.t;
  secret : string;
  user : string;
  repo : string;
  status : status;
  update_event : endpoint Lwt_condition.t;
  last_event : Int32.t;
  github : unit Github.Monad.t;
  handler : Http_server.response option Lwt.t Http_server.handler;
}

exception ConnectivityFailure of endpoint

val connect :
  unit Github.Monad.t ->
  (string, endpoint) Hashtbl.t ->
  Uri.t ->
  (string * string) *
  (Http_server.response option Lwt.t Http_server.handler) ->
  endpoint Lwt.t
