open Printf

open Cohttp

module CL = Cohttp_lwt_unix
module CLB = CL.Body
module Req = CL.Request

type response = CL.Response.t * CLB.t
type 'a handler = int -> ?body:CLB.contents -> Req.t -> 'a
type service = {
  name : string;
  routes : Re.t;
  handler : response option Lwt.t handler;
  startup : unit Lwt.t list;
}
type service_search = { service : service; continue : unit -> service_search }
type t = {
  host : string;
  port : int;
  services : service list;
  dispatch : service_search handler;
}

let some_response resp = Lwt.return (Some resp)

let not_found_service = {
  name="Default404";
  routes=Re.any;
  handler=Lwt.(fun conn_id ?body req ->
    let body = sprintf "404: Resource '%s' not found\n" (Req.path req) in
    CL.Server.respond_string ~status:`Not_found ~body ()
    >>= some_response
  );
  startup=[];
}

let service name ~routes ~handler ~startup =
  { name; routes; handler; startup }

let make_dispatch services =
  let routes = List.map (fun s ->
    Re.(compile (seq [char '/'; s.routes])), s
  ) services in
  fun conn_id ?body req ->
    let rec cascade = function
      | [] -> let rec fix =
                {service=not_found_service; continue=fun () -> fix} in fix
      | (rt,service)::rest ->
          if Re.execp rt (Req.path req)
          then {service; continue=fun () -> cascade rest}
          else cascade rest
    in cascade routes

let make_server host port =
  { host; port; services=[]; dispatch=make_dispatch [] }

let register_service server service =
  let services = service::server.services in
  let dispatch = make_dispatch services in
  { server with services; dispatch }

(*    
    match path with
      | "/" -> Pages.index registry
      | path -> begin
      end
*)
  
let run server =
  let port = server.port in
  let rec callback kfn conn_id ?body req =
    let {service; continue} = kfn conn_id ?body req in
    let pathquery = Uri.path_and_query (Req.uri req) in
    let () = eprintf "%s for %s dispatched to %s\n%!"
      (Code.string_of_method (Req.meth req)) pathquery service.name in
    Lwt.(service.handler conn_id ?body req
         >>= function
           | None -> eprintf "%s refused to service %s: continuing\n%!"
               service.name pathquery;
               callback (fun _ ?body _ -> continue ()) conn_id ?body req
           | Some resp -> return resp)
  in
  let conn_closed conn_id () =
    eprintf "conn %s closed\n%!" (CL.Server.string_of_conn_id conn_id)
  in
  let config = { CL.Server.callback=callback server.dispatch; conn_closed } in
  let startup_delay = Lwt_unix.sleep 0.2 in
  Lwt_list.iter_p (fun x -> x) Lwt.(
    (CL.server ~address:"0.0.0.0" ~port config)
    ::(List.rev_map (fun service ->
      startup_delay
      >>= fun () -> Lwt_list.iter_p (fun x -> x) service.startup
    ) server.services)
  )

