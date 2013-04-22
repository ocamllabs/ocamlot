(* TODO: security audit *)

open Cohttp
module CL = Cohttp_lwt_unix
module CLB = CL.Body

type status = Indicated | Unauthorized | Pending | Connected

type endpoint = {
  id: int;
  url: Uri.t;
  secret : string;
  user: string;
  repo: string;
  status : status;
  last_event : Int32.t;
  github : unit Github.Monad.t;
  handler : int -> ?body:CLB.contents -> CL.Request.t -> (CL.Response.t * CLB.t) Lwt.t
}

exception VerificationFailure of endpoint
exception ConnectivityFailure of endpoint

let secret_prefix = "ocamlot"

let hmac secret message = Cryptokit.(
  transform_string (Hexa.encode ()) (hash_string (MAC.hmac_sha1 secret) message)
)

let verify_event req body endpoint = Lwt.(
  match CL.Request.header req "x-hub-signature" with
    | Some sign ->
        let hmac_label = Re_str.string_before sign 5 in
        let hmac_hex = Re_str.string_after sign 5 in
        if hmac_label = "sha1="
        then (CLB.string_of_body body
              >>= fun body ->
              return (hmac_hex=(hmac (secret_prefix^":"^endpoint.secret) body))
        )
        else return false
    | None -> return false
)

let new_secret prefix = prefix ^ ":"
  ^ Cryptokit.(transform_string (Hexa.encode ()) Random.(string secure_rng 20))

let new_hook url = Github_t.(
  let secret = new_secret secret_prefix in {
    new_hook_name="web";
    new_hook_active=true;
    new_hook_events=[`PullRequest; `PullRequestReviewComment; `Status];
    new_hook_config={
      web_hook_config_url=Uri.to_string url;
      web_hook_config_content_type="json";
      web_hook_config_insecure_ssl="false";
      web_hook_config_secret=Some secret;
    };
  })

let endpoint_of_hook github hook user repo handler = Github_t.(
  let secret = match hook.hook_config.web_hook_config_secret with
    | None -> ""
    | Some secret -> secret
  in Github.Monad.return {
    id=hook.hook_id;
    url=Uri.of_string hook.hook_config.web_hook_config_url;
    secret; user; repo; handler;
    status=Indicated;
    last_event=Int32.zero;
    github;
  })

let register registry endpoint = Lwt.(
  let handler conn_id ?body req =
    verify_event req body endpoint
    >>= fun verified ->
    if verified then begin
      Github.Monad.run (Time.mono_msec ())
      >>= fun last_event ->
      Hashtbl.replace registry (Uri.path endpoint.url)
        {endpoint with last_event; status=Connected}; (* handler is closed *)
      Printf.eprintf "SUCCESS: Hook registration of %s for %s/%s\n%!"
        (Uri.to_string endpoint.url) endpoint.user endpoint.repo;
      CL.Server.respond_string ~status:`No_content ~body:"" ()
    end
    else begin
      Printf.eprintf "FAILURE: Hook registration of %s for %s/%s\n%!"
        (Uri.to_string endpoint.url) endpoint.user endpoint.repo;
      Hashtbl.replace registry (Uri.path endpoint.url)
        {endpoint with status=Unauthorized};
      Lwt.fail (VerificationFailure endpoint)
    end
  in
  Hashtbl.replace registry (Uri.path endpoint.url) {endpoint with handler}
)

let rec check_connectivity registry path k () =
  let endpoint = Hashtbl.find registry path in
  if k < 1 then begin
    Hashtbl.replace registry path {endpoint with status=Unauthorized};
    Lwt.fail (ConnectivityFailure endpoint)
  end else if endpoint.status=Connected
    then Lwt.(return ())
    else Lwt.((Lwt_unix.sleep 0.1)
              >>= (check_connectivity registry path (k-1)))

let connect github registry url ((user,repo),handler) = Github.(Github_t.(Lwt.(
  Monad.(run (
    github >> Hook.for_repo ~user ~repo ()
    >>= fun hooks ->
    let points_to_us h =
      h.hook_config.web_hook_config_url = (Uri.to_string url)
    in begin match List.filter points_to_us hooks with
      | h::_ -> endpoint_of_hook github h user repo handler
      | [] -> let hook = new_hook url in
              Hook.create ~user ~repo ~hook ()
              >>= fun h -> endpoint_of_hook github h user repo handler
    end
    >>= fun endpoint ->
    register registry {endpoint with status=Pending};
    Hook.test ~user ~repo ~num:endpoint.id ()
    >>= fun () -> return endpoint
  ))
  >>= fun endpoint -> check_connectivity registry (Uri.path endpoint.url) 5 ()
)))
