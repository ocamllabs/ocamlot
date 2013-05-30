(*
 * Copyright (c) 2013 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(* TODO: security audit *)

open Cohttp
module CL = Cohttp_lwt_unix
module CLB = Cohttp_lwt_body
module S = Http_server

type status = Indicated | Pending | Timeout | Unauthorized | Connected

type endpoint = {
  id: int;
  url: Uri.t;
  secret : string;
  user: string;
  repo: string;
  status : status;
  update_event : endpoint Lwt_condition.t;
  last_event : Time.t;
  github : unit Github.Monad.t;
  handler : S.response option Lwt.t S.handler;
}

let secret_prefix = "ocamlot"
let () = Random.self_init ()

let hmac secret message = Cryptokit.(
  transform_string (Hexa.encode ()) (hash_string (MAC.hmac_sha1 secret) message)
)

let verification_failure = Lwt.(
  CL.Server.respond_string
    ~status:`Forbidden
    ~body:"403: Forbidden (Request verification failure)" ()
  >>= S.some_response)

let verify_event req body secret = Lwt.(
  match Header.get (Request.headers req) "x-hub-signature" with
    | Some sign ->
        let hmac_label = Re_str.string_before sign 5 in
        let hmac_hex = Re_str.string_after sign 5 in
        if hmac_label = "sha1="
        then (CLB.string_of_body body
              >>= fun body ->
              return (hmac_hex=(hmac secret body))
        )
        else return false
    | None -> return false
)

let new_secret prefix = prefix ^ ":"
  ^ Cryptokit.(transform_string (Hexa.encode ()) (Util.randomish_string 20))

let new_hook url = Github_t.(
  let secret = new_secret secret_prefix in {
    new_hook_name="web";
    new_hook_active=true;
    new_hook_events=[`Push; `PullRequest; `PullRequestReviewComment; `Status];
    new_hook_config={
      web_hook_config_url=Uri.to_string url;
      web_hook_config_content_type="json";
      web_hook_config_insecure_ssl="1";
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
    secret; user; repo;
    handler=Lwt.(fun conn_id ?body req ->
      verify_event req body secret
      >>= function
        | true -> handler conn_id ?body req
        | false -> verification_failure
    );
    status=Indicated;
    update_event=Lwt_condition.create ();
    last_event=Time.min;
    github;
  })

let register registry endpoint = Lwt.(
  let rec handler conn_id ?body req =
    verify_event req body endpoint.secret
    >>= fun verified ->
    if verified then begin
      let last_event = Time.now () in
      let endpoint = { endpoint with last_event; status=Connected } in
      Hashtbl.replace registry (Uri.path endpoint.url) endpoint;
      Lwt_condition.broadcast endpoint.update_event endpoint;
      Printf.eprintf "SUCCESS: Hook registration of %s for %s/%s\n%!"
        (Uri.to_string endpoint.url) endpoint.user endpoint.repo;
      CL.Server.respond_string ~status:`No_content ~body:"" ()
      >>= S.some_response
    end
    else begin
      let endpoint = { endpoint with handler; status=Unauthorized } in
      Printf.eprintf "FAILURE: Hook registration of %s for %s/%s\n%!"
        (Uri.to_string endpoint.url) endpoint.user endpoint.repo;
      Hashtbl.replace registry (Uri.path endpoint.url) endpoint;
      Lwt_condition.broadcast endpoint.update_event endpoint;
      verification_failure
    end
  in
  Hashtbl.replace registry (Uri.path endpoint.url) {endpoint with handler}
)

let check_connectivity registry endpoint timeout_s = Lwt.(pick [
  Lwt_unix.sleep timeout_s
  >>= begin fun () ->
    let endpoint = {endpoint with status=Timeout} in
    Hashtbl.replace registry (Uri.path endpoint.url) endpoint;
    Lwt_condition.broadcast endpoint.update_event endpoint;
    Lwt.return ()
  end;
  let rec wait endpoint =
    Lwt_condition.wait endpoint.update_event
    >>= fun endpoint ->
    if endpoint.status=Connected then Lwt.return ()
    else wait endpoint
  in wait endpoint
])

let connect github registry url ((user,repo),handler) = Github.(Github_t.(Lwt.(
  Monad.(run (
    github >> Hook.for_repo ~user ~repo ()
    >>= fun hooks ->
    let points_to_us h =
      h.hook_config.web_hook_config_url = (Uri.to_string url)
    in List.fold_left (fun m h ->
      m >>= fun () ->
      if points_to_us h then Hook.delete ~user ~repo ~num:h.hook_id ()
      else return ()
    ) (return ()) hooks
    >>= fun () ->
    let hook = new_hook url in
    Hook.create ~user ~repo ~hook ()
    >>= fun h -> endpoint_of_hook github h user repo handler
  ))
  >>= fun endpoint ->
  register registry {endpoint with status=Pending};
  join [
    check_connectivity registry endpoint 10.;
    Github.(Monad.(run (github >> Hook.test ~user ~repo ~num:endpoint.id ())));
  ]
  >>= fun () -> return (Hashtbl.find registry (Uri.path url))
)))
