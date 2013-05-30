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

type access = [ `Public | `Private ]

type status = [ `Created | `Updated | `Presented ]
type key_path = string list
type 'a resource = status * key_path * 'a

let public_path = Config.public_store_path
let private_path = Config.private_store_path
let public_perm = 0o664
let private_perm = 0o600

let root = function
  | `Public -> public_path
  | `Private -> private_path

let perm = function
  | `Public -> public_perm
  | `Private -> private_perm

let resource_path key_path access = Lwt.(
  let dir = List.(rev (tl (rev key_path))) in
  Lwt_list.fold_left_s (fun p d ->
    let kp = p^d^"/" in
    catch (fun () ->
      Lwt_unix.mkdir ((root access)^"/"^kp) ((perm access) lor 0o111)
      >>= fun () -> return kp
    ) (function
      | Unix.Unix_error (Unix.EEXIST, _, _) -> return kp
      | exn -> fail exn
    )) "" dir
  >>= fun kp -> return ((root access)^"/"^kp^(List.(hd (rev key_path)))))

(* local, idempotent, provenance-tracking *)
let put ?(access=`Private) key_path value =
  let path = resource_path key_path access in
  let perm = perm access in
  Lwt.(Lwt_unix.(
    let flags = [O_RDWR; O_CREAT] in
    path >>= fun path ->
    openfile path flags perm
    >>= fun descr ->
    let ic = Lwt_io.(of_fd ~mode:input descr) in
    Lwt_io.length ic
    >>= fun len ->
    (if len = Int64.zero
     then return []
     else Lwt_io.read_value ic)
    >>= fun prov ->
    let oc = Lwt_io.(of_fd ~mode:output descr) in
    match prov with
      | [] ->
          Lwt_io.write_value oc [value]
          >>= fun () -> Lwt_io.close oc
          >>= fun () -> return (`Created, key_path, value)
      | hd::tl when value = hd -> return (`Presented, key_path, value)
      | hd::tl ->
          Lwt_io.write_value oc (value::hd::tl)
          >>= fun () -> Lwt_io.close oc
          >>= fun () -> return (`Updated, key_path, value)
  ))

let get ?(access=`Public) key_path =
  let path = resource_path key_path access in
  let perm = perm access in
  Lwt.(Lwt_unix.(
    let flags = [O_RDONLY] in
    path >>= fun path ->
    catch (fun () ->
      openfile path flags perm
      >>= fun descr ->
      let ic = Lwt_io.(of_fd ~mode:input descr) in
      Lwt_io.read_value ic
      >>= fun prov -> Lwt_io.close ic
      >>= fun () -> return (Some (`Presented, key_path, List.hd prov))
    ) (function
      | Unix.Unix_error(Unix.ENOENT, _, _) -> return None
      | exn -> fail exn
    )
  ))

(*
let post ?(access=`Private) key_path value = Lwt_unix.(
  let flags = [O_RDWR; O_EXCL] in
  openfile (resource_path key_path access)
)
*)
(*
let delete ~access key_path = Lwt_unix.(
  let flags = [O_WRONLY; O_TRUNC] in
  openfile (resource_path key_path access)
)
*)
(*
let get_prov ?(access=`Public) key_path = Lwt_unix.(
  let flags = [] in
  openfile (resource_path key_path access)
)
*)
