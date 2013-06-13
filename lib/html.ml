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

open Printf

open Cohttp
open Github_hook

open Cohttp_lwt_unix

let html_status href = function
  | Indicated -> "<span style='color: grey'>indicated</span>"
  | Unauthorized -> "<a href='"^href^"/authorize' style='color: red'>authorize</a>"
  | Pending -> "<span style='color: yellow'>pending</span>"
  | Connected -> "<span style='color: green'>connected</span>"

let index registry =
  let body = Body.body_of_string_list Github_hook.([
    "<!DOCTYPE html><html><head><title>ocamlot</title></head><body>";
    "<h1>ocamlot : OCaml Online Testing</h1>";
    "<table>";
  ]@(
    Hashtbl.fold (fun k v a ->
      let href = v.user ^ "/" ^ v.repo in
      (sprintf "<tr><td><a href=\"%s\">%s / %s</a></td><td>%s</td></tr>"
         href v.user v.repo
         (html_status href v.status)
      )::a
    ) registry []
  )@[
    "</table>";
    "</body></html>";
  ]) in
  Server.respond ~status:`OK ~body ()
