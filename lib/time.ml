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

open Sexplib.Std

type t = float with sexp
type duration = float with sexp

let min = 0.
let now () = Unix.gettimeofday ()
let date_to_string_ tm = Unix.(
  Printf.sprintf "%d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
)
let to_string_ tm = Unix.(
  Printf.sprintf "%sT%02d:%02d:%02dZ"
    (date_to_string_ tm)
    tm.tm_hour tm.tm_min tm.tm_sec
)
let date_to_string t = date_to_string_ (Unix.gmtime t)
let duration_to_string t = Printf.sprintf "%.2fs" t
let to_string t = to_string_ (Unix.gmtime t)
let elapsed t_0 t_1 = abs_float (t_0 -. t_1)
