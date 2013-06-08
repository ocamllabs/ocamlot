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

type output = {
  err : string;
  out : string;
  info: string;
} with sexp

type status =
  | Passed
  | Failed
with sexp

type t = {
  status : status;
  duration : Time.duration;
  output : output;
} with sexp

let string_of_status = function
  | Passed -> "PASSED"
  | Failed -> "FAILED"

let get_status {status} = status

let to_html { status; duration; output } =
  let status_class = match status with
    | Passed -> "passed"
    | Failed -> "failed"
  in
  Printf.sprintf "<div class='summary'><span class='status %s'>%s</span> in <span class='duration'>%s</span></div>\n<span>stderr</span><pre class='stderr'>%s</pre>\n<span>stdout</span><pre class='stdout'>%s</pre>\n<span>environment</span><pre class='info'>%s</pre>"
    status_class (string_of_status status) (Time.duration_to_string duration)
    output.err
    output.out
    output.info
