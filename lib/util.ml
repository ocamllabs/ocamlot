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

let randomish_string k =
  let buf = String.make k '\000' in
  let rec gen x =
    if k=x then buf
    else (buf.[x] <- Char.chr (Random.int 0x100); gen (x+1))
  in gen 0

let hex_str_of_string s =
  let len = String.length s in
  let hex = String.create (len*2) in
  let rec loop i =
    if i = len then hex
    else
      let x = int_of_char s.[i] in
      let j = i*2 in
      hex.[j] <- (Printf.sprintf "%x" ((0xF0 land x) lsr 4)).[0];
      hex.[j+1] <- (Printf.sprintf "%x" (0x0F land x)).[0];
      loop (i+1)
  in loop 0

let rec make_fresh_dir ?k ?root_dir prefix =
  let root_dir = match root_dir with
    | Some s -> s
    | None -> Filename.get_temp_dir_name ()
  in
  let k = match k with None -> 0 | Some k -> k in
  try
    let name = Filename.concat root_dir (prefix^(string_of_int k)) in
    Unix.mkdir name 0o700;
    name
  with Unix.Unix_error (Unix.EEXIST, "mkdir", _) ->
    make_fresh_dir ~k:(k+1) ~root_dir prefix

let rec mkdir_p dir perm =
  if not (Sys.file_exists dir)
  then begin
    mkdir_p (Filename.dirname dir) perm;
    Unix.mkdir dir perm
  end

let ltws = Re.(compile (seq [
  bos ; rep space ; group (non_greedy (rep any)) ; rep space ; eos
]))
let strip s = Re.(get (exec ltws s) 1)
