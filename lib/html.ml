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

open Cow
module HTML = Cow.Html

let style = Uri.of_string "//netdna.bootstrapcdn.com/bootswatch/2.3.1/flatly/bootstrap.min.css"

let site_title = "ocamlot"

module Table = struct
  type 'a t = {
    rc_idx : (string, (string, 'a) Hashtbl.t) Hashtbl.t;
    sortfn : (string * 'a list list) -> (string * 'a list list) -> int;
  }

  let create ?(sortfn=fun (x,_) (y,_) -> String.compare x y) rowfn colfn els =
    let rc_idx = Hashtbl.create 10 in
    List.iter
      (fun el -> match rowfn el with
        | Some row ->
            let cols =
              try Hashtbl.find rc_idx row
              with Not_found -> Hashtbl.create 10
            in
            Hashtbl.replace rc_idx row cols;
            Hashtbl.add cols (colfn el) el
        | None -> ()
      ) els;
    { rc_idx; sortfn; }

  let cell_count tbl = Hashtbl.fold
    (fun _ v a ->
      a + (Hashtbl.length v)
    ) tbl.rc_idx 0

  let columns tbl =
    let col_set = Hashtbl.create 10 in
    Hashtbl.iter (fun _ ct ->
      Hashtbl.iter (fun c _ -> Hashtbl.replace col_set c ()) ct
    ) tbl.rc_idx;
    Hashtbl.fold (fun c () l -> c::l) col_set []

  let rows tbl =
    let cols = columns tbl in
    List.sort tbl.sortfn
      (Hashtbl.fold (fun r ct l ->
        (r, List.map (Hashtbl.find_all ct) cols)::l
       ) tbl.rc_idx [])

  let render tbl render_cell =
    let headers = List.map
      (fun h -> <:html<<th>$str:h$</th>&>>) (columns tbl) in
    let rows = List.map (fun (rl,r) ->
      <:html<<tr><th>$str:rl$</th>$list:List.map render_cell r$</tr>&>>)
      (rows tbl) in
    if cell_count tbl > 0
    then <:html<
      <table>
      <tr><th></th>$list:headers$</tr>
      $list:rows$
      </table>&>>
    else []
end

let li x = <:html< <li>$x$</li> >>

let ul lst = <:html<
  <ul>
    $list:List.map li lst$
  </ul>
>>

let page ?title body =
  let page_title, header = match title with
    | None -> site_title, site_title
    | Some t -> t ^ " : " ^ site_title, t
  in
  <:html<
  <html xmlns="http://www.w3.org/1999/xhtml">
    <head>
      <link rel='stylesheet' type='text/css' href="$uri:style$"/>
      <style type='text/css'>
         .pass { background-color: green; }
         .fail { background-color: red; }
      </style>
      <title>$str:page_title$</title>
    </head>
    <body>
      <h1>$str:header$</h1>
      $body$
    </body>
  </html>
>>

let to_string = HTML.to_string
