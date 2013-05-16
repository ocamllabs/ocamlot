(* mutable, lazy, renderable, negotiated, aggregated web resource representations *)

type text_type = [
| `plain
| `html
]

type application_type = [
| `atom
| `rdf
| `xhtml
]

type 'a serialization = [
| `xml of 'a option
| `json of 'a option
]

type media_type = [
| `text of text_type
| `application of application_type serialization
]

type 'a content =
  | Content of 'a

type ('a, 'd) t = {
  uri : Uri.t;
  create_time : Time.t;
  mutable update_time : Time.t;
  rendering : (media_type, string Lazy.t) Hashtbl.t;
  renderer : (media_type, ('a, 'd) renderer) Hashtbl.t;
  mutable content : 'a content;
  update : 'a -> 'd -> 'a;
  updates : ('a, 'd) event Lwt_stream.t;
  updated : ('a, 'd) event -> unit;
}
and ('a, 'd) event =
  | Create of 'a * ('a, 'd) t
  | Update of 'd * ('a, 'd) t
and ('a, 'd) renderer = ('a, 'd) event -> string

type ('a, 'd) index = {
  index : (Uri.t, ('a, 'd) t) Hashtbl.t;
  generate_uri : 'a -> Uri.t;
}

type 'a archive = {
  archive : (Uri.t, ('a, unit) t) Hashtbl.t;
}

let render renderer ev rendering = Hashtbl.iter (fun t fn ->
  Hashtbl.replace rendering t (Lazy.from_fun (fun () -> fn ev))
) renderer

let stream r = Lwt_stream.clone r.updates

let create uri content update renderer =
  let now = Time.now () in
  let rendering = Hashtbl.create (Hashtbl.length renderer) in
  let updates, updated = Lwt_stream.create () in
  let r = {
    uri;
    create_time = now;
    update_time = now;
    rendering;
    renderer;
    content = Content content;
    update;
    updates;
    updated=(fun ev -> updated (Some ev));
  } in
  render renderer (Create (content,r)) rendering;
  r

let update r d = match r.content with
  | Content content ->
      let event = Update (d, r) in
      let content = r.update content d in
      r.update_time <- Time.now ();
      r.content <- Content content;
      render r.renderer event r.rendering;
      r.updated event;
      r

let content {content = Content content} = content
let uri {uri} = uri

let bubble child parent lift =
  let s = stream child in
  let rec pump () = Lwt.(
    Lwt_stream.next s
    >>= fun ev ->
    ignore (update parent (lift ev));
    pump ()
  ) in
  Lwt.async pump

let index idx content update renderer =
  let uri = idx.generate_uri content in
  let r = create uri content update renderer in
  Hashtbl.replace idx.index uri r;
  r

let create_index generate_uri curl =
  let size = 1 + (2 * (List.length curl)) in
  let idx = { index=Hashtbl.create size; generate_uri } in
  List.iter (fun (content, update, renderer) ->
    ignore (index idx content update renderer)
  ) curl;
  idx

let remove uri {index} =
  let r = Hashtbl.find index uri in
  Hashtbl.remove index uri;
  r

let insert {index} r =
  Hashtbl.replace index r.uri r

let to_list {index} =
  Hashtbl.fold (fun _ v l -> v::l) index []

let find {index} uri =
  Hashtbl.find index uri

let archive a freeze r =
  let updates, _ = Lwt_stream.create () in
  Hashtbl.replace a.archive r.uri { r with
    renderer = Hashtbl.create 1;
    content = (match r.content with Content c -> Content (freeze c));
    update = (fun r () -> r);
    updates;
    updated = (fun _ -> ());
  }

let create_archive rfl =
  let a = { archive=Hashtbl.create (List.length rfl); } in
  List.iter (fun (r, freeze) -> archive a freeze r) rfl;
  a

(*
let create_aggregate uri cl renderer diff_lift =
  let now = Time.now () in
  let children = Hashtbl.create (List.length cl) in
  List.iter (fun c -> Hashtbl.replace children c.uri c) cl;
  let updates, updated = Lwt_stream.create () in
  let updates = Lwt_stream.choose (updates::(List.rev_map (fun c ->
    Lwt_stream.filter_map (diff_lift c.uri) (stream c)
  ) cl)) in
  let rendering = Hashtbl.create (Hashtbl.length renderer) in
  let agg = {
    uri;
    create_time = now;
    update_time = now;
    rendering;
    renderer;
    content = Content children;
    update;
    updates;
    updated = (fun ev -> updated (Some ev));
  } in
  render renderer (Create agg) rendering;
  agg
*)
