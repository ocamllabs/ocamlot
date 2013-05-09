type endpoint = Github of Github_hook.endpoint

type git =
  | SSH of Uri.t * Uri.t
  | URL of Uri.t

type 'a t = {
  url : Uri.t;
  repo_url : 'a;
 (* endpoint : endpoint; *)
}

type sha = string

type reference =
  | Ref of string
  | Commit of string * sha
  | Copy of string * string

type 'a branch = {
  repo : 'a t;
  name : reference;
  label : string;
}

type 'a process = Continue of 'a | Terminate of Result.output

let (>>=) process f = match process with
  | Continue env -> f env
  | Terminate out -> Terminate out

let string_of_git = function
  | SSH (host, path) -> (Uri.to_string host)^":"^(Uri.to_string path)
  | URL url -> Uri.to_string url

let terminate_of_process_error e = OpamProcess.(
  List.iter (fun l -> Printf.eprintf "ERR: %s\n" l) e.r_stderr;
  Terminate Result.({
    err=String.concat "\n" e.r_stderr;
    out=String.concat "\n" e.r_stdout;
    info=e.r_info;
  })
)

let update_ref_cmd = function
  | Ref r -> r, []
  | Commit (r, sha) -> r, [[ "git" ; "update-ref" ; r ; sha ]]
  | Copy (a, b) -> a, [[ "git" ; "update-ref" ; a ; b ]]

let update_refs ~dir refs =
  OpamFilename.in_dir dir (fun () ->
    OpamSystem.commands (List.fold_left (fun cmds r ->
      (snd (update_ref_cmd r))@cmds
    ) [] refs));
  Continue dir

let clone_repo ~name ~branch =
  let dir = OpamFilename.Dir.of_string name in
  let url = string_of_git branch.repo.repo_url in
  let git_ref, ref_cmd = update_ref_cmd branch.name in
  try
    OpamFilename.in_dir dir (fun () -> OpamSystem.commands ([
      [ "git" ; "clone" ; url ; "." ];
    ]@ref_cmd@[
      [ "git" ; "checkout" ; git_ref ];
    ]));
    Printf.eprintf "OCAMLOT repo clone %s\n%!" branch.label;
    Continue dir
  with OpamSystem.Process_error e -> terminate_of_process_error e

let make_temp_dir ~root_dir ~prefix =
  let tmp_name = Util.make_fresh_dir ~root_dir
    ("ocamlot."^prefix^"."^Time.(date_to_string (now ()))^".") in
  Printf.eprintf "OCAMLOT temp_dir %s made\n%!" tmp_name;
  tmp_name

let fetch_refspec ~dir ~url ~refspec =
  try
    OpamFilename.in_dir dir (fun () -> OpamSystem.command [
      "git" ; "fetch" ; string_of_git url ; refspec ;
    ]);
    Printf.eprintf "OCAMLOT fetch refspec %s into %s\n%!"
      refspec (OpamFilename.Dir.to_string dir);
    Continue dir
  with OpamSystem.Process_error e -> terminate_of_process_error e

let push_refspec ~dir ~url ~refspec =
  try
    OpamFilename.in_dir dir (fun () -> OpamSystem.command [
      "git" ; "push" ; string_of_git url ; refspec ;
    ]);
    Printf.eprintf "OCAMLOT push refspec %s onto %s\n%!"
      refspec (string_of_git url);
    Continue dir
  with OpamSystem.Process_error e -> terminate_of_process_error e

let try_merge ~dir ~base ~head =
  let refspec = match head.name with Ref h | Commit (h,_) | Copy (h,_) -> h^":"^h in
  fetch_refspec ~dir ~url:head.repo.repo_url ~refspec
  >>= fun dir ->
  try (* TODO: update-ref ? *)
    OpamFilename.in_dir dir (fun () -> OpamSystem.command [
      "git" ; "merge" ; "--no-edit" ;
      (match head.name with Ref h | Commit (h,_) | Copy (h,_) -> h);
    ]);
      Printf.eprintf "OCAMLOT repo merge %s onto %s\n%!"
        head.label base.label;
      Continue dir
  with OpamSystem.Process_error e -> terminate_of_process_error e
