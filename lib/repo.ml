type endpoint = Github of Github_hook.endpoint

type git = Uri.t

type 'a t = {
  url : Uri.t;
  repo_url : 'a;
 (* endpoint : endpoint; *)
}

type sha = string

type reference = Ref of string | Commit of string * sha

type 'a branch = {
  repo : 'a t;
  name : reference;
  label : string;
}

type 'a process = Continue of 'a | Terminate of Result.output

let (>>=) process f = match process with
  | Continue env -> f env
  | Terminate out -> Terminate out

let terminate_of_process_error e = OpamProcess.(
  List.iter (fun l -> Printf.eprintf "ERR: %s\n" l) e.r_stderr;
  Terminate Result.({
    err=String.concat "\n" e.r_stderr;
    out=String.concat "\n" e.r_stdout;
    info=e.r_info;
  })
)

let clone_repo ~name ~branch =
  let dir = OpamFilename.Dir.of_string name in
  let url = Uri.to_string branch.repo.repo_url in
  let git_ref, ref_cmd = match branch.name with
    | Ref r -> r, []
    | Commit (r, sha) -> r, [[ "git" ; "update-ref" ; r ; sha ]]
  in
  try
    OpamFilename.in_dir dir (fun () -> OpamSystem.commands ([
      [ "git" ; "clone" ; url ; "." ];
    ]@ref_cmd@[
      [ "git" ; "checkout" ; git_ref ];
    ]));
    Printf.eprintf "OCAMLOT repo clone %s\n%!" branch.label;
    Continue dir
  with OpamSystem.Process_error e -> terminate_of_process_error e

