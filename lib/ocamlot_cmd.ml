open Printf
open Cmdliner
open Lwt
open Result

module Jar = Github_cookie_jar

exception WTFGitHub of string

let version = "0.0.0"

let user = "OCamlPro"
let repo = "opam-repository"
let cookie = "ocamlot"

let load_auth name =
  Jar.get ~name
  >>= function
    | None ->
        Printf.eprintf "No Github Cookie Jar cookie 'ocamlot'\n";
        Printf.eprintf "Use ocaml-github/jar.native to create a local token 'ocamlot'\n";
        exit 1
    | Some auth -> return Github.(Monad.(
      API.set_user_agent "ocamlot_cmd"
      >> API.set_token (Token.of_string auth.Github_t.auth_token)
    ))

let list_pulls () = Lwt_main.run (
  load_auth cookie
  >>= fun github -> Github.(Monad.(run Github_t.(
    github
    >> Pull.for_repo ~user ~repo ~state:`Open ()
    >>= fun pulls ->
    printf "%-8s | %-10s | %-40s\n" "Number" "Created" "Title";
    printf "%s\n" (String.make 80 '-');
    List.iter (fun pull ->
      printf "%8d | %10s | %-40s\n"
        pull.pull_number (String.sub pull.pull_created_at 0 10) pull.pull_title
    ) pulls;
    return ()
  ))))

let show_pull pull_id = Lwt_main.run (
  load_auth cookie
  >>= fun github -> Github.(Monad.(run Github_t.(
    github
    >> Pull.get ~user ~repo ~num:pull_id ()
    >>= fun pull ->
    printf "%s\n" (Yojson.Safe.prettify (Github_j.string_of_pull pull));
    return ()
  ))))

let base_branch_of_pull pull = Github_t.(
  match pull.pull_base.branch_repo with
    | None -> raise (WTFGitHub (sprintf "pull %d lacks a base repo" pull.pull_number))
    | Some repo -> let repo_url = Uri.of_string repo.repo_clone_url in Repo.({
      repo={ url=Uri.of_string ""; repo_url };
      name=pull.pull_base.branch_ref;
      label=pull.pull_base.branch_label;
    })
)

let head_branch_of_pull pull = Github_t.(
  match pull.pull_head.branch_repo with
    | None -> raise (WTFGitHub (sprintf "pull %d lacks a head repo" pull.pull_number))
    | Some repo -> let repo_url = Uri.of_string repo.repo_clone_url in Repo.({
      repo={ url=Uri.of_string ""; repo_url };
      name=pull.pull_head.branch_ref;
      label=pull.pull_head.branch_label;
    })
)

let test_pull pull_id = Lwt_main.run (
  load_auth cookie
  >>= fun github -> Github.(Monad.(run Github_t.(
    github
    >> Pull.get ~user ~repo ~num:pull_id ()
    >>= fun pull ->
    let work_dir = Filename.concat (Unix.getcwd ()) "work" in
    let task = Opam_task.(Repo.({
      base=base_branch_of_pull pull;
      head=head_branch_of_pull pull;
      target={ arch=Host.X86_64; os=OpamGlobals.Linux;
               compiler={ c_version="4.00.1"; c_build="" };
             };
      action=Build;
    })) in
    match Opam_task.run (string_of_int pull_id) work_dir task with
      | { status=`Failed; duration; output } ->
          Printf.eprintf "%s\n%!" output.err;
          Printf.eprintf "OCAMLOT %s failed in %s\n%!"
            "THE OPAM TASK"
            (Time.duration_to_string duration);
          return ()
      | { status=`Passed; duration; output } ->
          Printf.eprintf "OCAMLOT %s passed in %s\n%!"
            "THE OPAM TASK"
            (Time.duration_to_string duration);
          return ()
  ))))

(* CLI *)
let list_cmd =
  Term.(pure list_pulls $ pure ()),
  Term.info "list" ~doc:"list all outstanding Github OCamlPro/opam-repository pull requests"

let show_cmd =
  let number = Arg.(required & pos 0 (some int) None & info [] ~docv:"PULL_ID" ~doc:"Pull identifier.") in
  Term.(pure show_pull $ number),
  Term.info "show" ~doc:"show more details of a single Github OCamlPro/opam-repository pull request"


let test_cmd =
  let number = Arg.(required & pos 0 (some int) None & info [] ~docv:"PULL_ID" ~doc:"Pull identifier.") in
  Term.(pure test_pull $ number),
  Term.info "test" ~doc:"test a Github OCamlPro/opam-repository pull request"

let default_cmd =
  let doc = "conduct integration tests for opam-repository" in
  Term.(ret (pure (`Help (`Pager, None)))),
  let man = [
    `S "DESCRIPTION";
    `P "To maintain a high-level of quality assurance for the OCaml Platform, OPAM package repository maintainers need to quickly and accurately assess compatibility of proposed package updates. $(b,ocamlot) gives maintainers command-line access to GitHub pull requests and a simple means to test the effect of the proposed package updates in an isolated environment.";
    `S "COMMON OPTIONS";
    `P "$(b,--help) will show more help for each of the sub-commands above.";
    `S "BUGS";
     `P "Email bug reports to <mailto:infrastructure@lists.ocaml.org>, or report them online at <http://github.com/ocamllabs/ocamlot>."] in
  Term.info "ocamlot" ~version ~doc ~man

let cmds = [list_cmd; show_cmd; test_cmd] (* merge_cmd]*)

let () =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1 | _ -> exit 0
