open Printf
open Cmdliner
open Lwt
open Result

module Jar = Github_cookie_jar

exception WTFGitHub of string
exception MissingEnv of string

type testable = Pull of int | Packages of string list
type github_repo = {
  user : string;
  repo : string;
}

let version = "0.0.0"

let main_repo = {
  user = "OCamlPro";
  repo = "opam-repository";
}

let mirror_base_repo = {
  user = "ocamlot-dev";
  repo = "opam-repository";
}

let mirror_head_repo = {
  user = "ocamlot";
  repo = "opam-repository";
}

let load_auth {user;repo} =
  let name = user^"/"^repo in
  Jar.get ~name
  >>= function
    | None ->
        Printf.eprintf "No Github Cookie Jar cookie '%s'\n" name;
        Printf.eprintf "Use 'git jar' to create a local token '%s'\n" name;
        exit 1
    | Some auth -> return Github.(Monad.(
      API.set_user_agent "ocamlot_cmd"
      >> API.set_token (Token.of_string auth.Github_t.auth_token)
    ))

let list_pulls closed = Lwt_main.run (
  let {user; repo} = main_repo in
  load_auth main_repo
  >>= fun github -> Github.(Monad.(run Github_t.(
    github
    >> Pull.for_repo ~user ~repo ~state:(if closed then `Closed else `Open) ()
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
  let {user; repo} = main_repo in
  load_auth main_repo
  >>= fun github -> Github.(Monad.(run Github_t.(
    github
    >> Pull.get ~user ~repo ~num:pull_id ()
    >>= fun pull ->
    printf "%s\n" (Yojson.Safe.prettify (Github_j.string_of_pull pull));
    return ()
  ))))

let open_pull pull_id = Lwt_main.run (
  let {user; repo} = main_repo in
  load_auth main_repo
  >>= fun github -> Github.(Monad.(run Github_t.(
    github
    >> Pull.get ~user ~repo ~num:pull_id ()
    >>= fun pull ->
    try let browser = OpamMisc.getenv "BROWSER" in
        OpamSystem.command [ browser; pull.pull_html_url ];
        return ()
    with Not_found -> raise (MissingEnv "Missing BROWSER environment variable")
  ))))
(*
let mirror_pull pull_id = Lwt_main.run (
  let {user; repo} = main_repo
)
*)
let branch_of_proj_pull proj pull = Github_t.(
  let branch = proj pull in
  match branch.branch_repo with
    | None -> raise (WTFGitHub (sprintf "pull %d lacks a base repo" pull.pull_number))
    | Some repo -> let repo_url = Uri.of_string repo.repo_clone_url in Repo.(
      match pull.pull_state with
        | `Open -> {
          repo={ url=Uri.of_string ""; repo_url };
          name=Head branch.branch_ref;
          label=branch.branch_label;
        }
        | `Closed -> let sha = branch.branch_sha in {
          repo={ url=Uri.of_string ""; repo_url };
          name=Commit (branch.branch_ref, sha);
          label=branch.branch_user.user_login^":"^sha;
        })
)
let base_branch_of_pull = branch_of_proj_pull (fun pull -> pull.Github_t.pull_base)
let head_branch_of_pull = branch_of_proj_pull (fun pull -> pull.Github_t.pull_head)

let packages_of_pull pull_id = Lwt_main.run (
  let {user; repo} = main_repo in
  load_auth main_repo
  >>= fun github -> Github.(Monad.(run Github_t.(
    github
    >> Pull.get ~user ~repo ~num:pull_id ()
    >>= fun pull -> return Opam_task.(Diff {
      base=base_branch_of_pull pull;
      head=head_branch_of_pull pull;
    })
  ))))

let work_dir = Filename.(concat (get_temp_dir_name ()) "ocamlot")
let () = OpamSystem.mkdir work_dir
let build_testable testable =
  let prefix, packages = match testable with
    | Pull pull_id -> string_of_int pull_id, packages_of_pull pull_id
    | Packages pkglst -> String.concat "-" pkglst, Opam_task.List pkglst
  in
  let {Host.os; arch} = Host.detect () in
  let task = Opam_task.({
    packages;
    target={ arch; os; compiler={ c_version="4.00.1"; c_build="" }; };
    action=Build;
  }) in
  match Opam_task.run ~jobs:3 prefix work_dir task with
    | { status=`Failed; duration; output } ->
        Printf.eprintf "%s\n%!" output.err;
        Printf.eprintf "OCAMLOT %s FAILED in %s\n%!"
          (Opam_task.to_string task)
          (Time.duration_to_string duration)
    | { status=`Passed; duration; output } ->
        Printf.eprintf "OCAMLOT %s PASSED in %s\n%!"
          (Opam_task.to_string task)
          (Time.duration_to_string duration)

(* CLI *)
let pull_id = Arg.(required & pos 0 (some int) None & info [] ~docv:"PULL_ID" ~doc:"Pull identifier.")

let list_cmd =
  let closed = Arg.(value (flag (info ["closed"] ~docv:"CLOSED" ~doc:"Show closed pull requests."))) in
  Term.(pure list_pulls $ closed),
  Term.info "list" ~doc:"list all outstanding Github OCamlPro/opam-repository pull requests"

let show_cmd =
  Term.(pure show_pull $ pull_id),
  Term.info "show" ~doc:"show more details of a single Github OCamlPro/opam-repository pull request"

let open_cmd =
  Term.(pure open_pull $ pull_id),
  Term.info "open" ~doc:"open the GitHub pull request overview"

let int_re = Re.(compile (seq [bos; rep1 digit; eos]))
let is_int = Re.execp int_re
let testable_of_string s =
  if is_int s
  then Pull (int_of_string s)
  else Packages Re_str.(split (regexp_string ",") s)
let build_cmd =
  let testable_str = Arg.(required & pos 0 (some string) None & info []
                            ~docv:"PKGS_ID" ~doc:"Pull identifier or comma-separated package list") in
  Term.(pure build_testable $ (pure testable_of_string $ testable_str)),
  Term.info "build" ~doc:"build a Github OCamlPro/opam-repository pull request"
(*
let mirror_cmd =
  Term.(pure mirror_pull $ pull_id),
  Term.info "mirror" ~doc:"mirror the GitHub pull request"
*)
let default_cmd =
  let doc = "conduct integration tests for opam-repository" in
  Term.(ret (pure (`Help (`Pager, None)))),
  let man = [
    `S "DESCRIPTION";
    `P "To assure a high-level of quality for the OCaml Platform, OPAM package repository maintainers need to quickly and accurately assess compatibility of proposed package updates. $(b,ocamlot) gives maintainers command-line access to GitHub pull requests and a simple means to test the effect of the proposed package updates in an isolated environment.";
    `S "COMMON OPTIONS";
    `P "$(b,--help) will show more help for each of the sub-commands above.";
    `S "BUGS";
     `P "Email bug reports to <mailto:infrastructure@lists.ocaml.org>, or report them online at <http://github.com/ocamllabs/ocamlot>."] in
  Term.info "ocamlot" ~version ~doc ~man

let cmds = [list_cmd; show_cmd; open_cmd; build_cmd] (*; mirror_cmd] (* merge_cmd]*)*)

let () =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1 | _ -> exit 0
