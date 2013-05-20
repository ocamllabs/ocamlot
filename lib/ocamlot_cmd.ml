open Printf
open Cmdliner
open Lwt
open Result

module Jar = Github_cookie_jar

exception MissingEnv of string

type testable = Pull of int | Packages of string list
type github_repo = {
  user : string;
  repo : string;
}

let version = "0.0.0"

let work_dir = Filename.(concat (get_temp_dir_name ()) "ocamlot")

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

let git_ssh_of_repo {user; repo} =
  Repo.SSH (Uri.of_string "git@github.com", Uri.of_string (user^"/"^repo^".git"))

let url_of_repo {user; repo} = Uri.of_string
  (sprintf "https://github.com/%s/%s.git" user repo)

let load_auth ?(scopes=[]) {user;repo} =
  let name = user^"/"^repo in
  Jar.get ~name
  >>= function
    | None ->
        Printf.eprintf "No Github Cookie Jar cookie '%s'\n" name;
        Printf.eprintf "Use 'git jar' to create a local token '%s'\n" name;
        exit 1
    | Some auth ->
        let pred s = List.mem s auth.Github_t.auth_scopes in
        if List.for_all pred scopes
        then return Github.(Monad.(
          API.set_user_agent "ocamlot_cmd"
          >> API.set_token (Token.of_string auth.Github_t.auth_token)
        ))
        else fail
          (Failure (sprintf "Expected %s/%s to have scopes: %s"
                      user repo
                      (String.concat ","
                         (List.rev_map
                            Github.Scope.string_of_scope
                            (List.filter (fun s -> not (pred s)) scopes)))))

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

let mirror_pulls pull_ids = Lwt_main.run (Github_t.(
  let rev_map fn =
    let rec loop lst = Github.Monad.(function
      | [] -> return lst
      | x::xs -> fn x >>= fun ghob -> loop (ghob::lst) xs
    )
    in loop []
  in
  let {user; repo} = main_repo in
  let repo_url = git_ssh_of_repo main_repo in
  load_auth main_repo
  >>= fun github ->
  Github.(Monad.(run (
    github >> (rev_map (fun num -> Pull.get ~user ~repo ~num ()) pull_ids)
  )))
  >>= fun pulls ->
  let name = Repo.make_temp_dir ~root_dir:work_dir ~prefix:"mirror" in
  Repo.(match begin
    clone_repo ~name
      ~commit:{repo={ url=Uri.of_string ""; repo_url };
               reference=Ref "master";
               label=sprintf "%s/%s:%s" user repo "master";
              }
    >>= fun dir ->
    let refspec = "refs/pull/*/head:refs/pull/*/head" in
    fetch_refspec ~dir ~url:repo_url ~refspec
    >>= fun dir ->
    update_refs ~dir (List.rev_map (fun pull ->
      Commit (sprintf "refs/heads/pull-%d" pull.pull_number,
              pull.pull_base.branch_sha)) pulls)
    >>= fun dir ->
    let base_url = git_ssh_of_repo mirror_base_repo in
    let refspec = "refs/heads/*:refs/heads/*" in
    push_refspec ~dir ~url:base_url ~refspec
    >>= fun dir ->
    update_refs ~dir (List.rev_map (fun pull ->
      Copy (sprintf "refs/heads/pull-%d" pull.pull_number,
            Ref (sprintf "refs/pull/%d/head" pull.pull_number))) pulls)
    >>= fun dir ->
    let head_url = git_ssh_of_repo mirror_head_repo in
    let refspec = "refs/heads/*:refs/heads/*" in
    push_refspec ~dir ~url:head_url ~refspec
  end with
    | Continue _ -> return ()
    | Terminate _ -> (* TODO: do *)
        fail (Failure "something went wrong in mirror's clone+fetch+push")
  )
  >>= fun () ->
  let {user; repo} = mirror_base_repo in
  load_auth ~scopes:[`Public_repo] mirror_head_repo
  >>= fun github ->
  Github.(Monad.(run (
    github
    >>= fun () ->
    let create_pull = Pull.create ~user ~repo in
    rev_map (fun {pull_title; pull_body; pull_number} ->
      let pull = {
        new_pull_title=pull_title;
        new_pull_body=Some pull_body;
        new_pull_base=sprintf "pull-%d" pull_number;
        new_pull_head=sprintf "%s:pull-%d" mirror_head_repo.user pull_number;
      } in
      Printf.eprintf "OCAMLOT creating pull:\n%s\n" (Yojson.Safe.prettify (Github_j.string_of_new_pull pull));
      create_pull ~pull ()
    ) pulls
  )))
  >>= fun new_pulls -> ignore new_pulls; return ()
))

let diff_of_pull pull_id = Opam_task.diff_of_pull (Lwt_main.run (
  let {user; repo} = main_repo in
  load_auth main_repo
  >>= fun github -> Github.(Monad.(run Github_t.(
    github
    >> Pull.get ~user ~repo ~num:pull_id ()
  )))))

let () = OpamSystem.mkdir work_dir
let build_testable testable repo_opt branch_opt =
  let repo_of_path rpath name =
    let cwd = Uri.of_string (Filename.concat (Unix.getcwd ()) "") in
    let repo_url = Repo.URL Uri.(resolve "file" cwd (of_string rpath)) in
    Some Repo.({repo ={ url=Uri.of_string ""; repo_url };
                reference=Ref name; label=sprintf "%s:%s" rpath name;
               })
  in
  let branch_opt = match repo_opt, branch_opt with
    | None, None -> None
    | Some rpath, None -> repo_of_path rpath "master"
    | None, Some bname -> repo_of_path (Uri.to_string (url_of_repo main_repo)) bname
    | Some rpath, Some bname -> repo_of_path rpath bname
  in
  let prefix, packages = match testable with
    | Pull pull_id ->
        string_of_int pull_id, Opam_task.Diff (diff_of_pull pull_id, branch_opt)
    | Packages pkglst ->
        begin match branch_opt with
          | None -> String.concat "-" pkglst, Opam_task.List (pkglst, None)
          | Some head -> String.concat "-" pkglst,
            Opam_task.(List (pkglst, Some {
              base={
                Repo.repo =Repo.({
                  url=Uri.of_string "";
                  repo_url=git_ssh_of_repo main_repo
                });
                reference=Repo.Ref "master";
                label=sprintf "%s/%s:master" main_repo.user main_repo.repo;
              };
              head;
            }))
        end
  in
  let host = Host.detect () in
  let task = Opam_task.({
    packages;
    target={ host; compiler={ c_version="4.00.1"; c_build="" }; };
    action=Build;
  }) in
  match Opam_task.run ~jobs:3 prefix work_dir task with
    | { status=Failed; duration; output } ->
        Printf.eprintf "%s\n%!" output.err;
        Printf.eprintf "OCAMLOT %s FAILED in %s\n%!"
          (Opam_task.to_string task)
          (Time.duration_to_string duration)
    | { status=Passed; duration; output } ->
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
  let overlay = Arg.(value & pos 1 (some string) None & info []
                       ~docv:"REPO_HREF" ~doc:"opam-repository URI reference to merge last") in
  let overlay_branch = Arg.(value & pos 2 (some string) None & info []
                              ~docv:"REPO_BRANCH" ~doc:"branch of $(b,REPO_HREF) to merge last") in
  Term.(pure build_testable $ (pure testable_of_string $ testable_str) $ overlay $ overlay_branch),
  Term.info "build" ~doc:"build a Github OCamlPro/opam-repository pull request"

let mirror_cmd =
  let ids = Arg.(non_empty & pos_all int [] & info []
                   ~docv:"PULL_IDS" ~doc:"Pull identifiers to mirror") in
  Term.(pure mirror_pulls $ ids),
  Term.info "mirror" ~doc:"mirror the GitHub pull request(s)"

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

let cmds = [list_cmd; show_cmd; open_cmd; build_cmd; mirror_cmd] (* merge_cmd]*)

let () =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1 | _ -> exit 0
