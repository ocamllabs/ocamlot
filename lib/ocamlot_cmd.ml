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

let git_of_repo {user; repo} =
  Repo.URL (Uri.of_string ("git://github.com/"^user^"/"^repo^".git"))

let url_of_repo {user; repo} = Uri.of_string
  (sprintf "https://github.com/%s/%s.git" user repo)

let load_auth ?(scopes=[]) {user;repo} =
  let name = user^"/"^repo in
  Jar.init ()
  >>= fun jar ->
  Jar.get jar ~name
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
  )))
  >>= fun { Github_t.pull_html_url } ->
  try begin
    let browser = Unix.getenv "BROWSER" in
    Repo.run_command ~cwd:(Unix.getcwd ()) [ browser; pull_html_url ]
    >>= fun _ -> return ()
  end with Not_found ->
    raise (MissingEnv "Missing BROWSER environment variable")
)

let mirror_pulls pull_ids = Lwt_main.run (Github_t.(
  let rev_map fn =
    let rec loop lst = Github.Monad.(function
      | [] -> return lst
      | x::xs -> fn x >>= fun ghob -> loop (ghob::lst) xs
    )
    in loop []
  in
  let {user; repo} = main_repo in
  let repo_url = git_of_repo main_repo in
  load_auth main_repo
  >>= fun github ->
  Github.(Monad.(run (
    github >> (rev_map (fun num -> Pull.get ~user ~repo ~num ()) pull_ids)
  )))
  >>= fun pulls ->
  let dir = Repo.make_temp_dir ~root_dir:work_dir ~prefix:"mirror" in
  Repo.(
    clone_repo ~dir
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
    let base_url = git_of_repo mirror_base_repo in
    let refspec = "refs/heads/*:refs/heads/*" in
    push_refspec ~dir ~url:base_url ~refspec
    >>= fun dir ->
    update_refs ~dir (List.rev_map (fun pull ->
      Copy (sprintf "refs/heads/pull-%d" pull.pull_number,
            Ref (sprintf "refs/pull/%d/head" pull.pull_number))) pulls)
    >>= fun dir ->
    let head_url = git_of_repo mirror_head_repo in
    let refspec = "refs/heads/*:refs/heads/*" in
    push_refspec ~dir ~url:head_url ~refspec;
    >>= fun _ -> return ()
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

let diff_of_pull pull_id = Opam_repo.diff_of_pull (Lwt_main.run (
  let {user; repo} = main_repo in
  load_auth main_repo
  >>= fun github -> Github.(Monad.(run Github_t.(
    github
    >> Pull.get ~user ~repo ~num:pull_id ()
  )))))

let () = Util.mkdir_p work_dir 0o700
let build_testable testable debug repo_opt branch_opt = Lwt_main.run (
  catch (fun () ->
  let jobs = try int_of_string (Sys.getenv "OPAMJOBS") with Not_found -> 1 in
  let repo_of_path rpath name =
    let cwd = Uri.of_string (Filename.concat (Unix.getcwd ()) "") in
    let repo_url = Repo.URL Uri.(resolve "file" cwd (of_string rpath)) in
    Repo.({repo ={ url=Uri.of_string ""; repo_url };
           reference=Ref name; label=sprintf "%s:%s" rpath name;
          })
  in
  let branch_opt = match repo_opt, branch_opt with
    | None, None -> []
    | Some rpath, None -> [repo_of_path rpath "master"]
    | None, Some bname ->
        [repo_of_path (Uri.to_string (url_of_repo main_repo)) bname]
    | Some rpath, Some bname -> [repo_of_path rpath bname]
  in
  let host = Host.detect () in begin
    Printf.eprintf "detected host: %s\n%!" (Host.to_string host);
    Opam_task.(ocamlc_path
                 ~env:(basic_env
                         work_dir
                         (Filename.concat work_dir "opam-install"))
                 work_dir)
    >>= Opam_task.compiler_of_path
    >>= fun compiler ->
    let target = Opam_task.({ host; compiler; }) in
    begin match testable with
      | Pull pull_id ->
          let diff = branch_opt@(diff_of_pull pull_id) in
          let prefix = string_of_int pull_id in
          Opam_repo.packages_of_diff prefix work_dir diff
          >>= fun packages ->
          return (List.map (fun opam_task ->
            prefix, Ocamlot.Opam opam_task
          ) Opam_task.(tasks_of_packages [target] Build diff packages))
      | Packages packages ->
          let base = {
            Repo.repo = Repo.({
              url=Uri.of_string "";
              repo_url=git_of_repo main_repo
            });
            reference=Repo.Ref "master";
            label=sprintf "%s/%s:master" main_repo.user main_repo.repo;
          } in
          let diff = branch_opt@[base] in
          let prefix = String.concat "-" packages in
          return (List.map (fun opam_task ->
            prefix, Ocamlot.Opam opam_task
          ) Opam_task.(tasks_of_packages [target] Build diff packages))
    end
    >>= Lwt_list.map_s (fun (prefix, task) ->
      Work.execute ~debug ~jobs prefix work_dir work_dir task
      >>= fun result -> return (task, result)
    )
    >>= fun job_results ->
    return (List.iter (fun (task, result) ->
      Work.print_result ~debug task result
    ) job_results)
  end
  ) (Repo.die "build_testable")
)

let work_url url_str = Lwt_main.run (
  Work.forever
    work_dir
    (Filename.concat (Unix.getcwd ()) "ocaml")
    (Uri.of_string url_str)
)

let serve () =
  (* TODO: BEGIN should be adjustable from command line *)
  let open Config in
  let url_str = Uri.to_string
    (Uri.make ~scheme:"https" ~host ~port:outside_port ~path:"/" ()) in
  let port_opt = Some port in
  (* END *)
  let url = Uri.of_string url_str in
  let port = match port_opt, Uri.port url with
    | Some port, _ -> port
    | None, Some port -> port
    | None, None -> (prerr_endline "No server port specified; quitting."; exit 1)
  in
  prerr_endline "Starting ocamlot server...\n";
  Serve.forever url port

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
  let debug = Arg.(value & flag & info ["debug"]
                     ~docv:"DEBUG" ~doc:"retain opam install, repository, and build directory") in
  let overlay = Arg.(value & pos 1 (some string) None & info []
                       ~docv:"REPO_HREF" ~doc:"opam-repository URI reference to merge last") in
  let overlay_branch = Arg.(value & pos 2 (some string) None & info []
                              ~docv:"REPO_BRANCH" ~doc:"branch of $(b,REPO_HREF) to merge last") in
  Term.(pure build_testable $ (pure testable_of_string $ testable_str)
          $ debug $ overlay $ overlay_branch),
  Term.info "build" ~doc:"build a Github OCamlPro/opam-repository pull request"

let mirror_cmd =
  let ids = Arg.(non_empty & pos_all int [] & info []
                   ~docv:"PULL_IDS" ~doc:"Pull identifiers to mirror") in
  Term.(pure mirror_pulls $ ids),
  Term.info "mirror" ~doc:"mirror the GitHub pull request(s)"

let work_cmd =
  let url = Arg.(required & pos 0 (some string) None & info []
                   ~docv:"OCAMLOT_URL" ~doc:"URL of task queue resource") in
  Term.(pure work_url $ url),
  Term.info "work" ~doc:"queue for work"

let serve_cmd =
  Term.(pure serve $ pure ()),
  Term.info "serve" ~doc:"start an ocamlot server daemon"

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

let cmds = [
  list_cmd; show_cmd; open_cmd;
  build_cmd; mirror_cmd; work_cmd;
  serve_cmd;
]

let () =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1 | _ -> exit 0
