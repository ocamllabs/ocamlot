(*open Cmdliner*)
;;
Opam_task.run
  (Filename.concat (Unix.getcwd ()) "work")
  Opam_task.(Repo.({
    trunk={ branch_repo={ url=Uri.of_string "";
                          repo_url=Uri.of_string "https://github.com/OCamlPro/opam-repository.git";
                        }; branch_name="master"; };
    branch={ branch_repo={ url=Uri.of_string "";
                           repo_url=Uri.of_string "https://github.com/janestreet/opam-repository.git";
                         }; branch_name="core-109.21.00"; };
    target={ arch=Host.X86_64; os=OpamGlobals.Linux;
             compiler={ c_version="4.00.1"; c_build="" };
             packages=[{ p_name="async";         p_version="109.21.00" };
                       { p_name="async_unix";    p_version="109.21.00" };
                       { p_name="core";          p_version="109.21.00" };
                       { p_name="core_extended"; p_version="109.21.00" };
                       { p_name="jenga";         p_version="109.21.00" };
                       { p_name="zero";          p_version="109.21.00" };
                      ];
           };
    action=Build;
  }))
(*
let test_cmd =
  let source =
    let doc = "Use the $(docv) service to retrieve opam-repository" in
    Arg.(value & opt ~vopt:Github (enum services) Github & info ["service"]
           ~docv:"SERVICE" ~doc)
  in
  let doc = "use the provided service to retrieve opam-repository" in
  let main = [
    `S "DESCRIPTION";
    `P "Uses the provided service to retrieve opam-repository for testing.";
  ] in
  Term.(pure test $ source),
  Term.info "test" ~doc ~man
*)
