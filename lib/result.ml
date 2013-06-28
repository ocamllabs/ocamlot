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

open Sexplib.Std

module Uri = struct
  include Uri
  let t_of_sexp sexp =
    of_string (Sexplib.Std.string_of_sexp sexp)
  let sexp_of_t uri = Sexplib.Std.sexp_of_string (to_string uri)
end

type category =
  | Incompat
  | Dependency
  | Transient
  | System
  | Fixable (* Meta *)
  | Ext_dep
  | Errorwarn
  | Broken

type solver_error =
  | Unsatisfied_dep of string (* TODO: this is pkg + constraint right now *)
with sexp

type system_error =
  | No_space
with sexp

type analysis =
  | No_solution of solver_error option
  | System_error of system_error
  | Incompatible
  | Error_for_warn
  | Checksum of Uri.t * string * string
  | Pkg_config_dep_ext of string
  | Pkg_config_dep_ext_constraint of string * string
  | Header_dep_ext of string
  | Command_dep_ext of string
  | C_lib_dep_exts of string list
  | Missing_ocamlfind_dep of string
  | Missing_findlib_constraint of string * string
  | Broken_link of Uri.t
  | Dep_error of string * analysis list
with sexp

type analyses = analysis list with sexp

type error =
  | Process of Repo.proc_status * Repo.r
  | Other of string * string
with sexp

type status =
  | Passed of Repo.r
  | Failed of analysis list * error
with sexp

type t = {
  status : status;
  duration : Time.duration;
  info : string;
} with sexp

let is_failure = function Passed _ -> false | Failed (_,_) -> true

let get_status {status} = status

let worst_of_categories = List.fold_left (max) Incompat

let category_of_analysis = function
  | Incompatible -> Incompat
  | Error_for_warn -> Errorwarn
  | Pkg_config_dep_ext _
  | Pkg_config_dep_ext_constraint (_,_)
  | Header_dep_ext _
  | Command_dep_ext _
  | C_lib_dep_exts _ -> Ext_dep
  | Checksum (_,_,_)
  | Missing_ocamlfind_dep _
  | Missing_findlib_constraint (_,_) -> Fixable (* Meta *)
  | System_error _ -> System
  | Broken_link _ -> Transient
  | No_solution _
  | Dep_error (_, _) -> Dependency

let worst_of_analyses analyses =
  snd (List.fold_left (fun (mc,m) a ->
    let c = category_of_analysis a in
    if mc < c then (c,a) else (mc,m)
  ) (Incompat,Incompatible) analyses)

let string_of_category = function
  | Broken -> "ERROR"
  | Errorwarn -> "ERRWARN"
  | Incompat -> "INCOMPAT"
  | Dependency -> "DEP"
  | Fixable -> "META"
  | System -> "SYSTEM"
  | Transient -> "TRANS"
  | Ext_dep -> "EXTDEP"

let rec match_global ?(pos=0) ?(lst=[]) re s =
  let ofs = try Re.(get_all_ofs (exec ~pos re s))
    with Not_found -> [|-1,-1|] in
  if ofs.(0) = (-1,-1) then lst
  else
    let matches = ofs.(0), Array.map (fun (a,z) -> String.sub s a (z-a)) ofs in
    match_global ~pos:(snd ofs.(0)) ~lst:(matches::lst) re s

let unsat_dep_re = Re.(compile (seq [
  (* tested 2013/6/21 *)
  str "The dependency ";
  group (rep1 (compl [space]));
  str " of package ";
  group (rep1 (compl [space]));
  str " is not available";
]))
let solver_errors_of_r { Repo.r_args; r_stdout } =
  let matches = match_global unsat_dep_re r_stdout in
  if 0 = List.length matches then [No_solution None]
  else List.fold_left (fun lst (_,m) ->
    let err = No_solution (Some (Unsatisfied_dep m.(1))) in
    if List.mem m.(2) r_args
    then err::lst
    else (Dep_error (m.(2), [err]))::lst
  ) [] matches

let pkg_build_error_re = Re.(compile (seq [
  (* tested 2013/6/21 *)
  bol; str "==== ERROR [while installing ";
  group (rep1 (compl [set "]"]));
]))

let no_space_recognizer = Re.((* tested 2013/6/26 *)
  str "No space left on device", (fun _ -> System_error No_space)
)

let compile_pair (re,cons) = (Re.compile re,cons)
let build_error_stderr_re = Re.(List.map compile_pair [
  seq [ (* tested 2013/6/21 *)
    str "Wrong checksum for ";
    group (rep1 notnl);
    str "\n  - ";
    group (rep1 (compl [space]));
    str " [expected result]\n  - ";
    group (rep1 (compl [space]));
    str " [actual result]";
  ], (fun m ->
    Checksum (Uri.of_string (String.sub m.(1) 0 ((String.length m.(1)) - 1)),
              m.(2), m.(3)));
  seq [ (* tested 2013/6/21 *)
    str "configure: error: ";
    group (rep1 (compl [space]));
    str " not found";
  ], (fun m -> Pkg_config_dep_ext m.(1));
  seq [ (* tested 2013/6/26 *)
    str "configure: error: Cannot find ";
    group (rep1 (compl [set "."]));
    str ".";
  ], (fun m -> C_lib_dep_exts [m.(1)]);
  seq [ (* tested 2013/6/21 *)
    str "Cannot get ";
    group (rep1 notnl);
  ], (fun m -> Broken_link (Uri.of_string m.(1)));
  seq [ (* *)
    str "Internal error:\n";
    rep space;
    group (rep1 (compl [space]));
    str " is not available.";
  ], (fun m -> Broken_link (Uri.of_string m.(1)));
  seq [ (* tested 2013/6/26 *)
    str "Internal error:\n";
    rep space;
    str "\"";
    group (rep1 (compl [set "\""]));
    str "\": command not found.";
  ], (fun m -> Command_dep_ext m.(1));
  no_space_recognizer;
])

let build_error_stdout_re = Re.(List.map compile_pair [
  str "Error: Error-enabled warnings", (* tested 2013/6/21 *)
  (fun _ -> Error_for_warn);
  seq [ (* tested 2013/6/21 *)
    str "Package ";
    group (rep1 (compl [space]));
    str " was not found in the pkg-config search path";
  ], (fun m -> Pkg_config_dep_ext m.(1));
  seq [ (* tested 2013/6/21 *)
    str "checking whether pkg-config knows about ";
    group (rep1 (compl [space]));
    str " ";
    group (seq [compl [set "o"]; shortest (rep1 any)]);
    str "... "; compl [set "o"];
  ], (fun m -> Pkg_config_dep_ext_constraint (m.(1),m.(2)));
  seq [ (* tested 2013/6/21 *)
    str ": ";
    opt (str "fatal ");
    str "error: ";
    group (rep1 (compl [set "."]));
    str ".h: No such file or directory";
  ], (fun m -> Header_dep_ext m.(1));
  seq [ (* tested 2013/6/26 *)
    str ": fatal error: '";
    group (non_greedy (rep1 any));
    str ".h' file not found";
  ], (fun m -> Header_dep_ext m.(1));
  seq [ (* tested 2013/6/26 *)
    str "make: ";
    group (rep1 (compl [set ":"]));
    str ": ";
    alt [str "C"; str "c"];
    str "ommand not found";
  ], (fun m -> Command_dep_ext m.(1));
  seq [ (* tested 2013/6/28 *)
    str "configure: error: '";
    group (rep1 (compl [set "'"]));
    str "' command not found";
  ], (fun m -> Command_dep_ext m.(1));
  seq [ (* 2013/6/26 *)
    opt (str "/bin/");
    alt [str "sh: "; str "env: "];
    opt (seq [rep1 digit; str ": "]);
    group (rep1 (compl [set ":"]));
    str ": ";
    alt [
      seq [
        opt (str "command ");
        str "not found";
      ];
      str "No such file or directory";
    ];
  ], (fun m -> Command_dep_ext m.(1));
  seq [ (* tested 2013/6/21 *)
    str "ocamlfind: Package `";
    group (rep1 (compl [set "'"]));
    str "' not found";
  ], (fun m -> Missing_ocamlfind_dep m.(1));
  seq [ (* tested 2013/6/21 *)
    str "E: Cannot find findlib package ";
    group (rep1 (compl [space]));
    str " (";
    group (rep1 (compl [set ")"]));
  ], (fun m -> Missing_findlib_constraint (m.(1),m.(2)));
  seq [ (* tested 2013/6/21 *)
    str "The following re";
    opt (char 'c');
    str "quired C libraries are missing:";
    group (rep1 (seq [char ' '; rep1 (compl [space])]));
    str ".";
  ], (fun m -> C_lib_dep_exts Re_str.(split (regexp_string " ") m.(1)));
  seq [ (* tested 2013/6/26 *)
    str "ld: library not found for -l";
    group (rep1 notnl);
  ], (fun m -> C_lib_dep_exts [m.(1)]);
  no_space_recognizer;
])

let rec search k str = function
  | [] -> k ()
  | (patt,cons)::r ->
      (try [cons (Re.get_all (Re.exec patt str))]
       with Not_found -> search k str r)

(* given a string `str' and a list of constructors on regex patterns,
   find the last match in `str' for all patterns *)
let rec last_match ?x str = function
  | [] -> begin match x with Some (c,_) -> Some c | None -> None end
  | (patt,cons)::r -> begin match match_global patt str with
      | [] -> last_match ?x str r
      | ms ->
          let maxm = List.fold_left
            (fun (c,z) ((_,z'),m) -> if z > z' then (c,z) else (m,z'))
            ([||],-1) ms
          in begin match x with
            | Some (_,lmofs) when lmofs > snd maxm -> last_match ?x str r
            | _ -> last_match ~x:(cons (fst maxm), snd maxm) str r
          end
  end

(* TODO: catch multiple package failures and ensure they match their errors *)
let build_errors_of_r { Repo.r_args; r_stderr; r_stdout } =
  try
    let pkg = Re.(get (exec pkg_build_error_re r_stderr) 1) in
    let err = match last_match r_stdout build_error_stdout_re with
      | Some c -> [c]
      | None -> search (fun () -> []) r_stderr build_error_stderr_re
    in
    if List.mem pkg r_args
    then err
    else [Dep_error (pkg, err)]
  with _ -> []

let incompatible_error_re = Re.(compile (seq [
  (* tested 2013/6/21 *)
  bol; str "Version "; rep1 (compl [space]);
  str " of \""; rep1 (compl [set "\""]);
  str "\" is incompatible";
]))
let other_errors_of_r { Repo.r_stderr } =
  try
    if Re.execp incompatible_error_re r_stderr
    then [Incompatible]
    else []
  with _ -> []

let system_error_stderr_re = Re.(List.map compile_pair [
  no_space_recognizer;
])
let system_errors_of_r { Repo.r_stderr } =
  try begin match last_match r_stderr system_error_stderr_re with
    | Some c -> [c]
    | None -> []
  end with _ -> []

let analyze = Repo.(function
  | Process (Exited 1,
             ({ r_cmd = "opam" } as r)) -> system_errors_of_r r
  | Process (Exited 3,
             ({ r_cmd = "opam" } as r)) -> solver_errors_of_r r
  | Process (Exited 4,
             ({ r_cmd = "opam" } as r)) -> build_errors_of_r r
  | Process (Exited 66,
             ({ r_cmd = "opam" } as r)) -> other_errors_of_r r
  | Process ((Exited 128 | Stopped _ | Signaled _),
             ({ r_cmd = "git" } as r))  -> system_errors_of_r r
  | _                                   -> []
)

let error_of_exn = Repo.(function
  | ProcessError (status, r) -> Process (status, r)
  | exn -> Other (string_of_sexp (sexp_of_exn exn),
                  if Printexc.backtrace_status ()
                  then "Backtrace:\n"^(Printexc.get_backtrace ())
                  else "No backtrace available.")
)

let bufs_of_error site = Repo.(function
  | Process (Exited code, r) ->
      (Printf.sprintf "%s\nOCAMLOT %s \"%s %s\" failed (%d) in %s\n"
         r.r_stderr site r.r_cmd (String.concat " " r.r_args) code
         (Time.duration_to_string r.r_duration), r.r_stdout)
  | Process (Stopped signum, r)
  | Process (Signaled signum, r) ->
      (Printf.sprintf "%s\nOCAMLOT %s \"%s %s\" terminated by signal %d in %s\n"
         r.r_stderr site r.r_cmd (String.concat " " r.r_args) signum
         (Time.duration_to_string r.r_duration), r.r_stdout)
  | Other (sexn, backtrace) ->
      (Printf.sprintf "OCAMLOT %s terminated by\n%s\n%s\n" site sexn backtrace,
       "")
)

let to_bufs = Repo.(function
  | { status = Passed r; duration } ->
      let facts = Printf.sprintf "OCAMLOT \"%s %s\" succeeded in %s\n"
        r.r_cmd (String.concat " " r.r_args)
        (Time.duration_to_string duration) in
      (facts^r.r_stderr, r.r_stdout)
  | { status = Failed (_, error); duration } ->
      bufs_of_error
        (Printf.sprintf "After %s Opam_task.run"
           (Time.duration_to_string duration))
        error
)

let die site exn =
  let err, out = bufs_of_error site (error_of_exn exn) in
  Printf.eprintf "stdout: %s\nstderr: %s\n%!" out err;
  exit 1

let string_of_system_error = function
  | No_space -> "storage exhausted"

let rec string_of_analysis = function
  | No_solution None -> "no constraint solution"
  | No_solution (Some (Unsatisfied_dep dep)) ->
      "unsatisfied dependency \""^dep^"\""
  | Incompatible -> "incompatible"
  | Error_for_warn -> "error-enabled warnings"
  | Checksum (_, _, _) -> "invalid checksum"
  | Pkg_config_dep_ext pkg -> "no external dependency \""^pkg^"\""
  | Pkg_config_dep_ext_constraint (pkg, bound) ->
      "external dependency \""^pkg^"\" must be \""^bound^"\""
  | Header_dep_ext header -> "no external dependency \""^header^".h\""
  | Command_dep_ext command -> "no external dependency \""^command^"\""
  | C_lib_dep_exts exts -> "no external dependencies: "
      ^(String.concat ", " (List.map (fun ext -> "\""^ext^"\"") exts))
  | Missing_ocamlfind_dep dep -> "missing ocamlfind dependency \""^dep^"\""
  | Missing_findlib_constraint (pkg, bound) ->
      "missing findlib constraint \""^pkg^" "^bound^"\""
  | System_error sys_err -> "system error: "^(string_of_system_error sys_err)
  | Broken_link uri -> "could not retrieve <"^(Uri.to_string uri)^">"
  | Dep_error (dep, subanalyses) ->
      Printf.sprintf "error in dependency \"%s\" (%s)"
        dep (string_of_analysis_list subanalyses)
and string_of_analysis_list = function
  | [] -> "unknown"
  | al -> String.concat ", " (List.map string_of_analysis al)

let string_of_status = function
  | Passed _ -> "PASS"
  | Failed (al,_) -> Printf.sprintf "FAIL (%s)" (string_of_analysis_list al)

let to_html ({ status; duration; info } as t) =
  let err, out = to_bufs t in
  let status_class = match status with
    | Passed _ -> "passed"
    | Failed (_,_) -> "failed"
  in <:html<
  <div class='summary'>
    <span class=$str:"status" ^ status_class$>$str:string_of_status status$</span>
    in
    <span class='duration'>$str:Time.duration_to_string duration$</span>
  </div>
  <span>stderr</span>
  <pre class='stderr'>$str:err$</pre>
  <span>stdout</span>
  <pre class='stdout'>$str:out$</pre>
  <span>environment</span>
  <pre class='info'>$str:info$</pre>
  >>
