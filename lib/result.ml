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

type output = {
  err : string;
  out : string;
  info: string;
} with sexp

type solver_error =
  | Unsatisfied_dep of string (* TODO: this is pkg + constraint right now *)
with sexp

type analysis =
  | No_solution of solver_error option
  | Incompatible
  | Error_for_warn
  | Checksum of Uri.t * string * string
  | Pkg_config_dep_ext of string
  | Header_dep_ext of string
  | Missing_ocamlfind_dep of string
  | Missing_findlib_constraint of string * string
  | Broken_link of Uri.t
  | Dep_error of string * analysis
with sexp

type status =
  | Passed
  | Failed of analysis list
with sexp

type t = {
  status : status;
  duration : Time.duration;
  output : output;
} with sexp

let is_failure = function Passed -> false | Failed _ -> true

let rec match_global ?(pos=0) ?(lst=[]) re s =
  let matches = Re.(get_all (exec ~pos re s)) in
  if matches.(0) = ""
  then lst
  else match_global
    ~pos:(pos + (String.length matches.(0)) + 1)
    ~lst:(matches::lst) re s

let unsat_dep_re = Re.(compile (seq [
  str "The dependency ";
  group (rep1 (compl [space]));
  str " of package ";
  group (rep1 (compl [space]));
  str " is not available";
]))
let solver_errors_of_r { Repo.r_args; r_stdout } =
  let matches = match_global unsat_dep_re r_stdout in
  if 0 = List.length matches then [No_solution None]
  else List.fold_left (fun lst m ->
    let err = No_solution (Some (Unsatisfied_dep m.(1))) in
    if List.mem m.(2) r_args
    then err::lst
    else (Dep_error (m.(2), err))::lst
  ) [] matches

let pkg_build_error_re = Re.(compile (seq [
  bol; str "==== ERROR [while installing ";
  group (rep1 (compl [set "]"]));
]))

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
  seq [
    str "configure: error: ";
    group (rep1 (compl [space]));
    str " not found";
  ], (fun m -> Pkg_config_dep_ext m.(1));
  seq [ (* tested 2013/6/21 *)
    str "curl: 404 is not a valid return code.\nCannot get ";
    group (rep1 notnl);
  ], (fun m -> Broken_link (Uri.of_string m.(1)));
])
let build_error_stdout_re = Re.(List.map compile_pair [
  (* tested 2013/6/21 *)
  str "Error: Error-enabled warnings", (fun _ -> Error_for_warn);
  seq [str "Package ";
       group (rep1 (compl [space]));
       str " was not found in the pkg-config search path.";
      ], (fun m -> Pkg_config_dep_ext m.(1));
  seq [str ": fatal error: ";
       group (rep1 (compl [set "."]));
       str ".h: No such file or directory";
      ], (fun m -> Header_dep_ext m.(1));
  seq [str "ocamlfind: Package `";
       group (rep1 (compl [set "'"]));
       str "' not found";
      ], (fun m -> Missing_ocamlfind_dep m.(1));
  seq [str "E: Cannot find findlib package ";
       group (rep1 (compl [space]));
       str " (";
       group (rep1 (compl [set ")"]));
      ], (fun m -> Missing_findlib_constraint (m.(1),m.(2)));
])

let rec search k str = function
  | [] -> k ()
  | (patt,cons)::r ->
      (try cons (Re.get_all (Re.exec patt str))
       with Not_found -> search k str r)

(* TODO: catch multiple package failures and ensure they match their errors *)
let build_errors_of_r { Repo.r_args; r_stderr; r_stdout } =
  try
    let pkg = Re.(get (exec pkg_build_error_re r_stderr) 1) in
    let err = search
      (fun () -> search
        (fun () -> raise (Failure "couldn't match anything"))
        r_stderr build_error_stderr_re)
      r_stdout build_error_stdout_re
    in
    if List.mem pkg r_args
    then [err]
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

let analyze = Repo.(function
  | ProcessError (Unix.WEXITED 3, r) -> solver_errors_of_r r
  | ProcessError (Unix.WEXITED 4, r) -> build_errors_of_r r
  | ProcessError (Unix.WEXITED 66,r) -> other_errors_of_r r
  | exn -> []
)

let rec string_of_analysis = function
  | No_solution None -> "no constraint solution"
  | No_solution (Some (Unsatisfied_dep dep)) ->
      "unsatisfied dependency \""^dep^"\""
  | Incompatible -> "incompatible"
  | Error_for_warn -> "error-enabled warnings"
  | Checksum (_, _, _) -> "invalid checksum"
  | Pkg_config_dep_ext pkg -> "no external dependency \""^pkg^"\""
  | Header_dep_ext header -> "no external dependency \""^header^".h\""
  | Missing_ocamlfind_dep dep -> "missing ocamlfind dependency \""^dep^"\""
  | Missing_findlib_constraint (pkg, bound) ->
      "missing findlib constraint \""^pkg^" "^bound^"\""
  | Broken_link uri -> "could not retrieve <"^(Uri.to_string uri)^">"
  | Dep_error (dep, subanalysis) ->
      Printf.sprintf "error in dependency \"%s\" (%s)"
        dep (string_of_analysis subanalysis)

let string_of_analysis_list = function
  | [] -> "unknown"
  | al -> String.concat ", " (List.map string_of_analysis al)

let string_of_status = function
  | Passed -> "PASSED"
  | Failed al -> Printf.sprintf "FAILED (%s)" (string_of_analysis_list al)

let get_status {status} = status

let to_html { status; duration; output } =
  let status_class = match status with
    | Passed -> "passed"
    | Failed _ -> "failed"
  in <:html<
  <div class='summary'>
    <span class=$str:"status" ^ status_class$>$str:string_of_status status$</span>
    in
    <span class='duration'>$str:Time.duration_to_string duration$</span>
  </div>
  <span>stderr</span>
  <pre class='stderr'>$str:output.err$</pre>
  <span>stdout</span>
  <pre class='stdout'>$str:output.out$</pre>
  <span>environment</span>
  <pre class='info'>$str:output.info$</pre>
  >>
