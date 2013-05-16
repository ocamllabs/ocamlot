open Sexplib.Std

type output = {
  err : string;
  out : string;
  info: string;
} with sexp

type status =
  | Passed
  | Failed
with sexp

type t = {
  status : status;
  duration : Time.duration;
  output : output;
} with sexp

let string_of_status = function
  | Passed -> "PASSED"
  | Failed -> "FAILED"

let get_status {status} = status

let to_html { status; duration; output } =
  let status_class = match status with
    | Passed -> "passed"
    | Failed -> "failed"
  in
  Printf.sprintf "<div class='summary'><span class='status %s'>%s</span> in <span class='duration'>%s</span></div>\n<pre class='info'>%s</pre>\n<pre class='stdout'>%s</pre>\n<pre class='stderr'>%s</pre>"
    status_class (string_of_status status) (Time.duration_to_string duration)
    output.info
    output.out
    output.err
