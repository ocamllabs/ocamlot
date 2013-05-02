type t = float
type duration = float

let min = 0.
let now () = Unix.gettimeofday ()
let date_to_string_ tm = Unix.(
  Printf.sprintf "%d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
)
let to_string_ tm = Unix.(
  Printf.sprintf "%sT%02d:%02d:%02dZ"
    (date_to_string_ tm)
    tm.tm_hour tm.tm_min tm.tm_sec
)
let date_to_string t = date_to_string_ (Unix.gmtime t)
let duration_to_string t = Printf.sprintf "%.2fs" t
let to_string t = to_string_ (Unix.gmtime t)
let elapsed t_0 t_1 = abs_float (t_0 -. t_1)
