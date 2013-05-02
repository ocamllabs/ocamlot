type t = float
type duration = float

let min = 0.
let now () = Unix.gettimeofday ()
let to_string t = Unix.(
  let tm = gmtime t in
  Printf.sprintf "%d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec
)
let elapsed t_0 t_1 = abs_float (t_0 -. t_1)
