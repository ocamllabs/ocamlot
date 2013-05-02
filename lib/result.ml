type error = {
  err : string;
  out : string;
  info: string;
}

type t =
  | OK of Time.duration
  | Error of error
