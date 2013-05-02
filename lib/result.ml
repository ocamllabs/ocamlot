type output = {
  err : string;
  out : string;
  info: string;
}

type status = [ `Passed | `Failed ]

type t = {
  status : status;
  duration : Time.duration;
  output : output;
}
