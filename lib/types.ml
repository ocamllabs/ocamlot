type error = {
  err : string;
  out : string;
  info: string;
}

type result =
  | OK of float
  | Error of error

type package = {
  name   : string;
  version: string;
}

type compiler = {
  c_version : string;
  c_build   : string;
}

type t = {
  arch      : string;
  os        : string;
  compiler  : string;
  package   : package;
  depends   : package Set.t;
  extdepends: string;
}


