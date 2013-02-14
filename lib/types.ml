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

type t = {
  os        : string;
  package   : package;
  depends   : package list;
  extdepends: string;
}


