type t with sexp
type duration with sexp

val min : t
val now : unit -> t
val to_string : t -> string
val date_to_string : t -> string
val duration_to_string : duration -> string
val elapsed : t -> t -> duration
