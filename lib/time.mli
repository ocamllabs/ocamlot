type t
type duration

val min : t
val now : unit -> t
val to_string : t -> string
val elapsed : t -> t -> duration
