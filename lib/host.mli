type os =
    Darwin
  | Linux
  | FreeBSD
  | OpenBSD
  | NetBSD
  | DragonFly
  | Cygwin
  | Win32
  | Unix
  | Other of string
with sexp

type arch =
  | X86_64 
  | I386 
  | I686 
  | Armv5tel
  | Armv61 
  | PPC64
  | Powerpc
  | Unknown
with sexp

type t = { os : os; arch : arch; } with sexp

val string_of_os : os -> string
val os_of_string_opt : string option -> os
val string_of_arch : arch -> string
val arch_of_string_opt : string option -> arch
val to_string : t -> string
val detect : unit -> t
