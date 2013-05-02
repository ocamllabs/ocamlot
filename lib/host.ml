type os = OpamGlobals.os
(*type isa_exts*)
type arch = X86_64 | X86_32

let string_of_os = OpamGlobals.string_of_os

let string_of_arch = function
  | X86_64 -> "x86-64"
  | X86_32 -> "x86-32"
