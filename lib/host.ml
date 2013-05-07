type os = OpamGlobals.os
(*type isa_exts*)
(* TODO: differences? compatibilities? worth it? *)
type arch = X86_64 | I386 | I686 | Armv61 | Unknown
type t = { os : os; arch : arch }

let string_of_os = OpamGlobals.string_of_os

let string_of_arch = function
  | X86_64 -> "x86_64"
  | I386 -> "i386"
  | I686 -> "i686"
  | Armv61 -> "armv61"
  | Unknown -> "unknown"

let arch_of_string_opt = function
  | Some "x86_64" -> X86_64
  | Some "amd64" -> X86_64
  | Some "i386" -> I386
  | Some "i686" -> I686
  | Some "armv61" -> Armv61
  | Some _ | None -> Unknown

(* copied from OpamMisc :-/ *)
let with_process_in cmd f =
  let ic = Unix.open_process_in cmd in
  try
    let r = f ic in
    ignore (Unix.close_process_in ic) ; r
  with exn ->
    ignore (Unix.close_process_in ic) ; raise exn

let uname_m () =
  try with_process_in "uname -m"
        (fun ic -> Some (OpamMisc.strip (input_line ic)))
  with _ -> None

let detect () = {
  os=OpamGlobals.os ();
  arch=arch_of_string_opt (uname_m ());
}
