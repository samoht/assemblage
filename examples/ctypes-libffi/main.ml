
open Ctypes

let getenv = Foreign.foreign "getenv" (string @-> returning string_opt)

let () = match (getenv "USER") with
| Some u -> print_endline u
| None -> ()
