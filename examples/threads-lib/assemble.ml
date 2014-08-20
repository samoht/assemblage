open Assemblage

let units = `Units [ unit "tlib" (`Path []) ]

let posix = lib "tlib-posix" ~flags:Flags.thread units

(* FIXME bytecode only *)
let vm = lib "tlib-vm" ~flags:Flags.vmthread units

let () = assemble (project "threads-lib" [posix; vm])
