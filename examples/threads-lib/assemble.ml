open Assemblage

let units = `Units [ unit "tlib" (`Path []) ]

let posix = lib "tlib-posix" ~flags:Flags.thread units
let vm =
  lib "tlib-vm" ~native:false ~native_dynlink:false ~flags:Flags.vmthread units

let () = assemble (project "threads-lib" [posix; vm])
