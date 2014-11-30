open Assemblage

let units = [ unit "tlib" ]
let posix = lib "tlib-posix" ~args:Args.thread units
let vm =
  lib "tlib-vm" ~native:false ~native_dynlink:false ~args:Args.vmthread units

let () = assemble (Project.v "threads-lib" [posix; vm])
