open Assemblage

let units = `Units [ unit "main" (`Path []) ]

let posix =
  let deps = [ pkg "threads.posix" ] in
  bin "hello-pthread" ~deps ~flags:Flags.thread units

let vm =
  let deps = [ pkg "threads.vm" ] in
  bin "hello-vmthread" ~deps ~flags:Flags.vmthread ~native:false units

let () = assemble (project "hello-thread" [posix; vm])
