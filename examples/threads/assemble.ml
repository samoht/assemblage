open Assemblage

let pkg_threads_posix = pkg "threads.posix"
let pkg_threads_vm = pkg "threads.vm"

let main = unit "main"
let posix = bin "hello-pthread" ~args:Args.thread [ pkg_threads_posix; main ]
let vm =
  bin "hello-vmthread" ~args:Args.vmthread ~native:false
    [ pkg_threads_vm; main ]

let () = assemble (Project.v "hello-thread" [posix; vm])
