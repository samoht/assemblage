#directory "../../_build/lib/";;
open Project

let a = Lib.create
    [ Unit.create ~dir:"a" ~deps:[Dep.lib "ezjsonm"] "a"]
    "lib1"

let b =
  let b = Unit.create ~dir:"b" ~deps:[Dep.local_lib a] "b" in
  let c = Unit.create ~dir:"b" ~deps:[Dep.unit b] "c" in
  Lib.create [b; c] "lib2"

let () =
  Makefile.of_project (create ~libs:[a; b] ())
