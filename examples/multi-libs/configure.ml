open Project

let a =
  Lib.create
    [ Comp.create ~dir:"a" ~deps:[Dep.pkg "ezjsonm"] "a"]
    "lib1"

let b =
  let b = Comp.create ~dir:"b" ~deps:[`Lib a] "b" in
  let c = Comp.create ~dir:"b" ~deps:[`Comp b] "c" in
  Lib.create [b; c] "lib2"

let () =
  create ~libs:[a; b] "multi-libs"
