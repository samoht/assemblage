#use "../../lib/makefile.ml"

let a =
  Library.create ~dir:"a"
    [ Unit.create ~deps:[Depend.library "ezjsonm"] "a"]
    "lib1"

let b =
  Library.create ~dir:"b"
    [ Unit.create ~deps:[Depend.local a] "b"]
    "lib2"

let () =
  generate (libraries [a; b])
