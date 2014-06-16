open Project

let dir = "lib"
let p = Unit.create ~dir ~deps:[Dep.lib "cmdliner"] "project"
let o = Unit.create ~dir ~deps:[Dep.unit p]         "ocamlfind"
let m = Unit.create ~dir ~deps:(Dep.units [p;o])    "makefile"

let lib = Lib.create [p;o;m] "tools"

let top = Top.create [lib] "configure.top"

let bin =
  let main =
    Unit.create ~dir ~deps:[Dep.lib "unix"; Dep.local_lib lib] "opam_configure" in
  Bin.create [] [main] "opam-configure"

let () =
  Makefile.of_project (create ~libs:[lib] ~tops:[top] ~bins:[bin] ())
