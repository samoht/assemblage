open Project

let dir = "lib"
let g = Unit.create ~dir                                        "git"
let p = Unit.create ~dir ~deps:[Dep.lib "cmdliner"; Dep.unit g] "project"
let f = Unit.create ~dir ~deps:[Dep.unit p]                     "ocamlfind"
let o = Unit.create ~dir ~deps:[Dep.lib "opam"; Dep.unit p]     "opam"
let m = Unit.create ~dir ~deps:(Dep.units [p; f])               "makefile"
let t = Unit.create ~dir ~deps:(Dep.units [m; f; o])            "tools"

let lib = Lib.create [g; p; f; o; m; t] "tools"

let top = Top.create [lib] "configure.top"

let bin =
  let main =
    Unit.create ~dir ~deps:[Dep.lib "unix"; Dep.local_lib lib] "opam_configure" in
  Bin.create [] [main] "opam-configure"

let () =
  Tools.generate (create ~libs:[lib] ~tops:[top] ~bins:[bin] ()) `Makefile
