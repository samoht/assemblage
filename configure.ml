open Project

(* OCamlfind packages *)

let cmdliner = Dep.pkg "cmdliner"
let opam     = Dep.pkg "opam"

(* Compilation units *)

let dir = "lib"
let e = Unit.create ~dir ~deps:[cmdliner]            "env"
let p = Unit.create ~dir ~deps:[cmdliner]            "project"
let f = Unit.create ~dir ~deps:[Dep.unit p]          "ocamlfind"
let o = Unit.create ~dir ~deps:[opam; Dep.unit p]    "opam"
let m = Unit.create ~dir ~deps:(Dep.units [p; f])    "makefile"
let t = Unit.create ~dir ~deps:(Dep.units [m; f; o]) "tools"

(* Build artifacts *)

let lib = Lib.create [p; f; o; m; t] "tools"

let top =
  Top.create ~deps:[Dep.lib lib] "configure.top"

let bin =
  let main = Unit.create ~dir ~deps:[Dep.pkg "unix"; Dep.lib lib] "opam_configure" in
  Bin.create ~deps:[Dep.unit main] "opam-configure"

(* The project *)

let t =
  create ~libs:[lib] ~tops:[top] ~bins:[bin] ~version:"0.1" "tools"

let () =
  Tools.generate t `Makefile
