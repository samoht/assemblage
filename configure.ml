open Project

(* OCamlfind packages *)

let cmdliner = Dep.pkg    "cmdliner"
let opam     = Dep.pkg    "opam"
let graph    = Dep.pkg    "ocamlgraph"
let compiler = Dep.pkg    "compiler-libs.toplevel"
let optcomp  = Dep.pkg_pp "optcomp"

(* Compilation units *)

let unit deps name = Unit.create ~dir:"lib" ~deps name
let s = unit []                              "shell"
let g = unit []                              "git"
let p = unit [cmdliner; graph]               "project"
let e = unit [Dep.unit p]                    "build_env"
let f = unit (Dep.units [s;p])               "ocamlfind"
let c = unit [Dep.unit p; optcomp; compiler] "OCaml"
let o = unit [opam; Dep.unit c; Dep.unit p]  "opam"
let m = unit (Dep.units [p; f])              "makefile"
let t =
  let deps = compiler :: Dep.units [s; p; o; f; m; e] in
  unit deps "assemblage"

(* Build artifacts *)

let lib =
  Lib.create [g; s; p; e; f; c; o; m; t] "assemblage"

let configure =
  let c = Unit.create ~dir:"bin" ~deps:[Dep.lib lib] "configure" in
  Bin.create ~link_all:true ~byte_only:true [c] "configure.ml"

let describe =
  let c = Unit.create ~dir:"bin" ~deps:[Dep.lib lib] "describe" in
  Bin.create ~link_all:true ~byte_only:true [c] "describe.ml"

(* Tests *)

let mk_test name =
  let dir = "examples/" ^ name in
  Test.create ~dir describe [
    "--disable-auto-load";
    "-I"; Printf.sprintf "../../_build/%s" (Lib.id lib)
  ] name

let camlp4     = mk_test "camlp4"
let multi_libs = mk_test "multi-libs"

(* The project *)

let version = "0.1"

let () =
  let version = version ^ match Git.version () with
    | None   -> ""
    | Some v -> "~" ^ v in
  create
    ~libs:[lib]
    ~bins:[configure; describe]
    ~tests:[camlp4; multi_libs]
    ~css:"style.css"
    ~version
    "assemblage"
