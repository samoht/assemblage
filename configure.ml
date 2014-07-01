open Project

(* OCamlfind packages *)

let cmdliner = pkg    "cmdliner"
let opam     = pkg    "opam"
let graph    = pkg    "ocamlgraph"
let compiler = pkg    "compiler-libs.toplevel"
let optcomp  = pkg_pp "optcomp"

(* Compilation units *)

let unit = unit ~dir:"lib"

let shell      = unit []                           "shell"
let git        = unit [shell]                      "git"
let project    = unit [cmdliner; graph; git]       "project"
let build_env  = unit [project]                    "build_env"
let ocamlfind  = unit [shell; project]             "ocamlfind"
let ocaml      = unit [project; optcomp; compiler] "OCaml"
let opam       = unit [opam; ocamlfind; project]   "opam"
let makefile   = unit [project; ocamlfind]         "makefile"
let assemblage =
  let deps = [compiler; shell; project; opam; ocamlfind; makefile; build_env] in
  unit deps "assemblage"

(* Build artifacts *)

let lib = lib "assemblage"

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

let () =
  create
    ~libs:[lib]
    ~bins:[configure; describe]
    ~tests:[camlp4; multi_libs]
    "assemblage"
