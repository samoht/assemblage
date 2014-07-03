open Project

(* OCamlfind packages *)

let cmdliner = pkg    "cmdliner"
let graph    = pkg    "ocamlgraph"
let compiler = pkg    "compiler-libs.toplevel"
let optcomp  = pkg_pp "optcomp"

(* Compilation units *)

let shell      = comp []                           "shell"
let git        = comp [shell]                      "git"
let flags      = comp []                           "flags"
let resolver   = comp [flags]                      "resolver"
let feature    = comp [cmdliner]                   "feature"
let action     = comp [shell]                      "action"
let project    = comp [graph; git; resolver;
                       action; feature; flags]     "project"
let build_env  = comp [flags; feature; cmdliner]   "build_env"
let ocamlfind  = comp [shell; project]             "ocamlfind"
let ocaml      = comp [project; optcomp; compiler] "OCaml"
let opam       = comp [ocamlfind; project]         "opam"
let makefile   = comp [project; ocamlfind]         "makefile"
let assemblage =
  let deps = [compiler; shell; project; opam; ocamlfind; makefile; build_env] in
  comp deps "assemblage"

(* Build artifacts *)

let lib =
  lib "assemblage"

let configure =
  bin ~link_all:true ~byte_only:true [`Lib lib] ["configure"] "configure.ml"

let describe =
  bin ~link_all:true ~byte_only:true [`Lib lib] ["describe"] "describe.ml"

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
