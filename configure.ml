open Project

(* OCamlfind packages *)

let cmdliner = Dep.pkg "cmdliner"
let opam     = Dep.pkg "opam"
let graph    = Dep.pkg "ocamlgraph"
let compiler = Dep.pkg "compiler-libs.toplevel"

(* Compilation units *)

let dir = "lib"
let s = Unit.create ~dir                             "shell"
let g = Unit.create ~dir                             "git"
let p = Unit.create ~dir ~deps:[cmdliner; graph]     "project"
let e = Unit.create ~dir ~deps:[Dep.unit p]          "build_env"
let f = Unit.create ~dir ~deps:(Dep.units [s;p])     "ocamlfind"
let o = Unit.create ~dir ~deps:[opam; Dep.unit p]    "opam"
let m = Unit.create ~dir ~deps:(Dep.units [p; f])    "makefile"
let t =
  Unit.create ~dir
    ~deps:(compiler :: Dep.units [s; p; o; f; m; e])
    "tools"

(* Build artifacts *)

let lib =
  Lib.create [g; s; p; e; f; o; m; t] "tools"

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
    "--disable-auto-load-tools";
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
    ~version
    "tools"
