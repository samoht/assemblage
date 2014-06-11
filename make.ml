open Makefile

let () =
  let makefile = Unit.create "makefile" in
  let lib      = Library.create ~dir:"lib" [makefile] "makefile" in
  generate (ocaml [lib])
