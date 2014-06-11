#use "lib/makefile.ml"

let makefile =
  Library.create ~dir:"lib" [Unit.create "makefile"] "makefile"

let () =
  generate (libraries [makefile])
