#use "../../lib/makefile.ml"

let t = Unit.create ~deps:[
    Depend.camlp4o "sexplib.syntax";
    Depend.library "xmlm";
  ] "t"

let lib = Library.create [t] "mylib"

let () =
  generate (libraries [lib])
