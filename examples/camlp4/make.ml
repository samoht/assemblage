#use "../../lib/makefile.ml"

let t = Unit.create ~deps:[
    Depend.camlp4o "sexplib.syntax";
    Depend.library "xmlm";
  ] "t"

let () =
  let main = ocaml [Library.create [t] "mylib"] in
  generate main
