open Assemblage

let t = unit "t" ~deps:[
    pkg ~kind:(`OCaml_pp `OCamlfind) "sexplib.syntax";
    pkg ~kind:(`OCaml_pp `OCamlfind) "comparelib.syntax";
    pkg "sexplib";
    pkg "comparelib";
    pkg "xmlm";
  ]

let lib = lib "mylib" [t]
let () = assemble (project "camlp4o" [lib])
