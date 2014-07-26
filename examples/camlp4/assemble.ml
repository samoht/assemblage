open Assemblage

let t = unit "t" (`Dir ".") ~deps:[
    pkg_pp "sexplib.syntax";
    pkg_pp "comparelib.syntax";
    pkg    "sexplib";
    pkg    "comparelib";
    pkg    "xmlm";
  ]

let lib = lib "mylib" [t]
let () = add (create "camlp4o" [lib])
