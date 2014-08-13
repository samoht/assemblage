open Assemblage

let t = unit "t" (`Path []) ~deps:[
    pkg_pp "sexplib.syntax";
    pkg_pp "comparelib.syntax";
    pkg    "sexplib";
    pkg    "comparelib";
    pkg    "xmlm";
  ]

let lib = lib "mylib" (`Units [t])
let () = assemble (project "camlp4o" [lib])
