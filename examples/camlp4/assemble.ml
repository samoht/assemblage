open Assemblage

let t = unit "t" [
    pkg_pp "sexplib.syntax";
    pkg_pp "comparelib.syntax";
    pkg    "sexplib";
    pkg    "comparelib";
    pkg    "xmlm";
  ]
    
let lib =
  lib "mylib" [t]

let () =
  create "camlp4o" [lib]
