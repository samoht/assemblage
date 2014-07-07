open Assemblage

let t = cu [
    pkg_pp "sexplib.syntax";
    pkg_pp "comparelib.syntax";
    pkg    "sexplib";
    pkg    "comparelib";
    pkg    "xmlm";
  ] "t"

let lib =
  lib [t] "mylib"

let () =
  create [lib] "camlp4o"
