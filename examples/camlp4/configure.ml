open Project

let t = Comp.create ~deps:[
    pkg_pp "sexplib.syntax";
    pkg_pp "comparelib.syntax";
    pkg    "sexplib";
    pkg    "comparelib";
    pkg    "xmlm";
  ] "t"

let lib =
  Lib.create [t] "mylib"

let () =
  create ~libs:[lib] "camlp4o"
