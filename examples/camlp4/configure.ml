open Project

let t = Unit.create ~deps:[
    Dep.pkg_pp "sexplib.syntax";
    Dep.pkg_pp "comparelib.syntax";
    Dep.pkg    "sexplib";
    Dep.pkg    "comparelib";
    Dep.pkg    "xmlm";
  ] "t"

let lib = Lib.create [t] "mylib"

let () =
  create ~libs:[lib] "camlp4o"
