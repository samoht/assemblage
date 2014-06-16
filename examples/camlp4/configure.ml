#directory "../../_build/lib/";;
open Project

let t = Unit.create ~deps:[
    Dep.p4o "sexplib.syntax";
    Dep.lib "xmlm";
  ] "t"

let lib = Lib.create [t] "mylib"

let conf = Project.Conf.create ~native:false ()

let () =
  Makefile.of_project (create ~libs:[lib] ~conf ())
