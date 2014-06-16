#directory "../../_build/lib/";;
#directory "../../_build/tools/";;

open Project

let t = Unit.create ~deps:[
    Dep.pkg_p4o "sexplib.syntax";
    Dep.pkg     "xmlm";
  ] "t"

let lib = Lib.create [t] "mylib"

let conf = Project.Conf.create ~native:false ()

let () =
  Makefile.of_project (create ~libs:[lib] ~conf ())
