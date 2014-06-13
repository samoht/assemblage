#directory "../../lib/_build";;
#load "project.cmo";;
#load "makefile.cmo";;

open Project

let t = Unit.create ~deps:[
    Dep.findp4o "sexplib.syntax";
    Dep.findlib "xmlm";
  ] "t"

let lib = Lib.create [t] "mylib"

let conf = Project.Conf.create ~native:false ()

let () =
  Makefile.of_project (create ~libs:[lib] conf)
