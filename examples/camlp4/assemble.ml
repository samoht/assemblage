open Assemblage

let t = unit "t" ~needs:[
    pkg "sexplib.syntax";
    pkg "comparelib.syntax";
    pkg "sexplib";
    pkg "comparelib";
    pkg "xmlm"; ]

let lib = lib "mylib" [t]
let () = assemble (Project.v "camlp4o" [lib])
