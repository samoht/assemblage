open Assemblage


let unit ?dir u = unit u ?dir ~kind:(`OCaml (`Ml, `Normal))

let repro = bin "repro" [ unit "quine" ]

(* FIXME it would be better to generate in gen's root dir
   but the api is not good yet. Need to specify stdout when
   we don't have the part's root yet since it doesn't exist.
   We generate in repro's dir instead, but this kind of
   thing should not be done. *)
let repro_dir = Part.root_path repro
let repro_unit = "quine_repro"
let repro_src = repro_dir / (repro_unit ^ ".ml")
let gen = Bin.gen repro (Conf.const []) ~stdout:repro_src

let quine = bin "quine" [ unit ~dir:repro_dir repro_unit ]

let () = assemble (Project.v "quine" [gen; repro; quine])
