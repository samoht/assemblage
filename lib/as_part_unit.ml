(*
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2014 Daniel C. Bünzli
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let str = Printf.sprintf

type unit_ocaml_interface = [ `Normal | `Opaque | `Hidden ]
type unit_ocaml_unit = [ `Mli | `Ml | `Both ]
type unit_c_unit = [ `C | `H | `Both ]
type unit_kind =
  [ `OCaml of unit_ocaml_unit * unit_ocaml_interface
  | `C of unit_c_unit
  | `Js ]
type unit_meta =
  { unit_kind : unit_kind;
    unit_src_dir : As_path.rel }

(*
let unit_get_meta p = match p.p_meta with Unit m -> m | _ -> assert false
let unit_kind p = (unit_get_meta p).unit_kind
let unit_src_dir p = (unit_get_meta p).unit_src_dir
let unit_js p = part_filter `Unit unit_kind `Js p
let unit_ocaml p = match coerce_if `Unit p with
| None -> None
| Some part as p -> match unit_kind part with `OCaml _ -> p | _ -> None

let unit_c p = match coerce_if `Unit p with
| None -> None
| Some part as p -> match unit_kind part with `C _ -> p | _ -> None

*)

(* Metadata *)

type ocaml_interface = unit_ocaml_interface
type ocaml_unit = unit_ocaml_unit
type c_unit = unit_c_unit
type kind = unit_kind

let meta kind src_dir = failwith "TODO"
let get_meta m = failwith "TODO"
let kind u = failwith "TODO"
let src_dir u = failwith "TODO"


(*
let meta kind src_dir = Unit { unit_kind = kind; unit_src_dir = src_dir }
let get_meta = unit_get_meta
let kind = unit_kind
let src_dir = unit_src_dir
*)

(* Rules *)

(*
  let unit_file fext env u =
    As_path.(as_rel (As_env.build_dir env // (file (name u)) + fext))

  let unit_args u =
    let pkgs = keep_kind `Pkg (deps u) in
    let pkgs_args = As_args.concat (List.map args pkgs) in
    let libs = keep_kind `Lib (deps u) in
    let lib_args lib =
      let cma = List.filter (As_product.has_ext `Cma) (products lib) in
      let cmxa = List.filter (As_product.has_ext `Cmxa) (products lib) in
      match lib_kind lib with
      | `OCaml ->
          let inc ctxs a = As_product.dirname_to_args ~pre:["-I"] ctxs a in
          let prod ctxs a = As_product.target_to_args ctxs a in
          let cma_inc = List.map (inc [`Compile `Byte; `Link `Byte]) cma in
          let cma_prod = List.map (prod [`Link `Byte]) cma in
          let cmxa_inc = List.map (inc [`Compile `Native;`Link `Native]) cmxa in
          let cmxa_prod = List.map (prod [`Link `Native]) cmxa in
          As_args.concat (cma_inc @ cma_prod @ cmxa_inc @ cmxa_prod)
      | `OCaml_pp ->
          let prod a = As_product.target_to_args [`Pp `Byte; `Pp `Native] a in
          As_args.concat (List.map prod cma)
      | `C ->
          As_args.empty
    in
    As_args.(args env u @@@ pkgs_args @@@ concat (List.map lib_args libs))

  let link_src fext env u =
    let cond = cond u in
    let dst = unit_file fext env u in
    let src = As_path.(as_rel (src_dir env u / (basename dst))) in
    As_action.link ~cond env ~src ~dst

  let js_rules u env = [ link_src `Js u env ]
  let js_args u env = As_args.empty

  let rec c_rules c_unit env u = match c_unit with
  | `H -> [ link_src `H env u ]
  | `C -> [ link_src `C env u; c_compile env u; ]
  | `Both -> c_rules `H env u @ c_rules `C env u

  let ocamlpp_ext fext ctx =
    let kind = match fext with `Ml -> "cml" | `Mli -> "cmli" in
    let ctx = match ctx with `Byte -> "byte" | `Native -> "native" in
    `Ext (str "%s-%s" kind ctx)

  let rec ocaml_rules unit env u = match unit with
  | `Mli ->
      [ link_src `Mli env u;
        ocaml_pp `Mli `Byte env u;
        ocaml_compile_mli env u; ]
  | `Ml ->
      [ link_src `Ml env u;
        ocaml_pp `Ml `Byte env u;
        ocaml_pp `Ml `Native env u;
        ocaml_compile_ml_byte env u;
        ocaml_compile_ml_native env u; ]
  | `Both ->
      ocaml_rules `Mli env u @ ocaml_rules `Ml env u
*)
  let actions p = []




(*
    let u = coerce `Unit p in
    match kind u with
    | `Js -> js_rules env u
    | `C unit -> c_rules unit env u
    | `OCaml (unit, _) -> ocaml_rules unit env u
*)

(* Create *)

let create ?cond ?(args = As_args.empty) ?deps
    ?(src_dir = As_path.current) name kind =
(*  let meta = meta kind src_dir in *)
  let args _ = args in
  (Obj.magic (As_part.Base.create ?cond ~args ?deps name (fun _ -> [])))

(*
  create ?cond ~args ?deps ~actions name `Unit meta
*)
let of_base ~src_dir kind p = failwith "TODO"
(*
  { p with p_kind = `Unit;
           p_meta = meta kind src_dir }
*)
let check_set s = s (* TODO *)

(* Part filters *)

(*
let ocaml = unit_ocaml
let c = unit_c
let js = unit_js
*)
