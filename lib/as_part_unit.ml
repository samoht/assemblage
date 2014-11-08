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

type ocaml_interface = [ `Normal | `Opaque | `Hidden ]
type ocaml_unit = [ `Mli | `Ml | `Both ]
type c_unit = [ `C | `H | `Both ]

type kind =
  [ `OCaml of ocaml_unit * ocaml_interface
  | `C of c_unit
  | `Js ]

type meta =
  { kind : kind;
    src_dir : As_path.rel }

let inj, proj = As_part.meta_key ()
let get_meta p = As_part.get_meta proj p
let meta kind src_dir = inj { kind; src_dir }

let kind p = (get_meta p).kind
let src_dir p = (get_meta p).src_dir

let is_kind k p = match As_part.coerce_if `Unit p with
| None -> None
| Some p as r ->
  match kind p with
  | `OCaml _ when k = `OCaml -> r
  | `C _ when k = `C -> r
  | `Js when k = `Js -> r
  | _ -> None

let ocaml = is_kind `OCaml
let js = is_kind `Js
let c = is_kind `C

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
  let meta = meta kind src_dir in
  let args _ = args in
  As_part.create ?cond ~args ?deps name `Unit meta

let of_base ~src_dir kind p =
  let meta = meta kind src_dir in
  { p with As_part.kind = `Unit; meta }
