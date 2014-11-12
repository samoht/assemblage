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

(* Metadata *)

type ocaml_interface = [ `Normal | `Opaque | `Hidden ]
type ocaml_unit = [ `Mli | `Ml | `Both ]
type c_unit = [ `C | `H | `Both ]

type kind =
  [ `OCaml of ocaml_unit * ocaml_interface
  | `C of c_unit
  | `Js ]

let pp_kind ppf k = As_fmt.pp_str ppf begin match k with
  | `OCaml _ -> "OCaml" | `C _ -> "C" | `Js -> "JavaScript"
  end

type meta = { kind : kind; dir : As_path.t As_conf.value }
let meta_deps m = As_conf.deps m.dir
let inj, proj = As_part.meta_key meta_deps
let get_meta unit = As_part.get_meta proj unit
let meta ?(dir = As_conf.(value root_dir)) kind = inj { kind; dir }

let kind unit = (get_meta unit).kind
let dir unit = (get_meta unit).dir

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

let src e unit =               (* source file for the unit with extention e *)
  let mk_file d = As_path.(d / (As_part.name unit) + e) in
  As_conf.(const mk_file $ dir unit)

(* Check *)

let check p =
  let unit = As_part.coerce `Unit p in
  As_log.warn "%a part check is TODO" As_part.pp_kind (As_part.kind unit);
  true

(* Actions *)

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

let add_if c f v acc = if c then (f v) :: acc else acc

let js_actions unit =
  let src = src `Js unit in
  let dst = As_part.rooted unit (As_part.name unit) ~ext:`Js in
  As_action.link ~src ~dst () :: []

let c_actions spec unit =
  (* FIXME for C I think we want to distinguish two backends
     one that goes through ocamlc and the other who goes to Conf.cc.
     This should be reflected in the metadata. *)
  let has_h, has_c = match spec with
  | `H -> true, false | `C -> false, true | `Both -> true, true
  in
  let src_h = src `H unit in
  let src_c = src `C unit in
  let dst_h = As_part.rooted unit (As_part.name unit) ~ext:`H in
  let dst_c = As_part.rooted unit (As_part.name unit) ~ext:`C in
  add_if has_h (As_action.link ~src:src_h ~dst:dst_h) () @@
  add_if has_c (As_action.link ~src:src_c ~dst:dst_c) () @@
  []

let ocaml_actions spec unit =
  let has_mli, has_ml = match spec with
  | `Mli -> true, false | `Ml -> false, true | `Both -> true, true
  in
  let has_mli_v = As_conf.(const has_mli) in
  let incs = As_conf.(const []) in (* FIXME *)
  let src_mli = src `Mli unit in
  let src_ml = src `Ml unit in
  let mli = As_part.rooted unit (As_part.name unit) ~ext:`Mli in
  let ml = As_part.rooted unit (As_part.name unit) ~ext:`Ml in
  add_if has_mli (As_action.link ~src:src_mli ~dst:mli) () @@
  add_if has_ml (As_action.link ~src:src_ml ~dst:ml) () @@
  add_if has_mli (As_action_ocaml.compile_mli ~incs ~src:mli) () @@
  add_if has_ml (As_action_ocaml.compile_ml_byte
                   ~has_mli:has_mli_v ~incs ~src:ml) () @@
  add_if has_ml (As_action_ocaml.compile_ml_native
                   ~has_mli:has_mli_v ~incs ~src:ml) () @@
  []

let actions p =
  let unit = As_part.coerce `Unit p in
  match kind unit with
  | `C spec -> c_actions spec unit
  | `Js -> js_actions unit
  | `OCaml (spec, _) -> ocaml_actions spec unit

(* Create *)

let v ?usage ?cond ?args ?needs ?dir name kind =
  let meta = meta ?dir kind in
  As_part.v_kind ?usage ?cond ?args ~meta ?needs ~actions ~check name `Unit

let of_base ?dir kind p =
  let meta = meta ?dir kind in
  As_part.with_kind_meta `Unit meta p
