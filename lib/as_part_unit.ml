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
let inj, proj = As_part.meta_key ()
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

(* Check *)

let check p =
  let unit = As_part.coerce `Unit p in
  As_log.warn "%a part check is TODO" As_part.pp_kind (As_part.kind unit);
  As_conf.true_

(* Actions *)

let js_actions unit src_dir dst_dir =
  let actions symlink src_dir dst_dir =
    let name = As_part.name unit in
    let src = As_path.(src_dir / name + `Js) in
    let dst = As_path.(dst_dir / name + `Js) in
    [symlink src dst]
  in
  As_conf.(const actions $ As_action.symlink $ src_dir $ dst_dir)

let c_actions spec unit src_dir dst_dir =
  (* FIXME for C I think we want to distinguish two backends
     one that goes through ocamlc and the other who goes to Conf.cc.
     Maybe this should be reflected in the metadata. *)
  let actions symlink ocamlc ocamlopt native debug warn_error src_dir dst_dir =
    let open As_acmd.Args in
    As_log.warn "Full C unit part support is TODO";
    let has_h, has_c = match spec with
    | `H -> true, false | `C -> false, true | `Both -> true, true
    in
    let name = As_part.name unit in
    let src_h = As_path.(src_dir / name + `H) in
    let src_c = As_path.(src_dir / name + `C) in
    let h = As_path.(dst_dir / name + `H) in
    let c = As_path.(dst_dir / name + `C) in
    (* ccomp is here so that we don't fail if we don't have ocamlc *)
    let ccomp = if native then ocamlopt else ocamlc in
    let args =
      add_if debug "-g" @@ adds_if warn_error [ "-ccopt"; "-Werror" ] @@ []
    in
    add_if has_h (symlink src_h h) @@
    add_if has_c (symlink src_c c) @@
    fadd_if has_c
      (As_action_ocaml.compile_c ~args ~ocamlc:ccomp ~src:c) () @@ []
  in
  As_conf.(const actions $
           As_action.symlink $ As_acmd.cmd ocamlc $ As_acmd.cmd ocamlopt $
           value ocaml_native $ value debug $ value warn_error $
           src_dir $ dst_dir)

let ocaml_actions spec unit src_dir dst_dir =
  let actions symlink stamp ocamldep ocamlc ocamlopt debug profile warn_error
      annot byte native pkgs libs_pp_actions libs_actions src_dir dst_dir =
    let open As_acmd.Args in
    let has_mli, has_ml = match spec with
    | `Mli -> true, false | `Ml -> false, true | `Both -> true, true
    in
    let _pp =
      let outs = As_action.list_outputs libs_pp_actions in
      List.filter (As_path.has_ext `Cma) outs
    in
    let incs_byte, incs_native =
      let outs = As_action.list_outputs libs_actions in
      List.map As_path.dirname (List.filter (As_path.has_ext `Cma) outs),
      List.map As_path.dirname (List.filter (As_path.has_ext `Cmxa) outs)
    in
    let all_incs = adds incs_byte @@ adds incs_native @@ [dst_dir] in
    let incs_byte = adds incs_byte @@ [dst_dir]in
    let incs_native = adds incs_native @@ [dst_dir]in
    let name = As_part.name unit in
    let src_mli = As_path.(src_dir / name + `Mli) in
    let src_ml = As_path.(src_dir / name + `Ml) in
    let mli = As_path.(dst_dir / name + `Mli) in
    let ml = As_path.(dst_dir / name + `Ml) in
    (* mlicomp is here so that we don't fail if we don't have ocamlc *)
    let mlicomp = if native then ocamlopt else ocamlc in
    let args =
      add_if debug "-g" @@ adds_if warn_error [ "-warn_error"; "+a" ] @@ []
    in
    let ocamldep_args = add_if native "-native" @@ [] in
    add_if has_mli (symlink src_mli mli) @@
    add_if has_ml (symlink src_ml ml) @@
    add_if has_mli (As_action_ocaml.prepare ~stamp ~src:mli) @@
    add_if has_ml (As_action_ocaml.prepare ~stamp ~src:ml) @@
    fadd_if has_mli
      (As_action_ocaml.compute_deps_mli ~ocamldep ~pkgs ~args:ocamldep_args
         ~src: mli ~incs:all_incs) () @@
    fadd_if has_ml
      (As_action_ocaml.compute_deps_ml ~ocamldep ~pkgs ~args:ocamldep_args
         ~src:ml ~incs:all_incs) () @@
    fadd_if has_mli
      (As_action_ocaml.compile_mli
         ~ocamlc:mlicomp ~pkgs ~args ~annot
         ~target:(if native then `Target `Native else `Target `Byte)
         ~incs:(if native then incs_native else incs_byte) ~src:mli) () @@
    fadd_if (has_ml && byte)
      (As_action_ocaml.compile_ml_byte
         ~ocamlc ~pkgs ~args ~annot:(byte && not native)
         ~has_mli ~incs:incs_byte ~src:ml) () @@
    fadd_if (has_ml && native)
      (As_action_ocaml.compile_ml_native
         ~ocamlopt ~pkgs ~args:(add_if profile "-p" @@ args)
         ~annot ~has_mli ~incs:incs_native ~src:ml) () @@ []
  in
  let needs = As_part.needs unit in
  let libs_pp = As_part.list_keep_map As_part_lib.ocaml_pp needs in
  let libs = As_part.list_keep_map As_part_lib.ocaml needs in
  let pkgs = As_part_pkg.list_lookup needs in
  As_conf.(const actions $
           As_action.symlink $ As_acmd.stamp $ As_acmd.cmd ocamldep $
           As_acmd.cmd ocamlc $ As_acmd.cmd ocamlopt $
           value debug $ value profile $ value warn_error $
           value ocaml_annot $ value ocaml_byte $ value ocaml_native $
           pkgs $ As_part.list_actions libs_pp $ As_part.list_actions libs $
           src_dir $ dst_dir)

let actions p =
  let unit = As_part.coerce `Unit p in
  let src_dir = dir unit in
  let dst_dir = As_part.root_path unit in
  match kind unit with
  | `C spec -> c_actions spec unit src_dir dst_dir
  | `Js -> js_actions unit src_dir dst_dir
  | `OCaml (spec, _) -> ocaml_actions spec unit src_dir dst_dir

(* Create *)

let v ?usage ?exists ?args ?needs ?dir name kind =
  let meta = meta ?dir kind in
  As_part.v_kind ?usage ?exists ?args ~meta ?needs ~actions ~check name `Unit
