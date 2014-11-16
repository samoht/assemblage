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

(* Metadata *)

type kind = [ `OCaml | `OCaml_toplevel | `C ]

let pp_kind ppf k = As_fmt.pp_str ppf begin match k with
  | `OCaml -> "OCaml" | `OCaml_toplevel -> "OCaml_toplevel" | `C -> "C"
  end

type meta = { kind : kind; native : bool; byte : bool; js : bool }

let inj, proj = As_part.meta_key ()
let get_meta p = As_part.get_meta proj p
let meta ?byte ?native ?js kind =
  let def_byte, def_nat, def_js = match kind with
  | `OCaml -> true, true, false
  | `OCaml_toplevel -> true, false, false
  | `C -> false, true, false
  in
  let byte = match byte with None -> def_byte | Some b -> b in
  let native = match native with None -> def_nat | Some b -> b in
  let js = match js with None -> def_js | Some b -> b in
  inj { kind; byte; native; js }

let kind p = (get_meta p).kind
let byte p = (get_meta p).byte
let native p = (get_meta p).native
let js p = (get_meta p).js

let is_kind k p = match As_part.coerce_if `Bin p with
| None -> None
| Some p as r -> if kind p = k then r else None

let c = is_kind `C
let ocaml = is_kind `OCaml
let ocaml_toplevel = is_kind `OCaml_toplevel

(* Check *)

let check p =
  let bin = As_part.coerce `Bin p in
  As_log.warn "%a part check is TODO" As_part.pp_kind (As_part.kind bin);
  As_conf.true_

(* Actions *)

let ocaml_actions bin dst_dir unit_actions =
  let actions ocamlc ocamlopt debug profile ocaml_byte ocaml_native dst_dir
      unit_actions =
    let open As_acmd.Args in
    let name = As_path.(dst_dir / As_part.name bin) in
    let unit_outputs = As_action.list_outputs unit_actions in
    let cmos = List.filter (As_path.ext_matches [`Cmo; `O]) unit_outputs in
    let cmx_s = List.filter (As_path.ext_matches [`Cmx; `O]) unit_outputs in
    let byte = byte bin && ocaml_byte in
    let native = native bin && ocaml_native in
    let args = add_if debug "-g" @@ [] in
    fadd_if byte
      (As_action_ocaml.link_byte ~args ~ocamlc ~objs:cmos ~name) () @@
    fadd_if native
      (As_action_ocaml.link_native
         ~args:(add_if profile "-p" @@ args)
         ~ocamlopt ~objs:cmx_s ~name) () @@ []
  in
  As_conf.(const actions $ As_acmd.bin ocamlc $ As_acmd.bin ocamlopt $
           value debug $ value profile $
           value ocaml_byte $ value ocaml_native $
           dst_dir $ unit_actions)

let integrated_unit_actions bin = (* integrated actions of bin's `Unit needs  *)
  let integrate acc p = match As_part.coerce_if `Unit p with
  | None -> acc
  | Some u ->
      let add_need n = As_part.(kind n = `Pkg || kind n = `Lib) in
      As_part.integrate ~add_need u bin :: acc
  in
  As_part.list_actions (List.fold_left integrate [] (As_part.needs bin))

let actions p =
  let bin = As_part.coerce `Bin p in
  let dst_dir = As_part.root_path bin in
  let unit_actions = integrated_unit_actions bin in
  match kind p with
  | `OCaml -> ocaml_actions bin dst_dir unit_actions
  | k ->
      As_log.warn "%a %a part actions are TODO"
        As_part.pp_kind (As_part.kind bin) pp_kind k;
      As_conf.const []

(* Part *)

let v ?usage ?exists ?args ?byte ?native ?js name kind needs =
  let meta = meta ?byte ?native ?js kind in
  As_part.v_kind ?usage ?exists ?args ~meta ~needs ~actions ~check name `Bin
