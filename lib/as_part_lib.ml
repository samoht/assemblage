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

type kind = [ `OCaml | `OCaml_pp | `C ]

let pp_kind ppf k = As_fmt.pp_str ppf begin match k with
  | `OCaml -> "OCaml" | `OCaml_pp -> "OCaml_pp" | `C -> "C"
  end

type meta = { kind : kind; byte : bool; native : bool; native_dynlink : bool; }

let inj, proj = As_part.meta_key ()
let get_meta p = As_part.get_meta proj p
let meta ?byte ?native ?native_dynlink kind =
  let def_byte, def_nat, def_nat_dynlink = match kind with
  | `OCaml -> true, true, true
  | `OCaml_pp -> true, false, false
  | `C -> false, true, true
  in
  let byte = match byte with None -> def_byte | Some b -> b in
  let native = match native with None -> def_nat | Some b -> b in
  let native_dynlink = match native_dynlink with
  | None -> def_nat_dynlink | Some b -> b
  in
  inj { kind; byte; native; native_dynlink }

let kind p = (get_meta p).kind
let byte p = (get_meta p).byte
let native p = (get_meta p).native
let native_dynlink p = (get_meta p).native_dynlink

let is_kind k p = match As_part.coerce_if `Lib p with
| None -> None
| Some p as r -> if kind p = k then r else None

let ocaml = is_kind `OCaml
let ocaml_pp = is_kind `OCaml_pp
let c = is_kind `C

let warn_unit_dupe = format_of_string
    "More@ than@ one@ unit@ named@ `%s'@ in@ library@ part@ %s"

let find_unit u p =
  let is_u part = match As_part.coerce_if `Unit part with
  | Some part when As_part.name part = u -> Some part
  | _ -> None
  in
  match As_part.list_keep_map is_u (As_part.needs p) with
  | [] -> None
  | [u] -> Some u
  | us -> As_log.warn warn_unit_dupe u (As_part.name p); Some (List.hd us)

(* Checks *)

let check p =
  let lib = As_part.coerce `Lib p in
  As_log.warn "%a part check is TODO" As_part.pp_kind (As_part.kind lib);
  As_conf.true_

(* Actions *)

(* FIXME here we should add rules for ocamldep for each unit. And
   add the dep as an input to its actions. *)

let c_actions lib dst_dir unit_actions =
  As_log.warn "C library part support is TODO";
  As_conf.const []

let ocaml_actions kind lib dst_dir unit_actions =
  let actions ocamlc ocamlopt debug ocaml_byte ocaml_native ocaml_native_dynlink
      dst_dir unit_actions =
    let open As_acmd.Args in
    let not_pp = kind <> `OCaml_pp in
    let name = As_path.(dst_dir / As_part.name lib) in
    let unit_outputs = As_action.list_outputs unit_actions in
    let cmos = List.filter (As_path.has_ext `Cmo) unit_outputs in
    let cmx_s = List.filter (As_path.has_ext `Cmx) unit_outputs in
    let byte = byte lib && ocaml_byte in
    let native = native lib && ocaml_native && not_pp in
    let shared = native_dynlink lib && ocaml_native_dynlink && not_pp in
    let args = add_if debug "-g" @@ [] in
    fadd_if byte
      (As_action_ocaml.archive_byte ~args ~ocamlc ~cmos ~name) () @@
    fadd_if native
      (As_action_ocaml.archive_native ~args ~ocamlopt ~cmx_s ~name) () @@
    fadd_if shared
      (As_action_ocaml.archive_shared ~args ~ocamlopt ~cmx_s ~name) () @@
    unit_actions
  in
  As_conf.(const actions $ As_acmd.bin ocamlc $ As_acmd.bin ocamlopt $
           value debug $ value ocaml_byte $ value ocaml_native $
           value ocaml_native_dynlink $ dst_dir $ unit_actions)

let integrated_unit_actions lib = (* integrated actions of lib's unit needs  *)
  let integrate acc u =
    let add_need n = As_part.(kind n = `Pkg || kind n = `Lib) in
    As_part.integrate ~add_need u lib :: acc
  in
  let needs = As_part.needs lib in
  let rev_units = As_part.list_fold_kind `Unit integrate [] needs in
  let add_actions acc u =
    As_conf.(List.rev_append (List.rev (As_part.actions u)) acc)
  in
  List.fold_left add_actions (As_conf.const []) rev_units

let actions p =
  let lib = As_part.coerce `Lib p in
  let dst_dir = As_part.root_path lib in
  let unit_actions = integrated_unit_actions lib in
  match kind p with
  | `OCaml | `OCaml_pp as k -> ocaml_actions k lib dst_dir unit_actions
  | `C -> c_actions lib dst_dir unit_actions

(* Lib *)

let v ?usage ?exists ?args ?byte ?native ?native_dynlink name kind needs  =
  let meta = meta ?byte ?native ?native_dynlink kind in
  As_part.v_kind ?usage ?exists ?args ~meta ~needs ~actions ~check name `Lib
