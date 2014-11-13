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
  true

(* Actions *)

let add_if c f v acc = if c then (f v) :: acc else acc

(* FIXME here we should add rules for ocamldep for each unit. And
   add the dep as an input to its actions. *)

let ocaml_actions kind lib =
  let name = As_part.rooted lib (As_part.name lib) in
  let not_pp = kind <> `OCaml_pp in
  let reroot acc u = As_part.with_root (As_part.root lib) u :: acc in
  let rev_units = As_part.list_fold_kind `Unit reroot [] (As_part.needs lib) in
  let units = List.rev rev_units in
  let units_prods = As_part.list_products units in
  let cmos = As_conf.List.keep (As_path.has_ext `Cmo) units_prods in
  let cmx_s = As_conf.List.keep (As_path.has_ext `Cmx) units_prods in
  let add_actions acc u = List.rev_append (List.rev (As_part.actions u)) acc in
  let byte = byte lib in
  let native = native lib && not_pp in
  let shared = native_dynlink lib && not_pp in
  add_if byte (As_action_ocaml.archive_byte ~cmos ~name) () @@
  add_if native (As_action_ocaml.archive_native ~cmx_s ~name) () @@
  add_if shared (As_action_ocaml.archive_shared ~cmx_s ~name) () @@
  As_part.list_fold add_actions [] rev_units

let actions p =
  let l = As_part.coerce `Lib p in
  match kind p with
  | `OCaml | `OCaml_pp as k -> ocaml_actions k l
  | `C -> As_log.warn "Damned the C library part is TODO"; []

(* Lib *)

let v ?usage ?cond ?args ?byte ?native ?native_dynlink name kind needs  =
  let meta = meta ?byte ?native ?native_dynlink kind in
  As_part.v_kind ?usage ?cond ?args ~meta ~needs ~actions ~check name `Lib

let of_base ?byte ?native ?native_dynlink kind p =
  let meta = meta ?byte ?native ?native_dynlink kind in
  As_part.with_kind_meta `Lib meta p
