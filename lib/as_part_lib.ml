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

type kind = [ `OCaml | `OCaml_pp | `C ]
type meta =
  { kind : kind;
    byte : bool;
    native : bool;
    native_dynlink : bool; }

let inj, proj = As_part.meta_key ()
let get_meta p = As_part.get_meta proj p
let meta ?(byte = true) ?(native = true) ?(native_dynlink = true) kind =
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

(* Rules *)

(*
  let lib_file fext l =
    As_path.(as_rel (As_env.build_dir env // (file (name l)) + fext))

  let lib_args l =
    let pkgs = keep_kind `Pkg (deps l) in
    let pkgs_args = As_args.concat (List.map args pkgs) in
    As_args.(args env l @@@ pkgs_args)

  let ocaml_rules units env l =
    (*  FIXME: check if there are C units and use directly ocamlmklib *)
    let byte = if byte l then [ ocaml_archive_byte units env l ] else [] in
    let nat = if native l then [ ocaml_archive_native units env l ] else [] in
    let dyn =
      if native_dynlink l then [ ocaml_archive_shared units env l ] else []
    in
    List.concat [ byte; nat; dyn; ]

  let ocaml_pp_rules units env l = [ ocaml_archive_byte units env l ]
*)
  let actions units p = []
(*
    let l = coerce `Lib p in
    let build_dir = As_path.dir (kind_to_string `Lib ^ "-" ^ (name l)) in
    let env = As_env.push_build_dir env build_dir in
    let units_rules = List.(flatten (map (actions env) units)) in
    let mkdir_rule = As_action.mkdir env ~dir:(As_env.build_dir env) in
    match kind l with
    | `C -> mkdir_rule :: units_rules @ c_rules units env p
    | `OCaml -> mkdir_rule :: units_rules @ ocaml_rules units env p
    | `OCaml_pp -> mkdir_rule :: units_rules @ ocaml_pp_rules units env p
*)

  (* Create *)

let create ?cond ?(args = As_args.empty) ?deps:(ds = []) ?byte ?native
    ?native_dynlink name kind (units : [< `Unit] As_part.t list)  =
  let meta = meta ?byte ?native ?native_dynlink kind in
  let units = (* List.map (add_deps_args ds args) *) units in
  let deps = [] (* List.flatten (List.map deps units) *) in
  let args _  = args in
  let actions = actions units in
  As_part.create ?cond ~args ~deps ~actions name `Lib meta

let of_base ?byte ?native ?native_dynlink kind p =
  let meta = meta ?byte ?native ?native_dynlink kind in
  { p with As_part.kind = `Lib; meta }

(* Part filters *)
