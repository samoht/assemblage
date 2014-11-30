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
  let actions ocamlc ocamlopt debug profile ocaml_byte ocaml_native
      pkgs libs_actions dst_dir unit_actions =
    let open As_acmd.Args in
    let name = As_path.(dst_dir / As_part.name bin) in
    let unit_outputs = As_action.list_outputs unit_actions in
    let cmas, cmxas =
      let outs = As_action.list_outputs libs_actions in
      List.filter (As_path.has_ext `Cma) outs,
      List.filter (As_path.has_ext `Cmxa) outs
    in
    let cmos = List.filter (As_path.ext_matches [`Cmo; `O]) unit_outputs in
    let cmx_s = List.filter (As_path.ext_matches [`Cmx; `O]) unit_outputs in
    let byte_objs = List.rev_append (List.rev cmas) cmos in
    let native_objs = List.rev_append (List.rev cmxas) cmx_s in
    let byte = byte bin && ocaml_byte in
    let native = native bin && ocaml_native in
    let args = add_if debug "-g" @@ [] in
    fadd_if byte
      (As_action_ocaml.link_byte
         ~ocamlc ~pkgs ~args ~objs:byte_objs ~name) () @@
    fadd_if native
      (As_action_ocaml.link_native
         ~ocamlopt ~pkgs ~args:(add_if profile "-p" @@ args)
         ~objs:native_objs ~name) () @@
    unit_actions
  in
  let needs = As_part.needs bin in
  let libs = As_part.list_keep_map As_part_lib.ocaml needs in
  let pkgs = As_part_pkg.list_lookup needs in
  As_conf.(const actions $ As_acmd.cmd ocamlc $ As_acmd.cmd ocamlopt $
           value debug $ value profile $
           value ocaml_byte $ value ocaml_native $
           pkgs $ As_part.list_actions libs $
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

let to_cmd_path ?(abs = false) ?ext bin =
  let mk_path ocaml_native proj_root part_root =
    let ext = match kind bin with
    | `OCaml_toplevel | `C -> ext
    | `OCaml ->
        match ext with
        | Some _ as ext -> ext
        | None ->
            if ocaml_native && native bin then Some `Native else Some `Byte
    in
    let p = As_path.Rel.(part_root / As_part.name bin) in
    let p = if abs then As_path.(proj_root // p) else As_path.of_rel p in
    match ext with None -> p | Some e -> As_path.(p + e)
  in
  As_conf.(const mk_path $ value ocaml_native $ value root_dir $
           As_part.root bin)

(* FIXME this is the only place where we have absolute paths
   in the resulting build system (see e.g. the generated Makefile).
   The problem is for runs with ~dir argument specified. *)
let to_cmd ?ext bin =
  let mk_cmd path = As_acmd.static (As_path.to_string path) in
  As_conf.(const mk_cmd $ to_cmd_path ~abs:true ?ext bin)

let exists ?ext bin =
  let exists part_exists nat byt = match kind bin with
  | `C -> part_exists
  | `OCaml_toplevel -> part_exists && byt
  | `OCaml ->
      match ext with
      | Some `Byte -> part_exists && byt && byte bin
      | Some `Native -> part_exists && nat && native bin
      | _ -> part_exists && (nat && native bin || byt && byte bin)
  in
  As_conf.(const exists $ As_part.exists bin $ value ocaml_native $
           value ocaml_byte)

(* Run *)

let gen ?usage ?exists:exs ?args ?dir ?name ?ext ?stdin ?stdout ?stderr
    bin cargs
  =
  let name = match name with
  | None -> str "gen-%s" (As_part.name bin)
  | Some n -> n
  in
  let exists = match exs with
  | None -> exists ?ext bin
  | Some exs -> As_conf.(exs &&& exists ?ext bin)
  in
  let actions _ =
    let open As_acmd.Args in
    let actions cd dir cpath bin cargs stdin stdout stderr =
      let ctx = As_ctx.v [`Gen] in
      let inputs = match stdin with
      | None -> [ cpath ]
      | Some p -> [ p; cpath ]
      in
      let outputs = match stdout, stderr with
      | Some p, Some p' -> [p; p']
      | Some p, _ | _, Some p -> [p]
      | None, None -> []
      in
      let gen_cmd = As_acmd.v bin cargs ?stdin ?stdout ?stderr in
      let cmds = match dir with
      | None -> [ gen_cmd ]
      | Some dir -> [ cd dir; gen_cmd ]
      in
      [As_action.v ~ctx ~inputs ~outputs cmds]
    in
    let cpath = to_cmd_path ?ext bin in
    let bin = to_cmd ?ext bin in
    As_conf.(const actions $ As_acmd.cd $ Option.wrap dir $ cpath $ bin $
             cargs $ Option.wrap stdin $ Option.wrap stdout $
             Option.wrap stderr)
  in
  As_part.v_kind ?usage ~exists ?args ~actions name `Base
