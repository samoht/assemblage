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

let str = Format.asprintf

(* Metadata *)

type kind = [ `Lib | `Bin | `Sbin | `Toplevel | `Share | `Share_root
            | `Etc | `Doc | `Stublibs | `Man | `Other of As_path.t ]

let pp_kind ppf kind = As_fmt.pp_str ppf begin match kind with
  | `Lib -> "lib" | `Bin -> "bin" | `Sbin -> "sbin" | `Toplevel -> "toplevel"
  | `Share -> "share" | `Share_root -> "share_root" | `Etc -> "etc"
  | `Doc -> "doc" | `Stublibs -> "stublibs" | `Man -> "man"
  | `Other p -> str "other:%s" (As_path.to_string p)
  end

let name_of_kind = function
| `Other p -> As_path.basename p
| kind -> str "%a" pp_kind kind

type meta = { kind : kind; install : bool }

let inj, proj = As_part.meta_key ()
let get_meta p = As_part.get_meta proj p
let meta ?install kind =
  let install = match install with
  | Some install -> install
  | None -> match kind with `Other _ -> false | _ -> true
  in
  inj { kind; install }

let kind p = (get_meta p).kind
let install p = (get_meta p).install

(* Directory specifiers *)

type spec =
    As_part.kind As_part.t ->
    (As_path.t -> [ `Keep | `Rename of As_path.rel | `Drop]) As_conf.value

let all _ = As_conf.(const (fun _ -> `Keep))

let keep_if pred = As_conf.const (fun f -> if pred f then `Keep else `Drop)
let file_exts exts _ = keep_if (As_path.ext_matches exts)

let bin p = match As_part.coerce_if `Bin p with
| None -> all p
| Some bin ->
    match As_part_bin.kind bin with
    | `OCaml_toplevel -> (* FIXME *) all p
    | `OCaml ->
        let rename ocaml_native f =
          let rename f = `Rename As_path.(Rel.file (basename (rem_ext f))) in
          match As_path.ext f with
          | Some `Byte when As_part_bin.native bin && ocaml_native -> `Drop
          | Some `Byte -> rename f
          | Some `Native -> rename f
          | _ -> `Drop
        in
        As_conf.(const rename $ value ocaml_native)
    | `C ->
        let is_exec f = As_path.(basename (rem_ext f)) = As_part.name bin in
        keep_if is_exec

let warn_miss_unit = format_of_string
    "Library@ part@ %s:@ no@ compilation@ unit@ found@ for@ product@ %s"

let lib_ocaml lib f = match As_path.ext f with
| None -> `Drop
| Some (`Cma | `Cmxa | `Cmxs | `A | `So | `Dll) -> `Keep
| Some (`Cmx | `Cmi | `Cmti as ext) ->
    let unit_name = As_path.(basename (rem_ext f)) in
    begin match As_part_lib.find_unit unit_name lib with
    | None ->
        As_log.warn warn_miss_unit (As_part.name lib) (As_path.to_string f);
        `Drop
    | Some u ->
        begin match As_part_unit.kind u with
        | `OCaml (_, interface) ->
            begin match ext, interface with
            | `Cmx, `Normal -> `Keep
            | (`Cmi | `Cmti), (`Normal | `Opaque) -> `Keep
            | _ -> `Drop
            end
        | _ -> `Drop
        end
    end
| _ -> `Drop

let lib p = match As_part.coerce_if `Lib p with
| None -> all p
| Some lib ->
    match As_part_lib.kind lib with
    | `C -> file_exts [`Dll; `So; `A] lib
    | `OCaml | `OCaml_pp -> As_conf.(const (lib_ocaml lib))

(* Checks *)

let check p =
  let dir = As_part.coerce `Dir p in
  As_log.warn "%a part check is TODO" As_part.pp_kind (As_part.kind dir);
  true

(* Actions *)

let link_part_action keep dir p =
  let keep = keep p in
  let file_map keep part_root products =
    let part_root = As_path.of_rel part_root in
    let add acc product = match keep product with
    | `Drop -> acc
    | `Keep -> (product, As_path.(part_root / basename product)) :: acc
    | `Rename p -> (product, As_path.(part_root // p)) :: acc
    in
    List.rev (List.fold_left add [] products)
  in
  let file_map =
    As_conf.(const file_map $ keep $ As_part.root dir $ As_part.products p)
  in
  let cmds ln file_map =
    let add acc (src, dst) = ln src dst :: acc in
    List.rev (List.fold_left add [] file_map)
  in
  let ctx = As_ctx.v [ `Gen ] (* bof *) in
  let inputs = As_conf.List.map fst file_map in
  let outputs = As_conf.List.map snd file_map in
  let cmds = As_conf.(const cmds $ As_action.Sys.ln_rel $ file_map) in
  As_action.v ~ctx ~inputs ~outputs cmds

let actions keep p =
  let dir = As_part.coerce `Dir p in
  let add acc p = link_part_action keep dir p :: acc in
  List.rev (List.fold_left add [] (As_part.needs p))

(* Dir *)

let default_keep kind keep = match keep with
| Some keep -> keep
| None ->
    match kind with
    | `Bin -> bin
    | `Lib -> lib
    | _ -> all

let v ?usage ?cond ?args ?keep ?install kind needs =
  let keep = default_keep kind keep in
  let actions = actions keep in
  let meta = meta ?install kind in
  let name = name_of_kind kind in
  As_part.v_kind ?usage ?cond ?args ~meta ~needs ~actions ~check name `Dir

let of_base ?install kind p =
  let meta = meta ?install kind in
  As_part.with_kind_meta `Dir meta p
