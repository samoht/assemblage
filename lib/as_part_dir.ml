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

open Astring
open Bos

(* Metadata *)

type kind = [ `Lib | `Bin | `Sbin | `Toplevel | `Share | `Share_root
            | `Etc | `Doc | `Stublibs | `Man | `Other of Path.t ]

let pp_kind ppf kind = Fmt.string ppf begin match kind with
  | `Lib -> "lib" | `Bin -> "bin" | `Sbin -> "sbin" | `Toplevel -> "toplevel"
  | `Share -> "share" | `Share_root -> "share_root" | `Etc -> "etc"
  | `Doc -> "doc" | `Stublibs -> "stublibs" | `Man -> "man"
  | `Other p -> strf "other:%s" (Path.to_string p)
  end

let name_of_kind = function
| `Other p -> Path.filename p
| kind -> strf "%a" pp_kind kind

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

type spec = As_part.kind As_part.t -> (path * path option) list As_conf.value

let keep_if kind pred p =
  let keep_if acts =
    let add acc p = if pred p then (p, None) :: acc else acc in
    List.(rev (fold_left add [] (kind acts)))
  in
  As_conf.(const keep_if $ As_part.actions p)

let keep_map kind f p =
  let keep_map f acts =
    let add acc p = match f p with None -> acc | Some spec -> spec :: acc in
    List.(rev (fold_left add [] (kind acts)))
  in
  As_conf.(const keep_map $ f $ As_part.actions p)

let all kind p =
  let all acts = List.(rev (rev_map (fun p -> p, None) (kind acts))) in
  As_conf.(const all $ As_part.actions p)

let all_input = all As_action.list_inputs
let all_output = all As_action.list_outputs
let all = all As_action.list_products

let file_exts exts =
  let ext_matches exts p = List.exists (fun ext -> Path.ext_is ext p) exts in
  keep_if As_action.list_products (ext_matches exts)

let relativize root p = match Path.rem_prefix root p with
| None -> Path.base p
| Some p -> p

let bin p = match As_part.coerce_if `Bin p with
| None -> all_output p
| Some bin ->
    match As_part_bin.kind bin with
    | `OCaml_toplevel -> all_output p (* FIXME *)
    | `OCaml ->
        let spec ocaml_native root f =
          let rename f = f, Some (Path.rem_ext (relativize root f)) in
          match Path.ext f with
          | ".byte" when As_part_bin.native bin && ocaml_native -> None
          | ".byte" -> Some (rename f)
          | ".native" -> Some (rename f)
          | _ -> None
        in
        keep_map As_action.list_outputs
          As_conf.(const spec $ value ocaml_native $ As_part.root_path bin)
          bin
    | `C ->
      let is_exec f = Path.(filename (rem_ext f)) = As_part.name bin in
      keep_if As_action.list_outputs is_exec bin


let warn_miss_unit = format_of_string
    "Library@ part@ %s:@ no@ compilation@ unit@ found@ for@ product@ %s"

let lib_ocaml lib f = match Path.ext f with
| "" -> None
| ".cma" | ".cmxa" | ".cmxs" | ".a" | ".so" | ".dll" -> Some (f, None)
| (".cmx" | ".cmi" | ".cmti" as ext) ->
    let unit_name = Path.(filename (rem_ext f)) in
    begin match As_part_lib.find_unit unit_name lib with
    | None ->
        Log.warn warn_miss_unit (As_part.name lib) (Path.to_string f);
        None
    | Some u ->
        begin match As_part_unit.kind u with
        | `OCaml (_, interface) ->
            begin match ext, interface with
            | ".cmx", `Normal -> Some (f, None)
            | (".cmi" | ".cmti"), (`Normal | `Opaque) -> Some (f, None)
            | _ -> None
            end
        | _ -> None
        end
    end
| _ -> None

let lib p = match As_part.coerce_if `Lib p with
| None -> all_output p
| Some lib ->
    match As_part_lib.kind lib with
    | `C -> file_exts [".dll"; ".so"; ".a"] lib
    | `OCaml | `OCaml_pp ->
        keep_map As_action.list_outputs As_conf.(const (lib_ocaml lib)) lib

let doc p = match As_part.coerce_if `Doc p with
| None -> all p
| Some doc when As_part_doc.kind doc = `OCamldoc -> all_output p
| _ -> all p

(* Checks *)

let check spec p =
  let dir = As_part.coerce `Dir p in
  (* Here we could check for example that the directory specifier
     returns only products that belong to the part itself. *)
  Log.warn "%a part check is TODO" As_part.pp_kind (As_part.kind dir);
  As_conf.true_

(* Actions *)

let part_links acc symlink exists part_root specs dir_root =
  if not exists then acc else
  let add acc (src, dst) =
    let dst = match dst with
    | Some dst -> dst
    | None -> relativize part_root src
    in
    symlink src Path.(dir_root // dst) :: acc
  in
  List.fold_left add acc specs

let actions spec p =
  let dir = As_part.coerce `Dir p in
  let add_part acc part =
    As_conf.(const part_links $ acc $ As_action.symlink $
             As_part.exists part $ As_part.root_path part $
             spec part $ As_part.root_path dir)
  in
  let actions = List.fold_left add_part (As_conf.const []) (As_part.needs p) in
  As_conf.(const List.rev $ actions)

(* Dir *)

let default_spec kind spec = match spec with
| Some spec -> spec
| None ->
    match kind with
    | `Bin -> bin
    | `Lib -> lib
    | `Doc -> doc
    | _ -> all

let v ?usage ?exists ?args ?spec ?install kind needs =
  let spec = default_spec kind spec in
  let actions = actions spec in
  let check = check spec in
  let meta = meta ?install kind in
  let name = name_of_kind kind in
  As_part.v_kind ?usage ?exists ?args ~meta ~needs ~actions ~check name `Dir
