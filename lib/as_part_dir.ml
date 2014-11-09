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

type kind = [ `Lib | `Bin | `Sbin | `Toplevel | `Share | `Share_root
            | `Etc | `Doc | `Stublibs | `Man | `Other of As_path.t ]

let pp_kind ppf kind = As_fmt.pp_str ppf begin match kind with
  | `Lib -> "lib" | `Bin -> "bin" | `Sbin -> "sbin" | `Toplevel -> "toplevel"
  | `Share -> "share" | `Share_root -> "share_root" | `Etc -> "etc"
  | `Doc -> "doc" | `Stublibs -> "stublibs" | `Man -> "man"
  | `Other p -> str "other_%s" (As_path.to_string p)
  end

let name_of_kind k = Format.asprintf "%a" pp_kind k
(* FIXME name of `Other will have file seps *)

type meta = { kind : kind; install : bool }

let inj, proj = As_part.meta_key ()
let get_meta p = As_part.get_meta proj p
let meta kind install = inj { kind; install }
let kind p = (get_meta p).kind
let install p = (get_meta p).install

(* Directory specifiers *)

type spec = As_part.kind As_part.t -> As_action.product ->
  [ `Keep | `Rename of As_path.t | `Drop] As_conf.value

let keep_if pred f =
  let choose f = if pred f then `Keep else `Drop in
  As_conf.(const choose $ f)

let all _ _ = As_conf.const `Keep

let file_exts exts _ product =
  let one_of exts f = List.exists (fun e -> As_path.has_ext e f) exts in
  keep_if (one_of exts) product

let install_bin p product = match As_part.coerce_if `Bin p with
| None -> all p product
| Some p ->
    match As_part_bin.kind p with
    | `OCaml_toplevel -> (* FIXME *) As_conf.const `Keep
    | `OCaml ->
        let rename f = `Rename (As_path.(rem_ext (file (basename f)))) in
        let rename ocaml_native f =
          if As_path.has_ext `Byte f then
            (if As_part_bin.native p && ocaml_native then `Drop else rename f)
          else if As_path.has_ext `Native f then
            (if not ocaml_native then `Drop else rename f)
          else `Drop
        in
        As_conf.(const rename $ value ocaml_native $ product)
    | `C ->
        let is_exec f = As_path.basename f = As_part.name p in
        (* FIXME windows exe ? *)
        keep_if is_exec product

let warn_miss_unit = format_of_string
    "Library@ part@ %s:@ no@ compilation@ unit@ found@ for@ product@ %s"

let install_lib_ocaml_product p f = match As_path.ext f with
| None -> `Drop
| Some (`Cma | `Cmxa | `Cmxs | `A | `So | `Dll) -> `Keep
| Some (`Cmx | `Cmi | `Cmti as ext) ->
    let uname = As_path.(basename (rem_ext f)) in
    begin match As_part_lib.find_unit uname p with
    | None ->
        As_log.warn warn_miss_unit (As_part.name p) (As_path.to_string f);
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

let install_lib p product = match As_part.coerce_if `Lib p with
| None -> all p product
| Some p ->
    match As_part_lib.kind p with
    | `C -> file_exts [`Dll; `So; `A] p product
    | `OCaml | `OCaml_pp ->
        As_conf.(const (install_lib_ocaml_product p) $ product)


(* Actions *)

(* Dir *)

let v ?usage ?cond ?(args = As_args.empty) ?keep ?(install = true) kind needs =
  let _keep = match keep with
  | Some keep -> keep
  | None ->
      match kind, install with
      | `Bin, true -> install_bin
      | `Lib, true -> install_lib
      | _ -> all
  in
  let args _ = args in
  let meta = meta kind install in
  As_part.v_kind ?usage ?cond ~args ~meta (name_of_kind kind) `Dir

let of_base ?(install = true) kind p =
  let meta = meta kind install in
  As_part.with_kind_meta `Dir meta p
