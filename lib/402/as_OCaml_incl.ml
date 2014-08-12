(*
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Parsetree

module StringSet = Set.Make (String)

let init flags =
  match As_flags.get (`Pp `Byte) flags with
  | [] -> Clflags.preprocessor := None;
  | pp ->
      let pp = String.concat " " pp in
      Clflags.preprocessor := Some (sprintf "camlp4o %s" pp)

let add_module prefix set m =
  let p = prefix m in
  StringSet.add p set, fun x -> p ^ "." ^ x

let modules_of_ml ast =
  let rec structure_item prefix acc { pstr_desc; _ } =
    match pstr_desc with
    | Pstr_module b ->
        let acc, prefix = add_module prefix acc b.pmb_name.Asttypes.txt in
        module_expr prefix acc b.pmb_expr.pmod_desc
    | Pstr_recmodule l ->
        List.fold_left (fun acc b ->
            let acc, prefix = add_module prefix acc b.pmb_name.Asttypes.txt in
            module_expr prefix acc b.pmb_expr.pmod_desc
          ) acc l
    | _ -> acc
  and module_expr prefix acc = function
  | Pmod_structure l -> List.fold_left (structure_item prefix) acc l
  | _                -> acc
  in
  List.fold_left (structure_item (fun x -> x)) StringSet.empty ast

let modules_of_mli ast =
  let rec sig_item prefix acc { psig_desc; _ } =
    match psig_desc with
    | Psig_module b ->
        let acc, prefix = add_module prefix acc b.pmd_name.Asttypes.txt in
        module_type prefix acc b.pmd_type
    | Psig_recmodule l ->
        List.fold_left (fun acc b ->
            let acc, prefix = add_module prefix acc b.pmd_name.Asttypes.txt in
            module_type prefix acc b.pmd_type
          ) acc l
    | _ -> acc
  and module_type prefix acc { pmty_desc; _ } =
    match pmty_desc with
    | Pmty_signature s -> List.fold_left (sig_item prefix) acc s
    | _                -> acc
  in
  List.fold_left (sig_item (fun x -> x)) StringSet.empty ast
