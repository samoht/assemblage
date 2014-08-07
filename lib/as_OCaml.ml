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
open Printf

module StringSet = Set.Make (String)

#if ocaml_version < (4, 2)
module Pparse = struct
  include Pparse
  (* from `ocaml-4.02/driver/pparse.ml' *)
  let parse_all ~tool_name parse_fun magic ppf sourcefile =
    Location.input_name := sourcefile;
    let inputfile = Pparse.preprocess sourcefile in
    let ast =
      try Pparse.file ppf (*~tool_name*) inputfile parse_fun magic
      with exn ->
        Pparse.remove_preprocessed inputfile;
        raise exn
    in
    Pparse.remove_preprocessed inputfile;
    ast
  let parse_implementation ppf ~tool_name sourcefile =
    parse_all Parse.implementation ~tool_name Config.ast_impl_magic_number ppf sourcefile
  let parse_interface ppf ~tool_name sourcefile =
    parse_all Parse.interface ~tool_name Config.ast_intf_magic_number ppf sourcefile
end
#endif

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
#if ocaml_version >= (4, 2)
    | Pstr_module b    ->
      let acc, prefix = add_module prefix acc b.pmb_name.Asttypes.txt in
      module_expr prefix acc b.pmb_expr.pmod_desc
    | Pstr_recmodule l ->
      List.fold_left (fun acc b ->
          let acc, prefix = add_module prefix acc b.pmb_name.Asttypes.txt in
          module_expr prefix acc b.pmb_expr.pmod_desc
        ) acc l
#else
    | Pstr_module (l, e) ->
      let acc, prefix = add_module prefix acc l.Asttypes.txt in
      module_expr prefix acc e.pmod_desc
    | Pstr_recmodule l  ->
      List.fold_left (fun acc (l,_,e) ->
          let acc, prefix = add_module prefix acc l.Asttypes.txt in
          module_expr prefix acc e.pmod_desc
        ) acc l
#endif
    | _ -> acc
  and module_expr prefix acc = function
    | Pmod_structure l -> List.fold_left (structure_item prefix) acc l
    | _                -> acc
  in
  List.fold_left (structure_item (fun x -> x)) StringSet.empty ast

let modules_of_mli ast =
  let rec sig_item prefix acc { psig_desc; _ } =
    match psig_desc with
#if ocaml_version >= (4, 2)
    | Psig_module b  ->
      let acc, prefix = add_module prefix acc b.pmd_name.Asttypes.txt in
      module_type prefix acc b.pmd_type
    | Psig_recmodule l ->
      List.fold_left (fun acc b ->
          let acc, prefix = add_module prefix acc b.pmd_name.Asttypes.txt in
          module_type prefix acc b.pmd_type
        ) acc l
#else
    | Psig_module (l, s) ->
      let acc, prefix = add_module prefix acc l.Asttypes.txt in
      module_type prefix acc s
    | Psig_recmodule l  ->
      List.fold_left (fun acc (l,s) ->
          let acc, prefix = add_module prefix acc l.Asttypes.txt in
          module_type prefix acc s
        ) acc l
#endif
    | _ -> acc

  and module_type prefix acc { pmty_desc; _ } =
    match pmty_desc with
    | Pmty_signature s -> List.fold_left (sig_item prefix) acc s
    | _                -> acc
  in
  List.fold_left (sig_item (fun x -> x)) StringSet.empty ast

let modules ~build_dir unit =
  let r = As_ocamlfind.resolver `Direct  ~build_dir () in
  let () = init (As_project.Unit.flags unit r) in
  let aux ext =
    let source = As_project.Component.source (`Unit unit) (ext:>As_action.file) in
    let build = As_project.Component.source (`Unit unit) (ext:>As_action.file) in
    let parse f = match ext with
    | `Ml -> modules_of_ml (Pparse.parse_implementation ~tool_name:"" Format.err_formatter f)
    | `Mli -> modules_of_mli (Pparse.parse_interface ~tool_name:"" Format.err_formatter f)
    in
    if Sys.file_exists source then parse source else
    if Sys.file_exists build then parse build else
    StringSet.empty
  in
  let set =
    if As_project.Unit.has `Mli unit then aux `Mli
    else if As_project.Unit.has `Ml unit then aux `Ml
    else StringSet.empty in
  StringSet.elements set
