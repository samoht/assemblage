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

let (|>) x f = f x

module StringSet = Set.Make(struct
    type t = string
    let compare = String.compare
  end)

let (/) = Filename.concat
let (//) x y =
  match x with
  | None   -> y
  | Some x -> x / y

#if ocaml_version < (4, 2)
module Pparse = struct
  include Pparse
  (* from `ocaml-4.02/driver/pparse.ml' *)
  let parse_all parse_fun magic ppf sourcefile =
    Location.input_name := sourcefile;
    let inputfile = Pparse.preprocess sourcefile in
    let ast =
      try Pparse.file ppf inputfile parse_fun magic
      with exn ->
        Pparse.remove_preprocessed inputfile;
        raise exn
    in
    Pparse.remove_preprocessed inputfile;
    ast
  let parse_implementation ppf sourcefile =
    parse_all Parse.implementation Config.ast_impl_magic_number ppf sourcefile
  let parse_interface ppf sourcefile =
    parse_all Parse.interface Config.ast_intf_magic_number ppf sourcefile
end
#endif

let init flags =
  match As_flags.pp_byte flags with
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

let modules ~build_dir cu =
  let resolver = As_ocamlfind.resolver `Direct build_dir in
  let () =
    As_project.CU.deps cu
    |> As_project.Component.closure
    |> As_project.Component.(filter pkg_pp)
    |> As_resolver.pkgs resolver
    |> init in
  let aux = function
    | `ML ->
      let file = As_project.CU.(dir cu // name cu ^ ".ml") in
      let ast = Pparse.parse_implementation Format.err_formatter file in
      modules_of_ml ast
    | `MLI ->
      let file = As_project.CU.(dir cu // name cu ^ ".mli") in
      let ast = Pparse.parse_interface Format.err_formatter file in
      modules_of_mli ast in
  let set =
    if As_project.CU.mli cu then aux `MLI
    else if As_project.CU.ml cu then aux `ML
    else StringSet.empty in
  StringSet.elements set

let split str char =
  let len = String.length str in
  let return l =
    List.rev (List.filter ((<>)"") l) in
  let rec aux acc off =
    if off >= len then return acc
    else
      let word, off =
        try
          let i = String.index_from str off  char in
          String.sub str off (i - off), i
        with Not_found ->
          String.sub str off (len - off), len in
      aux (word :: acc) (off+1)
  in
  aux [] 0

(* XXX: ugly hack as tools/{depend.ml,ocamldep.ml} are not in
   `compiler-libs' *)
let depends ?flags ?(deps=fun _ -> []) resolver dir =
  let files =
    Array.to_list (Sys.readdir dir) |>
    List.filter (fun file ->
        Filename.check_suffix file ".ml" || Filename.check_suffix file ".mli")
  in
  let paths = List.map (fun file -> dir / file) files in
  let names = List.map Filename.chop_extension files in
  let all_deps =
    List.fold_left (fun acc name ->
        deps name
        |> As_project.Component.Set.of_list
        |> As_project.Component.Set.union acc
      ) As_project.Component.Set.empty names
    |> As_project.Component.Set.elements
  in
  let pp = match As_project.Component.pp_byte all_deps resolver with
    | [] -> ""
    | pp ->
      let pp = String.concat " " pp in
      sprintf "-pp \"camlp4o %s\"" pp in
  let incl = match As_project.Component.comp_byte all_deps resolver (fun _ -> dir) with
    | [] -> ""
    | ls -> " " ^ String.concat " " ls in
  let lines =
    As_shell.exec_output
      "ocamldep -one-line -modules %s%s %s"
      pp incl (String.concat " " paths)
  in
  let deps_tbl = Hashtbl.create (List.length names) in
  let add_dep line =
    match split line ' ' with
    | []
    | "Bad"   :: _
    | "File"  :: _
    | "Error" :: _        -> ()
    | name    :: modules  ->
      (* [name] is dir/<name>.ml[,i]: *)
      let name = Filename.(basename (chop_extension name)) in
      let deps = List.fold_left (fun acc m ->
          if List.mem m names then
            m :: acc
          else if List.mem (String.uncapitalize m) names then
            String.uncapitalize m :: acc
          else
            acc
        ) [] modules
      in
      Hashtbl.add deps_tbl name deps
  in
  List.iter add_dep lines;
  let cus_tbl = Hashtbl.create (List.length names) in
  let rec cu name =
    try Hashtbl.find cus_tbl name
    with Not_found ->
      let local_deps =
        try List.concat (Hashtbl.find_all deps_tbl name)
        with Not_found -> [] in
      let deps = deps name @ List.map (fun x -> `CU (cu x)) local_deps in
      let cu = As_project.CU.create ?flags ~dir ~deps name in
      Hashtbl.add cus_tbl name cu;
      cu in
  List.map cu names
