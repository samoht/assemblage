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

open Project
open Parsetree
open Printf

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
  match Flags.pp_byte flags with
  | [] -> Clflags.preprocessor := None;
  | pp ->
    let pp = String.concat " " pp in
    Clflags.preprocessor := Some (sprintf "camlp4o %s" pp)

let modules ~build_dir cu =
  let resolver = Ocamlfind.resolver `Direct build_dir in
  let () =
    CU.deps cu
    |> Component.closure
    |> Component.(filter pkg_pp)
    |> Resolver.pkgs resolver
    |> init in
  let aux = function
    | `ML ->
      let file = CU.dir cu // CU.name cu ^ ".ml" in
      let ast = Pparse.parse_implementation Format.err_formatter file in
      List.fold_left (fun acc { pstr_desc; _ } ->
          match pstr_desc with
#if ocaml_version >= (4, 2)
          | Pstr_module b    -> StringSet.add b.pmb_name.Asttypes.txt acc
          | Pstr_recmodule l ->
            List.fold_left (fun acc b ->
                StringSet.add b.pmb_name.Asttypes.txt acc
              ) acc l
#else
          | Pstr_module (l,_) -> StringSet.add l.Asttypes.txt acc
          | Pstr_recmodule l  ->
            List.fold_left (fun acc (l,_,_) ->
              StringSet.add l.Asttypes.txt acc
            ) acc l
#endif
          | _ -> acc
        ) StringSet.empty ast
    | `MLI ->
      let file = CU.dir cu // CU.name cu ^ ".mli" in
      let ast = Pparse.parse_interface Format.err_formatter file in
      List.fold_left (fun acc { psig_desc; _ } ->
          match psig_desc with
#if ocaml_version >= (4, 2)
          | Psig_module b    -> StringSet.add b.pmd_name.Asttypes.txt acc
          | Psig_recmodule l ->
            List.fold_left (fun acc b ->
                StringSet.add b.pmd_name.Asttypes.txt acc
              ) acc l
#else
          | Psig_module (l,_) -> StringSet.add l.Asttypes.txt acc
          | Psig_recmodule l  ->
            List.fold_left (fun acc (l,_) ->
                StringSet.add l.Asttypes.txt acc
              ) acc l
#endif
          | _ -> acc
        ) StringSet.empty ast in
  let set =
    if CU.mli cu then aux `MLI
    else if CU.ml cu then aux `ML
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
let depends ?flags ?(deps=[]) resolver dir =
  let pp = match Component.pp_byte deps resolver with
    | [] -> ""
    | pp ->
      let pp = String.concat " " pp in
      sprintf "-pp \"camlp4o %s\"" pp in
  let incl = match Component.comp_byte deps resolver (fun _ -> dir) with
    | [] -> ""
    | ls -> " " ^ String.concat " " ls in
  let names =
    Array.to_list (Sys.readdir dir)
    |> List.filter (fun file ->
        Filename.check_suffix file ".ml" || Filename.check_suffix file ".mli"
      )
    |> List.map Filename.chop_extension in
  let files =
    names
    |> List.map (fun name ->
        if Sys.file_exists (dir / name ^ ".ml") then
          dir / name ^ ".ml"
        else
          dir / name ^ ".mli"
      ) in
  let lines =
    Shell.exec_output
      ~verbose:true
      "ocamldep -one-line -modules %s%s %s"
      pp incl (String.concat " " files) in

  let deps_tbl = Hashtbl.create (List.length names) in
  let add_dep line =
    match split line ' ' with
    | []
    | "Bad"   :: _
    | "File"  :: _
    | "Error" :: _        -> ()
    | name    :: modules  ->
      (* [name] is dir/<name>.ml[,i]: *)
      let name = Filename.(basename @@ chop_extension name) in
      let deps = List.fold_left (fun acc m ->
          if List.mem m names then
            m :: acc
          else if List.mem (String.uncapitalize m) names then
            String.uncapitalize m :: acc
          else
            acc
        ) [] modules in
      Hashtbl.add deps_tbl name deps in
  List.iter add_dep lines;

  let cus_tbl = Hashtbl.create (List.length names) in
  let rec cu name =
    try Hashtbl.find cus_tbl name
    with Not_found ->
      let local_deps =
        try Hashtbl.find deps_tbl name
        with Not_found -> [] in
      let deps = deps @ List.map (fun x -> `CU (cu x)) local_deps in
      let cu = CU.create ?flags ~dir ~deps name in
      Hashtbl.add cus_tbl name cu;
      cu in
  List.map cu names
