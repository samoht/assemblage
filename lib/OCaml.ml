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

(* XXX: read the cmt instead *)
let modules ~build_dir comp =
  let resolver = Ocamlfind.resolver `Direct build_dir in
  let () =
    let pp =
      Comp.deps comp
      |> Dep.closure
      |> Dep.(filter pkg_pp)
      |> Resolver.pkgs resolver in
    match Flags.pp_byte pp with
    | [] -> Clflags.preprocessor := None;
    | pp ->
      let pp = String.concat " " pp in
      Clflags.preprocessor := Some (sprintf "camlp4o %s" pp) in
  let aux = function
    | `ML ->
      let file = Comp.dir comp // Comp.name comp ^ ".ml" in
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
      let file = Comp.dir comp // Comp.name comp ^ ".mli" in
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
    if Comp.mli comp then aux `MLI
    else if Comp.ml comp then aux `ML
    else StringSet.empty in
  StringSet.elements set
