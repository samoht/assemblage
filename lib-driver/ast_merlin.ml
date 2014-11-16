(*
 * Copyright (c) 2014 David Sheets <sheets@alum.mit.edu>
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

open Assemblage
open Assemblage.Private

let str = Printf.sprintf

(* Merlin project file *)

type directive =
  [ `REC | `S of string | `B of string | `PKG of string
  | `FLG of string list | `EXT of string list ]

type t = [ `Comment of string | `Blank | directive ] list

let to_string m =
  let b = Buffer.create 1024 in
  let pr fmt = Printf.bprintf b fmt in
  let add = function
  | `Blank -> pr "\n"
  | `Comment c -> pr "# %s\n" c
  | `S s -> pr "S %s\n" s
  | `B bdir -> pr "B %s\n" bdir
  | `PKG pkg -> pr "PKG %s\n" pkg
  | `FLG flags -> pr "FLG %s\n" (String.concat " " flags)
  | `EXT exts -> pr "EXT %s\n" (String.concat " " exts)
  | `REC -> pr "REC\n"
  in
  List.iter add m;
  Buffer.contents b

(* From assemblage project *)

let project_ocamlfind_pkgs proj =
  let add pkgs p = match Pkg.kind p with
  | `OCaml `OCamlfind -> String.Set.add (Part.name p) pkgs
  | _ -> pkgs
  in
  let init = (String.Set.singleton "assemblage") in
  Part.list_fold_kind_rec `Pkg add init (Project.parts proj)

let of_project p : t =
  let add v acc = v :: acc in
  let pkgs = project_ocamlfind_pkgs p in
  let rev_pkgs = String.Set.fold (fun pkg acc -> `PKG pkg :: acc) pkgs [] in
  let rev_ss =
    let srcs = Project.products ~kind:`Src p in
    let add_dir p acc = match Path.ext p with
    | Some (`Ml | `Mli) -> Path.Set.add (Path.dirname p) acc
    | _ -> acc
    in
    let ss = Path.Set.fold add_dir srcs Path.Set.empty in
    Path.(Set.fold (fun p acc -> `S (to_string p) :: acc) ss [])
  in
  let rev_bs =
    let builds = Project.products ~kind:`Build p in
    let add_dir p acc = match Path.ext p with
    | Some (`Cmi | `Cmti | `Cmt) -> Path.Set.add (Path.dirname p) acc
    | _ -> acc
    in
    let bs = Path.Set.fold add_dir builds Path.Set.empty in
    Path.(Set.fold (fun p acc -> `B (to_string p) :: acc) bs [])
  in
  add (`Comment (Project.watermark_string p)) @@
  add `Blank @@
  List.rev_append rev_pkgs (List.rev_append rev_ss (List.rev rev_bs))
