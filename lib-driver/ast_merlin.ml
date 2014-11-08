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

let of_project p : t =
  let add v acc = v :: acc in
(*  let add_if b v acc = if b then v :: acc else acc in *)
  let pkgs = String.Set.singleton "assemblage" (* TODO *) in
  let rev_pkgs = String.Set.fold (fun pkg acc -> `PKG pkg :: acc) pkgs [] in
  let products = Project.products p in
  let ss, bs =
    let add p (ss, bs as acc) = match Path.ext p with
    | Some (`Ml | `Mli) -> Path.Set.add (Path.dirname p) ss, bs
    | Some (`Cmi | `Cmti | `Cmt) -> ss, Path.Set.add (Path.dirname p) bs
    | _ -> acc
    in
    Path.Set.fold add products (Path.Set.empty, Path.Set.empty)
  in
  let rev_ss = Path.(Set.fold (fun p acc -> `S (to_string p) :: acc) ss []) in
  let rev_bs = Path.(Set.fold (fun p acc -> `B (to_string p) :: acc) bs []) in
  add (`Comment (Project.watermark_string p)) @@
  add `Blank @@
  List.rev_append rev_pkgs (List.rev_append rev_ss (List.rev rev_bs))