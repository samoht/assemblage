(*
 * Copyright (c) 2014 David Sheets <sheets@alum.mit.edu>
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

open Printf

module SSet = Set.Make(String)

module Directive = struct

  type t = [
  | `S of string
  | `B of string
  | `PKG of string list
  | `EXT of string list
  ]

  let s s = `S s

  let b s = `B s

  let pkg s = `PKG [s]

  let ext s = `EXT [s]

end

type line = [ Directive.t | `Comment of string ]
type file = line list
type t = {
  s   : SSet.t;
  b   : SSet.t;
  pkg : SSet.t;
  ext : SSet.t;
}

let map_revapp f = List.fold_left (fun l x -> (f x)::l)

let set_of_list = List.fold_left (fun set x -> SSet.add x set) SSet.empty

let create ?(s=[]) ?(b=[]) ?(pkg=[]) ?(ext=[]) () = {
  s   = set_of_list s;
  b   = set_of_list b;
  pkg = set_of_list pkg;
  ext = set_of_list ext;
}

let add_directive m = function
  | `S   s -> { m with   s=SSet.add s m.s }
  | `B   b -> { m with   b=SSet.add b m.b }
  | `PKG p -> { m with pkg=SSet.union (set_of_list p) m.pkg }
  | `EXT e -> { m with ext=SSet.union (set_of_list e) m.ext }

let of_project ~build_dir p =
  let r = As_resolver.create ~build_dir () in
  let open As_component in
  let components = As_project.components p in
  let s =
    List.fold_left (fun set u -> match Unit.source_dir u with
    | None     -> set
    | Some dir -> SSet.add dir set
    ) SSet.empty (filter_map unit_ocaml components)
  in
  let b = List.fold_left (fun set c ->
    SSet.add (build_dir c r) set
  ) SSet.empty components in
  let pkg =
    set_of_list (List.rev_map Pkg.name (filter_map pkg_ocaml    components))
  in
  let ext =
    set_of_list (List.rev_map Pkg.name (filter_map pkg_ocaml_pp components))
  in
  { s; b; pkg; ext; }

let prepare m =
  let dl = (`Comment "")::(map_revapp Directive.s   [] (SSet.elements m.s)) in
  let dl = (`Comment "")::(map_revapp Directive.b   dl (SSet.elements m.b)) in
  let dl = (`Comment "")::(map_revapp Directive.pkg dl (SSet.elements m.pkg)) in
  let dl = (`Comment "")::(map_revapp Directive.ext dl (SSet.elements m.ext)) in
  (`Comment " include assemblage package for editor assistance in assemble.ml")
  ::(`PKG ["assemblage"])
  ::dl

let to_buffer m =
  let ll = prepare m in
  let buf = Buffer.create 1024 in
  let rec add_rest = function
    | [] -> ()
    | (`Comment "")::rest -> bprintf buf "\n";         add_rest rest
    | (`Comment  c)::rest -> bprintf buf "#%s\n" c;    add_rest rest
    | (`S s)       ::rest -> bprintf buf "S %s\n" s;   add_rest rest
    | (`B b)       ::rest -> bprintf buf "B %s\n" b;   add_rest rest
    | (`PKG pkgs)  ::rest ->
      bprintf buf "PKG %s\n" (String.concat " " pkgs); add_rest rest
    | (`EXT exts)  ::rest ->
      bprintf buf "EXT %s\n" (String.concat " " exts); add_rest rest
  in
  add_rest ll;
  buf

let to_string ll = Buffer.contents (to_buffer ll)

let write_file file ll =
  let oc = open_out file in
  Buffer.output_buffer oc (to_buffer ll);
  close_out oc
