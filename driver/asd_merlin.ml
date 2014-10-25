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


open Assemblage

module Directive = struct
  type t =
    [ `S of string
    | `B of string
    | `PKG of string list
    | `EXT of string list ]

  let s s = `S s
  let b s = `B s
  let pkg s = `PKG [s]
  let ext s = `EXT [s]
end

type line = [ Directive.t | `Comment of string ]
type file = line list
type t =
  { s : String.Set.t;
    b : String.Set.t;
    pkg : String.Set.t;
    ext : String.Set.t; }

let map_revapp f = List.fold_left (fun l x -> (f x)::l)

let create ?(s = []) ?(b = []) ?(pkg = []) ?(ext = []) () =
  { s = String.Set.of_list s;
    b = String.Set.of_list b;
    pkg = String.Set.of_list pkg;
    ext = String.Set.of_list ext; }

let add_directive m = function
  | `S   s -> { m with s = String.Set.add s m.s }
  | `B   b -> { m with b = String.Set.add b m.b }
  | `PKG p -> { m with pkg = String.Set.(union (of_list p) m.pkg) }
  | `EXT e -> { m with ext = String.Set.(union (of_list e) m.ext) }

let is_built_component c = List.mem (As_part.kind c) [`Unit; `Lib; `Bin]

let of_project ~build_dir p =
(*
  let r = As_resolver.create ~build_dir () in
  let components = As_project.parts p in
  let s =
    List.fold_left (fun set u -> match As_part.Unit. u with
    | None     -> set
    | Some dir -> String.Set.add dir set
    ) String.Set.empty (filter_map unit_ocaml components)
  in
  let b = List.fold_left (fun set c ->
    String.Set.add (build_dir c r) set
  ) String.Set.empty (keep is_built_component components) in
  let pkg =
    set_of_list (List.rev_map Pkg.name (filter_map pkg_ocaml    components))
  in
  let ext =
    set_of_list (List.rev_map Pkg.name (filter_map pkg_ocaml_pp components))
  in
  { s; b; pkg; ext; }
*)
  { s = String.Set.empty; b = String.Set.empty;
    pkg = String.Set.empty; ext = String.Set.empty }

let prepare m =
  let dl =
    (`Comment "")::(map_revapp Directive.s [] (String.Set.elements m.s))
  in
  let dl =
    (`Comment "")::(map_revapp Directive.b dl (String.Set.elements m.b))
  in
  let dl =
    (`Comment "")::(map_revapp Directive.pkg dl (String.Set.elements m.pkg))
  in
  let dl =
    (`Comment "")::(map_revapp Directive.ext dl (String.Set.elements m.ext))
  in
  (`Comment " include assemblage package for editor assistance in assemble.ml")
  :: (`PKG ["assemblage"])
  :: dl

let to_buffer m =
  let ll = prepare m in
  let bprintf = Printf.bprintf in
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
