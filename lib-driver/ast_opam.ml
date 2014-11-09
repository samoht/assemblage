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

open Assemblage
open Assemblage.Private

(* Metadata synchronization *)

module Sync = struct

  (* Heeeeeeeeeelp ! *)

end

(* Install files *)

module Install = struct

  type move = string * string option * bool

  let move ?(maybe = false) ?dst src = (src, dst, maybe)

  type field_elt =
    [ `Bin of move | `Doc of move | `Etc of move | `Lib of move | `Man of move
    | `Misc of move | `Sbin of move | `Share of move | `Share_root of move
    | `Stublibs of move | `Toplevel of move ]

  let str_field_elt = function
  | `Bin m -> "bin", m
  | `Doc m -> "doc", m
  | `Etc m -> "etc", m
  | `Lib m -> "lib", m
  | `Man m -> "man", m
  | `Misc m -> "misc", m
  | `Sbin m -> "sbin", m
  | `Share m -> "share", m
  | `Share_root m -> "share_root", m
  | `Stublibs m -> "stublibs", m
  | `Toplevel m -> "toplevel", m

  type t = [ `Header of string option ] * field_elt list

  let to_string (`Header header, moves)  =
    let b = Buffer.create 1024 in
    let pr fmt = Printf.bprintf b fmt in
    let add_move last (field, (src, dst, maybe)) =
      if last = field then pr "\n  \"" else
      begin
        if last <> "" then (* close last field *) pr " ]\n";
        pr "%s: [\n  \"" field;
      end;
      if maybe then pr "?";
      pr "%s\"" src;
      (match dst with None -> () | Some dst -> pr " {\"%s\"}" dst);
      field
    in
    (match header with None -> () | Some h -> pr "# %s\n\n" h);
    let moves = List.sort compare (List.rev_map str_field_elt moves) in
    let last = List.fold_left add_move "" moves in
    if last <> "" then (* close last field *) pr " ]\n";
    Buffer.contents b

  (* For an assemblage project *)

  let of_project ?(add = []) proj =
    let init = add in
    let add_products ?prefix elt d acc =
      let add_product acc p =
        (* FIXME handle dst correctly: we should respect the hierarchy that
           the products give us up to the the Dir build root. *)
        let dst = match prefix with
        | None -> Path.(basename p)
        | Some other -> Path.(to_string (other / (basename p)))
        in
        elt (move (Path.to_string p) ~dst) :: acc
      in
      List.fold_left add_product acc (Project.eval proj (Part.products d))
    in
    let add_dir acc d =
      if not (Dir.install d) && Project.eval proj (Part.cond d) then acc else
      match Dir.kind d with
      | `Bin -> add_products (fun m -> `Bin m) d acc
      | `Doc -> add_products (fun m -> `Doc m) d acc
      | `Etc -> add_products (fun m -> `Etc m) d acc
      | `Lib -> add_products (fun m -> `Lib m) d acc
      | `Man -> add_products (fun m -> `Man m) d acc
      | `Other o -> add_products ~prefix:o (fun m -> `Misc m) d acc
      | `Sbin -> add_products (fun m -> `Sbin m) d acc
      | `Share -> add_products (fun m -> `Share m) d acc
      | `Share_root -> add_products (fun m -> `Share_root m) d acc
      | `Stublibs -> add_products (fun m -> `Stublibs m) d acc
      | `Toplevel -> add_products (fun m -> `Toplevel m) d acc
    in
    let header = `Header (Some (Project.watermark_string proj)) in
    let dirs = Part.keep_kind `Dir (Project.parts proj) in
    header, List.fold_left add_dir init dirs
end
