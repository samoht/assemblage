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

  let of_project ?(add = []) p =
    let header = `Header (Some (Project.watermark_string p)) in
    header, add (* TODO *)
end
