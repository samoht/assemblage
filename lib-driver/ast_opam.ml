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

  type move = Path.t * Path.t option * bool

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
      pr "%s\"" (Path.to_string src);
      begin match dst with
      | None -> ()
      | Some dst -> pr " {\"%s\"}" (Path.to_string dst)
      end;
      field
    in
    (match header with None -> () | Some h -> pr "# %s\n\n" h);
    let moves = List.sort compare (List.rev_map str_field_elt moves) in
    let last = List.fold_left add_move "" moves in
    if last <> "" then (* close last field *) pr " ]\n";
    Buffer.contents b

  (* For an assemblage project *)

  let err_abs_output = format_of_string
      "`Dir@ part@ has@ an@ absolute@ output@ (%a)@ (custom@ `Dir@ part ?)."

  let err_no_prefix = format_of_string
      "`Dir@ part@ product@ (%a) is@ not@ a@ prefix@ of@ part@ directory\
       @ root (%a) (custom `Dir@ part ?)."

  let of_project ?(add = []) proj =
    let add_outputs ?prefix dir_root acc outputs elt =
      let add_output acc output = match Path.is_rel output with
      | false -> Log.err err_abs_output Path.pp output; acc
      | true ->
          match Path.rem_prefix dir_root output with
          | None ->
              Log.err err_no_prefix Path.pp output Path.pp dir_root;
              acc
          | Some dst ->
              let dst = match prefix with
              | None -> dst
              | Some other -> Path.(other // dst)
              in
              elt (move output ~dst) :: acc
      in
      List.fold_left add_output acc outputs
    in
    let add_dir acc dir =
      if not (Dir.install dir && Project.eval proj (Part.exists dir))
      then acc else
      let dir_root = Project.eval proj (Part.root dir) in
      let actions = Project.eval proj (Part.actions dir) in
      let outputs = Action.list_outputs actions in
      let add_outputs ?prefix = add_outputs ?prefix dir_root acc outputs in
      match Dir.kind dir with
      | `Bin -> add_outputs (fun m -> `Bin m)
      | `Doc -> add_outputs (fun m -> `Doc m)
      | `Etc -> add_outputs (fun m -> `Etc m)
      | `Lib -> add_outputs (fun m -> `Lib m)
      | `Man -> add_outputs (fun m -> `Man m)
      | `Other o -> add_outputs ~prefix:o (fun m -> `Misc m)
      | `Sbin -> add_outputs (fun m -> `Sbin m)
      | `Share -> add_outputs (fun m -> `Share m)
      | `Share_root -> add_outputs (fun m -> `Share_root m)
      | `Stublibs -> add_outputs (fun m -> `Stublibs m)
      | `Toplevel -> add_outputs (fun m -> `Toplevel m)
    in
    let header = `Header (Some (Project.watermark_string proj)) in
    let init = add in
    header, Part.list_fold_kind `Dir add_dir init (Project.parts proj)
end
