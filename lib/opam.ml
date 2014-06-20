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
open Printf
open OpamTypes

let (/) = Filename.concat

type t = OpamFile.OPAM.t

let read () =
  let opam = OpamFilename.of_string "opam" in
  let opam_opam = OpamFilename.of_string "opam/opam" in
  if OpamFilename.exists opam then
    Some (OpamFile.OPAM.read opam)
  else if OpamFilename.exists opam_opam then
    Some (OpamFile.OPAM.read opam_opam)
  else
    None

let write t =
  OpamFile.OPAM.write (OpamFilename.of_string "opam") t

module Install = struct

  type t = {
    name    : string;
    contents: string;
  }

  let opt = function
    | [] -> ""
    | _  -> "?"

  let of_project ?(meta=true) ?(buildir="_build") t =
    let name = Project.name t in
    let libs = Project.libs t in
    let bins = Project.bins t in
    let tops = Project.tops t in
    let buf = Buffer.create 1024 in
    if libs <> [] then (
      bprintf buf "lib: [\n";
      bprintf buf "  \"META\"\n";
      List.iter (fun l ->
          let files = Lib.generated_files l in
          List.iter (fun (flags, file) ->
              bprintf buf "  \"%s%s\"\n" (opt flags) (buildir / file)
            ) files;
        ) libs;
      bprintf buf "]\n");
    if bins <> [] || tops <> [] then (
      bprintf buf "bin: [\n";
      List.iter (fun b ->
          let files = Bin.generated_files b in
          List.iter (fun (flags, file) ->
              bprintf buf "  \"%s%s\" {\"%s\"}\n"
                (opt flags) (buildir / file) (Bin.name b)
            ) files;
        ) bins;
      List.iter (fun t ->
          let files = Top.generated_files t in
          List.iter (fun (flags, file) ->
              bprintf buf "  \"%s%s\" {\"%s\"}\n"
                (opt flags) (buildir / file) (Top.name t)
            ) files;
        ) tops;
      bprintf buf "]\n";
    );
    let contents = Buffer.contents buf in
    { name; contents }

  let write ?dir t =
    let file =
      let f = t.name ^ ".install" in
      match dir with
      | None   -> f
      | Some d -> d / f in
    printf "\027[36m+ write %s\027[m\n" file;
    let oc = open_out file in
    output_string oc t.contents;
    close_out oc

end
