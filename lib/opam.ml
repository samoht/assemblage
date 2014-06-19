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

let version t =
  match read () with
  | None   -> Git.version ()
  | Some t ->
    let git_version = match Git.version () with
      | None   -> ""
      | Some v -> "-" ^ v in
    Some (OpamPackage.Version.to_string (OpamFile.OPAM.version t)
          ^ git_version)

let name t =
  match read () with
  | None   -> None
  | Some t -> Some (OpamPackage.Name.to_string (OpamFile.OPAM.name t))

(*
let with_configure t flags =
  let build = match OpamFile.OPAM.build t with
    | ( (CString "opam", o) :: (CString "configure", c) :: _ , f) :: t ->
      let flags = List.map (fun f ->
          CString (sprintf "--%%{enable:%s}%%-%s" (Flag.name f) (Flag.name f)), None
        ) flags in
      ( (CString "opam", o) :: (CString "configure", c) :: flags, f) :: t
    | l -> l in
  OpamFile.OPAM.with_build t build
*)

module Install = struct

  type t = {
    name    : string;
    contents: string;
  }

  let create ?(libs=[]) ?(bins=[]) ?(tops=[]) env =
    let name = match Env.name env with
      | None   -> "not-set"
      | Some n -> n in
    let buf = Buffer.create 1024 in
    if libs <> [] then (
      bprintf buf "lib: [\n";
      bprintf buf " \"%s/META\"" (Env.destdir env / name);
      List.iter (fun l ->
          let files = Lib.generated_files l in
          List.iter (fun (flags, file) ->
              if Env.enable env flags then bprintf buf "  \"%s\"\n" file
            ) files;
        ) libs;
      bprintf buf "]\n");
    if bins <> [] || tops <> [] then (
      bprintf buf "bin: [\n";
      List.iter (fun b ->
          let files = Bin.generated_files b in
          List.iter (fun (flags, file) ->
              if Env.enable env flags then bprintf buf "  \"%s\"\n" file
            ) files;
        ) bins;
      List.iter (fun t ->
          let files = Top.generated_files t in
          List.iter (fun (flags, file) ->
              if Env.enable env flags then bprintf buf "  \"%s\"\n" file
            ) files;
        ) tops;
      bprintf buf "]\n";
    );
    let contents = Buffer.contents buf in
    { name; contents }

  let write t =
    let file = t.name ^ ".install" in
    printf "\027[36m+ write %s\027[m\n" file;
    let oc = open_out file in
    output_string oc t.contents;
    close_out oc

  let of_project t env =
    let libs = Project.libs t in
    let bins = Project.bins t in
    let tops = Project.tops t in
    let t = create ~libs ~bins ~tops env in
    write t

end
