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

open Printf

let (/) = Filename.concat

module Install = struct

  type t = {
    name    : string;
    contents: string;
  }

  let opt f =
    if f = As_features.true_ || f = As_features.byte then ""
    else "?"

  let of_project ?(meta=true) ~build_dir t =
    let r = As_resolver.create ~build_dir () in
    let name = As_project.name t in
    let components = As_project.components t in
    let libs = As_project.Component.(filter lib_ocaml components) in
    let bins =
      List.filter As_project.Bin.install As_project.Component.(filter bin components) in
    let buf = Buffer.create 1024 in
    if libs <> [] then (
      bprintf buf "lib: [\n";
      if meta then bprintf buf "  \"META\"\n";
      List.iter (fun l ->
          let gens = As_project.Lib.generated_files l in
          List.iter (fun (flags, files) ->
              List.iter (fun file ->
                  let file = As_project.Component.file (`Lib l) r file in
                  bprintf buf "  \"%s%s\"\n" (opt flags) file
                ) files;
            ) gens;
        ) libs;
      bprintf buf "]\n");
    if bins <> [] then (
      bprintf buf "bin: [\n";
      List.iter (fun b ->
          let gens = As_project.Bin.generated_files b in
          List.iter (fun (flags, files) ->
              List.iter (fun file ->
                  let file = As_project.Component.file (`Bin b) r file in
                  bprintf buf "  \"%s%s\" {\"%s\"}\n"
                    (opt flags) file (As_project.Bin.name b)
                ) files;
            ) gens;
        ) bins;
      bprintf buf "]\n";
    );
    (* FIXME: output doc and other dirs *)
    let contents = Buffer.contents buf in
    { name; contents }

  let write ?dir t =
    let file =
      let f = t.name ^ ".install" in
      match dir with
      | None   -> f
      | Some d -> d / f in
    printf "%s write %s\n" (As_shell.color `Green "==>") file;
    let oc = open_out file in
    output_string oc t.contents;
    close_out oc

end
