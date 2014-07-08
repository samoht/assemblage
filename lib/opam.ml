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

let (/) = Filename.concat

module Install = struct

  type t = {
    name    : string;
    contents: string;
  }

  let opt f =
    if f = Feature.true_ then ""
    else "?"

  let of_project ?(meta=true) ~build_dir t =
    let name = Project.name t in
    let components = components t in
    let libs = Component.(filter lib components) in
    let bins =
      List.filter Project.Bin.install Component.(filter bin components) in
    let buf = Buffer.create 1024 in
    let resolver =
      Resolver.create
        ~build_dir
        ~pkgs:(fun _ -> Flags.empty) in
    if libs <> [] then (
      bprintf buf "lib: [\n";
      if meta then bprintf buf "  \"META\"\n";
      List.iter (fun l ->
          let gens = Lib.generated_files l resolver in
          List.iter (fun (flags, files) ->
              List.iter (fun file ->
                  bprintf buf "  \"%s%s\"\n" (opt flags) file
                ) files;
            ) gens;
        ) libs;
      bprintf buf "]\n");
    if bins <> [] then (
      bprintf buf "bin: [\n";
      List.iter (fun b ->
          let gens = Bin.generated_files b resolver in
          List.iter (fun (flags, files) ->
              List.iter (fun file ->
                  bprintf buf "  \"%s%s\" {\"%s\"}\n"
                    (opt flags) file (Bin.name b)
                ) files;
            ) gens;
        ) bins;
      bprintf buf "]\n";
    );
    if libs <> [] then (
      let mk fmt =
        ksprintf (fun file ->
            bprintf buf "  \"?%s/%s\"\n" (Project.doc_dir t) file
          ) fmt in
      bprintf buf "doc: [\n";
      mk "index.html";
      mk "index_attributes.html";
      mk "index_class_types.html";
      mk "index_classes.html";
      mk "index_exceptions.html";
      mk "index_methods.html";
      mk "index_module_types.html";
      mk "index_modules.html";
      mk "index_types.html";
      mk "index_values.html";
      mk "style.css";
      List.iter (fun l ->
          let units = Lib.compilation_units l in
          List.iter (fun u ->
              let name = String.capitalize (CU.name u) in
              mk "%s.html" name;
              mk "type_%s.html" name;
              let modules =
                if CU.generated u then []
                else OCaml.modules ~build_dir u in
              List.iter (fun m ->
                  mk "%s.%s.html" name m
                ) modules
            ) units;
        ) libs;
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
