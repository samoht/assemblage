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
    (* FIXME this completly is broken *)
    if f = As_features.true_ || f = As_features.byte then ""
    else "?"

  let lib_artifacts r buf lib =
    let library = `Lib lib in
    let add_files ?exts c =
      let keep file = match exts with
      | None -> true
      | Some exts -> List.mem file exts
      in
      let add_file f = bprintf buf "  \"?%s\"\n" (As_component.file c r f) in
      let files =
        let add_files acc (_, files) =
          let add_file acc f =
            if keep f then As_action.FileSet.add f acc else acc
          in
          List.fold_left add_file acc files
        in
        List.fold_left add_files As_action.FileSet.empty (As_component.files c)
      in
      As_action.FileSet.iter add_file files
    in
    let us = As_component.(filter_map unit (contents library)) in
    let keep itfs u = List.mem (As_component.Unit.interface u) itfs in
    let ifaces = List.filter (keep [`Normal; `Opaque]) us in
    let cross_inline = List.filter (keep [`Normal]) us in
    add_files library;
    List.iter (fun u -> add_files ~exts:[`Cmi; `Cmti; `Mli] (`Unit u)) ifaces;
    List.iter (fun u -> add_files ~exts:[`Cmx] (`Unit u)) cross_inline;
    ()

  let of_project ?(meta=true) ~build_dir t =
    let r = As_resolver.create ~build_dir () in
    let name = As_project.name t in
    let components = As_project.components t in
    let libs = As_component.(filter_map lib_ocaml components) in
    let bins =
      List.filter As_component.Bin.install
        As_component.(filter_map bin components)
    in
    let buf = Buffer.create 1024 in
    if libs <> [] then begin
      bprintf buf "lib: [\n";
      if meta then bprintf buf "  \"META\"\n";
      List.iter (lib_artifacts r buf) libs;
      bprintf buf "]\n"
    end;
    if bins <> [] then (
      bprintf buf "bin: [\n";
      List.iter (fun b ->
          let gens = As_component.files (`Bin b) in
          List.iter (fun (flags, files) ->
              List.iter (fun file ->
                  let file = As_component.file (`Bin b) r file in
                  bprintf buf "  \"%s%s\" {\"%s\"}\n"
                    (opt flags) file (As_component.name (`Bin b))
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
