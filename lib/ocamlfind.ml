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
open Project

let (/) = Filename.concat

let p4o names =
  let names = String.concat " " names in
  [sprintf
     "$(shell ocamlfind query %s -r -predicates byte,syntax -format \"-I %%d %%a\")"
     names]

let incl names =
  let names = String.concat " "  (List.rev names) in
  [sprintf
     "$(shell ocamlfind query %s -r -predicates byte -format \"-I %%d\")"
     names]

let bytlink names =
  let names = String.concat " "  (List.rev names) in
  [sprintf
     "$(shell ocamlfind query %s -r -predicates byte -format \"-I %%d %%a\")"
     names]

let natlink names =
  let names = String.concat " "  (List.rev names) in
  [sprintf
     "$(shell ocamlfind query %s -r -predicates native -format \"-I %%d %%a\")"
     names]

module META = struct

  type t = {
    file: string;
    contents: string;
  }

  let string_exists s fn =
    let exists = ref false in
    String.iter (fun c ->
        exists := !exists || fn c
      ) s;
    !exists

  let create ~version ~libs conf =
    match libs with
    | [] -> None
    | _  ->
      let others, main =
        List.partition (fun l -> string_exists (Lib.name l) ((=) '.')) libs in
      let main = match main with
        | [m] -> m
        | []  -> failwith "Missing toplevel library"
        | _   -> failwith "Too many toplevel libraries" in
      let others = List.map (fun l ->
          let s = Lib.name l in
          let i = String.rindex s '.' in
          let p = String.sub s 0 i in
          let r = String.sub s (i+1) (String.length s - i - 1) in
          if p <> Lib.name main then
            failwith (sprintf "%s: invalid library name, it should start with %s."
                        s (Lib.name main))
          else
            (r, l)
        ) others in
      let buf = Buffer.create 1024 in
      let aux lib =
        let requires =
          Lib.deps lib
          |> Dep.get_pkgs
          |> String.concat " " in
        bprintf buf "version  = \"%s\"\n" version;
        bprintf buf "requires = \"%s\"\n" requires;
        bprintf buf "archive(byte) = \"%s.cma\"\n" (Lib.name lib);
        bprintf buf "archive(byte, plugin) = \"%s.cma\"\n" (Lib.name lib);
        if Conf.native conf then
          bprintf buf "archive(native) = \"%s.cmxa\"\n" (Lib.name lib);
        if Conf.native_dynlink conf then
          bprintf buf "archive(native, plugin) = \"%s.cmxs\"\n" (Lib.name lib);
        bprintf buf "exist_if = \"%s.cma\"\n" (Lib.name lib) in
      aux main;
      List.iter (fun (name, l) ->
          bprintf buf "package \"%s\" (" name;
          aux l;
          bprintf buf ")\n"
        ) others;
      let contents = Buffer.contents buf in
      let file = Conf.destdir conf / Lib.name main / "META" in
      Some { file; contents }

  let write t =
    printf "\027[36m+ write %s\027[m\n" t.file;
    let oc = open_out t.file in
    output_string oc t.contents;
    close_out oc

  let of_project t =
    let conf = Project.conf t in
    let libs = Project.libs t in
    let version = match Project.version t with
      | None   -> "version"
      | Some v -> v in
    match create ~version ~libs conf with
    | None   -> ()
    | Some t -> write t

end
