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

type t = string

let to_string m = m
let of_project t = "TODO"
(*
  let libs = Part.(keep_map Lib.ocaml) (Project.parts t) in
  let buf = Buffer.create 1024 in
  let one lib =
    let requires =
      conmap
          Part.deps ([lib])
      |> Part.(keep_map Pkg.ocaml)
      |> List.map Part.name
      |> String.concat " "
    in
    let name = Part.name lib in
    Printf.bprintf buf "version  = \"%s\"\n" version;
    Printf.bprintf buf "requires = \"%s\"\n" requires;
    Printf.bprintf buf "archive(byte) = \"%s.cma\"\n" name;
    Printf.bprintf buf "archive(byte, plugin) = \"%s.cma\"\n" name;
    Printf.bprintf buf "archive(native) = \"%s.cmxa\"\n" name;
    Printf.bprintf buf "archive(native, plugin) = \"%s.cmxs\"\n" name;
    Printf.bprintf buf "exist_if = \"%s.cma\"\n" name
  in
    List.iteri (fun i lib ->
      if i = 0 then one lib
      else (
        Printf.bprintf buf "package \"%s\" (" (Part.name lib);
        one lib;
        Printf.bprintf buf ")\n"
      )
    ) libs;
    Buffer.contents buf
*)
