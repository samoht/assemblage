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

type kind = [ `Lib | `Bin | `Sbin | `Toplevel | `Share | `Share_root
            | `Etc | `Doc | `Misc | `Stublibs | `Man | `Other of string ]

type meta = { install : bool }

let inj, proj = As_part.meta_key ()
let get_meta p = As_part.get_meta proj p
let meta ?(install = true) () = inj { install }

let install p = (get_meta p).install

let kind p = match p.As_part.name with
| "lib" -> `Lib | "bin" -> `Bin | "sbin" -> `Sbin | "toplevel" -> `Toplevel
| "share" -> `Share | "share_root" -> `Share_root | "etc" -> `Etc
| "doc" -> `Doc | "misc" -> `Misc | "stublibs" -> `Stublibs | "man" -> `Man
| k -> `Other k

let name_of_kind = function
| `Lib -> "lib" | `Bin -> "bin" | `Sbin -> "sbin" | `Toplevel -> "toplevel"
| `Share -> "share" | `Share_root -> "share_root" | `Etc -> "etc"
| `Doc -> "doc" | `Misc -> "misc" | `Stublibs -> "stublibs" | `Man -> "man"
| `Other o -> o

(* Create *)

let create ?cond ?keep ?install kind ps =
  let meta = meta ?install () in
  As_part.create ?cond (name_of_kind kind) `Dir meta

let of_base ?install p =
  let meta = meta ?install () in
  { p with As_part.kind = `Dir; meta = meta }

(* Product filters *)

let default _ = failwith "TODO"
