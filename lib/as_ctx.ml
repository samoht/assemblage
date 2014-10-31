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

let str = Printf.sprintf

(* Context elements *)

type build_phase =
  [ `Prepare | `Gen | `Dep | `Pp | `Compile | `Archive | `Link | `Doc ]

type language = [ `OCaml | `C | `Js ]
type ocaml_source = [ `Ml | `Mli ]
type ocaml_target = [ `Byte | `Native | `Js ]
type archive_product = [ `Shared ]
type command = [ `Cmd of string As_conf.key ]
type tag = [ `Tag of string ]

module Elt = struct
  type t = [ build_phase | language | ocaml_source | ocaml_target
           | archive_product | command | tag ]

  let compare (e : t) (e' : t) = match e, e' with
  | `Cmd k0, `Cmd k1 -> As_conf.Key.(compare (V k0) (V k1))
  | `Cmd _, _ | _, `Cmd _ -> -1
  | c, c' -> compare c c'

  let pp ppf = function
  | `Prepare -> As_fmt.pp_str ppf "prepare"
  | `Gen -> As_fmt.pp_str ppf "gen"
  | `Dep -> As_fmt.pp_str ppf "dep"
  | `Pp -> As_fmt.pp_str ppf "pp"
  | `Compile -> As_fmt.pp_str ppf "compile"
  | `Archive -> As_fmt.pp_str ppf "archive"
  | `Link -> As_fmt.pp_str ppf "link"
  | `Doc -> As_fmt.pp_str ppf "doc"
  | `Test -> As_fmt.pp_str ppf "test"
  | `OCaml -> As_fmt.pp_str ppf "ocaml"
  | `C -> As_fmt.pp_str ppf "c"
  | `Js -> As_fmt.pp_str ppf "js"
  | `Ml -> As_fmt.pp_str ppf "ml"
  | `Mli -> As_fmt.pp_str ppf "mli"
  | `Byte -> As_fmt.pp_str ppf "byte"
  | `Native -> As_fmt.pp_str ppf "native"
  | `Shared -> As_fmt.pp_str ppf "shared"
  | `Cmd k -> As_fmt.pp ppf "cmd:%s" (As_conf.Key.name k)
  | `Tag t -> As_fmt.pp ppf "tag:%s" t
end

let pp_elt = Elt.pp

(* Contexts *)

include Set.Make (Elt)

let v elts = List.fold_left (fun acc e -> add e acc) empty elts
let matches = subset

let pp ppf c =
  let pp_sep ppf () = As_fmt.pp ppf "," in
  As_fmt.pp_list ~pp_sep pp_elt ppf (elements c)
