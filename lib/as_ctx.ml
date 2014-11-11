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

type tag = [ `Tag of string ]
type language = [ `OCaml | `C | `Js | `Lang of string ]
type build_phase =
  [ `Gen | `Dep | `Pp | `Compile | `Archive of [ `Static | `Shared ] | `Link
  | `Doc ]

type source = [ `Src of As_path.ext ]
type target = [ `Target of [`Src | `Byte | `Native | `Js | `Other of string ]]
type command = [ `Cmd of string As_conf.key ]
type part_usage = [ `Build | `Dev | `Doc | `Other of string | `Outcome | `Test ]
type part_kind =
  [ `Base | `Bin | `Dir | `Doc | `Lib | `Pkg | `Run | `Silo | `Unit ]

type part = [ `Part of [ part_usage | part_kind | `Name of string ]]

module Elt = struct
  type t = [ tag | language | build_phase | source | target | command | part ]

  let compare (e : t) (e' : t) = match e, e' with
  | `Cmd k0, `Cmd k1 -> As_conf.Key.(compare (V k0) (V k1))
  | `Cmd _, _ | _, `Cmd _ -> -1
  | c, c' -> compare c c'

  let pp_target ppf = function
  | `Byte -> As_fmt.pp_str ppf "byte"
  | `Js -> As_fmt.pp_str ppf "js"
  | `Native -> As_fmt.pp_str ppf "native"
  | `Other o -> As_fmt.pp_str ppf o
  | `Src -> As_fmt.pp_str ppf "src"

  let pp_kind ppf = function
  | `Base -> As_fmt.pp_str ppf "base"
  | `Bin -> As_fmt.pp_str ppf "bin"
  | `Dir -> As_fmt.pp_str ppf "dir"
  | `Doc -> As_fmt.pp_str ppf "doc"
  | `Lib -> As_fmt.pp_str ppf "lib"
  | `Pkg -> As_fmt.pp_str ppf "pkg"
  | `Run -> As_fmt.pp_str ppf "run"
  | `Silo -> As_fmt.pp_str ppf "silo"
  | `Unit -> As_fmt.pp_str ppf "unit"

  let pp_usage ppf = function
  | `Build -> As_fmt.pp_str ppf "build"
  | `Dev -> As_fmt.pp_str ppf "dev"
  | `Doc -> As_fmt.pp_str ppf "doc"
  | `Other s -> As_fmt.pp_str ppf s
  | `Outcome -> As_fmt.pp_str ppf "outcome"
  | `Test -> As_fmt.pp_str ppf "test"

  let pp ppf = function
  | `Archive `Shared -> As_fmt.pp_str ppf "archive:shared"
  | `Archive `Static -> As_fmt.pp_str ppf "archive:static"
  | `C -> As_fmt.pp_str ppf "c"
  | `Cmd k -> As_fmt.pp ppf "cmd:%s" (As_conf.Key.name k)
  | `Compile -> As_fmt.pp_str ppf "compile"
  | `Dep -> As_fmt.pp_str ppf "dep"
  | `Doc -> As_fmt.pp_str ppf "doc"
  | `Gen -> As_fmt.pp_str ppf "gen"
  | `Js -> As_fmt.pp_str ppf "js"
  | `Lang l -> As_fmt.pp ppf "lang:%s" l
  | `Link -> As_fmt.pp_str ppf "link"
  | `OCaml -> As_fmt.pp_str ppf "ocaml"
  | `Part (#part_kind as k) -> As_fmt.pp ppf "part:kind:%a" pp_kind k
  | `Part (#part_usage as u) -> As_fmt.pp ppf "part:usage:%a" pp_usage u
  | `Part (`Name n) -> As_fmt.pp ppf "part:%s" n
  | `Pp -> As_fmt.pp_str ppf "pp"
  | `Src ext -> As_fmt.pp ppf "src:%a" As_path.pp_ext ext
  | `Tag t -> As_fmt.pp ppf "tag:%s" t
  | `Target t -> As_fmt.pp ppf "target:%a" pp_target t
end

let pp_elt = Elt.pp
let pp_kind = Elt.pp_kind
let pp_usage = Elt.pp_usage

(* Contexts *)

include Set.Make (Elt)

let v elts = List.fold_left (fun acc e -> add e acc) empty elts
let matches = subset

let pp ppf c =
  let pp_sep ppf () = As_fmt.pp ppf "," in
  As_fmt.pp_list ~pp_sep pp_elt ppf (elements c)
