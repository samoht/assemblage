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
type cmd = [ `Cmd of string As_conf.key | `Cmd_static of string ]
type part_usage = [ `Build | `Dev | `Doc | `Other of string | `Outcome | `Test ]
type part_kind = [ `Base | `Bin | `Dir | `Doc | `Lib | `Pkg | `Run | `Unit ]
type part = [ `Part of [ part_usage | part_kind | `Name of string ]]

module Elt = struct
  type t = [ tag | language | build_phase | source | target | cmd | part ]

  let compare (e : t) (e' : t) = match e, e' with
  | `Cmd k0, `Cmd k1 -> As_conf.Key.(compare (V k0) (V k1))
  | `Cmd _, _ | _, `Cmd _ -> -1
  | c, c' -> compare c c'

  let pp_target ppf = function
  | `Byte -> Fmt.string ppf "byte"
  | `Js -> Fmt.string ppf "js"
  | `Native -> Fmt.string ppf "native"
  | `Other o -> Fmt.string ppf o
  | `Src -> Fmt.string ppf "src"

  let pp_kind ppf = function
  | `Base -> Fmt.string ppf "base"
  | `Bin -> Fmt.string ppf "bin"
  | `Dir -> Fmt.string ppf "dir"
  | `Doc -> Fmt.string ppf "doc"
  | `Lib -> Fmt.string ppf "lib"
  | `Pkg -> Fmt.string ppf "pkg"
  | `Run -> Fmt.string ppf "run"
  | `Unit -> Fmt.string ppf "unit"

  let pp_usage ppf = function
  | `Build -> Fmt.string ppf "build"
  | `Dev -> Fmt.string ppf "dev"
  | `Doc -> Fmt.string ppf "doc"
  | `Other s -> Fmt.string ppf s
  | `Outcome -> Fmt.string ppf "outcome"
  | `Test -> Fmt.string ppf "test"

  let pp ppf = function
  | `Archive `Shared -> Fmt.string ppf "archive:shared"
  | `Archive `Static -> Fmt.string ppf "archive:static"
  | `C -> Fmt.string ppf "c"
  | `Cmd k -> Fmt.pf ppf "cmd:%s" (As_conf.Key.name k)
  | `Cmd_static n -> Fmt.pf ppf "cmd-static:%s" n
  | `Compile -> Fmt.string ppf "compile"
  | `Dep -> Fmt.string ppf "dep"
  | `Doc -> Fmt.string ppf "doc"
  | `Gen -> Fmt.string ppf "gen"
  | `Js -> Fmt.string ppf "js"
  | `Lang l -> Fmt.pf ppf "lang:%s" l
  | `Link -> Fmt.string ppf "link"
  | `OCaml -> Fmt.string ppf "ocaml"
  | `Part (#part_kind as k) -> Fmt.pf ppf "part-kind:%a" pp_kind k
  | `Part (#part_usage as u) -> Fmt.pf ppf "part-usage:%a" pp_usage u
  | `Part (`Name n) -> Fmt.pf ppf "part-name:%s" n
  | `Pp -> Fmt.string ppf "pp"
  | `Src ext -> Fmt.pf ppf "src:%a" As_path.pp_ext ext
  | `Tag t -> Fmt.pf ppf "tag:%s" t
  | `Target t -> Fmt.pf ppf "target:%a" pp_target t
end

let pp_elt = Elt.pp
let pp_kind = Elt.pp_kind
let pp_usage = Elt.pp_usage

(* Contexts *)

include Set.Make (Elt)

let v elts = List.fold_left (fun acc e -> add e acc) empty elts
let matches = subset

let pp ppf c =
  let sep ppf () = Fmt.pf ppf ",@ " in
  Fmt.list ~sep pp_elt ppf (elements c)
