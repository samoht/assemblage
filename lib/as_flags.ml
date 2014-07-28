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

type phase = [ `Pp | `Compile | `Link | `Run | `Test | `Other ]
type mode = [ `Byte | `Native | `Shared | `C | `Js ]
type args = string list
type flag =
  { available : As_features.t;
    phase : phase;
    mode : mode;
    args : args; }

let string_of_phase = function
| `Pp -> "pp"
| `Compile -> "compile"
| `Link -> "link"
| `Run -> "run"
| `Test -> "test"
| `Other -> "other"

let string_of_mode = function
| `Byte -> "byte"
| `Native -> "native"
| `Shared -> "shared"
| `C -> "c"
| `Js -> "js"

type t = flag list

let v ?(available = As_features.true_) phase mode args =
  [ { available; phase; mode; args } ]

let empty = []
let ( @@@ ) fs fs' = fs @ fs'

(* FIXME need an Features.t eveluation context as argument. *)
let get ((* ctx *)) phase mode flags =
  let rec loop acc = function
    | [] -> List.flatten (List.rev acc)
    | atom :: atoms ->
        if atom.phase = phase
        && atom.mode = mode
        && true (* FIXME As_features.eval ctx t.features *)
        then loop (atom.args :: acc) atoms
        else loop acc atoms
  in
  loop [] flags

let get phase mode flags = get () phase mode flags

let debug =
  let f = ["-g"] in
  let v = v ~available:As_features.debug in
  v `Compile `Byte f @@@
  v `Compile `Native f @@@
  v `Compile `Js [ "-pretty"; "-debuginfo"; "-sourcemap"] @@@
  v `Compile `C f @@@
  v `Link `Byte f @@@
  v `Link `Native f @@@
  v `Link `C f

let annot =
  let f = ["-bin-annot"] in
  let v = v ~available:As_features.annot in
  v `Compile `Byte f @@@
  v `Compile `Native f


let linkall =
  let f = ["-linkall"] in
  v `Link `Byte f @@@
  v `Link `Native f @@@
  v `Link `Js f

let warn_error =
  let f = ["-warn-error A-44-4 -w A-44-4"] in
  let v = v ~available:As_features.warn_error in
  v `Compile `Byte f @@@
  v `Compile `Native f

let thread =
  let f = ["-thread"] in
  v `Compile `Byte f @@@
  v `Compile `Native f @@@
  v `Link `Byte f @@@
  v `Link `Native f @@@
  v `Link `Shared f

let cclib args =
  let f = List.map (sprintf "-cclib %s") args in
  v `Link `Byte f @@@
  v `Link `Byte f @@@
  v `Link `Shared f @@@
  v `Compile `C args

let ccopt args =
  let f = List.map (sprintf "-ccopt %s") args in
  v `Compile `Byte f @@@
  v `Compile `Native f @@@
  v `Link `Byte f @@@
  v `Link `Native f @@@
  v `Link `Shared f @@@
  v `Link `C args

let stub s =
  v `Link `Byte [sprintf "-cclib -l%s -dllib -l%s" s s] @@@
  v `Link `Native [sprintf "-cclib -l%s" s]
