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

type phase =
  [ `Prepare
  | `Dep
  | `Pp of [`Byte|`Native]
  | `Compile of [`Intf|`Byte|`Native|`C|`Js]
  | `Archive of [`Byte|`Native|`Shared|`C]
  | `Link of [`Byte|`Native|`Js]
  | `Run of [`Byte|`Native]
  | `Test
  | `Doc
  | `Other of string ]

type args = string list

type flag =
  { available : As_features.t;
    phase : phase;
    args : args; }

let string_of_mode = function
| `Byte -> "byte"
| `Native -> "native"
| `Shared -> "shared"
| `C -> "c"
| `Js -> "js"
| `Camlp4o -> "camlp4o"
| `Intf -> "intf"

let string_of_phase: phase -> string = function
| `Prepare -> "prepare"
| `Dep -> "dep"
| `Pp m -> "pp-" ^ string_of_mode m
| `Compile m -> "compile-" ^ string_of_mode m
| `Archive m -> "archive-" ^ string_of_mode m
| `Link m -> "link-" ^ string_of_mode m
| `Run m -> "run-" ^ string_of_mode m
| `Test -> "test"
| `Doc -> "doc"
| `Other s -> s

module PhaseSet = struct
  include Set.Make(struct
      type t = phase
      let compare x y = String.compare (string_of_phase x) (string_of_phase y)
    end)
  let to_list = elements
  let of_list ss = List.fold_left (fun acc s -> add s acc) empty ss
end

type t = flag list

let v ?(available = As_features.true_) phase args =
  [ { available; phase; args } ]

let empty = []
let ( @@@ ) fs fs' = fs @ fs'

(* FIXME need an Features.t eveluation context as argument. *)
let get ((* ctx *)) phase flags =
  let rec loop acc = function
    | [] -> List.flatten (List.rev acc)
    | atom :: atoms ->
        if atom.phase = phase
        && true (* FIXME As_features.eval ctx t.features *)
        then loop (atom.args :: acc) atoms
        else loop acc atoms
  in
  loop [] flags

let get phase flags = get () phase flags

let debug =
  let f = ["-g"] in
  let v = v ~available:As_features.debug in
  v (`Compile `Byte) f @@@
  v (`Compile `Native) f @@@
  v (`Compile `Js) [ "-pretty"; "-debuginfo"; "-sourcemap"] @@@
  v (`Compile `C) f @@@
  v (`Link `Byte) f @@@
  v (`Link `Native) f

let annot =
  let f = ["-bin-annot"] in
  let v = v ~available:As_features.annot in
  v (`Compile `Byte) f @@@
  v (`Compile `Native) f

let linkall =
  let f = ["-linkall"] in
  v (`Archive `Shared) f @@@
  v (`Link `Byte) f @@@
  v (`Link `Native) f @@@
  v (`Link `Js) f

let warn_error =
  let f = ["-warn-error A-44-4-48 -w A-44-4-48"] in
  let v = v ~available:As_features.warn_error in
  v (`Compile `Byte) f @@@
  v (`Compile `Native) f

let thread =
  let f = ["-thread"] in
  v (`Compile `Byte) f @@@
  v (`Compile `Native) f @@@
  v (`Link `Byte) f @@@
  v (`Link `Native) f

(* FIXME: which phase? *)
let cclib args =
  let f = List.map (sprintf "-cclib %s") args in
  v (`Compile `C) args @@@
  v (`Link `Byte) f @@@
  v (`Link `Byte) f

(* FIXME: which phase? *)
let ccopt args =
  let f = List.map (sprintf "-ccopt %s") args in
  v (`Compile `Byte) f @@@
  v (`Compile `Native) f @@@
  v (`Compile `C) args @@@
  v (`Link `Byte) f @@@
  v (`Link `Native) f

(* FIXME: which phase? *)
let stub s =
  v (`Link `Byte) [sprintf "-cclib -l%s -dllib -l%s" s s] @@@
  v (`Link `Native) [sprintf "-cclib -l%s" s]

let doc_css s =
  v `Doc [sprintf "-css-style %s" s]

let doc_intro s =
  v `Doc [sprintf "-intro %s" s]
