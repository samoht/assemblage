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

(* Arguments *)

type raw_args = string list
type arg = { cond : As_cond.t; context : As_context.t; raw_args : raw_args; }
type t = arg list

let create ?(cond = As_cond.true_) context raw_args =
  [{ cond; context; raw_args }]

let empty = []
let append a a' = a @ a'
let ( @@@ ) = append
let concat al = List.fold_left append empty al
let get ctx args =
  let add acc arg =
    if arg.context <> ctx then acc else
    (arg.cond, arg.raw_args) :: acc
  in
  List.rev (List.fold_left add [] args)

(* Built-in arguments *)

let debug =
  let f = ["-g"] in
  let create = create ~cond:As_cond.debug in
  concat
    [ create (`Compile `Byte) f;
      create (`Compile `Native) f;
      create (`Compile `Js) [ "-pretty"; "-debuginfo"; "-sourcemap"];
      create (`Compile `C) f;
      create (`Link `Byte) f;
      create (`Link `Native) f; ]

let annot =
  let f = ["-bin-annot"] in
  let create = create ~cond:As_cond.annot in
  concat
    [ create (`Compile `Byte) f;
      create (`Compile `Native) f; ]

let warn_error =
  let f = ["-warn-error A-44-4-48 -w A-44-4-48"] in
  let create = create ~cond:As_cond.warn_error in
  concat
    [ create (`Compile `Byte) f;
      create (`Compile `Native) f; ]

let linkall =
  let f = ["-linkall"] in
  concat
    [ create (`Archive `Shared) f;
      create (`Link `Byte) f;
      create (`Link `Native) f;
      create (`Link `Js) f; ]

let thread =
  let f = ["-thread"] in
  concat
    [ create (`Compile `Byte) f;
      create (`Compile `Native) f;
      create (`Link `Byte) f;
      create (`Link `Native) f; ]

let vmthread =
  let f = ["-vmthread"] in
  concat
    [ create (`Compile `Byte) f;
      create (`Link `Byte) f; ]

(* FIXME: which phase? *)
let cclib args =
  let f = List.map (str "-cclib %s") args in
  concat
    [ create (`Compile `C) args;
      create (`Link `Byte) f;
      create (`Link `Byte) f; ]

(* FIXME: which phase? *)
let ccopt args =
  let f = List.map (str "-ccopt %s") args in
  concat
    [ create (`Compile `Byte) f;
      create (`Compile `Native) f;
      create (`Compile `C) args;
      create (`Link `Byte) f;
      create (`Link `Native) f; ]

(* FIXME: which phase? *)
let stub s =
  concat
    [ create (`Link `Byte) [str "-cclib -l%s -dllib -l%s" s s];
      create (`Link `Native) [str "-cclib -l%s" s]; ]
