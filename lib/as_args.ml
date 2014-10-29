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

(* Arguments with conditions *)

type cargs = { cond : bool As_conf.value; args : string list As_conf.value }
let cond ca = ca.cond
let args ca = ca.args

(* Argument bundles *)

module Cmap = Map.Make (As_ctx)

type t = cargs list Cmap.t

let v ?(cond = As_conf.true_) ctx args = Cmap.singleton ctx [{ cond; args }]
let vc ?cond ctx args = v ?cond ctx (As_conf.const args)
let empty = Cmap.empty
let is_empty = Cmap.is_empty
let append a0 a1 =
  let merge _ v v' = match v, v' with
  | Some cl, Some cl' -> Some (List.rev_append (List.rev cl) cl')
  | (Some _ as cl), None | None, (Some _ as cl) -> cl
  | None, None -> assert false
  in
  Cmap.merge merge a0 a1

let ( @@@ ) = append
let concat al = List.fold_left append empty al
let bindings = Cmap.bindings

let for_ctx a ctx =
  let add bctx cargs_list acc =
    if not (As_ctx.matches bctx ctx) then acc else
    List.rev_append cargs_list acc
  in
  List.rev (Cmap.fold add a [])

(* Built-in argument bundles *)

let linkall =
  let f = As_conf.const ["-linkall"] in
  concat
    [ v (As_ctx.v [`OCaml; `Archive; `Shared]) f;
      v (As_ctx.v [`OCaml; `Link; `Byte]) f;
      v (As_ctx.v [`OCaml; `Link; `Native]) f;
      v (As_ctx.v [`OCaml; `Link; `Js]) f; ]

let thread =
  let f = As_conf.const ["-thread"] in
  concat
    [ v (As_ctx.v [`OCaml; `Compile; `Byte]) f;
      v (As_ctx.v [`OCaml; `Compile; `Native]) f;
      v (As_ctx.v [`OCaml; `Link; `Byte]) f;
      v (As_ctx.v [`OCaml; `Link; `Native]) f; ]

let vmthread =
  let f = As_conf.const ["-vmthread"] in
  concat
    [ v (As_ctx.v [`OCaml; `Compile; `Byte]) f;
      v (As_ctx.v [`OCaml; `Link; `Byte]) f; ]

(* FIXME: which phase? *)
let cclib args =
  let f = As_conf.const (List.map (str "-cclib %s") args) in
  concat
    [ v (As_ctx.v [`OCaml; `Compile; `C]) (As_conf.const args);
      v (As_ctx.v [`OCaml; `Link; `Byte]) f;
      v (As_ctx.v [`OCaml; `Link; `Byte]) f; ]

(* FIXME: which phase? *)
let ccopt args =
  let f = As_conf.const (List.map (str "-ccopt %s") args) in
  concat
    [ v (As_ctx.v [`OCaml; `Compile; `Byte]) f;
      v (As_ctx.v [`OCaml; `Compile; `Native]) f;
      v (As_ctx.v [`C; `Compile]) (As_conf.const args);
      v (As_ctx.v [`OCaml; `Link; `Byte]) f;
      v (As_ctx.v [`OCaml; `Link; `Native]) f; ]

(* FIXME: which phase? *)
let stub s =
  concat
    [ v (As_ctx.v [`OCaml; `Link; `Byte])
        (As_conf.const [str "-cclib -l%s -dllib -l%s" s s]);
      v (As_ctx.v [`OCaml; `Link; `Native])
        (As_conf.const [str "-cclib -l%s" s]); ]

let debug =
  let f = As_conf.const ["-g"] in
  let v = v ~cond:As_conf.(value debug) in
  concat
    [ v (As_ctx.v [`OCaml; `Compile; `Byte]) f;
      v (As_ctx.v [`OCaml; `Compile; `Native]) f;
      v (As_ctx.v [`OCaml; `Compile; `Js])
           (As_conf.const [ "-pretty"; "-debuginfo"; "-sourcemap"]);
      v (As_ctx.v [`C; `Compile]) f;
      v (As_ctx.v [`OCaml; `Link; `Byte]) f;
      v (As_ctx.v [`OCaml; `Link; `Native]) f; ]

let annot =
  let f = As_conf.const ["-bin-annot"] in
  let v = v ~cond:As_conf.(value ocaml_annot) in
  concat
    [ v (As_ctx.v [`OCaml; `Compile; `Byte]) f;
      v (As_ctx.v [`OCaml; `Compile; `Native]) f; ]

let warn_error =
  let f = As_conf.const ["-warn-error A-44-4-48 -w A-44-4-48"] in
  let v = v ~cond:As_conf.(value warn_error) in
  concat
    [ v (As_ctx.v [`OCaml; `Compile; `Byte]) f;
      v (As_ctx.v [`OCaml; `Compile; `Native]) f; ]
