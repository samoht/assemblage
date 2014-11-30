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

type cargs = { exists : bool As_conf.value; args : string list As_conf.value }
let cargs_exists ca = ca.exists
let cargs_args ca = ca.args
let cargs_deps ca = As_conf.(Key.Set.union (deps ca.exists) (deps ca.args))
let cargs_pp conf ppf ca =
  As_fmt.pp ppf "@[<1>[exists:%b %a]@]" (As_conf.eval conf ca.exists)
    As_fmt.(pp_list ~pp_sep:pp_sp pp_str) (As_conf.eval conf ca.args)

(* Argument bundles *)

module Cmap = Map.Make (As_ctx)

type t = cargs list Cmap.t  (* maps ctxs to list of conditionalized args *)

let v ?(exists = As_conf.true_) ctx args = Cmap.singleton ctx [{exists; args}]
let vc ?exists ctx args = v ?exists ctx (As_conf.const args)
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

let deps a =
  let add_carg acc cargs = As_conf.Key.Set.union acc (cargs_deps cargs) in
  let add_ctx _ cargss acc = List.fold_left add_carg acc cargss in
  Cmap.fold add_ctx a As_conf.Key.Set.empty

let cargs_for_ctx ctx a =
  let add bctx cargs_list acc =
    if not (As_ctx.matches bctx ctx) then acc else
    List.rev_append cargs_list acc
  in
  List.rev (Cmap.fold add a [])

let for_ctx conf ctx a =
  let cargs = cargs_for_ctx ctx a in
  let add acc cargs =
    if not (As_conf.eval conf cargs.exists) then acc else
    let args = As_conf.eval conf cargs.args in
    List.rev_append args acc
  in
  List.rev (List.fold_left add [] cargs)

let pp conf ppf args =
  let pp_binding ppf (ctx, cargs) =
    As_fmt.pp ppf "@[<2>%a %a@]"
      As_ctx.pp ctx As_fmt.(pp_list ~pp_sep:pp_sp (cargs_pp conf)) cargs
  in
  As_fmt.pp ppf "@[<v>%a@]" As_fmt.(pp_list pp_binding) (Cmap.bindings args)

(* Built-in argument bundles *)

let linkall =
  let f = As_conf.const ["-linkall"] in
  concat
    [ v (As_ctx.v [`OCaml; `Archive `Shared]) f;
      v (As_ctx.v [`OCaml; `Link; `Target `Byte]) f;
      v (As_ctx.v [`OCaml; `Link; `Target `Native]) f;
      v (As_ctx.v [`OCaml; `Link; `Target `Js]) f; ]

let thread =
  let f = As_conf.const ["-thread"] in
  concat
    [ v (As_ctx.v [`OCaml; `Compile; `Target `Byte]) f;
      v (As_ctx.v [`OCaml; `Compile; `Target `Native]) f;
      v (As_ctx.v [`OCaml; `Link; `Target `Byte]) f;
      v (As_ctx.v [`OCaml; `Link; `Target `Native]) f; ]

let vmthread =
  let f = As_conf.const ["-vmthread"] in
  concat
    [ v (As_ctx.v [`OCaml; `Compile; `Target `Byte]) f;
      v (As_ctx.v [`OCaml; `Link; `Target `Byte]) f; ]

(* FIXME: which phase? *)
let cclib args =
  let f = As_conf.const (List.map (str "-cclib %s") args) in
  concat
    [ v (As_ctx.v [`OCaml; `Compile; `C]) (As_conf.const args);
      v (As_ctx.v [`OCaml; `Link; `Target `Byte]) f;
      v (As_ctx.v [`OCaml; `Link; `Target `Byte]) f; ]

(* FIXME: which phase? *)
let ccopt args =
  let f = As_conf.const (List.map (str "-ccopt %s") args) in
  concat
    [ v (As_ctx.v [`OCaml; `Compile; `Target `Byte]) f;
      v (As_ctx.v [`OCaml; `Compile; `Target `Native]) f;
      v (As_ctx.v [`C; `Compile]) (As_conf.const args);
      v (As_ctx.v [`OCaml; `Link; `Target `Byte]) f;
      v (As_ctx.v [`OCaml; `Link; `Target `Native]) f; ]

(* FIXME: which phase? *)
let stub s =
  concat
    [ v (As_ctx.v [`OCaml; `Link; `Target `Byte])
        (As_conf.const ["-cclib"; (str "-l%s" s); "-dllib "; (str "-l%s" s)]);
      v (As_ctx.v [`OCaml; `Link; `Target `Native])
        (As_conf.const [str "-cclib -l%s" s]); ]
