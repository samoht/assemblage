(*
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

(** Built-in actions for OCaml.

    See {!Assemblage.Action.OCaml}. *)

(** {1 Types} *)

type includes = As_path.rel list As_conf.value
type name = As_path.rel As_conf.value

(** {1 Preprocess} *)

val compile_src_ast : [`Ml | `Mli ] -> src:As_action.product -> unit ->
  As_action.t

(** {1 Compiling} *)

val compile_mli : incs:includes -> src:As_action.product -> unit -> As_action.t
val compile_ml_byte : has_mli:bool As_conf.value -> incs:includes ->
  src:As_action.product -> unit -> As_action.t

val compile_ml_native : has_mli:bool As_conf.value -> incs:includes ->
  src:As_action.product -> unit -> As_action.t

val compile_c : src:As_action.product -> unit -> As_action.t

(** {1 Archiving} *)

val archive_byte : cmos:As_path.rel list As_conf.value -> name:name ->
  unit -> As_action.t

val archive_native : cmx_s:As_path.rel list As_conf.value ->
  name:name -> unit -> As_action.t

val archive_shared : cmx_s:As_action.products -> name:name -> unit ->
  As_action.t

val archive_c : objs:As_action.products -> name:name -> unit ->
  As_action.t

(** {1 Linking} *)
