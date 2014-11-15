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

(** Binary executable part.

    See {!Assemblage.Bin} *)

(** {1 Metadata} *)

type kind = [ `OCaml | `OCaml_toplevel | `C ]
val pp_kind : Format.formatter -> kind -> unit
val kind : [< `Bin] As_part.t -> kind
val byte : [< `Bin] As_part.t -> bool
val native : [< `Bin] As_part.t -> bool
val js : [< `Bin] As_part.t -> bool
val ocaml : 'a As_part.t -> [> `Bin] As_part.t option
val ocaml_toplevel : 'a As_part.t -> [> `Bin] As_part.t option
val c : 'a As_part.t -> [> `Bin] As_part.t option

(** {1 Bin} *)

val v :
  ?usage:As_part.usage ->
  ?cond:bool As_conf.value ->
  ?args:As_args.t ->
  ?byte:bool -> ?native:bool -> ?js:bool -> string -> kind ->
  [< `Unit | `Lib | `Pkg ] As_part.t list ->
  [> `Bin] As_part.t

(*
   TODO
  val cmd : ?args:As_args.t -> ?kind:[`Byte | `Native] -> [< `Bin] As_part.t ->
    (string list -> string list) -> As_action.cmd
*)
