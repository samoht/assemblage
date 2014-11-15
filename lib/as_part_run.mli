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

(** Run part.

    See {!Assemblage.Run}. *)

(** {1 Metadata} *)

val dir : [< `Run] As_part.t -> As_path.t As_conf.value

(** {1 Run} *)

val v : ?usage:As_part.usage -> ?exists:bool As_conf.value ->
  ?args:As_args.t -> ?dir:As_path.t As_conf.value ->
  string -> As_action.t -> [> `Run] As_part.t

val of_bin : ?usage:As_part.usage -> ?exists:bool As_conf.value ->
  ?args:As_args.t -> ?dir:As_path.t As_conf.value ->
  [< `Bin] As_part.t -> (string list) As_conf.value -> [> `Run] As_part.t
