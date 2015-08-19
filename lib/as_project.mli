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

(** Project.

    For documentation see {!Assemblage.Private.Project}. *)

(** {1 Project} *)

open Bos

type t

val v : ?exists:bool As_conf.value -> ?args:As_args.t ->
  ?schemes:As_conf.scheme list -> string -> parts:'a As_part.t list -> t

val name : t -> string
val exists : t -> bool As_conf.value
val args : t -> As_args.t
val schemes : t -> As_conf.scheme list
val parts : t -> As_part.kind As_part.t list

(** {1 Configuration} *)

val deps : t -> As_conf.Key.Set.t
val conf : t -> As_conf.t
val with_conf : t -> As_conf.t -> t

val eval : t -> 'a As_conf.value -> 'a
val eval_key : t -> 'a As_conf.key -> 'a

(** {1 Configuration dependent value} *)

val version : t -> string
val products : ?kind:[`Source | `Input | `Output | `Any ] -> t -> Path.Set.t
val watermark_string : ?suffix:string -> t -> string
val pp_signature : Format.formatter -> t -> unit

(** {1 Assembling projects} *)

val assemble : t -> unit
val list : unit -> t list
