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

(** Command line arguments *)

type phase = [ `Pp | `Compile | `Link | `Run | `Test | `Other ]
type mode = [ `Byte | `Native | `Shared | `C | `Js ]
type args = string list
type t

val string_of_phase: phase -> string
val string_of_mode: mode -> string

val v : ?available:As_features.t -> phase -> mode -> args -> t
val ( @@@ ) : t -> t -> t

(* FIXME: this needs an Features evaluation context. *)
val get : phase -> mode -> t -> args

(* Built-in flags *)

val empty : t
val debug : t
val annot : t
val warn_error : t
val linkall : t
val thread : t
val cclib : string list -> t
val ccopt : string list -> t
val stub : string -> t
