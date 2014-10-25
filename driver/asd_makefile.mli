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

(** Makefiles. *)

(** {1 Makefile specification} *)

type var
(** The type for makefile variable declarations. *)

type rule
(** The type for makefile rules. *)

type statement =
  [ `Var of var
  | `Rule of rule ]
(** The type for makefile statements. *)

type t = [ statement | `Comment of string | `Blank ] list
(** The type for makefiles. *)

val ( === ) : string -> string list -> [> `Var of var ]
val ( =:= ) : string -> string list -> [> `Var of var ]
val ( =::= ) : string -> string list -> [> `Var of var ]
val ( =+= ) : string -> string list -> [> `Var of var ]
val ( =?= ) : string -> string list -> [> `Var of var ]

val rule : ?ext:bool -> ?order_only_prereqs:string list ->
  targets:string list -> prereqs:string list -> recipe:string list list ->
  unit -> [> `Rule of rule ]

module Op : sig
  val ( === ) : string -> string list -> [> `Var of var ]
  val ( =:= ) : string -> string list -> [>`Var of var ]
  val ( =::= ) : string -> string list -> [> `Var of var ]
  val ( =+= ) : string -> string list -> [> `Var of var ]
  val ( =?= ) : string -> string list -> [> `Var of var ]
end

(** {1 Output} *)

val to_string : t -> string
val write_file : string -> t -> unit
