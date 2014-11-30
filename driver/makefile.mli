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

(** Makefiles.

    Functions to specify and generate
    {{:http://www.gnu.org/software/make/manual/make.html}Makefiles}. *)

(** {1 Makefile variables} *)

type var
(** The type for makefile variable declarations. *)

(** {b Note.} In the following functions. The right hand-side of
    variables is a list of strings. On output these strings are
    separated by one space and can be used as a break point if the
    line becomes too long. *)

val ( === ) : string -> string list -> [> `Var of var ]
(** [v === def] is [v = def]. *)

val ( =:= ) : string -> string list -> [> `Var of var ]
(** [v =:= def] is [v := def]. *)

val ( =::= ) : string -> string list -> [> `Var of var ]
(** [v =::= def] is [v ::= def]. *)

val ( =+= ) : string -> string list -> [> `Var of var ]
(** [v =+= def] is [v += def]. *)

val ( =?= ) : string -> string list -> [> `Var of var ]
(** [v =+= def] is [v += def]. *)

(** Infix operators. *)
module Infix : sig
  val ( === ) : string -> string list -> [> `Var of var ]
  (** [( === )] is {!( === )}. *)
  val ( =:= ) : string -> string list -> [>`Var of var ]
  (** [( =:= )] is {!( =:= )}. *)
  val ( =::= ) : string -> string list -> [> `Var of var ]
  (** [( =::= )] is {!( =::= )}. *)
  val ( =+= ) : string -> string list -> [> `Var of var ]
  (** [( =+= )] is {!( =+= )}. *)
  val ( =?= ) : string -> string list -> [> `Var of var ]
  (** [( =?= )] is {!( =?= )}. *)
end

(** {1:rules Rules} *)

type rule
(** The type for makefile rules. *)

val rule : ?ext:bool -> ?order_only_prereqs:string list ->
  targets:string list -> prereqs:string list -> recipe:string list list ->
  unit -> [> `Rule of rule ]
(** [rule ext order_only_prereqs targets prerequs recipe ()] is a makefile
    rule. [ext] indicates whether the rule should be extensible (double colon
    rule, defaults to [false]).

    [recipe] is a list of commands. Commands are list of strings.
    On output the later strings are separated by one space and can be used
    as a break point if the line becomes too long. *)

(** {1:makefile Makefiles} *)

type statement =
  [ `Var of var
  | `Rule of rule ]
(** The type for makefile statements. *)

type t = [ statement | `Comment of string | `Blank ] list
(** The type for makefiles. *)

val to_string : t -> string
(** [to_string m] is [m] as a string. *)
