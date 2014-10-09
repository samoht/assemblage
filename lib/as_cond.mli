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

(** Build conditions.

    For documentation see {!Assemblage.Cond}. *)

(** {1 Conditions}  *)

type t
val create : ?default:bool -> string -> doc:string ->  t
val true_ : t
val false_ : t
val neg : ?on:bool -> t -> t
val ( &&& ) : t -> t -> t
val ( ||| ) : t -> t -> t

(** {1 Built-in conditions} *)

val byte : t
val native : t
val native_dynlink : t
val native_tools : t
val js : t
val annot : t
val debug : t
val warn_error : t
val test : t
val doc : t

(** {1 Atomic conditions} *)

type atom
val name : atom -> string
val default : atom -> bool
val atom_doc : atom -> string
val eval : (atom * bool) list -> t -> bool

(** {1 Sets of atomic conditions} *)

module Set : Set.S with type elt = atom
val atoms : t -> Set.t
val builtin : Set.t

(** {1 Conditions in conjunctive normal form} *)

type clause = [ `P of atom | `N of atom ] list
type cnf = [ `True | `False | `And of clause list ]
val cnf : t -> cnf
