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

(** Project features. *)

(** {1 Atomic features} *)

type atom
(** An atomic feature. *)

val create_atom: ?default:bool -> string -> doc:string ->  atom
(** Create a single feature. *)

val name: atom -> string
(** The atomic feature name. *)

val default: atom -> bool
(** The atomic feature default value. *)

val doc: atom -> string
(** [doc f] is the feature documentation. *)

val with_default: atom -> bool -> atom
(** Return the feature with an other default. *)

val parse: atom -> (atom * bool) Cmdliner.Term.t
(** A cmdliner term which parses an atomic feature. *)

(** {1 Atomic feature sets} *)

module Set: Set.S with type elt = atom
(** Sets of atoms. *)

type set = Set.t
(** The type for sets of atoms. *)

val (++): Set.t -> Set.t -> Set.t
(** Union of atom sets. *)

(** {1 Features} *)

type t
(** Formula of features. *)

val create: ?default:bool -> string -> doc:string ->  t
(** Same as [create_atom] but for single formula. *)

val true_: t
(** The formula which is always [true]. *)

val false_: t
(** The formula which is always [false]. *)

val atom: atom -> t
(** [atom t] is the formula containing the singleton feature [t]. *)

val not_: t -> t
(** [not f] negates the formula [f]. *)

val (&&&): t -> t -> t
(** [f1 && f2] is the conjonction of [f1] and [f2]. *)

val (|||): t -> t -> t
(** [f1 || f2] is the disjonction of [f1] and [f2]. *)

val atoms: t -> Set.t
(** [atoms f] is the set of atoms appearing in the formula [f]. *)

val eval: (atom * bool) list -> t -> bool
(** [eval tbl f] evaluates the formula [f] given the truth table
    [tbl]. If a feature [t] does not appear in [tbl] is is
    considered as associated to [false]. *)

type cnf = [ `Conflict | `And of [ `P of atom | `N of atom ] list ]
(** Conjonctive Normal Form. *)

val (@): cnf -> cnf -> cnf
(** Concatenation of CNF formulaes. *)

val cnf: t -> cnf
(** [normalize f] transform [f] in a conjonctive-normal form. *)

(** {1 Built-in features} *)

val byte: t
val byte_atom: atom

val native: t
val native_atom: atom
(** Is native-code enabled ? *)

val native_dynlink: t
val native_dynlink_atom: atom
(** Is dynlink for native code enabled ? *)

val js: t
val js_atom: atom
(** Build the javascript objects. *)

val annot: t
val annot_atom: atom
(** Generate annot files ? *)

val debug: t
val debug_atom: atom
(** Generate debug symbols ? *)

val warn_error: t
val warn_error_atom: atom
(** Consider warning as error. *)

val test: t
val test_atom: atom
(** Compile and run tests. *)

val public_doc: t
val public_doc_atom: atom
(** Build the documentation. *)

val full_doc: t
val full_doc_atom: atom
(** Generate the full documentation (and not just the public doc). *)

val builtin: Set.t
(** The set of built-in atomic features. *)
