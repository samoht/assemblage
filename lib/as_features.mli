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

(** Project features.

    Typical OCaml project might have multiple features, which modifies
    the list of generated artifacts. An example is a project which
    depends on either [lwt] or [async], and which choose to compile
    the relevant libraries only if one of the other is installed on
    the system. Other typical feature might depend on the machine the
    project is running on, for instance the fact that the native
    toolchain is available or not. *)

type t
(** Formula of features. *)

type atom
(** An atomic feature. *)

type cnf = [ `Conflict | `And of [ `P of atom | `N of atom ] list ]
(** Conjonctive Normal Form. *)

module Set: Set.S with type elt = atom
(** Sets of atoms. *)

type set = Set.t
(** The type for sets of atoms. *)

val (++): Set.t -> Set.t -> Set.t
(** Union of atom sets. *)

val (@): cnf -> cnf -> cnf
(** Concatenation of CNF formulaes. *)

val atoms: t -> Set.t
(** [atoms f] is the set of atoms appearing in the formula [f]. *)

val cnf: t -> cnf
(** [normalize f] transform [f] in a conjonctive-normal form. *)

val eval: (atom * bool) list -> t -> bool
(** [eval tbl f] evaluates the formula [f] given the truth table
    [tbl]. If a feature [t] does not appear in [tbl] is is
    considered as associated to [false]. *)

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

val name: atom -> string
(** The atomic feature name. *)

val default: atom -> bool
(** The atomic feature default value. *)

val with_default: atom -> bool -> atom
(** Return the feature with an other default. *)

val create_atom: doc:string -> default:bool -> string -> atom
(** Create a single feature. *)

val create: doc:string -> default:bool -> string -> t
(** Same as [create_atom] but for single formula. *)

val doc: atom -> string
(** [doc f] is the feature documentation. *)

val parse: atom -> (atom * bool) Cmdliner.Term.t
(** A cmdliner term which parses an atomic feature. *)

val native: t
val native_atom: atom
(** Is native-code enabled ? *)

val native_dynlink: t
val native_dynlink_atom: atom
(** Is dynlink for native code enabled ? *)

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

val js: t
val js_atom: atom
(** Build the javascript objects. *)

val base: Set.t
(** The base features. *)
