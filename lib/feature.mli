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
(** Feature values. *)

type formula
(** Formulaes of features. *)

type cnf = [ `Conflict | `And of [ `P of t | `N of t ] list ]
(** Conjonctive Normal Form. *)

module Set: Set.S with type elt = t
(** Set of features. *)

val (++): Set.t -> Set.t -> Set.t
(** Union of feature sets. *)

val (@): cnf -> cnf -> cnf
(** Concatenation of CNF formulaes. *)

val atoms: formula -> Set.t
(** [atoms f] is the list of atoms appearing in the formula [f]. *)

val cnf: formula -> cnf
(** [normalize f] transform [f] in a conjonctive-normal form. *)

val eval: (t * bool) list -> formula -> bool
(** [eval tbl f] evaluates the formula [f] given the truth table
    [tbl]. If a feature [t] does not appear in [tbl] is is
    considered as associated to [false]. *)

val true_: formula
(** The formula which is always [true]. *)

val false_: formula
(** The formula which is always [false]. *)

val atom: t -> formula
(** [atom t] is the formula containing the singleton feature [t]. *)

val not: formula -> formula
(** [not f] negates the formula [f]. *)

val (&&): formula -> formula -> formula
(** [f1 && f2] is the conjonction of [f1] and [f2]. *)

val (||): formula -> formula -> formula
(** [f1 || f2] is the disjonction of [f1] and [f2]. *)

val name: t -> string
(** The feature name. *)

val default: t -> bool
(** Default value. *)

val with_default: t -> bool -> t
(** Return the feature with an other default. *)

val create: doc:string -> default:bool -> string -> t
(** Create a feature. *)

val parse: t -> (t * bool) Cmdliner.Term.t
(** A cmldiner term which parses a feature. *)

val native: formula
val native_t: t
(** Is native-code enabled ? *)

val native_dynlink: formula
val native_dynlink_t: t
(** Is dynlink for native code enabled ? *)

val annot: formula
val annot_t: t
(** Generate annot files ? *)

val debug: formula
val debug_t: t
(** Generate debug symbols ? *)

val warn_error: formula
val warn_error_t: t
(** Consider warning as error. *)

val test: formula
val test_t: t
(** Compile and run tests. *)

val doc: formula
val doc_t: t
(** Build the documentation. *)

val js: formula
val js_t: t
(** Build the javascript objects. *)

val base: Set.t
(** The base features. *)
