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

(** EDSL to manipulate Makefiles. *)

module Rule: sig

  (** Rules. *)

  type t
  (** A rule value. *)

  val create:
    name:string ->
    targets:string list ->
    prereqs:string list ->
    ?order_only_prereqs:string list ->
    recipe:string list ->
    t
  (** Generate a Makefile rule:

      targets : inputs | order-only-inputs
         recipe
         ...
  *)

  val target: string
  (** The file name of the target of the rule. If the target is an
      archive member, then ‘$@’ is the name of the archive file. In a
      pattern rule that has multiple targets (see Introduction to Pattern
      Rules), ‘$@’ is the name of whichever target caused the rule's
      recipe to be run. *)

  val target_member: string
  (** The target member name, when the target is an archive
      member. See Archives. For example, if the target is foo.a(bar.o)
      then ‘$%’ is bar.o and ‘$@’ is foo.a. ‘$%’ is empty when the
      target is not an archive member. *)

  val prereq: string
  (** The name of the first prerequisite. If the target got its recipe
      from an implicit rule, this will be the first prerequisite added
      by the implicit rule (see Implicit Rules). *)

  val prereqs: string
 (** The names of all the prerequisites that are newer than the
      target, with spaces between them. For prerequisites which are
      archive members, only the named member is used (see
      Archives). *)

  val changed_prereqs: string
  (** The names of all the prerequisites, with spaces between
      them. For prerequisites which are archive members, only the
      named member is used (see Archives). A target has only one
      prerequisite on each other file it depends on, no matter how
      many times each file is listed as a prerequisite. So if you list
      a prerequisite more than once for a target, the value of $^
      contains just one copy of the name. This list does not contain
      any of the order-only prerequisites; for those see the ‘$|’
      variable, below.  *)

  val dedup_prereqs: string
  (** This is like ‘$^’, but prerequisites listed more than once are
      duplicated in the order they were listed in the makefile. This
      is primarily useful for use in linking commands where it is
      meaningful to repeat library file names in a particular
      order. *)

  val stem: string
  (** The stem with which an implicit rule matches (see How Patterns
      Match). If the target is dir/a.foo.b and the target pattern is
      a.%.b then the stem is dir/foo. The stem is useful for
      constructing names of related files. In a static pattern rule,
      the stem is part of the file name that matched the ‘%’ in the
      target pattern.

      In an explicit rule, there is no stem; so ‘$*’ cannot be
      determined in that way. Instead, if the target name ends with a
      recognized suffix (see Old-Fashioned Suffix Rules), ‘$*’ is set
      to the target name minus the suffix. For example, if the target
      name is ‘foo.c’, then ‘$*’ is set to ‘foo’, since ‘.c’ is a
      suffix. GNU make does this bizarre thing only for compatibility
      with other implementations of make. You should generally avoid
      using ‘$*’ except in implicit rules or static pattern rules.

      If the target name in an explicit rule does not end with a
      recognized suffix, ‘$*’ is set to the empty string for that
      rule. *)

end

module Variable: sig

  (** Variables. *)

  type t
  (** A variable value. *)

  val name: t -> string
  (** Variable name. *)

  val (=): string -> string ->  t
  (** VAR = x *)

  val (:=): string -> string ->  t
  (** VAR := x *)

  val (+=): string -> string -> t
  (** VAR += x *)

  val subst: t -> string -> input:string -> output:string -> t
    (** Create a new variable by sustituting the contents of an other one.

      NEWVAR = $(subst ${VAR}:input=output)

    *)

  val shell: string -> string -> t
  (** VAR = $(shell <command>) *)

  val files: string -> dir:string -> ext:string -> t
  (** VAR = $(wildcard <dir>/*.<ext>)  *)

end

type t
(** Makefile documents. *)

val create:
  ?header:string list ->
  ?phony:string list ->
  Variable.t list -> Rule.t list -> t
(** Create a Makefile. *)

val generate: ?file:string -> t -> unit
(** Generate a Makefile. *)

(** {2 OCaml specific rules} *)

module rec Depend: sig

  (** Library dependencies. *)

  type t
  (** Dependency values. *)

  val unit: string -> t
  (** A compilation unit in the same directory. *)

  val local: Library.t -> t
  (** A local library. *)

  val camlp4o : string -> t
  (** A syntax extension using camlp4o, managed by ocamlfind. *)

  val library: string -> t
  (** A library managed by ocamlfind. *)

  val ppflags: string -> t list -> Variable.t option
  (** Compute the flags to pass to the pre-processor and store it in a
      variable. *)

  val compflags: string -> t list -> Variable.t
  (** Compute the flags to pass the the bytecode and native compilers
      and store it in a variable. *)

  val prereqs: ?cmx:bool -> t list -> string list
  (** Compute the list of prerequisite. *)

end

and Unit: sig

  (** Compilation unit. *)

  type t

  val name: t -> string
  (** Return the compilation unit name. *)

  val dir: t -> string option
  (** Return the compilation unit directory. *)

  val with_dir: t -> string option -> t
  (** Change the compilation unit directory. *)

  val create: ?dir:string -> ?deps:Depend.t list -> string -> t
  (** Create a compilation unit. *)

  val variables: t -> Variable.t list
  (** Return the list of variables to build the given compilation
      unit. *)

  val rules: t -> Rule.t list
  (** Return the list of rules to build the given compilation unit. *)

  val generated: t -> string list
  (** Return the list of generated files. *)

end

and Library: sig

  (** Libraries. *)

  type t

  val name: t -> string
  (** Return the library name. *)

  val dir: t -> string option
  (** Return the library directory. *)

  val units: t -> Unit.t list
  (** Return the list of compilation units. *)

  val create: ?dir:string -> Unit.t list -> string -> t
  (** Create a library. *)

  val variables: t -> Variable.t list
  (** Return the list of variables to build the given library. *)

  val rules: t -> Rule.t list
  (** Return the list of rules to build the given library. *)

  val generated: t -> string list
  (** Return the list of generated files. *)

end

val libraries: Library.t list -> t
(** Generate a Makefile for the given libraries. *)
