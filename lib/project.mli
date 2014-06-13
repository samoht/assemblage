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

(** EDSL to describe OCaml projects. *)

module Flag: sig

  (** Conditional flags. *)

  type t
  (** Flag values. *)

  type name = string
  (** Flag names. *)

  val name: t -> name
  (** The flag name. *)

  val value: t -> bool
  (** The flag value. *)

  val create: name -> bool -> t
  (** Create a flag. *)

end

module Conf: sig

  (** Global project configuration. *)
  type t

  val create:

    ?native:bool ->
    (** Enable native compilation. Default is [true]. *)

    ?native_dynlink: bool ->
    (** Enable compilation of native dynlink units. Default is
        [true]. *)

    ?flags:Flag.t list ->
    (** Environment flags. *)

    ?comp: string list ->
    (** Additional compilation flags passed to both [ocamlc] and
        [ocamlopt]. *)

    ?bytcomp: string list ->
    (** Additional compilation flags passed to [ocamlc] only. *)

    ?natcomp: string list ->
    (** Additional compilation flags passed to [ocamlopt] only. *)

    ?link: string list ->
    (** Additional link flags passed to both [ocamlc] and [ocamlopt]. *)

    ?bytlink: string list ->
    (** Addtional link flags passed to [ocamlc]. *)

    ?natlink: string list ->
    (** Addtional link flags passed to [ocamlopt]. *)

    ?p4: string list ->
    (** Addition flags passed to the preprocessor. *)

    ?destdir: string ->
    (** Location of the generated files. [None] means the files stays in
        the same directory. *)

    unit -> t

  val default: t
  (** Default project configuration. *)

  val native: t -> bool
  (** Check if the native compilers are enable in the given
      configuration. *)

  val native_dynlink: t -> bool
  (** Check if the native dynlinker is enable in the given
      configuration. *)

  val comp: t -> string list
  (** Return the global comand-line flags for compilation. *)

  val link: t -> string list
  (** Return the global command-line flags for linking. *)

  val p4: t -> string list
  (** Return the global command-line option for the camlp4o preprocessor. *)

  val destdir: t -> string
  (** Return the directory where build artififacts are generated. *)

  val enable: t -> Flag.name list -> bool
  (** Check if the given set of flags are all enabled. *)

end

module rec Dep: sig

  (** Library dependencies. *)

  type t
  (** Dependency values. *)

  type custom = {
    inputs : string list;
    outputs: string list;
    recipe : string list
  }
  (** Custom rules. *)

  val unit: Unit.t -> t
  (** A compilation unit in the same directory. *)

  val lib: Lib.t -> t
  (** A local library. *)

  val findp4o: string -> t
  (** A syntax extension using camlp4o, managed by ocamlfind. *)

  val findlib: string -> t
  (** A library managed by ocamlfind. *)

  val custom: custom -> t
  (** Custom generation rule. *)

  val units: t list -> Unit.t list
  (** Return the list of compilation unit dependencies. *)

  val libs: t list -> Lib.t list
  (** Return the list of local library dependencies. *)

  val findp4os: t list -> string list
  (** Return the list of ocamlfind syntax dependencies. *)

  val findlibs: t list -> string list
  (** Return the list of ocamlfind library dependencies. *)

  val customs: t list -> custom list
  (** Return the list of custom rules. *)

end

and Unit: sig

  (** Compilation unit. *)

  type t

  val name: t -> string
  (** Return the compilation unit name. *)

  val dir: t -> string option
  (** Return the compilation unit directory. *)

  val deps: t -> Dep.t list
  (** Return the compilation unit dependencies. *)

  val flags: t -> Flag.name list
  (** Return the compilation unit conditional flags. *)

  val with_dir: t -> string option -> t
  (** Change the compilation unit directory. *)

  val create: ?dir:string -> ?deps:Dep.t list -> ?flags:Flag.name list -> string -> t
  (** Create a compilation unit. *)

  val generated_files: t -> Conf.t -> string list
  (** Return the list of generated files for the given project
      configuration. *)

  val compflags: t -> Conf.t -> string list
  (** Return the computed compilation flags. *)

  val p4flags: t -> Conf.t -> string list
  (** Return the computed pre-processing flags. *)

end

and Lib: sig

  (** Libraries. *)

  type t

  val name: t -> string
  (** Return the library name. *)

  val units: t -> Unit.t list
  (** Return the list of compilation units. *)

  val create: ?flags:Flag.name list -> Unit.t list -> string -> t
  (** Create a library. *)

  val generated_files: t -> Conf.t -> string list
  (** Return the list of generated files for the given project
      configuration. *)

end

type t
(** Project values. *)

val libs: t -> Lib.t list
(** Return the list of libraries defined by the project. *)

val conf: t -> Conf.t
(** Return the project configuration. *)

val create: libs:Lib.t list -> Conf.t -> t
(** Generate a project description for the given collection of
    libraries, with the given project settings. *)
