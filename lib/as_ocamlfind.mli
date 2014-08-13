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

(** Manage OCamlfind invocations. *)

type mode = [`Direct|`Indirect|`Makefile]
(** The different invocation modes. [Direct] is the expanded
    command-line, without any mention of `ocamlfind'
    anymore. [Indirect] return the `ocamlfind' invocation to run to
    get the right command-line, and [Makefile] is the same as
    [Indirect], but quoted inside a {i $(shell ...)} string. *)

val query:
  mode:mode ->
  ?predicates:string list ->
  ?format:string ->
  ?uniq:bool ->
  ?recursive:bool ->
  string list -> string list
(** [ocamlfind_query ~direct ?predicates ?format packages] is the
    result of executing [ocamlfind query] with the given
    parameters. If [direct] is set, call the `ocamlfind' commands
    directly. Otherwise, return the ocamlfind to run to get the
    expected result. *)

val resolver: mode ->
  ?ocamlc:string ->
  ?ocamlopt:string ->
  ?ocamldep:string ->
  ?ocamlmklib:string ->
  ?ocamldoc:string ->
  ?preprocessor:string option ->
  ?ln:string ->
  ?mkdir:string ->
  ?js_of_ocaml:string ->
  ?build_dir:string ->
  ?lib_dir:string ->
  ?root_dir:string ->
  unit -> As_resolver.t
(** Resolve command-line arguments for ocamlfind packages. Same as
    [As_resolver.maker] but without the ~pkgs argument. *)

module META: sig

  (** Generate META files. *)

  type t

  val of_project: As_project.t -> t
  (** Create a META file. *)

  val write: ?dir:string -> t -> unit
  (** Write a META file. *)

end
