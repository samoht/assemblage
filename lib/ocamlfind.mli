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

val query_str:
  ?predicates:string list ->
  ?format:string ->
  ?uniq:bool ->
  ?recursive:bool ->
  string list -> string
(** [ocamlfind_query ?predicates ?format packages] is corresponding
    `ocamlfind' command-line invocation. *)

val query:
  ?predicates:string list ->
  ?format:string ->
  ?uniq:bool ->
  ?recursive:bool ->
  string list -> string list
(** [ocamlfind_query ?predicates ?format packages] is the result of
    executing [ocamlfind query] with the given parameters. *)

val resolver: (string -> string) -> Project.Resolver.t
(** Resolve command-line arguments for ocamlfind packages. *)

module META: sig

  (** Generate META files. *)

  type t

  val of_project: Project.t -> t
  (** Create a META file. *)

  val write: ?dir:string -> t -> unit
  (** Write a META file. *)

end
