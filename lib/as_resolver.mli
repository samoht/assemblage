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

(** Internal and external name resolvers.

    A project defines different kinds of names: local and external
    library names. A local library name needs to be associated with
    the name of the local directory where the build artifacts for that
    library are created and an external library name needs to be
    associated with the global directory where the object files for
    that library are installed. *)

type t
(** The type for internal and external name resolvers. *)

val create:
  ?ocamlc:string ->
  ?ocamlopt:string ->
  ?ocamldep:string ->
  ?ocamlmklib:string ->
  ?ocamldoc:string ->
  ?camlp4o:string ->
  ?js_of_ocaml:string ->
  ?build_dir:string ->
  ?lib_dir:string ->
  ?root_dir:string ->
  ?pkgs:(string list -> As_flags.t) ->
  unit -> t

val ocamlc: t -> string
val ocamlopt: t -> string
val ocamldep: t -> string
val ocamlmklib: t -> string
val ocamldoc: t -> string
val camlp4o: t -> string
val js_of_ocaml: t -> string
val build_dir: t -> string
val lib_dir: t -> string
val root_dir: t -> string
val pkgs: t -> string list -> As_flags.t
