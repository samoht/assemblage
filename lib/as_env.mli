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

(** Build environment.

    For documentation see {!Assemblage.Env}. *)

(** {1 Build environment} *)

type t

val create :
  ?ocamlc:string ->
  ?ocamlopt:string ->
  ?ocamldep:string ->
  ?ocamlmklib:string ->
  ?ocamldoc:string ->
  ?ocaml_pp:string option ->
  ?ln:string ->
  ?mkdir:string ->
  ?js_of_ocaml:string ->
  ?build_dir:As_path.rel ->
  ?root_dir:As_path.t ->
  ?ocamlfind_pkgs:(string list -> As_args.t) ->
  ?pkg_config:(string list -> As_args.t) ->
  unit -> t

(** {1 Directories} *)

val build_dir : t -> As_path.rel
val root_dir : t -> As_path.t
val push_build_dir : t -> As_path.rel -> t

(** {1 Program binaries} *)

val ocamlc : t -> string
val ocamlopt : t -> string
val ocamldep : t -> string
val ocamlmklib : t -> string
val ocamldoc : t -> string
val ocaml_pp : t -> string option
val js_of_ocaml : t -> string
val mkdir : t -> string
val ln : t -> string

(** {1 Package queries} *)

val ocamlfind_pkgs : t -> string list -> As_args.t
val pkg_config : t -> string list -> As_args.t
