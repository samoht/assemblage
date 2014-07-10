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

(** Command line arguments *)

type t
(** Full command-line arguments. Values of this type carry the
    command-line arguments to use in the different phases and
    modes. *)

type s = string list
(** An single command-line argument, for a given phase and mode. *)

val create:
  ?comp_byte:s -> ?comp_native:s ->
  ?pp_byte:s -> ?pp_native:s ->
  ?link_byte:s -> ?link_native:s -> ?link_shared:s ->
  ?c:s ->
  unit -> t
(** Create a full command-line argument using the the given single
    command-line arguments. *)

val (@@@): t -> t -> t
(** Append command-line flags. *)

val comp_byte: t -> s
(** The command-line arguments for compiling compilation units in
    bytecode mode. *)

val comp_native: t -> s
(** The command-line arguments for compiling compilation units in
    native mode. *)

val pp_byte: t -> s
(** The command-line arguments for pre-processing files in bytecode
    mode. *)

val pp_native: t -> s
(** The command-line arguments for pre-processing files in native
    mode. *)

val link_byte: t -> s
(** The command-line arguments to link compilation units in bytecode
    mode. *)

val link_native: t -> s
(** The command-line arguments to link compilation units in native
    mode. *)

val link_shared: t -> s
(** The command-line arguments to link shared libraries. *)

val c: t -> s
(** The command-line arguments to pass to the C compiler. *)

(** {2 Built-in flags} *)

val empty: t
(** Empty flags. *)

val debug: t
(** Add [-g]. *)

val annot: t
(** Add [-bin-annot]. *)

val warn_error: t
(** Add [-warn-error]. *)

val linkall: t
(** Add [-linkall]. *)

val thread: t
(** Add [-thread]. *)

val cclib: string list -> t
(** Add [-cclib] flags to the linker. *)

val ccopt: string list -> t
(** Add [-ccopt] flags to the compiler and the linker. *)

val stub: string -> t
(** [stub s] Add {i -cclib -l[s] -dllib -l[s]} to the bytecode linking
    options and {i -cclib -l[s]} to the native linking options. *)
