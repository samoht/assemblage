(*
 * Copyright (c) 2014 Daniel C. Bünzli.
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

(** Execution of shell commands *)

val show: ('a, unit, string, unit) format4 -> 'a
(** [show msg] outputs the given message on stdout. *)

val fatal_error: int -> ('a, unit, string, 'b) format4 -> 'a
(** [fatal_error i msg] signals an error and stops the program with
    the exit code [i]. *)

val exec: ?verbose:bool -> ('a, unit, string, unit) format4 -> 'a
(** Execute a shell command. *)

val exec_output: ?verbose:bool -> ('a, unit, string, string list) format4 -> 'a
(** Execute a shell command and returns its output. *)

(** {2 Terminal Colors} *)

type text_style =
  [ `bold
  | `underline
  | `black
  | `red
  | `green
  | `yellow
  | `blue
  | `magenta
  | `cyan
  | `white ]

val color: text_style -> string -> string
(** Colorize a string using the given style. *)
