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

(** Execution of shell commands. *)

val show : ('a, unit, string, unit) format4 -> 'a
(** [show msg] outputs the message [msg] on stdout. *)

val warn : ('a, unit, string, unit) format4 -> 'a
(** [warn msg] outputs the warning message [msg] on stderr. *)

val error : ('a, unit, string, unit) format4 -> 'a
(** [error msg] outputs the error message [msg] on stderr. *)

val fatal_error : int -> ('a, unit, string, 'b) format4 -> 'a
(** [fatal_error i msg] output the error message [msg] on stderr
    and stops the program with exit code [i]. *)

val has_cmd : string -> bool
(** [has_cmd cmd] is [true] iff the shell has the command [cmd]. *)

val exec: ?verbose:bool -> ('a, unit, string, unit) format4 -> 'a
(** Execute a shell command. *)

val exec_output: ?verbose:bool -> ('a, unit, string, string list) format4 -> 'a
(** Execute a shell command and returns its output. *)

val try_exec: ('a, unit, string, bool) format4 -> 'a
(** Try to run a given command. *)

val in_dir: string -> (unit -> 'a) -> 'a
(** Execute a command in a given directory. *)

(** {2 Terminal Colors} *)

type text_style =
  [ `Bold
  | `Underline
  | `Black
  | `Red
  | `Green
  | `Yellow
  | `Blue
  | `Magenta
  | `Cyan
  | `White ]

val color: text_style -> string -> string
(** Colorize a string using the given style. *)
