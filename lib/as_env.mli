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

(** Command runtime environment. *)

(** {1 Setup environements} *)

type setup =
  { auto_load : bool;          (** [true] to add assemblage lib to includes. *)
    includes : string list;        (** includes to add to toploop execution. *)
    assemble_file : string;                             (** file to execute. *)
    exec_status :                   (** execution status of [assemble_file]. *)
      [ `Error | `Ok | `No_cmd | `No_file ]; }
(** The type for setup environments.

    This is only used by the assemblage tool. It determines the environment
    in which assemble.ml will be executed and the result of that execution. *)

val parse_setup : unit -> setup
(** [parse_setup ()] must be called by the assemblage tool.

    This function is not called by assemble.ml files (and thus not
    called if the assemble.ml file is used as a standalone
    executable). *)

val get_setup : unit -> setup option
(** [get_setup ()] returns the setup environment, if any. *)

(** {1 Command runtime environments} *)

type t =
  { setup : setup option;         (* None if not run by assemblage. *)
    verbose : bool;
    color : [`Auto | `Always | `Never ];
    utf8_msgs : bool; }
(** The type for command runtime environments. *)

val create : setup option -> bool -> [`Auto | `Always | `Never] -> t
(** [create setup verbose color] is an environement with the corresponding
    parameters or as overriden by Sys.env values. *)

val created : unit -> bool
(** [true] if an environment value was created with {!created} *)

val variable_docs : (string * string) list
(** [variable_docs] is a list of environement variables and their
    documentation *)
