(*
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

(** Command lines for drivers.

    [Assemblage_cli] contains a few useful tools to define command lines
    and documentation with {!Cmdliner}.

    Open the module to use it.
*)

open Assemblage
open Assemblage.Private

(** {1 Command line} *)

(** Command lines for drivers. *)
module Cli : sig

  (** {1 Configuration specification and documentation} *)

  val term_of_conf : Conf.t -> Conf.t Cmdliner.Term.t
  (** [term_of_conf c] is a command line term that allows to set the
      value of the public configuration keys of [c] using flags on the
      command line. For a given key [k], the default value if the flag
      is absent on the command line is the value of [k] in [c]
      ({b not} the default value of the key). *)

  val man_of_conf : Conf.t -> Cmdliner.Manpage.block list
  (** [man_of_conf c] is a man page fragment for the configuration [c]. *)
end
