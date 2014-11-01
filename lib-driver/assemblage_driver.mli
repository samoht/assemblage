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

(** Driver helpers.

    Private functions and types for implementing drivers.

    {b Warning.} [Assemblage] users should not use these definitions
    they are subject to change even between minor versions of [Assemblage]. *)

open Assemblage
open Assemblage.Private
open Cmdliner

(** {1 Driver helpers} *)

(** Configuration specification.

    [Conf_spec] provides standard means for drivers to let the end
    user specify project configurations through command line
    arguments.  *)
module Conf_spec : sig

  (** {1 Configuration specification} *)

  val ui : Conf.t -> Conf.t Term.t
  (** [ui c] is a {!Cmdliner} term that allows to set the value of
      the public configuration keys of [c] using flags on the command
      line. For a given key [k], the default value if the flag is
      absent on the command line is the value of [k] in [c] ({b not}
      the default value of the key). *)

  val man : Conf.t -> Cmdliner.Manpage.block list
  (** [man c] is a man page fragment for the the term [ui c]. *)
end

(** Library preferences.

    [Lib_prefs] provides standard means for drivers to let the end
    user control the assemblage library preferences through command
    line arguments and environment variables. *)
module Lib_prefs : sig

  (** {1 Library preferences} *)

  type t =
    { fmt_utf8_enabled : bool;
      (** See {!Assemblage.Private.Fmt.utf8_enabled} *)
      fmt_style_tags : [ `Ansi | `None ];
      (** See {!Assemblage.Private.Fmt.style_tags} *)
      log_level : Log.level option;
      (** See {!Assemblage.Private.Log.level} *)
      cmd_vcs_override_kind : Cmd.Vcs.t option;
      (** See {!Assemblage.Private.Cmd.Vcs.override_kind} *)
      cmd_vcs_override_exec : string option;
      (** See {!Assemblage.Private.Cmd.Vcs.override_exec} *) }
    (** The type for library preferences. *)

  val set : t -> unit
  (** [set p] sets the library preferences to [p]. *)

  val get : unit -> t
  (** [get ()] is the library's current preferences. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf p] prints an unspecified representation of [p] on [ppf]. *)

  (** {1 User interface} *)

  val ui : docs:string -> t Term.t
  (** [ui ~docs] is a {!Cmdliner} term that defines a library
      preferences value either through command line options or through
      environment variables (see {!man_vars}). An environment variable
      definition takes over a corresponding command line option.
      [docs] is the manual section in which the command line options are
      documented. *)

  val man_vars : Manpage.block list
  (** [man_vars] is a man page fragment for the environment variables
      used by {!Lib_prefs.ui}. *)
end

(** Assemble file loader. *)
module Loader : sig

  (** {1 Loader} *)

  type kind = [ `Toplevel ]
  (** The type for kinds of loaders. *)

  val pp_kind : Format.formatter -> kind -> unit
  (** [pp_kind ppf kind] prints an unspecified representation of [kind]
      on [ppf]. *)

  type t =
    { kind : [ `Toplevel ];                            (** kind of loader. *)
      ocamlfind_exec : string;                     (** [ocamlfind] to use. *)
      auto_lib : bool;         (** [true] to add assemblage lib to loader. *)
      includes : string list;    (** includes to add to toploop execution. *)
      files : Path.t list;                          (** .ml files to load. *) }
  (** The type for loader settings. TODO: we could add packages to the mix. *)

  val load : ?level:Log.level -> t -> unit Cmd.result
  (** [load l] loads according to settings [l]. [level] indicates
      with which level file loads are logged, defaults to {!Level.Show}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf l] prints an unspecified representation of [l] on [ppf]. *)

  (** {1 User interface} *)

  val ui : ?kinds:kind list -> ?files:Path.t list Term.t -> docs:string ->
    unit -> t Term.t
  (** [ui ~docs] is a {!Cmdliner} term that defines a loader setting
      value either through command line options or through environment
      variables. An environment variable definition takes over a
      corresponding command line option. [kinds] lists the kind of
      loaders that should be supported, if absent all loader are supported.
      [files] defines how files to load should be parsed from the command
      line, default uses an optional argument. *)

  val man_vars : ?kinds:kind list -> unit -> Manpage.block list
  (** [man_vars] is a man page fragment for the environment variables
      used by {!Loader.ui}. *)
end

(** Initializing drivers.

    [Driver] uses the {!Lib_prefs} and {!Loader} modules transparently
    to initialize a driver. See an {{!example}example}. *)
module Driver : sig

  (** {1 Initialization} *)

  type init = (Lib_prefs.t * Loader.t) option
  (** The type for initialisation results. *)

  val init : ?version_opt:bool -> ?kinds:Loader.kind list -> docs:string ->
    unit -> init * unit Term.t
  (** [init ?version_opt ?kinds ~docs] an initialization value
      [(init,t)].  [docs] is the manual section in which the command
      line options for initialization are documented. [kinds] is the
      kinds of loader that should be supported (defaults to all).
      [version_opt] should be true if the driver supports a
      [--version] option through {!Cmdliner} (defaults to [false]).

      The returned term [t] should always be used in all the
      {!Cmdliner} terms that your program may evaluate. It contains
      the command line specification for initialization and may
      evaluate to an error that happened during initalization that
      will be reported by {!Cmdliner}'s {{!Cmdliner.Term.result}
      error mechanism}.

      There are two cases to consider:
      {ul
      {- [init] is [None]. In that case the driver should assume the
         contents of {!Assemblage.Private.Project.list} is meaningless
         and simply create its terms using [t]. The evaluation of
         these term will error either because of [t] or because
         of a command line parse error.}
      {- [init] is [Some (prefs, loader)]. In that case library
         preferences are already setup according to [prefs] and
         assemble files are already loaded according to [loader]. The
         {!Assemblage.Private.Project.list} contains the project that
         were registered by the assemble files. The driver can now
         consult the configuration of these project for example to
         specify configuration options for them on the command line
         with the {!Conf_spec} module. Eventually this should lead
         {!Cmdliner} term evaluations by the driver that must include
         [t] (so that the initalization options are correctly
         parsed).}} *)

  val man_vars : ?kinds:Loader.kind list -> unit -> Manpage.block list
  (** [man_vars kinds] is a man page fragment for the environment
      variables used by {!init}. *)

(** {1:example Example}

    This simple example shows how to output the version of a project
    as it could be configured on the command line.

{[
  let main () = ()
]}

*)


end
