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

(** Assemble software projects.

    [Assemblage] provides functions to describe the structure of your
    software project as a {{!project}project description} value. This
    data structure allows tools known as assemblage {e drivers} to
    explore, build and manage your project.

    Open the module to use it, this defines only modules, types and a
    few combinators in your scope.

    Consult the {{!basics}basics}.

    {e Release %%VERSION%% — %%MAINTAINER%% } *)

(** {1 Preliminaries} *)

(**/**)
include module type of Astring
module Fmt : module type of Fmt with type 'a t = 'a Fmt.t
(**/**)

(** File system paths, path sets and maps.

    [Path] provides three types for handling paths. Values of type
    {!Path.t} are for paths that are either relative or absolute while
    those of type {!Path.rel} and {!Path.abs} specialize to either
    case.

    Relative paths and absolute path each have corresponding modules
    {!Rel} and {!Abs} with specialized functions. {{!conversions}Conversion}
    between the three type of paths are explicit.

    {b FIXME}. We need to properly handle {!Filename.current_dir_name} and
    {!Filename.parent_dir_name} in path segments. *)
module Path : sig

  (** {1:filepaths File paths} *)

  type filename = string
  (** The type for file names (basenames). *)

  type rel
  (** The type for relative paths. *)

  type abs
  (** The type for absolute paths. *)

  type t
  (** The type for absolute or relative paths. *)

  val root : t
  (** [root] is the root absolute path (empty list of segments). *)

  val empty : t
  (** [empty] is the empty relative path (empty list of segments). *)

  val dash : t
  (** [dash] is the ["-"] relative path. *)

  val add : t -> string -> t
  (** [add p seg] concatenates [seg] at the end of [p]. For any [p],
      [add p "" = p]. *)

  val concat : t -> rel -> t
  (** [concat p p'] concatenates [p'] at the end of [p]. *)

  val ( / ) : t -> string -> t
  (** [p / c] is [add p c]. Left associative. *)

  val ( // ) : t -> rel -> t
  (** [p // p'] is [concat p p']. Left associative. *)

  val file : filename -> t
  (** [file name] is [add empty f]. *)

  val base : string -> t
  (** [base name] is [add empty f]. *)

  val basename : t -> string
  (** [basename p] is the basename of [p]. If [p] has no segments the
      empty string is returned. *)

  val dirname :  t -> t
  (** [dirname p] is the dirname of [p]. If [p] has no segments [p]
      is returned. *)

  val rem_prefix : t -> t -> rel option
  (** [rem_prefix pre p] is [p] with the literal prefix [pre] removed. [None]
      if [pre] is not a prefix of [p]. *)

  val find_prefix : t -> t -> t option
  (** [find_prefix p p'] is a common prefix for [p] and [p']. There is
      always a common prefix between path of the same kind (either {!root}
      or {!empty} and [None] is only returned if [p] and [p'] are of
      different kind. *)

  (** {1:predicates Predicates and comparison} *)

  val is_root : t -> bool
  (** [is_root p] is [true] iff [p] is {!root}. *)

  val is_empty : t -> bool
  (** [is_empty p] is [true] iff [p] is {!empty}. *)

  val is_dash : t -> bool
  (** [is_dash p] is [true] iff [p] is {!dash}. *)

  val is_rel : t -> bool
  (** [is_rel p] is [true] iff [p] is a relative path. *)

  val is_abs : t -> bool
  (** [is_abs p] is [true] iff [p] is an absolute path. *)

  val is_prefix : t -> t -> bool
  (** [is_prefix p p'] is [true] if [p] is a literal prefix of [p']. *)

  val equal : t -> t -> bool
  (** [equal p p'] is [p = p']. *)

  val compare : t  -> t -> int
  (** [compare p p'] is [Pervasives.compare p p']. *)

  (** {1:conversions Conversions} *)

  val to_rel : t -> rel option
  (** [to_rel p] is [Some r] if [p] is a relative path. *)

  val of_rel : rel -> t
  (** [of_rel r] is [r] as a path. *)

  val to_abs : t -> abs option
  (** [to_abs p] is [Some a] if [p] is an absolute path. *)

  val of_abs : abs -> t
  (** [of_abs a] is [a] as a path. *)

  val to_segs : t -> [ `Abs of string list | `Rel of string list ]
  (** [to_segs p] is [p]'s segments. *)

  val of_segs : [ `Abs of string list | `Rel of string list ] -> t
  (** [of_segs segs] is a path from [segs] segments. *)

  val to_string : t -> string
  (** [to_string p] is the path [p] as a string according to
      the driver's platform convention with {!Filename.dir_sep}. *)

  val of_string : string -> t
  (** [of_string s] is the string [s] as a path. [s] is splitted
      according to the driver's platform convention with {!Filename.dir_sep}. *)

  val quote : t -> string
  (** [quote p] is the path [p] as a string, quoted according
       to the driver's platform conventions with {!Filename.quote}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf p] prints path [p] on [ppf] using {!to_string}. *)

  (** {1:file_exts File extensions} *)

  type ext =
    [ `A | `Byte | `C | `Cma | `Cmi | `Cmo | `Cmt | `Cmti | `Cmx | `Cmxa
    | `Cmxs | `Css | `Dll | `Exe | `Gif | `H | `Html | `Install | `Img
    | `Jpeg | `Js | `Json | `Lib | `Md | `Ml | `Ml_dep | `Ml_pp | `Mli
    | `Mli_dep | `Mli_pp | `Native | `O | `Opt | `Png | `Sh | `So | `Tar
    | `Tbz | `Xml | `Zip | `Prepare
    | `Ext of string ]
  (** The type for file extensions. *)

  val ext_to_string : ext -> string
  (** [ext_to_string ext] is [ext] as a string (without separator). *)

  val ext_of_string : string -> ext
  (** [ext_of_string ext] is [ext] as a file extension ([ext] without
      separator). *)

  val pp_ext : Format.formatter -> ext -> unit
  (** [pp_ext ppf p] prints file extension [ext] on [ppf] using
      {!ext_to_string}. *)

  val ext : t -> ext option
  (** [ext p] is [p]'s last segment file extension (if any). *)

  val get_ext : t -> ext
  (** [get_ext p] is [p]'s last segment file extension.

      @raise Invalid_argument if [p]'s last segment has no file extension. *)

  val add_ext : t -> ext -> t
  (** [add_ext p ext] is [p] with [ext] concatenated to [p]'s last segment. *)

  val rem_ext : t -> t
  (** [rem_ext p] is [p] with [ext] removed from [p]'s last segment
      (if it has an extension). *)

  val change_ext : t -> ext -> t
  (** [change_ext p e] is [add_ext (rem_ext p)]. *)

  val ( + ) : t -> ext -> t
  (** [p + ext] is [add_ext p e]. Left associative. *)

  val has_ext : ext -> t -> bool
  (** [has_ext p ext] is [true] iff [p]'s last segment has file extension
      [ext]. *)

  val ext_matches : ext list -> t -> bool
  (** [ext_matches exts p] is [true] iff [p]'s last segment has a file
      extension in [exts]. *)

  (** {1:rel Relative paths} *)

  (** Relative paths. *)
  module Rel : sig

    (** {1 Relative paths} *)

    type path = t
    (** The type for absolute or relative paths. *)

    type t = rel
    (** The type for relative paths. *)

    val empty : rel
    (** See {!Path.empty}. *)

    val dash : rel
    (** See {!Path.dash}. *)

    val add : rel -> string -> rel
    (** See {!Path.add}. *)

    val concat : rel -> rel -> rel
    (** See {!Path.concat}. *)

    val file : filename -> rel
    (** [file name] is [add empty f]. *)

    val base : string -> rel
    (** [base name] is [add empty f]. *)

    val ( / ) : rel -> string -> rel
    (** See {!Path.( / )}. *)

    val ( // ) : rel -> rel -> rel
    (** See {!Path.( // )}. *)

    val basename : rel -> string
    (** See {!Path.basename}. *)

    val dirname :  rel -> rel
    (** See {!Path.dirname}. *)

    val rem_prefix : rel -> rel -> rel option
    (** See {!Path.rem_prefix}. *)

    val find_prefix : rel -> rel -> rel
    (** See {!Path.find_prefix}. *)

    (** {1:predicates Predicates and comparison} *)

    val is_empty : rel -> bool
    (** See {!Path.is_empty}. *)

    val is_dash : rel -> bool
    (** See {!Path.is_dash}. *)

    val is_prefix : rel -> rel -> bool
    (** See {!Path.is_prefix}. *)

    val equal : rel -> rel -> bool
    (** See {!Path.equal}. *)

    val compare : rel  -> rel -> int
    (** See {!Path.compare}. *)

    (** {1 Conversions} *)

    val to_segs : rel -> string list
    (** [to_segs r] is [r]'s segments. *)

    val of_segs : string list -> rel
    (** [of_segs segs] is a path from [segs] segments. *)

    val to_string : rel -> string
    (** See {!Path.to_string}. *)

    val quote : rel -> string
    (** See {!Path.quote}. *)

    val pp : Format.formatter -> rel -> unit
    (** See {!Path.pp}. *)

    (** {1:file_exts File extensions} *)

    val ext : rel -> ext option
    (** See {!Path.ext}. *)

    val get_ext : rel -> ext
    (** See {!Path.get_ext}. *)

    val add_ext : rel -> ext -> rel
    (** See {!Path.add_ext}. *)

    val rem_ext : rel -> rel
    (** See {!Path.rem_ext}. *)

    val change_ext : rel -> ext -> rel
    (** See {!Path.change_ext}. *)

    val ( + ) : rel -> ext -> rel
    (** See {!Path.( + )}. *)

    val has_ext : ext -> rel -> bool
    (** See {!Path.has_ext}. *)

    val ext_matches : ext list -> rel -> bool
    (** See {!Path.ext_matches}. *)

    (** {1:sets_maps Path sets and maps} *)

    module Set : sig
      include Set.S with type elt = rel
      val of_list : elt list -> t
    end

    module Map : sig
      include Map.S with type key = rel
      val dom : 'a t -> Set.t
      (** [dom m] is the domain of [m]. *)
    end
  end

  (** {1:abs Absolute paths} *)

  (** Absolute paths. *)
  module Abs : sig

    (** {1 Absolute paths} *)

    type path = t
    (** The type for absolute or relative paths. *)

    type t = abs
    (** The type for absolute paths. *)

    val root : abs
    (** See {!Path.root}. *)

    val add : abs -> string -> abs
    (** See {!Path.add}. *)

    val concat : abs -> rel -> abs
    (** See {!Path.concat}. *)

    val ( / ) : abs -> string -> abs
    (** See {!Path.( / )}. *)

    val ( // ) : abs -> rel -> abs
    (** See {!Path.( // )}. *)

    val basename : abs -> string
    (** See {!Path.basename}. *)

    val dirname :  abs -> abs
    (** See {!Path.dirname}. *)

    val rem_prefix : abs -> abs -> rel option
    (** See {!Path.rem_prefix}. *)

    val find_prefix : abs -> abs -> abs
    (** See {!Path.find_prefix}. *)

    (** {1:predicates Predicates and comparison} *)

    val is_root : abs -> bool
    (** See {!Path.is_root}. *)

    val is_prefix : abs -> abs -> bool
    (** See {!Path.is_prefix}. *)

    val equal : abs -> abs -> bool
    (** See {!Path.equal}. *)

    val compare : abs  -> abs -> int
    (** See {!Path.compare}. *)

    (** {1:conversions Conversions} *)

    val to_segs : abs -> string list
    (** [to_segs a] is [a]'s segments. *)

    val of_segs : string list -> abs
    (** [of_segs segs] is a path from [segs] segments. *)

    val to_string : abs -> string
    (** See {!Path.to_string}. *)

    val quote : abs -> string
    (** See {!Path.quote}. *)

    val pp : Format.formatter -> abs -> unit
    (** See {!Path.pp}. *)

    (** {1:file_exts File extensions} *)

    val ext : abs -> ext option
    (** See {!Path.ext}. *)

    val get_ext : abs -> ext
    (** See {!Path.get_ext}. *)

    val add_ext : abs -> ext -> abs
    (** See {!Path.add_ext}. *)

    val rem_ext : abs -> abs
    (** See {!Path.rem_ext}. *)

    val change_ext : abs -> ext -> abs
    (** See {!Path.change_ext}. *)

    val ( + ) : abs -> ext -> abs
    (** See {!Path.( + )}. *)

    val has_ext : ext -> abs -> bool
    (** See {!Path.has_ext}. *)

    val ext_matches : ext list -> abs -> bool
    (** See {!Path.ext_matches}. *)

    (** {1:sets_maps Path sets and maps} *)

    module Set : sig
      include Set.S with type elt = abs
      val of_list : elt list -> t
    end

    module Map : sig
      include Map.S with type key = abs
      val dom : 'a t -> Set.t
      (** [dom m] is the domain of [m]. *)
    end
  end

  (** {1:sets_maps Path sets and maps} *)

  module Set : sig
    include Set.S with type elt = t
    val of_list : elt list -> t
  end

  module Map : sig
    include Map.S with type key = t
    val dom : 'a t -> Set.t
    (** [dom m] is the domain of [m]. *)
  end
end

(** Assemblage log.

    [Log] provides functions to log messages from assemble files. It
    is also used by the assemblage library itself. The log's output
    is controlled by drivers. *)
module Log : sig

  (** {1 Log level} *)

  (** The type for log levels. *)
  type level = Show | Error | Warning | Info | Debug

  val msg : ?header:string -> level ->
    ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [msg header l fmt ...] logs a message with level [l]. [header] is
      the message header, default depends on [l]. *)

  val kmsg : ?header:string ->
    (unit -> 'a) -> level -> ('b, Format.formatter, unit, 'a) format4 -> 'b
  (** [kmsg header k l fmt ...] is like [msg header l fmt] but calls [k ()]
      before returning. *)

  val show : ?header:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [show fmt ...] logs a message with level [Show]. [header] defaults
      to [None]. *)

  val err : ?header:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [err fmt ...] logs a message with level [Error]. [header] defaults
      to ["ERROR"]. *)

  val warn : ?header:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [warn fmt ...] logs a message with level [Warning]. [header] defaults
      to ["WARNING"]. *)

  val info : ?header:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [info fmt ...] logs a message with level [Info]. [header] defaults
      to ["INFO"]. *)

  val debug : ?header:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [debug info ...] logs a message with level [Debug]. [header] defaults
      to ["DEBUG"]. *)

end

(** Executing {e non-build} commands and IO operations.

    [Cmd] provides functions to execute {e non-build} commands
    and perform IO operations. They can be used to define the default
    value of {{!Conf}configuration keys} in assemble files. *)
module Cmd : sig

  (** {1:command_results Command and IO results} *)

  type 'a result = [ `Ok of 'a | `Error of string ]
  (** The type for command and IO results. *)

  val ret : 'a -> 'a result
  (** [ret v] is [`Ok v]. *)

  val error : string -> 'a result
  (** [error e] is [`Error e]. *)

  val bind : 'a result -> ('a -> 'b result) -> 'b result
  (** [bind r f] is [f v] if [r = `Ok v] and [r] if [r = `Error _]. *)

  val map : 'a result -> ('a -> 'b) -> 'b result
  (** [map r f] is [bind r (fun v -> ret (f v))]. *)

  val get : 'a result -> 'a
  (** [get r] is [v] if [r = `Ok v] and @raise Invalid_argument otherwise. *)

  val on_error : ?level:Log.level -> use:'a -> 'a result -> 'a
  (** [on_error ~level ~use r] is:
      {ul
      {- [v] if [r = `Ok v]}
      {- [use] if [r = `Error msg]. As a side effect [msg] is
       {{!Log}logged} with level [level] (defaults to
       {!Log.Error})}} *)

  val ignore_error : use:'a -> 'a result -> 'a
  (** [ignore_error ~use r] is like {!on_error} but the error
      is not logged. *)

  val reword_error : ?replace:bool -> string -> 'a result -> 'a result
  (** [reword_error msg r] uses [msg] for the error message in case of
      [`Error]. If replace is [false] (default), [msg] is stacked on
      top of the old message. *)

  val exn_error : ?msg:(Printexc.raw_backtrace -> exn -> 'a -> string) ->
    (('a -> 'b) -> ('a -> 'b result))

  val ( >>= ) : 'a result -> ('a -> 'b result) -> 'b result
  (** [r >>= f] is [bind r f]. *)

  val ( >>| ) : 'a result -> ('a -> 'b) -> 'b result
  (** [r >>| f] is [map r f]. *)

  (** Infix operators.

      Gathers {!Cmd}'s infix operators. *)
  module Infix : sig

    (** {1 Infix operators} *)

    val ( >>= ) : 'a result -> ('a -> 'b result) -> 'b result
    (** [(>>=)] is {!Cmd.( >>= )}. *)

    val ( >>| ) : 'a result -> ('a -> 'b) -> 'b result
    (** [(>>|)] is {!Cmd.( >>| )}. *)
  end

  (** {1:io IO and file system operations} *)

  type path = Path.t

  (** Path operations. *)
  module Path : sig

    (** {1:pathops Path operations} *)

    val exists : ?err:bool -> path -> bool result
    (** [exists err path] is [true] iff [path] exists.
        If [err] is [true] (defaults to [false]) an error is returned
        when the file doesn't exist. *)

    val move : ?force:bool -> path -> path -> unit result
    (** [move ~force src dst] moves path [src] to [dst]. If [force] is
        [false] (default) the operation fails if [dst] exists. *)
  end

  (** File operations. *)
  module File : sig

    (** {1:fileops File operations}

        {b Note.} When paths are {{!Path.rel}relative} they are expressed
        relative to the {{!Dir.getcwd}current working directory}. *)

    val exists : ?err:bool -> path -> bool result
    (** [exists err file] is [true] iff [file] exists and is not a directory.
        If [err] is [true] (defaults to [false]) an error is returned
        when the file doesn't exist. *)

    val dev_null : path
    (** [dev_null] represents a file that discards all writes.

        {b Warning.} Do not use this value to define build actions,
        use {!Acmd.dev_null}. *)

    val delete : ?maybe:bool -> path -> unit result
    (** [delete ~maybe file] deletes file [file]. If [maybe] is [false]
        (default) no error is returned if the file doesn't exit. *)

    val temp : ?dir:path -> string -> path result
    (** [temp dir suffix] creates a temporary file with suffix
        [suffix] in [dir] (defaults to {!Filename.get_temp_dir_name})
        and returns its name. The file is destroyed at the end of
        program execution. *)

    (** {1:input Input} *)

    val with_inf : (in_channel -> 'a -> 'b result) -> path -> 'a ->
      'b result
    (** [with_inf f inf v] opens [inf] as a channel [ic] and returns [f
        ic v] if no error occurs. In case of error the channel is closed
        and the error is returned. If [inf] is {!Path.dash}, [ic] is
        {!Pervasives.stdin} and not closed. *)

    val read : path -> string result
    (** [read file] is [file]'s content. If [file] is {!Path.dash} reads
        from {!Pervasives.stdin}. *)

    val read_lines : path -> string list result
    (** [read_lines file] is [file]'s content splitted at ['\n']. If
        [file] is {!Path.dash} reads from {!Pervasives.stdin}. *)

    (** {1:output Output} *)

    val with_outf : (out_channel -> 'a -> 'b result) -> path -> 'a ->
      'b result
    (** [with_inf f outf v] opens [outf] as a channel [oc] and returns
        [f oc v] if no error occurs. In case of error the channel is
        closed and the error is returned. If [outf] is {!Path.dash}, [oc] is
        {!Pervasives.stdout} and not closed. *)

    val write : path -> string -> unit result
    (** [write file content] outputs [content] to [file]. If [file]
        is {!Path.dash}, writes to {!Pervasives.stdout}. If an error is
        returned [file] is left untouched except if {!Pervasives.stdout}
        is written.*)

    val write_lines : path -> string list -> unit result
    (** [write_lines file lines] outputs [lines] separated by ['\n'] to
        [file]. If [file] is {!Path.dash}, writes to {!Pervasives.stdout}.
        If an error is returned [file] is left untouched except if
        {!Pervasives.stdout} is written.*)

    val write_subst : (string * string) list -> path -> string ->
      unit result
    (** [write_subst vars file content] outputs [content] to [file]. In
        [content] patterns of the form ["%%ID%%"] are replaced by the value
        of [List.assoc "ID" vars] (if any). If [file] is {!Path.dash}, writes
        to {!Pervasives.stdout}. If an error is returned [file] is left
        untouched except if {!Pervasives.stdout} is written.

        FIXME: add optional argument [delim] that defaults to [%%],
        the latter may appear in programs. *)
  end

  (** Directory operations. *)
  module Dir : sig

    (** {1:dirops Directory operations}

        {b Note.} When paths are {{!Path.rel}relative} they are expressed
        relative to the {{!Dir.getcwd}current working directory}. *)

    val exists : ?err:bool -> path -> bool result
    (** [exists err dir] is [true] if directory [dir] exists.
        If [err] is [true] (defaults to [false]) an error is returned
        when the file doesn't exist. *)

    val getcwd : unit -> path result
    (** [getcwd ()] is the current working directory. *)

    val chdir : path -> unit result
    (** [chdir dir] changes the current working directory to [dir]. *)

    val fold_files_rec : ?skip:string list -> (string -> 'a -> 'a result) ->
      'a -> string list -> 'a result
    (** [fold_files_rec skip f acc paths] folds [f] over the files
        found in [paths]. Files and directories whose suffix matches an
        element of [skip] are skipped. {b FIXME} this should be using
        {!Path.t} and {!Path.ext}. *)
  end

  (** Version control system operations.

      {b Note.} To remain VCS agnostic use the result of
      {!find} or {!get} rather than explicitely mentioning your VCS. *)
  module Vcs : sig

    (** {1:vcsops Version control system operations} *)

    type t = [ `Git | `Hg ]
    (** The type for version control systems. *)

    val exists : path -> t -> bool result
    (** [exists d vcs] is [true] if the VCS [vcs] is detected in
        directory [d]. *)

    val find : path -> t option result
    (** [find d] looks for an arbitrary VCS in directory [d]. *)

    val get : path -> t result
    (** [get d] is like {!exists} but returns an error if no VCS
        was found. *)

    val head : ?dirty:bool -> path -> t -> string result
    (** [head dirty d vcs] is the HEAD commit identifier of the VCS
        [vcs] in in directory [d]. If [dirty] is [true] (default)
        an indicator is appended to the identifier if the working tree
        is dirty. *)

    val describe : ?dirty:bool -> path -> t -> string result
    (** [describe dirty d vcs] identifies the HEAD commit using
        tags from the VCS [vcs] in directory [d]. If [dirty] is
        [true] (default) an indicator is appended to the identifier if
        the working tree is dirty. *)
  end

  (** {1:env_lookup Environment variables lookup} *)

  val env : string -> string option
  (** [env var] is the value if the environment variable [var], if
      defined. *)

  val get_env : string -> string result
  (** [get_env var] is like {!env} but returns an error if [var] is
      undefined. *)

  (** {1:executing_commands Executing commands} *)

  val exists : ?err:bool -> string -> bool result
  (** [exists err cmd] is [true] if [cmd] exists and can be invoked.
      If [err] is [true] (defaults to [false]) an error is returned
      when the command doesn't exist. *)

  val exec_ret : string -> string list -> int
  (** [exec_ret cmd args] executes [cmd] with arguments [args] and
      returns the exit code of the invocation. *)

  val exec : string -> string list -> unit result
  (** [exec cmd args] executes [cmd] with arguments [args]. On exit
      code [0] returns [`Ok ()]. Otherwise an error message with
      the failed invocation and its exit code is returned in [`Error]. *)

  val read : ?trim:bool -> string -> string list -> string result
  (** [read cmd args] execute [cmd] with arguments [args] and returns
      its standard output. If [cmd]'s return code is non zero returns
      an error message. If [trim] is [true] (default) the contents is
      passed to {!String.trim} before being returned. *)

  val read_lines : string -> string list -> string list result
  (** [input_lines cmd args] is like [input ~trim:false cmd args] but
      the input is splitted at ['\n']. *)

  val write : string -> string list -> path -> unit result
  (** [write cmd args file] execute [cmd] with arguments [args] and writes
      the invocation's [stdout] to [file]. In [cmd]'s return code is non
      zero returns an error message and [file] is left intact. *)
end

(** {1:building Building} *)

(** Build configuration.

    A configuration represents a concrete build environment for a
    project. It allows to adjust a project's outcomes to the build
    environment and desires of the end user.

    A {e configuration} is a map from named {{!type:key}keys} to
    configuration {{!type:value}values}. The value associated to a key
    is either determined automatically from the environment by the
    key's default value or explicitly set by the end user of the build
    system (e.g. manually from the command line or via an IDE).

    A {e configuration value} denotes an OCaml value that depends on
    the configuration. Configuration values are {{!values}transformed
    and composed} into new values and keep track of the set of keys
    that are needed to define them. Since they must be used to define
    a project, it means that assemblage can (conservatively) determine
    the configuration keys of a project.

    Configuration {{!type:scheme}schemes} are named, user defined,
    partial configurations. They allow the ends user to quickly setup
    a given configuration (in general it's a good practice not to
    use them in the context of package distribution, try to rely on
    the defaults as much as possible).

    {b Important.} Before {{!key}defining} your own keys you should prefer the
    {{!builtin_keys}built-in ones}. *)
module Conf : sig

  (** {1:values Configuration values} *)

  type 'a value
  (** The type for configuration values evaluating to OCaml values of type ['a].

      A value of this type means that it depends on the configuration:
      its concrete value can not be determined without a
      configuration. We say that we {e evaluate} a value when we take
      a configuration and determine its OCaml value in that configuration. *)

  val const : 'a -> 'a value
  (** [const v] is a configuration value that evaluates to [v]. *)

  val app : ('a -> 'b) value -> 'a value -> 'b value
  (** [app f v] is a configuration value that evaluates to the result
      of applying the evaluation of [v] to the one of [f]. *)

  val ( $ ) : ('a -> 'b) value -> 'a value -> 'b value
  (** [f $ v] is [app f v]. *)

  val true_ : bool value
  (** [true_] is [const true]. *)

  val false_ : bool value
  (** [false_] is [const false]. *)

  val neg : bool value -> bool value
  (** [neg a] is [const ( not ) $ a]. *)

  val ( &&& ) : bool value -> bool value -> bool value
  (** [a &&& b] is [const ( && ) $ a $ b]. *)

  val ( ||| ) : bool value -> bool value -> bool value
  (** [a &&& b] is [const ( || ) $ a $ b]. *)

  val pick_if : bool value -> 'a value -> 'a value -> 'a value
  (** [pick_if c a b] is [a] if [c] evaluates to [true] and [b]
      otherwise. *)

  module Option : sig

    (** {1 Options} *)

    val wrap : 'a value option -> 'a option value
    (** [wrap o] is [o] as an optional value. *)

    val some : 'a value -> 'a option value
    (** [some v] is the value of [v] as a [Some] value. *)

    val get : ?none:'a value -> 'a option value -> 'a value
    (** [get none v] is the value of the option [v].

        @raise Invalid_argument if the value of [v] is [None] and
        there [none] is unspecified. *)
  end

  (** {1 Configuration converters}

      A configuration converter transforms a string value to an OCaml
      value and vice-versa. There are a few {{!builtin_converters}built-in
      converters}. *)

  type 'a parser = string -> [ `Error of string | `Ok of 'a ]
  (** The type for configuration converter parsers. *)

  type 'a printer = Format.formatter -> 'a -> unit
  (** The type for configuration converter printers. *)

  type 'a converter = 'a parser * 'a printer
  (** The type for configuration converters. *)

  val parser : 'a converter -> 'a parser
  (** [parser c] is [c]'s parser. *)

  val printer : 'a converter -> 'a printer
  (** [converter c] is [c]'s printer. *)

  (** {1 Configuration keys} *)

  type 'a key
  (** The type for configuration keys that map to
      {{!type:value}values} of type ['a]. *)

  val key : ?public:bool -> ?docs:string -> ?docv:string -> ?doc:string ->
    string -> 'a converter -> 'a value -> 'a key
  (** [key public docs docv doc name conv v] is a configuration key
      named [name] that maps to value [v] by default. [converter] is
      used to convert key values provided by end users.

      If [public] is [true] (default), the key is public which means
      that it can be redefined by the end user. In this case [docs] is
      the title of a documentation section under which the key is
      documented (defaults to {!docs_project}). [doc] is a short
      documentation string for the key, this should be a single
      sentence or paragraph starting with a capital letter and ending
      with a dot.  [docv] is a meta-variable for representing the
      values of the key (e.g. ["BOOL"] for a boolean).

      @raise Invalid_argument if the key name is not made of a
      sequence of ASCII lowercase letter, digit, dash or underscore.
      FIXME not implemented.

      {b Warning.} No two public keys should share the same [name] as
      this may lead to difficulties in the UI of certain assemblage
      drivers like the inability to be able to precisely refer to a
      key.  In particular do not reuse the {{!builtin_keys}built-in
      names} (they have the same name as the key variables with
      underscores replaced by dashes). *)

  val value : 'a key -> 'a value
  (** [value k] is a value that evaluates to [k]'s binding in the
      configuration. Note that this may be different from the value given in
      {!key}. *)

  (** {1:scheme Configuration schemes} *)

  type scheme
  (** The type for configuration schemes. A configuration scheme is
      a named set of configuration key-value binding definitions. *)

  type def
  (** The type for key-value binding definitions. *)

  val def : 'a key -> 'a -> def
  (** [def k v] is a definition that binds key [k] to value [v]. *)

  val defv : 'a key -> 'a value -> def
  (** [defv k v] is a definition that binds key [k] to value [v]. *)

  val scheme : ?doc:string -> ?base:scheme -> string -> def list -> scheme
  (** [scheme base name defs] is a configuration scheme named [name]
      that has the key-value bindings of [base] together with those
      of [defs], the latter taking precedence. [doc] is a documentation
      string for the scheme, it should be a single sentence or paragraph
      starting with a capital letter and ending with a dot. *)

  (** {1:builtin_keys Built-in configuration keys} *)

  (** {2:project_keys Project keys} *)

  val project_version : string key
  (** [project_version] is the version number of the project (defaults
      is inferred from the VCS). FIXME what to do on distrib setup ?
      read from a file ? Best would be that the distrib procedure
      sets the version field of the opam file in the tarball and
      that we read from there. *)

  val docs_project : string
  (** [docs_project] is the name of the documentation section
      in which project keys are described. *)

  (** {2:build_property_keys Build property keys} *)

  val debug : bool key
  (** [debug] is [true] iff build products in general must support debugging
      (defaults to [false]). *)

  val profile : bool key
  (** [profile] is [true] iff build products in general must support profiling
      (defaults to [false]). *)

  val warn_error : bool key
  (** [warn_error] is [true] iff tools in general should treat
      warnings as errors (defaults to [false]). *)

  val test : bool key
  (** [test] is [true] iff test build products should be built
      (defaults to [false]). *)

  val doc : bool key
  (** [doc] is [true] iff documentation should be built
      (defaults to [false]). *)

  val jobs : int key
  (** [jobs] is the number of jobs to run for building (defaults to machine
      processor count). *)

  val docs_build_properties : string
  (** [docs_build_properties] is the name of the documentation section
      in which build property keys are described. *)

  (** {2:build_directory_keys Build directory keys} *)

  val root_dir : Path.t key
  (** [root_dir] is the absolute path to the project directory (defaults
      to the driver program current directory). *)

  val build_dir : Path.rel key
  (** [build_dir] is the path to the build directory expressed relative to the
      {!root_dir} (defaults to ["_build"]). *)

  val docs_build_directories : string
  (** [docs_build_directories] is the name of the documentation section
      in which build directory keys are described. *)

  (** {2:ocaml_system_keys OCaml system keys} *)

  val ocaml_native_tools : bool key
  (** [ocaml_native_tools] is [true] to use the native compiled ([.opt])
      OCaml tools (defaults to [true]). *)

  val ocaml_version : (int * int * int * string option) key
  (** [ocaml_version] is the OCaml compiler version (defaults is inferred
      by invoking an OCaml compiler). *)

  val ocaml_byte : bool key
  (** [ocaml_byte] is [true] iff OCaml byte code compilation is
      requested (defaults to [true]). *)

  val ocaml_native : bool key
  (** [ocaml_native] is [true] iff OCaml native code compilation is
      requested (defaults to [true]). *)

  val ocaml_native_dynlink : bool key
  (** [ocaml_native_dynlink] is [true] iff OCaml native code dynamic linking
      compilation is requested (defaults to [true]). *)

  val ocaml_js : bool key
  (** [ocaml_js] is [true] iff OCaml JavaScript compilation is requested
      (defaults to [false]). *)

  val ocaml_annot : bool key
  (** [ocaml_annot] is [true] iff OCaml binary annotation files generation
      is requested (defaults to [true]). *)

  val ocaml_build_ast : bool key
  (** [ocaml_build_ast] is [true] if OCaml source AST should be built
      and dumped to a file with {!ocaml_dumpast} (defaults to
      [false]).  This may speed up your builds if you have
      pre-processing cancer. *)

  val ocaml_dumpast : string key
  (** [ocaml_dumpast] is the
      {{:https://github.com/samoht/ocaml-dumpast}[ocaml-dumpast]} utility
      (defaults to ["ocaml-dumpast"]). *)

  val ocamlc : string key
  (** [ocamlc] is the
      {{:http://caml.inria.fr/pub/docs/manual-ocaml/comp.html}[ocamlc]}
      utility (defaults to ["ocamlc"] or ["ocamlc.opt"] according to
      {!ocaml_native_tools}).*)

  val ocamlopt : string key
  (** [ocamlopt] is the
      {{:http://caml.inria.fr/pub/docs/manual-ocaml/native.html}[ocamlopt]}
      utility (defaults to ["ocamlopt"] or ["ocamlopt.opt"] according to
      {!ocaml_native_tools}).*)

  val js_of_ocaml : string key
  (** [js_of_ocaml] is the
      {{:http://ocsigen.org/js_of_ocaml/}[js_of_ocaml]} utility (defaults to
      ["js_of_ocaml"]). *)

  val ocamldep : string key
  (** [ocamldep] is the
      {{:http://caml.inria.fr/pub/docs/manual-ocaml/depend.html}[ocamldep]}
      utility (defaults to ["ocamldep"] or ["ocamldep.opt"] according to
      {!ocaml_native_tools}). *)

  val ocamlmklib : string key
  (** [ocamlmklib] is the
      {{:http://caml.inria.fr/pub/docs/manual-ocaml/intfc.html#sec468}
      [ocamlmklib]} utility (defaults to ["ocamlmklib"]). *)

  val ocamldep : string key
  (** [ocamldep] is the
      {{:http://caml.inria.fr/pub/docs/manual-ocaml/ocamldoc.html}[ocamldoc]}
      utility (defaults to ["ocamldoc"] or ["ocamldoc.opt"] according to
      {!ocaml_native_tools}). *)

  val ocamllex : string key
  (** [ocamllex] is the
      {{:http://caml.inria.fr/pub/docs/manual-ocaml/lexyacc.html#sec276}
      [ocamllex]} utility (defaults to ["ocamllex"] or ["ocamllex.opt"]
      according to {!ocaml_native_tools}). *)

  val ocamlyacc : string key
  (** [ocamlyacc] is the
      {{:http://caml.inria.fr/pub/docs/manual-ocaml/lexyacc.html#sec287}
      [ocamlyacc]} utility (defaults to ["ocamlyacc"]). *)

  val ocaml : string key
  (** [ocaml] is the
      {{:http://caml.inria.fr/pub/docs/manual-ocaml/toplevel.html}[ocaml]}
      utility (defaults to ["ocaml"]). *)

  val ocamlrun : string key
  (** [ocamlrun] is the
      {{:http://caml.inria.fr/pub/docs/manual-ocaml/runtime.html}[ocamlrun]}
      utility (defaults to ["ocamlrun"]). *)

  val ocamldebug : string key
  (** [ocamldebug] is the
      {{:http://caml.inria.fr/pub/docs/manual-ocaml/debugger.html}[ocamldebug]}
      utility (defaults to ["ocamldebug"]). *)

  val ocamlprof : string key
  (** [ocamlprof] is the
      {{:http://caml.inria.fr/pub/docs/manual-ocaml/profil.html}[ocamlprof]}
      utility (defaults to ["ocamlprof"]). *)

  val ocamlfind : string key
  (** [ocamlfind] is the
      {{:http://projects.camlcity.org/projects/findlib.html}[ocamlfind]}
      utility (defaults to ["ocamlfind"]). *)

  val opam : string key
  (** [opam] is the {{:http://opam.ocaml.org/}[opam]} tool
      (defaults to ["opam"]). *)

  val opam_installer : string key
  (** [opam_installer] is the [opam-installer] tool
      (defaults to ["opam-installer"]) *)

  val opam_admin : string key
  (** [opam_admin] is the [opam-admin] tool (defaults to ["opam-admin"]) *)

  val docs_ocaml_system : string
  (** [docs_ocaml_system] is the name of the documentation section
      in which OCaml system keys are described. *)

  (** {2:c_system_keys C system keys} *)

  val c_dynlink : bool key
  (** [c_dynlink] is [true] iff C dynamic linking compilation is
      requested (default to [true]). *)

  val c_js : bool key
  (** [c_js] is [true] iff C JavaScript compilation is request (defaults
      to [false]). FIXME through e.g. emscripten should we support that ?
      Well it's just a few actions after all. *)

  val cc : string key
  (** [cc] is the C compiler (defaults to ["cc"]) *)

  val pkg_config : string key
  (** [pkg_config] is the {{:http://pkg-config.freedesktop.org/}[pkg-config]}
      utility (defaults to ["pkg-config"]). *)

  val docs_c_system : string
  (** [docs_c_system] is the name of the documentation section
      in which C system keys are described. *)

  (** {2:machine_information_keys Machine information keys} *)

  val uname : string key
  (** [uname] is the
      {{:http://pubs.opengroup.org/onlinepubs/009695399/utilities/uname.html}
      [uname]} utility (defaults to ["uname"]). *)

  val host_os : string key
  (** [host_os] is the operating system name of the host machine (defaults
      is ["Win32"] if the driver's {!Sys.os_type} is ["Win32"]
      otherwise it is the lowercased result of {!uname} invoked with [-s]). *)

  val host_arch : string key
  (** [host_arch] is the hardware architecture of the host machine
      (defaults to the value of the driver's
      ["PROCESSOR_ARCHITECTURE"] environment variable if the driver
      {!Sys.os_type} is ["Win32"] otherwise it is the lowercased
      result of {!uname} invoked with [-m]) *)

  val host_word_size : int key
  (** [host_word_size] is the host machine word bit size (defaults to
      the driver's {!Sys.word_size}). *)

  val target_os : string key
  (** [target_os] is the operating system name of the target machine (defaults
      to the value of {!host_os}). *)

  val target_arch : string key
  (** [target_arch] is the hardware architecture of the target machine (defaults
      to the value of {!host_arch}). *)

  val target_word_size : int key
  (** [target_word_size] is the target machine word bit size (defaults to
      the value of {!host_word_size}). *)

  val docs_machine_information : string
  (** [docs_machine_utilities] is the name of the documentation section
      in which machine information keys are described. *)

  (** {2:system_utility_keys System utility keys}

      {b Note.} Action commands should not use these utilities directly
      but use {{!Acmd.portable_invocations}portable system utility
      invocations}. *)

  val echo : string key
  (** [echo] is the
      {{:http://pubs.opengroup.org/onlinepubs/009695399/utilities/echo.html}
      echo} utility (defaults to ["echo"]). *)

  val cd : string key
  (** [cd] is the
      {{:http://pubs.opengroup.org/onlinepubs/009695399/utilities/cd.html}[cd]}
      utility (defaults to ["cd"]). *)

  val ln : string key
  (** [ln] is the
      {{:http://pubs.opengroup.org/onlinepubs/009695399/utilities/ln.html}ln}
      utility (defaults to ["ln"]). FIXME windows. *)

  val cp : string key
  (** [cp] is the
      {{:http://pubs.opengroup.org/onlinepubs/009695399/utilities/cp.html}[cp]}
      utility (defaults to ["cp"] on Unix and ["copy"] on Windows). *)

  val mv : string key
  (** [mv] is the
      {{:http://pubs.opengroup.org/onlinepubs/009695399/utilities/mv.html}[mv]}
      utility (defaults to ["mv"] on Unix and ["move"] on Windows). *)

  val rm : string key
  (** [rm] is the
      {{:http://pubs.opengroup.org/onlinepubs/009695399/utilities/rm.html}[rm]}
      utility (defaults to ["rm"] on Unix and ["del"] on Windows). *)

  val rmdir : string key
  (** [rmdir] is the
      {{:http://pubs.opengroup.org/onlinepubs/009695399/utilities/rmdir.html}
      [rmdir]}
      utility (defaults to ["rmdir"]). *)

  val mkdir : string key
  (** [mkdir] is the
      {{:http://pubs.opengroup.org/onlinepubs/009695399/utilities/mkdir.html}
      [mkdir]} (defaults to ["mkdir"]). *)

  val cat : string key
  (** [cat] is the
      {{:http://pubs.opengroup.org/onlinepubs/009695399/utilities/cat.html}
      [cat]} (defaults to ["cat"] on Unix and ["type"] on Windows). *)

  val make : string key
  (** [make] is the
      {{:http://pubs.opengroup.org/onlinepubs/009695399/utilities/make.html}
      [make]} utility (defaults to ["make"]). {b FIXME} remove that. *)

  val docs_system_utilities : string
  (** [docs_system_utilities] is the name of the documentation section
      in which system utility keys are described. *)

  (** {1:builtin_converters Built-in value converters}  *)

  val bool : bool converter
  (** [bool] converts values with [bool_of_string].  *)

  val int : int converter
  (** [int] converts values with [int_of_string]. *)

  val string : string converter
  (** [string] converts values with the indentity function. *)

  val path : Path.t converter
  (** [path] converts value with {!Path.of_string}. *)

  val abs_path : Path.abs converter
  (** [path] converts value with {!Path.of_string}. *)

  val rel_path : Path.rel converter
  (** [path] converts value with {!Path.of_string}. *)

  val enum : (string * 'a) list -> 'a converter
  (** [enum l] converts values such that string names in l map to
      the corresponding value of type ['a]. *)

  val version : (int * int * int * string option) converter
  (** [version] converts values of the form
      [[v|V]major.minor[.patch][(-|+).*]]. *)

end

(** Command execution contexts.

    Execution contexts define an indirect addressing mechanism used to
    inject {{!Args}arguments} on the command lines of {{!Acmd}action
    commands}.

    The exact context associated to a command execution depends on the
    {{!type:part}part}, the action (via the [ctx] argument of
    {!Action.v}), and the {{!command}name} of the command being
    executed. *)
module Ctx : sig

  (** {1:elements Context elements} *)

  type tag = [ `Tag of string ]
  (** The type for user defined tags. *)

  type language = [ `OCaml | `C | `Js | `Lang of string ]
  (** The type for informing about the broad type of products being
      handled by an {{!Action}action}.
      {ul
      {- [`OCaml] working on OCaml related products.}
      {- [`C] working on C releated products.}
      {- [`Js] working on JavaScript related products.}
      {- [`Other l] working on language [l] related products.}} *)

  type build_phase = [ `Gen | `Dep | `Pp | `Compile
                     | `Archive of [ `Static | `Shared ]
                     | `Link | `Doc ]
  (** The type for informing about build phases.
      {ul
      {- [`Gen] source or data is being generated.}
      {- [`Dep] source is analyzed for dependencies.}
      {- [`Pp] source is pre-processed.}
      {- [`Compile] source is compiled.}
      {- [`Archive] compilation products are being archived.}
      {- [`Link] compilation products are being linked into an executable.}
      {- [`Doc] documentation is being generated. FIXME}} *)

  type source = [ `Src of Path.ext ]
  (** The type for informing about source products. *)

  type target = [ `Target of [`Src | `Byte | `Native | `Js | `Other of string ]]
  (** The type for informing about compilation targets.
      {ul
      {- [`Src] working on source code generation.}
      {- [`Byte] working on byte code generation.}
      {- [`Native] working on native code generation.}
      {- [`Js] working on JavaScript code generation.}
      {- [`Other o] working on other kind of generation.}} *)

  type cmd = [ `Cmd of string Conf.key | `Cmd_static of string ]
  (** The type for informing about the command being executed.
      {ul
      {- [`Cmd k], the command denoted by [k] is being executed.}
      {- [`Cmd n], the command statically named [n] is being executed.}} *)

  type part_usage = [ `Build | `Dev | `Doc | `Other of string
                    | `Outcome | `Test ]
  (** The type for {{!type:Part.usage}part usages}. *)

  type part_kind = [ `Base | `Bin | `Dir | `Doc | `Lib | `Pkg | `Run | `Unit ]
  (** The type for {{!type:Part.kind}part kinds}. *)

  type part = [ `Part of [part_usage | part_kind | `Name of string ] ]
  (** The type for informing about a part. Its name,
      {{!type:Part.kind}kind} and {{!type:Part.usage}usages}. *)

  type elt = [ tag | language | build_phase | source | target | cmd
             | part ]
  (** The type for context elements. *)

  val pp_elt : Format.formatter -> elt -> unit
  (** [pp_elt ppf e] prints an unspecified representation of [e] on [ppf]. *)

  (** {1:context Contexts} *)

  type t
  (** The type for contexts. A context is a set of context elements. *)

  val v : elt list -> t
  (** [v els] is the context with elements [els]. *)

  val matches : t -> t -> bool
  (** [matches c c'] is [true] if context [c] matches [c'], that is
      if [c] is a subset of [c']. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf c] prints an unspecified representation of [c] on [ppf]. *)

  include Set.S with type elt := elt
                 and type t := t
end

(** Command argument bundles.

    Argument bundles are conditional bindings from {{!Ctx}execution contexts}
    to lists of command arguments. They provide a simply mechanism
    to allow the end user to inject flags on the command lines of
    {{!Action}actions}.

    See {!basics} and {!propagation} for more details. *)
module Args : sig

  (** {1:argument_bundles Argument bundles} *)

  type t
  (** The type for argument bundles. *)

  val v : ?exists:bool Conf.value -> Ctx.t -> string list Conf.value -> t
  (** [v ~exists ctx args] is the bundle that binds [ctx] to [args]
      whenever [exists] evaluates to [true] (defaults to {!Conf.true_}). *)

  val vc : ?exists:bool Conf.value -> Ctx.t -> string list -> t
  (** [vc ~exists ctx args] is [v ~exists ctx (Conf.const args)]. *)

  val empty : t
  (** [empty] is the empty bundle. *)

  val is_empty : t -> bool
  (** [is_empty a] is [true] if [a] is empty. *)

  val append : t -> t -> t
  (** [append a a'] is the bundle that has the bindings of [a] and
      [a'].  If both [a] and [a'] have bindings for the same context,
      they are preserved each with their own condition of existence
      and it is guaranteed that the binding arguments of [a] will
      appear before those of [a'] on the command line. *)

  val (@@@) : t -> t -> t
  (** [a @@@ a'] is [append a a']. *)

  val concat : t list -> t
  (** [concat args] is [List.fold_left append empty args] *)

  (** {1:built_in Built-in argument bundles} *)

  val linkall : t
  (** [linkall] is the [-linkall] flag in the right [`OCaml] contexts. *)

  val thread : t
  (** [thread] is the [-thread] flag in the right [`OCaml] contexts. *)

  val vmthread : t
  (** [vmthread] is the [-vmthread] flag in the right [`OCaml] contexts. *)

  val cclib : string list -> t
  (** The [-cclib x] args. FIXME *)

  val ccopt : string list -> t
  (** The [-ccopt x] args. FIXME *)

  val stub : string -> t
  (** [stub s] adds {i -cclib -l[s] -dllib -l[s]} to the bytecode
      linking options and {i -cclib -l[s]} to the native linking
      options. FIXME *)

  (** {1:basics Argument bundles basics}

    Argument bundles allow to tweak actions by prepending additional
    arguments to their command invocations. For example the
    following bundle:
{[
let ocaml_debug =
  let debug ctx = Args.vc ~exists:Conf.debug ctx ["-g"] in
  Args.(debug [`OCaml; `Compile] @@@ debug [`OCaml; `Link])
]}
    used with a part will, whenever the configuration value of
    {!Conf.debug} evaluates to [true], prepend the option [-g] to any
    command of its actions that operates in a context that contains
    the [`OCaml] and [`Compile] or [`Link] elements. Note, you don't
    need the above definition, assemblage's OCaml built-in actions
    know how to handle the {!Conf.debug} key for you.

    Given an argument bundle and a command to execute, every binding
    in the bundle is considered, if the context of the binding
    {{!Ctx.matches}matches} the context of the command and the binding
    exists, the arguments of the binding are prepended to the command
    invocation.

    {b Warning.} Do not rely on the order of context matches for your
    command lines to be valid. The argument bundle mechanism is a
    rough end user build action tweaking mechanism that is mostly
    useful for injecting command line {e options}, not {e positional}
    arguments. The final bundle given to actions is the concatenation
    of many bundle sources (project, parts, action implementation)
    that may be applied in arbitrary order.

    {1:propagation Argument bundle propagation model}

    Which argument bundles are applied to an action (and its commands):
    {ol
    {- Each part has a user defined argument bundle, this bundle
       is automatically applied to any of its actions.}
    {- Each part has needs. In
       these needs there are parts that {e integrated} and other
       that are {e consulted}. When a part [i] is integrated by
       a part [p] its build
       actions are integrated into [p]. In that
       case these actions have both the argument bundle of the
       integrated part [i] and that of the integrating part [p].}
    {- Finally the {{!Project.v}project's argument} bundle is added to any
       action.}} *)
end

(** Action commands.

    An action command {e represents} the invocation of a program and
    the possible redirection of its standard file descriptors. Lists
    of action commands are the building block of {{!Action}actions}.

    Program executables are represented by the {!type:cmd}
    type. Values of this type can be created either with a
    configuration key or directly with a static string. If
    you are defining build actions you should use a configuration key
    and {!val:cmd} as it ensures the program can be redefined at
    configuration time thus making the build system more portable for
    end users.

    The module also provides a few system utility
    {{!portable_invocations}invocations} that enforce portable
    behaviour. *)
module Acmd : sig

  (** {1 Action commands} *)

  type cmd
  (** The type for program executables. *)

  val cmd : string Conf.key -> cmd Conf.value
  (** [cmd name] is a program whose name is the value of [name]. *)

  val static : string -> cmd
  (** [static name] is a program whose name is [name].

      {b Important.} For build commands, {!cmd} should be used.
      {!static} is best used for defining test {{!Run}runs} and other
      convenience development runs. *)

  type t
  (** The type for command runs. *)

  val v : ?stdin:Path.t -> ?stdout:Path.t -> ?stderr:Path.t -> cmd ->
    string list -> t
  (** [v cmd args] represents the execution of program [cmd] with
      argument [args] and standard file descriptors redirected to
      [stdin], [stdout], and [stderr] (if specified). *)

  (** Action command argument combinators.

      A few convience function to help in the definition
      of command arguments.

      {b FIXME.} Not really happy about this module, its name
      and contents. *)
  module Args : sig

    (** {1 Combinators} *)

    val add : 'a -> 'a list -> 'a list
    (** [add v l] is [v :: l]. *)

    val adds : 'a list -> 'a list -> 'a list
    (** [adds l l'] adds [l] in front of [l']. *)

    val add_if : bool -> 'a -> 'a list -> 'a list
    (** [add_if c v l] is [add v l] if [c] is true and [l] otherwise. *)

    val adds_if : bool -> 'a list -> 'a list -> 'a list
    (** [adds_if c l l'] if [adds l l'] if [c] is true and [l'] otherwise. *)

    val fadd_if : bool -> ('b -> 'a) -> 'b -> 'a list -> 'a list
    (** [add_if c f v l] is [add (f v) l] if [c] is true and [l] otherwise. *)

    val fadds_if : bool -> ('b -> 'a list) -> 'b -> 'a list -> 'a list
    (** [fadds_if c (f v) l] is [adds (f v) l] if [c] is true and [l]
        otherwise. *)

    val path_arg : ?opt:string -> Path.t -> string list -> string list
    (** [path_arg p l] is [Path.to_string p :: l]. If [opt] is present
        it is prepended in front of the path. *)

    val path_args : ?opt:string ->  Path.t list -> string list -> string list
    (** [path_args ?opt ps l] is like {!path_arg} but for each element
        of [ps]. If [opt] is present it is prepended in front of each path. *)

    val path : Path.t -> ext:Path.ext -> Path.t
    (** [path ext] is {!Path.change_ext}[ path ext]. *)
  end

  (** {1:portable_invocations Portable system utility invocations}

      Rather than using {{!Conf.system_utility_keys}system utility
      configuration keys} directly you should use the following functions,
      they will enforce portable behaviour.

      {b Note.} Function arguments could support more labelling but
      this doesn't blend well with {!Conf.app}. *)

    val dev_null : Path.t Conf.value
    (** [dev_null] is a file that discards all writes. *)

    val cd : (Path.t -> t) Conf.value
    (** [cd] has a command [exec dir] to change directory to [dir]. *)

    val ln : (Path.t -> Path.t -> t) Conf.value
    (** [ln] has a command [exec src dst] to link symbolically file [src] to
        [dst].

        {b Warning.} On Windows this is a copy. *)

    val ln_rel : (Path.t -> Path.t -> t) Conf.value
    (** [ln_rel] has a command [exec src dst] to link symbolically file
        [src] to [dst]. FIXME if [src] and [dst] are relative this links
        [src] to [dst] as seen from the empty relative directory (is
        that understandable ? is that really needed ? *)

    val cp : (Path.t -> Path.t -> t) Conf.value
    (** [cp] has a command [exec src dst] to copy file [src] to [dst]. *)

    val mv : (Path.t -> Path.t -> t) Conf.value
    (** [mv] has a command [exec src dst] to move path [src] to [dst]. *)

    val rm_files : (?f:bool -> Path.t list -> t) Conf.value
    (** [rm_files] has a command [exec ~f paths] to remove the {e file}
        paths [paths]. If [f] is [true] files are removed regardless of
        permissions (defaults to [false]). [paths] elements must be files,
        for directories, see {!rm_dirs}. *)

    val rm_dirs : (?f:bool -> ?r:bool -> Path.t list -> t) Conf.value
    (** [rm_dirs] has a command [exec ~f ~r paths] to remove the {e
        directory} paths [paths]. If [f] is [true] directories are
        removed regardless of permissions (defaults to [false]).  If [r]
        is [true] removes the file hierarchies rooted at the elements of
        [paths]. Note that [paths] must be directories, for removing
        files, see {!rm_files}. *)

    val mkdir : (Path.t -> t) Conf.value
    (** [mkdir] has a command [exec d] to create the directory [p].
        Intermediate directories are created as required ([mkdir -p] in
        Unix parlance). *)

    val stamp : (Path.t -> string -> t) Conf.value
    (** [stamp] has a command [execs f content] that writes [content] to [f]. *)
end

(** Build action.

    An {e action} determines how to output a list of build products
    from a list of existing input products using a sequence of
    {{!Acmd}commands}.

    Given a notion of product identity (e.g. hash, timestamp, etc.) an
    action defines a pure function from its input to its outputs, it
    is assumed that identical inputs yield identical outputs.

    In order to be productive it is important to understand the following
    terminology. With respect to a project:
    {ul
    {- A {e product} is any existing file in the {!Conf.root_dir}
       hierarchy.}
    {- A {e source product} is a product for which there exists no
       action to create it; typically the source code you write, a
       product from your brain.}
    {- A {e build product} is a product that is output from a build action.}
    {- An {e input product} is a product that is used as input
       to a build action, this can be a source product or a build product.}
    {- An {e output product} is synonym with build product}}

    A build driver is an assemblage driver that given a specification
    of the project's products yielded by a configuration ensures that
    all the products exists and are up-to-date according to the project's
    actions.

    {b FIXME.} The following need to clarified
    {ul
    {- Empty inputs, empty outputs, empty cmds or any combination there of.}
    {- {!Part.file} uses empty outputs and empty inputs. This allows
       {!Dir} which works on products regardless (i.e., input and
       output) to be made aware of their existence.}
    {- {!Run} use {e build} actions but they are not build per se as we
       run the command inconditionally.}
    {- The model is too weak. How need a way to account for
       [ocamldep], [gcc -MD -MP], etc.}} *)
module Action : sig

  (** {1 Build actions} *)

  type t
  (** The type for build actions. *)

  val v : ?log:string -> ?ctx:Ctx.t -> ?inputs:Path.t list ->
    ?outputs:Path.t list -> Acmd.t list -> t
  (** [v ctx inputs outputs cmds] is the action that given the
      existence of [inputs] creates [outputs] using the sequence of
      command [cmds]. [ctx] defaults to {!Ctx.empty}, [inputs] and
      [outputs] to the empty list. [log] is a high-level logging
      string about the action that may be used by drivers when the
      action is invoked.

      {b Warning.} To ensure determinism and parallelism correctness [cmds]
      must ensure that it only reads from the [inputs] and solely writes to
      [outputs]. *)

  val ctx : t -> Ctx.t
  (** [ctx a] is [a]'s context. *)

  val inputs : t -> Path.t list
  (** [inputs a] is the list of products input by [a]'s action. *)

  val outputs : t -> Path.t list
  (** [outputs a] is the list of products output by [a]'s action. *)

  val cmds : t -> Acmd.t list
  (** [cmds a] is [a]'s commands to generate outputs from the inputs. *)

  val log : t -> string option
  (** [log a] is [a]'s high-level logging string (if any). *)

  val products : t -> Path.t list
  (** [products a] is [inputs a @ outputs a] but tail recursive. *)

  (** {1 Built-in actions} *)

  val symlink : (Path.t -> Path.t -> t) Conf.value
  (** [symlink] has an action [action src dst] that links [src] to [dst] using
      {!Acmd.ln_rel}. *)

  (** {1 Action lists} *)

  val list_inputs : t list -> Path.t list
  (** [list_inputs l] is the list of inputs of the actions in [l].
      Ordered but may contain duplicates. *)

  val list_outputs : t list -> Path.t list
  (** [list_outputs l] is the list of outputs of the actions in [l].
      Ordered but may contain duplicates. *)

  val list_products : t list -> Path.t list
  (** [list_products l] is [list_inputs l @ list_outputs l]
      but tail-recursive. *)

(* FIXME this is outdated. It's unclear whether we want to expose that.
   It could be nice for users who want to develop their own OCaml
   parts, but OTOH if people start to do so it means that we are
   failing at providing the right things.  Having
   that would be one more thing to maintain and design with care.

  (** Actions for handling OCaml build products.

      All the actions have appropriate support for the {!Conf.debug},
      {!Conf.profile}, {!Conf.warn_error}, {!Conf.ocaml_annot}
      configuration keys.

      FIXME should we document contexts ? This is anyways going
      to be hidden behind parts. *)
  module OCaml : sig

    (** {1 Types} *)

    type includes = Path.t list Conf.value
    (** The type for lists of include directories. A configuration value
        holding a list of directory paths. *)

    type name = Path.t Conf.value
    (** The type for build product names. A name defines a build
        location through its {{!Path.dirname}dirname} and a name through its
        {e suffix-less} {{!Path.basename}basename}. *)

    (** {1 Preprocessing} *)

    val compile_src_ast : [`Ml | `Mli ] -> src:Path.t -> unit -> t
    (** [compile_src_ast kind src ()] treats [src] of the given [ml] type
        and builds its AST. FIXME needs arguments for
        applying pp on the way ? *)

    (** {1 Compiling} *)

    val compile_mli : incs:includes -> src:Path.t -> unit -> t
    (** [compile_mli ~incs ~src ()] compiles the mli file [src] (which can
        be an AST, see {!compile_src_ast}) with includes [incs]. *)

    val compile_ml_byte : has_mli:bool Conf.value -> incs:includes ->
      src:Path.t -> unit -> t
    (** [compile_ml_byte ~has_mli ~incs ~src ()] compiles to byte code the ml
        file [src] (which can be an AST, see {!compile_src_ast}) with
        includes [incs]. [has_mli] indicates whether the source has a
        corresponding mli file. *)

    val compile_ml_native : has_mli:bool Conf.value ->
      incs:includes -> src:Path.t -> unit -> t
    (** [compile_ml_native ~has_mli ~incs ~src ()] is like {!compile_ml_byte}
        but compiles to native code. *)

    val compile_c : src:Path.t -> unit -> t
    (** [compile_c src ()] compiles the C [src] to native code through
        the OCaml compiler. *)

    (** {1 Archiving} *)

    val archive_byte : cmos:Path.t -> name:name -> unit -> t
    (** [archive_byte cmos name ()] archives the byte code files [cmos]
        to a byte code archive (cma) named by [name]. *)

    val archive_native : cmx_s:Path.t -> name:name -> unit -> t
    (** [archive_native cmx_s name ()] archives the native code files [cmx_s]
        to a native code archive (cmxa) named by [name]. *)

    val archive_shared : cmx_s:Path.t -> name:name -> unit -> t
    (** [archive_shared cmx_s name ()] archives the native code files [cmx_s]
        to a native code shared archive (cmxs) named by [name]. *)

    val archive_c : objs:Path.t -> name:name -> unit -> t
    (** [archive_c cmx_s name ()] archives the C object files [objs]
        to a C archive and shared archived (a, so) named by [name]. *)

    (** {1 Linking} *)

  end
*)
end

(** {1:parts Parts} *)

type part_kind = [ `Base | `Unit | `Lib | `Bin | `Pkg | `Run | `Doc | `Dir ]
(** The type for part kinds. *)

type +'a part constraint 'a = [< part_kind ]
(** The type for project parts. *)

(** Parts, part sets and maps.

    Parts are the logical build units that define a project, they
    define the project's outcomes for every possible configuration.

    A {e part} defines a coherent set of build actions. There are
    different {{!type:part_kind}kinds} of part, each which specific
    metadata that it uses to define its action. Parts may also refer to,
    or integrate, other parts to define their actions.

    A part can also be seen as defining the outputs of its build
    actions. These build products depend on the configuration.

    In addition to part specific metadata, each part has the following
    attributes that the user specifies:
    {ul
    {- A {{!name}name} used to identify the part; note that it may not
       be unique. The name is often used as metadata itself.
       For example it defines the library name for {{!Lib}library parts} or
       the name of a {{!Unit} compilation unit}, etc.}
    {- A {{!type:kind}kind} that identifies the kind of metadata the part
       uses and the build action it produces.}
    {- A {{!type:usage}usage}. This is a broad usage label to classify
       parts. It can be used by drivers to derive bureaucratic
       information about a project. For example if some of parts
       are only used for developing the project they should have
       a [`Dev] usage; this will allow drivers to understand
       that their dependencies are only needed for development.}
    {- {{!needs}Needs.} This is a list of parts used by the part to
       define its actions. The way a part uses its needs depends
       on its kind, consult their specific documentation.}
    {- An {{!exists}exists} configuration boolean that indicates
       whether a part exists in a given configuration. If the
       part doesn't exist it doesn't define any build action. For example
       the existence of a library may depends on the presence of an
       optional package.}
    {- An {{!args}argument bundle} applied to its actions.
       See {!Args.basics} for more information about argument
       bundles.}}

    Given all this information a part will give you
    {{!actions}build actions} whose outputs define
    a part's build products. *)
module Part : sig

  (** {1 Kinds} *)

  type kind = [ `Base | `Unit | `Lib | `Bin | `Pkg | `Run | `Doc | `Dir ]
  (** The type for part kinds. See the corresponding individual modules
      for more information. *)

  val pp_kind : Format.formatter -> kind -> unit
  (** [pp_kind ppf k] prints an unspecified represetation of [k] on
      [ppf]. *)

  (** {1 Usage} *)

  type usage = [ `Build | `Dev | `Doc | `Other of string | `Outcome | `Test ]
  (** The type for part usage.
      {ul
      {- [`Outcome], the part is an outcome of the project.}
      {- [`Build], the part is used for building the project.}
      {- [`Dev], the part is used when developing the project.}
      {- [`Test], the part is used to test the project.}
      {- [`Doc], the part is used for building the project
         documentation.}
      {- [`Other u], the part is used according to a user-defined semantics.}}
   *)

  val pp_usage : Format.formatter -> usage -> unit
  (** [pp_usage ppf u] prints an unspecified representation of [u]
      on [ppf]. *)

  (** {1 Metadata} *)

  type meta
  (** The type for part metadata *)

  val meta_key : unit -> ('a -> meta) * (meta -> 'a option)
  (** [meta_key ()] pairs a metadata injector and projector. *)

  val meta_nil : meta
  (** [meta_nil] is metadata that cannot be accessed. *)

  (** {1 Parts} *)

  type +'a t = 'a part constraint 'a = [< kind]
  (** The type for parts. *)

  val v : ?usage:usage -> ?exists:bool Conf.value -> ?args:Args.t ->
    ?meta:meta -> ?needs:'a part list -> ?root:Path.rel Conf.value ->
    ?actions:(kind part -> Action.t list Conf.value) ->
    ?check:(kind part -> bool Conf.value) -> string -> [> `Base] part
  (** [v ?usage ?exists ?meta ?needs ?args ?actions ?check name] defines
      a base part named [name].
      {ul
      {- [usage] is the part's usage, default to
         [`Outcome].}
      {- [exists] is a condition for the part to exist, defaults to
         {!Conf.true_}.}
      {- [args] is an argument bundle added to the actions of the part,
         defaults to {!Args.empty}.}
      {- [meta] is the part's metadata, defaults to {!meta_nil}.}
      {- [needs] are the parts that are needed by the part, defaults to [[]],
         the given list is {!uniq}ified. }
      {- [root] is a build root for the part, usually best left unspecified.
         Note that this value should not be used as a constant for defining
         [actions] as it may be redefined by {{!integrate}integrating} parts.
         Use {!root}[ p] instead.}
      {- [actions] is the function that defines the actions associated to
         the part. The function is given the part itself which allows
         to access its metadata. Action outputs should be generated
         in the directory returned by {!root}[ p], {b not} the [root]
         argument given to the constructor. This will ensure that the
         part adapts correctly if it is integrated by another.}
      {- [check] is a diagnostic function that should {!Log} information
         about potential problems with the part and return [true] if there
         is no problem.}} *)

  val kind : 'a part -> kind
  (** [kind p] is [p]'s kind. *)

  val name : 'a part -> string
  (** [name p] is [p]'s name. *)

  val usage : 'a part -> usage
  (** [usage p] is [p]'s usage. *)

  val exists : 'a part -> bool Conf.value
  (** [exists p] determines if [p] exists. *)

  val meta : 'a part -> meta
  (** [meta p] is [p]'s metadata. *)

  val get_meta : (meta -> 'a option) -> 'b t -> 'a
  (** [get_meta proj p] uses [proj] on [p]'s metadata.

      @raise Invalid_argument if [proj] returns [None]. *)

  val root : 'a part -> Path.rel Conf.value
  (** [root p] is [p]'s root build directory expressed relative to
      {!Conf.root_dir}. Most of the time the actions of a part will
      output their build products in this direcory. This directory may
      change when a part is {{!integrate}integrated} in another. *)

  val root_path : 'a part -> Path.t Conf.value
  (** [root_path] is like {!root} but as a generic path. *)

  val needs : 'a part -> kind t list
  (** [needs p] is the (uniqified) list of parts needed by [p] to
      define itself. *)

  val actions : 'a part -> Action.t list Conf.value
  (** [actions p] are the actions to build part [p]. If
      {!exists}[ p] evaluates to [false] this evaluates to
      the empty list. *)

  val check : 'a part -> bool Conf.value
  (** [check p] logs information about potential problems with [p]
      and returns [true] if there is no such problem. *)

  val id : 'a part -> int
  (** [id p] is a unique id for the part. *)

  val equal : 'a part -> 'b part -> bool
  (** [equal p p'] is [(id p) = (id p')]. *)

  val compare : 'a part -> 'b part -> int
  (** [compare p p'] is [compare (id p) (id p')]. *)

  val redefine : ?check:(kind part -> bool Conf.value) ->
    ?actions:(kind part -> Action.t list Conf.value) -> 'a part -> 'a part
  (** [redefine check actions p] is [p] with check function [check]
      and actions function [action] (both defaults to [p]'s one if
      unspecified).

      {b Warning.} As far as [Assemblage] is concerned
      this is equivalent to Obj.magic. *)

  (** {1 File part} *)

  val file : ?usage:usage -> ?exists:bool Conf.value -> Path.t -> [> `Base] part
  (** [file p] is a part with a noop action that inputs [p]. *)

  (** {1 Part integration} *)

  val integrate : ?add_need:(kind part -> bool) -> 'a part -> 'b part -> 'a part
  (** [integrate ?add_need i p] is [i] as integrated in [p]. This is
      [i] except its root and usage now match those of [p] and the
      needs of [p] that returned [true] on [add_need] were added to the
      new part. [add_need] defaults to [fun _ -> false]. The new part
      will adapt to the new parameters by recomputing actions using
      the part's action definition function. *)

  (** {1 Coercions} *)

  val coerce : ([< kind] as 'b) -> 'a part -> 'b part
  (** [coerce k p] coerces [p] to kind [k],

      @raise Invalid_argument if [p]'s kind is not [k]. *)

  val coerce_if : ([< kind] as 'b) -> 'a part -> 'b part option
  (** [coerce_if k p] is [Some] if [p] is of kind [k] and
      [None] otherwise. *)

  (** {1 Part lists} *)

  val list_actions : kind part list -> Action.t list Conf.value
  (** [list_actions ps] is the list of actions of the parts [ps].
      The list order is preserved. *)

  val list_uniquify : kind part list -> kind part list
  (** [list_uniquify ps] is [ps] with duplicates as determined by {!equal}
      removed. The list order is preserved. *)

  val list_keep : ('a part -> bool) -> 'a part list -> 'a part list
  (** [list_keep pred ps] is the elements of [ps] that satisfy [pred].
      The list order is preserved. *)

  val list_keep_map : ('a part -> 'b option) -> 'a part list -> 'b list
  (** [list_keep_map f ps] is the elements of [ps] for which [f] returns
      [Some] value. The list order is according to [ps]. *)

  val list_keep_kind : ([< kind] as 'b) -> 'a part list -> 'b part list
  (** [list_keep_kind k ps] is the elements of [ps] that have kind [kind].
      The list order is preserved. *)

  val list_keep_kinds : kind list -> 'a part list -> 'a part list
  (** [list_keep_kinds ks] is the elements of [ps] that have one of the
      kind [ks]. The list order is preserved. *)

  val list_fold : ('a -> ([< kind] as 'b) part -> 'a) -> 'a -> 'b part list ->
    'a
  (** [list_fold f acc ps] is [List.fold_left f acc ps]. *)

  val list_fold_kind : ([< kind] as 'b) -> ('a -> 'b part -> 'a) -> 'a ->
    'c part list -> 'a
  (** [list_fold kind f acc ps] is like {!list_fold} but folds only
      over the parts whose kind is [kind]. *)

  val list_fold_rec : ('a -> kind part -> 'a) -> 'a -> kind part list -> 'a
  (** [fold_rec f acc l] folds over the parts of [l] and their needs
      recusively, in depth first pre-order. If a part appears twice in
      the traversal is it only folded over the first time. *)

  val list_fold_kind_rec : ([< kind] as 'b) -> ('a -> 'b t -> 'a) -> 'a ->
    kind t list -> 'a
  (** [list_fold_kind_rec] is the same traversal as {!list_fold_rec}
      but only parts whose kind is [kind] are folded over. *)

  (** {1 Part sets and maps} *)

  module Set : sig
    include Set.S with type elt = kind part
    val of_list : kind part list -> t
  end

  module Map : sig
    include Map.S with type key = kind part
    val dom : 'a t -> Set.t
    (** [dom m] is the domain of [m]. *)
  end
end

(** Compilation unit part.

    Defines a compilation unit's build products in
    a build directory defined by an enclosing {!Lib} or {!Bin} part. *)
module Unit : sig

  (** {1 Metadata} *)

  type ocaml_interface = [ `Normal | `Opaque | `Hidden ]
  (** The type for OCaml compilation unit interfaces.

      Adds additional information about how the interface of a
      compilation unit should be treated.
      {ul
      {- [`Normal] is for regular compilation units.}
      {- [`Opaque] is for compilation units with cross-module optimizations
         disabled.}
      {- [`Hidden] is for regular compilation units whose
         [cmi] files are hidden (implies [`Opaque]).}} *)

  type ocaml_unit = [ `Mli | `Ml | `Both ]
  (** The type for OCaml compilation units. *)

  type c_unit = [ `H | `C | `Both ]
  (** The type for C Compilation units *)

  type kind = [ `OCaml of ocaml_unit * ocaml_interface | `C of c_unit | `Js ]
  (** The type for kinds of compilation units *)

  val pp_kind : Format.formatter -> kind -> unit
  (** [pp_kind ppf k] prints an unspecified representation of [k] on
      [ppf]. *)

  val kind : [< `Unit] part -> kind
  (** [kind u] is the kind of [u]. *)

  val dir : [< `Unit] part -> Path.t Conf.value
  (** [dir u] is the directory where the unit [u] is located. *)

  val ocaml : 'a part -> [> `Unit ] part option
  (** [ocaml p] is [Some p] iff [p] is an OCaml compilation unit. *)

  val c : 'a part -> [> `Unit ] part option
  (** [c p] is [Some p] iff [p] is a C compilation unit. *)

  val js : 'a part -> [> `Unit ] part option
  (** [js p] is [Some p] iff [p] is a JavaScript compilation unit. *)

  (** {1 Units} *)

  val v : ?usage:Part.usage -> ?exists:bool Conf.value -> ?args:Args.t ->
    ?needs:[< `Lib | `Pkg] part list -> ?dir:Path.t Conf.value -> string ->
    kind -> [> `Unit] part
  (** [v ~needs ~dir name kind] is a compilation unit named [name],
      the name of the file without the suffix located in directory
      [dir] (defaults to {!root}) of kind [kind]. [needs] indicate the
      project libraries and packages that are needed to compile the
      unit. The [args] bundle is used by the unit's actions according
      to context. *)
end

(** Library part.

    Defines a library's build products by gathering a set of
    {{!Unit}unit parts}.  *)
module Lib : sig

  (** {1 Metadata} *)

  type kind = [ `OCaml | `OCaml_pp | `C ]
  (** The type for kinds of libraries.
      {ul
      {- [`OCaml] is an OCaml library.}
      {- [`OCaml_pp] is an OCaml pre-processing library}
      {- [`C] is a C library.}} *)

  val pp_kind : Format.formatter -> kind -> unit
  (** [pp_kind ppf k] prints an unspecified representation of [k] on
      [ppf]. *)

  val kind : [< `Lib] part -> kind
  (** [kind p] is the kind of [p]. *)

  val byte : [< `Lib] part -> bool
  (** [byte p] is [true] if [p] can compile to OCaml byte code. *)

  val native : [< `Lib] part -> bool
  (** [native p] is [true] if [p] can compile to native code. *)

  val native_dynlink : [< `Lib] part -> bool
  (** [native p] is [true] if [p] can compile to native dynamically
      linked code. *)

  val ocaml : 'a part -> [> `Lib] part option
  (** [ocaml p] is [Some p] iff [p] is an [`OCaml] library. *)

  val ocaml_pp : 'a part -> [> `Lib] part option
  (** [ocaml_pp p] is [Some p] iff [p] is [`OCaml_pp] library. *)

  val c : 'a part -> [> `Lib] part option
  (** [c p] is [Some p] iff [p] is a [`C] library. *)

  (**  {1 Libraries} *)

  val v : ?usage:Part.usage -> ?exists:bool Conf.value -> ?args:Args.t ->
    ?byte:bool -> ?native:bool -> ?native_dynlink:bool ->
    string -> kind -> [< `Unit | `Pkg | `Lib] part list -> [> `Lib] part
  (** [v ?byte ?native ?native_dynlink name kind needs] is a library
      named [name] of the given [kind]. [needs] has the compilation
      units [us] that define the libary, they are integrated. The
      package and libraries that are in [needs] are automatically
      added to [us] needs.

      The library's {e ability} to compile to different targets is
      specified by the arguments [?byte], [?native] and
      [?native_dynlink] whose defaults respectively depend on [kind]
      as follows.
      {ul
      {- [`OCaml], [true], [true], [true]}
      {- [`OCaml_pp], [true], [false], [false]}
      {- [`C], [false] (not applicable), [true], [true]}}
      Whether the outputs associated to a compilation target are
      concretly build depends on the configuration keys
      {!Conf.ocaml_byte}, {!Conf.ocaml_native},
      {!Conf.ocaml_native_dynlink} and {!Conf.c_dynlink}. *)
end

(** Binary executable part.

    Defines a program executable's build products by gathering a set of
    {{!Unit}unit parts}. *)
module Bin : sig

  (** {1 Metadata} *)

  type kind = [ `OCaml | `OCaml_toplevel | `C ]
  (** The type for kinds of binaries.
      {ul
      {- [`OCaml] is a OCaml executable.}
      {- [`OCaml_toplevel] is an OCaml toplevel.}
      {- [`C] is a C executable.}} *)

  val pp_kind : Format.formatter -> kind -> unit
  (** [pp_kind ppf k] prints an unspecified representation of [k] on
      [ppf]. *)

  val kind : [< `Bin] part -> kind
  (** [kind p] is [p]'s kind. *)

  val byte : [< `Bin] part -> bool
  (** [byte p] is [true] if [p] can compile to OCaml byte code. *)

  val native : [< `Bin] part -> bool
  (** [native p] is [true] if [p] can compile to native code. *)

  val js : [< `Bin] part -> bool
  (** [js p] is [true] if [p] can compile to JavaScript code. *)

  val ocaml : 'a part -> [> `Bin] part option
  (** [ocaml p] is [Some p] iff [p] is an [`OCaml] binary. *)

  val ocaml_toplevel : 'a part -> [> `Bin] part option
  (** [ocaml_toplevel p] is [Some p] iff [p] is an [`OCaml_toplevel] binary. *)

  val c : 'a part -> [> `Bin] part option
  (** [c p] is [Some p] iff [p] is an [`C] binary. *)

  (** {1 Binaries} *)

  val v :
    ?usage:Part.usage ->
    ?exists:bool Conf.value ->
    ?args:Args.t ->
    ?byte:bool -> ?native:bool -> ?js:bool ->
    string -> kind -> [< `Unit | `Lib | `Pkg ] part list -> [> `Bin] part
  (** [v ?byte ?native ?js name kind needs] is a binary named [name]
      of the given [kind]. [needs] has the compilation units [us] that
      define the binary, they are integrated. The package and
      libraries that are in [needs] are added to [us] needs and used
      at link time.

      The binary's {e ability} to compile to different targets is specified
      by the arguments [?byte], [?native] and [?js] whose defaults
      respectively depend on [kind] as follows.
      {ul
      {- [`OCaml], [true], [true], [false].}
      {- [`OCaml_toplevel], [true], [false], [false].}
      {- [`C], [false] (not applicable), [true], [false]}}
      Whether the outputs associated to a compilation target are
      concretly build depends on the configuration keys {!Conf.ocaml_byte},
      {!Conf.ocaml_native}, {!Conf.ocaml_js} and {!Conf.c_js}. *)

  val to_cmd : ?ext:Path.ext -> [< `Bin] part -> Acmd.cmd Conf.value
  (** [to_cmd ext bin] is an {{!type:Acmd.cmd}action command} for
      using the part's binary. [ext] specifies the binary to use if
      there are multiple of them; default favors native code over byte
      code if available and as per configuration.

      {b Warning.} When you use the resulting command in
      {{!Action}actions} make sure to specify it as an input product
      of the action using {!Bin.to_cmd_path} otherwise the binary may not
      exist when the action is executed. Also make sure to use the
      command in configurations in which [bin] exists, see
      {!exists}. For simple use cases {!Run.with_bin} and {!Run.bin}
      will take care of these details. *)

  val to_cmd_path : ?abs:bool -> ?ext:Path.ext -> [< `Bin] part ->
    Path.t Conf.value
  (** [to_cmd_path ext bin] is [to_cmd ext bin] as a path. If [abs] is
      true (default to [false]) returns an absolute path. *)

  val exists : ?ext:Path.ext -> [< `Bin] part -> bool Conf.value
  (** [exists ?ext bin] is [true] if the [to_cmd bin] command
      exists. *)

  (** {1 Binaries as product generators}  *)

  val gen : ?usage:Part.usage -> ?exists:bool Conf.value ->
    ?args:Args.t -> ?dir:Path.t Conf.value -> ?name:string ->
    ?ext:Path.ext -> ?stdin:Path.t Conf.value ->
    ?stdout:Path.t Conf.value -> ?stderr:Path.t Conf.value ->
    [< `Bin] Part.t -> string list Conf.value -> [> `Base] Part.t
  (** [gen ext bin args] is a part that uses {!to_cmd} to generate
      a product on [stdout].

      {b FIXME} This API is a proof of concept, it was quickly derived
      from {!Run.bin}, it's not designed. We need to devise sensitive
      things with [dir] and relative to what we express paths. Also
      want to specify the action's outputs explicitely.  See the
      gen-quine example. *)
end

(** Package part.

    Packages are named entities that provide action command arguments
    in certain {{!Ctx}contexts}. Some parts query and use these arguments
    when packages are specified in their needs. *)
module Pkg : sig

  (** {1 Metadata} *)

  type lookup = Ctx.t -> string list
  (** The type for lookups. Given a context the package answers
      with a list of command arguments. *)

  type kind = [ `OCamlfind
              | `Pkg_config
              | `Other of string * lookup Conf.value ]
  (** The type for package kinds.
      {ul
      {- [`OCamlfind] is for
         {{:http://projects.camlcity.org/projects/findlib.html}Findlib}
         packages.}
      {- [`Pkg_config] is for
         {{:http://pkg-config.freedesktop.org/}[pkg-config]} packages.}
      {- [`Other (n,lookup)] is for an other mecanism named [n] and
         that uses the function [lookup].}} *)

  val pp_kind : Format.formatter -> kind -> unit
  (** [pp_kind ppf k] prints an unspecified representation of [k] on
      [ppf]. *)

  val kind : [< `Pkg] part -> kind
  (** [kind p] is [p]'s package kind. *)

  val lookup : [< `Pkg] part -> lookup Conf.value
  (** [query p q] is [p]'s lookup function. *)

  val opt : [< `Pkg] part -> bool
  (** [opt p] is [true] if the package is optional. *)

  val ocamlfind : 'a part -> [> `Pkg] part option
  (** [ocamlfind p] is [Some p] iff [p] is an [`OCamlfind] package. *)

  val pkg_config : 'a part -> [> `Pkg] part option
  (** [pkg_config p] is [Some p] iff [p] is a [`Pkg_config] package. *)

  val other : 'a part -> [> `Pkg] part option
  (** [c p] is [Some p] iff [p] is a C package. *)

  (** {1 Packages} *)

  val v : ?usage:Part.usage -> ?exists:bool Conf.value ->
    ?opt:bool -> string -> kind -> [> `Pkg] part
  (** [v opt name kind] is a package named [name] of the given [kind].
      [name] is the name used for looking up the package system if
      [kind] is not [`Other]. [opt] should be [true] if the package is
      optional. *)

  val list_lookup : 'a part list -> lookup Conf.value
  (** [list_lookup ps] is a lookup function that looks up all
      package in [ps] and returns the aggregated result. The arguments
      are in order of package appearance. *)
end

(** Unit documentation set part.

    Defines a build product for the documentation set defined
    by a set of compilation units. *)
module Doc : sig

  (** {1 Metadata} *)

  type kind = [ `OCamldoc ]
  (** The type for unit documentation set generators. *)

  val pp_kind : Format.formatter -> kind -> unit
  (** [pp_kind ppf k] prints an unspecified representation of [k] on
      [ppf]. *)

  val kind : [< `Doc] part -> kind
  (** [kind p] is [p]'s kind. *)

  val ocamldoc : 'a part -> [> `Doc ] part option
  (** [ocamldoc p] is [Some p] iff [p] is an [`OCamldoc] documentation
      generator. *)

  (** {1 Unit filters}

      TODO we need something more general along the lines of
      {!Dir} parts. This will allow to cope with e.g. static files. *)

  val default : [< `Unit] part -> bool
  (** [default] is a part filter that selects only OCaml units whose
      {!Unit.ocaml_interface} is not [`Hidden]. *)

  val dev : [< `Unit] part -> bool
  (** [dev] is part filter that select any kind of OCaml unit. *)

  (** {1 Unit documentation sets} *)

  val v : ?usage:Part.usage -> ?exists:bool Conf.value -> ?args:Args.t ->
    ?keep:([< `Unit] part -> bool) -> string -> kind ->
    [< `Unit | `Lib | `Bin]  part list -> [> `Doc] part
  (** [v keep name kind needs] is a documentation set named [name] of
      the given [kind]. The units of the documentation set are those
      kept by [keep] and present in [needs] or part of the libraries
      and binaries listed in [needs]. [keep] defaults to {!dev} if
      [usage] is [`Dev] and {!default} otherwise. *)
end

(** Directory part.

    A directory part defines a clean directory in which a selection of
    part products (both source and build) can be gathered and
    renamed. Directory parts can be marked as being
    {!install}able; drivers can use this information to devise an
    installation procedure for your project's outcomes. *)
module Dir : sig

  (** {1 Metadata} *)

  type kind = [ `Lib | `Bin | `Sbin | `Toplevel | `Share | `Share_root
              | `Etc | `Doc | `Stublibs | `Man | `Other of Path.t ]
  (** The type for kinds of directories. They essentially correspond
      to the sections of an OPAM
      {{:http://opam.ocaml.org/doc/manual/dev-manual.html#sec25}install file}.
  *)

  val pp_kind : Format.formatter -> kind -> unit
  (** [pp_kind ppf k] prints an unspecified representation of [k] on
      [ppf]. *)

  val kind : [< `Dir] part -> kind
  (** [kind p] is [p]'s kind. *)

  val install : [< `Dir] part -> bool
  (** [kind p] is [true] if the directory contents is meant to be installed. *)

  (** {1 Directory specifiers}

      FIXME a few combinators to quickly devise spec values
      would be welcome, e.g. put a specific part in sub directory, etc.  *)

  type spec = Part.kind Part.t -> (Path.t * Path.rel option) list Conf.value
  (** The type for directory specifier.

      Given a part [p] it returns a list of tuples [(prod, path)],
      where [prod] is a product of [p] to add to the directory and
      [path] specifies under which name, relative to the directory, it
      should be added. If [path] is [None] and [prod] is in [p]'s
      build root directory, the path relative to this directory is used. *)

  val all : spec
  (** [all] is a specifier that keeps any product of any part. *)

  val all_output : spec
  (** [all_output] is a specifier that keeps any output product of any part. *)

  val all_input : spec
  (** [all_input] is a specifier that keeps any input product of any part. *)

  val file_exts : Path.ext list -> spec
  (** [file_exts exts] is a specifier that keeps, in any part,
      products that have an extension in [exts]. *)

  val bin : spec
  (** [bin] is {!all_output} except in the following cases:
      {ul
      {- For [`Bin] parts of kind [`OCaml] keeps only one of the
         byte and native code executable without its extension. If
         both are available favors native code over byte code.}
      {- For [`Bin] parts of kind [`C] keeps only the part's
         executable.}} *)

  val lib : spec
  (** [lib] is {!all_output} except in the following cases:
      {ul
      {- For [`Lib] parts of kind [`OCaml], keeps only the library's
         archives and the library's unit's [cmx], [cmi] and [cmti]
         files according to their
         {{!Unit.ocaml_interface}interface specification}.}
      {- For [`Lib] parts of kind [`C], keeps only the library's
         archives.}} *)

  val doc : spec
  (** [doc] is {!all} except in the following cases:
      {ul
      {- For [`Doc] parts of kind [`OCamldoc] it is {!any_output}.}} *)

  (** {1 Dir} *)

  val v : ?usage:Part.usage -> ?exists:bool Conf.value -> ?args:Args.t ->
    ?spec:spec -> ?install:bool -> kind ->
    [< `Base | `Bin | `Dir | `Doc | `Lib | `Unit ] part list ->
    [> `Dir ] part
  (** [v spec install kind needs] is a directory of type [kind] that
      gathers the products of existing [needs] as determined by
      the directory specifier [spec].

      If [install] is [true] the directory structure is meant to be
      installed at a root location defined by [kind]. [install]
      defaults to [true] except for kind [`Other].

      The name of the part is automatically derived from [kind]
      in the obvious way. For [`Other p] the name is the basename of
      [p].

      The default for [spec] depends in [kind] as follows:
      {ul
      {- [`Bin], {!bin} is used.}
      {- [`Lib], {!lib} is used.}
      {- [`Doc], {!doc} is used.}
      {- Otherwise, {!all} is used.}}

      {b Note.} Due to the way assemblage and ocamldoc work, trying
      to install a [`Doc] part will be disappointing. *)
end

(** Runs.

    A run is an {{!Action}action} whose commands are always run even if
    the inputs are up to date. Runs are typically used to run the
    project's test suites or other convenience development executions.

    Strictly speaking a run action is not part of the build system but
    it can input products of the build system to ensure they exist
    when the run is executed. The run part gives us an explicit name
    to invoke the run.

    {b Important.} Since runs always execute their action you should
    usually not use them for generating build products.


    {b FIXME} What's the semantics of outputs for a Run ? *)
module Run : sig

  (** {1 Runs} *)

  val v : ?usage:Part.usage -> ?exists:bool Conf.value -> ?args:Args.t ->
    ?dir:Path.t Conf.value -> string -> Action.t Conf.value -> [> `Run] part
  (** [v dir name action] is a run that executes the action
      [action]. If [dir] is specified an {!Acmd.cd} command to that
      directory is added in front of [action]'s commands. Otherwise
      the commands are executed from the {!Conf.root_dir} directory. *)

  val with_bin : ?usage:Part.usage -> ?exists:bool Conf.value ->
    ?args:Args.t -> ?dir:Path.t Conf.value -> ?name:string ->
    ?ext:Path.ext -> [< `Bin] part -> (Acmd.cmd -> Acmd.t list) Conf.value ->
    [> `Run] part
  (** [with_bin dir ext bin cmds] is a run that executes the sequence
      of commands [Conf.(cmds $ Bin.of_cmd ext bin)]. [name] is the
      name of the run; if unspecified this is derived from the [bin]
      part.  [ext] is the binary to select see {!Bin.to_cmd}.

      The binary is automatically added to the inputs of run's action
      and the resulting part only exists if the executable does. *)

  val bin : ?usage:Part.usage -> ?exists:bool Conf.value ->
    ?args:Args.t -> ?dir:Path.t Conf.value -> ?name:string ->
    ?ext:Path.ext -> ?stdin:Path.t Conf.value -> ?stdout:Path.t Conf.value ->
    ?stderr:Path.t Conf.value -> [< `Bin] part ->
    string list Conf.value -> [> `Run] part
  (** [bin bin args] is like {!with_bin} but directly executes the binary
      with [args] and redirects its outputs as specified. *)
end

(** {1:spec Part specification combinators} *)

type path = Path.t Conf.value
(** The type for part paths specifications. *)

val root : path
(** [root] is the {{!Path.current}current directory} relative to the
    project root.  *)

val ( / ) : path -> string -> path
(** [path / seg] is [Conf.(const Path.( / ) $ path $ const seg)]. *)

val ( // ) : path -> Path.rel Conf.value -> path
(** [path // rel] is [Conf.(const Path.( // ) $ path $ rel)]. *)

val unit : ?usage:Part.usage -> ?exists:bool Conf.value -> ?args:Args.t ->
  ?needs:[< `Lib | `Pkg ] part list -> ?kind:Unit.kind -> ?dir:path -> string ->
  [> `Unit] part
(** See {!Unit.v}. [kind] defaults to [`OCaml (`Both, `Normal)]. *)

val lib : ?usage:Part.usage -> ?exists:bool Conf.value -> ?args:Args.t ->
  ?byte:bool -> ?native:bool -> ?native_dynlink:bool -> ?kind:Lib.kind ->
  string -> [< `Unit | `Pkg | `Lib] part list -> [> `Lib] part
(** See {!Lib.v}. [kind] defaults to [`OCaml]. *)

val bin : ?usage:Part.usage -> ?exists:bool Conf.value -> ?args:Args.t ->
  ?byte:bool -> ?native:bool -> ?js:bool -> ?kind:Bin.kind -> string ->
  [< `Unit | `Pkg | `Lib] part list -> [> `Bin] part
(** See {!Bin.v}. [kind] defaults to [`OCaml]. *)

val pkg : ?usage:Part.usage -> ?exists:bool Conf.value -> ?opt:bool ->
  ?kind:Pkg.kind -> string -> [> `Pkg] part
(** See {!Pkg.v}. [kind] defaults to [`OCamlfind]. *)

val doc : ?usage:Part.usage -> ?exists:bool Conf.value -> ?args:Args.t ->
  ?keep:([< `Unit] part -> bool) -> ?kind:Doc.kind -> string ->
  [< `Bin | `Lib | `Unit ] part list -> [> `Doc] part
(** See {!Doc.v}. [kind] defaults to [`OCamldoc]. *)

val dir : ?usage:Part.usage -> ?exists:bool Conf.value -> ?args:Args.t->
  ?spec:Dir.spec -> ?install:bool -> Dir.kind ->
  [< `Base | `Bin | `Dir | `Doc | `Lib | `Unit ] part list -> [> `Dir ] part
(** See {!Dir.v}. *)

val file : ?usage:Part.usage -> ?exists:bool Conf.value -> Path.t ->
  [> `Base] part
(** See {!Part.file}. *)

val run : ?usage:Part.usage -> ?exists:bool Conf.value -> ?args:Args.t ->
  ?dir:path -> string -> Action.t Conf.value -> [> `Run] part
(** See {!Run.v}. FIXME maybe removed that one from the toplevel.  *)

(** {1:projects Projects} *)

type project
(** The type for projects descriptions. *)

(** Project descriptions. *)
module Project : sig

  (** {1 Projects} *)

  type t = project
  (** The type for describing projects. *)

  val v : ?exists:bool Conf.value -> ?args:Args.t ->
    ?schemes:Conf.scheme list -> string -> parts:'a part list -> project
  (** [v exists args parts n] is the project named [n] with parts [parts].
      [exists] determines if the project can exist in a build configuration. *)

  val name : project -> string
  (** [name p] is the [p]'s name. *)
end

val assemble : project -> unit
(** [assemble p] registers [p] for assembling by an assemblage driver. *)

(** {1 Private API} *)

(** Private functions and types for implementing drivers.

    Open the module after {!Assemblage} to use it. It extends
    Assemblage's module with private definitions.

    {b Warning.} Assemblage users must not use these definitions to
    describe their projects. They can hamper driver functionality and
    are subject to change even between minor versions of [Assemblage].
 *)
module Private : sig

  (** Log. *)
  module Log : sig

    (** {1 Log} *)

    include module type of Log with type level = Log.level

    (** {1 Log level and output} *)

    val level : unit -> level option
    (** [level ()] is the log level (if any). If the log level is [(Some l)]
        any message whose level is [<= l] is logged. If level is [None]
        no message is ever logged. Initially the level is [(Some Warning)]. *)

    val set_level : level option -> unit
    (** [set_level l] sets the log level to [l]. See {!level}. *)

    val set_formatter : [`All | `Level of level ] -> Format.formatter -> unit
    (** [set_formatter spec ppf] sets the formatter for a given level or
        for all the levels according to [spec]. Initially the formatter
        of level [Show] is {!Format.std_formatter} and all the other level
        formatters are {!Format.err_formatter}. *)

    (** {1 Log monitoring} *)

    val err_count : unit -> int
    (** [err_count ()] is the number of messages logged with level [Error]. *)

    val warn_count : unit -> int
    (** [warn_count ()] is the number of messages logged with level
        [Warning]. *)
  end

  (** Command. *)
  module Cmd : sig

  (** {1 Command} *)

    (** Version control systems *)
    module Vcs : sig

      (** {1 Version control systems} *)

      include module type of Cmd.Vcs

      val override_kind : unit -> t option
      (** [override_kind ()] is the current VCS kind override value.
          See {!set_override_kind}. *)

      val set_override_kind : t option -> unit
      (** [set_override_kind (Some vcs)] has the effect of bypassing
          VCS discovery. [vcs] will always {!Cmd.Vcs.exists},
          {!Cmd.Vcs.find} and {!Cmd.Vcs.get} will always always return
          this [vcs]. *)

      val override_exec : unit -> string option
      (** [override_bin] is the current VCS executable
          override. See {!set_override_exec}. *)

      val set_override_exec : string option -> unit
      (** [set_override_exec (Some vcs_exec)] uses [vcs_exec] as the
          VCS executable, use in conjunction with {!set_override_kind}
          otherwise it has no effect. *)
    end

    include module type of Cmd with module Vcs := Vcs
  end

  (** Build configuration. *)
  module Conf : sig

    (** {1:build Build configuration} *)

    type t
    (** The type for configurations. *)

    include module type of Conf
    with type 'a value = 'a Conf.value
     and type 'a key = 'a Conf.key
     and type scheme = string * (string * t)
     and type def = Conf.def

    (** {1:keys Keys} *)

    (** Configuration keys. *)
    module Key : sig

      (** {1 Existential keys} *)

      (** The type for existential keys. *)
      type t = V : 'a key -> t

      val hide_type : 'a key -> t
      (** [hide_type k] hides the type parameter of [k]. *)

      val equal : t -> t -> bool
      (** [equal k k'] is [true] iff [name k = name k']. *)

      val compare : t -> t -> int
      (** [compare k k'] compares [k] and [k'] and is compatible
          with {!equal}. *)

      (** {1:typed Typed key accessors} *)

      val id : 'a key -> int
      (** [id k] is the unique id of the key. *)

      val name : 'a key -> string
      (** [name k] is [k]'s name. *)

      val public : 'a key -> bool
      (** [public k] is [k]'s public status. *)

      val converter : 'a key -> 'a converter
      (** [converter k] is [k]'s value type converter. *)

      val default : 'a key -> 'a value
      (** [default k] is [k]'s default value. *)

      val doc : 'a key -> string option
      (** [doc k] is [k]'s documentation string (if any). *)

      val docv : 'a key -> string option
      (** [docv k] is [k]'s value documentation meta-variable (if any). *)

      val docs : 'a key -> string option
      (** [docs k] is [k]'s documentation section (if any). *)

      (** {1:setmap Key sets and maps} *)

      module Set : sig
        include Set.S with type elt = t
        val of_list : elt list -> t
      end

      module Map : sig
        include Map.S with type key = t
        val dom : 'a t -> Set.t
        (** [dom m] is the domain of [m]. *)
      end
    end

    (** {1:configurations Configurations} *)

    val empty : t
    (** [empty] is the empty configuration. *)

    val is_empty : t -> bool
    (** [is_empty c] is [true] iff [c] is empty. *)

    val mem : t -> 'a key -> bool
    (** [mem c k] is [true] iff [k] is in [c]. *)

    val add : t -> 'a key -> t
    (** [add c k] is [c] with [k] bindings to its default value. *)

    val set : t -> 'a key -> 'a value -> t
    (** [set c k v] is [c] with [k] binding to [v]. *)

    val rem : t -> 'a key -> t
    (** [rem c k] is [c] without [k]'s binding. *)

    val merge : t -> t -> t
    (** [merge c c'] merge the configuration [c] and [c']. If a key is
        defined in both [c] and [c'] the value of the key in [c'] takes
        over. *)

    val find : t -> 'a key -> 'a value option
    (** [find c k] is the binding of [k] in [c] (if any). *)

    val get : t -> 'a key -> 'a value
    (** [get c k] is the binding of [k] in [c].

        @raise Invalid_argument if [k] is not in [c]. *)

    val pp : Format.formatter -> t -> unit
    (** [pp ppf c] prints an unspecified representation of [c] on [ppf]. *)

    val of_keys : Key.Set.t -> t
    (** [of_keys ks] is a configuration where each key in [ks] binds to
        its default value. *)

    val domain : t -> Key.Set.t
    (** [domain c] is the configuration keys of [c]. *)

    (** {1 Configuration error messages} *)

    val pp_key_dup : Format.formatter -> Key.t -> unit
    (** [pp_key_dup ppf k] prints a message that says that the name of
        [k] is not unique in a configuration. *)

    (** {1 Configuration value dependencies and evaluation} *)

    val deps : 'a value -> Key.Set.t
    (** [deps v] is the set of configuration keys which may be needed
        for evaluating [v]. *)

    val eval : t -> 'a value -> 'a
    (** [eval c v] evaluates [v] in the configuration [c].

        @raise Invalid_argument if [c] is not a subset of [deps c]. *)

    (** {1 Builtin key sections documentation} *)

    val doc_project : string
    val doc_build_properties : string
    val doc_build_directories : string
    val doc_ocaml_system : string
    val doc_c_system : string
    val doc_machine_information : string
    val doc_system_utilities : string
  end

  (** Build argument bundles.

    {b Note.} From a driver implementation perspective the only thing
    one needs to care is to add the project's flags to the actions
    it consults. *)
  module Args : sig

    (** {1 Argument bundles} *)

    include module type of Args with type t = Args.t

    val deps : t -> Conf.Key.Set.t
    (** [deps a] is the set of configuration keys which may be needed
        for evaluating the constituents of [a]. *)

    val pp : Conf.t -> Format.formatter -> t -> unit
    (** [pp conf ppf args] prints an unspecified representation of
        [args] in context [conf] on [ppf]. *)

    (** {1 Argument lookup} *)

    val for_ctx : Conf.t -> Ctx.t -> t -> string list
    (** [with_ctx conf ctx args] is the arguments in [args] for
        context [ctx] in configuration [conf]. *)
  end

  (** Action commands. *)
  module Acmd : sig

    (** {1 Action commands} *)

    type args = Args.t (* because of the Args module in Acmd. *)

    include module type of Acmd with type t = Acmd.t
                                 and type cmd = Acmd.cmd


    val cmd_key : t -> string Conf.key option
    (** [cmd_key c] is [c]'s configuration key used to define {!cmd_name}
        (if any). *)

    val cmd_name : t -> string
    (** [cmd_name c] is [c]'s command name.  *)

    val args : t -> string list
    (** [args c] are [c]'s arguments. *)

    val stdin : t -> Path.t option
    (** [stdin c] is [c]'s stdin redirection (if any). *)

    val stdout : t -> Path.t option
    (** [stdout c] is [c]'s stdout redirection (if any). *)

    val stderr : t -> Path.t option
    (** [stderr c] is [c]'s stderr redirection (if any). *)

    val pp : Format.formatter -> t -> unit
    (** [pp ppf c] prints an unspecified representation of [c] on [ppf]. *)

    (** {1 Argument bundle injection} *)

    val ctx : Ctx.t -> t -> Ctx.t
    (** [ctx context c] is [context] augmented with [c]'s context. *)

    val args_with_ctx : Conf.t -> Ctx.t -> args -> t -> string list
    (** [args_with_ctx conf context args cmd] are [c]'s arguments
        prepended with the arguments found in [args] for the
        configuration [conf] and the context {!ctx}[context c]. *)
  end

  (** Build actions.

      {b Important.} Actions commands produce {e file} paths. Build system
      backends are in charge for making sure the {{!Path.dirname} directory
      name} of these paths exist before invoking the action. *)
  module Action : sig

    (** {1 Actions} *)

    include module type of Action with type t = Action.t

    val args : t -> Args.t
    (** [args a] is [a]'s argument bundle. *)

    val pp : Conf.t -> Format.formatter -> t -> unit
    (** [pp conf ppf a] prints an unspecified representation of
        [a] in context [conf] on [ppf]. *)
  end

  (** Parts. *)
  module Part : sig

    (** {1 Part} *)

    include module type of Part with type kind = Part.kind
                                 and type meta = Part.meta
                                 and type +'a t = 'a Part.t
                                   constraint 'a = [< part_kind ]

    val args : 'a part -> Args.t
    (** [args p] is [p]'s user defined argument bundle. *)

    val ctx : 'a part -> Ctx.t
    (** [ctx p] is a context that describes [p] using {{!type:Ctx.part}part
        context elements}. *)

    val deps : 'a part -> Conf.Key.Set.t
    (** [deps a] is the set of configuration keys which may be needed
        for evaluating [a]. *)
  end

  (** Projects.

      {b Important.} Project values may depend on the configuration
      it is the driver's responsability to set a project's configuration
      with {!with_conf} otherwise warnings are reported on configuration
      use. *)
  module Project : sig

    (** {1 Project} *)

    include module type of Project with type t = Project.t

    val exists : project -> bool Conf.value
    (** [exists p] is [p]'s condition of existence. *)

    val args : project -> Args.t
    (** [args p] is [p]'s args. *)

    val schemes : project -> Conf.scheme list
    (** [schemes p] is [p]'s configuration schemes. *)

    val parts : project -> part_kind part list
    (** [parts p] is [p]'s parts. *)

    (** {1 Configuration and evaluation} *)

    val deps : project -> Conf.Key.Set.t
    (** [deps p] is the set of configuration keys which may be needed
        for evaluating the constituents of [p]. *)

    val conf : project -> Conf.t
    (** [conf p] is the project's configuration.

        {b Note} The client should set a configuration whose
        domain has is at least {!deps}[ p] using {!with_conf}
        otherwise warnings are generated. *)

    val with_conf : project -> Conf.t -> project
    (** [with_conf p c] is [p] with configuration [c]. *)

    val eval : project -> 'a Conf.value -> 'a
    (** [eval p v] is [Conf.eval [conf p] v]. *)

    val eval_key : project -> 'a Conf.key -> 'a
    (** [eval_key p k] is [eval p (Conf.value k)]. *)

    (** {1 Configuration dependent values} *)

    val version : project -> string
    (** [version p] evaluates {!Conf.project_version} *)

    val products : ?kind:[`Source | `Input | `Output | `Any ] -> project ->
      Path.Set.t
    (** [products kind p] is the set of products known to the project
        in the current configuration. [kind] can be used to select
        source, input, output or all kind of products, defaults to [`Any]. *)

    val watermark_string : ?suffix:string -> t -> string
    (** [watermark_string suffix p] is a watermark that can be used in
        the comments of generated files for the project. It consists
        of the project {!name} followed by the project {!version}
        followed by [suffix] (defaults says it was generated by
        assemblage). *)

    val pp_signature : Format.formatter -> t -> unit
    (** [pp_signature ppf p] prints an unspecified signature for [p]
        on [ppf], includes the project name and version. *)

    (** {1 Assembled projects} *)

    val list : unit -> project list
    (** [list] is the list of projects that were {!assemble}d by
        the library so far. *)
  end
end

(** {1:basics Basics}

    An assemblage {{!project}project} is made of {{!parts}parts}. This
    was very basic.

    {1:dont Don't}

    Make the project depend on direct conditions. E.g.
{[
    if Sys.file_exists file then Some (unit "src")  else None
    if Sys.win32 then ...
    if Sys.ocaml_version = ...
]}

    That's the way of doing it:
{[
    let exists = Conf.(const Cmd.File.exists $ const (Path.file file)) in
     unit ~exists "src"
]}
    TODO the rule to hammer in people's mind is the following:
    don't do anything that doesn't make the set of configuration
    keys constant on *every* load of the [assemble.ml] file. Maybe
    we could still make an exception for a [units] combinator
    that looksup the units in a directory automatically.

   {1:limitations Limitations}

   The build model behind assemblage has the following limitation.
   {ul
   {- Doesn't work well with tools that are not able to determine
      their output statically, e.g. ocamldoc. FIXME dep
      discovery ?}
   {- TODO}}

   {1:design_part Designing new part kind}

   {ol
   {- Define configuration keys for the utilities.}
   {- Make non lifted action creators for the main
      operations. Don't use the keys yet, use a bin type to represent
      them. Arguments to the creators should be those that allow to
      correctly determine the inputs and outputs of the operation
      (e.g. for OCaml annot is handled in the creator). Add an
      [?args] optional argument.}
   {- In the part action definition function lift the utilities
      and common build configuration keys (debug, warn_error, etc).
      Handle the command argument for those at that level an pass
      them to actions using the [?args] optional argument of creators.}
   {- Don't support all command line flags, the user can use argument
      bundles. You may want to provide some of them already created.}} *)
