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

(** Extended [String] module, string sets and maps. *)
module String : sig

  (** {1 String} *)

  include module type of String

  (** These are sorely missing from the standard library. *)

  val split : sep:string -> string -> string list
  (** [split sep s] is the list of all (possibly empty) substrings of
      [s] that are delimited by matches of the non empty separator
      string [sep].

      Matching separators in [s] starts from the beginning of [s] and once
      one is found, the separator is skipped and matching starts again
      (i.e. separator matches can't overlap). If there is no separator
      match in [s], [[s]] is returned.

      The invariants [String.concat sep (String.split sep s) = s] and
      [String.split sep s <> []] always hold.

      @raise Invalid_argument if [sep] is the empty string. *)

  val rsplit : sep:string -> string -> string list
  (** [rsplit sep s] is like {!split} but the matching is
      done backwards, starting from the end of [s].

      @raise Invalid_argument if [sep] is the empty string. *)

  val cut : sep:string -> string -> (string * string) option
  (** [cut sep s] is either the pair [Some (l,r)] of the two
      (possibly empty) substrings of [s] that are delimited by the first
      match of the non empty separator string [sep] or [None] if [sep]
      can't be matched in [s]. Matching starts from the beginning of [s].

      The invariant [l ^ sep ^ r = s] holds.

      @raise Invalid_argument if [sep] is the empty string. *)

  val rcut : sep:string -> string -> (string * string) option
  (** [rcut sep s] is like {!cut} but the matching is done backwards
      starting from the end of [s].

      @raise Invalid_argument if [sep] is the empty string. *)

  val slice : ?start:int -> ?stop:int -> string -> string
  (** [slice ~start ~stop s] is the string s.[start], s.[start+1], ...
      s.[stop - 1]. [start] defaults to [0] and [stop] to [String.length s].

      If [start] or [stop] are negative they are subtracted from
      [String.length s]. This means that [-1] denotes the last
      character of the string. *)

  val tokens : string -> string list
  (** [tokens s] is the list of non empty strings made of characters
      that are not separated by [' '], ['\t'], ['\n'], ['\r'] characters in
      [s], the order of character appearance in the list is the same as
      in [s]. *)

  (** {1 String sets and maps} *)

  module Set : sig
    include Set.S with type elt = string
    val of_list : string list -> t
  end

  module Map : Map.S with type key = string
end

(** Formatters. *)
module Fmt : sig

  (** {1 Formatters} *)

  type 'a formatter = Format.formatter -> 'a -> unit
  (** The type for formatters of values of type ['a]. *)

  val pp : Format.formatter -> ('a, Format.formatter, unit) Pervasives.format ->
    'a
  (** [pp] is {!Format.fprintf} *)

  val rpp : ('a, Format.formatter, unit) Pervasives.format ->
    Format.formatter -> 'a
  (** [rpp] is [pp fmt ppf] *)

  val nop : 'a formatter
  (** [nop] does nothing. *)

  val pp_cut : unit formatter
  (** [pp_cut] is {!Format.pp_print_cut}. *)

  val pp_sp : unit formatter
  (** [pp_sp] is {!Format.pp_print_space}. *)

  val pp_str : string formatter
  (** [pp_str] is {!Format.pp_print_string}. *)

  val pp_int : int formatter
  (** [pp_int] is {!Format.pp_print_int}. *)

  val pp_bool : bool formatter
  (** [pp_bool] is {!Format.pp_print_bool}. *)

  val pp_larrow : unit formatter
  (** [pp_larrow] formats a left arrow. *)

  val pp_rarrow : unit formatter
  (** [pp_rarrow] formats a right arrow. *)

  val pp_opt : ?pp_none:unit formatter -> 'a formatter -> 'a option formatter
  (** [pp_opt pp_none pp_v] formats value of type ['a option]. The default
      value of [pp_none] prints nothing. *)

  val pp_list : ?pp_sep:unit formatter -> 'a formatter -> 'a list formatter
  (** [pp_list pp_sep pp_v] formats lists of type ['a]. Each value
      is printed with [pp_v] followed by [pp_sep] (defaults to {!pp_cut}).
      Empty lists never print anything. *)

  val pp_text : string formatter
  (** [pp_text] formats text by replacing spaces and newlines in the string
      with calls to {!Format.pp_print_space} and {!Format.pp_force_newline}. *)

  val pp_lines : string formatter
  (** [pp_lines] formats lines by replacing newlines in the string
      with calls to {!Format.pp_force_newline}. *)

  (** {1:utf8_cond Conditional UTF-8 formatting}

      {b Note.} Since {!Format} is not UTF-8 aware using UTF-8 output
      may derail the pretty printing process. The decision of allowing
      UTF-8 is left to the discretion of the drivers. *)

  val pp_if_utf8 : 'a formatter -> 'a formatter -> 'a formatter
  (** [pp_if_utf8 pp_u pp] is a formatter that will use [pp_u] if UTF-8
      output is enabled by the driver and [pp] otherwise. *)

  (** {1:styled Styled formatting}

      {b Note.} Text output using these functions may still appear
      unformatted, style application is left to the discretion of
      drivers. *)

  type style =
    [ `Bold
    | `Underline
    | `Black
    | `Red
    | `Green
    | `Yellow
    | `Blue
    | `Magenta
    | `Cyan
    | `White
    | `None ]
  (** The type for styles. *)

  val pp_styled : style -> 'a formatter -> 'a formatter
  (** [pp_styled style pp] formats according to [pp] but styled with [style]. *)

  val pp_styled_str : style -> string formatter
  (** [pp_styled_str style] is [pp_styled_str style pp_str]. *)
end

(** File system paths, path sets and maps.

    {b Note.} Don't use {!Filename.current_dir_name} and
    {!Filename.parent_dir_name} in path segments. FIXME/TODO should we
    raise [Invalid_argument] ? We could also normalize via the constructors.
    Cli interaction ? Path.of,to_string Path.quote define guidelines *)
module Path : sig

  (** {1:filepaths File paths} *)

  type filename = string
  (** The type for file names (basenames). *)

  type segs
  (** The type for lists of path segments. *)

  type rel = [`Rel of segs]
  (** The type for relative file paths. The empty list of segments denotes
      the current directory. *)

  type abs = [`Abs of segs]
  (** The type for absolute file paths. The empty list of segments denotes
      the root directory. *)

  type t = [ abs | rel ]
  (** The type for paths. Either relative or absolute paths. *)

  val current : [> rel]
  (** [current] is the current directory for relative paths. *)

  val root : [> abs]
  (** [root] is the root directory for absolute paths. *)

  val dash : [> rel]
  (** [dash] is {!file}[ "-"]. *)

  val is_current : [> rel] -> bool
  (** [is_current p] is true iff [p] is {!current}. *)

  val is_root : [> abs] -> bool
  (** [is_root p] is true iff [p] is {!root}. *)

  val is_rel : [> rel] -> bool
  (** [is_rel p] is [true] iff [p] is a relative path. *)

  val is_abs : [> abs] -> bool
  (** [is_abs p] is [true] iff [p] is an absolute path. *)

  val is_dash : [> rel] -> bool
  (** [is_dash] is [true] iff [p] is {!dash}. *)

  val as_rel : t -> rel
  (** [as_rel p] is [p] as a relative path.

      @raise Invalid_argument if [p] is an absolute path. *)

  val as_abs : t -> abs
  (** [as_abs p] is [p] as an absolute path.

      @raise Invalid_argument if [p] is a relative path. *)

  val as_path : [< t ] -> t
  (** [as_path p] is [p]. *)

  val basename : [< t ] -> string
  (** [basename p] is the basename of [p]. If [p] has no segments the
      empty string is returned. *)

  val dirname : [< t ] -> t
  (** [dirname p] is the dirname of [p]. If [p] has no segments [p]
      is returned. *)

  val concat_seg : [< t ] -> string -> t
  (** [concat_seg p seg] concatenates [seg] at the end of [p]. For any [p],
      [concat_seg p "" = p]. *)

  val concat : [< t ] -> rel -> t
  (** [concat p p'] concatenates [p'] at the end of [p]. *)

  val ( / ) : [< t ]  -> string -> t
  (** [p / c] is [concat_seg p c]. Left associative. *)

  val ( // ) : [< t ] -> rel -> t
  (** [p // p'] is [concat p p']. Left associative. *)

  val file : filename -> [> rel]
  (** [file f] is the relative path to file name [f].
      Equivalent to [current / f]. *)

  val dir : string -> [> rel]
  (** [dir d] is the relative path to directory [d].
      Equivalent to [current / d]. *)

  val rel_of_segs : string list -> t
  (** [rel_of_segs segs] is a relative path from segments [segs]. *)

  val abs_of_segs : string list -> t
  (** [abs_of_segs segs] is an absolute path from segments [segs]. *)

  val to_abs : ?rel_base:abs -> [< t ] -> t
  (** [to_abs rel_base p] is [p] if [p] is absolute and [rel_base // p] if
      [p] is relative. [rel_base] defaults to {!root}. *)

  val equal : t -> t -> bool
  (** [equal p p'] is [p = p']. *)

  val compare : t -> t -> int
  (** [compare p p'] is [Pervasives.compare p p']. *)

  val quote : [< t ] -> string
  (** [quote p] is the path [p] as a string, quoted according
      to the driver's platform conventions with {!Filename.quote}. *)

  val to_string : [< t ] -> string
  (** [to_string p] is the path [p] as a string according to
      the driver's platform convention with {!Filename.dir_sep}. *)

  val of_string : string -> t
  (** [of_string s] is the string [s] as a path. [s] is splitted
      according to the driver's platform convention with {!Filename.dir_sep}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf p] prints path [p] on [ppf] using {!to_string}. *)

  (** {1 File extensions} *)

  type ext =
  [ `A | `Byte | `C | `Cma | `Cmi | `Cmo | `Cmt | `Cmti | `Cmx | `Cmxa
  | `Cmxs | `Css | `Dll | `Exe | `Gif | `H | `Html | `Install | `Img
  | `Jpeg | `Js | `Json | `Lib | `Md | `Ml | `Ml_dep | `Ml_pp | `Mli
  | `Mli_dep | `Mli_pp | `Native | `O | `Opt | `Png | `Sh | `So | `Tar
  | `Tbz | `Xml | `Zip
  | `Ext of string ]
  (** The type for file extensions. *)

  val pp_ext : Format.formatter -> ext -> unit
  (** [pp_ext ppf p] prints file extension [ext] on [ppf] using
      {!ext_to_string}. *)

  val ext_to_string : ext -> string
  (** [ext_to_string ext] is [ext] as a string (without seperator). *)

  val ext_of_string : string -> ext
  (** [ext_of_string ext] is [ext] as a file extension. *)

  val has_ext : ext -> [< t ] -> bool
  (** [has_ext p ext] is [true] iff [p]'s last segment has file extension
      [ext]. *)

  val ext_matches : ext list -> [< t ] -> bool
  (** [ext_matches exts p] is [true] iff [p]'s last segment has a file
      extension in [exts]. *)

  val ext : [< t ] -> ext option
  (** [ext p] is [p]'s last segment file extension (if any). *)

  val get_ext : [< t ] -> ext
  (** [get_ext p] is [p]'s last segment file extension.

      @raise Invalid_argument if [p]'s last segment has no file extension. *)

  val add_ext : [< t ] -> ext -> t
  (** [add_ext p ext] is [p] with [ext] concatenated to the {!basename}
      of [p]. *)

  val rem_ext : [< t ] -> t
  (** [rem_ext p] is [p] with [ext] removed from the {!basename} of
      [p] (if it has an extension). *)

  val change_ext : [< t ] -> ext -> t
  (** [change_ext p e] is [add_ext (rem_ext p)]. *)

  val ( + ) : [< t ] -> ext -> t
  (** [p + ext] is [add_ext p e]. Left associative. *)

  val ( -+ ) : [< t ] -> ext -> t
  (** [p -+ ext] is [change_ext p e]. Left associative. *)

  (** {1 Path sets and maps} *)

  module Set : sig
    include Set.S with type elt = t
    val of_list : [ abs | rel ] list -> t
  end

  module Map : Map.S with type key = t
end

(** Assemblage log. *)
module Log : sig

  (** {1 Log} *)

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

(** Executing {e non-build} commands and IO operations. *)
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

    val exists : path -> bool result
    (** [exists path] is [true] iff [path] exists. *)

    val move : ?force:bool -> path -> path -> unit result
    (** [move ~force src dst] moves path [src] to [dst]. If [force] is
        [false] (default) the operation fails if [dst] exists. *)
  end

  (** File operations. *)
  module File : sig

    (** {1:fileops File operations}

        {b Note.} When paths are {{!Path.rel}relative} they are expressed
        relative to the {{!Dir.getcwd}current working directory}. *)

    val exists : path -> bool result
    (** [exists file] is [true] iff [file] exists and is not a directory. *)

    val dev_null : path
    (** [dev_null] represents a file that discards all writes.

        {b Warning.} Do not use this value to define build actions,
        use {!Action.dev_null}. *)

    val delete : ?maybe:bool -> path -> unit result
    (** [delete ~maybe file] deletes file [file]. If [maybe] is [false]
        (default) no error is returned if the file doesn't exit. *)

    val temp : string -> path result
    (** [temp suffix] creates a temporary file with suffix [suffix] and returns
        its name. The file is destroyed at the end of program execution. *)

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

    val exists : path -> bool result
    (** [exists dir] is [true] if directory [dir] exists. *)

    val getcwd : unit -> path result
    (** [getcwd ()] is the current working directory. *)

    val chdir : path -> unit result
    (** [chdir dir] changes the current working directory to [dir]. *)

    val fold_files_rec : ?skip:string list -> (string -> 'a -> 'a result) ->
      'a -> string list -> 'a result
    (** [fold_files_rec skip f acc paths] folds [f] over the files
        found in [paths]. Files and directories whose suffix matches an
        element of [skip] are skipped. *)
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

  val exists : string -> bool result
  (** [exists cmd] is [true] if [cmd] exists and can be invoked. *)

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

    TODO not good. Rephrase and say what a configuration is
    (map from keys to value).

    Assemblage keeps tracks of the configuration keys needed to define
    a project and its build system through configuration values. A
    configuration value denotes a concrete OCaml value of a certain
    type.

    Configuration {{!type:key}keys} are named configuration value that
    are meant to be redefined by the end user of the build system
    (e.g. from the command line or from an IDE). A configuration key
    can be used to determine a configuration value or other
    configuration keys.

    Configuration {{!type:scheme}schemes} are named, user defined,
    configuration key presets. They allow the end user to quickly
    setup a given configuration. Configuration schemes are meant to be
    used for development, not distribution, where configuration setup
    based on feature detection is more adapted.

    Before {{!key}defining} your own keys you should prefer the
    {{!builtin_keys}built-in ones}. *)
module Conf : sig

  (** {1 Configuration values} *)

  type 'a value
  (** The type for configuration values evaluating to values of type ['a]. *)

  val const : 'a -> 'a value
  (** [const v] is a configuration value that evaluates to [v]. *)

  val app : ('a -> 'b) value -> 'a value -> 'b value
  (** [app f v] is a configuration value that evaluates to the result
      of applying the evalaution of [v] to the one of [f]. *)

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

  (** Optional values. *)
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

  (** List values. *)
  module List : sig

    (** {1 Lists} *)

    val wrap : 'a value list -> 'a list value
    (** [wrap l] is [l] as a value. *)

    val rev_wrap : 'a value list -> 'a list value
    (** [rev_wrap l] is [List.rev l] as a value. *)

    val is_empty : 'a list value -> bool value
    (** [is_empty vl] is [true] if [vl] is empty. *)

    val empty : 'a list value
    (** [empty] is [const []]. *)

    val singleton : 'a value -> 'a list value
    (** [singleton v] is [const (fun v -> [v]) $ v]. *)

    val add : 'a value -> 'a list value -> 'a list value
    (** [add v vl] adds [v] in front of [vl]. *)

    val add_if : bool value -> 'a value -> 'a list value -> 'a list value
    (** [add_if v vl] adds [v] in front of [vl] only if [c] is true. *)

    val rev : 'a list value -> 'a list value
    (** [rev] lifts {!List.rev}. *)

    val append : 'a list value -> 'a list value -> 'a list value
    (** [append] lifts {!List.append} *)

    val rev_append : 'a list value -> 'a list value -> 'a list value
    (** [rev_append] lifts {!List.rev_append} *)

    val flatten : 'a list list value -> 'a list value
    (** [flatten] lifts {!List.flatten}. *)

    val map : ('a -> 'b) -> 'a list value -> 'b list value
    (** [map] lifts {!List.map}. *)

    val rev_map : ('a -> 'b) -> 'a list value -> 'b list value
    (** [rev_map] lifts {!List.rev_map}. *)

    val fold : ('b -> 'a -> 'b) -> 'b value -> 'a list value -> 'b value
    (** [fold] lifts {!List.fold_left}. *)

    val exists : ('a -> bool) -> 'a list value -> bool value
    (** [exists] lifts {!List.exists}. *)

    val keep : ('a -> bool) -> 'a list value -> 'a list value
    (** [keep] lifts {!List.filter} but is tail recursive. *)

    val keep_map : ('a -> 'b option) -> 'a list value -> 'b list value
    (** [keep_map f vl] keeps the element of [vl] that are mapped
        to some value by [f]. Tail recursive. *)
  end

  (** {1 Configuration value converters}

      A configuration value converter transforms a string value
      to an OCaml value. There are a few
      {{!builtin_converters}built-in converters}. *)

  type 'a parser = string -> [ `Error of string | `Ok of 'a ]
  (** The type for configuration value parsers. *)

  type 'a printer = Format.formatter -> 'a -> unit
  (** The type for configuration value printers. *)

  type 'a converter = 'a parser * 'a printer
  (** The type for configuration value converters. *)

  val parser : 'a converter -> 'a parser
  (** [parser c] is [c]'s parser. *)

  val printer : 'a converter -> 'a printer
  (** [converter c] is [c]'s printer. *)

  (** {1 Configuration keys} *)

  type 'a key
  (** The type for configuration keys. *)

  val key : ?public:bool -> ?docs:string -> ?docv:string -> ?doc:string ->
    string -> 'a converter -> 'a value -> 'a key
  (** [key public docs docv doc name conv v] is a configuration key
      named [name] converted using [converter] and that defaults to
      [v] if unspecified by the build system user. The key is public
      according to [public] (defaults to [true]) in which case [docs]
      is a documentation section under which the key should be
      documented according to the documentation string [doc] and the
      value documentation meta-variable [docv].

      TODO add hints about how to write doc, docv, and docs.
      docs defaults to {!docs_project}.

      TODO key name must be made of lowercase ASCII letters + dash or
      underscore.

      {b Warning.} No two public keys should share the same [name] as
      this may lead to difficulties in certain assemblage drivers
      (like the inability to define the key on the command line).  In
      particular do not reuse the {{!builtin_keys}built-in names}
      (they have the same name as the key variables with underscores
      replaced by dashes). *)

  val value : 'a key -> 'a value
  (** [value k] is [k]'s value. *)

  (** {1:scheme Configuration schemes} *)

  type scheme
  (** The type for configuration schemes. A configuration scheme is
      a named set of configuration key-value binding definitions. *)

  type def
  (** The type for key-value binding definitions. *)

  val def : 'a key -> 'a -> def
  (** [def k v] is a definition that binds key [k] to value [v]. *)

  val scheme : ?doc:string -> ?base:scheme -> string -> def list -> scheme
  (** [scheme base name defs] is a configuration scheme named [name]
      that has the key-value bindings of [base] together with those
      of [defs], the latter taking precedence. [doc] is a documentation
      string for the scheme. *)

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

      {b Note.} Build actions should not use these utilities directly
      but use {{!Action.portable_invocations}portable
      system utility invocations}. *)

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

  val cd : string key
  (** [cd] is the
      {{:http://pubs.opengroup.org/onlinepubs/009695399/utilities/cd.html}[cd]}
      utility (defaults to ["cd"]). *)

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
      [make]} utility (defaults to ["make"]). *)

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

(** Build contexts.

    Build contexts define an indirect addressing mechanism used to
    inject {{!Args}arguments} on the command lines of build
    {{!Action}Actions}. The concrete build context associated to an
    action's command depends on the the end-user (via the
    {{!Part.name}part name}), the action designer (the [context] argument
    of {!Action.v}), and the {{!Action.cmd}key name} of the command being
    executed. *)
module Ctx : sig

  (** {1:elements Context elements} *)

  type tag = [ `Tag of string ]
  (** The type for user defined tags. *)

  type language = [ `OCaml | `C | `Js | `Lang of string ]
  (** The type for informing about the broad type of input build products.
      {ul
      {- [`OCaml] working on OCaml related build products.}
      {- [`C] working on C releated build products.}
      {- [`Js] working on JavaScript related build products.}
      {- [`Other l] working on language [l] related build products.}} *)

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

  type command = [ `Cmd of string Conf.key ]
  (** The type for informing about the command being executed.
      {ul
      {- [`Cmd k], the command [k] is being executed.}} *)

  type part_usage = [ `Build | `Dev | `Doc | `Other of string
                    | `Outcome | `Test ]
  (** The type for {{!type:Part.usage}part usages}. *)

  type part_kind = [ `Base | `Bin | `Dir | `Doc | `Lib
                   | `Pkg | `Run | `Silo | `Unit ]
  (** The type for {{!type:Part.kind}part kinds}. *)

  type part = [ `Part of [part_usage | part_kind | `Name of string ] ]
  (** The type for informing about a part. Its name,
      {{!type:Part.kind}kind} and {{!type:Part.usage}usages}. *)

  type elt = [ tag | language | build_phase | source | target | command
             | part ]
  (** The type for context elements. *)

  val pp_elt : Format.formatter -> elt -> unit
  (** [pp_elt ppf e] prints an unspecified representation of [e] on [ppf]. *)

  (** {1:context Contexts} *)

  type t
  (** The type for contexts, sets of context elements. *)

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

(** Build argument bundles.

    Argument bundles are conditional bindings from {{!Ctx}build contexts}
    to ordered lists of build command arguments. See {{!basics}argument
    bundle basics} and {{!propagation}argument bundle propogation} for
    more details.  *)
module Args : sig

  (** {1:argument_bundles Argument bundles} *)

  type t
  (** The type for argument bundles. *)

  val v : ?cond:bool Conf.value -> Ctx.t -> string list Conf.value -> t
  (** [v ~cond ctx args] is the bundle that binds [ctx] to [args]
      whenever [cond] evaluates to [true] (defaults to {!Conf.true_}). *)

  val vc : ?cond:bool Conf.value -> Ctx.t -> string list -> t
  (** [vc ~active ctx args] is [v ~active ctx (Conf.const args)]. *)

  val empty : t
  (** [empty] is the empty bundle. *)

  val is_empty : t -> bool
  (** [is_empty a] is [true] if [a] is empty. *)

  val append : t -> t -> t
  (** [append a a'] is the bundle that has the bindings of [a] and [a'].
      If both [a] and [a'] have bindings for the same context, they
      are preserved each with their condition and it is guaranteed
      that the binding arguments of [a] will appear before those of
      [a'] on the command line. *)

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

    Argument bundles allow to tweak build actions by prepending additional
    arguments to their {{!Action.cmd}command} invocations whenever a
    particular configuration value is [true]. For example the
    following bundle:
{[
let ocaml_debug =
  let ctx = Ctx.v [`OCaml; `Compile] in
  Args.v ~cond:Conf.debug ctx (Conf.const ["-g"])
]}
    when used with an action will, whenever the configuration value of
    {!Conf.debug} denotes [true], prepend the option [-g] to any
    command of the action that operates in a context that contains the
    [`OCaml] and [`Compile] elements (note you don't need to do that,
    assemblage's OCaml built-in actions know how to handle the
    {!Conf.debug} key for you.)

    The general mechanism when an action is executed with a given
    bundle is for each command to look up every binding in the
    bundle. If the context of a binding {{!Ctx.matches}matches} the
    context of the command and the binding condition evaluates to [true]
    the arguments of the binding are prepended to the command invocation.

    {b Warning.} Do not rely on the order of context matches for your
    command lines to be valid. The argument bundle mechanism is a
    rough end user build action tweaking mechanism that is mostly
    useful for injecting command line {e options}, not {e positional}
    arguments. The final bundle given to actions is the concatenation
    of many bundle sources (project, parts, action implementation)
    that may be applied in arbitrary order. FIXME this is not exactly
    true we need to say something about {!append} and e.g. libs order,
    FIX the FIXME, bundles should not be used for libs order.


    {1:propagation Argument bundle propagation model}

    FIXME explain that better. Maybe also we want two
    separate explanations, one for the casual user one
    for the part designer.

    As a part/action designer we should never use the
    bundle mecanism to define our actions internally.
    It can be used though to provide an API to the part for
    the end user. E.g. by providing appropriate and high-level
    bundles that inject the right flags for a part.

    Which argument bundles are applied to an action:

    {ol
    {- Each part has a user defined argument bundle, this bundle
       is automatically added to any of its actions (i.e. are
       in {!Part.actions})}
    {- TODO find a good terminology. Each part has needs. In
       these needs there are parts that {e swallowed} and other
       that are {e tasted}. When a part is swallowed its build
       actions are integrated into the swallowing part. In that
       case these actions have both the argument bundle of the
       swallowed and the swallowing part.}
    {- Finally the project's argument bundle is added to any
       action.}}

    {b Note.} From a the driver implementation perspective the only thing
    one needs to care is to add the project's flags to the actions
    it consults. *)
end

(** Build action.

    A {e build product} is any existing file in the {!Conf.root_dir}
    hierarchy. A {e root} build product is a product for which there
    exists no build action to create it; typically the source code you
    write.

    Given a list of existing products, a {e build action} determines
    how to create a list of products using a sequence of build commands. *)
module Action : sig

  (** {1 Products} *)

  type product = Path.t Conf.value
  (** The type for build products. A configuration value holding
      a file path. *)

  type products = Path.t list Conf.value
  (** The type for lists of build products. A configuration value holding
      a list of file paths. *)

  (** {1 Build commands}

      One particular aspect of build commands is that the executing
      program must be a configuration key. This ensures that the
      executions can be addressed via a {{!Ctx.command}context element}
      and that the tool can be redefined by the build system end user. *)

  type cmd
  (** The type for a command execution. *)

  type cmds = cmd list Conf.value
  (** The type for sequences of command executions. *)

  type cmd_gen =
    ?stdin:Path.t -> ?stdout:Path.t -> ?stderr:Path.t -> string list ->
    cmd
  (** The type for command execution generators. The string list is
      the arguments given to the program. If present the optional
      arguments allow to redirect the standard file descriptors and *)

  val cmd : string Conf.key -> cmd_gen Conf.value
  (** [cmd cmd] is a value that allows to define an execution for the
      program [cmd] with the given generator. *)

  val cmd_exec : ?stdin:product -> ?stdout:product -> ?stderr:product ->
    string Conf.key -> string list Conf.value -> cmds
  (** [cmd_exec cmd args] is the sequence that executes program [cmd] with
      argument [args]. The optional [stdin], [stdout], and [stderr]
      arguments allow to redirect the standard file descriptors of the
      execution to files. Convenience function built on top of {!cmd}. *)

  val seq : cmds -> cmds -> cmds
  (** [seq cmds cmds'] is the sequence of build commands made of [cmds]
      followed by [cmds']. *)

  val ( <*> ) : cmds -> cmds -> cmds
  (** [cmds <*> cmds'] is [seq cmds cmds']. *)

  (** {2:portable_invocations Portable system utility invocations}

      Rather than using {{!Conf.system_utility_keys}system utility
      configuration keys} directly you should use the following functions,
      they will enforce portable behaviour.

      {b Note.} Function arguments could support more labelling but
      this doesn't blend well with {!Conf.app}. *)

  val dev_null : Path.t Conf.value
  (** [dev_null] is a file that discards all writes. *)

  val ln : (Path.t -> Path.t -> cmd) Conf.value
  (** [ln] has a command [exec src dst] to link symbolically file [src] to
      [dst].

      {b Warning.} On Windows this is a copy. *)

  val cp : (Path.t -> Path.t -> cmd) Conf.value
  (** [cp] has a command [exec src dst] to copy file [src] to [dst]. *)

  val mv : (Path.t -> Path.t -> cmd) Conf.value
  (** [mv] has a command [exec src dst] to move path [src] to [dst]. *)

  val rm_files : (?f:bool -> Path.t list -> cmd) Conf.value
  (** [rm_files] has a command [exec ~f paths] to remove the {e file}
      paths [paths]. If [f] is [true] files are removed regardless of
      permissions (defaults to [false]). [paths] elements must be files,
      for directories, see {!rm_dirs}. *)

  val rm_dirs : (?f:bool -> ?r:bool -> Path.t list -> cmd) Conf.value
  (** [rm_dirs] has a command [exec ~f ~r paths] to remove the {e
      directory} paths [paths]. If [f] is [true] directories are
      removed regardless of permissions (defaults to [false]).  If [r]
      is [true] removes the file hierarchies rooted at the elements of
      [paths]. Note that [paths] must be directories, for removing
      files, see {!rm_files}. *)

  val mkdir : (Path.t -> cmd) Conf.value
  (** [mkdir] has a command [exec d] to create the directory [p].
      Intermediate directories are created as required ([mkdir -p] in
      Unix parlance). *)

  (** {1 Build actions} *)

  type t
  (** The type for build actions. *)

  val v : ?cond:bool Conf.value -> ctx:Ctx.t -> inputs:products ->
    outputs:products -> cmds -> t
   (** [v cond args ctx inputs outputs cmds] is the action that given the
       existence of [inputs] creates the products [outputs] using the
       sequence of command [cmds]. The action is only available if
       [cond] evaluates to [true].

       {b Warning.} To ensure determinism and parallelism correctness [cmds]
       must ensure that it only reads from the [inputs] and solely writes to
       [outputs]. *)

  (** Combinators to define build actions.

      Open this module {e locally} to define your action. *)
  module Spec : sig

    (** {1 List configuration values} *)

    type 'a list_v = 'a list Conf.value
    (** The type for list configuration values. *)

    val atom : 'a -> 'a list_v
    (** [atom v] is [Conf.const [v]]. *)

    val atoms : 'a list ->  'a list_v
    (** [atoms l] is [Conf.const l]. *)

    val add : 'a list_v -> 'a list_v -> 'a list_v
    (** [add l l'] is [Conf.const ( @ ) l l'] but tail recursive. *)

    val add_if : bool Conf.value -> 'a list_v -> 'a list_v -> 'a list_v
    (** [add_if c l l'] is [add l l'] if [c] evaluates to [true] and [l']
        otherwise. *)

    val add_if_key : bool Conf.key -> 'a list_v -> 'a list_v -> 'a list_v
    (** [add_if_key k l l'] is [add_if (Conf.value k) l l']. *)

    (** {1 Paths and products} *)

    val path : product -> ext:Path.ext -> Path.t Conf.value
    (** [path p ~ext] is [p] with its extension (if any)
        {{!Path.change_ext}changed} to [ext]. *)

    val path_base : product -> string Conf.value
    (** [path_base p] is the {{!Path.basename}basename} of [p]. *)

    val path_dir : product -> Path.t Conf.value
    (** [path_dir p] is the {{!Path.dirname}dirname} of [p]. *)

    val path_arg : ?opt:string -> Path.t Conf.value -> string list_v
    (** [path_arg p] is the list made of [p] {{!Path.quote}converted}
        to a string. If [opt] is present it is added in front of the list. *)

    val paths_args : ?opt:string -> Path.t list Conf.value ->
      string list_v
    (** [paths_args p] is  like {!path_arg} but for a list of paths.
        If [opt] is present it is added in front of each path list element. *)

    val product : ?ext:Path.ext -> product -> products
    (** [product p] is the list made of [p]. If [ext] is present the suffix
        of [p] is {{!Path.change_ext}changed} to [ext]. *)

    (** {1 Commands} *)

    val ( <*> ) : cmds -> cmds -> cmds
    (** ( <*> ) is {!Action.( <*> )}. *)
  end

  (** {1 Built-in actions} *)

  val link : src:product -> dst:product -> unit -> t
  (** [link ~src ~dst ()] is an action that links [src] to [dst] using
      {!ln}. FIXME rel hack *)

  (** Actions for handling OCaml products.

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
    (** The type for product names. A product names defines a build
        location through its {{!Path.dirname}dirname} and a name through its
        {e suffix-less} {{!Path.basename}basename}. *)

    (** {1 Preprocessing} *)

    val compile_src_ast : [`Ml | `Mli ] -> src:product -> unit -> t
    (** [compile_src_ast kind src ()] treats [src] of the given [ml] type
        and builds its AST. FIXME needs arguments for
        applying pp on the way ? *)

    (** {1 Compiling} *)

    val compile_mli : incs:includes -> src:product -> unit -> t
    (** [compile_mli ~incs ~src ()] compiles the mli file [src] (which can
        be an AST, see {!compile_src_ast}) with includes [incs]. *)

    val compile_ml_byte : has_mli:bool Conf.value -> incs:includes ->
      src:product -> unit -> t
    (** [compile_ml_byte ~has_mli ~incs ~src ()] compiles to byte code the ml
        file [src] (which can be an AST, see {!compile_src_ast}) with
        includes [incs]. [has_mli] indicates whether the source has a
        corresponding mli file. *)

    val compile_ml_native : has_mli:bool Conf.value ->
      incs:includes -> src:product -> unit -> t
    (** [compile_ml_native ~has_mli ~incs ~src ()] is like {!compile_ml_byte}
        but compiles to native code. *)

    val compile_c : src:product -> unit -> t
    (** [compile_c src ()] compiles the C [src] to native code through
        the OCaml compiler. *)

    (** {1 Archiving} *)

    val archive_byte : cmos:products -> name:name -> unit -> t
    (** [archive_byte cmos name ()] archives the byte code files [cmos]
        to a byte code archive (cma) named by [name]. *)

    val archive_native : cmx_s:products -> name:name -> unit -> t
    (** [archive_native cmx_s name ()] archives the native code files [cmx_s]
        to a native code archive (cmxa) named by [name]. *)

    val archive_shared : cmx_s:products -> name:name -> unit -> t
    (** [archive_shared cmx_s name ()] archives the native code files [cmx_s]
        to a native code shared archive (cmxs) named by [name]. *)

    val archive_c : objs:products -> name:name -> unit -> t
    (** [archive_c cmx_s name ()] archives the C object files [objs]
        to a C archive and shared archived (a, so) named by [name]. *)

    (** {1 Linking} *)

  end
end

(** {1:parts Parts} *)

type part_kind =
  [ `Base | `Unit | `Lib | `Bin | `Pkg | `Run | `Doc | `Dir | `Silo ]
(** The type for part kinds. *)

type +'a part constraint 'a = [< part_kind ]
(** The type for project parts. *)

(** Parts, parts sets and maps.

    A part is a higher-level logical build unit. It has specific
    metadata that depends on its {{!type:part_kind}kind} and may refer
    to other parts. Using this information a part defines a set
    of build actions which in turn define the part's build products.
    Use the {{!spec}combinators} to specify your parts.

    In addition to part kind specific metadata, any part has the
    following attributes that you specify:
    {ul
    {- A {{!name}name} used to identify the part. Note that it may not
       be unique. The part's name is often used as metadata itself,
       e.g. it defines the library name for {{!Lib}library parts} or
       the name of a {{!Unit} compilation unit}, etc.}
    {- A {{!type:kind}kind} that identifies the of metadata it needs
       and the build actions it encapsulates.}
    {- A {{!type:usage}usage}. This is a broad usage label to classify
       your parts. It can be used by drivers to derive bureaucratic
       information about your project. For example if some of
       your parts are only used for developing the project you
       should mark them with the [`Dev] usage, this will allow
       to mark their package dependencies as being developement only.}
    {- {{!needs}Needs.} This is a list of parts it uses to
       define its actions. The way needs are used depend
       on the part kind. FIXME expand on that.}
    {- A {{!cond}configuration condition} that indicates whether
       a part is available in a given configuration. For example
       the existence of a library may depends on the presence
       of an optional package.}
    {- An {{!args}argument bundle} which it applies to its
       actions and to the part it swallows FIXME.}}

    Given all this information a part will give you
    {{!actions}build actions} which by themselves define build
    {{!products}build products}. *)
module Part : sig

  (** {1 Kinds} *)

  type kind =
    [ `Base | `Unit | `Lib | `Bin | `Pkg | `Run | `Doc | `Dir | `Silo ]
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

  (** {1 Metadata}

      FIXME Meta data creation function is currently only exposed in
      the {{!Private.Part.meta_key}private API}. Not doing so would
      mean we need to reveal part of the private API (Key sets) and
      concepts not really relevant to the casual user. We need to
      decide whether creating new part with metadata is sufficently
      mundane to be worth it.  My take on this is rather no, you can
      put your metadata in the [actions] closure (it will also prevent
      it from being rewritten by [of_base] functions). Metadata is
      useful if we want to provide a module for a custom part and that
      other parts use this module directly (like the builtin part
      do). *)

  type meta
  (** The type for part metadata *)

  val meta_nil : meta
  (** [meta_nil] is metadata that cannot be accessed. *)

  (** {1 Parts} *)

  type +'a t = 'a part constraint 'a = [< kind]
  (** The type for parts. *)

  val v : ?usage:usage -> ?cond:bool Conf.value -> ?args:Args.t ->
    ?meta:meta -> ?needs:'a part list -> ?root:Path.rel Conf.value ->
    ?actions:(kind part -> Action.t list) ->
    ?check:(kind part -> bool) -> string -> [> `Base] part
  (** [v ?usage ?cond ?meta ?needs ?args ?actions ?check name] defines
      a base part named [name].
      {ul
      {- [usage] is the part's usage, default to
         [`Outcome].}
      {- [cond] is configuration condition for the part to
          exist, defaults to {!Conf.true_}.}
      {- [args] is an argument bundle added to the actions of the part,
         defaults to {!Args.empty}.}
      {- [meta] is the part's metadata, defaults to {!meta_nil}.}
      {- [needs] are the parts that are needed by the part, defaults to [[]],
         the given list is {!uniq}ified. }
      {- [root] is a build root for the part. Note that this may be altered
         later by swallowing parts. Usually best left unspecified unless
         you know what you are doing.}
      {- [actions] are the definition of the actions associated to the part
         build products should be generated by consulting the {!root} of
         part given to the function, not the [root] argument given to
         the constructor.}
      {- [check] is a diagnostic function that should {!Log} information
         about potential problems with the part and return [true] if there
         is no problem.}} *)

  val kind : 'a part -> kind
  (** [kind p] is [p]'s kind. *)

  val name : 'a part -> string
  (** [name p] is [p]'s name. *)

  val usage : 'a part -> usage
  (** [usage p] is [p]'s usage. *)

  val cond : 'a part -> bool Conf.value
  (** [cond p] determines if [p] is available. *)

  val args : 'a part -> Args.t
  (** [args p] is the argument bundle associated to part [p]. *)

  val meta : 'a part -> meta
  (** [meta p] is [p]'s metadata. *)

  val get_meta : (meta -> 'a option) -> 'b t -> 'a
  (** [get_meta proj p] uses [proj] on [p]'s metadata.

      @raise Invalid_argument if [proj] returns [None]. *)

  val needs : 'a part -> kind t list
  (** [needs p] is the (uniqified) list of parts needed by [p] to
      define itself. *)

  val actions : 'a part -> Action.t list
  (** [actions env p] are the actions to build part [p]. *)

  val products : ?exts:Path.ext list -> 'a part -> Path.t list Conf.value
  (** [products p] are the products of part [p]. If [exts] is present
      only those that have one of the extensions in the list are selected.
      This is derived from {!rules}. *)

  val check : 'a part -> bool
  (** [check p] logs information about potential problems with [p]
      and returns [true] if there is no such problem. *)

  val id : 'a part -> int
  (** [id p] is a unique id for the part. *)

  val equal : 'a part -> 'b part -> bool
  (** [equal p p'] is [(id p) = (id p')]. *)

  val compare : 'a part -> 'b part -> int
  (** [compare p p'] is [compare (id p) (id p')]. *)

  (** {1 Coercions} *)

  val coerce : ([< kind] as 'b) -> 'a part -> 'b part
  (** [coerce k p] coerces [p] to kind [k],

      @raise Invalid_argument if [p]'s kind is not [k]. *)

  val coerce_if : ([< kind] as 'b) -> 'a part -> 'b part option
  (** [coerce_if k p] is [Some] if [p] is of kind [k] and
      [None] otherwise. *)

  (** {1 Part lists} *)

  val list_products : ?exts:Path.ext list -> 'a t list ->
    Path.t list Conf.value
  (** [list_products ?exts ps] is the list of products defined by
      parts [ps]. If [exts] is present only those products with extensions
      in [exts] are kept. *)

  val list_uniq : kind part list -> kind part list
  (** [list_uniq ps] is [ps] with duplicates as determined by {!equal} removed.
      The list order is preserved. *)

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

  module Map : Map.S with type key = kind part
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

  val v : ?usage:Part.usage -> ?cond:bool Conf.value -> ?args:Args.t ->
    ?needs:[< `Lib | `Pkg] part list -> ?dir:Path.t Conf.value -> string ->
    kind -> [> `Unit] part
  (** [v ~needs ~dir name kind] is a compilation unit named [name],
      the name of the file without the suffix located in directory
      [dir] (defaults to {!root}) of kind [kind]. [needs] indicate the
      project libraries and packages that are needed to compile the
      unit. The [args] bundle is used by the unit's product actions
      according to context. *)

  val of_base : ?dir:Path.t Conf.value -> kind -> [`Base] part -> [> `Unit] part
  (** [of_base kind p] is a compilation unit from [p]. [p]'s
      name is the compilation unit name. See {!v}. *)
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

  val v : ?usage:Part.usage -> ?cond:bool Conf.value -> ?args:Args.t ->
    ?byte:bool -> ?native:bool -> ?native_dynlink:bool ->
    string -> kind -> [< `Unit | `Pkg | `Lib] part list -> [> `Lib] part
  (** [v ?byte ?native ?native_dynlink name kind needs] is a library
      named [name] of the given [kind]. [needs] has the compilation
      units [us] that define the libary. The package and libraries
      that are in [needs] are automatically added to [us] needs.  The
      [args] bundle is used both by the library's product actions and
      by [us] product actions according to context.

      The library's {e ability} to compile to different targets is
      specified by the arguments [?byte], [?native] and
      [?native_dynlink] whose defaults respectively depend on [kind]
      as follows.
      {ul
      {- [`OCaml], [true], [true], [true]}
      {- [`OCaml_pp], [true], [false], [false]}
      {- [`C], [false] (not applicable), [true], [true]}}
      Whether the products associated to a compilation target are
      concretly build depends on the configuration keys
      {!Conf.ocaml_byte}, {!Conf.ocaml_native},
      {!Conf.ocaml_native_dynlink} and {!Conf.c_dynlink}. *)

  val of_base : ?byte:bool -> ?native:bool -> ?native_dynlink:bool ->
    kind -> [`Base] part -> [> `Lib] part
  (** [of_base kind p] is a library from [p]. [p]'s name is the library name.
      See {!v}. *)
end

(** Binary executable part.

    Defines a program executable's build products by gathering a set of
    {{!Unit}unit parts}.

    FIXME should we really call this Bin ? maybe Exec ? Prog ? *)
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
    ?cond:bool Conf.value ->
    ?args:Args.t ->
    ?byte:bool -> ?native:bool -> ?js:bool ->
    string -> kind -> [< `Unit | `Lib | `Pkg ] part list -> [> `Bin] part
  (** [v ?byte ?native ?js name kind needs] is a binary named [name]
      of the given [kind]. [needs] has the compilation units [us] that
      define the binary. The package and libraries that are in [needs]
      are automatically added to [us] needs and used at link time. The
      [args] bundle is used both by the binary's product actions
      and by [us] product actions according to context.

      The binary's {e ability} to compile to different targets is specified
      by the arguments [?byte], [?native] and [?js] whose defaults
      respectively depend on [kind] as follows.
      {ul
      {- [`OCaml], [true], [true], [false].}
      {- [`OCaml_toplevel], [true], [false], [false].}
      {- [`C], [false] (not applicable), [true], [false]}}
      Whether the products associated to a compilation target are
      concretly build dependson the configuration keys {!Conf.ocaml_byte},
      {!Conf.ocaml_native}, {!Conf.ocaml_js} and {!Conf.c_js}. *)

  val of_base : ?byte:bool -> ?native:bool -> ?js:bool -> kind ->
    [< `Base] part -> [> `Bin] part
  (** [of_base kind p] is a binary from [p]. [p]'s name is the binary name.
      See {!v}. *)
end

(** Package part.

    Packages are named entities that provide an argument bundle via
    the {!Pkg.lookup} function. This bundle is usually determined by
    an an external lookup mecanism.  Certain parts
    (e.g. {{!Unit}units}) use these arguments when packages are
    specified in their needs. *)
module Pkg : sig

  (** {1 Metadata} *)

  type other = [ `Other of string * Args.t ]
  (** The type for other package lookup mecanisms. The string is a
      name for the mecanism. The argument bundle defines what the
      mecanism yields. *)

  type kind = [ `OCaml of [`OCamlfind | other ]
              | `C of [ `Pkg_config | other ]]
  (** The type for package kinds.
      {ul
      {- [`OCaml] is for OCaml packages looked up either through
         [`OCamlfind] or an [`Other] mecanism.}
      {- [`C] is for C packages looked up either through [`Pkg_config]
         or an [`Other] mecanism.}} *)

  val pp_kind : Format.formatter -> kind -> unit
  (** [pp_kind ppf k] prints an unspecified representation of [k] on
      [ppf]. *)

  val kind : [< `Pkg] part -> kind
  (** [kind p] is [p]'s package kind. *)

  val lookup : [< `Pkg] part -> Args.t
  (** [lookup p] is [p]'s package lookup argument. *)

  val ocaml : 'a part -> [> `Pkg] part option
  (** [ocaml p] is [Some p] iff [p] is an OCaml package. *)

  val c : 'a part -> [> `Pkg] part option
  (** [c p] is [Some p] iff [p] is a C package. *)

  (** {1 Packages} *)

  val v : ?usage:Part.usage -> ?cond:bool Conf.value -> ?args:Args.t ->
    string -> kind -> [> `Pkg] part
  (** [v name kind] is a package named [name] of the given [kind].
      [name] is the name used for lookuping up the package system.

      {b Note.} [args] is unused. FIXME should we add a context
      to ocamlfind and pkg-config invocations ? *)

  val of_base : kind -> [< `Base] part -> [> `Pkg] part
  (** [of_base kind p] is a package from [p]. [p]'s name is
      package name. *)
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

  (** {1 Unit filters} *)

  val default : [< `Unit] part -> bool
  (** [default] is a part filter that selects only OCaml units whose
      {!Unit.ocaml_interface} is not [`Hidden]. *)

  val dev : [< `Unit] part -> bool
  (** [dev] is part filter that select any kind of OCaml unit. *)

  (** {1 Unit documentation sets} *)

  val v : ?usage:Part.usage -> ?cond:bool Conf.value -> ?args:Args.t ->
    ?keep:([< `Unit] part -> bool) -> string -> kind ->
    [< `Unit | `Lib | `Bin]  part list -> [> `Doc] part
  (** [v keep name kind needs] is a documentation set named [name] of
      the given [kind]. The units of the documentation set are those
      kept by [keep] and present in [needs] or part of the libraries
      and binaries listed in [needs]. [keep] defaults to {!dev} if
      [usage] is [`Dev] and {!default} otherwise. *)

  val of_base : kind -> [< `Base] part -> [> `Doc ] part
  (** [of_base kind p] is a documentation set from [p]. [p]'s name is
      the documentation set name. *)
end

(** Directory part.

    A directory part defines a directory to gather a selection
    of the build products of a part. *)
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

  (** {1 Directory specifiers} *)

  type spec = Part.kind Part.t -> Action.product ->
    [ `Keep | `Rename of Path.t | `Drop] Conf.value
  (** The type for directory specifiers. Given a part and a product that
      belongs to the part, the function returns:
      {ul
      {- [`Drop] to drop the product from the directory.}
      {- [`Keep] to keep the product in the directory with its basename.}
      {- [`Rename p] to keep the product in the directory but rename it
       to [p], where [p] is a path relative to the directory}} *)

  val all : spec
  (** [all] is a specifier that [`Keep]s any product of any part. *)

  val file_exts : Path.ext list -> spec
  (** [file_exts exts] is a specifiers that keeps, in any part,
      products that have an extension in [exts]. *)

  val install_bin : spec
  (** [install_bin] is {!all} but has special behaviour in the following
      cases:
      {ul
      {- For [`Bin] parts of kind [`OCaml] keeps only one of the
         byte and native code executable without its extension. If
         both are available favors native code over byte code.}
      {- For [`Bin] parts of kind [`C] keeps only the part's
         executable.}} *)

  val install_lib : spec
  (** [install_lib] is {!all} but has special behaviour in the following
      cases :
      {ul
      {- For [`Lib] parts of kind [`OCaml], keeps only the library's
         archives and the [cmx], [cmi] and [cmti] files according to
         the unit's {{!Unit.ocaml_interface}interface specification}.}
      {- For [`Lib] parts of kind [`C], keeps only the library's
         archives.}} *)

  (** {1 Dir} *)

  val v : ?usage:Part.usage -> ?cond:bool Conf.value -> ?args:Args.t ->
    ?keep:spec -> ?install:bool -> kind -> 'a part list -> [> `Dir ] part
  (** [v keep install kind needs] is a directory of type [kind] that
      gathers the build products of [needs] as filtered by [keep]. If
      [install] is [true] (defaults) the directory structure is meant
      to be installed at a location defined by [kind].

      The default for [keep] depends both on [kind] and [install]
      as follows:
      {ul
      {- [`Bin], [true], {!install_bin} is used.}
      {- [`Lib], [true], {!install_lib} is used.}
      {- Otherwise, {!all} is used.}} *)

  val of_base : ?install:bool -> kind -> [> `Base] part -> [> `Dir] part
  (** [of_base ?install kind p] is a directory from [p]. See {!v}. *)
end

(** Parts for build silos.

    FIXME. I think this should be removed. *)
module Silo : sig

  (** {1 Create} *)

  val v : ?usage:Part.usage -> ?cond:bool Conf.value -> ?args:Args.t ->
    string -> 'a part list -> [> `Silo] part

  val of_base : [< `Base] part -> [> `Silo] part
end

(** Project runs.

    Project runs are build actions whose commands are always run
    even if the actions inputs are up to date. Strictly speaking
    they are not part of the build system but they may depend
    on elements that need to be built by the build system. *)
module Run : sig

  (** {1 Metadata} *)

  val dir : [< `Run] part -> Path.t Conf.value
  (** [[dir p] is the directory in which the run should occur.] *)

  (** {1 Create} *)

  val v : ?usage:Part.usage -> ?cond:bool Conf.value -> ?args:Args.t ->
    ?dir:Path.t Conf.value -> string -> Action.t -> [> `Run] part
  (** [v dir name act] is the run that executes [act] in directory
      [dir] (defaults to {!Conf.root_dir}). *)

  val of_base : ?dir:Path.t Conf.value -> [< `Base] part -> [> `Run] part
end

(** {1:spec Part specification combinators} *)

type path = Path.t Conf.value
(** The type for part paths specifications. *)

val root : path
(** [root] is the {{!Path.current}current directory} relative to the
    project root.  *)

val ( / ) : path -> string -> path
(** [path / seg] is [Conf.(const Path.( / ) $ path $ const seg)]. *)

val unit : ?usage:Part.usage -> ?cond:bool Conf.value -> ?args:Args.t ->
  ?needs:[< `Lib | `Pkg ] part list -> ?kind:Unit.kind -> ?dir:path -> string ->
  [> `Unit] part
(** See {!Unit.v}. [kind] defaults to [`OCaml (`Both, `Normal)]. *)

val lib : ?usage:Part.usage -> ?cond:bool Conf.value -> ?args:Args.t ->
  ?byte:bool -> ?native:bool -> ?native_dynlink:bool -> ?kind:Lib.kind ->
  string -> [< `Unit | `Pkg | `Lib] part list -> [> `Lib] part
(** See {!Lib.v}. [kind] defaults to [`OCaml]. *)

val bin : ?usage:Part.usage -> ?cond:bool Conf.value -> ?args:Args.t ->
  ?byte:bool -> ?native:bool -> ?js:bool -> ?kind:Bin.kind -> string ->
  [< `Unit | `Pkg | `Lib] part list -> [> `Bin] part
(** See {!Bin.v}. [kind] defaults to [`OCaml]. *)

val pkg : ?usage:Part.usage -> ?cond:bool Conf.value -> ?args:Args.t ->
  ?kind:Pkg.kind -> string -> [> `Pkg] part
(** See {!Pkg.v}. [kind] defaults to [`OCaml `OCamlfind]. *)

val doc : ?usage:Part.usage -> ?cond:bool Conf.value -> ?args:Args.t ->
  ?keep:([< `Unit] part -> bool) -> ?kind:Doc.kind -> string ->
  [< `Bin | `Lib | `Unit ] part list -> [> `Doc] part
(** See {!Doc.v}. [kind] defaults to [`OCamldoc]. *)

val dir : ?usage:Part.usage -> ?cond:bool Conf.value -> ?args:Args.t->
  ?keep:Dir.spec -> ?install:bool -> Dir.kind -> 'a part list -> [> `Dir ] part
(** See {!Dir.v}. *)

val silo : ?usage:Part.usage -> ?cond:bool Conf.value -> ?args:Args.t ->
  string -> 'a part list -> [> `Silo] part
(** See {!Silo.v} *)

val run : ?usage:Part.usage -> ?cond:bool Conf.value -> ?args:Args.t ->
  ?dir:path -> string -> Action.t -> [> `Run] part
(** See {!Run.v} *)

(** {1:projects Projects} *)

type project
(** The type for projects descriptions. *)

(** Project descriptions. *)
module Project : sig

  (** {1 Projects} *)

  type t = project
  (** The type for describing projects. *)

  val v : ?cond:bool Conf.value -> ?args:Args.t -> ?schemes:Conf.scheme list ->
      string -> parts:'a part list -> project
  (** [v cond args parts n] is the project named [n] with parts [parts].
      [cond] determines if the project can exist in a build configuration. *)

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

  (** {1 Private} *)

  (** Formatters. *)
  module Fmt : sig

    (** {1 Formatters} *)

    include module type of Fmt

    (** {1:utf8_cond Conditional UTF-8 formatting control} *)

    val utf8_enabled : unit -> bool
    (** [utf8_enabled ()] is [true] if UTF-8 pretty-printing is enabled. *)

    val set_utf8_enabled : bool -> unit
    (** [set_utf8_enabled b] sets UTF-8 pretty-printing to [b]. *)

    (** {1 Styled formatting control} *)

    type style_tags = [ `Ansi | `None ]
    (** The type for style tags.
        {ul
        {- [`Ansi], tags the text with
      {{:http://www.ecma-international.org/publications/standards/Ecma-048.htm}
           ANSI escape sequences}.}
        {- [`None], text remains untagged.}} *)

    val style_tags : unit -> style_tags
    (** [style_tags ()] is the current tag style used by {!Fmt.pp_styled}.
        Initial value is [`None]. *)

    val set_style_tags : style_tags -> unit
    (** [set_style_tags s] sets the current tag style used by
        {!Fmt.pp_styled}. *)
  end

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

      module Map : Map.S with type key = t
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

  (** Build argument bundles. *)
  module Args : sig

    (** {1 Argument bundles} *)

    include module type of Args with type t = Args.t

    val deps : t -> Conf.Key.Set.t
    (** [deps a] is the set of configuration keys which may be needed
        for evaluating the constituents of [a]. *)

    (** {1 Conditonalized arguments} *)

    type cargs
    (** The type for conditionalized arguments. *)

    val cargs_cond : cargs -> bool Conf.value
    (** [cargs_cond args] is [args]'s condition. *)

    val cargs_args : cargs -> string list Conf.value
    (** [cargs_args cargs] is [args]'s arguments. *)

    val cargs_deps : cargs -> Conf.Key.Set.t
    (** [cargs_deps cargs] is the set of configuration keys which may be needed
        for evaluating the constituents of [cargs]. *)

    val bindings : t -> (Ctx.t * cargs list) list
    (** [bindings args] is the list of bindings in [args]. *)

    val cargs_for_ctx : Ctx.t -> t -> cargs list
    (** [cargs_with_ctx ctx -> args] is the list of conditionalized arguments
        in [args] for context [ctx]. *)

    (** {1 Argument lookup} *)

    val for_ctx : Conf.t -> Ctx.t -> t -> string list
    (** [with_ctx conf ctx args] is the arguments in [args] for
        context [ctx] in configuration [conf]. *)
  end

  (** Build actions.

      {b Important.} Actions commands produce {e file} paths. Build system
      backends are in charge for making sure the {{!Path.dirname} directory
      name} of these paths exist before invoking the action. *)
  module Action : sig

    (** {1 Actions} *)

    include module type of Action with type t = Action.t
                                   and type cmd = Action.cmd
                                   and type cmds = Action.cmds

    val cond : t -> bool Conf.value
    (** [cond a] is [a]'s condition. *)

    val args : t -> Args.t
    (** [args a] is [a]'s argument bundle. *)

    val ctx : t -> Ctx.t
    (** [ctx a] is [a]'s context. *)

    val inputs : t -> Path.t list Conf.value
    (** [inputs a] is the list of products input by [a]'s action. *)

    val outputs : t -> Path.t list Conf.value
    (** [outputs a] is the list of products output by [a]'s action. *)

    val cmds : t -> cmds
    (** [cmds a] is [a]'s commands to generate outputs from the inputs. *)

    val deps : t -> Conf.Key.Set.t
    (** [deps a] is the set of configuration keys which may be needed
        for evaluating the constituents of [a]. *)

    (** {1 Commands} *)

    val cmd_cmd : cmd -> string
    (** [cmd_cmd c] is [c]'s executable. *)

    val cmd_args : cmd -> string list
    (** [cmd_args cmd] are [c]'s arguments. *)

    val cmd_ctx : Ctx.t -> cmd -> Ctx.t
    (** [cmd_ctx ctx cmd] adds [cmd]'s context to [ctx]. *)

    val cmd_args_with_ctx : Conf.t -> Ctx.t -> Args.t -> cmd ->
      string list
    (** [cmd_args_with_ctx conf ctx args cmd] are [c]'s arguments
        prepended with the arguments found in [args] for the
        configuration [conf] and a context that is [cmd_ctx ctx cmd]. *)

    val cmd_stdin : cmd -> Path.t option
    (** [cmd_stdin c] is [c]'s stdin redirection (if any). *)

    val cmd_stdout : cmd -> Path.t option
    (** [cmd_stdout c] is [c]'s stdout redirection (if any). *)

    val cmd_stderr : cmd -> Path.t option
    (** [cmd_stderr c] is [c]'s stderr redirection (if any). *)
  end

  (** Parts. *)
  module Part : sig

    (** {1 Part} *)

    include module type of Part with type kind = Part.kind
                                 and type meta = Part.meta
                                 and type +'a t = 'a Part.t
                                   constraint 'a = [< part_kind ]

    val ctx : 'a t -> Ctx.t
    (** [ctx p] is a context that describes [p] using {{!type:Ctx.part}part
        context elements}. *)

    val deps : 'a t -> Conf.Key.Set.t
    (** [deps a] is the set of configuration keys which may be needed
        for evaluating [a]. *)

    (** {1 Metadata} *)

    val meta_deps_none : 'a -> Conf.Key.Set.t
    (** [meta_deps_none] is a metadata dependency function that
        returns {!Conf.Key.Set.empty}. See {!meta_key}. *)

    val meta_key : ('a -> Conf.Key.Set.t) -> ('a -> meta) * (meta -> 'a option)
    (** [meta_key deps] pairs a metadata injector and projector. The function
        [deps] must, given a metadata value be able to compute the configuration
        keys (if any) which are needed for evaluating the elements of the
        metadata. *)
  end

  (** Projects.

      {b Important.} Project values may depend on the configuration
      it is the driver's responsability to set a project's configuration
      with {!with_conf} otherwise warnings are reported on configuration
      use. *)
  module Project : sig

    (** {1 Project} *)

    include module type of Project with type t = Project.t

    val cond : project -> bool Conf.value
    (** [cond p] is [p]'s cond. *)

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

    val products : ?root:bool -> project -> Path.Set.t
    (** [products p] is the set of build products. If [root]
        is [true] (default) includes root build products. *)

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
    let cond = Conf.(const Cmd.File.exists $ const (Path.file file)) in
     unit ~cond "src"
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
      their output statically, e.g. ocamldoc. FIXME what about dep
      discovery ? E.g. Dir parts need to know the products.}
   {- TODO}}
*)
