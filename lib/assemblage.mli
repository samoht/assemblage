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
  (** [pp ppf p] prints path on [ppf] using {!to_string}. *)

  (** {1 File extensions} *)

  type ext =
  [ `A | `Byte | `C | `Cma | `Cmi | `Cmo | `Cmt | `Cmti | `Cmx | `Cmxa
  | `Cmxs | `Css | `Dll | `Exe | `Gif | `H | `Html | `Install | `Img
  | `Jpeg | `Js | `Json | `Lib | `Md | `Ml | `Ml_dep | `Ml_pp | `Mli
  | `Mli_dep | `Mli_pp | `Native | `O | `Opt | `Png | `Sh | `So | `Tar
  | `Tbz | `Xml | `Zip
  | `Ext of string ]
  (** The type for file extensions. *)

  val ext_to_string : ext -> string
  (** [ext_to_string ext] is [ext] as a string. *)

  val ext_of_string : string -> ext
  (** [ext_of_string ext] is [ext] as a file extension. *)

  val has_ext : ext -> [< t ] -> bool
  (** [has_ext p ext] is [true] iff [p]'s last segment has file extension
      [ext]. *)

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

    TODO reword, say what a configuration is (map from keys to value)
    TODO add configuration schemes.

    Assemblage keeps tracks of the configuration keys needed to define
    a project and its build system through configuration values. A
    configuration value denotes a concrete OCaml value of a certain
    type and remembers the configuration keys it needs to define its value
    in a given configuration.

    Configuration keys are named configuration value and they are
    meant to be redefined by the end user of the build system
    (e.g. from the command line). A configuration key can be used to
    determine a configuration value or other configuration keys.

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

  val product_dir : Path.rel key
  (** [product_dir] is the path to the directory where current build product
      should be produced. This key is private and expressed relative to the
      {!root_dir}. *)

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
  (** [ocaml_native_dynlink] is [true] iff OCaml native code dynamic linking is
      requested (defaults to [true]). *)

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
      (defaults to ["ocaml-dumpast"].) *)

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
    action's command depends on the the end-user (through parts TODO), the
    action designer (the [context] argument of {!Action.v}), and
    the {{!Action.cmd}key name} of the command being executed. *)
module Ctx : sig

  (** {1:elements Context elements} *)

  type build_phase = [ `Prepare | `Gen | `Dep | `Pp | `Compile | `Archive
                     | `Link | `Doc ]
  (** The type for informing about build phases.
      {ul
      {- [`Prepare] source is being prepared. FIXME.}
      {- [`Gen] source or data is being generated.}
      {- [`Dep] source is analyzed for dependencies.}
      {- [`Pp] source is pre-processed.}
      {- [`Compile] source is compiled.}
      {- [`Archive] compilation products are being archived.}
      {- [`Link] compilation products are being linked into an executable.}
      {- [`Doc] documentation is being generated. FIXME}} *)

  type language = [ `OCaml | `C | `Js ]
  (** The type for informing about the broad type of input build products.
      {ul
      {- [`OCaml] working on OCaml related build products.}
      {- [`C] working on C releated build products.}
      {- [`Js] working on JavaScript related build products.}} *)

  type ocaml_source = [ `Ml | `Mli ]
  (** The type for informing about OCaml source products.
      {ul
      {- [`Ml] working on ml sources.}
      {- [`Mli] workong on mli sources.}} *)

  type ocaml_target = [ `Byte | `Native | `Js ]
  (** The type for informing about OCaml build targets.
      {ul
      {- [`Byte] working on byte code generation.}
      {- [`Native] working on native code generation.}
      {- [`Js] working on JavaScript code generation.}} *)

  type archive_product = [ `Shared ]
  (** The type for informing about archive build products.
      {ul
      {- [`Shared], working on generating a shared archive.}} *)

  type command = [ `Cmd of string Conf.key ]
  (** The type for informing about the command being executed.
      {ul
      {- [`Cmd k], the command [k] is being executed.}} *)

  type tag = [ `Tag of string ]
  (** The type for user defined tags. TODO part tags ? *)

  type elt = [ build_phase | language | ocaml_source | ocaml_target
             | archive_product | command | tag ]
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

type args

(** Build argument bundles.

    Argument bundles are conditional bindings from {{!Ctx}build contexts}
    to ordered lists of build command arguments. See {{!basics}argument
    bundle basics} for more details.  *)
module Args : sig

  (** {1:argument_bundles Argument bundles} *)

  type t = args
  (** The type for argument bundles. *)

  val v : ?cond:bool Conf.value -> Ctx.t -> string list Conf.value -> args
  (** [v ~cond ctx args] is the bundle that binds [ctx] to [args]
      whenever [cond] evaluates to [true] (defaults to {!Conf.true_}). *)

  val vc : ?cond:bool Conf.value -> Ctx.t -> string list -> args
  (** [vc ~active ctx args] is [v ~active ctx (Conf.const args)]. *)

  val empty : args
  (** [empty] is the empty bundle. *)

  val is_empty : args -> bool
  (** [is_empty a] is [true] if [a] is empty. *)

  val append : args -> args -> args
  (** [append a a'] is the bundle that has the bindings of [a] and [a'].
      If both [a] and [a'] have bindings for the same context, they
      are preserved each with their condition and it is guaranteed
      that the binding arguments of [a] will appear before those of
      [a'] on the command line. *)

  val (@@@) : args -> args -> args
  (** [a @@@ a'] is [append a a']. *)

  val concat : args list -> args
  (** [concat args] is [List.fold_left append empty args] *)

  (** {1:built_in Built-in argument bundles} *)

  val linkall : args
  (** [linkall] is the [-linkall] flag in the right [`OCaml] contexts. *)

  val thread : args
  (** [thread] is the [-thread] flag in the right [`OCaml] contexts. *)

  val vmthread : args
  (** [vmthread] is the [-vmthread] flag in the right [`OCaml] contexts. *)

  val cclib : string list -> args
  (** The [-cclib x] args. FIXME *)

  val ccopt : string list -> args
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
    FIX the FIXME, bundles should not be used for libs order. *)
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

  type product = Path.rel Conf.value
  (** The type for build products. A configuration value holding
      a file path. *)

  type products = Path.rel list Conf.value
  (** The type for lists of build products. A configuration value holding
      a list of file paths. *)

  (** {1 Build commands} *)

  type cmds
  (** The type for sequences of build commands. *)

  val cmd : ?stdin:product -> ?stdout:product ->
    ?stderr:product -> string Conf.key ->
    string list Conf.value -> cmds
  (** [cmd exec args] is the sequence that executes [exec] with
      argument [args]. The optional [stdin], [stdout], and [stderr]
      arguments allow to redirect the standard file descriptors of the
      execution to files. *)

  val seq : cmds -> cmds -> cmds
  (** [seq cmds cmds'] is the sequence of build commands made of [cmds]
      followed by [cmds']. *)

  val ( <*> ) : cmds -> cmds -> cmds
  (** [cmds <*> cmds'] is [seq cmds cmds']. *)

  (** {2:portable_invocations Portable system utility invocations}

      Rather than using {{!Conf.system_utility_keys}system utility
      configuration keys} directly you should use the following functions,
      they will enforce portable behaviour. *)

  val dev_null : Path.t Conf.value
  (** [dev_null] is a file that discards all writes. *)

  val cp : ?stdout:product -> ?stderr:product -> src:Path.t Conf.value ->
    dst:Path.t Conf.value -> cmds
  (** [cp ~src ~dst] copies file [src] to [dst]. *)

  val mv : ?stdout:product -> ?stderr:product -> src:Path.t Conf.value ->
    dst:Path.t Conf.value -> cmds
  (** [mv ~src ~dst] moves path [src] to [dst]. *)

  val rm_files : ?stdout:product -> ?stderr:product ->
    ?f:bool Conf.value -> Path.t list Conf.value -> cmds
  (** [rm_files ~f paths] removes the {e file} paths [paths].
      If [f] is [true] files are removed regardless of permissions
      (defaults to [false]). [paths] elements must be files, for
      directories, see {!rm_dirs}. *)

  val rm_dirs : ?stdout:product -> ?stderr:product -> ?f:bool Conf.value ->
    ?r:bool Conf.value -> Path.t list Conf.value -> cmds
  (** [rm_dirs ~f ~r paths] removes the {e directory} paths [paths].
      If [f] is [true] directories are removed regardless of permissions
      (defaults to [false]). If [r] is [true] removes the file hierarchies
      rooted at the elements of [paths]. Note that [paths] must be
      directories, for removing files, see {!rm_files}. *)

  val mkdir : ?stdout:product -> ?stderr:product -> Path.t Conf.value -> cmds
  (** [mkdir p] creates the directory [p]. Intermediate directories
      are created as required ([mkdir -p] in Unix parlance). *)

  (** {1 Build actions} *)

  type t
  (** The type for build actions. *)

  val v :
    ?cond:bool Conf.value ->
    ctx:Ctx.t ->
    inputs:products  ->
    outputs:products ->
    cmds -> t
   (** [v cond ctx inputs outputs cmds] is the action that given the
       existence of [inputs] creates the products [outputs] using the
       sequence of command [cmds].  The action is only available if
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

    val path : product -> ext:Path.ext -> Path.rel Conf.value
    (** [path p ~ext] is [p] with its extension (if any)
        {{!Path.change_ext}changed} to [ext]. *)

    val path_base : product -> string Conf.value
    (** [path_base p] is the {{!Path.basename}basename} of [p]. *)

    val path_dir : product -> Path.rel Conf.value
    (** [path_dir p] is the {{!Path.dirname}dirname} of [p]. *)

    val path_arg : ?opt:string -> Path.rel Conf.value -> string list_v
    (** [path_arg p] is the list made of [p] {{!Path.quote}converted}
        to a string. If [opt] is present it is added in front of the list. *)

    val paths_args : ?opt:string -> Path.rel list Conf.value ->
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

  (** Actions for handling OCaml products.

      All the actions have appropriate support for the {!Conf.debug},
      {!Conf.profile}, {!Conf.warn_error}, {!Conf.ocaml_annot}
      configuration keys.

      FIXME should we document contexts ? This is anyways going
      to be hidden behind parts. *)
  module OCaml : sig

    (** {1 Types} *)

    type includes = Path.rel list Conf.value
    (** The type for lists of include directories. A configuration value
        holding a list of directory paths. *)

    type name = Path.rel Conf.value
    (** The type for product names. A product names defines a build
        location through its {{!Path.dirname}dirname} and a name through its
        {e suffix-less} {{!Path.basename}basename}. *)

    (** {1 Preprocessing} *)

    val compile_src_ast : [`Ml | `Mli ] -> src:product -> t
    (** [compile_src_ast kind src] treats [src] of the given [ml] type
        and builds its AST. FIXME needs arguments for
        applying pp on the way ? *)

    (** {1 Compiling} *)

    val compile_mli : incs:includes -> src:product -> t
    (** [compile_mli ~incs ~src] compiles the mli file [src] (which can
        be an AST, see {!compile_src_ast}) with includes [incs]. *)

    val compile_ml_byte : has_mli:bool Conf.value -> incs:includes ->
      src:product -> t
    (** [compile_ml_byte ~has_mli ~incs ~src] compiles to byte code the ml
        file [src] (which can be an AST, see {!compile_src_ast}) with
        includes [incs]. [has_mli] indicates whether the source has a
        corresponding mli file. *)

    val compile_ml_native : has_mli:bool Conf.value ->
      incs:includes -> src:product -> t
    (** [compile_ml_native ~has_mli ~incs ~src] is like {!compile_ml_byte}
        but compiles to native code. *)

    val compile_c : src:product -> t
    (** [compile_c src] compiles the C [src] to native code through
        the OCaml compiler. *)

    (** {1 Archiving} *)

    val archive_byte : cmos:products -> name:name -> t
    (** [archive_byte cmos name] archives the byte code files [cmos]
        to a byte code archive (cma) named by [name]. *)

    val archive_native : cmx_s:products -> name:name -> t
    (** [archive_native cmx_s name] archives the native code files [cmx_s]
        to a native code archive (cmxa) named by [name]. *)

    val archive_shared : cmx_s:products -> name:name -> t
    (** [archive_shared cmx_s name] archives the native code files [cmx_s]
        to a native code shared archive (cmxs) named by [name]. *)

    val archive_c : objs:products -> name:name -> t
    (** [archive_c cmx_s name] archives the C object files [objs]
        to a C archive and shared archived (a, so) named by [name]. *)

    (** {1 Linking} *)

  end
end

(** {1:parts Parts} *)

type part_kind =
  [ `Base | `Unit | `Lib | `Bin | `Pkg | `Run | `Doc | `Dir | `Silo | `Custom ]
(** The type for part kinds. *)

type +'a part constraint 'a = [< part_kind ]
(** The type for project parts. *)

(** Parts.

    Parts describe logical build units. Example of parts are a
    compilation unit, a library, a binary, a test, the documentation
    of a library, etc. A part:

    {ul
    {- Defines a kind of logical build unit and has meta data
       that depends on this kind.}
    {- Defines a set of build rules that it derives from its
       meta data and through these rules a set of build products.}} *)
module Part : sig

  (** {1 Types} *)

  type kind =
    [ `Base | `Unit | `Lib | `Bin | `Pkg | `Run | `Doc | `Dir | `Silo
    | `Custom ]
  (** The type for part kinds. *)

  val kind_to_string : kind -> string
  (** [kind_to_string k] is an unspecified string representation of [k]. *)

  type part_kind = kind
  (** Another name for {!kind}. *)

  type +'a t = 'a part constraint 'a = [< kind]
  (** The type for parts. *)

  (** {1 Coercions} *)

  val coerce : ([< kind] as 'b) -> 'a t -> 'b t
  val coerce_if : ([< kind] as 'b) -> 'a t -> 'b t option

  (** {1 Base fields} *)

  val name : 'a t -> string
  (** [name p] is [p]'s name. *)

  val kind : 'a t -> kind
  (** [kind p] is [p]'s kind. *)

  val cond : 'a t -> bool Conf.value
  (** [cond p] determines if [p] is available. *)

  val args : 'a t -> args
  (** [args env p] are arguments associated to part [p]. *)

  val deps : 'a t -> kind t list
  (** [deps p] is [p]'s dependencies. *)

  val actions : 'a t -> Action.t list
  (** [actions env p] are the actions to build part [p] in the environment
      [env]. *)

  (** {1 Derived fields} *)

  val products : 'a t -> Path.rel list Conf.value
  (** [products p] are the products of part [p] in the environment
      [env]. This is derived from {!rules}. *)

  (** {1 Comparing} *)

  val equal : 'a t -> 'b t -> bool
  (** [equal p p'] is [true] if [p] and [p'] have the same
      {!kind} and {!name}. *)

  val compare : 'a t -> 'b t -> int
  (** [compare p p'] is a total order on [p] and [p']. *)

  (** {1 Part list operations} *)

  val keep : ('a t -> bool) -> 'a t list -> 'a t list
  val keep_kind : (kind as 'b) -> 'a t list -> 'b t list
  val keep_kinds : kind list -> 'a t list -> 'a t list
  val keep_map : ('a t -> 'b option) -> 'a t list -> 'b list
  val to_set : 'a t list -> 'a t list
  (** [to_set ps] is [ps] without duplicates as determined
      by {!equal}. *)

  (** {1 Specific parts} *)

  (** Parts for arbitrary products. *)
  module Base : sig
    val create :
      ?cond:bool Conf.value -> ?args:(kind t -> args) ->
      ?deps:'a t list -> string ->
      (kind t -> Action.t list) -> [> `Base] t
  end

  (** Parts for compilation units.

      Encapsulates rules to compile a compilation unit in a build
      directory defined by a {!Lib} or {!Bin} parent part. *)
  module Unit : sig

    (** {1 Metadata} *)

    type ocaml_interface = [ `Normal | `Opaque | `Hidden ]
    (** The type for OCaml compilation units interfaces.

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

    val kind : [< `Unit] t -> kind
    (** [kind u] is the kind of [u]. *)

    val src_dir : [< `Unit] t -> Path.rel
    (** [src_dir env u] is the directory where the unit [u] is located
        relative to the project directory. *)

    (** {1 Create} *)

    val create :
      ?cond:bool Conf.value -> ?args:args -> ?deps:'a t list ->
      ?src_dir:(Path.rel) -> string ->
      kind -> [> `Unit] t

    val of_base : src_dir:(Path.rel) -> kind -> [`Base] t -> [> `Unit] t
  end

  (** Parts for libraries.

      Ensapsulates rules to gather compilation {{!Unit}units} in a build
      directory and compile them to library archive files. *)
  module Lib : sig

    (** {1 Metadata} *)

    type kind = [ `OCaml | `OCaml_pp | `C ]
    (** The type for kinds of libraries. *)

    val kind : [< `Lib] t -> kind
    val byte : [< `Lib] t -> bool
    val native : [< `Lib] t -> bool
    val native_dynlink : [< `Lib] t -> bool

    (**  {1 Create} *)

    val create :
      ?cond:bool Conf.value -> ?args:args -> ?deps:part_kind t list ->
      ?byte:bool -> ?native:bool -> ?native_dynlink:bool ->
      string -> kind -> [< `Unit] t list -> [> `Lib] t
    (** [create available args deps byte native native_dynlink name kind us]
        is a library part named [name] made of units [us].

        Dependencies specified in [deps] and arguments in [args] are
        added to the units [us]. Dependencies specified in units
        [us] are added to the dependencies of the library part. *)

    val of_base :
      ?byte:bool -> ?native:bool -> ?native_dynlink:bool ->
      kind -> [`Base] t -> [> `Lib] t

    (** {1 Part filters} *)

    val ocaml : 'a t -> [> `Lib] t option
    (** [ocaml p] is [Some p] iff [p] is a library part and has kind
        [`OCaml]. *)

    val ocaml_pp : 'a t -> [> `Lib] t option
    (** [ocaml_pp p] is [Some p] iff [p] is a library part and has kind
        [`OCaml_pp]. *)

    val c : 'a t -> [> `Lib] t option
    (** [c p] is [Some p] iff [p] is a library part and has kind
        [`C]. *)
  end

  (** Parts for binaries. *)
  module Bin : sig

    (** {1 Metadata} *)

    type kind = [ `OCaml | `OCaml_toplevel | `C ]
    (** The type for kinds of binaries. *)

    val kind : [< `Bin] t -> kind
    val byte : [< `Bin] t -> bool
    val native : [< `Bin] t -> bool
    val js : [< `Bin] t -> bool

    (** {1 Create} *)

    val create :
      ?cond:bool Conf.value ->
      ?args:args ->
      ?deps:part_kind t list ->
      ?byte:bool -> ?native:bool -> ?js:bool ->
      string -> kind -> [< `Unit] t list -> [> `Bin] t
    (** [create available args deps byte native js name kind us]
        is a binary named [name] made of units [us].

        Dependencies specified in [deps] and arguments in [args] are
        added to the units [us]. *)

    val of_base : ?byte:bool -> ?native:bool -> ?js:bool -> kind ->
      [< `Base] t -> [> `Bin] t

    (** {1 As build action} *)

(*
    val cmd : ?args:args -> ?kind:[`Byte | `Native] -> [< `Bin] t ->
      (string list -> string list) -> Action.cmd
*)

    (** {1 Part filters} *)

    val ocaml : 'a t -> [> `Bin] t option
    (** [ocaml p] is [Some p] iff [p] is a binary part and has kind
        [`OCaml]. *)

    val ocaml_toplevel : 'a t -> [> `Bin] t option
    (** [ocaml_toplevel p] is [Some p] iff [p] is a binary part and has kind
        [`OCaml_toplevel]. *)

    val c : 'a t -> [> `Bin] t option
    (** [c p] is [Some p] iff [p] is a binary part and has kind
        [`C]. *)
  end

  (** Packages.

      Packages are named entities that provide command arguments in
      certain contexts through their {!args} attribute. These
      arguments are determined by an external lookup mecanism.

      Certain parts (e.g. {{!Unit}units}) use these arguments when
      packages are specified in their dependencies. *)
  module Pkg : sig

    (** {1 Metadata} *)

    type kind = [ `OCaml | `C ]
    (** The type for package kinds.
        {ul
        {- [`OCaml] is for OCaml packages to be used in pre-preprocessing,
           compilation, and linking contexts}
        {- [`C] is for C packages to be used in compilation and linking
           contexts.}} *)

    val kind : [< `Pkg] t -> kind
    (** [kind p] is [p]'s package kind. *)

    (** {1 Create} *)

    type ocaml_lookup = [ `OCamlfind ]
    (** The type for ocaml package lookup mechanisms. The name of
        the part is the OCamlfind package name to lookup. *)

    type c_lookup = [ `Pkg_config ]
    (** The type for c package lookup mechanisms. The name of
        the part is the pkg-config package name to lookup. *)

    type spec = [ `OCaml of ocaml_lookup | `C of c_lookup ]
    (** The type for packages specification. *)

    val create : ?cond:bool Conf.value -> ?args:args -> string ->
      spec -> [> `Pkg] t

    val of_base : kind -> [< `Base] t -> [> `Pkg] t
    (** [of_base opt kind base] is a package from [base]. [base] should
        define an interesting {!args} function. *)

    (** {1 Part filters} *)

    val ocaml : 'a t -> [> `Pkg] t option
    (** [ocaml p] is [Some p] iff [p] is a package part and has kind
        [`OCaml]. *)

    val c : 'a t -> [> `Pkg] t option
    (** [c p] is [Some p] iff [p] is a package part and has kind
        [`C]. *)
  end

  (** Parts for command runs. *)
  module Run : sig

    (** {1 Metadata} *)

    val run_dir : [< `Run] t -> Path.t

    (** {1 Create} *)

    val create :
      ?cond:bool Conf.value ->
      ?args:args ->
      ?deps:'a t list ->
      ?run_dir:Path.t ->
      string -> (Action.t) -> [> `Run] t

    val of_base : ?run_dir:Path.t -> [< `Base] t -> [> `Run] t
  end

  (** Parts for documentation. *)
  module Doc : sig

    (** {1 Metadata} *)

    type kind = [ `OCamldoc ]
    val kind : [< `Doc] t -> [`OCamldoc ]

    (** {1 Create} *)

    val create :
      ?cond:bool Conf.value ->
      ?args:args ->
      ?deps:'a t list ->
      ?keep:([< `Unit] t -> bool) ->
      ?kind:kind -> string -> 'a t list -> [> `Doc] t

    val of_base : ?kind:kind -> [< `Base] t -> [> `Doc ] t

    (** {1 Documentation filters}. *)

    val default : [< `Unit] t -> bool
    val dev : [< `Unit] t -> bool
  end

  (** Parts for named directories of products. *)
  module Dir : sig

    (** {1 Metadata} *)

    type kind = [ `Lib | `Bin | `Sbin | `Toplevel | `Share | `Share_root
                | `Etc | `Doc | `Misc | `Stublibs | `Man | `Other of string ]

    val kind : [< `Dir] t -> kind
    (** {b Note.} This is derived from the part name. *)

    val install : [< `Dir] t -> bool

    (** {1 Dir} *)

    val create :
      ?cond:bool Conf.value ->
      ?args:args ->
      ?deps: 'a t list ->
      ?keep:('a t -> Path.t list) ->
      ?install:bool -> kind -> 'a t list -> [> `Dir ] t

    val of_base : ?install:bool -> [> `Base] t -> [> `Dir] t

    (** {1 Product filters} *)

    val default : 'a t -> Path.t list
  end

  (** Parts for build silos. *)
  module Silo : sig

    (** {1 Create} *)

    val create :
      ?cond:bool Conf.value ->
      ?args:args ->
      ?deps:'a t list ->
      string -> 'a t list -> [> `Silo] t

    val of_base : [< `Base] t -> [> `Silo] t
  end

  (** Parts for arbitrary products with custom metadata. *)
  module Custom : sig

    (** {1 Metadata} *)

    type data
    (** The type for custom metadata. *)

    val key : unit -> ('a -> data) * (data -> 'a option)
    (** [key ()] is a new data key. A function to inject a value
        in data, and to project a value from data. *)

    val data : [< `Custom] t -> data

    (** {1 Create} *)

    val of_base : data -> [< `Base ] t -> [> `Custom] t
  end
end

type path = string list
(** The type for paths relative to the root directory of the project. *)

val ( / ) : path -> string -> path
(** [path / seg] is [path @ [seg]]. *)

val unit : ?cond:bool Conf.value -> ?args:args -> ?deps:Part.kind part list ->
  ?kind:Part.Unit.kind -> ?dir:path -> string -> [> `Unit] part
(** See {!Part.Unit.create}. [kind] defaults to [`OCaml (`Both, `Normal)]. *)

val lib : ?cond:bool Conf.value -> ?args:args -> ?deps:Part.kind part list ->
  ?byte:bool -> ?native:bool -> ?native_dynlink:bool ->
  ?kind:Part.Lib.kind -> string -> [< `Unit] part list -> [> `Lib] part
(** See {!Part.Lib.create}. [kind] defaults to [`OCaml]. *)

val bin : ?cond:bool Conf.value -> ?args:args -> ?deps:Part.kind part list ->
  ?byte:bool -> ?native:bool -> ?js:bool -> ?kind:Part.Bin.kind -> string ->
  [< `Unit] part list -> [> `Bin] part
(** See {!Part.Bin.create}. [kind] defaults to [`OCaml]. *)

val pkg : ?cond:bool Conf.value -> ?args:args -> ?kind:Part.Pkg.spec -> string ->
  [> `Pkg] part
(** See {!Part.Pkg.create}. [kind] defaults to [`OCaml `OCamlfind]. *)

val run : ?cond:bool Conf.value -> ?args:args -> ?deps:'a part list ->
  ?dir:path -> string -> (Action.t) -> [> `Run] part

val doc : ?cond:bool Conf.value -> ?args:args -> ?deps:'a part list ->
  ?keep:([< `Unit] part -> bool) ->
  ?kind:Part.Doc.kind -> string -> 'a part list -> [> `Doc] part

val dir : ?cond:bool Conf.value -> ?args:args -> ?deps:'a part list ->
  ?keep:('a part -> Path.t list) ->
  ?install:bool -> Part.Dir.kind -> 'a part list -> [> `Dir ] part

val silo : ?cond:bool Conf.value -> ?args:args -> ?deps:'a part list ->
  string -> 'a part list -> [> `Silo] part

(** {1:projects Projects} *)

type project
(** The type for projects descriptions. *)

(** Project descriptions. *)
module Project : sig

  (** {1 Projects} *)

  type t = project
  (** The type for describing projects. *)

  val v : ?cond:bool Conf.value -> ?args:args -> ?actions:Action.t list ->
    string -> 'a part list -> project
  (** [v cond args cs n] is the project named [n] with components [cs].
      [cond] determines if the project can exist in a build configuration.

      FIXME [args]. *)

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

    include module type of Conf
    with type 'a value = 'a Conf.value
     and type 'a key = 'a Conf.key

    (** {1:keys Keys} *)

    (** Configuration keys. *)
    module Key : sig

      (** {1 Existential keys} *)

      (** The type for existential keys. *)
      type t = V : 'a key -> t

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

      module Set : Set.S with type elt = t
      module Map : Map.S with type key = t
    end

    (** {1:configurations Configurations} *)

    type t
    (** The type for configurations. *)

    val empty : t
    (** [empty] is the empty configuration. *)

    val is_empty : t -> bool
    (** [is_empty c] is [true] iff [c] is empty. *)

    val add : t -> 'a key -> t
    (** [add c k] adds key [k] to configuration [c]. *)

    val set : t -> 'a key -> 'a value -> t
    (** [set c k v] sets the key [k] to [v] in [c]. *)

    val merge : t -> t -> t
    (** [merge c c'] merge the configuration [c] and [c']. If a key is
        defined in both [c] and [c'] the value of the key in [c'] takes
        over. *)

    val find : t -> 'a key -> 'a value option
    (** [find c k] is the value of [k] in [c] (if any). *)

    val get : t -> 'a key -> 'a value
    (** [get c k] is the value of [k] in [c].

        @raise Invalid_argument if [k] is not in [c]. *)

    val pp : Format.formatter -> t -> unit
    (** [pp ppf c] prints an unspecified representation of [c] on [ppf]. *)

    val of_keys : Key.Set.t -> t
    (** [of_keys ks] is a configuration where each key of [ks] maps to
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

  (** Actions.

      {b Important.} Actions commands produce {e file} paths. Build system
      backends are in charge for making sure the {{!Path.dirname} directory
      name} of these paths exist before invoking the action. *)
  module Action : sig

    (** {1 Actions} *)

    include module type of Action with type t = Action.t
                                   and type cmds = Action.cmds

    val ctx : t -> Ctx.t
    (** [ctx a] is [a]'s context. *)

    val inputs : t -> Path.rel list Conf.value
    (** [inputs a] is the list of products input by [a]'s action. *)

    val outputs : t -> Path.rel list Conf.value
    (** [outputs a] is the list of products output by [a]'s action. *)

    val cmds : t -> cmds
    (** [cmds a] is [a]'s commands to generate outputs from the inputs. *)
  end

  (** Projects.

      {b Important.} Project values may depend on the configuration
      it is the driver's responsability to set a project's configuration
      with {!with_conf} otherwise warnings are reported on configuration
      use. *)
  module Project : sig

    (** {1 Project} *)

    include module type of Project with type t = Project.t

    (** {1 Static attributes} *)

    val cond : project -> bool Conf.value
    (** [cond p] is [p]'s cond. *)

    val args : project -> args
    (** [args p] is [p]'s args. *)

    val parts : project -> part_kind part list
    (** [parts p] is [p]'s parts. *)

    val actions : project -> Action.t list

    (** {1 Configuration} *)

    val default_conf : project -> Conf.t
    (** [default_conf p] is the configuration needed to define [p] as
        derived from the project's definition. *)

    val conf : project -> Conf.t
    (** [conf p] is the project's configuration. *)

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

    An assemblage {{!project}project} is made of {{!parts}parts}.


    {1:dont Don't}

    Make the project depend on direct conditions. E.g.
{[
    if Sys.file_exists file then Some (unit "src")  else None
]}

    That's the way of doing it:
{[
    let cond = Conf.(const Cmd.File.exists $ const (Path.file file)) in
     unit ~cond "src"
]}

    TODO the rule to hammer in people's mind is the following:
    don't do anything that doesn't make the set of configuration
    keys constant on *every* run of the [assemble.ml] file.
*)
