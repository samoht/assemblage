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

(** The Assemblage Library.

    [Assemblage] describes the structure of your OCaml project. Given
    a project description, specific assemblage drivers provide simple
    tools to setup, build and manage your project.

    Open the module to use it, this defines only modules, types and a
    few combinators in your scope to describe your project.

    Consult the {{!basics}basics}.

    {e Release %%VERSION%% — %%MAINTAINER%% } *)

(** {1:build Building} *)

type cond
type args
type rule
type env

(** File paths.

    {b Note.} Don't use {!Filename.current_dir_name} and
    {!Filename.parent_dir_name} in path segments. TODO
    should we raise [Invalid_argument] we could also normalize
    via the constructors. *)
module Path : sig

  (** {1:filepaths File paths} *)

  type filename = string
  (** The type for file names (basenames). *)

  type segs = private string list
  (** The type for lists of path segments. *)

  type rel = [`Rel of segs]
  (** The type for relative file paths. The empty list denotes the current
      directory. *)

  type abs = [`Abs of segs]
  (** The type for absolute file paths. The empty list denotes the root
      directory. *)

  type t = [ abs | rel ]
  (** The type for file paths. Either relative or absolute paths. *)

  val current : [> rel]
  (** [current] is the current directory for relative paths. *)

  val root : [> abs]
  (** [root] is the root directory for absolute paths. *)

  val is_current : [> rel] -> bool
  (** [is_current p] is true iff [p] is {!current}. *)

  val is_root : [> abs] -> bool
  (** [is_root p] is true iff [p] is {!root}. *)

  val is_rel : [> rel] -> bool
  (** [is_rel p] is [true] iff [p] is a relative path. *)

  val is_abs : [> abs] -> bool
  (** [is_abs p] is [true] iff [p] is an absolute path. *)

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

  val to_string : [< t ] -> string
  (** [to_string p] is the path [p] as a string. *)

  val of_string : string -> t
  (** [of_string s] is the string [s] as a path. *)

  (** {1 File system queries} *)

  val exists : [< t ] -> bool
  (** [exists p] is [true] iff file path [p] exists in the file system. *)

  val is_file : [< t ] -> bool
  (** [is_file p] is [true] iff [exists p] is [true] and is a file
      (vs a directory). *)

  val is_dir : [< t ] -> bool
  (** [is_dir p] is [true] iff [exists p] is [true] and
      if a directory (vs a file). *)

  (** {1 File extensions} *)

  type ext =
    [ `Ml_dep | `Mli_dep | `Ml | `Mli | `C | `H | `Js | `Cmi | `Cmo | `Cmx | `O
    | `Cmt | `Cmti | `Cma | `Cmxa | `Cmxs | `A | `So | `Byte | `Native
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

  val chop_ext : [< t ] -> t
  (** [chop_ext p] removes the extension of [p] (if any). *)

  val add_ext : [< t ] -> ext -> t
  (** [add_ext p ext] is [p] with [ext] concatenated to the {!basename}
      of [p]. *)

  val ( + ) : [< t ] -> ext -> t
  (** [p + ext] is [add_ext p e]. Left associative. *)
end

(** Build conditions.

    Build conditions denote boolean values determined by the build
    environment {e after} the project was setup. They are used to
    condition the presence of {{!Args}build arguments}, the
    existence of project {{!Part}parts} and the existence of part's
    {{!Product}build products}.

    Examples of conditions are: request byte code compilation, request
    native code compilation, request usage of the native compiled
    tools, presence of an optional package, request debug build support, etc. *)
module Cond : sig

  (** {1:conds Conditions} *)

  type t = cond
  (** The type for conditions. In a build environment a value of this type
      denotes a boolean value. *)

  val create : ?default:bool -> string -> doc:string -> cond
  (** [create default name doc] is an {{!atomic}atomic condition}
      named [name].  [default] (defaults to [true]) indicates the
      boolean value the condition takes if the build environment
      doesn't determine it. [doc] is used for documenting the
      condition, this should be a single, short, sentence starting
      with an uppercase letter and ending with a period. *)

  val true_ : cond
  (** [true_] is always true. *)

  val false_ : cond
  (** [false_] is always false. *)

  val neg : ?on:bool -> cond -> cond
  (** [neg c] is true [iff] [c] is false. If [on] is specified, negates
      if [on] is [true] but returns [true_] if [on] is [false]. *)

  val ( &&& ) : cond -> cond -> cond
  (** [c &&& c'] is true iff both [c] and [c'] are true. *)

  val ( ||| ) : cond -> cond -> cond
  (** [c ||| c'] is true iff either [c] or [c'] is true. *)

  (** {1:builtin Built-in conditions} *)

  val byte : cond
  (** [byte] is true iff byte code compilation is requested. *)

  val native : cond
  (** [native] is true iff native code compilation is requested. *)

  val native_dynlink : cond
  (** [native_dynlink] is true iff native code dynamic linking is requested. *)

  val native_tools : cond
  (** [native_tools] is true iff compilation with the native compiled
      OCaml toolchain is requested ([.opt] tools). *)

  val js : cond
  (** [js] is true iff JavaScript compilation is requested. *)

  val annot : cond
  (** [annot] is true iff binary annotation files must be built. *)

  val debug : cond
  (** [debug] is true iff builds must support debugging. *)

  val warn_error : cond
  (** [warn_error] is true iff builds must consider warnings as errors. *)

  val test : cond
  (** [test] is true iff tests must be built. *)

  val doc : cond
  (** [doc] is true iff the documentation must be built. *)

  (** {1:atomic Atomic conditions}

      Atomic conditions are the atoms of an {!cond} value that are
      assigned a truth value by the build environment. They correspond
      either to {{!section:builtin}built-in conditions} or conditions
      created with {!create}. The {!atoms} function allows to collect them
      from an arbitrary condition. *)

  type atom
  (** The type for atomic conditions. *)

  val name : atom -> string
  (** [name a] is the name of [a]. See {!create}. *)

  val default : atom -> bool
  (** [default a] is the default value of [a]. See {!create}. *)

  val atom_doc : atom -> string
  (** [atom_doc a] is the documentation of [a]. See {!create}. *)

  val eval : (atom * bool) list -> t -> bool
  (** [eval tbl c] evaluates condition [c] given the truth table
      [tbl]. If an atom of [c] does not appear in [tbl] it is
      assigned it's default value. *)

  (** {1:atomicsets Sets of atomic conditions} *)

  module Set : Set.S with type elt = atom
  (** Sets of atomic conditions. *)

  val atoms : t -> Set.t
  (** [atoms c] is the set of atomic conditions present in [c]. *)

  val builtin : Set.t
  (** [builtin] is the set of {{!builtin}built-in conditions}. *)

  (** {1:cnf Conditions in conjunctive normal form} *)

  type clause = [ `P of atom | `N of atom ] list
  (** The type for conjunctive normal form clauses. A disjunction of
      positive or negative atomic conditions. *)

  type cnf = [ `True | `False | `And of clause list ]
  (** The type for conditions in conjunctive normal form (CNF). Either
      [`True], [`False] or a conjunction of clauses. *)

  val cnf : t -> cnf
  (** [cnf c] is [c] as a conjunctive normal form *)
end

(** Build rule contexts.

    Build {{!Rule}rules} operate in a given context. This allows
    {{!Args}build arguments} to inject arguments on the command
    lines of a rule's action without having to rewrite the rules. *)
module Context : sig

  (** {1:contexts Contexts} *)

  type t =
    [ `Prepare
    | `Dep
    | `Pp of [`Byte | `Native | `Js | `C ]
    | `Compile of [`Intf | `Byte | `Native | `Js | `C ]
    | `Archive of [`Byte | `Native | `Shared | `C | `C_shared]
    | `Link of [`Byte | `Native | `Js | `C]
    | `Run of [`Byte | `Native | `Js | `C]
    | `Test
    | `Doc
    | `Other of string ]
  (** The type for build rule contexts. *)

  val to_string : t -> string
  (** [to_string c] is [c] as a string. *)
end

(** Build command arguments.

    Values of type {!t} denote a (possibly empty) partial command line
    for each possible {{!Context}build context}. In a given context
    the arguments can be present only if a {{!Cond}build condition} is
    true. *)
module Args : sig

  (** {1:args Arguments} *)

  type raw_args = string list
  (** The type for partial command lines. *)

  type t = args
  (** The type for contextualized partial command lines. *)

  val create : ?cond:cond -> Context.t -> raw_args -> args
  (** [create cond ctx args] is the partial command line [args] in the
      context [ctx]. This partial command line is only present
      whenever the condition [cond] is true (defaults to
      {!Cond.true_}). *)

  val empty : args
  (** [empty] is the command line [[]] for every context.  *)

  val append : args -> args -> args
  (** [append a a'] appends context wise [a'] to [a]. In each context
      [a] and [a'] remain present according to their own [cond] argument. *)

  val (@@@) : args -> args -> args
  (** [a @@@ a'] is [append a a']. *)

  val concat : args list -> args
  (** [concat args] is [List.fold_left append empty args] *)

  val get : Context.t -> args -> (cond * raw_args) list
  (** [get ctx args] is [args]'s list of conditional arguments in
      context [ctx]. *)

  (** {1 Built-in arguments} *)

  val debug : args
  (** [debug] is the debug flag in the right contexts, conditioned
      on {!Cond.debug}. *)

  val annot : args
  (** [annot] is the [-bin-annot] flag in the right contexts, conditioned
      on {!Cond.annot}. *)

  val warn_error : args
  (** [warn_error] is the [-warn-error] flag in the right contexts,
      conditioned on {!Cond.warn_error}. *)

  val linkall : args
  (** [linkall] is the [-linkall] flag in the right contexts. *)

  val thread : args
  (** [thread] is the [-thread] flag in the right contexts. *)

  val vmthread : args
  (** [vmthread] is the [-vmthread] flag in the right contexts. *)

  val cclib : string list -> args
  (** The [-cclib x] args. FIXME *)

  val ccopt : string list -> args
  (** The [-ccopt x] args. FIXME *)

  val stub : string -> t
  (** [stub s] adds {i -cclib -l[s] -dllib -l[s]} to the bytecode
      linking options and {i -cclib -l[s]} to the native linking
      options. FIXME *)
end

(** Build environments.

    Build environments allow to write {{!Rule}build rules} that adapt
    to the build environment and/or build backend. Projects parts
    themselves may alter the build environment (mainly the
    {!build_dir} argument). Using solely the build environment in your rule
    action improves the portability of your project's build system. *)
module Env : sig

  (** {1 Build environments} *)

  type t = env
  (** The type for build environments. *)

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
    ?build_dir:Path.rel ->
    ?root_dir:Path.t ->
    ?ocamlfind_pkgs:(string list -> args) ->
    ?pkg_config:(string list -> args) ->
    unit -> env

  (** {1 Directories} *)

  val root_dir : env -> Path.t
  (** [root_dir env] is the absolute path to the project directory. *)

  val build_dir : env -> Path.rel
  (** [build_dir env] is the path to the build directory relative
      to the {!root_dir}. *)

  val push_build_dir : env -> Path.rel -> env
  (** [push_build_dir env dir] is [env] with its {!build_dir} altered
      to [dir] expressed relative to [build_dir env]. *)

  (** {1 Program binaries} *)

  val ocamlc : env -> string
  (** [ocamlc env] is the [ocamlc] command line tool. *)

  val ocamlopt : env -> string
  (** [ocamlopt env] is the [ocamlopt] command line tool. *)

  val ocamldep : env -> string
  (** [ocamldep env] is the [ocamldep] command line tool. *)

  val ocamlmklib : env -> string
  (** [ocamlmklib env] is the [ocamlmklib] command line tool. *)

  val ocamldoc : env -> string
  (** [ocamldoc env] is the [ocamldoc] command line tool. *)

  val ocaml_pp : env -> string option
  (** [ocaml_pp env] is an OCaml pre-processor command line tool. *)

  val js_of_ocaml : env -> string
  (** [js_of_ocaml env] is the [js_of_ocaml] command line tool. *)

  val mkdir : env -> string
  (** [mkdir env] is the [mkdir] command line tool. *)

  val ln : env -> string
  (** [ln env] is the [ln] command line tool. *)

  (** {1 Package queries} *)

  val ocamlfind_pkgs : env -> string list -> args
  (** [ocamlfind_pkgs env pkgs] are arguments in the right context to use
      [pkgs]. They are found using the [ocamlfind] command line tool. *)

  val pkg_config : env -> string list -> args
  (** [pkg_config env pkgs] are arguments in the right context to use
      [pkgs]. They are found using the [pkg_config] command line tool. *)
end

(** Build products.

    Build products are the inputs and outputs of {{!Rule}build
    rules}.  *)
module Product : sig

  (** {1:products Build products} *)

  type t = [ `File of Path.rel | `Effect of string * Path.rel ] * cond
  (** The type for build products.
      {ul
      {- [`File f] produces the {e file} file [f] expressed
         relative to {!Env.root_dir}.}
      {- [`Effect (e, dir)] produces an unknown effect named [e] in
         the directory [dir] expressed relative to {!Env.root_dir}.}}

      The condition determines whether the product exists in a given
      build environment. *)

  val cond : t -> cond
  (** [cond t] is [snd t]. *)

  val target : t -> string
  (** [target p] is the path for path products and the effect name
      for effect products. *)

  val path : t -> Path.rel
  (** [path p] is the path for path products and the directory
      of the effect for effect products. *)

  val raw_path : t -> string
  (** [raw_path p] is [As_path.to_string (path p)]. *)

  val basename : t -> string
  (** [basename p] is [As_path.basename (path p)]. *)

  val dirname : t -> Path.rel
  (** [dirname p] is the dirname of the path for path products and
      the direcctory of the effect for effect products. *)

  (** {1 Predicates} *)

  val is_file : t -> bool
  (** [is_file p] is [true] iff [p] is a file product. *)

  val is_effect : t -> bool
  (** [is_effect p] is [true] iff [p] is an effect product. *)

  val has_ext : As_path.ext -> t -> bool
  (** [has_ext ext p] is [true] iff [p] is a file product and has
      extension [ext]. *)

  val keep_ext : As_path.ext -> t ->
    ([`File of As_path.rel ] * cond) option
  (** [keep_ext ext p] is [Some p] iff [has_ext ext p] is [true]. *)

  (** {1:args Converting to arguments} *)

  val target_to_args : ?pre:string list -> As_context.t list -> t -> As_args.t
  (** [target_to_args pre ctxs p] is [target p] prefixed by [pre]
      (defaults to [[]]) in contexts [ctxs] for product [p]. [p]'s
      condition is propagated in the arguments. *)

  val dirname_to_args : ?pre:string list -> As_context.t list -> t ->
    As_args.t
  (** [dirname_to_args pre ctxs p] is [dirname p] prefixed by [pre]
      (defaults to [[]]) in contexts [ctxs]. [p]'s condition is
      propagated in the arguments. *)
end

(** Build rules.

    A build rule determines how to output {{!Product}products} from
    a list of input products using an action, that is a sequence
    of command line invocations. *)
module Rule : sig

  (** {1 Action} *)

  type cmd = args * (string list -> string list)
  (** The type for command line invocations.

      Given a rule and command line [(args, cmd)], the command line is
      determined by getting the arguments for the rule context from
      [args], then they are conditioned according to the build
      environment and given to the function [cmd] which should return
      the command line. *)

  val cmd : string list -> cmd
  (** [cmd line] is [(Args.empty, fun _ -> line)]. *)

  type action = cmd list
  (** The type for actions. *)

  (** {1 Build rules} *)

  type t = rule
  (** The type for build rules. *)

  val create :
    context:Context.t ->
    inputs:Product.t list ->
    outputs:Product.t list ->
    action:action -> rule
  (** [rule context inputs outputs action] is the rule that given
      the products [inputs] creates the products [outputs] using
      the action [action].

      The {{!Product.cond}conditions} of the input products are
      added to each output product.

      See {!type:cmd} to understand how the actual command lines of
      the action are determined at build time.

      {b Warning.} To ensure determinism and parallelism correctness [action]
      must ensure that it only reads from the [inputs] and writes to
      [outputs].

      {b Important.} TODO For [`File] build products, build systems backends
      are in charge or producing the intermediate directories to the file. *)

  val context : rule -> Context.t
  (** [context r] is [r]'s context. *)

  val inputs : rule -> Product.t list
  (** [inputs r] is the list of products input by [r]'s action. *)

  val outputs : rule -> Product.t list
  (** [outputs r] is the list of products output by [r]'s action. *)

  val action : rule -> action
  (** [action r] is [r]'s action to generate outputs from the inputs. *)

  (** {1 Built-in rules} *)

  val link : ?cond:cond -> ?args:args -> env -> src:Path.rel -> dst:Path.rel ->
    rule
  (** [link cond args env ~src ~dst] is a rule with context [`Other
      "link"] that links the product [(`File src, cond)] to the
      product [(`File dst, cond)]. [src] and [dst] are expressed
      relative to {!Env.root_dir}[ env]. [cond] defaults to
      {!Cond.true_} and [args] to {!Args.empty}. *)

  val mkdir : ?cond:cond -> ?args:args -> env -> dir:Path.rel -> rule
  (** [mkdir env ~dir] is a rule with context [`Other "mkdir"] that creates
      the product [(`File dir, cond)]. [dir] is expressed realtive to
      {!Env.root_dir}[ env]. [cond] defaults to
      {!Cond.true_} and [args] to {!Args.empty}. *)
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

  val cond : 'a t -> cond
  (** [cond p] determines if [p] is available. *)

  val args : env -> 'a t -> args
  (** [args env p] are arguments associated to part [p] in the
      environment [env]. *)

  val deps : 'a t -> kind t list
  (** [deps p] is [p]'s dependencies. *)

  val rules : env -> 'a t -> Rule.t list
  (** [rules env p] are the rules associated to part [p] in the environment
      [env]. *)

  (** {1 Derived fields} *)

  val products : env -> 'a t -> Product.t list
  (** [products env p] are the products of part [p] in the environment
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
      ?cond:cond -> ?args:(env -> kind t -> args) ->
      ?deps:'a t list -> string ->
      (env -> kind t -> Rule.t list) -> [> `Base] t
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

    val src_dir : env -> [< `Unit] t -> Path.rel
    (** [src_dir env u] is the directory where the unit [u] is located
        relative to the project directory. *)

    (** {1 Create} *)

    val create :
      ?cond:cond -> ?args:args -> ?deps:'a t list ->
      ?src_dir:(env -> Path.rel) -> string ->
      kind -> [> `Unit] t

    val of_base : src_dir:(env -> Path.rel) -> kind -> [`Base] t -> [> `Unit] t
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
      ?cond:cond -> ?args:args -> ?deps:part_kind t list ->
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
      ?cond:cond ->
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

    val cmd : ?args:args -> ?kind:[`Byte | `Native] -> [< `Bin] t ->
      (string list -> string list) -> Rule.cmd

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

    val create : ?cond:cond -> ?args:args -> string ->
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
      ?cond:cond ->
      ?args:args ->
      ?deps:'a t list ->
      ?run_dir:Path.t ->
      string -> (env -> Rule.action) -> [> `Run] t

    val of_base : ?run_dir:Path.t -> [< `Base] t -> [> `Run] t
  end

  (** Parts for documentation. *)
  module Doc : sig

    (** {1 Metadata} *)

    type kind = [ `OCamldoc ]
    val kind : [< `Doc] t -> [`OCamldoc ]

    (** {1 Create} *)

    val create :
      ?cond:cond ->
      ?args:args ->
      ?deps:'a t list ->
      ?keep:(env -> [< `Unit] t -> bool) ->
      ?kind:kind -> string -> 'a t list -> [> `Doc] t

    val of_base : ?kind:kind -> [< `Base] t -> [> `Doc ] t

    (** {1 Documentation filters}. *)

    val default : env -> [< `Unit] t -> bool
    val dev : env -> [< `Unit] t -> bool
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
      ?cond:cond ->
      ?args:args ->
      ?deps: 'a t list ->
      ?keep:(env -> 'a t -> (Path.t * Product.t) list) ->
      ?install:bool -> kind -> 'a t list -> [> `Dir ] t

    val of_base : ?install:bool -> [> `Base] t -> [> `Dir] t

    (** {1 Product filters} *)

    val default : env -> 'a t -> (Path.t * Product.t) list
  end

  (** Parts for build silos. *)
  module Silo : sig

    (** {1 Create} *)

    val create :
      ?cond:cond ->
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

val unit : ?cond:cond -> ?args:args -> ?deps:Part.kind part list ->
  ?kind:Part.Unit.kind -> ?dir:path -> string -> [> `Unit] part
(** See {!Part.Unit.create}. [kind] defaults to [`OCaml (`Both, `Normal)]. *)

val lib : ?cond:cond -> ?args:args -> ?deps:Part.kind part list ->
  ?byte:bool -> ?native:bool -> ?native_dynlink:bool ->
  ?kind:Part.Lib.kind -> string -> [< `Unit] part list -> [> `Lib] part
(** See {!Part.Lib.create}. [kind] defaults to [`OCaml]. *)

val bin : ?cond:cond -> ?args:args -> ?deps:Part.kind part list ->
  ?byte:bool -> ?native:bool -> ?js:bool -> ?kind:Part.Bin.kind -> string ->
  [< `Unit] part list -> [> `Bin] part
(** See {!Part.Bin.create}. [kind] defaults to [`OCaml]. *)

val pkg : ?cond:cond -> ?args:args -> ?kind:Part.Pkg.spec -> string ->
  [> `Pkg] part
(** See {!Part.Pkg.create}. [kind] defaults to [`OCaml `OCamlfind]. *)

val run : ?cond:cond -> ?args:args -> ?deps:'a part list ->
  ?dir:path -> string -> (env -> Rule.action) -> [> `Run] part

val doc : ?cond:cond -> ?args:args -> ?deps:'a part list ->
  ?keep:(env -> [< `Unit] part -> bool) ->
  ?kind:Part.Doc.kind -> string -> 'a part list -> [> `Doc] part

val dir : ?cond:cond -> ?args:args -> ?deps:'a part list ->
  ?keep:(env -> 'a part -> (Path.t * Product.t) list) ->
  ?install:bool -> Part.Dir.kind -> 'a part list -> [> `Dir ] part

val silo : ?cond:cond -> ?args:args -> ?deps:'a part list ->
  string -> 'a part list -> [> `Silo] part

(** {1:projects Projects} *)

type project
(** The type for projects descriptions. *)

(** Project descriptions. *)
module Project : sig

  (** {1 Projects} *)

  type t = project
  (** The type for describing projects. *)

  val create : ?cond:cond -> ?args:args -> string ->
    'a part list -> project
  (** [create cs n] is the project named [n] with components [cs]. *)

  val name : project -> string
  (** [name p] is the [p]'s name. *)

  val parts : project -> part_kind part list
  (** [parts p] is [p]'s parts. *)

  val cond_atoms : project -> Cond.Set.t
  (** [cond_atoms proj] is the collection of features used in the project. *)
end

val assemble : project -> unit
(** [assemble p] registers [p] for assembling with an assemblage driver. *)

(**/**)
val projects : unit -> project list
(**/**)

(** {1 Miscellaneous} *)

(** String utilities and string sets.

    Adds {!String.split} and {!String.Set} to the OCaml String module. *)
module String : sig
  include module type of String

  val split : sep:string -> string -> string list
  (** [split sep s] is the list of all (possibly empty)
      substrings of [s] that are delimited by matches of the non empty
      separator string [sep].

      Matching separators in [s] starts from the beginning of [s] and once
      one is found, the separator is skipped and matching starts again
      (i.e. separator matches can't overlap). If there is no separator
      match in [s], [[s]] is returned.

      The invariants [String.concat sep (String.split sep s) = s] and
      [String.split sep s <> []] always hold.

      @raise Invalid_argument if [sep] is the empty string. *)


  (** {1 Sets of strings} *)

  module Set : sig
    include Set.S with type elt = string
    val of_list : string list -> t
  end
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

  (** {1:styled Styled formatting} *)

  type style_tags = [ `Ansi | `None ]
  (** The type for style tags.
      {ul
      {- [`Ansi], tags the text with
      {{:http://www.ecma-international.org/publications/standards/Ecma-048.htm}
      ANSI escape sequences}.}
      {- [`None], text remains untagged.}} *)

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
    | `White ]
  (** The type for styles. *)

  val style_tags : unit -> style_tags
  (** [style_tags ()] is the current tag style used by {!pp_styled}.
      Initial value is [`None]. *)

  val set_style_tags : style_tags -> unit
  (** [set_style_tags s] sets the current tag style used by {!pp_style}. *)

  val pp_styled : style -> 'a formatter -> 'a formatter
  (** [pp_styled style pp] formats according to [pp] but styled with [style]. *)

  val pp_styled_str : style -> string formatter
  (** [pp_styled_str style] is [pp_styled_str style pp_str]. *)
end

(** Assemblage log. *)
module Log : sig

  (** {1 Log level and output} *)

  (** The type for log levels. *)
  type level = Show | Marker | Error | Warning | Info | Debug

  val level : unit -> level option
  (** [level ()] is the log level (if any). If the log level is [(Some l)]
      any message whose level is [<= l] is logged. If level is [None]
      no message is ever logged. At startup the level is [(Some Info)]. *)

  val set_level : level option -> unit
  (** [set_level l] sets the log level to [l]. See {!level}. *)

  val set_formatter : [`All | `Level of level ] -> Format.formatter -> unit
  (** [set_formatter spec ppf] sets the formatter for a given level or
      for all the levels according to [spec]. At startup the formatter
      of level [Show] is {!Format.std_formatter} and all the other level
      formatters are {!Format.err_formatter}. *)

  (** {1 Logging} *)

  val msg : level -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [msg l fmt ...] logs a message with level [l]. *)

  val kmsg :
    (unit -> 'a) -> level -> ('b, Format.formatter, unit, 'a) format4 -> 'b
  (** [kmsg k l fmt ...] is like [msg l fmt] but calls [k ()] before
      returning. *)

  val show : ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [show fmt ...] logs a message with level [Show]. *)

  val mark : ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [mark fmt ...] logs  a message with level [Marker]. *)

  val err : ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [err fmt ...] logs a message with level [Error]. *)

  val warn : ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [warn fmt ...] logs a message with level [Warning]. *)

  val info : ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [info fmt ...] logs a message with level [Info]. *)

  val debug : ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [debug info ...] logs a message with level [Debug]. *)

  (** {1 Log monitoring} *)

  val err_count : unit -> int
  (** [err_count ()] is the number of messages logged with level [Error]. *)

  val warn_count : unit -> int
  (** [warn_count ()] is the number of messages logged with level [Warning]. *)
end

(** {1:basics Basics}

    An assemblage {{!project}project} is made of {{!parts}parts}.

    Describing a project eventually leads to describe its build
    artifacts. The exact mapping betweem project elements and those
    build artifacts depends on the presence or absence of
    {{!Features}features}. Examples of such features are weather to
    compile and run the tests (user-defined feature), the presence of
    the native compiler (system-dependant feature), compiling a
    library using [async] or [lwt] as a backend (environment-dependant
    feature), ...

    Finally, generating build artifacts means refining the project
    description into concrete {{!Args}command line arguments} to pass
    to the different compilers ([ocamlc], [ocamlopt]) on the different
    compilation and linking phases.

    {1 Examples}

    {ul
    {- Show how to create a package from Base that hooks into pcre-config}
    {- Show how to create a package from Base that runs a discover.ml}}
*)
