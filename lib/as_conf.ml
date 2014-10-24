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

let str = Printf.sprintf

(* Configuration value converters. *)

type 'a parser = string -> [ `Error of string | `Ok of 'a ]
type 'a printer = Format.formatter -> 'a -> unit
type 'a converter = 'a parser * 'a printer

(* Universal type, see http://mlton.org/UniversalType *)

module Univ : sig
  type t
  val create : unit -> (('a -> t) * (t -> 'a option))
end = struct
  type t = exn
  let create (type s) () =
    let module M = struct exception E of s option end in
    (fun x -> M.E (Some x)), (function M.E x -> x | _ -> None)
end

(* Typed keys and values are related through existential key maps to
   universal types. We try to make that recursive definition as
   compact and simple as possible here.  *)

module rec Def : sig

  type conf = Univ.t Kmap.t
  (* A configuration maps existential keys to their concrete value. *)

  type 'a value = Kset.t * (conf -> 'a)
  (* A value is made of a key set that tracks the configuration keys
     that are accessed and a function that returns the value for a
     configuration. Invariant: the key set should be included in the
     domain of the configuration passed to the function. Drivers are
     responsible for maintaing that when they use [eval], otherwise
     warnings are reported see the [value] function. *)

  type 'a key =                          (* typed keys to any kind of value. *)
    { id : int;                                  (* a unique id for the key. *)
      name : string;                                 (* the name of the key. *)
      public : bool;        (* true if value can be defined by the end user. *)
      converter : 'a converter;     (* value type parser and pretty printer. *)
      default : 'a value;                      (* default value for the key. *)
      to_univ : 'a value -> Univ.t;             (* injection to a univ type. *)
      of_univ : Univ.t -> 'a value option;   (* projection from a univ type. *)
      doc : string option;                                    (* doc string. *)
      docv : string option;          (* doc meta-variable for the key value. *)
      docs : string option; }                    (* doc section for the key. *)

  type t = V : 'a key -> t         (* Existential to hide the key parameter. *)
  val compare : t -> t -> int
end = struct
  type conf = Univ.t Kmap.t
  type 'a value = Kset.t * (conf -> 'a)
  type 'a key =
    { id : int; name : string; public : bool;
      converter : 'a converter; default : 'a value;
      to_univ : 'a value -> Univ.t; of_univ : Univ.t -> 'a value option;
      doc : string option; docv : string option; docs : string option; }

  type t = V : 'a key -> t
  let compare (V k0) (V k1) = (compare : int -> int -> int) k0.id k1.id
end

and Kmap : (Map.S with type key = Def.t) = Map.Make (Def)
and Kset : (Set.S with type elt = Def.t) = Set.Make (Def)

(* Configuration values *)

type 'a value = 'a Def.value

let const v = Kset.empty, fun _ -> v
let app (fdeps, f) (vdeps, v) = Kset.union fdeps vdeps, fun c -> (f c) (v c)
let deps (deps, _) = deps
let eval c (_, v) = v c
let ( $ ) = app

let true_ = const true
let false_ = const false
let ( ||| ) a b = const ( || ) $ a $ b
let ( &&& ) a b = const ( && ) $ a $ b

(* Configuration keys *)

type 'a key = 'a Def.key

module Key = struct

  (* Existential key *)

  type t = Def.t = V : 'a key -> t

  let equal (V k0) (V k1) = (k0.Def.id : int) = (k1.Def.id : int)
  let compare = Def.compare

  (* Typed key *)

  let id k = k.Def.id
  let name k = k.Def.name
  let public k = k.Def.public
  let converter k = k.Def.converter
  let default k = k.Def.default

  let doc k = k.Def.doc
  let docv k = k.Def.docv
  let docs k = k.Def.docs
  let of_univ k = k.Def.of_univ
  let to_univ k = k.Def.to_univ

  module Set = Kset
  module Map = Kmap
end

let pp_key_dup ppf (Key.V k) = As_fmt.pp ppf
    "Key name `%s'@ not unique@ in@ the@ configuration.@ \
     This@ may@ lead@ to@ unexpected@ driver@ behaviour." (Key.name k)

let pp_miss_key ppf (Key.V k) = As_fmt.pp ppf
    "Key@ `%s'@ not@ found@ in@ configuration@ (driver@ error).@ \
     The@ key's@ default@ value@ will@ be@ used." (Key.name k)

let docs_project = "Project keys"
let doc_project =
  "These keys inform the build system about properties specific to
   this project."

let key_id =
  let count = ref (-1) in
  fun () -> incr count; !count

let key ?(public = true) ?(docs = docs_project) ?docv ?doc name
    converter default
  =
  let id = key_id () in
  let to_univ, of_univ = Univ.create () in
  { Def.id; name; public; converter; default; doc; docv; docs = Some docs;
    to_univ; of_univ }

let value k =
  let deps, _ = Key.default k in
  let deps = Kset.add (Key.V k) deps in
  let v c = match try Some (Kmap.find (Key.V k) c) with Not_found -> None with
  | None -> As_log.warn "%a" pp_miss_key (Key.V k); (snd (Key.default k)) c
  | Some u ->
      match Key.of_univ k u with
      | Some (_, v) -> v c
      | None -> assert false
  in
  deps, v

(* Configurations *)

type t = Def.conf

let empty = Kmap.empty
let is_empty = Kmap.is_empty
let add c k = Kmap.add (Key.V k) Key.(to_univ k (default k)) c
let set c k v = Kmap.add (Key.V k) (Key.to_univ k v) c
let merge =
  let choose _ l r = match l, r with
  | (Some _ as v), None | None, (Some _ as v) -> v
  | Some _, (Some _ as v) -> v
  | None, None -> None
  in
  Kmap.merge choose

let find c k =
  try match Key.of_univ k (Kmap.find (Key.V k) c) with
  | Some (deps, v) -> Some (Kset.add (Key.V k) deps, v)
  | None -> assert false
  with Not_found -> None

let get c k = match find c k with
| None -> invalid_arg (str "no key named %s in configuration" (Key.name k))
| Some v -> v

let domain c =
  let add key _ acc = Kset.add key acc in
  Kmap.fold add c Kset.empty

let of_keys s =
  let add (Key.V kt as k) acc =
    let u = Key.(to_univ kt (default kt)) in
    Kmap.add k u acc
  in
  Kset.fold add s Kmap.empty

(* Bultin configuration value converters
   FIXME remove Cmdliner dep *)

let bool = Cmdliner.Arg.bool
let int = Cmdliner.Arg.int
let string = Cmdliner.Arg.string
let enum = Cmdliner.Arg.enum

let path =
  let parse s = `Ok (As_path.of_string s) in
  let print ppf v = As_fmt.pp_str ppf (As_path.to_string v) in
  parse, print

let path_kind check ok err =
  let parse s =
    let p = As_path.of_string s in
    if check p then `Ok (ok p) else `Error (err s)
  in
  let print ppf v = As_fmt.pp_str ppf (As_path.to_string v) in
  parse, print

let rel_path =
  let err s = str "`%s' is not a relative path." s in
  path_kind As_path.is_rel As_path.as_rel err

let abs_path =
  let err s = str "`%s' is not an absolute path." s in
  path_kind As_path.is_abs As_path.as_abs err

let version : (int * int * int * string option) converter =
  let parser s =
    try
      let slice = As_string.slice in
      let s = if s.[0] = 'v' || s.[0] = 'V' then slice ~start:1 s else s in
      let dot = try String.index s '.' with Not_found -> raise Exit in
      let maj, s = slice ~stop:dot s, slice ~start:(dot + 1) s in
      let dot = try Some (String.index s '.') with Not_found -> None in
      let sep =
        let plus = try Some (String.index s '+') with Not_found -> None in
        let minus = try Some (String.index s '-') with Not_found -> None in
        match plus, minus with
        | Some i0, Some i1 -> Some (min i0 i1)
        | Some i, None | None, Some i -> Some i
        | None, None -> None
      in
      let min, patch, info = match dot, sep with
      | Some dot, Some sep when dot < sep ->
          slice ~stop:dot s,
          slice ~start:(dot + 1) ~stop:sep s,
          Some (slice ~start:sep s)
      | (Some _ | None) , Some sep ->
          slice ~stop:sep s, "0", Some (slice ~start:sep s)
      | Some dot, None ->
          slice ~stop:dot s, slice ~start:(dot + 1) s, None
      | None, None ->
          s, "0", None
      in
      let maj = try int_of_string maj with Failure _ -> raise Exit in
      let min = try int_of_string min with Failure _ -> raise Exit in
      let patch = try int_of_string patch with Failure _ -> raise Exit in
      `Ok (maj, min, patch, info)
    with Exit ->
      let err = str "invalid value `%s', expected a version number of the \
                     form [v|V]major.minor[.patch][(+|-)info]." s
      in
      `Error err
  in
  let printer ppf (maj, min, patch, info) =
    let info = match info with None -> "" | Some info -> info in
    Format.fprintf ppf "%d.%d.%d%s" maj min patch info
  in
  parser, printer

(* Builtin configuration keys *)

let utility_key ~docs bin_str =
  let doc = str "The %s utility." bin_str in
  key bin_str string (const bin_str) ~doc ~docv:"BIN" ~docs

(* Project keys *)

let project_key = key ~docs:docs_project
let project_version =
  let doc = "Version of the project." in
  let v = const "TODO" in
  project_key "project-version" string v ~doc ~docv:"VERSION"

(* Build property keys *)

let docs_build_properties = "Build property keys"
let doc_build_properties =
  "These keys inform the build system about global build settings."

let build_properties_key = key ~docs:docs_build_properties

let debug =
  let doc = "Build products with debugging support." in
  build_properties_key "debug" bool (const false) ~doc ~docv:"BOOL"

let profile =
  let doc = "Build products with profiling support." in
  build_properties_key "profile" bool (const false) ~doc ~docv:"BOOL"

let warn_error =
  let doc = "Try to treat utility warnings as errors." in
  build_properties_key "warn-error" bool (const false) ~doc ~docv:"BOOL"

let test =
  let doc = "Build test build products." in
  build_properties_key "test" bool (const false) ~doc ~docv:"BOOL"

let doc =
  let doc = "Build documentation." in
  build_properties_key "doc" bool (const false) ~doc ~docv:"BOOL"

let jobs =
  let doc = "Number of jobs to run for building."
  in
  let get_jobs () =
    try match Sys.os_type with
    | "Win32" -> int_of_string (Sys.getenv "NUMBER_OF_PROCESSORS")
    | _ ->
        As_cmd.on_error ~use:1 @@
        As_cmd.(input "getconf" [ "_NPROCESSORS_ONLN" ] >>| String.trim >>|
                int_of_string)
    with Not_found | Failure _ -> 1
  in
  let get_jobs = const get_jobs $ const () in
  build_properties_key "jobs" int get_jobs ~doc ~docv:"COUNT"

(* Build directories keys *)

let docs_build_directories = "Directory keys"
let doc_build_directories =
  "These keys inform the build system about directories."

let build_directories_key = key ~docs:docs_build_directories

let root_dir =
  let doc = "Absolute path to the project directory." in
  let get_cwd () = As_path.(to_abs (of_string (Sys.getcwd ()))) in
  let get_cwd = const get_cwd $ const () in
  build_directories_key "root-dir" path get_cwd ~doc ~docv:"PATH"

let build_dir =
  let doc = "Path to the build directory expressed relative the root \
             directory (see $(b,--root-dir))."
  in
  let build_dir = const (As_path.dir "_build") in
  build_directories_key "build-dir" rel_path build_dir ~doc ~docv:"PATH"

let product_dir =
  build_directories_key "product-dir" rel_path ~public:false (value build_dir)

(* OCaml system keys *)

let docs_ocaml_system = "OCaml system keys"
let doc_ocaml_system =
  "These keys inform the build system about the OCaml system and
   desired compilation outcomes."

let ocaml_system_key = key ~docs:docs_ocaml_system
let ocaml_system_utility = utility_key ~docs:docs_ocaml_system

let ocaml_byte =
  let doc = "true if OCaml byte code compilation is requested." in
  ocaml_system_key "ocaml-byte" bool (const true) ~doc ~docv:"BOOL"

let ocaml_native =
  let doc = "true if OCaml native code compilation is requested." in
  ocaml_system_key "ocaml-native" bool (const true) ~doc ~docv:"BOOL"

let ocaml_native_dynlink =
  let doc = "true if OCaml native code dynamic linking is requested." in
  ocaml_system_key "ocaml-native-dynlink" bool (const true) ~doc ~docv:"BOOL"

let ocaml_js =
  let doc = "true if OCaml JavaScript compilation is requested." in
  ocaml_system_key "ocaml-js" bool (const false) ~doc ~docv:"BOOL"

let ocaml_annot =
  let doc = "true if OCaml binary annotation files is requested." in
  ocaml_system_key "ocaml-annot" bool (const true) ~doc ~docv:"BOOL"

let ocaml_build_ast =
  let doc = "Parse and dump the OCaml source AST using the tool specified
             with $(b,--ocaml-dumpast)."
  in
  ocaml_system_key "ocaml-build-ast" bool (const false) ~doc ~docv:"BOOL"

let ocaml_native_tools =
  let doc = "true to use the native compiled (.opt) OCaml tools." in
  ocaml_system_key "ocaml-native-tools" bool (const true) ~doc ~docv:"BOOL"

let ocaml_system_utility_maybe_opt bin_str =
  let doc = str "The %s utility." bin_str in
  let bin native = if native then str "%s.opt" bin_str else bin_str in
  let bin = const bin $ value ocaml_native_tools in
  ocaml_system_key bin_str string bin ~doc ~docv:"BIN"

let ocaml_dumpast = ocaml_system_utility "ocaml_dumpast"
let ocamlc = ocaml_system_utility_maybe_opt "ocamlc"
let ocamlopt = ocaml_system_utility_maybe_opt "ocamlopt"
let js_of_ocaml = ocaml_system_utility "js_of_ocaml"
let ocamldep = ocaml_system_utility_maybe_opt "ocamldep"
let ocamlmklib = ocaml_system_utility"ocamlmklib"
let ocamldoc = ocaml_system_utility_maybe_opt "ocamldoc"
let ocamllex = ocaml_system_utility_maybe_opt "ocamllex"
let ocamlyacc = ocaml_system_utility "ocamlyacc"
let ocaml = ocaml_system_utility "ocaml"
let ocamlrun = ocaml_system_utility "ocamlrun"
let ocamldebug = ocaml_system_utility "ocamldebug"
let ocamlprof = ocaml_system_utility "ocamlprof"
let ocamlfind = ocaml_system_utility "ocamlfind"
let opam = ocaml_system_utility "opam"
let opam_installer = ocaml_system_utility "opam-installer"
let opam_admin = ocaml_system_utility "opam-admin"

let ocaml_version =
  let doc = "The OCaml compiler version. Default inferred by invoking
             an OCaml compiler with `-version'."
  in
  let tool nat ocamlopt ocamlc = if nat then ocamlopt else ocamlc in
  let tool =
    const tool $ value ocaml_native_tools $ value ocamlopt $ value ocamlc
  in
  let get_version tool =
    As_cmd.on_error ~use:(0, 0, 0, Some "unknown") @@
    As_cmd.(input tool [ "-version" ] >>| String.trim >>= (fst version))
  in
  let get_version = const get_version $ tool in
  ocaml_system_key "ocaml-version" version get_version ~doc ~docv:"VERSION"

(* C system keys *)

let docs_c_system = "C system keys"
let doc_c_system =
  "These keys inform the build system about the C system."

let c_system_key = key ~docs:docs_c_system
let c_system_utility = utility_key ~docs:docs_c_system

let cc = c_system_utility "cc"
let pkg_config = c_system_utility "pkg-config"

(* Machine information keys *)

let docs_machine_information = "Machine information keys"
let doc_machine_information =
  "These keys inform the build system about the host and target machines. The
   defaults for the host machine are inferred by invoking the $(b,uname) key.
   The defaults for the target machine equate those of the host."

let machine_info_key = key ~docs:docs_machine_information
let machine_info_utility = utility_key ~docs:docs_machine_information

let uname = machine_info_utility "uname"
let host_os =
  let doc = "The host machine operating system name." in
  let get_os uname =
    As_cmd.on_error ~use:"unknown" @@
    As_cmd.(input uname [ "-s" ] >>| String.trim >>| String.lowercase)
  in
  let get_os = const get_os $ value uname in
  machine_info_key "host-os" string get_os ~doc ~docv:"STRING"

let host_arch =
  let doc = "The host machine hardware architecture." in
  let get_arch uname =
    As_cmd.on_error ~use:"unknown" @@
    As_cmd.(input uname [ "-m" ] >>| String.trim >>| String.lowercase)
  in
  let get_arch = const get_arch $ value uname in
  machine_info_key "host-arch" string get_arch ~doc ~docv:"STRING"

let target_os =
  let doc = "The target machine operating system name." in
  machine_info_key "target-os" string (value host_os) ~doc ~docv:"STRING"

let target_arch =
  let doc = "The target machine hardware architecture." in
  machine_info_key "target-arch" string (value host_arch) ~doc ~docv:"STRING"

(* System utility keys *)

let docs_system_utilities = "System utility keys"
let doc_system_utilities =
  "These keys inform the build system about the system utilities to use."

let system_utilities_key = key ~docs:docs_system_utilities
let system_utilities_utility = utility_key ~docs:docs_system_utilities

let echo = system_utilities_utility "echo"
let ln = system_utilities_utility "ln"
let cp = system_utilities_utility "cp"
let mkdir = system_utilities_utility "mkdir"
let cat = system_utilities_utility "cat"
let make = system_utilities_utility "make"
