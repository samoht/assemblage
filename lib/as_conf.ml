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

(* Types *)

(* A key is a name for a value. It has a default value, a converter to
   interact with the end user and the conversion functions to a
   universal value. The concrete value of a key is defined by the map
   of a configuration. *)

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

(* Configuration error messages *)

let pp_key_dup ppf (Key.V k) = As_fmt.pp ppf
    "Key name `%s'@ not unique@ in@ the@ configuration.@ \
     This@ may@ lead@ to@ unexpected@ driver@ behaviour." (Key.name k)

let pp_miss_key ppf (Key.V k) = As_fmt.pp ppf
    "Key@ `%s'@ not@ found@ in@ configuration@ (driver@ error).@ \
     The@ key's@ default@ value@ will@ be@ used." (Key.name k)

(* Key creation and value. *)

let key_id =
  let count = ref (-1) in
  fun () -> incr count; !count

let key ?(public = true) ?docs ?docv ?doc name converter default =
  let id = key_id () in
  let to_univ, of_univ = Univ.create () in
  { Def.id; name; public; converter; default; doc; docv; docs;
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

(* Configuration value converters
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

let utility_key docs bin_str =
  let doc = str "The %s utility." bin_str in
  key bin_str string (const bin_str) ~doc ~docv:"BIN" ~docs

(* Build property keys *)

let docs = "BUILD PROPERTIES"

let debug =
  let doc = "Build products with debugging support." in
  key "debug" bool (const false) ~docs ~doc ~docv:"BOOL"

let profile =
  let doc = "Build products with profiling support." in
  key "profile" bool (const false) ~docs ~doc ~docv:"BOOL"

let warn_error =
  let doc = "Try to treat tool warnings as errors." in
  key "warn-error" bool (const true) ~doc ~docv:"BOOL" ~docs

let test =
  let doc = "Build test build products." in
  key "test" bool (const false) ~docs ~doc ~docv:"BOOL"

let doc =
  let doc = "Build documentation." in
  key "doc" bool (const false) ~docs ~doc ~docv:"BOOL"

let jobs =
  let doc = "Number of jobs to run for building (defaults to machine processor
             count)." in
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
  key "jobs" int get_jobs ~docs ~doc ~docv:"COUNT"

(* Build directories keys *)

let docs = "DIRECTORIES"

let root_dir =
  let doc = "Absolute path to the project directory." in
  let get_cwd () = As_path.(to_abs (of_string (Sys.getcwd ()))) in
  let get_cwd = const get_cwd $ const () in
  key "root-dir" path get_cwd ~docs ~doc ~docv:"PATH"

let build_dir =
  let doc = "Path to the build directory expressed relative the root \
             directory (see $(b,--root-dir))."
  in
  let build_dir = const (As_path.dir "_build") in
  key "build-dir" rel_path build_dir ~docs ~doc ~docv:"PATH"

let product_dir =
  key "product-dir" rel_path ~public:false ~docs (value build_dir)

(* OCaml system keys *)

let docs = "OCAML SYSTEM"

let ocaml_native_tools =
  let doc = "true to use the native compiled (.opt) OCaml tools." in
  key "ocaml-native-tools" bool (const true) ~doc ~docv:"BOOL" ~docs

let ocaml_byte =
  let doc = "true if OCaml byte code compilation is requested." in
  key "ocaml-byte" bool (const true) ~doc ~docv:"BOOL" ~docs

let ocaml_native =
  let doc = "true if OCaml native code compilation is requested." in
  key "ocaml-native" bool (const true) ~doc ~docv:"BOOL" ~docs

let ocaml_native_dynlink =
  let doc = "true if OCaml native code dynamic linking is requested." in
  key "ocaml-native-dynlink" bool (const true) ~doc ~docv:"BOOL" ~docs

let ocaml_js =
  let doc = "true if OCaml JavaScript compilation is requested." in
  key "ocaml-js" bool (const false) ~doc ~docv:"BOOL" ~docs

let ocaml_annot =
  let doc = "true if OCaml binary annotation files is requested." in
  key "ocaml-annot" bool (const true) ~doc ~docv:"BOOL" ~docs

let ocaml_utility_key = utility_key docs
let ocaml_utility_key_maybe_opt bin_str =
  let doc = str "The %s utility." bin_str in
  let bin native = if native then str "%s.opt" bin_str else bin_str in
  let bin = const bin $ value ocaml_native_tools in
  key bin_str string bin ~doc ~docv:"BIN" ~docs

let ocaml_pp = ocaml_utility_key "ocaml_pp"
let ocamlc = ocaml_utility_key_maybe_opt "ocamlc"
let ocamlopt = ocaml_utility_key_maybe_opt "ocamlopt"
let js_of_ocaml = ocaml_utility_key "js_of_ocaml"
let ocamldep = ocaml_utility_key_maybe_opt "ocamldep"
let ocamlmklib = ocaml_utility_key"ocamlmklib"
let ocamldoc = ocaml_utility_key_maybe_opt "ocamldoc"
let ocamllex = ocaml_utility_key_maybe_opt "ocamllex"
let ocamlyacc = ocaml_utility_key "ocamlyacc"
let ocaml = ocaml_utility_key "ocaml"
let ocamlrun = ocaml_utility_key "ocamlrun"
let ocamldebug = ocaml_utility_key "ocamldebug"
let ocamlprof = ocaml_utility_key "ocamlprof"
let ocamlfind = ocaml_utility_key "ocamlfind"
let opam = ocaml_utility_key "opam"
let opam_installer = ocaml_utility_key "opam-installer"
let opam_admin = ocaml_utility_key "opam-admin"

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
  key "ocaml-version" version get_version ~doc ~docv:"VERSION" ~docs

(* Basic system utilities *)

let docs = "BASE SYSTEM"
let base_utility_key = utility_key docs

let echo = base_utility_key "echo"
let ln = base_utility_key "ln"
let cp = base_utility_key "cp"
let mkdir = base_utility_key "mkdir"
let cat = base_utility_key "cat"
let make = base_utility_key "make"

(* C system keys *)

let docs = "C SYSTEM"
let c_utility_key = utility_key docs

let cc = c_utility_key "cc"
let pkg_config = c_utility_key "pkg-config"

(* Machine information keys *)

let docs = "MACHINE INFORMATION"
let machine_info_key = utility_key docs

let uname = machine_info_key "uname"
let os =
  let doc = "The operating system name. Default inferred by lowercasing the
             result of invoking the tool specified by option $(b,--uname) with
             option `-s'."
  in
  let get_os uname =
    As_cmd.on_error ~use:"unknown" @@
    As_cmd.(input uname [ "-s" ] >>| String.trim >>| String.lowercase)
  in
  let get_os = const get_os $ value uname in
  key "os" string get_os ~doc ~docs ~docv:"STRING"

let arch =
  let doc = "The hardware architecture to compile for. Default inferred
             by lowercasing the result of invoking the tool specified by
             option $(b,--uname) with option `-m'."
  in
  let get_arch uname =
    As_cmd.on_error ~use:"unknown" @@
    As_cmd.(input uname [ "-m" ] >>| String.trim >>| String.lowercase)
  in
  let get_arch = const get_arch $ value uname in
  key "arch" string get_arch ~doc ~docs ~docv:"STRING"