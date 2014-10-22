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
   compact and simple as possible and here. Concrete modules and types
   with further explanations are defined later. *)

type 'a key_ =                           (* typed keys to any kind of value. *)
  { id : int;                                    (* a unique id for the key. *)
    name : string;                                   (* the name of the key. *)
    public : bool;          (* true if value can be defined by the end user. *)
    converter : 'a converter;        (* key value parser and pretty printer. *)
    default_value : 'a;                        (* default value for the key. *)
    to_univ : 'a -> Univ.t;                (* injection to a universal type. *)
    of_univ : Univ.t -> 'a option;      (* projection from a universal type. *)
    doc : string option;                                      (* doc string. *)
    docv : string option;            (* doc meta-variable for the key value. *)
    docs : string option; }                      (* doc section for the key. *)

type key_value = Default | Value of Univ.t

module rec Def : sig
  type conf = key_value Kmap.t                             (* configuration. *)
  type 'a value = conf * (conf -> 'a)        (* value, see 'a Value.t below. *)
  type t = V : 'a value key_ -> t    (* existential of a typed key to value. *)
  val compare : t -> t -> int
end = struct
  type conf = key_value Kmap.t
  type 'a value = conf * (conf -> 'a)
  type t = V : 'a value key_ -> t
  let compare (V k0) (V k1) = (compare : int -> int -> int) k0.id k1.id
end

and Kmap : (Map.S with type key = Def.t) = Map.Make (Def)

(* Configurations *)

module Conf = struct
  include Kmap
  let merge =                       (* the right map overrides the left one. *)
    let choose _ l r = match l, r with
    | (Some _ as v), None | None, (Some _ as v) -> v
    | Some _, (Some _ as v) -> v
    | None, None -> None
    in
    Kmap.merge choose

  let add c k v = add k v c
  let find c k = find k c
end

type t = Def.conf

(* Configuration values *)

module Value = struct
  type 'a t = Def.conf * (Def.conf -> 'a)
  (* The first component of a value of type 'a Def.value is a map that
     keeps, in its domain, track of the configuration keys we access;
     the range of the map should be [`Default]. The second component
     is a function that given an actual configuration yields a
     value. *)

  let const v = Conf.empty, fun _ -> v
  let app (cf, f) (cv, v) = Conf.merge cf cf, fun c -> (f c) (v c)
  let deps (c, _) = c
  let eval c (_, v) = v c
end

type 'a value = 'a Value.t
let const = Value.const
let app = Value.app
let ( $ ) = app
let deps = Value.deps
let eval = Value.eval

(* Configuration keys *)

type 'a key = 'a value key_

module Key = struct

  (* Existential key (needs to be exposed to drivers) *)

  type t = Def.t = V : 'a key -> t

  let equal (V k0) (V k1) = (k0.id : int) = (k1.id : int)
  let compare (V k0) (V k1) = (compare : int -> int -> int) k0.id k1.id

  (* Typed key *)

  let value_converter_of_converter (parse, print) =
    let parse s = match parse s with
    | `Ok v -> `Ok (Value.const v)
    | `Error _ as e -> e
    in
    let print ppf v = print ppf (Value.eval (Value.deps v) v) in
    parse, print

  let id =
    let count = ref (-1) in
    fun () -> incr count; !count

  let create ?(public = true) ?docs ?docv ?doc name converter default_value =
    let id = id () in
    let converter = value_converter_of_converter converter in
    let to_univ, of_univ = Univ.create () in
    { id; name; public; converter; default_value; doc; docv; docs;
      to_univ; of_univ }

  let name k = k.name
  let public k = k.public
  let converter k = k.converter
  let default_value k : 'a Value.t =
    let deps, v = k.default_value in
    Conf.add deps (V k) Default, v

  let doc k = k.doc
  let docv k = k.docv
  let docs k = k.docs
  let of_univ k = k.of_univ
  let to_univ k = k.to_univ

  let value k =
    let deps, default_value = k.default_value in
    let deps = Conf.add deps (V k) Default in
    let v c = match try Some (Conf.find c (V k)) with Not_found -> None with
    | None ->
        As_log.warn
          "Key@ `%s'@ not@ found@ in@ configuration@ (driver@ error).@ \
           Using@ the@ key's@ default@ value@ (`%a')"
            k.name (snd k.converter) k.default_value;
        snd (k.default_value) c
    | Some Default -> snd (k.default_value) c
    | Some Value u ->
        match k.of_univ u with
        | Some (_, v) -> v c
        | None -> assert false
    in
    deps, v

end

let key = Key.create
let value = Key.value

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

let version =
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

(* Configurations *)

let empty = Conf.empty
let is_empty = Conf.is_empty
let add c k = Conf.add c (Key.V k) Default
let set c k v = Conf.add c (Key.V k) (Value (Key.to_univ k v))
let find c k =
  try match Conf.find c (Key.V k) with
  | Default -> Some (Key.default_value k)
  | Value v ->
      match Key.of_univ k v with
      | Some (deps, v) -> Some (Conf.add deps (Key.V k) Default, v)
      | None -> assert false
  with Not_found -> None

let get c k = match find c k with
| None -> invalid_arg (str "no key named %s in configuration" (Key.name k))
| Some v -> v

let merge = Conf.merge
(*
let fold f acc c = Kset.fold (fun k acc -> f acc k) c acc
let iter = Kset.iter
let exists = Kset.exists
let keep = Kset.filter
let subset = Kset.subset
let diff = Kset.diff
let keys = Kset.elements
let name_dups c =
  let check_dup (seen, dups as acc) (Key.V k) =
    let name = Key.name k in
    if not (As_string.Set.mem name seen) then acc else
    As_string.Set.add name seen, add dups k
  in
  let _, dups = fold check_dup (As_string.Set.empty, empty) c in
  keys dups
*)

let ( + ) = add
let ( @ ) = get
let ( ++ ) = merge

let parse c =
  (* FIXME move that out of the module.
     FIXME need to consider deps. *)
  let add (Key.V k) _ acc =
    let c = Key.converter k in
    let v = Key.default_value k in
    let doc = Key.doc k in
    let docs = Key.docs k in
    let docv = Key.docv k in
    let i = Cmdliner.Arg.info [Key.name k] ?doc ?docv ?docs in
    let opt = Cmdliner.Arg.(value (opt c v & i)) in
    Cmdliner.Term.(pure set $ acc $ pure k $ opt)
  in
  Conf.fold add c (Cmdliner.Term.pure c)

(* Builtin configuration keys *)

let utility_key docs bin_str =
  let doc = "The %s utility." in
  key bin_str string (const bin_str) ~doc ~docv:"BIN" ~docs

(* Build property keys *)

let docs = "BUILD PROPERTIES"

let debug =
  let doc = "Build products with debugging support." in
  key "debug" bool (const false) ~docs ~doc ~docv:"BOOL"

let profile =
  let doc = "Build products with profiling support." in
  key "profile" bool (const false) ~docs ~doc ~docv:"BOOL"

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
          As_cmd.(input "getconf" [ "_NPROCESSORS_ONLN" ] >>| int_of_string)
      with Not_found | Failure _ -> 1
  in
  let get_jobs = const get_jobs $ const () in
  key "jobs" int get_jobs ~docs ~doc ~docv:"COUNT"

let builtin_build_props = empty + debug + profile + test + doc + jobs

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
  key "root-dir" rel_path build_dir ~docs ~doc ~docv:"PATH"

let product_dir =
  key "product-dir" rel_path ~public:false (value build_dir)

let builtin_build_dirs = empty + root_dir + build_dir + product_dir

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

let ocaml_warn_error =
  let doc = "true if OCaml compilers should treat warnings as errors." in
  key "ocaml-warn-error" bool (const true) ~doc ~docv:"BOOL" ~docs

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

let builtin_ocaml =
  empty + ocaml_native_tools + ocaml_byte + ocaml_native +
  ocaml_native_dynlink + ocaml_js + ocaml_annot + ocaml_warn_error +
  ocaml_pp + ocamlc + ocamlopt + js_of_ocaml + ocamldep + ocamlmklib +
  ocamldoc + ocamllex + ocamlyacc + ocaml + ocamlrun + ocamldebug + ocamlprof +
  ocamlfind + ocaml_version

(* Basic system utilities *)

let docs = "BASE SYSTEM"
let base_utility_key = utility_key docs

let echo = base_utility_key "echo"
let ln = base_utility_key "ln"
let cp = base_utility_key "cp"
let mkdir = base_utility_key "mkdir"
let cat = base_utility_key "cat"
let make = base_utility_key "make"

let builtin_base_utils = empty + echo + ln + cp + mkdir + cat + make

(* C system keys *)

let docs = "C SYSTEM"
let c_utility_key = utility_key docs

let cc = c_utility_key "cc"
let pkg_config = c_utility_key "pkg-config"
let builtin_c = empty + cc + pkg_config

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

let builtin_machine_info = empty + os + arch

let builtin_base =
  let ( +++ ) = Conf.merge in
  builtin_build_props +++ builtin_build_dirs +++ builtin_ocaml +++
  builtin_base_utils
