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

let parser (p, _) = p
let printer (_, p) = p

(* Typed keys and values are related through existential key maps to
   universal types. We try to make that recursive definition as
   compact and simple as possible here.  *)

module rec Def : sig

  type conf = As_univ.t Kmap.t
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
      to_univ : 'a value -> As_univ.t;          (* injection to a univ type. *)
      of_univ : As_univ.t -> 'a value option;(* projection from a univ type. *)
      doc : string option;                                    (* doc string. *)
      docv : string option;          (* doc meta-variable for the key value. *)
      docs : string option; }                    (* doc section for the key. *)

  type t = V : 'a key -> t         (* Existential to hide the key parameter. *)
  val compare : t -> t -> int
end = struct
  type conf = As_univ.t Kmap.t
  type 'a value = Kset.t * (conf -> 'a)
  type 'a key =
    { id : int; name : string; public : bool;
      converter : 'a converter; default : 'a value;
      to_univ : 'a value -> As_univ.t; of_univ : As_univ.t -> 'a value option;
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
let neg a = const (fun v -> not v) $ a
let ( ||| ) a b = const ( || ) $ a $ b
let ( &&& ) a b = const ( && ) $ a $ b
let pick_if c a b = const (fun c a b -> if c then a else b) $ c $ a $ b

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
    "Key@ `%s'@ not@ found@ in@ configuration.@ \
     The@ key's@ default@ value@ will@ be@ used." (Key.name k)

let docs_project = "Project keys" (* defined here for [key], see below *)

let key_id =
  let count = ref (-1) in
  fun () -> incr count; !count

let key ?(public = true) ?(docs = docs_project) ?docv ?doc name
    converter default
  =
  let id = key_id () in
  let to_univ, of_univ = As_univ.create () in
  { Def.id; name; public; converter; default; doc; docv; docs = Some docs;
    to_univ; of_univ }

let value k =
  let deps, _ = Key.default k in
  let deps = Kset.add (Key.V k) deps in
  let v c = match try Some (Kmap.find (Key.V k) c) with Not_found -> None with
  | None ->
      As_log.msg_driver_fault As_log.Warning  "%a" pp_miss_key (Key.V k);
      (snd (Key.default k)) c
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
let mem c k = Kmap.mem (Key.V k) c
let add c k = Kmap.add (Key.V k) Key.(to_univ k (default k)) c
let set c k v = Kmap.add (Key.V k) (Key.to_univ k v) c
let rem c k = Kmap.remove (Key.V k) c
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

let pp ppf c =
  let cmp (Key.V k0, _) (Key.V k1, _) = compare (Key.name k0) (Key.name k1) in
  let defs = List.sort cmp (Kmap.bindings c) in
  let pp_binding ppf (Key.V k, v) =
    let v = match Key.of_univ k v with None -> assert false | Some v -> v in
    let v = eval c v in
    As_fmt.pp ppf "@[%s@ = @[%a@]@]"
      (Key.name k) (printer (Key.converter k)) v
  in
  As_fmt.pp ppf "@[<v>%a@]" (As_fmt.pp_list pp_binding) defs

(* Configuration schemes *)

type scheme = string * (string * t)
type def = D : 'a key * 'a -> def
let def k v = D (k, v)

let scheme ?doc ?base name defs =
  let doc = match doc with None -> str "The %s scheme." name | Some d -> d in
  let init = match base with None -> empty | Some (_, (_, conf)) -> conf in
  let add acc (D (k, v)) = set acc k (const v) in
  name, (doc, List.fold_left add init defs)

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
                     form [v|V]majoAr.minor[.patch][(+|-)info]." s
      in
      `Error err
  in
  let printer ppf (maj, min, patch, info) =
    let info = match info with None -> "" | Some info -> info in
    Format.fprintf ppf "%d.%d.%d%s" maj min patch info
  in
  parser, printer

(* Built-in configuration keys *)

open As_cmd.Infix

let err_det this = str "Could not determine %s." this

(* Build directories keys *)

let docs_build_directories = "Directory keys"
let doc_build_directories =
  "These keys inform the build system about directories."

let build_directories_key = key ~docs:docs_build_directories

let root_dir =
  let doc = "Absolute path to the project directory." in
  let get_cwd () =
    As_cmd.on_error ~use:As_path.root @@
    As_cmd.Dir.getcwd ()
  in
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

(* Project keys *)

let doc_project =
  "These keys inform the build system about properties specific to
   this project."

let project_key = key ~docs:docs_project (* defined above, [key] ~docs def. *)
let project_version =
  let doc = "Version of the project." in
  let vcs_info root =
    begin As_cmd.Vcs.find root >>= function
    | None ->
        As_log.warn "No VCS found to derive project version.";
        As_cmd.ret "unknown"
    | Some vcs ->
        As_cmd.Vcs.describe root vcs
    end
    |> As_cmd.reword_error (err_det "project version")
    |> As_cmd.on_error ~use:"unknown"
  in
  let vcs = const vcs_info $ (value root_dir) in
  project_key "project-version" string vcs ~doc ~docv:"VERSION"

(* Builtin configuration keys *)

let utility_key ?exec ~docs name =
  let exec = match exec with None -> const name | Some e -> e in
  let doc = str "The %s utility." name in
  key name string exec ~doc ~docv:"BIN" ~docs

(* Machine information keys *)

let docs_machine_information = "Machine information keys"
let doc_machine_information =
  "These keys inform the build system about the host and target machines."

let machine_info_key = key ~docs:docs_machine_information
let machine_info_utility ?exec =
  utility_key ?exec ~docs:docs_machine_information

let uname = machine_info_utility "uname"
let host_os =
  let doc = "The host machine operating system name. Defaults to `Win32' if
             the driver is running on native Windows otherwise it is the
             lowercased result of invoking the key $(b,uname) with `-s'."
  in
  let get_os uname = match Sys.os_type with
  | "Win32" -> "Win32"
  | _ ->
      As_cmd.read uname [ "-s" ] >>| String.lowercase
      |> As_cmd.reword_error (err_det "host machine operating system name")
      |> As_cmd.on_error ~use:"unknown"
  in
  let get_os = const get_os $ value uname in
  machine_info_key "host-os" string get_os ~doc ~docv:"STRING"

let host_arch =
  let doc = "The host machine hardware architecture. Defaults to the value of
             the driver's $(i,PROCESSOR_ARCHITECTURE) environment variable
             if it is running on native Windows otherwise it the lowercased
             result of invoking the key $(b,uname) with `-m'."
  in
  let get_arch uname =
    begin match Sys.os_type with
    | "Win32" -> As_cmd.get_env "PROCESSOR_ARCHITECTURE"
    | _ -> As_cmd.read uname [ "-m" ] >>| String.lowercase
    end
    |> As_cmd.reword_error (err_det "host machine hardware architecture")
    |> As_cmd.on_error ~use:"unknown"
  in
  let get_arch = const get_arch $ value uname in
  machine_info_key "host-arch" string get_arch ~doc ~docv:"STRING"

let host_word_size =
  let doc = "The host machine word size in bits. Defaults to the word size of
             the driver."
  in
  machine_info_key "host-word-size" int (const Sys.word_size) ~doc ~docv:"INT"

let target_os =
  let doc = "The target machine operating system name. Defaults to the host
             machine operating system name."
  in
  machine_info_key "target-os" string (value host_os) ~doc ~docv:"STRING"

let target_arch =
  let doc = "The target machine hardware architecture. Defaults to the host
             machine hardware architecture."
  in
  machine_info_key "target-arch" string (value host_arch) ~doc ~docv:"STRING"

let target_word_size =
  let doc = "The target machine word size in bits. Default to the host machine
             word size."
  in
  let docv = "INT" in
  machine_info_key "target-word-size" int (value host_word_size) ~doc ~docv

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

let jobs = (* FIXME not really sure that belongs here. This would
              give a jobs values for being used to defined a build
              system, but it's when we invoke it that we want to
              give such a value (e.g. on `assemblage build`). Still
              this is a portable way of finding out such a value. *)
  let doc = "Number of jobs to run for building." in
  let get_jobs () =
    begin match Sys.os_type with
    | "Win32" -> As_cmd.get_env "NUMBER_OF_PROCESSORS" >>= (parser int)
    | _ -> As_cmd.read "getconf" [ "_NPROCESSORS_ONLN" ] >>= (parser int)
    end
    |> As_cmd.reword_error (err_det "the number of host processors")
    |> As_cmd.on_error ~level:As_log.Warning (* or Info ? *) ~use:1
  in
  let get_jobs = const get_jobs $ const () in
  build_properties_key "jobs" int get_jobs ~doc ~docv:"COUNT"

(* OCaml system keys *)

let docs_ocaml_system = "OCaml system keys"
let doc_ocaml_system =
  "These keys inform the build system about the OCaml system and
   desired compilation outcomes."

let ocaml_system_key = key ~docs:docs_ocaml_system
let ocaml_system_utility ?exec = utility_key ?exec ~docs:docs_ocaml_system

let ocaml_byte =
  let doc = "true if OCaml byte code compilation is requested." in
  ocaml_system_key "ocaml-byte" bool (const true) ~doc ~docv:"BOOL"

let ocaml_native =
  let doc = "true if OCaml native code compilation is requested." in
  ocaml_system_key "ocaml-native" bool (const true) ~doc ~docv:"BOOL"

let ocaml_native_dynlink =
  let doc = "true if OCaml native code dynamic linking compilation \
             is requested."
  in
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

let ocaml_system_utility_maybe_opt exec =
  let doc = str "The %s utility." exec in
  let bin native = if native then str "%s.opt" exec else exec in
  let bin = const bin $ value ocaml_native_tools in
  ocaml_system_key exec string bin ~doc ~docv:"BIN"

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
  let tool = pick_if (value ocaml_native) (value ocamlopt) (value ocamlc) in
  let get_version tool =
    As_cmd.read tool [ "-version" ] >>= (parser version)
    |> As_cmd.reword_error (err_det "OCaml compiler version")
    |> As_cmd.on_error ~level:As_log.Warning ~use:(0, 0, 0, Some "-unknown")
  in
  let get_version = const get_version $ tool in
  ocaml_system_key "ocaml-version" version get_version ~doc ~docv:"VERSION"

(* C system keys *)

let docs_c_system = "C system keys"
let doc_c_system =
  "These keys inform the build system about the C system."

let c_system_key = key ~docs:docs_c_system
let c_system_utility ?exec = utility_key ?exec ~docs:docs_c_system

let c_dynlink =
  let doc = "true if C dynamic linking compilation is requested." in
  c_system_key "c-dynlink" bool (const true) ~doc ~docv:"BOOL"

let c_js =
  let doc = "true if C JavaScript compilation is requested." in
  c_system_key "c-js" bool (const true) ~doc ~docv:"BOOL"

let cc = c_system_utility "cc"
let pkg_config = c_system_utility "pkg-config"

(* System utility keys

   Win32, see http://www.covingtoninnovations.com/mc/winforunix.html *)

let docs_system_utilities = "System utility keys"
let doc_system_utilities =
  "These keys inform the build system about the system utilities to use."

let system_utilities_key = key ~docs:docs_system_utilities
let system_utilities_utility ?win32 name =
  let exec = match win32 with
  | None -> None
  | Some win32 ->
      let exec = function "Win32" -> win32 | _ -> name in
      Some (const exec $ (value host_os))
  in
  utility_key ~docs:docs_system_utilities ?exec name

let ln = system_utilities_utility "ln" (* FIXME windows *)
let cp = system_utilities_utility "cp" ~win32:"copy"
let mv = system_utilities_utility "mv" ~win32:"move"
let cd = system_utilities_utility "cd"
let rm = system_utilities_utility "rm" ~win32:"del"
let rmdir = system_utilities_utility "rmdir"
let mkdir = system_utilities_utility "mkdir"
let cat = system_utilities_utility "cat" ~win32:"type"
let make = system_utilities_utility "make"
