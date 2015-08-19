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

open Astring
open Rresult
open Bos

(* Configuration value converters. *)

type 'a parser = string -> ('a, R.msg) result
type 'a printer = Format.formatter -> 'a -> unit
type 'a converter = 'a parser * 'a printer

let parser (p, _) = p
let printer (_, p) = p

(* Typed keys and values are related through existential key maps to
   universal types. We try to make that recursive definition as
   compact and simple as possible here.  *)

module rec Def : sig

  type conf = { cid : int; map : As_univ.t Kmap.t }
  (* A configuration maps existential keys to their concrete value.
     The id is there for caching purposes on evaluation. *)

  type 'a value =
    { deps : Kset.t;                        (* Dependencies of the value. *)
      def : conf -> 'a;           (* Definition in a given configuration. *)
      mutable cache :       (* cache, config id and value in that config. *)
        (int * 'a) option; }
  (* A value is made of a key set that tracks the configuration keys
     that are accessed and a function that returns the value for a
     configuration. Invariant: the key set should be included in the
     domain of the configuration passed to the function. Drivers are
     responsible for maintaing that when they use [eval], otherwise
     warnings are reported see the [value] function.

     The caching mechanism remembers only the last evaluation. But that
     seems sufficient for now since usually once a config is setup for
     a project we no longer touch it. *)

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
  type conf = { cid : int; map : As_univ.t Kmap.t }

  type 'a value =
    { deps : Kset.t; def : conf -> 'a; mutable cache : (int * 'a) option; }

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

type 'a value = 'a Def.value =
  { deps : Kset.t; def : Def.conf -> 'a; mutable cache : (int * 'a) option; }

let eval c v = match v.cache with
| Some (cid, v) when cid = c.Def.cid -> v
| _ ->
    let cv = v.def c in
    v.cache <- Some (c.Def.cid, cv);
    cv

let const v = { deps = Kset.empty; def = (fun _ -> v); cache = None }
let manual_value deps v = { deps; def = (fun _ -> v); cache = None }
let app f v =
  { deps = Kset.union f.deps v.deps;
    def = (fun c -> (eval c f) (eval c v));
    cache = None }

let deps v = v.deps

let ( $ ) = app

let true_ = const true
let false_ = const false
let neg a = const (fun v -> not v) $ a
let ( ||| ) a b = const ( || ) $ a $ b
let ( &&& ) a b = const ( && ) $ a $ b
let pick_if c a b = const (fun c a b -> if c then a else b) $ c $ a $ b

module Option = struct
  let wrap = function
  | None -> const None
  | Some v -> const (fun v -> Some v) $ v

  let some v = const (fun v -> Some v) $ v

  let get ?none v = match none with
  | None ->
      let get v = match v with
      | Some v -> v
      | None -> invalid_arg "option is None and no ~none argument provided"
      in
      const get $ v
  | Some none ->
      let get none v = match v with
      | Some v -> v
      | None -> none
      in
      const get $ none $ v
end


(* Configuration keys *)

type 'a key = 'a Def.key

module Key = struct

  (* Existential key *)

  type t = Def.t = V : 'a key -> t

  let hide_type k = V k
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

  module Set = struct
    include Kset
    let of_list = List.fold_left (fun acc s -> add s acc) empty
  end

  module Map = struct
    include Kmap
    let dom m = fold (fun k _ acc -> Set.add k acc) m Set.empty
  end
end

let pp_key_dup ppf (Key.V k) = Fmt.pf ppf
    "Key name `%s'@ not unique@ in@ the@ configuration." (Key.name k)

let pp_miss_key ppf (Key.V k) = Fmt.pf ppf
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
  let deps = (Key.default k).deps in
  let deps = Kset.add (Key.V k) deps in
  let def c =
    match try Some (Kmap.find (Key.V k) c.Def.map) with Not_found -> None with
  | None ->
      As_misc.log_driver_fault Log.Warning  "%a" pp_miss_key (Key.V k);
      eval c (Key.default k)
  | Some u ->
      match Key.of_univ k u with
      | Some v -> eval c v
      | None -> assert false
  in
  { deps; def; cache = None }

(* Configurations *)

(* FIXME at the moment we generate a new conf id even when we just add
   a key to a config. This seems ok since usually we more or less
   simply generate a config from the Key.Set of the project deps in a
   single shot plus a few overrides from the command lin,e plus the
   user's configuration scheme additions, seems unlikely that the ids
   would roll over and even in that case it seems hard to confuse the
   evaluation cache since those configuration are never used for
   evaluation per se.

   To do it cleanly it should be done in two stages, a [pre_config]
   type which is just the map used for specifying the configuration
   and a [config] type which is map + id, only used for
   evalation. Under that regime configuration schemes would become
   [pre_configs]. Note that this is only of concern to the driver api,
   end users don't see the notion of configuration. *)

type t = Def.conf

let conf_id =
  let count = ref (min_int) in
  fun () -> incr count; !count (* FIXME should we warn if we roll over ? *)

let empty = { Def.cid = conf_id (); map = Kmap.empty }
let is_empty c = Kmap.is_empty c.Def.map
let mem c k = Kmap.mem (Key.V k) c.Def.map
let add c k =
  { Def.cid = conf_id ();
    map = Kmap.add (Key.V k) Key.(to_univ k (default k)) c.Def.map }

let set c k v =
  { Def.cid = conf_id ();
    map = Kmap.add (Key.V k) (Key.to_univ k v) c.Def.map }

let rem c k = { Def.cid = conf_id (); map = Kmap.remove (Key.V k) c.Def.map }

let merge l r =
  let choose _ l r = match l, r with
  | (Some _ as v), None | None, (Some _ as v) -> v
  | Some _, (Some _ as v) -> v
  | None, None -> None
  in
  { Def.cid = conf_id (); map = Kmap.merge choose l.Def.map r.Def.map }

let find c k =
  try match Key.of_univ k (Kmap.find (Key.V k) c.Def.map) with
  | Some v -> Some ({ deps = Kset.add (Key.V k) v.deps; def = v.def;
                      cache = v.cache; })
  | None -> assert false
  with Not_found -> None

let get c k = match find c k with
| None -> invalid_arg (strf "no key named %s in configuration" (Key.name k))
| Some v -> v

let domain c = Key.Map.dom c.Def.map

let of_keys s =
  let add (Key.V kt as k) acc =
    let u = Key.(to_univ kt (default kt)) in
    Kmap.add k u acc
  in
  { Def.cid = conf_id (); map = Kset.fold add s Kmap.empty }

let pp ppf c =
  let cmp (Key.V k0, _) (Key.V k1, _) = compare (Key.name k0) (Key.name k1) in
  let defs = List.sort cmp (Kmap.bindings c.Def.map) in
  let pp_binding ppf (Key.V k, v) =
    let v = match Key.of_univ k v with None -> assert false | Some v -> v in
    let v = eval c v in
    Fmt.pf ppf "@[%s@ = @[%a@]@]" (Key.name k) (printer (Key.converter k)) v
  in
  Fmt.pf ppf "@[<v>%a@]" (Fmt.list pp_binding) defs

(* Configuration schemes *)

type scheme = string * (string * t)
type def = D : 'a key * [`Const of 'a | `Value of 'a value] -> def
let def k v = D (k, `Const v)
let defv k v = D (k, `Value v)

let scheme ?doc ?base name defs =
  let doc = match doc with None -> strf "The %s scheme." name | Some d -> d in
  let init = match base with None -> empty | Some (_, (_, conf)) -> conf in
  let add acc (D (k, v)) =
    let v = match v with `Const v -> const v | `Value v -> v in
    set acc k v
  in
  name, (doc, List.fold_left add init defs)

let quote s = strf "`%s'" s
let alts_str = function
| [] -> assert false
| [a] -> quote a
| [a; b] -> strf "either %s or %s" (quote a) (quote b)
| alts ->
    let rev_alts = List.rev alts in
    strf "one of %s or %s"
      (String.concat ~sep:", " (List.rev_map quote @@ List.tl rev_alts))
      (quote @@ List.hd rev_alts)

let msg_invalid_value s exp = R.msgf "invalid value %s, %s" (quote s) exp

let bool =
  let parse s = match String.to_bool s with
  | Some b -> Ok b
  | None -> Error (msg_invalid_value s (alts_str ["true"; "false"]))
  in
  parse, Fmt.bool

let int =
  let parse s = match String.to_int s with
  | Some i -> Ok i
  | None -> Error (msg_invalid_value s "integer")
  in
  parse, Fmt.int

let string = (fun s -> Ok s), String.pp

let enum sl =
  if sl = [] then invalid_arg "enum list can't be empty" else
  let sl_inv = List.rev_map (fun (s, v) -> (v, s)) sl in
  let parse s = try Ok (List.assoc s sl) with
  | Not_found ->
      let sl = List.rev (List.rev_map fst sl) in
      Error (msg_invalid_value s (alts_str sl))
  in
  let print ppf v = Fmt.string ppf (List.assoc v sl_inv) in
  parse, print

let path =
  let parse s = match Path.of_string s with
  | None -> R.error_msgf "%a: not a path" String.pp s
  | Some p -> Ok p
  in
  parse, Path.pp

let path_kind is_kind kind =
  let parse s = match Path.of_string s with
  | Some p when is_kind p -> Ok p
  | Some p -> R.error_msgf "`%a': not a %s path" Path.pp p kind
  | None -> R.error_msgf "%a: not a path" String.pp s
  in
  parse, Path.pp

let rel_path = path_kind Path.is_rel "relative"
let abs_path = path_kind Path.is_abs "absolute"

let version : (int * int * int * string option) converter =
  let parser s =
    try
      let parse_opt_v s = match String.Sub.head s with
      | Some ('v'|'V') -> String.Sub.tail s
      | Some _ -> s
      | None -> raise Exit
      in
      let parse_dot s = match String.Sub.head s with
      | Some '.' -> String.Sub.tail s
      | Some _ | None -> raise Exit
      in
      let parse_int s =
        match String.Sub.min_span ~min:1 ~sat:Char.Ascii.is_digit s with
        | None -> raise Exit
        | Some (i, s) ->
            match String.Sub.to_int i with
            | None -> raise Exit | Some i -> i, s
      in
      let maj, s = parse_int (parse_opt_v (String.sub s)) in
      let min, s = parse_int (parse_dot s) in
      let patch, s = match String.Sub.head s with
      | Some '.' -> parse_int (parse_dot s)
      | _ -> 0, s
      in
      let info = match String.Sub.head s with
      | Some ('+' | '-') -> Some (String.Sub.(to_string (tail s)))
      | Some _ -> raise Exit
      | None -> None
      in
      Ok (maj, min, patch, info)
    with Exit ->
      R.error_msgf "invalid value `%s', expected a version number of the \
                    form [v|V]majoAr.minor[.patch][(+|-)info]." s
  in
  let printer ppf (maj, min, patch, info) =
    let info = match info with None -> "" | Some info -> info in
    Format.fprintf ppf "%d.%d.%d%s" maj min patch info
  in
  parser, printer

(* Built-in configuration keys *)

let err_det this _ = R.msgf "Could not determine %s." this

(* Build directories keys *)

let docs_build_directories = "Directory keys"
let doc_build_directories =
  "These keys inform the build system about directories."

let build_directories_key = key ~docs:docs_build_directories

let root_dir =
  let doc = "Absolute path to the project directory." in
  let get_cwd () = Log.on_error_msg ~use:Path.root @@ OS.Dir.current () in
  let get_cwd = const get_cwd $ const () in
  build_directories_key "root-dir" path get_cwd ~doc ~docv:"PATH"

let build_dir =
  let doc = "Path to the build directory expressed relative the root \
             directory (see $(b,--root-dir))."
  in
  let build_dir = const (Path.v "_build") in
  build_directories_key "build-dir" rel_path build_dir ~doc ~docv:"PATH"

(* Project keys *)

let doc_project =
  "These keys inform the build system about properties specific to
   this project."

let project_key = key ~docs:docs_project (* defined above, [key] ~docs def. *)
let project_version =
  let doc = "Version of the project." in
  let vcs_info root =
    begin As_vcs.find root >>= function
    | None ->
        Log.warn "No VCS found to derive project version.";
        Ok "unknown"
    | Some vcs ->
        As_vcs.describe root vcs
    end
    |> R.reword_error_msg (err_det "project version")
    |> Log.on_error_msg ~use:"unknown"
  in
  let vcs = const vcs_info $ (value root_dir) in
  project_key "project-version" string vcs ~doc ~docv:"VERSION"

(* Builtin configuration keys *)

let utility_key ?exec ~docs name =
  let exec = match exec with None -> const name | Some e -> e in
  let doc = strf "The %s utility." name in
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
      OS.Cmd.exec_read uname [ "-s" ] >>| String.Ascii.lowercase
      |> R.reword_error_msg (err_det "host machine operating system name")
      |> Log.on_error_msg ~use:"unknown"
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
    | "Win32" ->
        (* TODO fix Bos.OS.Env *)
        begin match OS.Env.var "PROCESSOR_ARCHITECTURE" with
        | None -> R.error_msg "PROCESSOR_ARCHITECTURE env variable undefined"
        | Some arch -> Ok (String.Ascii.lowercase arch)
        end
    | _ -> OS.Cmd.exec_read uname [ "-m" ] >>| String.Ascii.lowercase
    end
    |> R.reword_error_msg (err_det "host machine hardware architecture")
    |> Log.on_error_msg ~use:"unknown"
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
  let doc = "Build test parts." in
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
    | "Win32" ->
        (* TODO fix Bos.OS.Env *)
        begin match OS.Env.var "NUMBER_OF_PROCESSORS" with
        | None -> R.error_msg "PROCESSOR_ARCHITECTURE env variable undefined"
        | Some n -> (parser int) n
        end
    | _ -> OS.Cmd.exec_read "getconf" [ "_NPROCESSORS_ONLN" ] >>= (parser int)
    end
    |> R.reword_error_msg (err_det "the number of host processors")
    |> Log.on_error_msg ~level:Log.Warning (* or Info ? *) ~use:1
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
  let doc = strf "The %s utility." exec in
  let bin native = if native then strf "%s.opt" exec else exec in
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
    OS.Cmd.exec_read tool [ "-version" ] >>= (parser version)
    |> R.reword_error_msg (err_det "OCaml compiler version")
    |> Log.on_error_msg ~level:Log.Warning ~use:(0, 0, 0, Some "-unknown")
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

let echo = system_utilities_utility "echo"
let cd = system_utilities_utility "cd"
let ln = system_utilities_utility "ln" ~win32:"copy" (* FIXME windows *)
let cp = system_utilities_utility "cp" ~win32:"copy"
let mv = system_utilities_utility "mv" ~win32:"move"
let rm = system_utilities_utility "rm" ~win32:"del"
let rmdir = system_utilities_utility "rmdir"
let mkdir = system_utilities_utility "mkdir"
let cat = system_utilities_utility "cat" ~win32:"type"
let make = system_utilities_utility "make"
