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

(* Build configuration *)

let str = Printf.sprintf

type 'a parser = string -> [ `Error of string | `Ok of 'a ]
type 'a printer = Format.formatter -> 'a -> unit
type 'a converter = 'a parser * 'a printer

module rec Value : sig
  type 'a typed = Kset.t * 'a

  val const : 'a -> 'a typed
  val app : ('a -> 'b) typed -> 'a typed -> 'b typed
  val eval : 'a typed -> 'a
  val deps : 'a typed -> Kset.t

  type t

  val create : unit -> ('a -> t) * (t -> 'a option)
end = struct
  type 'a typed = Kset.t * 'a

  let const v = Kset.empty, v
  let app (kf, f) (kv, v) = (Kset.union kf kv), f v
  let eval v = snd v
  let deps v = fst v

  type t = exn

  let create (type s) () =
    (* universal type see http://mlton.org/UniversalType *)
    let module M = struct exception E of s option end in
    (fun x -> M.E (Some x)), (function M.E x -> x | _ -> None)
end

and Key : sig
  type 'a typed

  val name : 'a typed -> string
  val public : 'a typed -> bool
  val converter : 'a typed -> 'a Value.typed converter
  val value : 'a typed -> 'a Value.typed
  val doc : 'a typed -> string option
  val docs : 'a typed -> string option
  val docv : 'a typed -> string option
  val untyped_value : 'a typed -> Value.t
  val of_value : 'a typed -> Value.t -> 'a Value.typed option
  val to_value : 'a typed -> 'a Value.typed -> Value.t
  val with_value : 'a typed -> 'a Value.typed -> 'a typed

  type t = V : 'a typed -> t

  val create : ?public:bool -> ?docs:string -> ?docv:string -> ?doc:string ->
    string -> 'a converter -> 'a Value.typed -> 'a typed

  val equal : t -> t -> bool
  val compare : t -> t -> int
end = struct

  type 'a typed =
    { id : int;
      name : string;
      public : bool;
      converter : 'a Value.typed converter;
      value : Value.t;
      doc : string option;
      docv : string option;
      docs : string option;
      to_value : 'a Value.typed -> Value.t;
      of_value : Value.t -> 'a Value.typed option; }

  let name k = k.name
  let public k = k.public
  let converter k = k.converter
  let value k : 'a Value.typed =
    let deps, v = match k.of_value k.value with
    | None -> assert false
    | Some v -> v
    in
    Kset.add (Key.V k) deps, v

  let doc k = k.doc
  let docv k = k.docv
  let docs k = k.docs
  let untyped_value k = k.value
  let with_value k v = { k with value = k.to_value v }
  let of_value k = k.of_value
  let to_value k = k.to_value

  type t = V : 'a typed -> t

  let value_converter_of_converter (parse, print) =
    let parse s = match parse s with
    | `Ok v -> `Ok (Value.const v)
    | `Error _ as e -> e
    in
    let print ppf v = print ppf (Value.eval v) in
    parse, print

  let id =
    let count = ref (-1) in
    fun () -> incr count; !count

  let create ?(public = true) ?docs ?docv ?doc name converter value =
    let id = id () in
    let converter = value_converter_of_converter converter in
    let to_value, of_value = Value.create () in
    let value = to_value value in
    { id; name; public; converter; value; doc; docv; docs; to_value; of_value }

  let equal (V k0) (V k1) = (k0.id : int) = (k1.id : int)
  let compare (V k0) (V k1) =
    (Pervasives.compare : int -> int -> int) k0.id k1.id

end

and Kset : (Set.S with type elt = Key.t) = Set.Make (Key)

type 'a key = 'a Key.typed
type 'a value = 'a Value.typed

(* Configuration values *)

let const = Value.const
let app = Value.app
let ( $ ) = app
let eval = Value.eval
let value_deps = Value.deps

(* Configuration value converters *)

let bool = Cmdliner.Arg.bool
let int = Cmdliner.Arg.int
let string = Cmdliner.Arg.string

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

let enum = Cmdliner.Arg.enum
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

(* Configuration keys

   FIXME: we could check here that [key] is never called twice in the
   program with the same name. *)

let key = Key.create

(* Configurations *)

type t = Kset.t

let empty = Kset.empty
let is_empty = Kset.is_empty
let add c k = Kset.add (Key.V k) c
let set c k v = Kset.add (Key.V (Key.with_value k v)) c
let find c k =
  try
    let Key.V kv = Kset.find (Key.V k) c in
    let deps, v = match Key.of_value k (Key.untyped_value kv) with
    | None -> assert false
    | Some v -> v
    in
    Some (Kset.add (Key.V k) deps, v)
  with Not_found -> None

let get c k = match find c k with
| None -> invalid_arg (str "no key named %s in configuration" (Key.name k))
| Some v -> v

let merge c0 c1 =
  let add k acc =
    if not (Kset.mem k acc) then Kset.add k acc else
    (* Value may differ, remove from acc and add overriding value *)
    Kset.add k (Kset.remove k acc)
  in
  Kset.fold add c1 c0

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

let ( + ) = add
let ( @ ) = get
let ( ++ ) = merge

let parse c = (* FIXME move that out of the module *)
  let add (Key.V k) acc =
    let c = Key.converter k in
    let v = Key.value k in
    let doc = Key.doc k in
    let docs = Key.docs k in
    let docv = Key.docv k in
    let i = Cmdliner.Arg.info [Key.name k] ?doc ?docv ?docs in
    let opt = Cmdliner.Arg.(value (opt c v & i)) in
    Cmdliner.Term.(pure set $ acc $ pure k $ opt)
  in
  Kset.fold add c (Cmdliner.Term.pure c)

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
      | _ -> 1 (* TODO Cmd.read "getconf" [ "_NPROCESSORS_ONLN" ] *)
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
  key "product-dir" rel_path ~public:false (Key.value build_dir)

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
  let bin = const bin $ Key.value ocaml_native_tools in
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
             the tool specified by option $(b,--ocamlc) or $(b,--ocamlopt)."
  in
(*
  TODO
  let tool nat = if nat then value ocamlopt else value ocamlc in
  let get_version tool = Cmd.read tool [ "-version" ] >>= (fst version) in
  let get_version = const get_version $ (const tool $ ocaml_native-tools) in
*)
  let get_version = const (4, 01, 0, None) in
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
  let doc = "The operating system name (defaults inferred by lowercasing the
             result of invoking the tool specified by option $(b,--uname) with
             option `-s')."
  in
(*
   TODO
  let get_os uname = Cmd.read uname [ "-u" ] >>= String.lowercase in
  let get_os = const get_os $ (value uname) in
*)
  let get_os = const Sys.os_type in
  key "os" string get_os ~doc ~docs ~docv:"STRING"

let arch =
  let doc = "The operating system name (defaults inferred by lowercasing the
             result of invoking the tool specified by option $(b,--uname) with
             option `-s')."
  in
(*
   TODO
  let get_arch uname = Cmd.read uname [ "-m" ] in
  let get_arch = const get_os $ (value uname) in
*)
  let get_arch = const "x86_64" in
  key "arch" string get_arch ~doc ~docs ~docv:"STRING"

let builtin_machine_info = empty + os + arch

let builtin_base =
  let ( +++ ) = Kset.union in
  builtin_build_props +++ builtin_build_dirs +++ builtin_ocaml +++
  builtin_base_utils
