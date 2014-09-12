(*
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Printf

let conmap f l = List.concat (List.map f l)
let (|>) x f = f x
let (/) x y = Filename.concat x y
let path = String.concat Filename.dir_sep

module StringSet = Set.Make (String)

module type Set = sig
  include Set.S
  val to_list : t -> elt list
  val of_list : elt list -> t
end

(* Component kinds *)

type kind = [ `Unit | `Pkg | `Lib | `Bin | `Container | `Test | `Doc | `Other ]
let string_of_kind = function
| `Unit -> "unit" | `Pkg -> "pkg" | `Lib -> "lib"
| `Bin -> "bin" | `Container -> "cont" | `Test -> "test"
| `Doc -> "doc" | `Other -> "other"

(* Component type *)

type component =
  [ `Unit of comp_unit
  | `Pkg of pkg
  | `Lib of lib
  | `Bin of bin
  | `Container of container
  | `Test of test
  | `Doc of doc
  | `Other of other ]

and 'a base =
  { base_name : string;
    base_kind : kind;
    base_available : As_features.t;
    base_deps : component list;
    base_parent : (unit -> component) option;
    base_contents : component list;
    base_files : (As_features.t * As_action.file list) list;
    base_rules : 'a base -> component As_action.rule list;
    base_flags : 'a base -> As_resolver.t -> As_flags.t;
    base_payload : 'a; }

and container = unit base
and other = unit base

and comp_unit = comp_unit_ base
and comp_unit_ =
  { u_kind : [ `OCaml | `C | `Js ];
    u_origin : [`Path of string list | `Other of other];
    u_interface : [ `Normal | `Opaque | `Hidden ]; }

and pkg = pkg_ base
and pkg_ =
  { p_kind : [`OCaml | `OCaml_pp | `C ]; }

and lib = lib_ base
and lib_ =
  { l_kind : [ `OCaml | `OCaml_pp ];
    l_origin : [`Units of comp_unit list | `Other of other]; }

and bin = bin_ base
and bin_ =
  { b_origin : [`Units of comp_unit list | `Other of other];
    b_toplevel : bool;
    b_install : bool;
    b_js : bool; }

and test = test_ base
and test_ =
  { t_dir : string option;
    t_commands : test_command list; }
and test_args = As_resolver.t -> string list
and test_command =
  [ `Bin of [`Bin of bin] * test_args
  | `Shell of string ]

and doc = doc_ base
and doc_ =
  { doc_install : bool; }

(* Base components *)

module Base = struct
  let name t = t.base_name
  let kind t = t.base_kind
  let available t = t.base_available
  let deps t = t.base_deps
  let parent t = match t.base_parent with None -> None | Some f -> Some (f ())
  let contents t = t.base_contents
  let files t = t.base_files
  let rules t = t.base_rules t
  let flags t r = t.base_flags t r
  let payload t r = t.base_payload
  let id t = string_of_kind t.base_kind ^ "-" ^ t.base_name

  let with_available base_available t = { t with base_available }
  let with_deps base_deps t = { t with base_deps }
  let with_parent c t = { t with base_parent = Some c }
  let with_contents base_contents t = { t with base_contents }

  let create ?(available=As_features.true_) ?(flags=fun _ _ -> As_flags.empty)
      ?(deps=[]) ?(contents=[]) ?(files=[]) ?(rules=fun _ -> [])
      ?(parent = None)
      name kind payload
    =
    { base_name = name; base_kind = kind; base_available = available;
      base_deps = deps; base_parent = parent; base_contents = contents;
      base_files = files; base_rules = rules; base_flags = flags;
      base_payload = payload; }
end

(* Components *)

type t = component

(* Base field access *)

type 'b app = { app: 'a. 'a base -> 'b }

let base f = function
| `Unit u -> f.app u
| `Other o -> f.app o
| `Pkg p -> f.app p
| `Lib l -> f.app l
| `Bin b  -> f.app b
| `Container c -> f.app c
| `Test t -> f.app t
| `Doc d -> f.app d

let base_name t = base { app = Base.name } t
let base_kind t = base { app = Base.kind } t
let base_available t = base { app = Base.available } t
let base_deps t = base { app = Base.deps } t
let base_contents t = base { app = Base.contents } t
let base_parent t = base { app = Base.parent } t
let base_files t = base { app = Base.files } t
let base_rules t = base { app = Base.rules } t
let base_flags t = base { app = Base.flags } t
let base_id t = base { app = Base.id } t

(* Base maps *)

type 'b map = { map: 'a. 'a base -> 'a base }
let with_base f = function
| `Unit u -> `Unit (f.map u)
| `Other o -> `Other (f.map o)
| `Pkg p -> `Pkg (f.map p)
| `Lib l -> `Lib (f.map l)
| `Bin b -> `Bin (f.map b)
| `Container c -> `Container (f.map c)
| `Test t -> `Test (f.map t)
| `Doc d -> `Doc (f.map d)

let with_available av = with_base { map = fun c -> Base.with_available av c }
let with_deps ds = with_base { map = fun c -> Base.with_deps ds c }
let with_parent p = with_base { map = fun c -> Base.with_parent p c }
let with_contents cs = with_base { map = fun c -> Base.with_contents cs c}

(* Ancestor navigation *)

let path_to_root c =  (* c and its ancestors *)
  let rec loop acc = function
  | None   -> List.rev acc
  | Some c -> loop (c :: acc) (base_parent c)
  in
  loop [c] (base_parent c)

let id ?(all = true) t =
  if not all then base_id t else
  let full_ids = List.map base_id (path_to_root t) in
  String.concat "-" full_ids

(* Component sets *)

module Set = struct
  include Set.Make(struct
      type t = component
      let compare x y = String.compare (id ~all:true x) (id ~all:true y)
    end)

  let to_list = elements
  let of_list l =
    let add set elt = add elt set in
    List.fold_left add empty l
end

let dedup cs =
  let add (seen, acc) c =
    if Set.mem c seen then (seen, acc) else
    Set.add c seen, c :: acc
  in
  List.rev (snd (List.fold_left add (Set.empty, []) cs))

(* Component fields *)

let name = base_name
let kind = base_kind
let available ?(all = true) c =
  if not all then base_available c else
  let full_avail = List.map base_available (path_to_root c) in
  List.fold_left As_features.(&&&) As_features.true_ full_avail

let deps ?(all = true) c =
  if not all then base_deps c else
  let full_deps = List.map base_deps (path_to_root c) in
  dedup (List.fold_left (@) [] full_deps)

let parent = base_parent
let contents = base_contents
let files c =
  let files = base_files c in
  let available = available ~all:true c in
  let add_files (a, files) = As_features.(available &&& a), files in
  List.map add_files files

let rules = base_rules
let flags ?(all = true) c r =
  let resolved_flags c = base_flags c r in
  if not all then resolved_flags c else
  let full_flags = List.map resolved_flags (path_to_root c) in
  List.fold_left As_flags.(@@@) As_flags.empty full_flags

let build_dir c r = match parent c with
| None   -> As_resolver.build_dir r / base_id c
| Some c -> As_resolver.build_dir r / id ~all:true c

let file c r a = build_dir c r / As_action.string_of_file (name c) a
let source_dir c af =
  let error () =
    failwith (sprintf "%s does not have any source directory." (id c))
  in
  match c with
  | `Unit u ->
      (match u.base_payload.u_origin with
       | `Path p -> path p / As_action.string_of_file (name c) af
       | _ -> error ())
  | _ -> error ()

let phases t =
  let rec aux acc t =
    let phases =
      rules t
      |> List.map (fun r -> r.As_action.phase)
      |> As_flags.PhaseSet.of_list
    in
    List.fold_left aux
      (As_flags.PhaseSet.union phases acc)
      (contents t)
  in
  aux As_flags.PhaseSet.empty t
  |> As_flags.PhaseSet.remove `Prepare
  |> As_flags.PhaseSet.to_list

(* Component list operations *)

let keep pred cs =
  let keep acc c = if pred c then c :: acc else acc in
  List.rev (List.fold_left keep [] cs)

let filter_map fn cs =
  let add acc c = match fn c with None -> acc | Some v -> v :: acc in
  List.rev (List.fold_left add [] cs)

let closure ?(link=false) (ts : t list) : t list =
  let deps_tbl = Hashtbl.create 24 in
  let rec aux acc = function
  | []            -> List.rev acc
  | (h :: t) as d ->
      if Hashtbl.mem deps_tbl (id h) then
        match Hashtbl.find deps_tbl (id h) with
        | 0 -> Hashtbl.replace deps_tbl (id h) 1; aux (h :: acc) t
        | _ -> aux acc t
      else (
        Hashtbl.add deps_tbl (id h) 0;
        let contents = match kind h with
        | `Lib when link ->
            let c = Set.of_list (contents h) in
            conmap deps (contents h)
            |> List.filter (fun x -> not (Set.mem x c))
        | _ -> contents h in
        let deps = deps h @ contents in
          let d' = if not link then deps else
            List.filter
              (function
              | `Unit _ -> true
              | `Pkg pkg when pkg.base_payload.p_kind = `OCaml -> true
              | `Lib lib when lib.base_payload.l_kind = `OCaml -> true
              | _ -> false)
              deps in
        aux acc (d' @ d)
        )
  in
  aux [] ts

let map fn ts =
  let tbl = Hashtbl.create (List.length ts) in
  List.iter (fun t -> Hashtbl.add tbl (id t) None) ts;
  let rec aux t =
    try match Hashtbl.find tbl (id t) with
    | Some t -> t
    | None   ->
        let deps = List.map aux (deps t) in
        let t' = with_deps deps t in
          let t' = fn t' in
        Hashtbl.replace tbl (id t) (Some t');
        t'
    with Not_found -> t
  in
  List.map aux ts

(* Component kinds *)

let unit = function `Unit u -> Some u | _ -> None
let unit_ocaml = function
| `Unit u when u.base_payload.u_kind = `OCaml -> Some u | _ -> None

let unit_c = function
| `Unit u when u.base_payload.u_kind = `C -> Some u | _ -> None

let unit_js = function
| `Unit u when u.base_payload.u_kind = `Js -> Some u | _ -> None

let other = function `Other u -> Some u | _ -> None

let pkg = function `Pkg p -> Some p | _ -> None
let pkg_kind k = function
| `Pkg p when p.base_payload.p_kind = k -> Some p | _ -> None

let pkg_ocaml = pkg_kind `OCaml
let pkg_ocaml_pp = pkg_kind `OCaml_pp
let pkg_c = pkg_kind `C

let lib = function `Lib l -> Some l | _ -> None
let lib_kind k = function
| `Lib l when l.base_payload.l_kind = k -> Some l | _ -> None

let lib_ocaml = lib_kind `OCaml
let lib_ocaml_pp = lib_kind `OCaml_pp

let bin = function `Bin b -> Some b | _ -> None
let container = function `Container c -> Some c | _ -> None
let test = function `Test t -> Some t | _ -> None
let doc = function `Doc d -> Some d | _ -> None

(* Rules *)

module Rule = struct
  let link (x:As_action.file) =
    As_action.rule
      ~phase:`Prepare
      ~targets:[`Self x]
      ~prereqs:[`Self (`Source x); `Self `Dir]
      (fun t r _f ->
         let source_dir = source_dir t x in
         let target = file t r x in
         let cwd = As_resolver.root_dir r in
         As_action.link r ~source:(cwd / source_dir) ~target)

  let mkdir =
    As_action.rule
      ~phase:`Prepare
      ~targets:[`Self `Dir]
      ~prereqs:[]
      (fun t r _f -> As_action.mkdir r (build_dir t r))

  let files t r ns =
    List.fold_left (fun acc -> function
      | `Phony x -> x :: acc
      | `Self (`Source f) -> source_dir t f :: acc
      | `Self f  -> file t r f :: acc
      | `N (c,f) -> file c r f :: acc
      ) [] ns
    |> List.rev

  let phony_run t = id t ^ "-run"
end

module Container = struct
  type t = container

  let rules _ = [Rule.mkdir]

  let create ?(available = As_features.true_) ?flags ?deps name contents =
    let flags = match flags with
    | None   -> None
    | Some f -> Some (fun _ _ -> f) in
    let base = Base.create ~available ?flags ?deps ~rules name `Container () in
    let c = ref base in
    let contents = map (with_parent (fun () -> `Container !c)) contents in
    c := Base.with_contents contents base;
    !c
end

module Other = struct
  type t = other

  let self_targets_aux rules =
    let aux acc r =
      List.fold_left (fun acc -> function
        | `Self s -> As_action.FileSet.add s acc
        | _ -> acc
        ) acc r.As_action.targets
    in
    List.fold_left aux As_action.FileSet.empty rules
    |> As_action.FileSet.to_list

  let self_targets t = self_targets_aux (rules (`Other t))

  let create ?(available=As_features.true_) ?flags ?deps name rules =
    let files = [available, self_targets_aux rules] in
    let flags = match flags with
    | None   -> None
    | Some f -> Some (fun _ _ -> f)
    in
    let rules _ = rules in
    Base.create ~available ?flags ?deps ~files ~rules name `Other ()
end

module Pkg = struct
  type kind = [ `OCaml | `OCaml_pp | `C ]
  type t = pkg
  let name t = name (`Pkg t)

  let create ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(opt = false) name kind
    =
    let opt_feature name kind =
      let kind = match kind with
      | `OCaml -> "OCaml"
      | `OCaml_pp -> "OCaml pre-processor"
      | `C -> "C"
      in
      let doc = sprintf "%s %s package available" name kind in
      As_features.create name ~default:true ~doc
    in
    let pkg_f = if opt then opt_feature name kind else As_features.true_ in
    let available = As_features.(available &&& pkg_f) in
    let flags t r =
      let open As_flags in
      flags @@@ As_resolver.pkgs r [name]
    in
    Base.create ~flags ~available name `Pkg { p_kind = kind }

  let opt t = available (`Pkg t) <> As_features.true_
  let kind t = t.base_payload.p_kind

  (* Builtin packages *)

  let compiler_libs_toplevel = create "compiler-libs.toplevel" `OCaml
  let ctypes_stub = create "ctypes.stubs" `OCaml
end

module Unit = struct
  type t = comp_unit
  type kind = [ `OCaml | `C | `Js]
  type interface = [ `Normal | `Opaque | `Hidden ]

  let kind t = t.base_payload.u_kind
  let interface t = t.base_payload.u_interface

  let map fn ts =
    List.map (fun u -> `Unit u) ts
    |> map (function `Unit u -> `Unit (fn u) | x -> x)
    |> filter_map unit

  let source_dir t = match t.base_payload.u_origin with
  | `Path p -> Some (path p)
  | _ -> None

  let generated t = match t.base_payload.u_origin with
  | `Path _  -> false
  | `Other _ -> true

  let _string_of_kind = function
  | `C -> "c"
  | `OCaml -> "ocaml"
  | `Js -> "js"

  let has (file:As_action.file) t =
    let name = name (`Unit t) in
    let check k files =
      if k <> t.base_payload.u_kind then false
      else match t.base_payload.u_origin with
      | `Path p -> List.exists (fun f ->
          Sys.file_exists (path p /  As_action.string_of_file name f)
        ) files
      | `Other o -> List.exists (fun f ->
          List.mem f (Other.self_targets o)
        ) files
    in
    match file with
    | `Source f -> (match t.base_payload.u_origin with
      | `Path p -> Sys.file_exists (path p / As_action.string_of_file name f)
      | _ -> false)
    | `Dir -> false
    | `Mli -> check `OCaml [`Mli]
    | `Ml  -> check `OCaml [`Ml]
    | `Dep x -> check `OCaml [(x:>As_action.file)]
    | `Cmti -> check `OCaml [`Mli]
    | `Cmt -> check `OCaml [`Ml]
    | `Cmi -> check `OCaml [`Ml;`Mli;`Cmi]
    | `Cmo -> check `OCaml [`Ml;`Cmo]
    | `Cmx -> check `OCaml [`Ml;`Cmx]
    | `Cma -> check `OCaml [`Ml;`Cmo;`Cma]
    | `Cmxa | `Cmxs | `A | `Byte | `Native -> false
    | `C -> check `C [`C]
    | `O -> check `C [`C;`O] || check `OCaml [`Ml; `O]
    | `So -> check `C [`C;`O]
    | `Js -> check `Js [`Js]
    | (`Ext _ | `Other _) as x ->
        match t.base_payload.u_origin with
        | `Other o -> List.mem x (Other.self_targets o)
        | _ -> false

  let js_rules _t =
    [ Rule.link `Js ]

  let c_rules _t =
    [ Rule.link `C
    ; As_action.rule
        ~phase:(`Compile `C)
        ~targets:[`Self `O]
        ~prereqs:[`Self `C]
        (fun t r f ->
           let file = file t r `C in
           let dir = Filename.dirname file in
           As_action.create ~dir "%s -c -I %s %s %s"
             (As_resolver.ocamlc r)
             (As_resolver.lib_dir r)
             (Filename.basename file)
             (String.concat " " (As_flags.get (`Compile `C) f)))
    ; As_action.rule
        ~phase:(`Archive `C)
        ~targets:[`Self `So]
        ~prereqs:[`Self `O ]
        (fun t r f ->
           let filename = file t r `So in
           As_action.create "%s -o %s %s %s"
             (As_resolver.ocamlmklib r)
             (Filename.chop_extension filename)
             (file t r `O)
             (String.concat " " (As_flags.get (`Archive `C) f)))
    ]

  let ocaml_rules t =
    let ext file mode =
    `Ext
      ((match file with `Ml -> "cml" | `Mli -> "cmli")
       ^ "-" ^
       (match mode with `Byte -> "byte" | `Native -> "native"))
    in
    let precompile x mode =
      let y = ext x mode in
      let x = (x:>As_action.file) in
      As_action.rule
        ~phase:(`Pp mode)
        ~targets:[`Self y]
        ~prereqs:[`Self x]
        (fun t r f ->
           match As_resolver.preprocessor r with
           | None ->
               let source = As_resolver.root_dir r / file t r x in
               let target = file t r y in
               As_action.link r ~source ~target
           | Some preprocessor ->
             let out_file = file t r y in
               As_action.create "%s %s %s > %s || ( rm -f %s && false )"
                 preprocessor
                 (String.concat " " (As_flags.get (`Pp mode) f))
                 (file t r x)
                 out_file out_file)
    in
    let ocamldep x =
      let ocaml_files = match parent (`Unit t) with
      | None   -> []
      | Some p ->
          let deps =
            deps ~all:true (`Unit t)
            |> closure ~link:true
            |> conmap (function
              | `Lib _ as c -> contents c
              | c -> [c]
              )
            |> filter_map unit
          in
          let contents = (filter_map unit) (contents p) in
          let units = deps @ contents in
          let mls =
            List.filter (has `Ml) units
            |> List.map (fun u -> `N (`Unit u, `Ml))
          in
          let mlis =
            List.filter (has `Mli) units
            |> List.map (fun u -> `N (`Unit u, `Mli))
          in
          mls @ mlis
      in
      let y = ext x `Byte in
      let link_sources = `Self y :: ocaml_files in
      As_action.rule
        ~phase:`Dep
        ~targets:[`Self (`Dep x)]
        ~prereqs:link_sources
        (fun t r f ->
           let k = match x with `Ml -> "-impl" | `Mli -> "-intf" in
           As_action.create "%s %s %s %s > %s"
             (As_resolver.ocamldep r)
             (String.concat " " (As_flags.get `Dep f))
             k (file t r y)
             (file t r (`Dep x)))
    in
    (if not (has `Mli t) then [] else [
        Rule.link `Mli;
        precompile `Mli `Byte;
        precompile `Mli `Native;
        ocamldep `Mli;
        As_action.rule
          ~phase:(`Compile `Byte)
          ~targets:[`Self `Cmi]
          ~prereqs:[`Self (ext `Mli `Byte); `Self (`Dep `Mli)]
          (fun t r f ->
             As_action.create "%s -c %s -intf %s"
               (As_resolver.ocamlc r)
               (String.concat " " (As_flags.get (`Compile `Byte) f))
               (file t r (ext `Mli `Byte)))])
    @ (if not (has `Ml t) then [] else [
        Rule.link `Ml;
        precompile `Ml `Byte;
        precompile `Ml `Native;
        ocamldep `Ml;
        As_action.rule
          ~phase:(`Compile `Byte)
          ~targets:(if has `Mli t then [`Self `Cmo] else [`Self `Cmi; `Self `Cmo])
          ~prereqs:(`Self (`Dep `Ml) :: `Self (ext `Ml `Byte) ::
                    if has `Mli t then [`Self `Cmi] else [])
          (fun t r f ->
             As_action.create "%s -c %s -impl %s"
               (As_resolver.ocamlc r)
               (String.concat " " (As_flags.get (`Compile `Byte) f))
               (file t r (ext `Ml `Byte)));
        As_action.rule
          ~phase:(`Compile `Native)
          ~targets:[`Self `Cmx]
          ~prereqs:[`Self (`Dep `Ml); `Self `Cmi; `Self (ext `Ml `Native)]
          (fun t r f ->
             As_action.create "%s -c %s -impl %s"
               (As_resolver.ocamlopt r)
               (String.concat " " (As_flags.get (`Compile `Native) f))
               (file t r (ext `Ml `Native)))])

  let rules t =
    (match t.base_payload.u_origin with
     | `Other o -> rules (`Other o)
     | `Path _  -> [])
    @ (match t.base_parent with
      | None   -> [Rule.mkdir]
      | Some _ -> [])
    @ match t.base_payload.u_kind with
    | `C     -> c_rules t
    | `Js    -> js_rules t
    | `OCaml -> ocaml_rules t

  let pp_deps_flags mode deps r =
    let add_pp_lib l =
      let dir = build_dir (`Lib l) r in
      let file = match mode with
      | `Byte -> file (`Lib l) r `Cma
      | `Native -> file (`Lib l) r `Cmxa
      in
      sprintf "%s/%s" dir file
    in
    let lib_flags = List.map add_pp_lib (filter_map lib_ocaml_pp deps) in
    let pp_pkgs = filter_map pkg_ocaml_pp deps in
    let pp_pkgs_flags = As_resolver.pkgs r (List.map Pkg.name pp_pkgs) in
    let pp_pkgs_flags = As_flags.get (`Pp mode) pp_pkgs_flags in
    pp_pkgs_flags @ lib_flags

  let pkg_deps_flags mode deps r =
    let pkgs = filter_map pkg_ocaml deps in
    let pkg_flags = As_resolver.pkgs r (List.map Pkg.name pkgs) in
    As_flags.get (`Compile mode) pkg_flags

  let local_deps_flags mode deps ~bdir r =
    let units_and_libs = function `Lib _ | `Unit _ -> true | _ -> false in
    let units_and_libs = keep units_and_libs deps in
    let dirs = List.map (fun c -> build_dir c r) units_and_libs in
    let incs =
      (* Need to kep the -I flags in the right order, bdir is excluded
         because it should already be included by the container. *)
      let add_dir (seen, acc) dir =
        if StringSet.mem dir seen then (seen, acc) else
        (StringSet.add dir seen, (sprintf "-I %s" dir) :: acc)
      in
      let seen = StringSet.singleton bdir in
      List.rev (snd (List.fold_left add_dir (seen, []) dirs))
    in
    incs

  let comp_flags deps ~bdir r =
    let byte_local = local_deps_flags `Byte deps ~bdir r in
    let byte_pkgs = pkg_deps_flags `Byte deps r in
    let native_pkgs = pkg_deps_flags `Native deps r in
    let native_local = local_deps_flags `Native deps ~bdir r in
    let open As_flags in
    v `Dep byte_local @@@
    v (`Compile `Byte) (byte_pkgs @ byte_local) @@@
    v (`Compile `Native) (native_pkgs @ native_local) @@@
    v (`Pp `Byte) (pp_deps_flags `Byte deps r) @@@
    v (`Pp `Native) (pp_deps_flags `Native deps r)

  let mk_flags t r =
    let c = `Unit t in
    (* FIXME: Component.closure ~link:true [c] ? *)
    let deps = deps ~all:false c |> closure ~link:true in
    let bdir = build_dir c r in
    comp_flags deps ~bdir r

  let files = function
  | `OCaml -> [
      As_features.byte  , [`Mli; `Cmi; `Cmo];
      As_features.native, [`Mli; `Cmi; `O; `Cmx];
      As_features.annot , [`Cmt; `Cmti];
    ]
  | `C -> [ As_features.true_, [`So; `So] ]
  | `Js -> []

  let check t =
    let is_generated = match t.base_payload.u_origin with
    | `Path _ -> false
    | `Other _ -> true
    in
    if is_generated then () else
    let name = t.base_name in
    let dir = match t.base_payload.u_origin with
    | `Other _ -> assert false | `Path p -> path p / ""
    in
    match t.base_payload.u_kind with
    | `OCaml when not (List.exists (fun f -> has f t) [`Ml; `Mli]) ->
        As_shell.warn "unit %s: cannot find %s.ml or %s.mli in `%s'"
          name name name dir
    | `C when not (has `C t) ->
        As_shell.warn "unit %s: cannot find %s.c in `%s'" name name dir
    | `Js when not (has `Js t) ->
        As_shell.warn "unit %s: cannot find %s.js in `%s'" name name dir
    | _ -> ()

  let create ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(deps = []) ?(interface = `Normal) name (kind:kind) origin
    =
    let deps = match origin with
    | `Path _  ->  deps
    | `Other o -> `Other o :: deps
    in
    let files = files kind in
    let flags t r = As_flags.(flags @@@ mk_flags t r) in
    let t = Base.create ~available ~deps ~flags ~files ~rules name `Unit
        { u_kind = kind; u_origin = origin; u_interface = interface }
    in
    check t;
    t

  let pack ?available ?flags ?deps name units =
    let pack =
      Container.create ~flags:(As_flags.for_pack name)
        ("pack-" ^ name)
        (List.map (fun u -> `Unit u) units)
    in
    let units = contents (`Container pack) |> filter_map unit in
    let rules =
      let cmos =
        List.filter (has `Cmo) units
        |> List.map (fun u -> `N (`Unit u, `Cmo))
      in
      let cmxs =
        List.filter (has `Cmx) units
        |> List.map (fun u -> `N (`Unit u, `Cmx))
      in
      [ As_action.rule
          ~phase:(`Compile `Byte)
          ~targets:[`Self `Cmo; `Self `Cmi]
          ~prereqs:(`Self `Dir :: cmos)
          (fun t r f ->
             let cmos = Rule.files t r cmos in
             As_action.create "%s -pack %s %s -o %s"
               (As_resolver.ocamlc r)
               (String.concat " " (As_flags.get (`Compile `Byte) f))
               (String.concat " " cmos)
               (file t r `Cmo))
      ; As_action.rule
          ~phase:(`Compile `Native)
          ~targets:[(`Self `Cmx)]
          ~prereqs:cmxs
          (fun t r f ->
             let cmxs = Rule.files t r cmxs in
             As_action.create "%s -pack %s %s -o %s"
               (As_resolver.ocamlopt r)
               (String.concat " " (As_flags.get (`Compile `Native) f))
               (String.concat " " cmxs)
               (file t r `Cmx)) ]
    in
    let origin = Other.create ~deps:[`Container pack] name rules in
    create ?available ?flags ?deps name `OCaml (`Other origin)
end

module Lib = struct
  type kind = [ `OCaml | `OCaml_pp ]
  type t = lib

  let kind t = t.base_payload.l_kind
  let units t = match t.base_payload.l_origin with `Units us -> us | _ -> []
  let files =
    [ As_features.byte, [`Cma];
      As_features.native, [`Cmxa; `A];
      As_features.native_dynlink, [`Cmxs] ]

  let mk_flags t r =
    let bdir = build_dir (`Lib t) r in
    let incl = [sprintf "-I %s" bdir] in
    let us = units t in
    let dep ext u = file (`Unit u) r ext in
    let cmo_deps = List.filter (Unit.has `Cmo) us |> List.map (dep `Cmo) in
    let cmx_deps = List.filter (Unit.has `Cmo) us |> List.map (dep `Cmx) in
    let comp = Unit.comp_flags t.base_deps ~bdir r in
    let open As_flags in
    comp @@@
    v `Dep incl @@@
    v (`Compile `Byte) incl @@@
    v (`Compile `Native) incl @@@
    v (`Archive `Byte) ("-a" :: cmo_deps) @@@
    v (`Archive `Native) ("-a" :: cmx_deps) @@@
    v (`Archive `Shared) ("-shared" :: "-linkall" :: cmx_deps)

  let archive_action compiler phase ext c r flags =
    let flags = String.concat " " (As_flags.get phase flags) in
    As_action.create "%s %s -o %s" (compiler r) flags (file c r ext)

  let rules t = match t.base_payload.l_origin with
  | `Other o  -> rules (`Other o)
  | `Units us ->
      let dep ext u = `N (`Unit u, ext) in
      let archive_byte =
        let phase = `Archive `Byte in
        let cmo_deps = List.filter (Unit.has `Cmo) us |> List.map (dep `Cmo) in
        As_action.rule
          ~phase ~targets:[`Self `Cma] ~prereqs:(`Self `Dir :: cmo_deps)
          (archive_action As_resolver.ocamlc phase `Cma)
      in
      let archive_native mode =
        let phase = (`Archive mode :> As_flags.phase) in
        let cmx_deps = List.filter (Unit.has `Cmx) us |> List.map (dep `Cmx) in
        let ext = match mode with `Shared -> `Cmxs | `Native -> `Cmxa in
        let exts = match mode with `Shared -> [`Cmxs] | `Native -> [`Cmxa;`A] in
        As_action.rule
          ~phase ~targets:(List.map (fun x -> `Self x) exts)
          ~prereqs:(`Self `Dir :: cmx_deps)
          (archive_action As_resolver.ocamlopt phase ext)
      in
      [ Rule.mkdir;
        archive_byte; archive_native `Native; archive_native `Shared ]

  let create ?(available = As_features.true_) ?(flags = As_flags.empty) ?deps
      ?(byte = true) ?(native = true) ?(native_dynlink = true)
      ?(pack = false) name kind origin
    =
    let available =
      let not_byte = not byte in
      let not_native = not native in
      let not_native_dynlink = not native_dynlink in
      As_features.(available &&&
                   neg ~on:not_byte byte &&&
                   neg ~on:not_native native &&&
                   neg ~on:not_native_dynlink native_dynlink)
    in
    let flags t r = As_flags.(flags @@@ mk_flags t r) in
    match origin with
    | `Other o  ->
        let ofiles = Other.self_targets o in
        let no_cma = not (List.mem `Cma ofiles) in
        let no_cmxa = not (List.mem `Cmxa ofiles) in
        let no_cmxs = not (List.mem `Cmxs ofiles) in
        let available = As_features.(available &&&
                                     neg ~on:no_cma byte &&&
                                     neg ~on:no_cmxa native &&&
                                     neg ~on:no_cmxs native_dynlink)
        in
        let warn file =
          As_shell.warn "Lib %s: no rule to generate the %s file." name file
        in
        if no_cma && byte then warn "cma";
        if no_cmxa && native then warn "cmxa";
        if no_cmxs && native_dynlink then warn "cmxs";
        Base.create ~available ~flags ~rules ~files ?deps name `Lib
          { l_kind = kind; l_origin = `Other o }
    | `Units us ->
        let base = Base.create ~available ~flags ~rules ~files ?deps name `Lib
            { l_kind = kind; l_origin = `Units []; }
        in
        let c = ref base in
        let us = List.map (function `Unit u -> u) us in
        let us = if pack then [Unit.pack name us] else us in
        let us = Unit.map (Base.with_parent (fun () -> `Lib !c)) us in
        let base_contents = List.map (fun u -> `Unit u) us in
        let base_payload = { l_kind = kind; l_origin = `Units us } in
        c := { base with base_contents; base_payload; };
        !c
end

module Bin = struct
  type t = bin

  let is_toplevel t = t.base_payload.b_toplevel
  let install t = t.base_payload.b_install
  let js t = t.base_payload.b_js
  let units t = match t.base_payload.b_origin with `Units us -> us | _ -> []
  let with_origin b_origin t =
    { t with base_payload = { t.base_payload with b_origin } }

  let link_flags mode t r = match t.base_payload.b_origin with
  | `Other _  -> []
  | `Units _ ->
      let deps = deps ~all:false (`Bin t) @ contents (`Bin t)
                 |> closure ~link:true
      in
      let local = conmap (function
        | `Unit u ->
            if not (Unit.has `Cmo u) then [] else [
              match mode with
              | `Byte   -> file (`Unit u) r `Cmo
              | `Native -> file (`Unit u) r `Cmx ]
        | `Lib l ->
            [ match mode with
              | `Byte   -> file (`Lib l) r `Cma
              | `Native -> file (`Lib l) r `Cmxa ]
        | _ -> []
        ) deps in
      let global = match filter_map pkg_ocaml deps with
      | []   -> []
      | pkgs ->
          let pkgs = List.map (fun p -> name (`Pkg p)) pkgs in
          let pkgs = As_resolver.pkgs r pkgs in
          match mode with
          | `Byte   -> As_flags.get (`Link `Byte) pkgs
          | `Native -> As_flags.get (`Link `Native) pkgs in
      global @ local

  let mk_flags t r =
    let deps = deps ~all:false (`Bin t) |> closure ~link:true in
    let bdir = build_dir (`Bin t) r in
    let incl = [sprintf "-I %s" bdir] in
    let comp = Unit.comp_flags deps ~bdir r in
    let flags =
      let open As_flags in
      v `Dep incl @@@
      v (`Compile `Byte) incl @@@
      v (`Compile `Native) incl @@@
      v (`Link `Byte) (link_flags `Byte t r) @@@
      v (`Link `Native) (link_flags `Native t r)
    in
    As_flags.(flags @@@ comp)

  let rules t =
    let deps = deps (`Bin t) @ contents (`Bin t) in
    let deps = List.filter (function
      | `Unit _ | `Lib _ -> true | _ -> false
      ) deps in
    let cmo_and_cma_deps =
      List.map (function
        | `Unit _ as t -> `N (t, `Cmo)
        | `Lib _ as t  -> `N (t, `Cma)
        | _ -> assert false
        ) deps in
    let cmx_and_cmxa_deps =
      List.map (function
        | `Unit _ as t -> `N (t, `Cmx)
        | `Lib _ as t  -> `N (t, `Cmxa)
        | _ -> assert false
        ) deps in
    let byte =
      let phase = `Link `Byte in
      As_action.rule
        ~phase
        ~targets:[`Self `Byte]
        ~prereqs:(`Self `Dir :: cmo_and_cma_deps)
        (fun t r f ->
           As_action.create "%s %s -o %s"
             (As_resolver.ocamlc r)
             (String.concat " " (As_flags.get phase f))
             (file t r `Byte))
    in
    let native =
      let phase = `Link `Native in
      As_action.rule
        ~phase
        ~targets:[`Self `Native]
        ~prereqs:(`Self `Dir :: cmx_and_cmxa_deps)
        (fun t r f ->
           As_action.create "%s %s -o %s"
             (As_resolver.ocamlopt r)
             (String.concat " " (As_flags.get phase f))
             (file t r `Native))
    in
    let js =
      let phase = `Link `Js in
      As_action.rule
        ~phase
        ~targets:[`Self `Js]
        ~prereqs:[`Self `Byte]
        (fun t r f ->
           As_action.create "%s %s %s"
             (As_resolver.js_of_ocaml r)
             (String.concat " " (As_flags.get phase f))
             (file t r `Js))
    in
    [ Rule.mkdir; byte; native; js ]
    @ match t.base_payload.b_origin with
    | `Other o -> rules (`Other o)
    | `Units _ -> []

  let files = [
    As_features.byte  , [`Byte];
    As_features.native, [`Native];
    As_features.js    , [`Js];
  ]

  let create ?(available = As_features.true_) ?(flags=As_flags.empty) ?deps
      ?(byte = true) ?(native = true) ?(js = false)
      ?(linkall = false) ?(install = true)
      name origin
    =
    let available =
      let not_native = not native in
      let not_byte = not byte in
      let not_js = not js in
      As_features.(available &&&
                   neg ~on:not_native native &&&
                   neg ~on:not_byte byte &&&
                   neg ~on:not_js js)
    in
    let flags t r =
      let flags = if linkall then As_flags.(linkall @@@ flags) else flags in
      As_flags.(flags @@@ mk_flags t r)
    in
    let base = Base.create ~available ~flags ~rules ~files ?deps name `Bin
        { b_origin = `Units []; b_toplevel = false;
          b_install = install; b_js = js }
    in
    let b = ref base in
    let () = match origin with
    | `Units us ->
        let us = List.map (function `Unit u -> u) us in
        let us = Unit.map (Base.with_parent (fun () -> `Bin !b)) us in
        let contents = List.map (fun u -> `Unit u) us in
        b := with_origin (`Units us) (Base.with_contents contents base)
    | `Other o  ->
        let files = Other.self_targets o in
        let no_byte = not (List.mem `Byte files) in
        let no_native = not (List.mem `Native files) in
        let no_js = not (List.mem `Js files) in
        let available =
          As_features.(available &&&
                       neg ~on:no_byte byte &&&
                       neg ~on:no_native native &&&
                       neg ~on:no_js js)
        in
        b := with_origin (`Other o) (Base.with_available available base)
    in
    !b

  let toplevel ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(deps = []) ?(custom = false) ?install name comps
    =
    let available = As_features.(neg native &&& available) in
    let deps = `Pkg (Pkg.compiler_libs_toplevel) :: deps in
    let link_byte = [
      (if custom then "-custom " else "") ^ "-I +compiler-libs topstart.cmo"
    ] in
    let nflags = As_flags.v (`Link `Byte) link_byte in
    let flags = As_flags.(flags @@@ nflags) in
    let t = create ~available ~flags ~linkall:true ~deps ?install name comps in
    { t with base_payload = { t.base_payload with b_toplevel = true } }
end

module Test = struct
  type args = test_args
  type command = test_command
  type t = test

  let commands t = t.base_payload.t_commands
  let dir t = t.base_payload.t_dir

  let rules t =
    let deps = List.fold_left (fun acc -> function
      | `Bin _ as c -> `N (c, `Byte) :: acc
      | _ -> acc
      ) [] (deps (`Test t)) in
    [ Rule.mkdir;
      As_action.rule
        ~phase:`Test
        ~targets:[`Phony (Rule.phony_run (`Test t))]
        ~prereqs:(`Self `Dir :: deps)
        (fun _t r f ->
           let dir = dir t in
           List.map (function
             | `Shell cmd -> As_action.create ?dir "%s" cmd
             | `Bin (`Bin b, args) ->
                 let c = (`Bin b:>component) in
                 let args = String.concat " " (args r) in
                 let bin =
                   As_resolver.root_dir r / file c r `Byte
                 in
                 As_action.create ?dir "%s %s %s"
                   bin (String.concat " " (As_flags.get `Test f)) args
             ) (commands t)
           |> As_action.seq
        )]

  let create ?available ?flags ?(deps = [])
      ?dir name cmds =
    let deps =
      let add_dep acc = function
      | `Shell _ -> acc | `Bin (`Bin bin, _) -> `Bin bin :: acc
      in
      List.fold_left add_dep deps cmds
    in
    let flags = match flags with
    | None   -> None
    | Some f -> Some (fun _ _ -> f)
    in
    Base.create ?available ?flags ~rules ~deps name `Test
      { t_dir = dir; t_commands = cmds; }
end

module Doc = struct
  type t = doc

  let mk_flags t r =
    let deps = closure ~link:true [`Doc t] in
    let pkgs = deps |> filter_map pkg |> List.map Pkg.name |> As_resolver.pkgs r
    in
    let incls =
      deps
      |> List.fold_left (fun acc -> function
        | `Unit _ | `Lib _ as t ->
            let bdir = build_dir t r in
            if List.mem bdir acc then acc
            else bdir :: acc
        | _ -> acc
        ) []
      |> List.rev_map (fun d -> "-I " ^ d)
    in
    let open As_flags in
    v `Doc (get (`Compile `Byte) pkgs) @@@
    v `Doc incls

  let units t =
    List.fold_left (fun acc -> function
      | `Unit u -> u :: acc
      | c -> filter_map unit (contents c) @ acc
      ) [] (contents (`Doc t))
    |> List.rev

  (* FIXME: support opamdoc *)
  let rules t =
    let units = units t in
    let deps = List.map (fun u -> `N (`Unit u, `Cmi)) units in
    [ Rule.mkdir
    ; As_action.rule
        ~phase:`Doc
        ~targets:[`Phony (id (`Doc t))]
        ~prereqs:(`Self `Dir :: deps)
        (fun t r f ->
           let files = conmap (fun u ->
               let ml = file (`Unit u) r `Ml in
               let mli = file (`Unit u) r `Mli in
               if Unit.has `Mli u then [mli] else
               if Unit.has `Ml u then [ml] else
               []
             ) units
           in
           let bdir = build_dir t r in
           let flags = As_flags.get `Doc f in
           As_action.create "%s %s \
                            \ %s -short-functors -colorize-code -html -d %s"
             (As_resolver.ocamldoc r)
             (String.concat " " flags)
             (String.concat " " files)
             bdir) ]

  let create ?available ?(flags = As_flags.empty) ?deps
      ?(install = true) name contents =
    let flags t r =
      let open As_flags in
      flags @@@ mk_flags t r
    in
    Base.create ?available ~flags ?deps ~rules ~contents name `Doc
      { doc_install = install; }
end
