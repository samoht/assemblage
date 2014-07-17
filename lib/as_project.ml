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
let (//) x y = match x with
| None   -> y
| Some x -> Filename.concat x y

module StringSet = Set.Make (String)

type component =
  [ `Unit of comp_unit
  | `Gen of gen
  | `File of file
  | `C of c
  | `JS of js
  | `Pkg_pp of pkg
  | `Pkg of pkg
  | `Lib of lib
  | `Pp of lib
  | `Bin of bin
  | `Dir of dir
  | `Test of test ]

and comp_unit =
  { cu_name : string;
    cu_available : As_features.t;
    mutable cu_flags : As_flags.t;
    cu_dir : string option;
    mutable cu_deps : component list;
    mutable cu_container: [`Lib of lib | `Bin of bin] option;
    mutable cu_for_pack : string option;
    mutable cu_pack : comp_unit list;
    cu_generated : bool;
    cu_mli : bool;
    cu_ml  : bool; }

and file =
  { f_name : string;
    f_available : As_features.t;
    f_flags : As_flags.t;
    f_deps : component list;
    f_dir : string option; }

and gen =
  { g_name : string;
    g_available : As_features.t;
    g_flags : As_flags.t;
    mutable g_comp : comp_unit option;
    g_action : As_action.t option;
    g_deps : component list;
    g_files : [`C |`Ml | `Mli] list; }

and c =
  { c_name : string;
    c_available : As_features.t;
    c_flags : As_flags.t;
    c_dir : string option;
    c_link_flags : string list;
    c_deps : component list;
    c_generated : bool;
    mutable c_container: [`Lib of lib | `Bin of bin] option; }

and js =
  { j_bin : bin;
    j_available : As_features.t;
    j_args: string list; }

and pkg =
  { p_name : string;
    p_available : As_features.t;
    p_flags : As_flags.t; }

and lib =
  { l_name : string;
    l_available : As_features.t;
    mutable l_flags : As_flags.t;
    l_cus : comp_unit list;
    l_cs  : c list;
    mutable l_filename : string; }

and bin =
  { b_name : string;
    b_available : As_features.t;
    b_flags : As_flags.t;
    b_deps : component list;
    b_comps : comp_unit list;
    b_toplevel : bool;
    b_install : bool; }

and dir =
  { d_name :
      [ `Lib | `Bin | `Sbin | `Toplevel | `Share | `Share_root | `Etc | `Doc
      | `Misc | `Stublibs | `Man | `Other of string ];
    d_available : As_features.t;
    d_flags : As_flags.t;
    d_deps : component list;
    d_install : bool;
    d_contents : component list; }

and test_args = (component -> string) -> string list
and test_command =
  [ `Bin of [`Bin of bin] * test_args
  | `Shell of string ]

and test =
  { t_name : string;
    t_available : As_features.t;
    t_flags : As_flags.t;
    t_dir : string option;
    t_commands : test_command list;
    t_deps : component list; }

type t =
  { name : string;
    version : string;
    available : As_features.t;
    flags : As_flags.t;
    components : component list;
    doc_css : string option;
    doc_intro : string option;
    doc_dir : string;
    doc_public : string list; }

module type Component_base = sig
  type t
  val id: t -> string
  val name: t -> string
  val available: t -> As_features.t
  val flags: t -> As_resolver.t -> As_flags.t
  val deps: t -> component list
  val build_dir: t -> As_resolver.t -> string
  val file: t -> As_resolver.t -> string -> string
  val generated_files: t -> As_resolver.t -> (As_features.t * string list) list
  val prereqs: t -> As_resolver.t -> [`Byte|`Native|`Shared] -> string list
end

module type Graph = sig
  include Graph.Sig.I
  val iter: (V.t -> unit) -> t -> unit
  val fold: (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  val to_list: t -> V.t list
  val of_list: V.t list -> t
end

module type Set = sig
  include Set.S
  val of_list: elt list -> t
end

module Graph (X: sig type t val id: t -> string val deps: t -> t list end) =
struct
    module G = Graph.Imperative.Digraph.ConcreteBidirectional(struct
        type t = X.t
        let compare x y = String.compare (X.id x) (X.id y)
        let equal x y = X.id x = X.id y
        let hash x = Hashtbl.hash (X.id x)
      end)
    include G
    include Graph.Topological.Make(G)

    let to_list t =
      fold (fun v acc -> v :: acc) t []
      |> List.rev

    let of_list ts =
      let g = create () in
      List.iter (fun t ->
          List.iter (fun d -> if List.mem d ts then add_edge g d t) (X.deps t)
        ) ts;
      g
end

module rec Component : sig
  include Component_base with type t = component
  val unit : t -> comp_unit option
  val file_ : t -> file option
  val gen: t -> gen option
  val c: t -> c option
  val js: t -> js option
  val pkg: t -> pkg option
  val pkg_pp: t -> pkg option
  val lib: t -> lib option
  val pp: t -> lib option
  val bin: t -> bin option
  val dir: t -> dir option
  val test: t -> test option
  val filter: (t -> 'a option) -> t list -> 'a list
  val closure: t list -> t list
  val comp_byte: t list -> As_resolver.t -> (As_resolver.t -> string) ->
    string list
  val comp_native: t list -> As_resolver.t -> (As_resolver.t -> string) ->
    string list
  val pp_byte: t list -> As_resolver.t -> string list
  val pp_native: t list -> As_resolver.t -> string list
  val link_byte: t list -> As_resolver.t -> comp_unit list ->
    string list
  val link_native: t list -> As_resolver.t -> comp_unit list ->
    string list
 module Graph: Graph with type V.t = component
 module Set: Set with type elt = component
end = struct
  type t = component
  type component = t

  let id = function
  | `Unit cu  -> Unit.id cu
  | `File f -> File.id f
  | `Gen g  -> Gen.id g
  | `C c    -> C.id c
  | `JS js  -> JS.id js
  | `Pkg p
  | `Pkg_pp p -> Pkg.id p
  | `Lib l
  | `Pp l   -> Lib.id l
  | `Bin b  -> Bin.id b
  | `Dir d -> Dir.id d
  | `Test t -> Test.id t

  let name = function
  | `Unit cu  -> Unit.name cu
  | `File f -> File.name f
  | `Gen g  -> Gen.name g
  | `C c    -> C.name c
  | `JS js  -> JS.name js
  | `Pkg p
  | `Pkg_pp p -> Pkg.name p
  | `Lib l
  | `Pp l   -> Lib.name l
  | `Bin b  -> Bin.name b
  | `Dir d -> Dir.name d
  | `Test t -> Test.name t

  let available = function
  | `Unit cu  -> Unit.available cu
  | `File f -> File.available f
  | `Gen g  -> Gen.available g
  | `C c    -> C.available c
  | `JS js  -> JS.available js
  | `Pkg p
  | `Pkg_pp p -> Pkg.available p
  | `Lib l
  | `Pp l   -> Lib.available l
  | `Bin b  -> Bin.available b
  | `Dir d -> Dir.available d
  | `Test t -> Test.available t

  let prereqs = function
  | `Unit cu  -> Unit.prereqs cu
  | `File f -> File.prereqs f
  | `Gen g  -> Gen.prereqs g
  | `C c    -> C.prereqs c
  | `JS js  -> JS.prereqs js
  | `Pkg p
  | `Pkg_pp p -> Pkg.prereqs p
  | `Lib l
  | `Pp l   -> Lib.prereqs l
  | `Bin b  -> Bin.prereqs b
  | `Dir d -> Dir.prereqs d
  | `Test t -> Test.prereqs t

  let flags = function
  | `Unit cu  -> Unit.flags cu
  | `File f -> File.flags f
  | `Gen g  -> Gen.flags g
  | `C c    -> C.flags c
  | `JS js  -> JS.flags js
  | `Pkg p
  | `Pkg_pp p -> Pkg.flags p
  | `Lib l
  | `Pp l   -> Lib.flags l
  | `Bin b  -> Bin.flags b
  | `Dir d -> Dir.flags d
  | `Test t -> Test.flags t

  let generated_files = function
  | `Unit cu  -> Unit.generated_files cu
  | `File f -> File.generated_files f
  | `Gen g  -> Gen.generated_files g
  | `C c    -> C.generated_files c
  | `JS js  -> JS.generated_files js
  | `Pkg p
  | `Pkg_pp p -> Pkg.generated_files p
  | `Lib l
  | `Pp l   -> Lib.generated_files l
  | `Bin b  -> Bin.generated_files b
  | `Dir d -> Dir.generated_files d
  | `Test t -> Test.generated_files t

  let file = function
  | `Unit cu  -> Unit.file cu
  | `File f -> File.file f
  | `Gen g  -> Gen.file g
  | `C c    -> C.file c
  | `JS js  -> JS.file js
  | `Pkg p
  | `Pkg_pp p -> Pkg.file p
  | `Lib l
  | `Pp l   -> Lib.file l
  | `Bin b  -> Bin.file b
  | `Dir d -> Dir.file d
  | `Test t -> Test.file t

  let build_dir = function
  | `Unit cu  -> Unit.build_dir cu
  | `File f -> File.build_dir f
  | `Gen g  -> Gen.build_dir g
  | `C c    -> C.build_dir c
  | `JS js  -> JS.build_dir js
  | `Pkg p
  | `Pkg_pp p -> Pkg.build_dir p
  | `Lib l
  | `Pp l   -> Lib.build_dir l
  | `Bin b  -> Bin.build_dir b
  | `Dir d -> Dir.build_dir d
  | `Test t -> Test.build_dir t

  let deps = function
  | `Unit cu  -> Unit.deps cu
  | `File f -> File.deps f
  | `Gen g  -> Gen.deps g
  | `C c    -> C.deps c
  | `JS js  -> JS.deps js
  | `Pkg p
  | `Pkg_pp p -> Pkg.deps p
  | `Lib l
  | `Pp l   -> Lib.deps l
  | `Bin b  -> Bin.deps b
  | `Dir d -> Dir.deps d
  | `Test t -> Test.deps t

  let unit = function `Unit x -> Some x | _ -> None
  let file_ = function `File x -> Some x | _ -> None
  let gen = function `Gen x -> Some x | _ -> None
  let c = function `C c -> Some c | _ -> None
  let js = function `JS x -> Some x | _ -> None
  let pkg = function `Pkg x -> Some x | _ -> None
  let pkg_pp = function `Pkg_pp x -> Some x | _ -> None
  let lib = function `Lib x -> Some x | _ -> None
  let pp = function `Pp x -> Some x | _ -> None
  let bin = function `Bin x -> Some x | _ -> None
  let dir = function `Dir x -> Some x | _ -> None
  let test = function `Test x -> Some x | _ -> None

  let filter fn l =
    List.fold_left (fun acc x ->
        match fn x with
        | None   -> acc
        | Some x -> x :: acc
      ) [] l
  |> List.rev

  let closure (ts:t list): t list =
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
          let d' = List.filter
              (function `Pkg_pp _ | `Pp _ -> false | _     -> true)
              (deps h) in
          aux acc (d' @ d)
        )
    in
    aux [] ts

  let comp_flags mode (deps:t list) resolver build_dir =
    let incl = build_dir resolver in
    let cus = filter unit deps |> List.map (fun u -> Unit.build_dir u resolver)
    in
    let libs = filter lib deps |> List.map (fun l -> Lib.build_dir l resolver)in
    let includes =
      (* We need to keep the -I flags in the right order *)
      let iflags inc acc = sprintf "-I %s" inc :: acc in
      let add (seen, acc) i =
        if StringSet.mem i seen then (seen, acc)
        else (StringSet.add i seen, iflags i acc) in
      let (_, incs) =
        List.fold_left add (StringSet.empty, []) (incl :: cus @ libs) in
      List.rev incs
    in
    let pkgs = match filter pkg deps with
    | [] -> []
    | pkgs ->
        let pkgs = List.map Pkg.name pkgs in
        let pkgs = As_resolver.pkgs resolver pkgs in
        match mode with
        | `Byte   -> As_flags.comp_byte pkgs
        | `Native -> As_flags.comp_native pkgs
    in
    pkgs @ [String.concat " " includes]

  let comp_byte = comp_flags `Byte
  let comp_native = comp_flags `Native

  let link_flags mode (deps:t list) resolver comps =
    let comps = List.map (fun u ->
        let file = match mode with
          | `Byte   -> Unit.cmo u resolver
          | `Native -> Unit.cmx u resolver in
        sprintf "%s/%s" (Filename.dirname file) (Filename.basename file)
      ) comps in
    let libs = filter lib deps in
    let libs = List.map (fun l ->
        let file = match mode with
          | `Byte   -> Lib.cma l resolver
          | `Native -> Lib.cmxa l resolver in
        sprintf "%s/%s" (Filename.dirname file) (Filename.basename file)
      ) libs in
    let pkgs = filter pkg deps in
    let pkgs = match pkgs with
      | [] -> []
      | _  ->
        let pkgs = List.map Pkg.name pkgs in
        let pkgs = As_resolver.pkgs resolver pkgs in
        match mode with
        | `Byte   -> As_flags.link_byte pkgs
        | `Native -> As_flags.link_native pkgs in
    pkgs @ libs @ comps

  let link_byte = link_flags `Byte
  let link_native = link_flags `Native

  let pp_flags mode (deps:t list) resolver =
    let libs = filter pp deps in
    let pkgs = filter pkg_pp deps in
    match libs, pkgs with
    | [], [] -> []
    | _ , _  ->
      let libs = List.map (fun l ->
          sprintf "%s/%s" (Lib.build_dir l resolver)
            (match mode with
             | `Byte   -> Lib.cma  l resolver
             | `Native -> Lib.cmxa l resolver)
        ) libs in
      let pkgs =
        let pkgs = List.map Pkg.name pkgs in
        let pkgs = As_resolver.pkgs resolver pkgs in
        match mode with
        | `Byte   -> As_flags.pp_byte pkgs
        | `Native -> As_flags.pp_native pkgs in
      pkgs @ libs

  let pp_byte = pp_flags `Byte
  let pp_native = pp_flags `Native

  module Graph = Graph(struct
      type t = component
      let id = id
      let deps = deps
    end)

  module Set = struct

    include Set.Make(struct
        type t = component
        let compare x y = String.compare (id x) (id y)
      end)

    let of_list l =
      let add set elt = add elt set in
      List.fold_left add empty l
  end
end

and Unit: sig
  include Component_base with type t = comp_unit

  val create :
    ?available:As_features.t ->
    ?flags:As_flags.t ->
    ?dir:string ->
    ?deps:Component.t list -> string -> t

  val copy: t -> t
  val dir: t -> string option
  val container: t -> [`Lib of Lib.t |`Bin of Bin.t]  option
  val mli: t -> bool
  val ml: t -> bool
  val for_pack: t -> string option
  val generated: t -> bool
  val pack : ?available:As_features.t ->
    ?flags:As_flags.t -> t list -> string -> t
  val unpack: t -> t list
  val cmi: t -> As_resolver.t -> string
  val cmo: t -> As_resolver.t -> string
  val cmx: t -> As_resolver.t -> string
  val o: t -> As_resolver.t -> string
  (* not exported *)
  val set_lib_container: t -> Lib.t -> unit
  val set_bin_container: t -> Bin.t -> unit
  val add_deps: t -> Component.t list -> unit
end = struct
  type t = comp_unit

  let id t =
    match t.cu_container with
    | None          -> t.cu_name
    | Some (`Lib l) -> Lib.id l ^ "-" ^ t.cu_name
    | Some (`Bin b) -> Bin.id b ^ "-" ^ t.cu_name

  let ml t = t.cu_ml
  let mli t = t.cu_mli
  let generated t = t.cu_generated
  let rec copy t =
    { cu_dir       = t.cu_dir;
      cu_available = t.cu_available;
      cu_deps      = t.cu_deps;
      cu_name      = t.cu_name;
      cu_container = t.cu_container;
      cu_for_pack  = t.cu_for_pack;
      cu_pack      = List.map copy t.cu_pack;
      cu_flags     = t.cu_flags;
      cu_ml        = t.cu_ml;
      cu_mli       = t.cu_mli;
      cu_generated = t.cu_generated;
    }

  let dir t = t.cu_dir

  let build_dir t resolver =
    match t.cu_container with
    | None          -> As_resolver.build_dir resolver ""
    | Some (`Lib l) -> Lib.build_dir l resolver
    | Some (`Bin b) -> Bin.build_dir b resolver

  let name t = t.cu_name
  let available t = t.cu_available
  let deps t = t.cu_deps
  let container t = t.cu_container
  let for_pack t = t.cu_for_pack
  let unpack t = t.cu_pack
  let set_lib_container t lib = t.cu_container <- Some (`Lib lib)
  let set_bin_container t bin = t.cu_container <- Some (`Bin bin)
  let add_deps t deps = t.cu_deps <- t.cu_deps @ deps

  let create ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?dir ?(deps = []) name
    =
    let gens = Component.(filter gen) deps in
    let mli = match gens with
    | [] -> Sys.file_exists (dir // name ^ ".mli")
    | _  -> List.exists (fun g -> List.mem `Mli g.g_files) gens
    in
    let ml = match gens with
    | [] -> Sys.file_exists (dir // name ^ ".ml")
    | _  -> List.exists (fun g -> List.mem `Ml g.g_files) gens
    in
    if not ml && not mli
    then
      As_shell.fatal_error 1
        "unit %s: cannot find %s.ml or %s.mli in `%s', stopping.\n"
        name name name (match dir with None -> "." | Some d -> d / "")
    else
    let cu_generated = gens <> [] in
    let t =
      { cu_name = name; cu_available = available; cu_dir = dir; cu_deps = deps;
        cu_flags = flags; cu_container = None; cu_for_pack = None; cu_pack = [];
        cu_generated; cu_ml = ml; cu_mli = mli; }
    in
    (* FIXME: mutation *)
    List.iter (fun g ->
        let g = match g.g_comp with
        | None   -> g
        | Some _ -> { g with g_comp = None } in
        g.g_comp <- Some t
      ) gens;
    t

  let pack ?(available = As_features.true_) ?(flags = As_flags.empty) comps
      name
    =
    (* FIXME: mutation *)
    List.iter (fun u -> u.cu_for_pack <- Some (String.capitalize name)) comps;
    { cu_name = name; cu_available = available; cu_flags = flags; cu_dir = None;
      cu_container = None; cu_for_pack = None; cu_deps = []; cu_pack = comps;
      cu_ml = false; cu_mli = false; cu_generated = false; }

  let file t r ext = build_dir t r / t.cu_name ^ ext
  let cmi t r = file t r ".cmi"
  let cmo t r = file t r ".cmo"
  let cmx t r = file t r ".cmx"
  let o t r = file t r ".o"
  let cmt t r = file t r ".cmt"
  let cmti t r = file t r ".cmti"

  let generated_files t resolver =
    let mk f = f t resolver in
    [
      As_features.true_ , [mk cmi ; mk cmo ];
      As_features.native, [mk o   ; mk cmx ];
      As_features.annot , [mk cmt ; mk cmti];
    ]

  let prereqs t resolver mode =
    let deps = deps t in
    let comps = Component.(filter unit) deps in
    let comps = conmap (fun u ->
        match mode with
        | `Native -> [cmx u resolver]
        | `Byte   -> [cmi u resolver]
        | `Shared ->
          let cs = Component.(filter c) u.cu_deps in
          let cobjs = List.map (fun c -> C.dll_so c resolver) cs in
          cmx u resolver :: cobjs
      ) comps in
    let libs = Component.(filter lib) deps in
    let libs = List.map (fun l ->
        match mode with
        | `Shared
        | `Native -> Lib.cmxa l resolver
        | `Byte   -> Lib.cma  l resolver
      ) libs in
    let pps = Component.(filter pp) deps in
    let pps = List.map (fun l -> Lib.cma l resolver) pps in
    comps @ libs @ pps

  (* XXX: memoize the function *)
  let flags t resolver =
    let deps = deps t |> Component.closure in
    let comp_byte = Component.comp_byte deps resolver (build_dir t) in
    let comp_native = Component.comp_native deps resolver (build_dir t) in
    let pp_byte = Component.pp_byte deps resolver in
    let pp_native = Component.pp_native deps resolver in
    let t' = As_flags.create ~comp_byte ~comp_native ~pp_byte ~pp_native () in
    As_flags.(t' @@@ t.cu_flags)

end

and File : sig
  include Component_base with type t = file
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list -> ?dir:string -> string -> file
end = struct
  type t = file

  let create ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(deps = []) ?dir f_name =
    { f_name; f_available = available; f_flags = flags; f_deps = deps;
      f_dir = dir }

  let id f = "file-" ^ f.f_name
  let name f = f.f_name
  let available f = f.f_available
  let flags f _ = f.f_flags
  let deps f = f.f_deps
  let prereqs _ _ _ = []
  let file _ = invalid_arg "File.file: not applicable"
  let build_dir _ = invalid_arg "File.build_dir: not applicable"
  let generated_files f r =
    let file = As_resolver.build_dir r f.f_name in
    [f.f_available, [ file ]]
end


and Gen: sig
  include Component_base with type t = gen

  val create: ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:Component.t list -> ?action:As_action.t ->
    [`C | `Ml | `Mli ] list -> string -> t
  val copy: t -> t
  val files: t -> As_resolver.t -> string list
  val actions: t -> As_resolver.t -> string list
end = struct
  type t = gen

  let id t = "gen-" ^ t.g_name
  let name t = t.g_name
  let available g = g.g_available
  let deps t = t.g_deps

  let create ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(deps = []) ?action g_files g_name
    =
    { g_name; g_available = available; g_flags = flags; g_comp = None; g_files;
      g_action = action; g_deps = deps }

  let copy t = { t with g_comp = None }

  let build_dir t r =
    match t.g_comp with
    | None   -> As_resolver.build_dir r ""
    | Some u -> Unit.build_dir u r

  let prereqs t r mode =
    let bins = Component.(filter bin t.g_deps) in
    List.map (fun b -> match mode with
        | `Byte   -> Bin.byte b r
        | `Shared
        | `Native -> Bin.native b r
      ) bins

  let flags t _ = t.g_flags
  let file t r ext = build_dir t r / t.g_name ^ ext
  let ml t r = file t r ".ml"
  let mli t r = file t r ".mli"

  let files t r =
    List.map (function
        | `C    -> file t r ".c"
        | `Ml   -> ml t r
        | `Mli  -> mli t r
      ) t.g_files

  let generated_files t r = [ As_features.true_, files t r ]
  let actions t r =
    match t.g_action with
    | None   -> []
    | Some a -> As_action.actions a r
end

and C: sig
  include Component_base with type t = c
  val create :
    ?available:As_features.t -> ?flags:As_flags.t ->
    ?dir:string -> ?generated:bool -> ?link_flags:string list ->
    ?deps:Component.t list -> string -> t
  val container: t -> [`Lib of Lib.t |`Bin of Bin.t]  option
  val set_lib_container: t -> Lib.t -> unit
  val set_bin_container: t -> Bin.t -> unit
  val link_flags: t -> string list
  val dll_so: t -> As_resolver.t -> string
  val symlink_c: t -> As_resolver.t -> string
  val o: t -> As_resolver.t -> string
end = struct
  type t = c

  let id t = "c-" ^ t.c_name
  let name t = t.c_name
  let available t = t.c_available

  let create ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?dir ?(generated = false) ?(link_flags = []) ?(deps = []) name
    =
    { c_name = name; c_available = available; c_flags = flags; c_dir = dir;
      c_link_flags = link_flags; c_deps = deps; c_generated = generated;
      c_container = None }

  let build_dir t r =
    match t.c_container with
    | Some (`Lib l) -> Lib.build_dir l r
    | Some (`Bin b) -> Bin.build_dir b r
    | None          -> As_resolver.build_dir r (id t)

  let container t = t.c_container
  let set_lib_container t lib = t.c_container <- Some (`Lib lib)
  let set_bin_container t bin = t.c_container <- Some (`Bin bin)
  let file t r ext = build_dir t r / t.c_name ^ ext
  let dll_so t r = build_dir t r / "dll" ^ t.c_name ^ ".so"
  let o t r = file t r ".o"
  let symlink_c t r = file t r ".c"
  let prereqs _t _r _mode = failwith "C.prereqs"
  let deps t = t.c_deps
  let generated_files t r = [As_features.true_, [dll_so t r]]
  let flags t _ = t.c_flags
  let link_flags t = t.c_link_flags
end

and JS: sig
  include Component_base with type t = js
  val create: ?available:As_features.t -> Bin.t -> string list -> t
  val js: t -> As_resolver.t -> string
end = struct
  type t = js

  let bin t = t.j_bin
  let name t = Bin.name (bin t)
  let available t = t.j_available

  let id t = Bin.id t.j_bin ^ "-js"
  let create ?(available = As_features.true_) bin args =
    { j_bin  = bin; j_available = available; j_args = args; }

  let file t r e = Bin.file (bin t) r e
  let js t r = Bin.file (bin t) r ".js"
  let flags t _r = As_flags.create ~link_byte:t.j_args ()
  let build_dir t r = Bin.build_dir (bin t) r
  let prereqs t r _ = [Bin.byte (bin t) r]
  let generated_files t r = [As_features.js, [js t r]]
  let deps t = [`Bin (bin t)]
end

and Pkg: sig
  include Component_base with type t = pkg
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?opt:bool -> string -> is_pp:bool -> t
  val compiler_libs_toplevel : t
  val ctypes_stub : t
end = struct
  type t = pkg

  let create ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(opt = false) name ~is_pp
    =
    let opt_feature name is_pp =
      let kind = if is_pp then " pre-processeor" else "" in
      let doc = sprintf "%s%s package available" name kind in
      As_features.create name ~default:true ~doc
    in
    let pkg_f = if opt then opt_feature name is_pp else As_features.true_ in
    let p_available = As_features.(available &&& pkg_f) in
    { p_name = name; p_flags = flags; p_available }

  let id t = "pkg-" ^ t.p_name
  let name t = t.p_name
  let available t = t.p_available
  let prereqs _t _r _mode = []
  let flags t r = As_flags.(As_resolver.pkgs r [t.p_name] @@@ t.p_flags)
  let build_dir _t _r = failwith "Pkg.build_dir"
  let deps _t = []
  let generated_files _t _r = []
  let file _t _r _ext = failwith "Pkg.file"

  (* Builtin packages *)

  let compiler_libs_toplevel = create "compiler-libs.toplevel" ~is_pp:false
  let ctypes_stub = create "ctypes.stubs" ~is_pp:false
end

and Lib: sig
  include Component_base with type t = lib
  val create :
    ?available:As_features.t ->
    ?flags:As_flags.t ->
    ?pack:bool ->
    ?deps:(string -> Component.t list) ->
    ?c:C.t list ->
    Unit.t list -> string -> t
  val filename: t -> string
  val units: t -> Unit.t list
  val c_objects: t -> C.t list
  val available: t -> As_features.t
  val cma: t -> As_resolver.t -> string
  val cmxa: t -> As_resolver.t -> string
  val a: t -> As_resolver.t -> string
  val cmxs: t -> As_resolver.t -> string
  (* not exported *)
  val set_filename: t -> string -> unit
end = struct
  type t = lib

  let id t = "lib-" ^ t.l_name

  let deps t =
    let cus = t.l_cus |> conmap Unit.deps in
    let cs  = t.l_cs  |> conmap C.deps in
    cus @ cs
    |> Component.Set.of_list
    |> Component.Set.elements

  let name t = t.l_name
  let build_dir t resolver = As_resolver.build_dir resolver (id t)
  let units t = t.l_cus
  let c_objects t = t.l_cs
  let filename t = t.l_filename
  let set_filename t f = t.l_filename <- f
  let available t = t.l_available
  let nil _ = []

  let create ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(pack = false) ?(deps = nil) ?(c = []) cus name
    =
    let cus' = if pack then [Unit.pack cus name] else cus in
    let t =
      { l_name = name; l_available = available; l_cus = cus'; l_cs = c;
        l_flags = flags; l_filename = name; }
    in
    (* FIXME: mutation *)
    List.iter (fun u -> Unit.add_deps u (deps (Unit.name u))) cus;
    List.iter (fun u -> Unit.set_lib_container u t) (cus' @ cus);
    List.iter (fun c -> C.set_lib_container c t) c;
    t

  let file t r ext = build_dir t r / t.l_name ^ ext
  let cma t r = file t r ".cma"
  let cmxa t r = file t r ".cmxa"
  let cmxs t r = file t r ".cmxs"
  let a t r = file t r ".a"

  let generated_files t resolver =
    let mk f = f t resolver in
    [
      t.l_available                              , [mk cma]       ;
      As_features.(native         &&& t.l_available), [mk cmxa; mk a];
      As_features.(native_dynlink &&& t.l_available), [mk cmxs]      ;
    ]
    @ conmap (fun u -> Unit.generated_files u resolver) t.l_cus

  let flags t resolver =
    let comps = units t in
    let link_byte = Component.link_byte [] resolver comps in
    let link_native = Component.link_native [] resolver comps in
    let t' = As_flags.create ~link_byte ~link_native () in
    As_flags.(t' @@@ t.l_flags)

  let rec prereqs t resolver mode =
    let c_deps = match mode with
      | `Byte
      | `Native -> List.map (fun c -> C.o c resolver) (c_objects t)
      | `Shared -> List.map (fun c -> C.dll_so c resolver) (c_objects t) in
    let ml_deps = match mode with
      | `Byte   -> List.map (fun u -> Unit.cmo u resolver) (units t)
      | `Native -> List.map (fun u -> Unit.cmx u resolver) (units t)
      | `Shared -> prereqs t resolver `Native in
    c_deps @ ml_deps

end

and Bin: sig
  include Component_base with type t = bin
  val create :
    ?available:As_features.t ->
    ?flags:As_flags.t ->
    ?byte_only:bool ->
    ?link_all:bool ->
    ?install:bool ->
    ?deps:(string -> Component.t list) -> Unit.t list -> string -> t
  val toplevel:
    ?available:As_features.t ->
    ?flags:As_flags.t ->
    ?custom:bool ->
    ?install:bool ->
    ?deps:(string -> Component.t list) ->
    Unit.t list -> string -> t
  val units: t -> Unit.t list
  val available: t -> As_features.t
  val is_toplevel: t -> bool
  val install: t -> bool
  val byte: t -> As_resolver.t -> string
  val native: t -> As_resolver.t -> string
end = struct
  type t = bin

  let id t = "bin-" ^ t.b_name
  let units t = t.b_comps
  let build_dir t resolver = As_resolver.build_dir resolver (id t)
  let is_toplevel t = t.b_toplevel
  let install t = t.b_install
  let name t = t.b_name
  let deps t = t.b_deps
  let available t = t.b_available
  let nil _ = []

  let create ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(byte_only = false) ?(link_all = false) ?(install = true)
      ?(deps = nil) comps name
    =
    let available =
      if byte_only then As_features.(not_ native &&& available) else available
    in
    let flags =
      if link_all then As_flags.(linkall @@@ flags) else flags
    in
    (* FIXME: side effect *)
    List.iter (fun cu ->
        Unit.add_deps cu (deps (Unit.name cu))
      ) comps;
    let deps =
      List.fold_left (fun deps cu ->
          Component.Set.(union deps (of_list (Unit.deps cu)))
        ) Component.Set.empty comps
      |> Component.Set.elements in
    let t =
      { b_deps = deps; b_available = available; b_flags = flags;
        b_name = name; b_comps = comps; b_toplevel = false;
        b_install = install }
    in
    List.iter (fun u -> Unit.set_bin_container u t) comps;
    t

  let toplevel ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(custom = false) ?install ?(deps = nil) comps name
    =
    let available = As_features.(not_ native &&& available) in
    let deps x = `Pkg (Pkg.compiler_libs_toplevel) :: deps x in
    let link_byte = [
      (if custom then "-custom " else "") ^ "-I +compiler-libs topstart.cmo"
    ] in
    let nflags = As_flags.create ~link_byte () in
    let flags = As_flags.(nflags @@@ flags) in
    let t = create ~available ~flags ~link_all:true ~deps ?install comps name in
    { t with b_toplevel = true }

  let file t r ext = build_dir t r / t.b_name ^ ext
  let byte t r = file t r ".byte"
  let native t r = file t r ".native"

  let generated_files t resolver =
    let mk f = f t resolver in
    [
      t.b_available                      , [mk byte  ];
      As_features.(native &&& t.b_available), [mk native];
    ]

  (* XXX: handle native pps *)
  let prereqs t resolver mode =
    let comps = units t in
    let deps = deps t in
    let libs = Component.(filter lib) deps in
    let pps = Component.(filter pp) deps in
    let bytpps = List.map (fun l -> Lib.cma l resolver) pps in
    match mode with
    | `Byte   ->
      let bytlibs  = List.map (fun l -> Lib.cma  l resolver) libs in
      let bytcomps = List.map (fun u -> Unit.cmo u resolver) comps in
      bytpps @ bytlibs @ bytcomps
    | `Shared
    | `Native ->
      let natlibs  = List.map (fun l -> Lib.cmxa l resolver) libs in
      let natcomps = List.map (fun u -> Unit.cmx u resolver) comps in
      bytpps @ natlibs @ natcomps

  let flags t resolver =
    let comps = units t in
    let all_deps =
      (deps t @ List.map (fun x -> `Unit x) comps) |> Component.closure in
    let comp_byte = Component.comp_byte (deps t) resolver (build_dir t) in
    let comp_native = Component.comp_native (deps t) resolver (build_dir t) in
    let link_byte = Component.link_byte all_deps resolver comps in
    let link_native = Component.link_native all_deps resolver comps in
    let t' = As_flags.create ~link_byte ~link_native ~comp_byte ~comp_native ()
    in
    As_flags.(t' @@@ t.b_flags)

end

and Dir : sig
  include Component_base with type t = dir
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list -> ?install:bool ->
    [ `Lib | `Bin | `Sbin | `Toplevel | `Share | `Share_root | `Etc | `Doc
    | `Misc | `Stublibs | `Man | `Other of string ] -> component list -> dir
end = struct
  type t = dir

  let create ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(deps = []) ?(install = true) d_name d_contents =
    { d_name; d_available = available; d_flags = flags; d_deps = deps;
      d_install = install; d_contents = d_contents; }

  let name d = match d.d_name with
  | `Lib -> "lib" | `Bin -> "bin" | `Sbin -> "sbin" | `Toplevel -> "toplevel"
  | `Share -> "share" | `Share_root -> "share-root" | `Etc -> "etc"
  | `Doc -> "doc" | `Misc -> "misc" | `Stublibs -> "stublibs"
  | `Man -> "man" | `Other n -> n

  let id d = "dir-" ^ (name d)
  let available d = d.d_available
  let flags d _ = d.d_flags
  let deps d = d.d_deps
  let prereqs _ _ _ = [] (* FIXME mode doesn't make sense *)
  let file _ = invalid_arg "Dir.file: not applicable"
  let build_dir d r = As_resolver.build_dir r (id d)
  let generated_files d r =
    let add c =
      let refine_avail (a, files) = As_features.(d.d_available &&& a), files in
      List.map refine_avail (Component.generated_files c r)
    in
    List.flatten (List.map add d.d_contents)
end

and Test: sig
  include Component_base with type t = test
  type args = test_args
  type command = test_command
  val create: ?available:As_features.t -> ?flags:As_flags.t ->
    ?dir:string -> ?deps:Component.t list -> command list -> string -> t
  val dir: t -> string option
  val commands: t -> command list
end = struct
  type args = test_args
  type command = test_command
  type t = test

  let id t = "test-" ^ t.t_name
  let name t = t.t_name
  let available t = t.t_available

  let prereqs t r mode =
    List.fold_left (fun acc -> function
        | `Bin bin ->
          begin match mode with
            | `Byte   -> Bin.byte bin r   :: acc
            | `Shared
            | `Native -> Bin.native bin r :: acc
          end
        | _ -> acc
      ) [] t.t_deps

  let flags t _ = t.t_flags
  let generated_files _t _r = []

  let file _t _r _ext = failwith "Test.file"
  let build_dir _t _r = failwith "Test.build_dir"
  let deps t = t.t_deps
  let create ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?dir ?(deps = []) cmds name
    =
    let bin_deps =
      let add_dep acc = function
      | `Shell _ -> acc | `Bin (`Bin bin, _) -> `Bin bin :: acc
      in
      List.fold_left add_dep [] cmds
    in
    { t_name = name; t_available = available; t_flags = flags; t_dir = dir;
      t_commands = cmds; t_deps = bin_deps @ deps; }

  let commands t = t.t_commands
  let dir t = t.t_dir
end

let name t = t.name
let version t = t.version
let components t = t.components
let doc_css t = t.doc_css
let doc_dir t = t.doc_dir
let doc_intro t = t.doc_intro
let doc_public t = t.doc_public

let files_of_generators t resolver =
  let comps = Component.(filter unit t.components) in
  List.fold_left (fun acc u ->
      if not (Unit.generated u) then acc
      else
        let ml = match Unit.ml u with
          | true  -> [Unit.build_dir u resolver / Unit.name u ^ ".ml"]
          | false -> [] in
        let mli = match Unit.mli u with
          | true  -> [Unit.build_dir u resolver / Unit.name u ^ ".mli"]
          | false -> [] in
        ml @ mli @ acc
    ) [] comps

let create ?(available = As_features.true_) ?(flags = As_flags.empty)
    ?doc_css ?doc_intro  ?(doc_dir = "doc")
    ?doc_public ?version name components =
  let version = match version with
    | Some v -> v
    | None   ->
      match As_git.describe () with
      | Some v -> v
      | None   ->
        match As_git.head () with
        | Some v -> v
        | None   -> "version-not-set"
  in
  let components = Component.closure components in
  let libs = Component.(filter lib components) in
  List.iter (fun l ->
      if Lib.name l <> name then
        Lib.set_filename l (name ^ "." ^ Lib.name l)
    ) libs;
  let doc_public = match doc_public with
    | Some d -> d
    | None   -> conmap (function
        | `Lib l -> List.map Unit.name (Lib.units l)
        | `Unit cu -> [Unit.name cu]
        | _      -> []
      ) components in
  { name; version; available; flags; components;
    doc_css; doc_intro; doc_dir; doc_public }

let unionmap fn t =
  List.fold_left (fun set t ->
      As_features.(set ++ (fn t))
    ) As_features.Set.empty t

let features t =
  let libs =
    let libs = Component.(filter lib t.components) in
    unionmap (fun x -> As_features.atoms (Lib.available x)) libs in
  let pps  =
    let pps = Component.(filter pp t.components) in
    unionmap (fun x -> As_features.atoms (Lib.available x)) pps in
  let bins =
    let bins = Component.(filter bin t.components) in
    unionmap (fun x -> As_features.atoms (Bin.available x)) bins in
  As_features.(builtin ++ libs ++ pps ++ bins)
