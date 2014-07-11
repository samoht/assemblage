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
module Flags = As_flags
module Features = As_features
module Resolver = As_resolver
module Action = As_action
module Git = As_git

type component =
  [ `CU of cu
  | `Lib of lib
  | `Pp of lib
  | `Bin of bin
  | `Pkg_pp of string
  | `Pkg of string
  | `C of c
  | `JS of js
  | `Test of test
  | `Gen of gen ]

and cu = {
  cu_name             : string;
  cu_dir              : string option;
  mutable cu_deps     : component list;
  mutable cu_container: [`Lib of lib | `Bin of bin] option;
  mutable cu_for_pack : string option;
  mutable cu_pack     : cu list;
  mutable cu_flags    : Flags.t;
  cu_generated        : bool;
  cu_mli              : bool;
  cu_ml               : bool;
}

and lib = {
  l_name            : string;
  l_cus             : cu list;
  l_cs              : c list;
  mutable l_filename: string;
  l_available       : Features.t;
  mutable l_flags   : Flags.t;
}

and bin = {
  b_deps     : component list;
  b_comps    : cu list;
  b_name     : string;
  b_available: Features.t;
  b_flags    : Flags.t;
  b_toplevel : bool;
  b_install  : bool;
}

and c = {
  c_dir              : string option;
  c_name             : string;
  c_link_flags       : string list;
  c_deps             : component list;
  c_generated        : bool;
  mutable c_container: [`Lib of lib | `Bin of bin] option;
}

and test = {
  t_name     : string;
  t_dir      : string option;
  t_commands : test_command list;
  t_deps     : component list;
}

and js = {
  j_bin : bin;
  j_args: string list;
}

and test_args = (component -> string) -> string list

and test_command =
  [ `Bin of [`Bin of bin] * test_args
  | `Shell of string ]

and gen = {
  g_name        : string;
  mutable g_comp: cu option;
  g_action      : (Resolver.t -> Action.t);
  g_deps        : component list;
  g_files       : [`C|`ML|`MLI] list;
}

type t = {
  name      : string;
  version   : string;
  flags     : Flags.t;
  components: component list;
  doc_css   : string option;
  doc_intro : string option;
  doc_dir   : string;
  doc_public: string list;
}

module type S = sig
  type t
  type component
  val id: t -> string
  val name: t -> string
  val deps: t -> component list
  val build_dir: t -> Resolver.t -> string
  val file: t -> Resolver.t -> string -> string
  val generated_files: t -> Resolver.t -> (Features.t * string list) list
  val flags: t -> Resolver.t -> Flags.t
  val prereqs: t -> Resolver.t -> [`Byte|`Native|`Shared] -> string list
end

let (/) x y = Filename.concat x y

let conmap f l = List.concat (List.map f l)

let (//) x y =
  match x with
  | None   -> y
  | Some x -> Filename.concat x y

let (|>) x f = f x

module type Graph = sig
  include Graph.Sig.I
  val iter: (V.t -> unit) -> t -> unit
  val fold: (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  val vertex: t -> V.t list
end

module type Set = sig
  include Set.S
  val of_list: elt list -> t
end

module Graph (X: sig type t val id: t -> string end) = struct
    module G = Graph.Imperative.Digraph.ConcreteBidirectional(struct
        type t = X.t
        let compare x y = String.compare (X.id x) (X.id y)
        let equal x y = X.id x = X.id y
        let hash x = Hashtbl.hash (X.id x)
      end)
    include G
    include Graph.Topological.Make(G)
    let vertex t =
      fold (fun v acc -> v :: acc) t []
      |> List.rev
  end

module rec Component: sig
  include S with type t = component and type component = component
  val cu: t -> cu option
  val lib: t -> lib option
  val pkg: t -> string option
  val pp: t -> lib option
  val pkg_pp: t -> string option
  val bin: t -> bin option
  val c: t -> c option
  val js: t -> js option
  val gen: t -> gen option
  val test: t -> test option
  val filter: (t -> 'a option) -> t list -> 'a list
  val closure: t list -> t list
  val comp_byte: t list -> Resolver.t -> (Resolver.t -> string) -> string list
  val comp_native: t list -> Resolver.t -> (Resolver.t -> string) -> string list
  val pp_byte: t list -> Resolver.t -> string list
  val pp_native: t list -> Resolver.t -> string list
  val link_byte: t list -> Resolver.t -> cu list -> string list
  val link_native: t list -> Resolver.t -> cu list -> string list
  module Graph: Graph with type V.t = component
  module Set: Set with type elt = component
end = struct

  type t = component

  type component = t

  let id = function
    | `CU cu  -> CU.id cu
    | `Lib l
    | `Pp l   -> Lib.id l
    | `Bin b  -> Bin.id b
    | `C c    -> C.id c
    | `Pkg_pp p
    | `Pkg p  -> Pkg.id p
    | `Test t -> Test.id t
    | `JS js  -> JS.id js
    | `Gen g  -> Gen.id g

  let name = function
    | `CU cu  -> CU.name cu
    | `Lib l
    | `Pp l   -> Lib.name l
    | `Bin b  -> Bin.name b
    | `C c    -> C.name c
    | `Pkg_pp p
    | `Pkg p  -> Pkg.name p
    | `Test t -> Test.name t
    | `JS js  -> JS.name js
    | `Gen g  -> Gen.name g

  let prereqs = function
    | `CU cu  -> CU.prereqs cu
    | `Lib l
    | `Pp l   -> Lib.prereqs l
    | `Bin b  -> Bin.prereqs b
    | `C c    -> C.prereqs c
    | `Pkg_pp p
    | `Pkg p  -> Pkg.prereqs p
    | `Test t -> Test.prereqs t
    | `JS js  -> JS.prereqs js
    | `Gen g  -> Gen.prereqs g

  let flags = function
    | `CU cu  -> CU.flags cu
    | `Lib l
    | `Pp l   -> Lib.flags l
    | `Bin b  -> Bin.flags b
    | `C c    -> C.flags c
    | `Pkg_pp p
    | `Pkg p  -> Pkg.flags p
    | `Test t -> Test.flags t
    | `JS js  -> JS.flags js
    | `Gen g  -> Gen.flags g

  let generated_files = function
    | `CU cu  -> CU.generated_files cu
    | `Lib l
    | `Pp l   -> Lib.generated_files l
    | `Bin b  -> Bin.generated_files b
    | `C c    -> C.generated_files c
    | `Pkg_pp p
    | `Pkg p  -> Pkg.generated_files p
    | `Test t -> Test.generated_files t
    | `JS js  -> JS.generated_files js
    | `Gen g  -> Gen.generated_files g

  let file = function
    | `CU cu  -> CU.file cu
    | `Lib l
    | `Pp l   -> Lib.file l
    | `Bin b  -> Bin.file b
    | `C c    -> C.file c
    | `Pkg_pp p
    | `Pkg p  -> Pkg.file p
    | `Test t -> Test.file t
    | `JS js  -> JS.file js
    | `Gen g  -> Gen.file g

  let build_dir = function
    | `CU cu  -> CU.build_dir cu
    | `Lib l
    | `Pp l   -> Lib.build_dir l
    | `Bin b  -> Bin.build_dir b
    | `C c    -> C.build_dir c
    | `Pkg_pp p
    | `Pkg p  -> Pkg.build_dir p
    | `Test t -> Test.build_dir t
    | `JS js  -> JS.build_dir js
    | `Gen g  -> Gen.build_dir g

  let deps = function
    | `CU cu  -> CU.deps cu
    | `Lib l
    | `Pp l   -> Lib.deps l
    | `Bin b  -> Bin.deps b
    | `C c    -> C.deps c
    | `Pkg_pp p
    | `Pkg p  -> Pkg.deps p
    | `Test t -> Test.deps t
    | `JS js  -> JS.deps js
    | `Gen g  -> Gen.deps g

  let cu = function `CU x -> Some x  | _ -> None

  let lib = function `Lib x -> Some x | _ -> None

  let pp = function `Pp x -> Some x | _ -> None

  let pkg = function `Pkg x -> Some x | _ -> None

  let pkg_pp = function `Pkg_pp x -> Some x | _ -> None

  let bin = function `Bin x -> Some x | _ -> None

  let test = function `Test x -> Some x | _ -> None

  let js = function `JS x -> Some x | _ -> None

  let c = function `C c -> Some c | _ -> None

  let gen = function `Gen x -> Some x | _ -> None

  let filter fn l =
    List.fold_left (fun acc x ->
        match fn x with
        | None   -> acc
        | Some x -> x :: acc
      ) [] l

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
              (deps h)in
          aux acc (d' @ d)
        )
    in
    aux [] ts

  let comp_flags mode (deps:t list) resolver build_dir =
    let incl = build_dir resolver in
    let cus = filter cu deps |> List.map (fun u -> CU.build_dir u resolver) in
    let libs = filter lib deps |> List.map (fun l -> Lib.build_dir l resolver)in
    let includes = 
      let module Sset = Set.Make (String) in
      let iflags inc acc = sprintf "-I %s" inc :: acc in 
      let add acc i = Sset.add i acc in
      let incs = List.fold_left add Sset.empty (incl :: cus @ libs) in 
      Sset.fold iflags incs [] 
    in
    let pkgs = match filter pkg deps with
    | [] -> []
    | pkgs ->
        let pkgs = Resolver.pkgs resolver pkgs in
        match mode with
        | `Byte   -> Flags.comp_byte pkgs
        | `Native -> Flags.comp_native pkgs 
    in
    pkgs @ [String.concat " " includes]
           
  let comp_byte = comp_flags `Byte

  let comp_native = comp_flags `Native

  let link_flags mode (deps:t list) resolver comps =
    let comps = List.map (fun u ->
        let file = match mode with
          | `Byte   -> CU.cmo u resolver
          | `Native -> CU.cmx u resolver in
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
        let pkgs = Resolver.pkgs resolver pkgs in
        match mode with
        | `Byte   -> Flags.link_byte pkgs
        | `Native -> Flags.link_native pkgs in
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
          sprintf "-I %s %s" (Lib.build_dir l resolver)
            (match mode with
             | `Byte   -> Lib.cma  l resolver
             | `Native -> Lib.cmxa l resolver)
        ) libs in
      let pkgs =
        let pkgs = Resolver.pkgs resolver pkgs in
        match mode with
        | `Byte   -> Flags.pp_byte pkgs
        | `Native -> Flags.pp_native pkgs in
      pkgs @ libs

  let pp_byte = pp_flags `Byte

  let pp_native = pp_flags `Native

  module Graph = Graph(struct
      type t = component
      let id = id
    end)

  module Set = struct

    include Set.Make(struct
        type t = component
        let compare x y = String.compare (id x) (id y)
      end)

    let of_list l =
      List.fold_left (fun set elt ->
          add elt set
        ) empty l

  end

end

and Pkg: sig
  include S with type t = string and type component = Component.t
end = struct

  type t = string

  type tmp = component
  type component = tmp

  let id t = "pkg-" ^ t

  let name t = t

  let prereqs _t _r _mode = []

  let flags t r =
    Resolver.pkgs r [t]

  let build_dir _t _r =
    failwith "Pkg.build_dir"

  let deps _t = []

  let generated_files _t _r = []

  let file _t _r _ext =
    failwith "Pkg.file"

end

and CU: sig
  include S with type t = cu and type component = Component.t
  val create:
    ?flags:Flags.t ->
    ?dir:string ->
    ?deps:Component.t list -> string -> t
  val copy: t -> t
  val dir: t -> string option
  val container: t -> [`Lib of Lib.t |`Bin of Bin.t]  option
  val mli: t -> bool
  val ml: t -> bool
  val for_pack: t -> string option
  val generated: t -> bool
  val pack: ?flags:Flags.t -> t list -> string -> t
  val unpack: t -> t list
  val cmi: t -> Resolver.t -> string
  val cmo: t -> Resolver.t -> string
  val cmx: t -> Resolver.t -> string
  val o: t -> Resolver.t -> string
  (* not expored *)
  val set_lib_container: t -> Lib.t -> unit
  val set_bin_container: t -> Bin.t -> unit
  val add_deps: t -> Component.t list -> unit
end = struct

  type t = cu

  type tmp = component
  type component = tmp

  let id t =
    match t.cu_container with
    | None          -> t.cu_name
    | Some (`Lib l) -> Lib.id l ^ "-" ^ t.cu_name
    | Some (`Bin b) -> Bin.id b ^ "-" ^ t.cu_name

  let ml t = t.cu_ml

  let mli t = t.cu_mli

  let generated t =
    t.cu_generated

  let rec copy t =
    { cu_dir       = t.cu_dir;
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
    | None          -> Resolver.build_dir resolver ""
    | Some (`Lib l) -> Lib.build_dir l resolver
    | Some (`Bin b) -> Bin.build_dir b resolver

  let name t = t.cu_name

  let deps t = t.cu_deps

  let container t = t.cu_container

  let for_pack t = t.cu_for_pack

  let unpack t = t.cu_pack

  let set_lib_container t lib =
    t.cu_container <- Some (`Lib lib)

  let set_bin_container t bin =
    t.cu_container <- Some (`Bin bin)

  let add_deps t deps =
    t.cu_deps <- t.cu_deps @ deps

  let create
      ?(flags=Flags.empty)
      ?dir ?(deps=[])
      name =
    let gens = Component.(filter gen) deps in
    let mli = match gens with
      | [] -> Sys.file_exists (dir // name ^ ".mli")
      | _  -> List.exists (fun g -> List.mem `MLI g.g_files) gens in
    let ml = match gens with
      | [] -> Sys.file_exists (dir // name ^ ".ml")
      | _  -> List.exists (fun g -> List.mem `ML g.g_files) gens in
    if not ml && not mli then (
      eprintf
        "\027[31m[ERROR]\027[m Cannot find %s.ml or %s.mli, stopping.\n"
        name name;
      exit 1;
    );
    let t = {
      cu_name      = name;
      cu_dir       = dir ;
      cu_deps      = deps;
      cu_flags     = flags;
      cu_container = None;
      cu_for_pack  = None;
      cu_pack      = [];
      cu_generated = gens <> [];
      cu_ml        = ml;
      cu_mli       = mli;
    } in
    List.iter (fun g ->
        let g = match g.g_comp with
          | None   -> g
          | Some _ -> { g with g_comp = None } in
        g.g_comp <- Some t
      ) gens;
    t

  let pack ?(flags=Flags.empty) comps name =
    List.iter (fun u -> u.cu_for_pack <- Some (String.capitalize name)) comps;
    {
      cu_name      = name;
      cu_flags     = flags;
      cu_dir       = None;
      cu_container = None;
      cu_for_pack  = None;
      cu_deps      = [];
      cu_pack      = comps;
      cu_ml        = false;
      cu_mli       = false;
      cu_generated = false;
    }

  let file t r ext =
    build_dir t r / t.cu_name ^ ext

  let cmi t r =
    file t r ".cmi"

  let cmo t r =
    file t r ".cmo"

  let cmx t r =
    file t r ".cmx"

  let o t r =
    file t r ".o"

  let cmt t r =
    file t r ".cmt"

  let cmti t r =
    file t r ".cmti"

  let generated_files t resolver =
    let mk f = f t resolver in
    [
      Features.true_ , [mk cmi ; mk cmo ];
      Features.native, [mk o   ; mk cmx ];
      Features.annot , [mk cmt ; mk cmti];
    ]

  let prereqs t resolver mode =
    let deps = deps t in
    let comps = Component.(filter cu) deps in
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
    let t' = Flags.create ~comp_byte ~comp_native ~pp_byte ~pp_native () in
    Flags.(t' @@@ t.cu_flags)

  module Graph = Graph(struct type t = cu let id = id end)

end

and Lib: sig
  include S with type t = lib and type component = Component.t
  val create:
    ?available:Features.t ->
    ?flags:Flags.t ->
    ?pack:bool ->
    ?deps:(string -> Component.t list) ->
    ?c:C.t list ->
    CU.t list -> string -> t
  val filename: t -> string
  val compilation_units: t -> CU.t list
  val c_objects: t -> C.t list
  val available: t -> Features.t
  val cma: t -> Resolver.t -> string
  val cmxa: t -> Resolver.t -> string
  val a: t -> Resolver.t -> string
  val cmxs: t -> Resolver.t -> string
  (* not exported *)
  val set_filename: t -> string -> unit
end = struct

  type t = lib

  type tmp = component
  type component = tmp

  let id t = "lib-" ^ t.l_name

  let deps t =
    let cus = t.l_cus |> conmap CU.deps in
    let cs  = t.l_cs  |> conmap C.deps in
    cus @ cs
    |> Component.Set.of_list
    |> Component.Set.elements

  let name t = t.l_name

  let build_dir t resolver =
    Resolver.build_dir resolver (id t)

  let compilation_units t = t.l_cus

  let c_objects t = t.l_cs

  let filename t = t.l_filename

  let set_filename t f =
    t.l_filename <- f

  let available t = t.l_available

  let nil _ = []

  let create
      ?(available=Features.true_)
      ?(flags=Flags.empty)
      ?(pack=false) ?(deps=nil) ?(c=[]) cus name =
    let cus' = if pack then [CU.pack cus name] else cus in
    let t = {
      l_name      = name;
      l_cus       = cus';
      l_cs        = c;
      l_available = available;
      l_flags     = flags;
      l_filename  = name;
    } in
    List.iter (fun u -> CU.add_deps u (deps (CU.name u))) cus;
    List.iter (fun u -> CU.set_lib_container u t) (cus' @ cus);
    List.iter (fun c -> C.set_lib_container c t) c;
    t

  let file t r ext =
    build_dir t r / t.l_name ^ ext

  let cma t r =
    file t r ".cma"

  let cmxa t r =
    file t r ".cmxa"

  let cmxs t r =
    file t r ".cmxs"

  let a t r =
    file t r ".a"

  let generated_files t resolver =
    let mk f = f t resolver in
    [
      t.l_available                              , [mk cma]       ;
      Features.(native         &&& t.l_available), [mk cmxa; mk a];
      Features.(native_dynlink &&& t.l_available), [mk cmxs]      ;
    ]
    @ conmap (fun u -> CU.generated_files u resolver) t.l_cus

  let flags t resolver =
    let comps = compilation_units t in
    let link_byte = Component.link_byte [] resolver comps in
    let link_native = Component.link_native [] resolver comps in
    let t' = Flags.create ~link_byte ~link_native () in
    Flags.(t' @@@ t.l_flags)

  let rec prereqs t resolver mode =
    let c_deps = match mode with
      | `Byte
      | `Native -> List.map (fun c -> C.o c resolver) (c_objects t)
      | `Shared -> List.map (fun c -> C.dll_so c resolver) (c_objects t) in
    let ml_deps = match mode with
      | `Byte   -> List.map (fun u -> CU.cmo u resolver) (compilation_units t)
      | `Native -> List.map (fun u -> CU.cmx u resolver) (compilation_units t)
      | `Shared -> prereqs t resolver `Native in
    c_deps @ ml_deps

end

and Bin: sig
  include S with type t = bin and type component = Component.t
  val create:
    ?available:Features.t ->
    ?byte_only:bool ->
    ?link_all:bool ->
    ?install:bool ->
    ?flags:Flags.t ->
    ?deps:(string -> Component.t list) ->
    CU.t list -> string -> t
  val toplevel:
    ?available:Features.t ->
    ?flags:Flags.t ->
    ?custom:bool ->
    ?install:bool ->
    ?deps:(string -> Component.t list) ->
    CU.t list -> string -> t
  val compilation_units: t -> CU.t list
  val available: t -> Features.t
  val is_toplevel: t -> bool
  val install: t -> bool
  val byte: t -> Resolver.t -> string
  val native: t -> Resolver.t -> string
end = struct

  type t = bin

  type tmp = component
  type component = tmp

  let id t = "bin-" ^ t.b_name

  let compilation_units t = t.b_comps

  let build_dir t resolver =
    Resolver.build_dir resolver (id t)

  let is_toplevel t = t.b_toplevel

  let install t = t.b_install

  let name t = t.b_name

  let deps t = t.b_deps

  let available t = t.b_available

  let nil _ = []

  let create
      ?(available=Features.true_)
      ?(byte_only=false)
      ?(link_all=false)
      ?(install=true)
      ?(flags=Flags.empty)
      ?(deps=nil)
      comps name =
    let available =
      if byte_only then Features.(not_ native &&& available) else available in
    let flags =
      if link_all then Flags.(linkall @@@ flags) else flags in
    List.iter (fun cu ->
        CU.add_deps cu (deps (CU.name cu))
      ) comps;
    let deps =
      List.fold_left (fun deps cu ->
          Component.Set.(union deps (of_list (CU.deps cu)))
        ) Component.Set.empty comps
      |> Component.Set.elements in
    let t = {
      b_deps      = deps;
      b_flags     = flags;
      b_available = available;
      b_name      = name;
      b_comps     = comps;
      b_toplevel  = false;
      b_install   = install
    } in
    List.iter (fun u -> CU.set_bin_container u t) comps;
    t

  let toplevel
      ?(available=Features.true_)
      ?(flags=Flags.empty)
      ?(custom=false)
      ?install
      ?(deps=nil)
      comps name =
    let available = Features.(not_ native &&& available) in
    let deps x =
      `Pkg "compiler-libs.toplevel" :: deps x in
    let link_byte = [
      (if custom then "-custom " else "") ^ "-I +compiler-libs topstart.cmo"
    ] in
    let nflags = Flags.create ~link_byte () in
    let flags = Flags.(nflags @@@ flags) in
    let t = create ~available ~flags ~link_all:true ~deps ?install comps name in
    { t with b_toplevel = true }

  let file t r ext =
    build_dir t r / t.b_name ^ ext

  let byte t r =
    file t r ".byte"

  let native t r =
    file t r ".opt"

  let generated_files t resolver =
    let mk f = f t resolver in
    [
      t.b_available                      , [mk byte  ];
      Features.(native &&& t.b_available), [mk native];
    ]

  (* XXX: handle native pps *)
  let prereqs t resolver mode =
    let comps = compilation_units t in
    let deps = deps t in
    let libs = Component.(filter lib) deps in
    let pps = Component.(filter pp) deps in
    let bytpps = List.map (fun l -> Lib.cma l resolver) pps in
    match mode with
    | `Byte   ->
      let bytlibs  = List.map (fun l -> Lib.cma  l resolver) libs in
      let bytcomps = List.map (fun u -> CU.cmo u resolver) comps in
      bytpps @ bytlibs @ bytcomps
    | `Shared
    | `Native ->
      let natlibs  = List.map (fun l -> Lib.cmxa l resolver) libs in
      let natcomps = List.map (fun u -> CU.cmx u resolver) comps in
      bytpps @ natlibs @ natcomps

  let flags t resolver =
    let comps = compilation_units t in
    let all_deps =
      (deps t @ List.map (fun x -> `CU x) comps) |> Component.closure in
    let comp_byte = Component.comp_byte (deps t) resolver (build_dir t) in
    let comp_native = Component.comp_native (deps t) resolver (build_dir t) in
    let link_byte = Component.link_byte all_deps resolver comps in
    let link_native = Component.link_native all_deps resolver comps in
    let t' = Flags.create ~link_byte ~link_native ~comp_byte ~comp_native () in
    Flags.(t.b_flags @@@ t')

end

and Gen: sig
  include S with type t = gen and type component = Component.t
  val create: ?deps:Component.t list -> ?action:(Resolver.t -> Action.t) ->
    [`C|`ML|`MLI] list -> string -> t
  val copy: t -> t
  val files: t -> Resolver.t -> string list
  val actions: t -> Resolver.t -> string list
end = struct

  type t = gen

  type tmp = component
  type component = tmp

  let id t = "gen-" ^ t.g_name

  let name t = t.g_name

  let deps t = t.g_deps

  let create ?(deps=[]) ?action g_files g_name =
    let g_action = match action with
      | None   -> (fun _ -> Action.none)
      | Some a -> a in
    { g_name; g_comp = None; g_files; g_action; g_deps = deps }

  let copy t =
     { t with g_comp = None }

  let build_dir t r =
    match t.g_comp with
    | None   -> Resolver.build_dir r ""
    | Some u -> CU.build_dir u r

  let prereqs t r mode =
    let bins = Component.(filter bin t.g_deps) in
    List.map (fun b -> match mode with
        | `Byte   -> Bin.byte b r
        | `Shared
        | `Native -> Bin.native b r
      ) bins

  let flags _t _r = Flags.empty

  let file t r ext =
    build_dir t r / t.g_name ^ ext

  let ml t r =
    file t r ".ml"

  let mli t r =
    file t r ".mli"

  let files t r =
    List.map (function
        | `C    -> file t r ".c"
        | `ML   -> ml t r
        | `MLI  -> mli t r
      ) t.g_files

  let generated_files t r = [
    Features.true_, files t r
  ]

  let actions t r =
    Action.actions (t.g_action r)

end

and C: sig
  include S with type t = c and type component = Component.t
  val create:
    ?dir:string -> ?generated:bool -> ?link_flags:string list ->
    ?deps:Component.t list -> string -> t
  val container: t -> [`Lib of Lib.t |`Bin of Bin.t]  option
  val set_lib_container: t -> Lib.t -> unit
  val set_bin_container: t -> Bin.t -> unit
  val link_flags: t -> string list
  val dll_so: t -> Resolver.t -> string
  val symlink_c: t -> Resolver.t -> string
  val o: t -> Resolver.t -> string
end = struct

  type t = c

  type tmp = component
  type component = tmp

  let name t = t.c_name

  let id t = "c-" ^ t.c_name

  let create ?dir ?(generated=false) ?(link_flags=[]) ?(deps=[]) name =
    { c_dir        = dir;
      c_name       = name;
      c_link_flags = link_flags;
      c_deps       = deps;
      c_generated  = generated;
      c_container  = None }

  let build_dir t r =
    match t.c_container with
    | Some (`Lib l) -> Lib.build_dir l r
    | Some (`Bin b) -> Bin.build_dir b r
    | None          -> Resolver.build_dir r (id t)

  let container t = t.c_container

  let set_lib_container t lib =
    t.c_container <- Some (`Lib lib)

  let set_bin_container t bin =
    t.c_container <- Some (`Bin bin)

  let file t r ext =
    build_dir t r / t.c_name ^ ext

  let dll_so t r =
    build_dir t r / "dll" ^ t.c_name ^ ".so"

  let o t r =
    file t r ".o"

  let symlink_c t r =
    file t r ".c"

  let prereqs _t _r _mode =
    failwith "C.prereqs"

  let deps t = t.c_deps

  let generated_files t r =
    [Features.true_, [dll_so t r]]

  let flags _t _r = Flags.empty

  let link_flags t = t.c_link_flags

end

and Test: sig
  include S with type t = test and type component = Component.t
  type args = test_args
  type command = test_command
  val create: ?dir:string -> ?deps:Component.t list -> command list -> string -> t
  val dir: t -> string option
  val commands: t -> command list
end = struct

  type args = test_args

  type command = test_command

  type t = test

  type tmp = component
  type component = tmp

  let id t =
    "test-" ^ t.t_name

  let name t = t.t_name

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

  let flags _t _r = Flags.empty

  let generated_files _t _r = []

  let file _t _r _ext =
    failwith "Test.file"

  let build_dir _t _r =
    failwith "Test.build_dir"

  let deps t = t.t_deps

  let create ?dir ?(deps=[]) commands name =
    let bin_deps =
      List.fold_left (fun acc -> function
          | `Shell _           -> acc
          | `Bin (`Bin bin, _) -> `Bin bin :: acc
        ) [] commands in
    {
      t_name     = name;
      t_dir      = dir;
      t_commands = commands;
      t_deps     = bin_deps @ deps;
    }

  let commands t = t.t_commands

  let dir t = t.t_dir

end

and JS: sig
  include S with type t = js and type component = Component.t
  val create: Bin.t -> string list -> t
  val js: t -> Resolver.t -> string
end = struct

  type t = js

  type tmp = component
  type component = tmp

  let bin t =
    t.j_bin

  let name t =
    Bin.name (bin t)

  let id t =
    Bin.id t.j_bin ^ "-js"

  let create bin args = {
    j_bin  = bin;
    j_args = args;
  }

  let file t r e =
    Bin.file (bin t) r e

  let js t r =
    Bin.file (bin t) r ".js"

  let flags t _r =
    Flags.create ~link_byte:t.j_args ()

  let build_dir t r =
    Bin.build_dir (bin t) r

  let prereqs t r _ =
    [Bin.byte (bin t) r]

  let generated_files t r =
    [Features.js, [js t r]]

  let deps t =
    [`Bin (bin t)]

end

let name t = t.name

let version t = t.version

let components t = t.components

let doc_css t = t.doc_css

let doc_dir t = t.doc_dir

let doc_intro t = t.doc_intro

let doc_public t = t.doc_public

let files_of_generators t resolver =
  let comps = Component.(filter cu t.components) in
  List.fold_left (fun acc u ->
      if CU.generated u then acc
      else
        let ml = match CU.ml u with
          | true  -> [CU.build_dir u resolver / CU.name u ^ ".ml"]
          | false -> [] in
        let mli = match CU.mli u with
          | true  -> [CU.build_dir u resolver / CU.name u ^ ".mli"]
          | false -> [] in
        ml @ mli @ acc
    ) [] comps

let create
    ?(flags=Flags.empty)
    ?doc_css ?doc_intro  ?(doc_dir="doc") ?doc_public
    ?version
    components name =
  let version = match version with
    | Some v -> v
    | None   ->
      match Git.describe () with
      | Some v -> v
      | None   ->
        match Git.head () with
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
        | `Lib l -> List.map CU.name (Lib.compilation_units l)
        | `CU cu -> [CU.name cu]
        | _      -> []
      ) components in
  { name; version; flags; components;
    doc_css; doc_intro; doc_dir; doc_public }

let unionmap fn t =
  List.fold_left (fun set t ->
      Features.(set ++ (fn t))
    ) Features.Set.empty t

let features t =
  let libs =
    let libs = Component.(filter lib t.components) in
    unionmap (fun x -> Features.atoms (Lib.available x)) libs in
  let pps  =
    let pps = Component.(filter pp t.components) in
    unionmap (fun x -> Features.atoms (Lib.available x)) pps in
  let bins =
    let bins = Component.(filter bin t.components) in
    unionmap (fun x -> Features.atoms (Bin.available x)) bins in
  Features.(base ++ libs ++ pps ++ bins)
