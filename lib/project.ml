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

type dep =
  [ `Comp of comp
  | `Lib of lib
  | `Pp of lib
  | `Bin of bin
  | `Pkg_pp of string
  | `Pkg of string ]

and comp = {
  u_name             : string;
  u_dir              : string option;
  mutable u_deps     : dep list;
  mutable u_container: [`Lib of lib | `Bin of bin] option;
  mutable u_for_pack : string option;
  mutable u_pack     : comp list;
  mutable u_flags    : Flags.t;
  u_action           : (Action.t * [`Both|`ML|`MLI]) option;
  u_mli              : bool;
  u_ml               : bool;
}

and lib = {
  l_name            : string;
  l_comps           : comp list;
  mutable l_filename: string;
  l_available       : Feature.formula;
  mutable l_flags   : Flags.t;
  l_deps            : dep list;
}

and bin = {
  b_deps     : dep list;
  b_comps    : comp list;
  b_name     : string;
  b_available: Feature.formula;
  b_flags    : Flags.t;
  b_toplevel : bool;
  b_install  : bool;
}

type test = {
  t_name: string;
  t_bin: bin;
  t_dir: string option;
  t_args: string list;
}

type js = {
  j_bin: bin;
  j_args: string list;
}

type t = {
  name: string;
  version: string;
  flags: Flags.t;
  libs: lib list;
  pps: lib list;
  bins: bin list;
  tests: test list;
  jss: js list;
  doc_css: string option;
  doc_intro: string option;
  doc_dir: string;
}

module type G = sig
  include Graph.Sig.I
  val iter: (V.t -> unit) -> t -> unit
  val fold: (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  val vertex: t -> V.t list
end

module type S = sig
  type t
  val id: t -> string
  val name: t -> string
  val deps: t -> dep list
  val file: t -> Resolver.t -> string -> string
  val generated_files: t -> Resolver.t -> (Feature.formula * string list) list
  val flags: t -> Resolver.t -> Flags.t
  val prereqs: t -> Resolver.t -> [`Byte | `Native] -> string list
  val build_dir: t -> Resolver.t -> string
  module Graph: G with type V.t = t
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

let (/) x y = Filename.concat x y

let conmap f l = List.concat (List.map f l)

let (//) x y =
  match x with
  | None   -> y
  | Some x -> Filename.concat x y

let rec comp_id t =
  match t.u_container with
  | None          -> t.u_name
  | Some (`Lib l) -> lib_id l ^ "-" ^ t.u_name
  | Some (`Bin b) -> bin_id b ^ "-" ^ t.u_name

and lib_id t = "lib-" ^ t.l_name

and bin_id t = "bin-" ^ t.b_name

let comp_deps t = t.u_deps

let lib_deps t =
  t.l_comps
  |> conmap comp_deps

let bin_deps t =
  t.b_deps

let rec comp_build_dir t resolver =
  match t.u_container with
  | None          -> Resolver.build_dir resolver ""
  | Some (`Lib l) -> lib_build_dir l resolver
  | Some (`Bin b) -> bin_build_dir b resolver

and lib_build_dir t resolver =
  Resolver.build_dir resolver (lib_id t)

and bin_build_dir t resolver =
  Resolver.build_dir resolver (bin_id t)

module Comp_file = struct

  let file t r ext =
    comp_build_dir t r / t.u_name ^ ext

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

end

module Lib_file = struct

  let file t r ext =
    lib_build_dir t r / t.l_name ^ ext

  let cma t r =
    file t r ".cma"

  let cmxa t r =
    file t r ".cmxa"

  let cmxs t r =
    file t r ".cmxs"

  let a t r =
    file t r ".a"

end

module Dep = struct

  type t = dep

  let id = function
    | `Comp u -> comp_id u
    | `Lib l
    | `Pp l   -> lib_id l
    | `Bin b  -> bin_id b
    | `Pkg_pp p
    | `Pkg p  -> "ext-" ^ p

  module Graph = Graph(struct type t = dep let id = id end)

  let bin t: t = `Bin t
  let bins = List.map bin

  let comp t: t = `Comp t
  let comps = List.map comp

  let lib t: t = `Lib t
  let libs = List.map lib

  let pp t: t = `Pp t
  let pps = List.map pp

  let pkg_pp t: t = `Pkg_pp t
  let pkg_pps = List.map pkg_pp

  let pkg t: t = `Pkg t
  let pkgs = List.map pkg

  let filter_comps t =
    List.fold_left (fun acc -> function `Comp t -> t :: acc | _ -> acc) [] t
    |> List.rev

  let filter_libs t =
    List.fold_left (fun acc -> function `Lib t -> t :: acc | _ -> acc) [] t
    |> List.rev

  let filter_pps t =
    List.fold_left (fun acc -> function `Pp t -> t :: acc | _ -> acc) [] t
    |> List.rev

  let filter_pkgs t =
    List.fold_left (fun acc -> function `Pkg t -> t :: acc | _ -> acc) [] t
    |> List.rev

  let filter_pkg_pps t =
    List.fold_left (fun acc -> function `Pkg_pp t -> t :: acc | _ -> acc) [] t
    |> List.rev

  let closure ts =
    let deps = Hashtbl.create 24 in
    let rec aux acc = function
      | []          -> List.rev acc
      | h :: t as d ->
        if Hashtbl.mem deps h then
          match Hashtbl.find deps h with
          | 0 -> Hashtbl.replace deps h 1; aux (h :: acc) t
          | _ -> aux acc t
        else (
          Hashtbl.add deps h 0;
          match h with
          | `Comp u ->
            let d' = comp_deps u in
            aux acc (d' @ d)
          | `Lib l ->
            let d' = lib_deps l in
            aux acc (d' @ d)
          | _ -> Hashtbl.replace deps h 1; aux (h :: acc) t
        )
    in
    aux [] ts

  let comp_flags mode (deps:t list) incl resolver =
    let incl = sprintf "-I %s" incl in
    let libs = filter_libs deps in
    let libs = List.map (fun l ->
        sprintf "-I %s" (lib_build_dir l resolver);
      ) libs in
    let libs = String.concat " " (incl :: libs) in
    let pkgs = filter_pkgs deps in
    let pkgs = match pkgs with
      | [] -> []
      | _  ->
        let pkgs = Resolver.pkgs resolver pkgs in
        match mode with
        | `Byte   -> Flags.comp_byte pkgs
        | `Native -> Flags.comp_native pkgs in
    pkgs @ [libs]

  let comp_byte = comp_flags `Byte

  let comp_native = comp_flags `Native

  let link_flags mode (deps:t list) comps resolver =
    let comps = List.map (fun u ->
        let file = match mode with
          | `Byte   -> Comp_file.cmo u resolver
          | `Native -> Comp_file.cmx u resolver in
        sprintf "%s/%s" (Filename.dirname file) (Filename.basename file)
      ) comps in
    let libs = filter_libs deps in
    let libs = List.map (fun l ->
        let file = match mode with
          | `Byte   -> Lib_file.cma l resolver
          | `Native -> Lib_file.cmxa l resolver in
        sprintf "%s/%s" (Filename.dirname file) (Filename.basename file)
      ) libs in
    let pkgs = filter_pkgs deps in
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
    let libs = filter_pps deps in
    let pkgs = filter_pkg_pps deps in
    match libs, pkgs with
    | [], [] -> []
    | _ , _  ->
      let libs = List.map (fun l ->
          sprintf "-I %s %s" (lib_build_dir l resolver)
            (match mode with
             | `Byte   -> Lib_file.cma  l resolver
             | `Native -> Lib_file.cmxa l resolver)
        ) libs in
      let pkgs =
        let pkgs = Resolver.pkgs resolver pkgs in
        match mode with
        | `Byte   -> Flags.pp_byte pkgs
        | `Native -> Flags.pp_native pkgs in
      pkgs @ libs

  let pp_byte = pp_flags `Byte

  let pp_native = pp_flags `Native

end

module Comp = struct

  type t = comp

  let id = comp_id

  module Graph = Graph(struct type t = comp let id = id end)

  let ml t = t.u_ml

  let mli t = t.u_mli

  let action t =
    t.u_action

  let rec copy t =
    { u_dir       = t.u_dir;
      u_deps      = t.u_deps;
      u_name      = t.u_name;
      u_container = t.u_container;
      u_for_pack  = t.u_for_pack;
      u_pack      = List.map copy t.u_pack;
      u_flags     = t.u_flags;
      u_ml        = t.u_ml;
      u_mli       = t.u_mli;
      u_action    = t.u_action;
    }

  let dir t = t.u_dir

  let build_dir t = comp_build_dir t

  let name t = t.u_name

  let deps = comp_deps

  let container t = t.u_container

  let for_pack t = t.u_for_pack

  let unpack t = t.u_pack

  let set_lib t lib =
    t.u_container <- Some (`Lib lib)

  let set_bin t bin =
    t.u_container <- Some (`Bin bin)

  let add_deps t deps =
    t.u_deps <- t.u_deps @ deps

  let create
      ?(flags=Flags.empty)
      ?action
      ?dir ?(deps=[])
      name =
    let mli = match action with
      | None      -> Sys.file_exists (dir // name ^ ".mli")
      | Some (_, `Both)
      | Some (_, `MLI) -> true
      | Some (_, `ML)  -> false in
    let ml = match action with
      | None      -> Sys.file_exists (dir // name ^ ".ml")
      | Some (_, `Both)
      | Some (_, `ML)  -> true
      | Some (_, `MLI) -> false in
    if not ml && not mli then (
      eprintf
        "\027[31m[ERROR]\027[m Cannot find %s.ml or %s.mli, stopping.\n"
        name name;
      exit 1;
    );
    {
      u_name = name; u_dir = dir ; u_deps = deps; u_flags = flags;
      u_container = None;
      u_for_pack  = None;
      u_pack      = [];
      u_ml = ml; u_mli = mli; u_action = action;
    }

  let pack ?(flags=Flags.empty) comps name =
    List.iter (fun u -> u.u_for_pack <- Some (String.capitalize name)) comps;
    {
      u_name = name; u_flags = flags;
      u_dir       = None;
      u_container = None;
      u_for_pack  = None;
      u_deps      = [];
      u_pack      = comps;
      u_ml        = false;
      u_mli       = false;
      u_action    = None;
    }

  include Comp_file

  let generated_files t resolver =
    let mk f = f t resolver in
    [
      Feature.true_ , [mk cmi ; mk cmo ];
      Feature.native, [mk o   ; mk cmx ];
      Feature.annot , [mk cmt ; mk cmti];
    ]

  let prereqs t resolver mode =
    let deps = deps t in
    let comps = Dep.filter_comps deps in
    let comps = List.map (fun u ->
        match mode with
        | `Native -> cmx u resolver
        | `Byte   -> cmi u resolver
      ) comps in
    let libs = Dep.filter_libs deps in
    let libs = List.map (fun l ->
        match mode with
        | `Native -> Lib_file.cmxa l resolver
        | `Byte   -> Lib_file.cma  l resolver
      ) libs in
    let pps = Dep.filter_pps deps in
    let pps = List.map (fun l -> Lib_file.cma l resolver) pps in
    comps @ libs @ pps

  (* XXX: memoize the function *)
  let flags t resolver =
    let deps = deps t |> Dep.closure in
    let incl = build_dir t resolver in
    let comp_byte = Dep.comp_byte deps incl resolver in
    let comp_native = Dep.comp_native deps incl resolver in
    let pp_byte = Dep.pp_byte deps resolver in
    let pp_native = Dep.pp_native deps resolver in
    let t' = Flags.create ~comp_byte ~comp_native ~pp_byte ~pp_native () in
    Flags.(t' @ t.u_flags)

end

module Lib = struct

  type t = lib

  let id = lib_id

  module Graph = Graph(struct type t = lib let id = id end)

  let deps = lib_deps

  let name t = t.l_name

  let build_dir = lib_build_dir

  let comps t = t.l_comps

  let filename t = t.l_filename

  let set_filename t f =
    t.l_filename <- f

  let available t = t.l_available

  let create
      ?(available=Feature.true_)
      ?(flags=Flags.empty)
      ?(pack=false) ?(deps=[]) comps name =
    let comps' = if pack then [Comp.pack comps name] else comps in
    let t = {
      l_name      = name;
      l_comps     = comps';
      l_available = available;
      l_flags     = flags;
      l_filename  = name;
      l_deps      = deps } in
    List.iter (fun u -> Comp.add_deps u deps) comps;
    List.iter (fun u -> Comp.set_lib u t) (comps' @ comps);
    t

  include Lib_file

  let generated_files t resolver =
    let mk f = f t resolver in
    [
      t.l_available                            , [mk cma]       ;
      Feature.(native         && t.l_available), [mk cmxa; mk a];
      Feature.(native_dynlink && t.l_available), [mk cmxs]      ;
    ]
    @ conmap (fun u -> Comp.generated_files u resolver) t.l_comps

  let flags t resolver =
    let comps = comps t in
    let link_byte = Dep.link_byte [] comps resolver in
    let link_native = Dep.link_native [] comps resolver in
    let t' = Flags.create ~link_byte ~link_native () in
    Flags.(t' @ t.l_flags)

  let prereqs t resolver = function
    | `Byte   -> List.map (fun u -> Comp.cmo u resolver) (comps t)
    | `Native -> List.map (fun u -> Comp.cmx u resolver) (comps t)

end

module Bin = struct

  type t = bin

  let id = bin_id

  module Graph = Graph(struct type t = bin let id = id end)

  let comps t = t.b_comps

  let build_dir = bin_build_dir

  let is_toplevel t = t.b_toplevel

  let install t = t.b_install

  let name t = t.b_name

  let deps = bin_deps

  let available t = t.b_available

  let create
      ?(available=Feature.true_)
      ?(byte_only=false)
      ?(link_all=false)
      ?(install=true)
      ?(flags=Flags.empty)
      ?(deps=[])
      comps name =
    let available =
      if byte_only then Feature.(not native && available) else available in
    let flags =
      if link_all then Flags.(linkall @ flags) else flags in
    let t = {
      b_deps      = deps;
      b_flags     = flags;
      b_available = available;
      b_name      = name;
      b_comps     = comps;
      b_toplevel  = false;
      b_install   = install
    } in
    List.iter (fun u -> Comp.set_bin u t) comps;
    t

  let toplevel
      ?(available=Feature.true_)
      ?(flags=Flags.empty)
      ?(custom=false)
      ?install
      ?(deps=[])
      comps name =
    let available = Feature.(not native && available) in
    let deps = Dep.pkg "compiler-libs.toplevel" :: deps in
    let link_byte = [
      (if custom then "-custom " else "") ^ "-I +compiler-libs topstart.cmo"
    ] in
    let nflags = Flags.create ~link_byte () in
    let flags = Flags.(nflags @ flags) in
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
      t.b_available                    , [mk byte  ];
      Feature.(native && t.b_available), [mk native];
    ]

  (* XXX: handle native pps *)
  let prereqs t resolver mode =
    let comps = comps t in
    let deps = deps t in
    let libs = Dep.filter_libs deps in
    let pps = Dep.filter_pps deps in
    let bytpps = List.map (fun l -> Lib.cma l resolver) pps in
    match mode with
    | `Byte   ->
      let bytlibs  = List.map (fun l -> Lib.cma  l resolver) libs in
      let bytcomps = List.map (fun u -> Comp.cmo u resolver) comps in
      bytpps @ bytlibs @ bytcomps
    | `Native ->
      let natlibs  = List.map (fun l -> Lib.cmxa l resolver) libs in
      let natcomps = List.map (fun u -> Comp.cmx u resolver) comps in
      bytpps @ natlibs @ natcomps

  let flags t resolver =
    let comps = comps t in
    let all_deps  = (deps t @ Dep.comps comps) |> Dep.closure in
    let incl = build_dir t resolver in
    let comp_byte = Dep.comp_byte (deps t) incl resolver in
    let comp_native = Dep.comp_native (deps t) incl resolver in
    let link_byte = Dep.link_byte all_deps comps resolver in
    let link_native = Dep.link_native all_deps comps resolver in
    let t' = Flags.create ~link_byte ~link_native ~comp_byte ~comp_native () in
    Flags.(t.b_flags @ t')

end

module Test = struct

  type t = test

  let id t =
    "test-" ^ t.t_name

  let name t = t.t_name

  module Graph = Graph(struct type t = test let id = id end)

  let prereqs t r = function
    | `Byte   -> [Bin.byte t.t_bin r]
    | `Native -> [Bin.native t.t_bin r]

  let flags _t _r = Flags.empty

  let generated_files _t _r = []

  let file _t _r _ext =
    failwith "TODO"

  let build_dir _t _r =
    failwith "TODO"

  let deps t = [`Bin t.t_bin]

  let create ?dir bin args name = {
    t_name = name;
    t_bin  = bin;
    t_dir  = dir;
    t_args = args;
  }

  let bin t = t.t_bin

  let dir t = t.t_dir

  let args t = t.t_args

end

module JS = struct

  type t = js

  let bin t =
    t.j_bin

  let name t =
    Bin.name (bin t)

  let id t =
    Bin.id (bin t) ^ "-js"

  module Graph = Graph(struct type t = js let id = id end)

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
    [Feature.js, [js t r]]

  let deps t =
    [`Bin (bin t)]

end

let js = JS.create


let name t = t.name

let version t = t.version

let libs t = t.libs

let pps t = t.pps

let bins t = t.bins

let tests t = t.tests

let jss t = t.jss

let comps t =
  let deps =
    conmap Lib.deps t.libs
    @ conmap Lib.deps t.pps
    @ conmap Bin.deps t.bins in
  Dep.closure deps |> Dep.filter_comps

let doc_css t = t.doc_css

let doc_dir t = t.doc_dir

let doc_intro t = t.doc_intro

let projects = ref []

let list () = !projects

let generated_from_custom_actions t resolver =
  let libs =
    t.libs @ t.pps
    |> Dep.libs
    |> Dep.closure
    |> Dep.filter_libs
    |> conmap Lib.comps in
  let bins =
    conmap Bin.comps t.bins @
    (t.bins
     |> Dep.bins
     |> Dep.closure
     |> Dep.filter_libs
     |> conmap Lib.comps) in
  List.fold_left (fun acc u ->
      match Comp.action u with
      | None        -> acc
      | Some (_, k) ->
        let ml = match k with
          | `ML | `Both -> [Comp.build_dir u resolver / Comp.name u ^ ".ml"]
          | `MLI        -> [] in
        let mli = match k with
          | `MLI | `Both -> [Comp.build_dir u resolver / Comp.name u ^ ".mli"]
          | `ML          -> [] in
        ml @ mli @ acc
    ) [] (libs @ bins)

let create
    ?(flags=Flags.empty)
    ?(libs=[]) ?(pps=[]) ?(bins=[]) ?(tests=[]) ?(jss=[])
    ?doc_css ?doc_intro  ?(doc_dir="doc")
    ?version
    name =
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
  List.iter (fun l ->
      if Lib.name l <> name then
        Lib.set_filename l (name ^ "." ^ Lib.name l)
    ) libs;
  let t =
    { name; version; flags; libs; pps; bins; tests; jss;
      doc_css; doc_intro; doc_dir } in
  projects := t :: !projects

let (++) = Feature.Set.union

let unionmap fn t =
  List.fold_left (fun set t ->
      set ++ (fn t)
    ) Feature.Set.empty t

let features t =
  let libs = unionmap (fun x -> Feature.atoms @@ Lib.available x) t.libs in
  let pps  = unionmap (fun x -> Feature.atoms @@ Lib.available x) t.pps  in
  let bins = unionmap (fun x -> Feature.atoms @@ Bin.available x) t.bins in
  Feature.base ++ libs ++ pps ++ bins

let comp_tbl = Hashtbl.create 8
let main = "main"
let find name =
  try Hashtbl.find comp_tbl name
  with Not_found ->
    let g = Comp.Graph.create () in
    Hashtbl.add comp_tbl name g;
    g

let comp ?(bag=main) ?(dir="lib") deps name =
  let g = find bag in
  let u = Comp.create ~dir ~deps name in
  let () =
    Comp.Graph.add_vertex g u;
    Dep.filter_comps deps
    |> List.iter (fun u' -> Comp.Graph.add_edge g u' u) in
  `Comp u

let lib ?(bag=main) name =
  let g = find bag in
  Lib.create (Comp.Graph.vertex g) name

let bin ?byte_only ?link_all ?install ?(dir="bin") deps comps name =
  let comps = List.map (Comp.create ~dir ~deps) comps in
  Bin.create ?byte_only ?link_all ?install comps name

let pkg = Dep.pkg

let pkg_pp = Dep.pkg_pp
