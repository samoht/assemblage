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

module StringSet = struct
  include Set.Make (String)
  let of_list ss = List.fold_left (fun acc s -> add s acc) empty ss
end

type component =
  [ `Unit of comp_unit
  | `Other of other
  | `Pkg of pkg
  | `Lib of lib
  | `Bin of bin
  | `Dir of dir
  | `Test of test ]

and container =
  [ `Lib of lib
  | `Bin of bin
  | `Dir of dir ]

and comp_unit =
  { u_name : string;
    u_available : As_features.t;
    u_flags : As_flags.t;
    u_deps : component list;
    u_container: container option;
    u_origin : [`Dir of string | `Other of other];
    mutable u_for_pack : string option;
    mutable u_pack : comp_unit list;
    u_kind : [ `OCaml | `C | `Js ];
    u_has : As_action.kind -> bool;
  }

and other =
  { o_name : string;
    o_available : As_features.t;
    o_flags : As_flags.t;
    o_deps : component list;
    o_action : As_action.t;
    o_kinds : As_action.kind list; }

and pkg =
  { p_name : string;
    p_available : As_features.t;
    p_flags : As_flags.t;
    p_kind : [`OCaml | `OCaml_pp | `C ]; }

and lib =
  { l_name : string;
    l_available : As_features.t;
    l_flags : As_flags.t;
    l_deps : component list;
    l_kind : [ `OCaml | `OCaml_pp ];
    mutable l_units : comp_unit list;
    mutable l_filename : string; }

and bin =
  { b_name : string;
    b_available : As_features.t;
    b_flags : As_flags.t;
    b_deps : component list;
    mutable b_cus : comp_unit list;
    b_toplevel : bool;
    b_install : bool;
    b_js: bool; }

and dir =
  { d_name :
      [ `Lib | `Bin | `Sbin | `Toplevel | `Share | `Share_root | `Etc | `Doc
      | `Misc | `Stublibs | `Man | `Other of string ];
    d_available : As_features.t;
    d_flags : As_flags.t;
    d_deps : component list;
    d_install : bool;
    mutable d_contents : component list; }

and test_args = (component -> string) -> string list
and test_command =
  [ `Bin of [`Bin of bin] * test_args
  | `Shell of string ]

and test =
  { t_name : string;
    t_available : As_features.t;
    t_flags : As_flags.t;
    t_deps : component list;
    t_dir : string option;
    t_commands : test_command list; }

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
  val targets: t -> As_resolver.t -> As_flags.mode -> As_flags.phase -> string list
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
        let equal x y = (X.id x) = (X.id y)
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
  val unit_ocaml : t -> comp_unit option
  val unit_c : t -> comp_unit option
  val unit_js : t -> comp_unit option
  val other : t -> other option
  val pkg : t -> pkg option
  val pkg_ocaml : t -> pkg option
  val pkg_ocaml_pp : t -> pkg option
  val pkg_c : t -> pkg option
  val lib : t -> lib option
  val lib_ocaml : t -> lib option
  val lib_ocaml_pp : t -> lib option
  val bin : t -> bin option
  val dir : t -> dir option
  val test : t -> test option
  val filter : (t -> 'a option) -> t list -> 'a list
  val prereqs: t -> As_resolver.t -> As_flags.mode -> As_flags.phase -> string list
  val closure : ?link:bool -> t list -> t list
  val comp_byte : t list -> As_resolver.t -> (As_resolver.t -> string) ->
    string list
  val comp_native : t list -> As_resolver.t -> (As_resolver.t -> string) ->
    string list
  val pp_byte : t list -> As_resolver.t -> string list
  val pp_native : t list -> As_resolver.t -> string list
  val link_byte : t list -> As_resolver.t -> comp_unit list ->
    string list
  val link_native : t list -> As_resolver.t -> comp_unit list ->
    string list
 module Graph: Graph with type V.t = component
 module Set: Set with type elt = component
end = struct
  type t = component
  type component = t

  let id = function
  | `Unit u -> Unit.id u
  | `Other o -> Other.id o
  | `Pkg p -> Pkg.id p
  | `Lib l -> Lib.id l
  | `Bin b -> Bin.id b
  | `Dir d -> Dir.id d
  | `Test t -> Test.id t

  let name = function
  | `Unit u -> Unit.name u
  | `Other o -> Other.name o
  | `Pkg p -> Pkg.name p
  | `Lib l -> Lib.name l
  | `Bin b  -> Bin.name b
  | `Dir d -> Dir.name d
  | `Test t -> Test.name t

  let available = function
  | `Unit u -> Unit.available u
  | `Other o -> Other.available o
  | `Pkg p -> Pkg.available p
  | `Lib l -> Lib.available l
  | `Bin b -> Bin.available b
  | `Dir d -> Dir.available d
  | `Test t -> Test.available t

  let targets = function
  | `Unit u -> Unit.targets u
  | `Other o -> Other.targets o
  | `Pkg p -> Pkg.targets p
  | `Lib l -> Lib.targets l
  | `Bin b -> Bin.targets b
  | `Dir d -> Dir.targets d
  | `Test t -> Test.targets t

  let flags = function
  | `Unit u -> Unit.flags u
  | `Other o -> Other.flags o
  | `Pkg p -> Pkg.flags p
  | `Lib l -> Lib.flags l
  | `Bin b -> Bin.flags b
  | `Dir d -> Dir.flags d
  | `Test t -> Test.flags t

  let generated_files = function
  | `Unit u -> Unit.generated_files u
  | `Other o -> Other.generated_files o
  | `Pkg p -> Pkg.generated_files p
  | `Lib l -> Lib.generated_files l
  | `Bin b -> Bin.generated_files b
  | `Dir d -> Dir.generated_files d
  | `Test t -> Test.generated_files t

  let file = function
  | `Unit u -> Unit.file u
  | `Other o -> Other.file o
  | `Pkg p -> Pkg.file p
  | `Lib l -> Lib.file l
  | `Bin b -> Bin.file b
  | `Dir d -> Dir.file d
  | `Test t -> Test.file t

  let build_dir = function
  | `Unit u -> Unit.build_dir u
  | `Other o -> Other.build_dir o
  | `Pkg p -> Pkg.build_dir p
  | `Lib l -> Lib.build_dir l
  | `Bin b  -> Bin.build_dir b
  | `Dir d -> Dir.build_dir d
  | `Test t -> Test.build_dir t

  let deps = function
  | `Unit u -> Unit.deps u
  | `Other o -> Other.deps o
  | `Pkg p -> Pkg.deps p
  | `Lib l -> Lib.deps l
  | `Bin b -> Bin.deps b
  | `Dir d -> Dir.deps d
  | `Test t -> Test.deps t

  let unit = function `Unit x -> Some x | _ -> None
  let unit_ocaml = function `Unit x when x.u_kind = `OCaml -> Some x | _ -> None
  let unit_c = function `Unit x when x.u_kind = `C -> Some x | _ -> None
  let unit_js = function `Unit x when x.u_kind = `Js -> Some x | _ -> None
  let other = function `Other x -> Some x | _ -> None

  let pkg = function `Pkg x -> Some x | _ -> None
  let pkg_kind k = function `Pkg x when Pkg.kind x = k -> Some x | _ -> None
  let pkg_ocaml = pkg_kind `OCaml
  let pkg_ocaml_pp = pkg_kind `OCaml_pp
  let pkg_c = pkg_kind `C

  let lib = function `Lib x -> Some x | _ -> None
  let lib_kind k = function `Lib x when Lib.kind x = k -> Some x | _ -> None
  let lib_ocaml = lib_kind `OCaml
  let lib_ocaml_pp = lib_kind `OCaml_pp

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

  let prereqs t r mode phase =
    List.fold_left (fun acc d ->
        let d = StringSet.of_list (targets d r mode phase) in
        StringSet.union d acc
      ) StringSet.empty (deps t)
    |> StringSet.elements

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
          let linkable =
            match h with `Unit _ | `Lib _ | `Bin _ -> true | _ -> false in
          let d' =
            if not (linkable && link) then deps h else
            List.filter
              (function
              | `Unit _ -> true
              | `Pkg pkg when Pkg.kind pkg = `OCaml -> true
              | `Lib lib when Lib.kind lib = `OCaml -> true
              | _     -> false)
              (deps h) in
          aux acc (d' @ d)
        )
    in
    aux [] ts

  let comp_flags mode (deps:t list) resolver build_dir =
    let incl = build_dir resolver in
    let us = filter unit_ocaml deps |> List.map (fun u -> Unit.build_dir u resolver) in
    let libs = filter lib_ocaml deps |> List.map (fun l -> Lib.build_dir l resolver) in
    let includes =
      (* We need to keep the -I flags in the right order *)
      let iflags inc acc = sprintf "-I %s" inc :: acc in
      let add (seen, acc) i =
        if StringSet.mem i seen then (seen, acc)
        else (StringSet.add i seen, iflags i acc) in
      let (_, incs) =
        List.fold_left add (StringSet.empty, []) (incl :: us @ libs) in
      List.rev incs
    in
    let pkgs = match filter pkg_ocaml deps with
    | [] -> []
    | pkgs ->
        let pkgs = List.map Pkg.name pkgs in
        let pkgs = As_resolver.pkgs resolver pkgs in
        match mode with
        | `Byte   -> As_flags.get `Compile `Byte pkgs
        | `Native -> As_flags.get `Compile `Native pkgs
    in
    pkgs @ [String.concat " " includes]

  let comp_byte = comp_flags `Byte
  let comp_native = comp_flags `Native

  let link_flags mode (deps:t list) resolver comps =
    let comps = List.filter (Unit.has `Cmo) comps in
    let comps = List.map (fun u ->
        let file = match mode with
          | `Byte   -> Unit.cmo u resolver
          | `Native -> Unit.cmx u resolver in
        sprintf "%s/%s" (Filename.dirname file) (Filename.basename file)
      ) comps in
    let libs = filter lib_ocaml deps in
    let libs = List.map (fun l ->
        let file = match mode with
          | `Byte   -> Lib.cma l resolver
          | `Native -> Lib.cmxa l resolver in
        sprintf "%s/%s" (Filename.dirname file) (Filename.basename file)
      ) libs in
    let pkgs = filter pkg_ocaml deps in
    let pkgs = match pkgs with
      | [] -> []
      | _  ->
        let pkgs = List.map Pkg.name pkgs in
        let pkgs = As_resolver.pkgs resolver pkgs in
        match mode with
        | `Byte   -> As_flags.get `Link `Byte pkgs
        | `Native -> As_flags.get `Link `Native pkgs in
    pkgs @ libs @ comps

  let link_byte = link_flags `Byte
  let link_native = link_flags `Native

  let pp_flags mode (deps:t list) resolver =
    let libs = filter lib_ocaml_pp deps in
    let pkgs = filter pkg_ocaml_pp deps in
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
        | `Byte   -> As_flags.get `Pp `Byte pkgs
        | `Native -> As_flags.get `Pp `Native pkgs in
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

  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:Component.t list -> string -> [`OCaml | `C | `Js] ->
    [`Dir of string | `Other of other] -> t
  val with_container: container -> t -> t
  val with_containers: container -> t list -> t list
  val container: t -> container option
  val dir: t -> string option
  val kind: t -> [`OCaml | `C | `Js]
  val has: As_action.kind -> t -> bool
  val for_pack: t -> string option
  val generated: t -> bool
  val pack : ?available:As_features.t ->
    ?flags:As_flags.t -> string -> t list -> t
  val unpack: t -> t list
  val source: t -> string -> string
  val ml: t -> As_resolver.t -> string
  val mli: t -> As_resolver.t -> string
  val c: t -> As_resolver.t -> string
  val js: t -> As_resolver.t -> string
  val cmi: t -> As_resolver.t -> string
  val cmo: t -> As_resolver.t -> string
  val cmx: t -> As_resolver.t -> string
  val o: t -> As_resolver.t -> string
  val dll_so: t -> As_resolver.t -> string
  (* not exported *)
  val sort : [ `Unit of t ] list -> t list
end = struct
  type t = comp_unit

  let id t = match t.u_container with
  | None   -> "unit-" ^ t.u_name
  | Some c -> Component.id (c:>component) ^ "-" ^ t.u_name

  let kind t = t.u_kind
  let name t = t.u_name
  let container t = t.u_container
  let has kind t = t.u_has kind
  let dir t = match t.u_origin with
  | `Dir d   -> Some d
  | `Other _ -> None

  let build_dir t r = match t.u_container with
  | None   -> As_resolver.build_dir r (id t)
  | Some c -> Component.build_dir (c:>component) r

  let source t ext = match dir t with
  | None   -> t.u_name ^ ext
  | Some d -> d / t.u_name ^ ext

  let file t r ext = build_dir t r / t.u_name ^ ext
  let cmi t r = file t r ".cmi"
  let cmo t r = file t r ".cmo"
  let cmx t r = file t r ".cmx"
  let o t r = file t r ".o"
  let cmt t r = file t r ".cmt"
  let cmti t r = file t r ".cmti"
  let dll_so t r = build_dir t r / "dll" ^ t.u_name ^ ".so"
  let c t r = file t r ".c"
  let js t r = file t r ".js"
  let ml t r = file t r ".ml"
  let mli t r = file t r ".mli"

  let generated t =
    match t.u_origin with
    | `Dir _   -> false
    | `Other _ -> true

  let deps t =
    match t.u_origin with
    | `Dir _   ->  t.u_deps
    | `Other o -> `Other o :: t.u_deps

  let with_container c t = { t with u_container = Some c }
  let with_containers c ts =
    List.fold_left (fun acc u ->
        let u_deps =
          List.map (function
            | (`Unit u) as d ->
                (try `Unit (List.assoc (name u) acc) with Not_found -> d)
            | d -> d
            ) u.u_deps in
        (name u, { u with u_container = Some c; u_deps }) :: acc
      ) [] ts
    |> List.rev_map snd

  let available t = t.u_available
  let for_pack t = t.u_for_pack
  let unpack t = t.u_pack

  let create ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(deps = []) name (kind:[`OCaml|`C|`Js]) origin
    =
    let has: As_action.kind -> bool = function
    | `Mli -> (match kind with
      | `C | `Js -> false
      | `OCaml -> match origin with
      | `Dir dir -> Sys.file_exists (dir / name ^ ".mli")
      | `Other o -> List.mem `Mli o.o_kinds)
    | `Ml -> (match kind with
      | `C | `Js -> false
      | `OCaml -> match origin with
      | `Dir dir -> Sys.file_exists (dir / name ^ ".ml")
      | `Other o -> List.mem `Ml o.o_kinds)
    | `Cmi -> (match kind with
      | `C | `Js -> false
      | `OCaml -> match origin with
      | `Dir dir -> Sys.file_exists (dir / name ^ ".mli")
      | `Other o -> List.mem `Mli o.o_kinds || List.mem `Cmi o.o_kinds)
    | `Cmo -> (match kind with
      | `C | `Js -> false
      | `OCaml -> match origin with
      | `Dir dir -> Sys.file_exists (dir / name ^ ".ml")
      | `Other o -> List.mem `Ml o.o_kinds || List.mem `Cmo o.o_kinds)
    | `Cmx -> (match kind with
      | `C | `Js -> false
      | `OCaml -> match origin with
      | `Dir dir -> Sys.file_exists (dir / name ^ ".ml")
      | `Other o -> List.mem `Ml o.o_kinds || List.mem `Cmx o.o_kinds)
    | `C -> (match kind with
      | `Js | `OCaml -> false
      | `C -> match origin with
      | `Dir dir -> Sys.file_exists (dir / name ^ ".c")
      | `Other o -> List.mem `C o.o_kinds)
    | `O -> (match kind with
      | `Js -> false
      | `C | `OCaml -> match origin with
      | `Dir dir -> Sys.file_exists (dir / name ^ ".c")
      | `Other o -> List.mem `Ml o.o_kinds
                    || List.mem `C o.o_kinds
                    || List.mem `O o.o_kinds)
    | `Js -> (match kind with
      | `C | `OCaml -> false
      | `Js -> match origin with
      | `Dir dir -> Sys.file_exists (dir / name ^ ".js")
      | `Other o -> List.mem `Js o.o_kinds)
    | `Other _ -> false
    in
    if kind = `OCaml && not (has `Ml || has `Mli || has `Cmo || has `Cmi || has `Cmx)
    then
      As_shell.fatal_error 1
        "unit %s: cannot find %s.ml or %s.mli in `%s', stopping.\n"
        name name name (match origin with `Other _ -> "." | `Dir d -> d / "")
    else if kind = `C && not (has `C || has `O) then
      As_shell.fatal_error 1
        "unit %s: cannot find %s.c in `%s', stopping.\n"
        name name (match origin with `Other _ -> "."  | `Dir d -> d / "")
    else if kind = `Js && not (has `Js) then
      As_shell.fatal_error 1
        "unit %s: cannot find %s.js in `%s', stopping.\n"
        name name (match origin with `Other _ -> "." | `Dir d -> d / "/")
    else
    { u_name = name; u_available = available; u_deps = deps; u_origin = origin;
      u_flags = flags; u_for_pack = None; u_pack = []; u_container = None;
      u_kind = `OCaml; u_has = has; }

  let pack ?(available = As_features.true_) ?(flags = As_flags.empty) name units =
    (* FIXME: mutation *)
    let pack = Dir.create (`Other ("pack-" ^ name)) [] in
    let units = Unit.with_containers (`Dir pack) units in
    pack.d_contents <- (List.map (fun u -> `Unit u) units);
    let action r =
      let units = List.filter (has `Cmo) units in
      [ ( [`Cmo],
         let cmo = List.map (fun u -> cmo u r) units in
         (* FIXME: -for-pack *)
         As_action.create "%s -pack %s %s -o %s"
           (As_resolver.ocamlc r)
           (String.concat " " (As_flags.get `Compile `Byte flags))
           (String.concat " " cmo)
           name);
        ( [`Cmx],
          let cmx = List.map (fun u -> cmx u r) units in
          As_action.create "%s -pack %s %s -o %s"
            (As_resolver.ocamlopt r)
            (String.concat " " (As_flags.get `Compile `Native flags))
            (String.concat " " cmx)
            name ) ] in
    let origin = `Other (Other.create name action [`Cmo;`Cmx]) in
    let has = function `Cmo | `Cmx -> true | _ -> false in
    (* FIXME: mutation *)
    List.iter (fun u -> u.u_for_pack <- Some (String.capitalize name)) units;
    { u_name = name; u_available = available; u_flags = flags; u_origin = origin;
      u_for_pack = None; u_deps = []; u_pack = units;
      u_container = None; u_kind = `OCaml;
      u_has = has; }

  let generated_files t r =
    match t.u_kind with
    | `OCaml ->  [
        As_features.byte  , [cmi t r; cmo t r];
        As_features.native, [cmi t r; o t  r ; cmx t r];
        As_features.annot , [cmt t r; cmti t r];
      ]
    | `C -> [ As_features.true_, [dll_so t r; dll_so t r] ]
    | `Js -> []

  let _string_of_kind = function
  | `C -> "c"
  | `OCaml -> "ocaml"
  | `Js -> "js"

  let targets t resolver (mode:As_flags.mode) (phase:As_flags.phase) =
    let cmo_or_cmi () = [if has `Cmo t then cmo t resolver else cmi t resolver] in
    let cmx_or_cmi () = [if has `Cmx t then cmx t resolver else cmi t resolver] in
    match mode, phase, kind t with
    | `C      , `Link   , `C     -> [o t resolver]
    | `Js     , `Link   , `Js    -> [js t resolver]
    | `Native , `Link   , `OCaml -> cmx_or_cmi ()
    | `Native , `Compile, `OCaml -> [cmi t resolver]
    | `Byte   , `Link   , `OCaml -> cmo_or_cmi ()
    | `Byte   , `Compile, `OCaml -> [cmi t resolver]
    | `Shared , `Link   , `C     -> [dll_so t resolver]
    | `Shared , `Link   , `OCaml -> cmx_or_cmi ()
    | _ -> []

  (* XXX: memoize the function *)
  let flags t resolver =
    let deps = deps t |> Component.closure ~link:true in
    let flags =
      let open As_flags in
      v `Compile `Byte  (Component.comp_byte deps resolver (build_dir t)) @@@
      v `Compile `Native (Component.comp_native deps resolver (build_dir t)) @@@
      v `Compile `Shared (Component.comp_native deps resolver (build_dir t)) @@@
      v `Pp `Byte (Component.pp_byte deps resolver) @@@
      v `Pp `Native (Component.pp_native deps resolver)
    in
    As_flags.(flags @@@ t.u_flags)

  let sort us =
    let g = Component.Graph.create () in
    List.iter (fun (`Unit u) ->
        Component.Graph.add_vertex g (`Unit u);
        List.iter (function
          | `Unit dep -> Component.Graph.add_edge g (`Unit dep) (`Unit u)
          | _ -> ()
          ) (deps u)
      ) us;
    let us = Component.Graph.to_list g in
    List.map (function `Unit u -> u | _ -> assert false) us
end

and Other : sig
  include Component_base with type t = other

  val create: ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:Component.t list ->
    string -> As_action.t -> As_action.kind list -> t
  val file_of_kind: t -> As_resolver.t -> As_action.kind -> string
  val files: t -> As_resolver.t -> string list
  val actions: t -> As_resolver.t -> (As_action.kind list * string list) list
end = struct
  type t = other

  let id t = "other-" ^ t.o_name
  let name t = t.o_name
  let available g = g.o_available
  let deps t = t.o_deps
  let build_dir t r = As_resolver.build_dir r (id t)

  let create ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(deps = []) o_name o_action o_kinds
    =
    { o_name; o_available = available; o_flags = flags; o_deps = deps;
      o_kinds; o_action; }

  let flags t _ = t.o_flags
  let file t r ext = build_dir t r / t.o_name ^ ext
  let c t r = file t r ".c"
  let o t r = file t r ".o"
  let ml t r = file t r ".ml"
  let mli t r = file t r ".mli"
  let cmi t r = file t r ".cmi"
  let cmo t r = file t r ".cmo"
  let cmx t r = file t r ".cmx"
  let js t r = file t r ".js"

  let file_of_kind t r = function
  | `C   -> c t r
  | `O   -> o t r
  | `Ml  -> ml t r
  | `Mli -> mli t r
  | `Cmo -> cmo t r
  | `Cmi -> cmi t r
  | `Cmx -> cmx t r
  | `Js  -> js t r
  | `Other s -> file t r s

  let rec targets t r (mode:As_flags.mode) (phase:As_flags.phase) =
    let mk k = if List.mem k t.o_kinds then [file_of_kind t r k] else [] in
    match mode, phase with
    | `Byte  , `Compile -> mk `Ml @ mk `Mli @ mk `Cmi
    | `Byte  , `Link    -> mk `Cmo @ targets t r mode `Compile
    | `Native, `Compile -> mk `Ml @ mk `Mli @ mk `Cmi
    | `Native, `Link    -> mk `Cmx @ targets t r mode `Compile
    | `Shared, `Compile -> targets t r `Native `Compile
    | `Js    , `Link    -> mk `Js
    | `C     , `Link    -> mk `C
    | _ -> []

  let files t r =
    List.map (file_of_kind t r) t.o_kinds

  let generated_files t r = [ As_features.true_, files t r ]
  let actions t k = As_action.actions t.o_action k

end

and Pkg: sig
  include Component_base with type t = pkg
  type kind = [ `OCaml | `OCaml_pp | `C ]
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?opt:bool -> string -> kind -> t
  val kind : t -> kind
  val compiler_libs_toplevel : t
  val ctypes_stub : t
end = struct
  type kind = [ `OCaml | `OCaml_pp | `C ]
  type t = pkg

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
    let p_available = As_features.(available &&& pkg_f) in
    { p_name = name; p_flags = flags; p_available; p_kind = kind }

  let id t = "pkg-" ^ t.p_name
  let name t = t.p_name
  let available t = t.p_available
  let targets _t _r _mode _phase = []
  let flags t r = As_flags.(As_resolver.pkgs r [t.p_name] @@@ t.p_flags)
  let build_dir _t _r = failwith "Pkg.build_dir"
  let deps _t = []
  let generated_files _t _r = []
  let file _t _r _ext = failwith "Pkg.file"
  let kind t = t.p_kind

  (* Builtin packages *)

  let compiler_libs_toplevel = create "compiler-libs.toplevel" `OCaml
  let ctypes_stub = create "ctypes.stubs" `OCaml
end

and Lib : sig
  include Component_base with type t = lib
  type kind = [ `OCaml | `OCaml_pp ]
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:Component.t list -> ?pack:bool ->
    string -> kind -> [`Unit of Unit.t ] list -> t
  val kind : t -> kind
  val filename : t -> string
  val units : t -> Unit.t list
  val available : t -> As_features.t
  val cma : t -> As_resolver.t -> string
  val cmxa : t -> As_resolver.t -> string
  val a : t -> As_resolver.t -> string
  val cmxs : t -> As_resolver.t -> string
  (* not exported *)
  val set_filename : t -> string -> unit
end = struct
  type kind = [ `OCaml | `OCaml_pp ]
  type t = lib

  let id t = "lib-" ^ t.l_name

  let deps t =
    let us = List.map (fun u -> `Unit u) t.l_units in
    t.l_deps @ us

  let name t = t.l_name
  let kind t = t.l_kind
  let build_dir t r = As_resolver.build_dir r (id t)
  let units t = t.l_units
  let filename t = t.l_filename
  let set_filename t f = t.l_filename <- f
  let available t = t.l_available

  let create ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(deps = []) ?(pack = false) name kind units
    =
    let t =
      { l_name = name; l_available = available; l_kind = kind;
        l_units = []; l_deps = deps;
        l_flags = flags; l_filename = name; } in
    let units = Unit.sort units in
    let units =
      if pack then [Unit.with_container (`Lib t) (Unit.pack name units)]
      else Unit.with_containers (`Lib t) units in
    t.l_units <- units;
    t

  let file t r ext = build_dir t r / t.l_name ^ ext
  let cma t r = file t r ".cma"
  let cmxa t r = file t r ".cmxa"
  let cmxs t r = file t r ".cmxs"
  let a t r = file t r ".a"

  let targets t r (mode:As_flags.mode) _phase = match mode with
  | `Byte    -> [cma t r]
  | `Native  -> [cmxa t r; a t r]
  | `Shared  -> [cmxs t r]
  | (`C|`Js) -> []

  let generated_files t r = [
    As_features.(byte           &&& t.l_available), targets t r `Byte   `Link;
    As_features.(native         &&& t.l_available), targets t r `Native `Link;
    As_features.(native_dynlink &&& t.l_available), targets t r `Shared `Link;
  ] @ conmap (fun u -> Unit.generated_files u r) t.l_units

  let flags t resolver =
    let units = units t in
    let flags =
      let open As_flags in
      v `Link `Byte (Component.link_byte [] resolver units) @@@
      v `Link `Native (Component.link_native [] resolver units) @@@
      v `Link `Shared (Component.link_native [] resolver units)
    in
    As_flags.(flags @@@ t.l_flags)

end

and Bin: sig
  include Component_base with type t = bin
  val create :
    ?available:As_features.t ->
    ?flags:As_flags.t ->
    ?deps:Component.t list ->
    ?byte:bool ->
    ?native:bool ->
    ?js: bool ->
    ?link_all:bool ->
    ?install:bool ->
    string -> [`Unit of Unit.t ] list -> t
  val toplevel:
    ?available:As_features.t ->
    ?flags:As_flags.t ->
    ?deps:Component.t list ->
    ?custom:bool ->
    ?install:bool ->
    string -> [`Unit of Unit.t ] list -> t
  val units: t -> Unit.t list
  val has_js: t -> bool
  val available: t -> As_features.t
  val is_toplevel: t -> bool
  val install: t -> bool
  val byte: t -> As_resolver.t -> string
  val native: t -> As_resolver.t -> string
  val js: t -> As_resolver.t -> string
end = struct
  type t = bin

  let id t = "bin-" ^ t.b_name
  let units t = t.b_cus
  let build_dir t r = As_resolver.build_dir r (id t)
  let is_toplevel t = t.b_toplevel
  let install t = t.b_install
  let name t = t.b_name
  let available t = t.b_available
  let has_js t = t.b_js

  let deps t =
    t.b_deps @ List.map (fun u -> `Unit u) t.b_cus

  let create ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(deps = [])
      ?(byte = true) ?(native = true) ?(js = false)
      ?(link_all = false) ?(install = true)
      name comps
    =
    let available =
      let (&&&) = As_features.(&&&) in
      available
      &&& (if not native then As_features.(not_ native) else As_features.true_)
      &&& (if not byte then As_features.(not_ byte) else As_features.true_)
      &&& (if not js then As_features.(not_ js) else As_features.true_)
    in
    let flags =
      if link_all then As_flags.(linkall @@@ flags) else flags
    in
    let t =
      { b_name = name; b_available = available; b_flags = flags; b_deps = deps;
        b_cus = []; b_toplevel = false; b_install = install; b_js = js } in
    let us = Unit.sort comps in
    let us = Unit.with_containers (`Bin t) us in
    t.b_cus <- us;
    t

  let toplevel ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(deps = []) ?(custom = false) ?install name comps
    =
    let available = As_features.(not_ native &&& available) in
    let deps = `Pkg (Pkg.compiler_libs_toplevel) :: deps in
    let link_byte = [
      (if custom then "-custom " else "") ^ "-I +compiler-libs topstart.cmo"
    ] in
    let nflags = As_flags.v `Link `Byte link_byte in
    let flags = As_flags.(nflags @@@ flags) in
    let t = create ~available ~flags ~link_all:true ~deps ?install name comps in
    { t with b_toplevel = true }

  let file t r ext = build_dir t r / t.b_name ^ ext
  let byte t r = file t r ".byte"
  let native t r = file t r ".native"
  let js t r = file t r ".js"

  let targets t r (mode:As_flags.mode) _phase = match mode with
  | `Byte   -> [byte t r]
  | `Shared -> []
  | `Native -> [native t r]
  | `Js     -> [js t r]
  | `C      -> []

  let generated_files t r = [
    As_features.(byte   &&& t.b_available), targets t r `Byte   `Link;
    As_features.(native &&& t.b_available), targets t r `Native `Link;
    As_features.(js     &&& t.b_available), targets t r `Js     `Link;
  ]

  let flags t r =
    let cus = units t in
    let all_deps = Component.closure ~link:true (deps t) in
    let flags =
      let open As_flags in
      v `Compile `Byte (Component.comp_byte (deps t) r (build_dir t)) @@@
      v `Compile `Native (Component.comp_native (deps t) r (build_dir t)) @@@
      v `Link `Byte (Component.link_byte all_deps r cus) @@@
      v `Link `Native (Component.link_native all_deps r cus)
    in
    As_flags.(flags @@@ t.b_flags)

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
      d_install = install; d_contents; }

  let name d = match d.d_name with
  | `Lib -> "lib" | `Bin -> "bin" | `Sbin -> "sbin" | `Toplevel -> "toplevel"
  | `Share -> "share" | `Share_root -> "share-root" | `Etc -> "etc"
  | `Doc -> "doc" | `Misc -> "misc" | `Stublibs -> "stublibs"
  | `Man -> "man" | `Other n -> n

  let id d = "dir-" ^ (name d)
  let available d = d.d_available
  let flags d _ = d.d_flags
  let deps d = d.d_deps
  let targets t r mode phase =  conmap (fun c -> Component.targets c r mode phase) t.d_contents
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
    ?deps:Component.t list -> ?dir:string -> string -> command list -> t
  val dir: t -> string option
  val commands: t -> command list
end = struct
  type args = test_args
  type command = test_command
  type t = test

  let id t = "test-" ^ t.t_name
  let name t = t.t_name
  let available t = t.t_available
  let targets _t _r _mode _phase = []
  let flags t _ = t.t_flags
  let generated_files _t _r = []

  let file _t _r _ext = failwith "Test.file"
  let build_dir _t _r = failwith "Test.build_dir"
  let deps t = t.t_deps
  let create ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(deps = []) ?dir name cmds
    =
    let bin_deps =
      let add_dep acc = function
      | `Shell _ -> acc | `Bin (`Bin bin, _) -> `Bin bin :: acc
      in
      List.fold_left add_dep [] cmds
    in
    { t_name = name; t_available = available; t_flags = flags;
      t_deps = bin_deps @ deps; t_dir = dir; t_commands = cmds;  }

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
  let others = Component.(filter other t.components) in
  List.fold_left (fun acc o ->
      Other.files o resolver @ acc
    ) [] others

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
  let components = Component.closure ~link:false components in
  let components = List.filter (function
    | `Unit u -> Unit.container u = None
    | _ -> true
    ) components in
  let libs = Component.(filter lib_ocaml components) in
  List.iter (fun l ->
      if Lib.name l <> name then
        Lib.set_filename l (name ^ "." ^ Lib.name l)
    ) libs;
  let doc_public = match doc_public with
    | Some d -> d
    | None   -> conmap (function
        | `Lib l -> List.map Unit.name (Lib.units l)
        | `Unit u -> [Unit.name u]
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
    let libs = Component.(filter lib_ocaml t.components) in
    unionmap (fun x -> As_features.atoms (Lib.available x)) libs in
  let pps  =
    let pps = Component.(filter lib_ocaml_pp t.components) in
    unionmap (fun x -> As_features.atoms (Lib.available x)) pps in
  let bins =
    let bins = Component.(filter bin t.components) in
    unionmap (fun x -> As_features.atoms (Bin.available x)) bins in
  As_features.(builtin ++ libs ++ pps ++ bins)
