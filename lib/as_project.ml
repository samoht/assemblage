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

module StringSet = Set.Make (String)

type component =
  [ `Unit of comp_unit
  | `Other of other
  | `C of c
  | `JS of js
  | `Pkg of pkg
  | `Lib of lib
  | `Bin of bin
  | `Files of files
  | `Test of test ]

and container =
  [ `Lib of lib
  | `Bin of bin ]

and comp_unit =
  { u_name : string;
    u_available : As_features.t;
    u_flags : As_flags.t;
    u_deps : component list;
    u_origin : [`Dir of string | `Other of other];
    mutable u_container: container option;
    mutable u_for_pack : string option;
    mutable u_pack : comp_unit list;
    u_mli : bool;
    u_ml  : bool; }

and other =
  { o_name : string;
    o_available : As_features.t;
    o_flags : As_flags.t;
    o_deps : component list;
    o_action : As_action.t option;
    o_files : [`C |`Ml | `Mli] list; }

and c =
  { c_name : string;
    c_available : As_features.t;
    c_flags : As_flags.t;
    c_deps : component list;
    c_dir : string option;
    c_link_flags : string list;
    c_generated : bool;
    mutable c_container: container option; }

and js =
  { j_bin : bin;
    j_available : As_features.t;
    j_args: string list; }

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
    l_us : comp_unit list;
    l_cs  : c list;
    mutable l_filename : string; }

and bin =
  { b_name : string;
    b_available : As_features.t;
    b_flags : As_flags.t;
    b_deps : component list;
    b_cus : comp_unit list;
    b_toplevel : bool;
    b_install : bool; }

and files =
  { f_name :
      [ `Lib | `Bin | `Sbin | `Toplevel | `Share | `Share_root | `Etc | `Doc
      | `Misc | `Stublibs | `Man | `Other of string ];
    f_available : As_features.t;
    f_flags : As_flags.t;
    f_deps : component list;
    f_install : bool;
    f_contents : component list; }

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
  val other : t -> other option
  val c : t -> c option
  val js : t -> js option
  val pkg : t -> pkg option
  val pkg_ocaml : t -> pkg option
  val pkg_ocaml_pp : t -> pkg option
  val pkg_c : t -> pkg option
  val lib : t -> lib option
  val lib_ocaml : t -> lib option
  val lib_ocaml_pp : t -> lib option
  val bin : t -> bin option
  val files : t -> files option
  val test : t -> test option
  val filter : (t -> 'a option) -> t list -> 'a list
  val closure : t list -> t list
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
 (* not exported *)
 val container_error: t -> current:container -> container -> 'a
end = struct
  type t = component
  type component = t

  let id = function
  | `Unit u -> Unit.id u
  | `Other o -> Other.id o
  | `C c -> C.id c
  | `JS js -> JS.id js
  | `Pkg p -> Pkg.id p
  | `Lib l -> Lib.id l
  | `Bin b -> Bin.id b
  | `Files d -> Files.id d
  | `Test t -> Test.id t

  let name = function
  | `Unit u -> Unit.name u
  | `Other o -> Other.name o
  | `C c -> C.name c
  | `JS js -> JS.name js
  | `Pkg p -> Pkg.name p
  | `Lib l -> Lib.name l
  | `Bin b  -> Bin.name b
  | `Files d -> Files.name d
  | `Test t -> Test.name t

  let available = function
  | `Unit u -> Unit.available u
  | `Other o -> Other.available o
  | `C c -> C.available c
  | `JS js -> JS.available js
  | `Pkg p -> Pkg.available p
  | `Lib l -> Lib.available l
  | `Bin b -> Bin.available b
  | `Files d -> Files.available d
  | `Test t -> Test.available t

  let prereqs = function
  | `Unit u -> Unit.prereqs u
  | `Other o -> Other.prereqs o
  | `C c -> C.prereqs c
  | `JS js -> JS.prereqs js
  | `Pkg p -> Pkg.prereqs p
  | `Lib l -> Lib.prereqs l
  | `Bin b -> Bin.prereqs b
  | `Files d -> Files.prereqs d
  | `Test t -> Test.prereqs t

  let flags = function
  | `Unit u -> Unit.flags u
  | `Other o -> Other.flags o
  | `C c -> C.flags c
  | `JS js -> JS.flags js
  | `Pkg p -> Pkg.flags p
  | `Lib l -> Lib.flags l
  | `Bin b -> Bin.flags b
  | `Files d -> Files.flags d
  | `Test t -> Test.flags t

  let generated_files = function
  | `Unit u -> Unit.generated_files u
  | `Other o -> Other.generated_files o
  | `C c -> C.generated_files c
  | `JS js -> JS.generated_files js
  | `Pkg p -> Pkg.generated_files p
  | `Lib l -> Lib.generated_files l
  | `Bin b -> Bin.generated_files b
  | `Files d -> Files.generated_files d
  | `Test t -> Test.generated_files t

  let file = function
  | `Unit u -> Unit.file u
  | `Other o -> Other.file o
  | `C c -> C.file c
  | `JS js -> JS.file js
  | `Pkg p -> Pkg.file p
  | `Lib l -> Lib.file l
  | `Bin b -> Bin.file b
  | `Files d -> Files.file d
  | `Test t -> Test.file t

  let build_dir = function
  | `Unit u -> Unit.build_dir u
  | `Other o -> Other.build_dir o
  | `C c -> C.build_dir c
  | `JS js -> JS.build_dir js
  | `Pkg p -> Pkg.build_dir p
  | `Lib l -> Lib.build_dir l
  | `Bin b  -> Bin.build_dir b
  | `Files d -> Files.build_dir d
  | `Test t -> Test.build_dir t

  let deps = function
  | `Unit u -> Unit.deps u
  | `Other o -> Other.deps o
  | `C c -> C.deps c
  | `JS js -> JS.deps js
  | `Pkg p -> Pkg.deps p
  | `Lib l -> Lib.deps l
  | `Bin b -> Bin.deps b
  | `Files d -> Files.deps d
  | `Test t -> Test.deps t

  let unit = function `Unit x -> Some x | _ -> None
  let other = function `Other x -> Some x | _ -> None
  let c = function `C c -> Some c | _ -> None
  let js = function `JS x -> Some x | _ -> None

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
  let files = function `Files x -> Some x | _ -> None
  let test = function `Test x -> Some x | _ -> None

  let filter fn l =
    List.fold_left (fun acc x ->
        match fn x with
        | None   -> acc
        | Some x -> x :: acc
      ) [] l
  |> List.rev

  let closure (ts : t list) : t list =
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
              (function
                | `Pkg pkg when Pkg.kind pkg = `OCaml_pp -> false
                | `Lib lib when Lib.kind lib = `OCaml_pp -> false
                | _     -> true)
              (deps h) in
          aux acc (d' @ d)
        )
    in
    aux [] ts

  let comp_flags mode (deps:t list) resolver build_dir =
    let incl = build_dir resolver in
    let us = filter unit deps |> List.map (fun u -> Unit.build_dir u resolver)in
    let libs = filter lib_ocaml deps |> List.map (fun l -> Lib.build_dir l resolver)in
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

  let container_error t ~(current:container) (container:container) =
    let current = (current:>t) in
    let container = (container:>t) in
    As_shell.fatal_error 1
      "The component %s cannot be put inside %s as it already belongs to %s."
      (name t) (id container) (id current)

end

and Unit: sig
  include Component_base with type t = comp_unit

  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:Component.t list -> string -> [`Dir of string | `Other of other] -> t
  val dir: t -> string option
  val container: t -> container option
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
  val sort : [ `Unit of t ] list -> t list
end = struct
  type t = comp_unit

  let id t =
    match t.u_container with
    | None   -> t.u_name
    | Some c -> Component.id (c:>component) ^ "-" ^ t.u_name

  let ml t = t.u_ml
  let mli t = t.u_mli
  let dir t = match t.u_origin with
  | `Dir d   -> Some d
  | `Other _ -> None

  let generated t =
    match t.u_origin with
    | `Dir _   -> false
    | `Other _ -> true

  let build_dir t resolver =
    match t.u_container with
    | None   -> As_resolver.build_dir resolver ""
    | Some c -> Component.build_dir (c:>component) resolver

  let deps t =
    match t.u_origin with
    | `Dir _   ->  t.u_deps
    | `Other o -> `Other o :: t.u_deps

  let name t = t.u_name
  let available t = t.u_available
  let container t = t.u_container
  let for_pack t = t.u_for_pack
  let unpack t = t.u_pack
  let set_lib_container t lib =
    match t.u_container with
    | Some c -> Component.container_error (`Unit t) ~current:c (`Lib lib)
    | None   -> t.u_container <- Some (`Lib lib)
  let set_bin_container t bin =
    match t.u_container with
    | Some c -> As_shell.fatal_error 1
                  "%s is already in %s." (name t) (Component.id (c:>component))
    | None   -> t.u_container <- Some (`Bin bin)

  let create ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(deps = []) name origin
    =
    let mli = match origin with
    | `Dir dir -> Sys.file_exists (dir / name ^ ".mli")
    | `Other o -> List.mem `Mli o.o_files
    in
    let ml = match origin with
    | `Dir dir -> Sys.file_exists (dir / name ^ ".ml")
    | `Other o -> List.mem `Ml o.o_files
    in
    if not ml && not mli
    then
      As_shell.fatal_error 1
        "unit %s: cannot find %s.ml or %s.mli in `%s', stopping.\n"
        name name name (match origin with `Other _ -> "." | `Dir d -> d / "")
    else
    { u_name = name; u_available = available; u_deps = deps; u_origin = origin;
      u_flags = flags; u_container = None; u_for_pack = None; u_pack = [];
      u_ml = ml; u_mli = mli; }

  let pack ?(available = As_features.true_) ?(flags = As_flags.empty) cus name =
    (* FIXME: mutation *)
    let origin = `Other (Other.create [`Ml] name) in
    List.iter (fun u -> u.u_for_pack <- Some (String.capitalize name)) cus;
    { u_name = name; u_available = available; u_flags = flags; u_origin = origin;
      u_container = None; u_for_pack = None; u_deps = []; u_pack = cus;
      u_ml = false; u_mli = false;  }

  let file t r ext = build_dir t r / t.u_name ^ ext
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
          let cs = Component.(filter c) u.u_deps in
          let cobjs = List.map (fun c -> C.dll_so c resolver) cs in
          cmx u resolver :: cobjs
      ) comps in
    let libs = Component.(filter lib_ocaml) deps in
    let libs = List.map (fun l ->
        match mode with
        | `Shared
        | `Native -> Lib.cmxa l resolver
        | `Byte   -> Lib.cma  l resolver
      ) libs in
    let pps = Component.(filter lib_ocaml_pp) deps in
    let pps = List.map (fun l -> Lib.cma l resolver) pps in
    comps @ libs @ pps

  (* XXX: memoize the function *)
  let flags t resolver =
    let deps = deps t |> Component.closure in
    let flags =
      let open As_flags in
      v `Compile `Byte  (Component.comp_byte deps resolver (build_dir t)) @@@
      v `Compile `Native (Component.comp_native deps resolver (build_dir t)) @@@
      v `Pp `Byte (Component.pp_byte deps resolver) @@@
      v `Pp `Native (Component.pp_native deps resolver)
    in
    As_flags.(flags @@@ t.u_flags)

  let sort us =
    let g = Component.Graph.create () in
    List.iter (fun (`Unit u) ->
        Component.Graph.add_vertex g (`Unit u);
        List.iter (function
          | `Unit dep ->
              if List.mem (`Unit dep) us then
                Component.Graph.add_edge g (`Unit dep) (`Unit u)
          | _ -> ()
          ) (deps u)
      ) us;
    let us = Component.Graph.to_list g in
    List.map (function `Unit u -> u | _ -> assert false) us
end

and Other : sig
  include Component_base with type t = other

  val create: ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:Component.t list -> ?action:As_action.t ->
    [`C | `Ml | `Mli ] list -> string -> t
  val files: t -> As_resolver.t -> string list
  val actions: t -> As_resolver.t -> string list
end = struct
  type t = other

  let id t = "other-" ^ t.o_name
  let name t = t.o_name
  let available g = g.o_available
  let deps t = t.o_deps

  let create ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(deps = []) ?action o_files o_name
    =
    { o_name; o_available = available; o_flags = flags; o_deps = deps;
      o_files; o_action = action; }

  let build_dir t r =
    As_resolver.build_dir r (id t)

  let prereqs t r mode =
    let bins = Component.(filter bin t.o_deps) in
    List.map (fun b -> match mode with
        | `Byte   -> Bin.byte b r
        | `Shared
        | `Native -> Bin.native b r
      ) bins

  let flags t _ = t.o_flags
  let file t r ext = build_dir t r / t.o_name ^ ext
  let ml t r = file t r ".ml"
  let mli t r = file t r ".mli"

  let files t r =
    List.map (function
        | `C    -> file t r ".c"
        | `Ml   -> ml t r
        | `Mli  -> mli t r
      ) t.o_files

  let generated_files t r = [ As_features.true_, files t r ]
  let actions t r =
    match t.o_action with
    | None   -> []
    | Some a -> As_action.actions a r
end

and C: sig
  include Component_base with type t = c
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:Component.t list -> ?dir:string -> ?generated:bool ->
    ?link_flags:string list -> string -> t
  val container: t -> container option
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
      ?(deps = []) ?dir ?(generated = false) ?(link_flags = []) name
    =
    { c_name = name; c_available = available; c_flags = flags; c_deps = deps;
      c_dir = dir; c_link_flags = link_flags; c_generated = generated;
      c_container = None }

  let build_dir t r =
    match t.c_container with
    | None   -> As_resolver.build_dir r (id t)
    | Some c -> Component.build_dir (c:>component) r

  let container t = t.c_container
  let set_lib_container t lib =
    match t.c_container with
    | Some c -> Component.container_error (`C t) ~current:c (`Lib lib)
    | None   -> t.c_container <- Some (`Lib lib)
  let set_bin_container t bin =
    match t.c_container with
    | Some c -> Component.container_error (`C t) ~current:c (`Bin bin)
    | None   -> t.c_container <- Some (`Bin bin)
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
  let flags t _r = As_flags.v `Link `Byte t.j_args
  let build_dir t r = Bin.build_dir (bin t) r
  let prereqs t r _ = [Bin.byte (bin t) r]
  let generated_files t r = [As_features.js, [js t r]]
  let deps t = [`Bin (bin t)]
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
  let prereqs _t _r _mode = []
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
    ?c:C.t list -> string -> kind -> [`Unit of Unit.t ] list -> t
  val kind : t -> kind
  val filename : t -> string
  val units : t -> Unit.t list
  val c_objects : t -> C.t list
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
    let us = List.map (fun u -> `Unit u) t.l_us in
    let cs = List.map (fun c -> `C c) t.l_cs in
    cs @ us @ t.l_deps

  let name t = t.l_name
  let kind t = t.l_kind
  let build_dir t resolver = As_resolver.build_dir resolver (id t)
  let units t = t.l_us
  let c_objects t = t.l_cs
  let filename t = t.l_filename
  let set_filename t f = t.l_filename <- f
  let available t = t.l_available

  let create ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(deps = []) ?(pack = false) ?(c = []) name kind us
    =
    let us = Unit.sort us in
    let us' = if pack then [Unit.pack us name] else us in
    let t =
      { l_name = name; l_available = available; l_kind = kind;
        l_us = us'; l_cs = c; l_deps = deps;
        l_flags = flags; l_filename = name; }
    in
    (* FIXME: mutation *)
    List.iter (fun u -> Unit.set_lib_container u t)
      (if pack then us' @ us else us);
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
    @ conmap (fun u -> Unit.generated_files u resolver) t.l_us

  let flags t resolver =
    let comps = units t in
    let flags =
      let open As_flags in
      v `Link `Byte (Component.link_byte [] resolver comps) @@@
      v `Link `Native (Component.link_native [] resolver comps)
    in
    As_flags.(flags @@@ t.l_flags)

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
    ?deps:Component.t list ->
    ?byte_only:bool ->
    ?link_all:bool ->
    ?install:bool ->
    [`Unit of Unit.t ] list -> string -> t
  val toplevel:
    ?available:As_features.t ->
    ?flags:As_flags.t ->
    ?deps:Component.t list ->
    ?custom:bool ->
    ?install:bool ->
    [`Unit of Unit.t ] list -> string -> t
  val units: t -> Unit.t list
  val available: t -> As_features.t
  val is_toplevel: t -> bool
  val install: t -> bool
  val byte: t -> As_resolver.t -> string
  val native: t -> As_resolver.t -> string
end = struct
  type t = bin

  let id t = "bin-" ^ t.b_name
  let units t = t.b_cus
  let build_dir t resolver = As_resolver.build_dir resolver (id t)
  let is_toplevel t = t.b_toplevel
  let install t = t.b_install
  let name t = t.b_name
  let available t = t.b_available

  let deps t =
    List.map (fun u -> `Unit u) t.b_cus @ t.b_deps

  let create ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(deps = []) ?(byte_only = false) ?(link_all = false) ?(install = true)
      comps name
    =
    let us = Unit.sort comps in
    let available =
      if byte_only then As_features.(not_ native &&& available) else available
    in
    let flags =
      if link_all then As_flags.(linkall @@@ flags) else flags
    in
    let t =
      { b_name = name; b_available = available; b_flags = flags; b_deps = deps;
        b_cus = us; b_toplevel = false; b_install = install }
    in
    List.iter (fun u -> Unit.set_bin_container u t) us;
    t

  let toplevel ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(deps = []) ?(custom = false) ?install comps name
    =
    let available = As_features.(not_ native &&& available) in
    let deps = `Pkg (Pkg.compiler_libs_toplevel) :: deps in
    let link_byte = [
      (if custom then "-custom " else "") ^ "-I +compiler-libs topstart.cmo"
    ] in
    let nflags = As_flags.v `Link `Byte link_byte in
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
    let libs = Component.(filter lib_ocaml) deps in
    let pps = Component.(filter lib_ocaml_pp) deps in
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
    let flags =
      let open As_flags in
      v `Compile `Byte (Component.comp_byte (deps t) resolver (build_dir t)) @@@
      v `Compile `Native
        (Component.comp_native (deps t) resolver (build_dir t)) @@@
      v `Link `Byte (Component.link_byte all_deps resolver comps) @@@
      v `Link `Native (Component.link_native all_deps resolver comps)
    in
    As_flags.(flags @@@ t.b_flags)

end

and Files : sig
  include Component_base with type t = files
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list -> ?install:bool ->
    [ `Lib | `Bin | `Sbin | `Toplevel | `Share | `Share_root | `Etc | `Doc
    | `Misc | `Stublibs | `Man | `Other of string ] -> component list -> files
end = struct
  type t = files

  let create ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(deps = []) ?(install = true) f_name f_contents =
    { f_name; f_available = available; f_flags = flags; f_deps = deps;
      f_install = install; f_contents; }

  let name d = match d.f_name with
  | `Lib -> "lib" | `Bin -> "bin" | `Sbin -> "sbin" | `Toplevel -> "toplevel"
  | `Share -> "share" | `Share_root -> "share-root" | `Etc -> "etc"
  | `Doc -> "doc" | `Misc -> "misc" | `Stublibs -> "stublibs"
  | `Man -> "man" | `Other n -> n

  let id d = "dir-" ^ (name d)
  let available d = d.f_available
  let flags d _ = d.f_flags
  let deps d = d.f_deps
  let prereqs _ _ _ = [] (* FIXME mode doesn't make sense *)
  let file _ = invalid_arg "Files.file: not applicable"
  let build_dir d r = As_resolver.build_dir r (id d)
  let generated_files d r =
    let add c =
      let refine_avail (a, files) = As_features.(d.f_available &&& a), files in
      List.map refine_avail (Component.generated_files c r)
    in
    List.flatten (List.map add d.f_contents)
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
