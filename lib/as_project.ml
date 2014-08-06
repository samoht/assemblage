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
  | `Test of test
  | `Doc of doc ]

and container =
  { c_name: string;
    c_id: string;
    c_available: As_features.t;
    c_flags: As_flags.t;
    c_deps: component list;
    c_container: container option;
    c_contents: component list; }

and comp_unit =
  { u_name : string;
    u_available : As_features.t;
    u_flags : As_flags.t;
    u_deps : component list;
    u_container : container option;
    u_origin : [`Dir of string | `Other of other];
    u_kind : [ `OCaml | `C | `Js ];
    u_has : As_action.file -> bool; }

and other =
  { o_name : string;
    o_available : As_features.t;
    o_flags : As_flags.t;
    o_deps : component list;
    o_container : container option;
    o_actions : component As_action.rule list; }

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
    l_origin : [`Units of comp_unit list | `Other of other];
    l_container : container option;
    l_kind : [ `OCaml | `OCaml_pp ]; }

and bin =
  { b_name : string;
    b_available : As_features.t;
    b_flags : As_flags.t;
    b_deps : component list;
    b_origin : [`Units of comp_unit list | `Other of other];
    b_container : container option;
    b_toplevel : bool;
    b_install : bool;
    b_js: bool; }

and dirname =
  [ `Lib | `Bin | `Sbin | `Toplevel | `Share | `Share_root | `Etc | `Doc
  | `Misc | `Stublibs | `Man | `Other of string ]

and dir =
  { d_name : dirname;
    d_available : As_features.t;
    d_flags : As_flags.t;
    d_deps : component list;
    d_contents : component list;
    d_container : container option;
    d_install : bool; }

and test_args = As_resolver.t -> string list

and test_command =
  [ `Bin of [`Bin of bin] * test_args
  | `Shell of string ]

and test =
  { t_name : string;
    t_available : As_features.t;
    t_flags : As_flags.t;
    t_deps : component list;
    t_container : container option;
    t_dir : string option;
    t_commands : test_command list; }

and doc =
  { doc_name: string;
    doc_available: As_features.t;
    doc_flags: As_flags.t;
    doc_deps: component list;
    doc_container: container option;
    doc_contents: component list;
    doc_install: bool;
  }

type t =
  { name : string;
    version : string;
    available : As_features.t;
    flags : As_flags.t;
    components : component list; }

let string_of_dirname = function
| `Lib -> "lib" | `Bin -> "bin" | `Sbin -> "sbin" | `Toplevel -> "toplevel"
| `Share -> "share" | `Share_root -> "share-root" | `Etc -> "etc"
| `Doc -> "doc" | `Misc -> "misc" | `Stublibs -> "stublibs"
| `Man -> "man" | `Other n -> n

module type Component_base = sig
  type t
  val id: t -> string
  val name: t -> string
  val source_dir: t -> string option
  val available: t -> As_features.t
  val flags : t -> As_resolver.t -> As_flags.t
  val deps : t -> component list
  val container : t -> container option
  val contents : t -> component list
  val rules: t -> component As_action.rule list
  val generated_files : t -> (As_features.t * As_action.file list) list
end

(* not exported *)
module type Component_ext = sig
  include Component_base
  val with_container : container -> t -> t
  val with_deps : component list -> t -> t
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
  val to_list: t -> elt list
  val of_list: elt list -> t
end

module Graph
    (X: sig
       type t
       val id: t -> string
       val deps: t -> t list
       val contents: t -> t list
     end) =
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
        let deps = X.deps t @ X.contents t in
        List.iter (fun d -> if List.mem d ts then add_edge g d t) deps
      ) ts;
    g
end

module rec Container: sig
  type t = container
  val name: t -> string
  val id: all:bool -> t -> string
  val available: all:bool -> t -> As_features.t
  val flags: all:bool -> t -> As_flags.t
  val deps: all:bool -> t -> component list
  val phony_prepare: t -> string
  val contents: t -> component list
end = struct
  type t = container
  let name t = t.c_name

  let containers t =
    let rec aux acc = function
    | None   -> List.rev acc
    | Some c -> aux (c::acc) c.c_container
    in
    aux [t] t.c_container

  let id ~all t = match all with
  | false -> t.c_id
  | true  ->
      let ids = List.map (fun t -> t.c_id) (containers t) in
      String.concat "-" ids

  let available ~all t = match all with
  | false -> t.c_available
  | true  ->
      List.fold_left As_features.(&&&) As_features.true_
        (List.map (fun t -> t.c_available) (containers t))

  let flags ~all t = match all with
  | false -> t.c_flags
  | true ->
      List.fold_left As_flags.(@@@) As_flags.empty
        (List.map (fun t -> t.c_flags) (containers t))

  let deps ~all t = match all with
  | false -> t.c_deps
  | true  ->
      List.fold_left (@) []
        (List.map (fun t -> t.c_deps) (containers t))
      |> Component.dedup

  let phony_prepare t =
    id ~all:true t ^ "-prepare"

  let contents t =
    Component.map (Component.with_container t) t.c_contents

end

and Component : sig
  include Component_ext with type t = component
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
  val doc : t -> doc option
  val filter : (t -> 'a option) -> t list -> 'a list
  val closure : ?link:bool -> t list -> t list
  val build_dir: t -> As_resolver.t -> string
  val file : t -> As_resolver.t -> As_action.file -> string
  val source: t -> As_action.file -> string
  val map: (t -> t) -> t list -> t list
  val phases: t -> As_flags.phase list
  module Set: Set with type elt = t
  module Graph: Graph with type V.t = t
  (* not exported *)
  val dedup: t list -> t list
end = struct
  type t = component
  type component = t

  let name = function
  | `Unit u -> Unit.name u
  | `Other o -> Other.name o
  | `Pkg p -> Pkg.name p
  | `Lib l -> Lib.name l
  | `Bin b  -> Bin.name b
  | `Dir d -> Dir.name d
  | `Test t -> Test.name t
  | `Doc d -> Doc.name d

  let base_id = function
  | `Unit u -> Unit.id u
  | `Other o -> Other.id o
  | `Pkg p -> Pkg.id p
  | `Lib l -> Lib.id l
  | `Bin b  -> Bin.id b
  | `Dir d -> Dir.id d
  | `Test t -> Test.id t
  | `Doc d -> Doc.id d

  let container = function
  | `Unit u -> Unit.container u
  | `Other o -> Other.container o
  | `Pkg p -> Pkg.container p
  | `Lib l -> Lib.container l
  | `Bin b -> Bin.container b
  | `Dir d -> Dir.container d
  | `Test t -> Test.container t
  | `Doc d -> Doc.container d

  let id (t:t) = match container t with
  | Some c -> Container.id ~all:true c ^ "-" ^ name t
  | None   -> base_id t

  module Set = struct

    include Set.Make(struct
        type t = component
        let compare x y = String.compare (id x) (id y)
      end)

    let to_list = elements

    let of_list l =
      let add set elt = add elt set in
      List.fold_left add empty l
  end

  let dedup l =
    List.fold_left (fun (saw, result) elt ->
        if Set.mem elt saw then (saw, result)
        else (Set.add elt saw, elt :: result)
      ) (Set.empty, []) l
    |> snd |> List.rev

  let source_dir = function
  | `Unit u -> Unit.source_dir u
  | `Other o -> Other.source_dir o
  | `Pkg p -> Pkg.source_dir p
  | `Lib l -> Lib.source_dir l
  | `Bin b -> Bin.source_dir b
  | `Dir d -> Dir.source_dir d
  | `Test t -> Test.source_dir t
  | `Doc d -> Doc.source_dir d

  let base_available = function
  | `Unit u -> Unit.available u
  | `Other o -> Other.available o
  | `Pkg p -> Pkg.available p
  | `Lib l -> Lib.available l
  | `Bin b -> Bin.available b
  | `Dir d -> Dir.available d
  | `Test t -> Test.available t
  | `Doc d -> Doc.available d

  let available t = match container t with
  | None   -> base_available t
  | Some c ->
      As_features.(base_available t &&& Container.available ~all:true c)

  let rules = function
  | `Unit u -> Unit.rules u
  | `Other o -> Other.rules o
  | `Pkg p -> Pkg.rules p
  | `Lib l -> Lib.rules l
  | `Bin b -> Bin.rules b
  | `Dir d -> Dir.rules d
  | `Test t -> Test.rules t
  | `Doc d -> Doc.rules d

  let base_deps = function
  | `Unit u -> Unit.deps u
  | `Other o -> Other.deps o
  | `Pkg p -> Pkg.deps p
  | `Lib l -> Lib.deps l
  | `Bin b -> Bin.deps b
  | `Dir d -> Dir.deps d
  | `Test t -> Test.deps t
  | `Doc d -> Doc.deps d

  let deps t = match container t with
  | None   -> base_deps t
  | Some c -> dedup (Container.deps ~all:true c @ base_deps t)

  let contents = function
  | `Unit u -> Unit.contents u
  | `Other o -> Other.contents o
  | `Pkg p -> Pkg.contents p
  | `Lib l -> Lib.contents l
  | `Bin b -> Bin.contents b
  | `Dir d -> Dir.contents d
  | `Test t -> Test.contents t
  | `Doc d -> Doc.contents d

  let with_deps x = function
  | `Unit u -> `Unit (Unit.with_deps x u)
  | `Other o -> `Other (Other.with_deps x o)
  | `Pkg p -> `Pkg (Pkg.with_deps x p)
  | `Lib l -> `Lib (Lib.with_deps x l)
  | `Bin b -> `Bin (Bin.with_deps x b)
  | `Dir d -> `Dir (Dir.with_deps x d)
  | `Test t -> `Test (Test.with_deps x t)
  | `Doc d -> `Doc (Doc.with_deps x d)

  let with_container x = function
  | `Unit u -> `Unit (Unit.with_container x u)
  | `Other o -> `Other (Other.with_container x o)
  | `Pkg p -> `Pkg (Pkg.with_container x p)
  | `Lib l -> `Lib (Lib.with_container x l)
  | `Bin b -> `Bin (Bin.with_container x b)
  | `Dir d -> `Dir (Dir.with_container x d)
  | `Test t -> `Test (Test.with_container x t)
  | `Doc d -> `Doc (Doc.with_container x d)

  let generated_files = function
  | `Unit u -> Unit.generated_files u
  | `Other o -> Other.generated_files o
  | `Pkg p -> Pkg.generated_files p
  | `Lib l -> Lib.generated_files l
  | `Bin b -> Bin.generated_files b
  | `Dir d -> Dir.generated_files d
  | `Test t -> Test.generated_files t
  | `Doc d -> Doc.generated_files d

  let base_flags = function
  | `Unit u -> Unit.flags u
  | `Other o -> Other.flags o
  | `Pkg p -> Pkg.flags p
  | `Lib l -> Lib.flags l
  | `Bin b -> Bin.flags b
  | `Dir d -> Dir.flags d
  | `Test t -> Test.flags t
  | `Doc d -> Doc.flags d

  let flags t r = match container t with
  | None   -> base_flags t r
  | Some c -> As_flags.(base_flags t r @@@ Container.flags ~all:true c)

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
  let doc = function `Doc d -> Some d | _ -> None

  let filter fn l =
    List.fold_left (fun acc x ->
        match fn x with
        | None   -> acc
        | Some x -> x :: acc
      ) [] l
    |> List.rev

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
          let deps = deps h @ contents h in
          let d' = if not link then deps else
            List.filter
              (function
              | `Unit _ -> true
              | `Pkg pkg when Pkg.kind pkg = `OCaml -> true
              | `Lib lib when Lib.kind lib = `OCaml -> true
              | _     -> false)
              deps in
          aux acc (d' @ d)
        )
    in
    aux [] ts

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

  module Graph = Graph(struct
      type t = component
      let id = id
      let deps = deps
      let contents = contents
    end)

  let sort ts =
    let ts' = closure ts in
    List.filter (fun t -> List.memq t ts) ts'

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

  let source t x = match source_dir t with
  | None   -> failwith (sprintf "%s does not have any source directory." (id t))
  | Some d -> d / As_action.string_of_file (name t) x

  let build_dir t r = match container t with
  | None   -> As_resolver.build_dir r / base_id t
  | Some c -> As_resolver.build_dir r / Container.id ~all:true c

  let file t r x =
    build_dir t r / As_action.string_of_file (name t) x

end

and Rule: sig
  val link: As_action.file -> component As_action.rule
  val mkdir: component As_action.rule
  val files: component -> As_resolver.t -> component As_action.node list -> string list
  val phony_prepare: component -> string
  val phony_run: component -> string
end = struct

  let link (x:As_action.file) =
    As_action.rule
      ~phase:`Prepare
      ~targets:[`Self x]
      ~prereqs:[`Self (`Source x); `Self `Dir]
      (fun t r f ->
         let source = Component.source t x in
         let target = Component.file t r x in
         let cwd = As_resolver.root_dir r in
         As_action.link r ~source:(cwd / source) ~target)

  let mkdir =
    As_action.rule
      ~phase:`Prepare
      ~targets:[`Self `Dir]
      ~prereqs:[]
      (fun t r f -> As_action.mkdir r (Component.build_dir t r))

  let files t r ns =
    List.fold_left (fun acc -> function
      | `Phony x -> x :: acc
      | `Self (`Source f) -> Component.source t f :: acc
      | `Self f  -> Component.file t r f :: acc
      | `N (c,f) -> Component.file c r f :: acc
      ) [] ns
    |> List.rev

  let phony_prepare t = Component.id t ^ "-prepare"
  let phony_run t = Component.id t ^ "-run"

end

and Unit: sig
  include Component_ext with type t = comp_unit
  type kind = [`OCaml|`C|`Js]
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list -> string -> kind ->
    [`Dir of string | `Other of other] -> t
  val pack : ?available:As_features.t -> ?flags:As_flags.t ->
    string -> t list -> t
  val generated: t -> bool
  val kind: t -> [`OCaml | `C | `Js]
  val has: As_action.file -> t -> bool
  (* not exported *)
  val map: (t -> t) -> t list -> t list
  val comp_flags: component list -> build_dir:string -> As_resolver.t -> As_flags.t
end = struct
  type t = comp_unit
  type kind = [`OCaml|`C|`Js]
  let name t = t.u_name
  let id t = "unit-" ^ name t
  let kind t = t.u_kind
  let has kind t = t.u_has kind
  let available t = t.u_available
  let contents _t = []
  let container t = t.u_container
  let with_container c t = { t with u_container = Some c }
  let with_deps u_deps t = { t with u_deps }
  let generated t = match t.u_origin with `Dir _ -> false | _ -> true

  let map fn ts =
    List.map (fun u -> `Unit u) ts
    |> Component.map (function `Unit u -> `Unit (fn u) | x -> x)
    |> Component.(filter unit)

  let source_dir t = match t.u_origin with
  | `Dir d   -> Some d
  | `Other _ -> None

  let generated t = match t.u_origin with
  | `Dir _   -> false
  | `Other _ -> true

  let deps t = match t.u_origin with
  | `Dir _   ->  t.u_deps
  | `Other o -> `Other o :: t.u_deps

  let create ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(deps = []) name (kind:[`OCaml|`C|`Js]) origin
    =
    let has (file:As_action.file) =
      let check k files =
        if k <> kind then false
        else match origin with
        | `Dir dir -> List.exists (fun f ->
            Sys.file_exists (dir / As_action.string_of_file name f)
          ) files
        | `Other o -> List.exists (fun f ->
            List.mem f (Other.self_targets o)
          ) files
      in
      match file with
      | `Source f -> (match origin with
        | `Dir dir -> Sys.file_exists (dir / As_action.string_of_file name f)
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
          match origin with
          | `Other o -> List.mem x (Other.self_targets o)
          | _ -> false
    in
    let is_generated = match origin with `Dir _ -> false | _ -> true in
    if not is_generated && kind = `OCaml && not (List.exists has [`Ml;`Mli])
    then
      As_shell.fatal_error 1
        "unit %s: cannot find %s.ml or %s.mli in `%s', stopping.\n"
        name name name (match origin with `Other _ -> "." | `Dir d -> d / "")
    else if not is_generated && kind = `C && not (has `C) then
      As_shell.fatal_error 1
        "unit %s: cannot find %s.c in `%s', stopping.\n"
        name name (match origin with `Other _ -> "."  | `Dir d -> d / "")
    else if not is_generated && kind = `Js && not (has `Js) then
      As_shell.fatal_error 1
        "unit %s: cannot find %s.js in `%s', stopping.\n"
        name name (match origin with `Other _ -> "." | `Dir d -> d / "/")
    else
    { u_name = name; u_available = available; u_deps = deps; u_origin = origin;
      u_flags = flags; u_kind = `OCaml; u_has = has; u_container = None; }

  let pack ?(available = As_features.true_) ?(flags = As_flags.empty) name units =
    let pack =
      Dir.create
        (`Other ("pack-" ^ name))
        (List.map (fun u -> `Unit u) units)
    in
    let units =
      Dir.contents pack
      |> Component.(filter unit)
    in
    let has = function `Cmo | `Cmx -> true | _ -> false in
    let actions =
      let cmos =
        List.filter (Unit.has `Cmo) units
        |> List.map (fun u -> `N (`Unit u, `Cmo))
      in
      let cmxs =
        List.filter (Unit.has `Cmx) units
        |> List.map (fun u -> `N (`Unit u, `Cmx))
      in
      [ As_action.rule
          ~phase:(`Compile `Byte)
          ~targets:[(`Self `Cmo)]
          ~prereqs:cmos
          (fun t r f ->
             let dir = Component.build_dir t r in
             let cmos = Rule.files t r cmos in
             As_action.create ~dir "%s -pack %s %s -o %s"
               (As_resolver.ocamlc r)
               (String.concat " " (As_flags.get (`Compile `Byte) f))
               (String.concat " " cmos)
               name)
      ; As_action.rule
          ~phase:(`Compile `Native)
          ~targets:[(`Self `Cmx)]
          ~prereqs:cmxs
          (fun t r f ->
             let dir = Component.build_dir t r in
             let cmxs = Rule.files t r cmxs in
             As_action.create ~dir "%s -pack %s %s -o %s"
               (As_resolver.ocamlopt r)
               (String.concat " " (As_flags.get (`Compile `Native) f))
               (String.concat " " cmxs)
               name) ]
    in
    let origin = `Other (Other.create ~deps:[`Dir pack] name actions) in
    { u_name = name; u_available = available; u_flags = flags;
      u_origin = origin;
      u_deps = []; u_kind = `OCaml; u_has = has; u_container = None; }

  let generated_files t =
    match t.u_kind with
    | `OCaml ->  [
        As_features.byte  , [`Cmi; `Cmo];
        As_features.native, [`Cmi; `O; `Cmx];
        As_features.annot , [`Cmt; `Cmti];
      ]
    | `C -> [ As_features.true_, [`So; `So] ]
    | `Js -> []

  let _string_of_kind = function
  | `C -> "c"
  | `OCaml -> "ocaml"
  | `Js -> "js"

  let js_rules _t =
    [ Rule.link `Js ]

  let c_rules _t =
    [ Rule.link `C
    ; As_action.rule
        ~phase:(`Compile `C)
        ~targets:[`Self `O]
        ~prereqs:[`Self `C]
        (fun t r f ->
           let file = Component.file t r `C in
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
           let file = Component.file t r `So in
           As_action.create "%s -o %s %s %s"
             (As_resolver.ocamlmklib r)
             (Filename.chop_extension file)
             (Component.file t r `O)
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
           (* FIXME: how to dump the parsetree without using camlp4? *)
           As_action.create "%s %s %s > %s"
             (As_resolver.camlp4o r)
             (String.concat " " (As_flags.get (`Pp mode) f))
             (Component.file t r x)
             (Component.file t r y))
    in
    let ocamldep x =
      let ocaml_files = match container t with
      | None   -> []
      | Some c ->
          let deps =
            Component.closure ~link:true (Container.deps ~all:true c)
            |> conmap (function
              | `Lib _ as c -> Component.contents c
              | c -> [c]
              )
            |> Component.(filter unit)
          in
          let contents = Component.(filter unit) (Container.contents c) in
          let units = deps @ contents in
          let mls =
            List.filter (Unit.has `Ml) units
            |> List.map (fun u -> `N (`Unit u, `Ml))
          in
          let mlis =
            List.filter (Unit.has `Mli) units
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
             k (Component.file t r y)
             (Component.file t r (`Dep x)))
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
               (Component.file t r (ext `Mli `Byte)))])
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
               (Component.file t r (ext `Ml `Byte)));
        As_action.rule
          ~phase:(`Compile `Native)
          ~targets:[`Self `Cmx]
          ~prereqs:[`Self (`Dep `Ml); `Self (ext `Mli `Byte); `Self (ext `Ml `Native)]
          (fun t r f ->
             As_action.create "%s -c %s -impl %s"
               (As_resolver.ocamlopt r)
               (String.concat " " (As_flags.get (`Compile `Native) f))
               (Component.file t r (ext `Ml `Native)))])

  let rules t =
    (match container t with
     | None   -> [Rule.mkdir]
     | Some _ -> [])
    @ match kind t with
    | `C     -> c_rules t
    | `Js    -> js_rules t
    | `OCaml -> ocaml_rules t

  let pp_flags mode deps r =
    let libs = Component.(filter lib_ocaml_pp) deps in
    let pkgs = Component.(filter pkg_ocaml_pp) deps in
    match libs, pkgs with
    | [], [] -> []
    | _ , _  ->
        let libs = List.map (fun l ->
            sprintf "%s/%s" (Component.build_dir (`Lib l) r)
              (match mode with
               | `Byte   -> Component.file (`Lib l) r `Cma
               | `Native -> Component.file (`Lib l) r `Cmxa)
          ) libs in
        let pkgs =
          let pkgs = List.map Pkg.name pkgs in
          let pkgs = As_resolver.pkgs r pkgs in
          match mode with
          | `Byte   -> As_flags.get (`Pp `Byte) pkgs
          | `Native -> As_flags.get (`Pp `Native) pkgs in
        pkgs @ libs

  let comp_flags mode deps build_dir r =
    let units =
      Component.(filter unit_ocaml) deps
      |> List.map (fun u -> Component.build_dir (`Unit u) r) in
    let libs =
      Component.(filter lib_ocaml) deps
      |> List.map (fun l -> Component.build_dir (`Lib l) r) in
    let includes =
      (* We need to keep the -I flags in the right order *)
      let iflags inc acc = sprintf "-I %s" inc :: acc in
      let add (seen, acc) i =
        if StringSet.mem i seen then (seen, acc)
        else (StringSet.add i seen, iflags i acc) in
      let (_, incs) =
        List.fold_left add (StringSet.singleton build_dir, []) (units @ libs) in
      List.rev incs
    in
    let pkgs = match Component.(filter pkg_ocaml) deps with
    | [] -> []
    | pkgs ->
        let pkgs = List.map Pkg.name pkgs in
        let pkgs = As_resolver.pkgs r pkgs in
        match mode with
        | `Byte   -> As_flags.get (`Compile `Byte) pkgs
        | `Native -> As_flags.get (`Compile `Native) pkgs
    in
    match includes with
    | [] -> pkgs
    | _  -> pkgs @ [String.concat " " includes]

  let comp_flags deps ~build_dir r =
    let byte = comp_flags `Byte deps build_dir r in
    let native = comp_flags `Native deps build_dir r in
    let open As_flags in
    v `Dep byte @@@
    v (`Compile `Byte) byte @@@
    v (`Compile `Native) native @@@
    v (`Pp `Byte) (pp_flags `Byte deps r) @@@
    v (`Pp `Native) (pp_flags `Native deps r)

  let flags t r =
    let deps = deps t |> Component.closure ~link:true in
    let build_dir = Component.build_dir (`Unit t) r in
    let open As_flags in
    t.u_flags @@@ comp_flags deps ~build_dir r

end

and Other : sig
  include Component_ext with type t = other
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list -> string ->
    component As_action.rule list -> t
  val empty: t
  val self_targets : t -> As_action.file list
end = struct
  type t = other

  let name t = t.o_name
  let id t = "other-" ^ name t
  let available g = g.o_available
  let deps t = t.o_deps
  let with_deps o_deps t = { t with o_deps }
  let container t = t.o_container
  let with_container c o = { o with o_container = Some c }
  let contents t = []
  let flags t _ = t.o_flags
  let source_dir _ = None
  let rules t = t.o_actions

  let create ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(deps = []) o_name o_actions
    =
    { o_name; o_available = available; o_flags = flags; o_deps = deps;
      o_actions; o_container = None; }

  let empty = create "empty" []

  let self_targets t =
    let aux acc r =
      List.fold_left (fun acc -> function
        | `Self s -> As_action.FileSet.add s acc
        | _ -> acc
        ) acc r.As_action.targets
    in
    List.fold_left aux As_action.FileSet.empty t.o_actions
    |> As_action.FileSet.to_list

  (* the files are supposed to be taken into account by the component
     it is part of. *)
  let generated_files t = []

end

and Pkg: sig
  include Component_ext with type t = pkg
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

  let name t = t.p_name
  let id t = "pkg-" ^ name t
  let available t = t.p_available
  let flags t r = As_flags.(As_resolver.pkgs r [t.p_name] @@@ t.p_flags)
  let deps _t = []
  let with_deps _ t = t
  let container _t = None
  let with_container _ t = t
  let source_dir t = None (* FIXME: look into libdir *)
  let contents _t = []
  let generated_files _t = []
  let kind t = t.p_kind
  let rules _t = []

  (* Builtin packages *)

  let compiler_libs_toplevel = create "compiler-libs.toplevel" `OCaml
  let ctypes_stub = create "ctypes.stubs" `OCaml
end

and Lib : sig
  include Component_ext with type t = lib
  type kind = [ `OCaml | `OCaml_pp ]
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:Component.t list -> ?pack:bool ->
    string -> kind ->
    [`Units of [`Unit of Unit.t] list | `Other of other] -> t
  val kind : t -> kind
  val units : t -> comp_unit list
end = struct
  type kind = [ `OCaml | `OCaml_pp ]
  type t = lib

  let name t = t.l_name
  let id t = "lib-" ^ name t
  let kind t = t.l_kind
  let deps t = t.l_deps
  let with_deps l_deps t = { t with l_deps }
  let container t = t.l_container
  let with_container c t = { t with l_container = Some c }
  let available t = t.l_available
  let source_dir _t = None
  let units t = match t.l_origin with `Units us -> us | _ -> []
  let contents t = List.map (fun u -> `Unit u) (units t)

  let create ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(deps = []) ?(pack = false) name kind origin
    =
    let c =
      { c_name = name; c_id = "lib-" ^ name; c_flags = flags;
        c_available = available; c_deps = deps; c_container = None;
        c_contents = []; }
    in
    let origin, available = match origin with
    | `Other o  ->
        let files = Other.self_targets o in
        let cma = List.mem `Cma files in
        let cmxa = List.mem `Cmxa files in
        let cmxs = List.mem `Cmxs files in
        if not cma && not cmxa && not cmxs then
          As_shell.fatal_error 1
            "No rule to generate the files of the %s."
            name
        else
        let available =
          As_features.(available
                       &&& (if not cma then not_ byte else true_)
                       &&& (if not cmxa then not_ native else true_)
                       &&& (if not cmxs then not_ native_dynlink else true_))
        in
        `Other o, available
    | `Units us ->
        let us = List.map (function `Unit u -> u) us in
        let us = if pack then [Unit.pack name us] else us in
        let c = { c with c_contents = List.map (fun u -> `Unit u) us } in
        let us = Unit.map (Unit.with_container c) us in
        `Units us, available
    in
    { l_name = name; l_available = available; l_kind = kind;
      l_origin = origin; l_deps = deps;
      l_flags = flags; l_container = None; }

  let flags t r =
    let us = units t in
    let build_dir = Component.build_dir (`Lib t) r in
    let incl = [sprintf "-I %s" build_dir] in
    let cmo_deps =
      List.filter (Unit.has `Cmo) us
      |> List.map (fun u -> Component.file (`Unit u) r `Cmo)
    in
    let cmx_deps =
      List.filter (Unit.has `Cmx) us
      |> List.map (fun u -> Component.file (`Unit u) r `Cmx)
    in
    let comp = Unit.comp_flags (deps t) ~build_dir r in
    let open As_flags in
    t.l_flags @@@ comp @@@
    v `Dep incl @@@
    v (`Compile `Byte) incl @@@
    v (`Compile `Native) incl @@@
    v (`Archive `Byte) cmo_deps @@@
    v (`Archive `Native) cmx_deps @@@
    v (`Archive `Shared) cmx_deps

  let generated_files t = [
    As_features.(byte           &&& t.l_available), [`Cma];
    As_features.(native         &&& t.l_available), [`Cmxa; `A];
    As_features.(native_dynlink &&& t.l_available), [`Cmxs];
  ] @ match t.l_origin with
    | `Units us -> conmap Unit.generated_files us
    | `Other _  -> []

  let rules t = match t.l_origin with
  | `Other _  -> []
  | `Units us ->
      let cmo_deps =
        List.filter (Unit.has `Cmo) us
        |> List.map (fun u -> `N (`Unit u, `Cmo))
      in
      let cmx_deps =
        List.filter (Unit.has `Cmx) us
        |> List.map (fun u -> `N (`Unit u, `Cmx))
      in
      let byte =
        As_action.rule
          ~phase:(`Archive `Byte)
          ~targets:[`Self `Cma]
          ~prereqs:(`Self `Dir :: cmo_deps)
          (fun t r f ->
             As_action.create "%s -a %s -o %s"
               (As_resolver.ocamlc r)
               (String.concat " " (As_flags.get (`Archive `Byte) f))
               (Component.file t r `Cma))
      in
      let native mode =
        let ext = match mode with `Shared -> `Cmxs | `Native -> `Cmxa in
        let exts = match mode with `Shared -> [`Cmxs] | `Native -> [`Cmxa;`A] in
        let phase = match mode with
        | `Shared -> `Archive `Shared
        | `Native -> `Archive `Native
        in
        As_action.rule
          ~phase
          ~targets:(List.map (fun x -> `Self x) exts)
          ~prereqs:(`Self `Dir :: cmx_deps)
          (fun t r f ->
             As_action.create "%s %s %s -o %s"
               (As_resolver.ocamlopt r)
               (match mode with `Shared -> "-shared -linkall"
                              | `Native -> "-a")
               (String.concat " " (As_flags.get phase f))
               (Component.file t r ext))
      in
      [ Rule.mkdir; byte; native `Native; native `Shared ]

end

and Bin: sig
  include Component_ext with type t = bin
  val create :
    ?available:As_features.t ->
    ?flags:As_flags.t ->
    ?deps:Component.t list ->
    ?byte:bool ->
    ?native:bool ->
    ?js: bool ->
    ?link_all:bool ->
    ?install:bool ->
    string ->
    [`Units of [`Unit of Unit.t] list | `Other of other] -> t
  val toplevel:
    ?available:As_features.t ->
    ?flags:As_flags.t ->
    ?deps:Component.t list ->
    ?custom:bool ->
    ?install:bool ->
    string ->
    [`Units of [`Unit of Unit.t ] list | `Other of other] -> t
  val js: t -> bool
  val available: t -> As_features.t
  val is_toplevel: t -> bool
  val install: t -> bool
end = struct
  type t = bin

  let name t = t.b_name
  let id t = "bin-" ^ name t
  let container t = t.b_container
  let with_container c t = { t with b_container = Some c }
  let is_toplevel t = t.b_toplevel
  let install t = t.b_install
  let available t = t.b_available
  let js t = t.b_js
  let deps t = t.b_deps
  let with_deps b_deps t = { t with b_deps }
  let source_dir _t = None
  let units t = match t.b_origin with `Units us -> us | _ -> []
  let contents t = List.map (fun u -> `Unit u) (units t)

  let create ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(deps = [])
      ?(byte = true) ?(native = true) ?(js = false)
      ?(link_all = false) ?(install = true)
      name origin
    =
    let available =
      let n = native and b = byte and j = js in
      As_features.(available &&&
                   (if not n then not_ native else true_) &&&
                   (if not b then not_ byte else true_) &&&
                   (if not j then not_ js else true_))
    in
    let flags =
      if link_all then As_flags.(linkall @@@ flags) else flags
    in
    let c =
      { c_name = name; c_id = "bin-" ^ name; c_flags = flags;
        c_deps = deps; c_available = available; c_container = None;
        c_contents = []; }
    in
    let origin, available = match origin with
    | `Units us ->
        let us = List.map (function `Unit u -> u) us in
        let c = { c with c_contents = List.map (fun u -> `Unit u) us } in
        `Units (Unit.map (Unit.with_container c) us), available
    | `Other o  ->
        let files = Other.self_targets o in
        let b = List.mem `Byte files in
        let n = List.mem `Native files in
        let j = List.mem `Js files in
        `Other o,
        As_features.(available &&&
                     (if not n then not_ native else true_) &&&
                     (if not b then not_ byte else true_) &&&
                     (if not j then not_ js else true_))
    in
    { b_name = name; b_available = available; b_flags = flags; b_deps = deps;
      b_container = None; b_origin = origin; b_toplevel = false;
      b_install = install; b_js = js }

  let toplevel ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(deps = []) ?(custom = false) ?install name comps
    =
    let available = As_features.(not_ native &&& available) in
    let deps = `Pkg (Pkg.compiler_libs_toplevel) :: deps in
    let link_byte = [
      (if custom then "-custom " else "") ^ "-I +compiler-libs topstart.cmo"
    ] in
    let nflags = As_flags.v (`Link `Byte) link_byte in
    let flags = As_flags.(flags @@@ nflags) in
    let t = create ~available ~flags ~link_all:true ~deps ?install name comps in
    { t with b_toplevel = true }

  let generated_files t = [
    As_features.(byte   &&& t.b_available), [`Byte];
    As_features.(native &&& t.b_available), [`Native];
    As_features.(js     &&& t.b_available), [`Js];
  ]

  let link_flags mode t r = match t.b_origin with
  | `Other _  -> []
  | `Units us ->
      let deps = Component.closure ~link:true (deps t @ contents t) in
      let units = List.filter (Unit.has `Cmo) us in
      let units = List.map (fun u ->
          let file = match mode with
          | `Byte   -> Component.file (`Unit u) r `Cmo
          | `Native -> Component.file (`Unit u) r `Cmx in
          sprintf "%s/%s" (Filename.dirname file) (Filename.basename file)
        ) units in
      let libs = Component.(filter lib_ocaml) deps in
      let libs = List.map (fun l ->
          let file = match mode with
          | `Byte   -> Component.file (`Lib l) r `Cma
          | `Native -> Component.file (`Lib l) r `Cmxa in
          sprintf "%s/%s" (Filename.dirname file) (Filename.basename file)
        ) libs in
      let pkgs = Component.(filter pkg_ocaml) deps in
      let pkgs = match pkgs with
      | [] -> []
      | _  ->
          let pkgs = List.map Pkg.name pkgs in
          let pkgs = As_resolver.pkgs r pkgs in
          match mode with
          | `Byte   -> As_flags.get (`Link `Byte) pkgs
          | `Native -> As_flags.get (`Link `Native) pkgs in
      pkgs @ libs @ units

  let flags t r =
    let deps = Component.closure ~link:true (deps t) in
    let build_dir = Component.build_dir (`Bin t) r in
    let incl = [sprintf "-I %s" build_dir] in
    let comp = Unit.comp_flags deps ~build_dir r in
    let flags =
      let open As_flags in
      v `Dep incl @@@
      v (`Link `Byte) (link_flags `Byte t r) @@@
      v (`Link `Native) (link_flags `Native t r)
    in
    As_flags.(flags @@@ comp @@@ t.b_flags)

  let rules t =
    let deps = Component.deps (`Bin t) @ Component.contents (`Bin t) in
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
             (Component.file t r `Byte))
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
             (Component.file t r `Native))
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
             (Component.file t r `Js))
    in
    [ Rule.mkdir; byte; native; js ]

end

and Dir : sig
  include Component_ext with type t = dir
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list -> ?install:bool ->
    [ `Lib | `Bin | `Sbin | `Toplevel | `Share | `Share_root | `Etc | `Doc
    | `Misc | `Stublibs | `Man | `Other of string ] -> component list -> dir
  val dirname: t -> dirname
end = struct
  type t = dir

  let dirname t = t.d_name
  let name d = string_of_dirname d.d_name
  let id d = "dir-" ^ name d
  let available d = d.d_available
  let flags d _ = d.d_flags
  let deps d = d.d_deps
  let with_deps d_deps t = { t with d_deps }
  let contents d = d.d_contents
  let container d = d.d_container
  let with_container c d = { d with d_container = Some c }
  let source_dir _t = None
  let rules t = [Rule.mkdir]

  let create ?(available = As_features.true_) ?(flags = As_flags.empty)
      ?(deps = []) ?(install = true) dirname contents =
    let name = string_of_dirname dirname in
    let c =
      { c_name = name; c_id = "dir-" ^ name; c_flags = flags;
        c_deps = deps; c_available = available; c_container = None;
        c_contents = contents; }
    in
    let contents = Component.map (Component.with_container c) contents in
    { d_name = dirname; d_available = available; d_flags = flags; d_deps = deps;
      d_install = install; d_contents = contents; d_container = Some c; }

  let generated_files d =
    let add c =
      let refine_avail (a, files) = As_features.(d.d_available &&& a), files in
      List.map refine_avail (Component.generated_files c)
    in
    List.flatten (List.map add d.d_contents)

end

and Test: sig
  include Component_ext with type t = test
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

  let name t = t.t_name
  let id t = "test-" ^ name t
  let available t = t.t_available
  let targets _t _r _mode _phase = []
  let flags t _ = t.t_flags
  let generated_files _t = []
  let deps t = t.t_deps
  let with_deps t_deps t = { t with t_deps }
  let container t = t.t_container
  let with_container c t = { t with t_container = Some c }
  let source_dir _t = None
  let contents _t = []

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
      t_deps = bin_deps @ deps; t_dir = dir; t_commands = cmds;
      t_container = None; }

  let commands t = t.t_commands
  let dir t = t.t_dir

  let rules t =
    let deps = List.fold_left (fun acc -> function
      | `Bin _ as c -> `N (c, `Byte) :: acc
      | _ -> acc
      ) [] (deps t) in
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
                   As_resolver.root_dir r / Component.file c r `Byte
                 in
                 As_action.create ?dir "%s %s" bin args
             ) (commands t)
           |> As_action.seq
        )]

end

and Doc: sig
  include Component_ext with type t = doc
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list -> ?install:bool ->
    string -> component list -> t
end = struct
  type t = doc
  let name t = t.doc_name
  let id t = "doc-" ^ name t

  let create ?(available=As_features.true_) ?(flags=As_flags.empty)
      ?(deps=[]) ?(install=true) name contents =
    { doc_name = name; doc_contents = contents;
      doc_available = available; doc_flags = flags;
      doc_install = install; doc_deps = deps;
      doc_container = None; }

  (* FIXME: support opamdoc *)
  let generated_files t = []
  let contents t = t.doc_contents
  let container t = t.doc_container
  let with_container c t = { t with doc_container = Some c }
  let deps t = t.doc_deps
  let with_deps doc_deps t = { t with doc_deps }
  let available t = t.doc_available
  let source_dir _t = None

  let units t =
    List.fold_left (fun acc -> function
      | `Unit u -> u :: acc
      | `Lib l -> Component.(filter unit) (Lib.contents l) @ acc
      | _ -> acc
      ) [] (contents t)
    |> List.rev

  let flags t r =
    let units =
      List.fold_left (fun acc u ->
          let build_dir = Component.build_dir (`Unit u) r in
          if List.mem build_dir acc then acc
          else build_dir :: acc
        ) [] (units t)
      |> List.rev_map (fun d -> "-I " ^ d)
    in
    let open As_flags in
    t.doc_flags @@@
    v `Doc units

  (* FIXME: support opamdoc *)
  let rules t =
    let units = units t in
    let deps = List.map (fun u -> `N (`Unit u, `Cmi)) units in
    [ Rule.mkdir
    ; As_action.rule
        ~phase:`Doc
        ~targets:[`Phony (Doc.id t)]
        ~prereqs:(`Self `Dir :: deps)
        (fun t r f ->
           let files = conmap (fun u ->
               let ml = Component.file (`Unit u) r `Ml in
               let mli = Component.file (`Unit u) r `Mli in
               if Unit.has `Mli u then [mli] else
               if Unit.has `Ml u then [ml] else
               []
             ) units
           in
           let dir = Component.build_dir t r in
           let flags = As_flags.get `Doc f in
           As_action.create "%s %s \
                            \ %s -short-functors -colorize-code -html -d %s"
             (As_resolver.ocamldoc r)
             (String.concat " " flags)
             (String.concat " " files)
             dir) ]

end

let name t = t.name
let version t = t.version
let components t = t.components

let create ?(available = As_features.true_) ?(flags = As_flags.empty)
    ?version name components =
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
  { name; version; available; flags; components; }

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
