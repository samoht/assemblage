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

let (/) x y = Filename.concat x y

let conmap f l = List.concat (List.map f l)

let (//) x y =
  match x with
  | None   -> y
  | Some x -> Filename.concat x y

module Flags = struct

  type t = {
    comp_byte  : string list -> string list;
    comp_native: string list -> string list;
    pp_byte    : string list -> string list;
    pp_native  : string list -> string list;
    link_byte  : string list -> string list;
    link_native: string list -> string list;
  }

  type f = string list -> string list

  let (@) f g =
    { comp_byte   = (fun x -> f.comp_byte   @@ g.comp_byte   x);
      comp_native = (fun x -> f.comp_native @@ g.comp_native x);
      pp_byte     = (fun x -> f.pp_byte     @@ g.pp_byte     x);
      pp_native   = (fun x -> f.pp_native   @@ g.pp_native   x);
      link_byte   = (fun x -> f.link_byte   @@ g.link_byte   x);
      link_native = (fun x -> f.link_native @@ g.link_native x);
    }

  let id x = x

  let create
      ?(comp_byte=id) ?(comp_native=id)
      ?(pp_byte=id)   ?(pp_native=id)
      ?(link_byte=id) ?(link_native=id)
      () =
    { comp_byte; comp_native;
      pp_byte; pp_native;
      link_byte; link_native }

  let empty =
    { comp_byte = id; comp_native = id;
      pp_byte = id; pp_native = id;
      link_byte = id; link_native = id }

  let comp_byte t = t.comp_byte

  let comp_native t = t.comp_native

  let pp_byte t = t.pp_byte

  let pp_native t = t.pp_native

  let link_byte t = t.link_byte

  let link_native t = t.link_native

  let debug =
    let f x = "-g" :: x in
    { empty with
      comp_byte   = f; comp_native = f;
      link_byte   = f; link_native = f;
    }

  let annot =
    let f x = "-bin-annot" :: x in
    { empty with comp_byte = f; comp_native = f }

  let linkall =
    let f x = "-linkall" :: x in
    { empty with link_byte = f; link_native = f }

  let warn_error =
    let f x = "-warn-error A-44-4 -w A-44-4" :: x in
    { empty with comp_byte = f; comp_native = f }

end

module Feature = struct

  open Cmdliner

  type t = {
    name: string;
    default: bool;
    doc: string;
  }

  type formula =
    | True | False
    | Atom of t
    | Not of formula
    | And of formula * formula
    | Or of formula * formula

  type cnf =
    [ `Conflict | `And of [ `P of t | `N of t ] list ]

  let atom t = Atom t

  module Set = Set.Make(struct
      type s = t
      type t = s
      let compare x y = String.compare x.name y.name
    end)

  let atoms t =
    let set = ref Set.empty in
    let rec aux = function
      | True
      | False     -> ()
      | Atom t    -> set := Set.add t !set
      | Not x     -> aux x
      | And (x, y)
      | Or (x, y) -> aux x; aux y in
    aux t;
    !set

  let negate: cnf -> cnf = function
    | `Conflict -> `And []
    | `And []   -> `Conflict
    | `And l    -> `And (List.map (function `P t -> `N t | `N t -> `P t) l)

  let (@) (x:cnf) (y:cnf) = match x, y with
    | `Conflict , _ | _, `Conflict -> `Conflict
    | `And []   , x | x, `And []   -> x
    | `And x , `And y  ->
      let p = Hashtbl.create (List.length x + List.length y) in
      let n = Hashtbl.create (List.length x + List.length y) in
      let add = function
        | `P x -> Hashtbl.replace p x true
        | `N x -> Hashtbl.replace n x true in
      List.iter add x;
      List.iter add y;
      try
        let ps =
          Hashtbl.fold (fun p _ acc ->
              if Hashtbl.mem n p then raise Exit
              else `P p :: acc
            ) p [] in
        let ns = Hashtbl.fold (fun n _ acc ->
            if Hashtbl.mem p n then raise Exit
            else `N n :: acc
          ) n [] in
        `And (ps @ ns)
      with Exit ->
        `Conflict

  let rec normalize: formula -> cnf = function
    | True       -> `And []
    | False      -> `Conflict
    | Atom x     -> `And [`P x]
    | Not x      -> negate (normalize x)
    | And (x, y) -> normalize x @ normalize y
    | Or (x, y)  -> normalize (Not x) @ normalize (Not y)

  let rec eval tbl = function
    | True       -> true
    | False      -> false
    | Atom t     -> (try List.assoc t tbl with Not_found -> false)
    | Not f      -> not (eval tbl f)
    | And (x, y) -> (eval tbl x) && (eval tbl y)
    | Or (x, y)  -> (eval tbl x) || (eval tbl y)

  let not f = Not f

  let true_ = True

  let false_ = False

  let (&&) x y = And (x, y)

  let (||) x y = Or (x, y)

  let name t = t.name

  let default t = t.default

  let with_default t default =
    { t with default }

  let create ~doc ~default name = { name; default; doc }

  let parse t =
    let default = if t.default then "$(b,enable)" else "$(b,disable)" in
    let enable =
      let d = Arg.info ~doc:(sprintf "Enable %s (default is %s)." t.doc default)
          ["enable-" ^ t.name] in
      Arg.(value & flag & d) in
    let disable =
      let d = Arg.info ~doc:(sprintf "Disable %s (default is %s)." t.doc default)
          ["disable-" ^ t.name] in
      Arg.(value & flag & d) in
    let create enable disable =
      let v = match enable, disable with
        | true , false -> true
        | false, true  -> false
        | false, false -> t.default
        | true , true  -> failwith "Invalid flag" in
      (t, v) in
    Term.(pure create $ enable $ disable)

  let native_t =
    create ~doc:"native code compilation." ~default:true "native"

  let native_dynlink_t =
    create ~doc:"native plugins for native code." ~default:true "native-dynlink"

  let annot_t =
    create ~doc:"generation of binary annotations." ~default:true "annot"

  let debug_t =
    create ~doc:"generation of debug symbols." ~default:true "debug"

  let warn_error_t =
    create ~doc:"warning as errors." ~default:false "warn-error"

  let test_t =
    create ~doc:"tests." ~default:false "test"

  let doc_t =
    create ~doc:"the generation of documentation." ~default:true "doc"

  let base = List.fold_left (fun set t -> Set.add t set) Set.empty [
      native_t; native_dynlink_t;
      debug_t; annot_t; warn_error_t;
      test_t; doc_t;
    ]

  let native = atom native_t
  let native_dynlink = atom native_dynlink_t
  let annot = atom annot_t
  let warn_error = atom warn_error_t
  let debug = atom debug_t
  let test = atom test_t
  let doc = atom doc_t

end

module Resolver = struct

  type t = {
    buildir: string -> string;
    pkgs   : string list -> Flags.t;
  }

  let create ~buildir ~pkgs =
    { buildir; pkgs }

  let build_dir t = t.buildir

  let pkgs t = t.pkgs

end

module Gen = struct

  type shell = {
    dir : string;
    prog: string;
    args: string list;
  }

  type bash = {
    bash_dir: string;
    bash    : string;
  }

  type t =
    | Func of (unit -> unit)
    | Shell of shell
    | Bash of bash

  let shell ~dir prog args =
    Shell { dir; prog; args }

  let func fn =
    Func fn

  let bash ~dir fmt =
    ksprintf (fun bash ->
        let bash_dir = dir in
        Bash { bash_dir; bash }
      ) fmt

  let run = function
    | Func fn -> fn ()
    | Shell s ->
      let args = String.concat " " (s.prog :: s.args) in
      Shell.in_dir s.dir (fun () -> Shell.exec "%s" args)
    | Bash b  ->
      Shell.in_dir b.bash_dir (fun () -> Shell.exec "%s" b.bash)

end

module rec Dep: sig
  type t =
    [ `Unit of Unit.t
    | `Lib of Lib.t
    | `Pp of Lib.t
    | `Bin of Bin.t
    | `Pkg_pp of string
    | `Pkg of string ]
  val id: t -> string
  module Graph: Graph.Sig.I with type V.t = t
  val unit: Unit.t -> t
  val units: Unit.t list -> t list
  val filter_units: t list -> Unit.t list
  val lib: Lib.t -> t
  val libs: Lib.t list -> t list
  val filter_libs: t list -> Lib.t list
  val pkg: string -> t
  val pkgs: string list -> t list
  val filter_pkgs: t list -> string list
  val bin: Bin.t -> t
  val bins: Bin.t list -> t list
  val pp: Lib.t -> t
  val pps: Lib.t list -> t list
  val filter_pps: t list -> Lib.t list
  val pkg_pp: string -> t
  val pkg_pps: string list -> t list
  val filter_pkg_pps: t list -> string list
  val closure: t list -> t list
  val comp_byte: t list -> string -> Resolver.t -> Flags.f
  val comp_native: t list -> string -> Resolver.t -> Flags.f
  val link_byte: t list -> Unit.t list -> Resolver.t -> Flags.f
  val link_native: t list -> Unit.t list -> Resolver.t -> Flags.f
  val pp_byte: t list -> Resolver.t -> Flags.f
  val pp_native: t list -> Resolver.t -> Flags.f
end = struct

  type t =
    [ `Unit of Unit.t
    | `Lib of Lib.t
    | `Pp of Lib.t
    | `Bin of Bin.t
    | `Pkg_pp of string
    | `Pkg of string ]

  let id = function
    | `Unit u -> Unit.id u
    | `Lib l
    | `Pp l   -> Lib.id l
    | `Bin b  -> Bin.id b
    | `Pkg_pp p
    | `Pkg p  -> "ext-" ^ p

  module V = struct
    type s = t
    type t = s
    let equal x y =
      match x, y with
      | `Unit x, `Unit y ->
        Unit.name x = Unit.name y && Unit.build_dir x = Unit.build_dir y
      | `Pp x    , `Pp y
      | `Lib x   , `Lib y -> Lib.name x = Lib.name y
      | `Pkg_pp x, `Pkg_pp y
      | `Pkg x   , `Pkg y -> x = y
      | `Bin x   , `Bin y -> Bin.name x = Bin.name y
      | _ -> false
    let compare x y =
      if equal x y then 0
      else match Hashtbl.hash x - Hashtbl.hash y with
        | 0 ->
          let n =
            String.length (Marshal.to_string x [])
            - String.length (Marshal.to_string y []) in
          if n = 0 then failwith "Dep.compare" else n
        | i -> i
    let hash = Hashtbl.hash
  end
  module Graph = Graph.Imperative.Digraph.ConcreteBidirectional(V)

  let bin t: t = `Bin t
  let bins = List.map bin

  let unit t: t = `Unit t
  let units = List.map unit

  let lib t: t = `Lib t
  let libs = List.map lib

  let pp t: t = `Pp t
  let pps = List.map pp

  let pkg_pp t: t = `Pkg_pp t
  let pkg_pps = List.map pkg_pp

  let pkg t: t = `Pkg t
  let pkgs = List.map pkg

  let filter_units t =
    List.fold_left (fun acc -> function `Unit t -> t :: acc | _ -> acc) [] t
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
          | `Unit u ->
            let d' = Unit.deps u in
            aux acc (d' @ d)
          | `Lib l ->
            let d' = Lib.deps l in
            aux acc (d' @ d)
          | _ -> Hashtbl.replace deps h 1; aux (h :: acc) t
        )
    in
    aux [] ts

  let comp_flags mode (deps:t list) incl resolver args =
    let incl = sprintf "-I %s" incl in
    let libs = filter_libs deps in
    let libs = List.map (fun l ->
        sprintf "-I %s" (Lib.build_dir l resolver);
      ) libs in
    let libs = String.concat " " (incl :: libs) in
    let pkgs = filter_pkgs deps in
    let pkgs = match pkgs with
      | [] -> []
      | _  ->
        let pkgs = Resolver.pkgs resolver pkgs in
        match mode with
        | `Byte   -> Flags.comp_byte pkgs []
        | `Native -> Flags.comp_native pkgs [] in
    pkgs @ [libs] @ args

  let comp_byte = comp_flags `Byte

  let comp_native = comp_flags `Native

  let link_flags mode (deps:t list) units resolver args =
    let units = List.map (fun u ->
        let file = match mode with
          | `Byte   -> Unit.cmo u resolver
          | `Native -> Unit.cmx u resolver in
        sprintf "%s/%s" (Filename.dirname file) (Filename.basename file)
      ) units in
    let libs = filter_libs deps in
    let libs = List.map (fun l ->
        let file = match mode with
          | `Byte   -> Lib.cma l resolver
          | `Native -> Lib.cmxa l resolver in
        sprintf "%s/%s" (Filename.dirname file) (Filename.basename file)
      ) libs in
    let pkgs = filter_pkgs deps in
    let pkgs = match pkgs with
      | [] -> []
      | _  ->
        let pkgs = Resolver.pkgs resolver pkgs in
        match mode with
        | `Byte   -> Flags.link_byte pkgs []
        | `Native -> Flags.link_native pkgs [] in
    pkgs @ libs @ units @  args

  let link_byte = link_flags `Byte

  let link_native = link_flags `Native

  let pp_flags mode (deps:t list) resolver args =
    let libs = filter_pps deps in
    let pkgs = filter_pkg_pps deps in
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
        | `Byte   -> Flags.pp_byte pkgs []
        | `Native -> Flags.pp_native pkgs [] in
      pkgs @ libs @ args

  let pp_byte = pp_flags `Byte

  let pp_native = pp_flags `Native

end

and Unit: sig
  type t
  val id: t -> string
  val copy: t -> t
  val name: t -> string
  val dir: t -> string option
  val deps: t -> Dep.t list
  val container: t -> [`Lib of Lib.t | `Bin of Bin.t] option
  val ml: t -> bool
  val mli: t -> bool
  val for_pack: t -> string option
  val add_deps: t -> Dep.t list -> unit
  val set_lib: t -> Lib.t -> unit
  val set_bin: t -> Bin.t -> unit
  val create:
    ?flags:Flags.t ->
    ?generator:(Gen.t * [`Both|`ML|`MLI]) ->
    ?dir:string ->
    ?deps:Dep.t list ->
    string -> t
  val generator: t -> (Gen.t * [`Both|`ML|`MLI]) option
  val pack: ?flags:Flags.t -> t list -> string -> t
  val unpack: t -> t list
  val cmi: t -> Resolver.t -> string
  val cmo: t -> Resolver.t -> string
  val cmx: t -> Resolver.t -> string
  val o: t -> Resolver.t -> string
  val file: t -> Resolver.t -> string -> string
  val generated_files: t -> Resolver.t -> (Feature.formula * string list) list
  val prereqs: t -> Resolver.t -> [`Byte | `Native] -> string list
  val flags: t -> Resolver.t -> Flags.t
  val build_dir: t -> Resolver.t -> string
end = struct

  type t = {
    name: string;
    dir: string option;
    mutable deps: Dep.t list;
    mutable container: [`Lib of Lib.t | `Bin of Bin.t] option;
    mutable for_pack: string option;
    mutable pack: t list;
    mutable flags: Flags.t;
    generator: (Gen.t * [`Both|`ML|`MLI]) option;
    mli: bool;
    ml: bool;
  }

  let ml t = t.ml

  let mli t = t.mli

  let generator t = t.generator

  let id t =
    match t.container with
    | None          -> t.name
    | Some (`Lib l) -> Lib.id l ^ "-" ^ t.name
    | Some (`Bin b) -> Bin.id b ^ "-" ^ t.name

  let rec copy t =
    { dir       = t.dir;
      deps      = t.deps;
      name      = t.name;
      container = t.container;
      for_pack  = t.for_pack;
      pack      = List.map copy t.pack;
      flags     = t.flags;
      ml        = t.ml;
      mli       = t.mli;
      generator = t.generator;
    }

  let dir t = t.dir

  let build_dir t resolver =
    match t.container with
    | None          -> Resolver.build_dir resolver ""
    | Some (`Lib l) -> Lib.build_dir l resolver
    | Some (`Bin b) -> Bin.build_dir b resolver

  let name t = t.name

  let deps t = t.deps

  let container t = t.container

  let for_pack t = t.for_pack

  let unpack t = t.pack

  let set_lib t lib =
    t.container <- Some (`Lib lib)

  let set_bin t bin =
    t.container <- Some (`Bin bin)

  let add_deps t deps =
    t.deps <- t.deps @ deps

  let create
      ?(flags=Flags.empty)
      ?generator
      ?dir ?(deps=[])
      name =
    let mli = match generator with
      | None      -> Sys.file_exists (dir // name ^ ".mli")
      | Some (_, `Both)
      | Some (_, `MLI) -> true
      | Some (_, `ML)  -> false in
    let ml = match generator with
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
      name; dir; deps; flags;
      container = None;
      for_pack  = None;
      pack      = [];
      ml; mli; generator;
    }

  let pack ?(flags=Flags.empty) units name =
    List.iter (fun u -> u.for_pack <- Some (String.capitalize name)) units;
    {
      name; flags;
      dir       = None;
      container = None;
      for_pack  = None;
      deps      = [];
      pack      = units;
      ml        = false;
      mli       = false;
      generator = None;
    }

  let file t r ext =
    Unit.build_dir t r / Unit.name t ^ ext

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
      Feature.true_ , [mk cmi ; mk cmo ];
      Feature.native, [mk o   ; mk cmx ];
      Feature.annot , [mk cmt ; mk cmti];
    ]

  let prereqs t resolver mode =
    let deps = Unit.deps t in
    let units = Dep.filter_units deps in
    let units = List.map (fun u ->
        match mode with
        | `Native -> cmx u resolver
        | `Byte   -> cmi u resolver
      ) units in
    let libs = Dep.filter_libs deps in
    let libs = List.map (fun l ->
        match mode with
        | `Native -> Lib.cmxa l resolver
        | `Byte   -> Lib.cma  l resolver
      ) libs in
    let pps = Dep.filter_pps deps in
    let pps = List.map (fun l -> Lib.cma l resolver) pps in
    units @ libs @ pps


  (* XXX: memoize the function *)
  let flags t resolver =
    let deps = Unit.deps t |> Dep.closure in
    let incl = Unit.build_dir t resolver in
    let comp_byte = Dep.comp_byte deps incl resolver in
    let comp_native = Dep.comp_native deps incl resolver in
    let pp_byte = Dep.pp_byte deps resolver in
    let pp_native = Dep.pp_native deps resolver in
    let t' = Flags.create ~comp_byte ~comp_native ~pp_byte ~pp_native () in
    Flags.(t' @ t.flags)

end

and Lib: sig
  type t
  val id: t -> string
  val name: t -> string
  val filename: t -> string
  val set_filename: t -> string -> unit
  val units: t -> Unit.t list
  val available: t -> Feature.formula
  val deps: t -> Dep.t list
  val create:
    ?available:Feature.formula ->
    ?flags:Flags.t ->
    ?pack:bool ->
    ?deps:Dep.t list ->
    Unit.t list -> string -> t
  val cma: t -> Resolver.t -> string
  val cmxa: t -> Resolver.t -> string
  val a: t -> Resolver.t -> string
  val cmxs: t -> Resolver.t -> string
  val file: t -> Resolver.t -> string -> string
  val generated_files: t -> Resolver.t -> (Feature.formula * string list) list
  val flags: t -> Resolver.t -> Flags.t
  val prereqs: t -> Resolver.t -> [`Byte|`Native] -> string list
  val build_dir: t -> Resolver.t -> string
end = struct

  type t = {
    name: string;
    units: Unit.t list;
    mutable filename: string;
    available: Feature.formula;
    mutable flags: Flags.t;
    deps: Dep.t list;
  }

  let name t = t.name

  let id t = "lib-" ^ t.name

  let build_dir t resolver =
    Resolver.build_dir resolver (id t)

  let units t = t.units

  let filename t = t.filename

  let set_filename t f =
    t.filename <- f

  let available t = t.available

  let create
      ?(available=Feature.true_)
      ?(flags=Flags.empty)
      ?(pack=false) ?(deps=[]) units name =
    let units' = if pack then [Unit.pack units name] else units in
    let t = { name; units = units'; available; flags; filename = name; deps } in
    List.iter (fun u -> Unit.add_deps u deps) units;
    List.iter (fun u -> Unit.set_lib u t) (units' @ units);
    t

  let file t r ext =
    Lib.build_dir t r / Lib.name t ^ ext

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
      t.available                            , [mk cma]       ;
      Feature.(native         && t.available), [mk cmxa; mk a];
      Feature.(native_dynlink && t.available), [mk cmxs]      ;
    ]
    @ conmap (fun u -> Unit.generated_files u resolver) t.units

  let deps t =
    Lib.units t
    |> List.map Unit.deps
    |> List.concat

  let flags t resolver =
    let units = Lib.units t in
    let link_byte = Dep.link_byte [] units resolver in
    let link_native = Dep.link_native [] units resolver in
    let t' = Flags.create ~link_byte ~link_native () in
    Flags.(t' @ t.flags)

  let prereqs t resolver = function
    | `Byte   ->
      List.map (fun u -> Unit.cmo u resolver) (Lib.units t)
    | `Native ->
      List.map (fun u -> Unit.cmx u resolver) (Lib.units t)

end

and Bin: sig
  type t
  val id: t -> string
  val name: t -> string
  val units: t -> Unit.t list
  val deps: t -> Dep.t list
  val create:
    ?available:Feature.formula ->
    ?byte_only:bool ->
    ?link_all:bool ->
    ?install:bool ->
    ?flags:Flags.t ->
    ?deps:Dep.t list ->
    Unit.t list -> string -> t
  val toplevel:
    ?available:Feature.formula ->
    ?flags:Flags.t ->
    ?custom:bool ->
    ?install:bool ->
    ?deps:Dep.t list ->
    Unit.t list -> string -> t
  val byte: t -> Resolver.t -> string
  val native: t -> Resolver.t -> string
  val is_toplevel: t -> bool
  val install: t -> bool
  val generated_files: t -> Resolver.t -> (Feature.formula * string list) list
  val flags: t -> Resolver.t -> Flags.t
  val prereqs: t -> Resolver.t -> [`Byte | `Native] -> string list
  val available: t -> Feature.formula
  val build_dir: t -> Resolver.t -> string
end = struct

  type t = {
    deps: Dep.t list;
    units: Unit.t list;
    name: string;
    available: Feature.formula;
    flags: Flags.t;
    toplevel: bool;
    install: bool;
  }

  let units t = t.units

  let id t = "bin-" ^ t.name

  let build_dir t resolver =
    Resolver.build_dir resolver (id t)

  let is_toplevel t = t.toplevel

  let install t = t.install

  let name t = t.name

  let deps t = t.deps

  let available t = t.available

  let create
      ?(available=Feature.true_)
      ?(byte_only=false)
      ?(link_all=false)
      ?(install=true)
      ?(flags=Flags.empty)
      ?(deps=[])
      units name =
    let available =
      if byte_only then Feature.(not native && available) else available in
    let flags =
      if link_all then Flags.(linkall @ flags) else flags in
    let t = { deps; flags; available; name; units; toplevel = false; install } in
    List.iter (fun u -> Unit.set_bin u t) units;
    t

  let toplevel
      ?(available=Feature.true_)
      ?(flags=Flags.empty)
      ?(custom=false)
      ?install
      ?(deps=[])
      units name =
    let available = Feature.(not native && available) in
    let deps = Dep.pkg "compiler-libs.toplevel" :: deps in
    let link_byte args =
      args @ [
        (if custom then "-custom " else "") ^ "-I +compiler-libs topstart.cmo"
      ] in
    let nflags = Flags.create ~link_byte () in
    let flags = Flags.(nflags @ flags) in
    let t = create ~available ~flags ~link_all:true ~deps ?install units name in
    { t with toplevel = true }

  let byte t r =
    build_dir t r / t.name ^ ".byte"

  let native t r =
    build_dir t r / t.name ^ ".opt"

  let generated_files t resolver =
    let mk f = f t resolver in
    [
      t.available                    , [mk byte  ];
      Feature.(native && t.available), [mk native];
    ]

  (* XXX: handle native pps *)
  let prereqs t resolver mode =
    let units = Bin.units t in
    let deps = deps t in
    let libs = Dep.filter_libs deps in
    let pps = Dep.filter_pps deps in
    let bytpps = List.map (fun l -> Lib.cma l resolver) pps in
    match mode with
    | `Byte   ->
      let bytlibs  = List.map (fun l -> Lib.cma  l resolver) libs in
      let bytunits = List.map (fun u -> Unit.cmo u resolver) units in
      bytpps @ bytlibs @ bytunits
    | `Native ->
      let natlibs  = List.map (fun l -> Lib.cmxa l resolver) libs in
      let natunits = List.map (fun u -> Unit.cmx u resolver) units in
      bytpps @ natlibs @ natunits

  let flags t resolver =
    let units = Bin.units t in
    let all_deps  = (Bin.deps t @ Dep.units units) |> Dep.closure in
    let incl = Bin.build_dir t resolver in
    let comp_byte = Dep.comp_byte (Bin.deps t) incl resolver in
    let comp_native = Dep.comp_native (Bin.deps t) incl resolver in
    let link_byte = Dep.link_byte all_deps units resolver in
    let link_native = Dep.link_native all_deps units resolver in
    let t' = Flags.create ~link_byte ~link_native ~comp_byte ~comp_native () in
    Flags.(t.flags @ t')

end

module Test = struct

  type t = {
    name: string;
    bin: Bin.t;
    dir: string option;
    args: string list;
  }

  let create ?dir bin args name = { name; bin; dir; args }

  let bin t = t.bin

  let dir t = t.dir

  let args t = t.args

  let id t =
    "test-" ^ t.name

end

type t = {
  name: string;
  version: string;
  flags: Flags.t;
  libs: Lib.t list;
  pps: Lib.t list;
  bins: Bin.t list;
  tests: Test.t list;
  css: string option;
  intro: string option;
  doc_dir: string;
}

let name t = t.name

let version t = t.version

let libs t = t.libs

let pps t = t.pps

let bins t = t.bins

let tests t = t.tests

let css t = t.css

let doc_dir t = t.doc_dir

let intro t = t.intro

let projects = ref []

let list () = !projects

let generators t resolver =
  let libs =
    t.libs @ t.pps
    |> Dep.libs
    |> Dep.closure
    |> Dep.filter_libs
    |> conmap Lib.units in
  let bins =
    conmap Bin.units t.bins @
    (t.bins
     |> Dep.bins
     |> Dep.closure
     |> Dep.filter_libs
     |> conmap Lib.units) in
  List.fold_left (fun acc u ->
      match Unit.generator u with
      | None        -> acc
      | Some (_, k) ->
        let ml = match k with
          | `ML | `Both -> [Unit.build_dir u resolver / Unit.name u ^ ".ml"]
          | `MLI        -> [] in
        let mli = match k with
          | `MLI | `Both -> [Unit.build_dir u resolver / Unit.name u ^ ".mli"]
          | `ML          -> [] in
        ml @ mli @ acc
    ) [] (libs @ bins)

let create
    ?(flags=Flags.empty)
    ?(libs=[]) ?(pps=[]) ?(bins=[]) ?(tests=[]) ?css ?intro ?(doc_dir="doc")
    ?(version="version-not-set")
    name =
  List.iter (fun l ->
      if Lib.name l <> name then
        Lib.set_filename l (name ^ "." ^ Lib.name l)
    ) libs;
  let t = { name; version; flags; libs; pps; bins; tests; css; intro; doc_dir } in
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
