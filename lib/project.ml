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

module rec Dep: sig
  type t
  val unit: Unit.t -> t
  val units: Unit.t list -> t list
  val get_units: t list -> Unit.t list
  val lib: Lib.t -> t
  val libs: Lib.t list -> t list
  val get_libs: t list -> Lib.t list
  val pkg: string -> t
  val pkgs: string list -> t list
  val get_pkgs: t list -> string list
  val p4o: Lib.t -> t
  val p4os: Lib.t list -> t list
  val get_p4os: t list -> Lib.t list
  val pkg_p4o: string -> t
  val pkg_p4os: string list -> t list
  val get_pkg_p4os: t list -> string list
  type custom = {
    inputs : string list;
    outputs: string list;
    recipe : string list
  }
  val custom: custom -> t
  val get_customs: t list -> custom list
  type resolver = [`Destdir of string | `Pkgs of string list] -> string
  val closure: t list -> t list
end = struct

  type custom = {
    inputs: string list;
    outputs: string list;
    recipe: string list
  }

  type t =
    | Unit of Unit.t
    | Lib of Lib.t
    | P4o of Lib.t
    | Pkg_p4o of string
    | Pkg of string
    | Custom of custom

  let unit t = Unit t
  let units = List.map unit

  let lib t = Lib t
  let libs = List.map lib

  let p4o t = P4o t
  let p4os = List.map p4o

  let pkg_p4o t = Pkg_p4o t
  let pkg_p4os = List.map pkg_p4o

  let pkg t = Pkg t
  let pkgs = List.map pkg

  let custom t = Custom t

  let get_units t =
    List.fold_left (fun acc -> function Unit t -> t :: acc | _ -> acc) [] t
    |> List.rev

  let get_libs t =
    List.fold_left (fun acc -> function Lib t -> t :: acc | _ -> acc) [] t
    |> List.rev

  let get_p4os t =
    List.fold_left (fun acc -> function P4o t -> t :: acc | _ -> acc) [] t
    |> List.rev

  let get_pkgs t =
    List.fold_left (fun acc -> function Pkg t -> t :: acc | _ -> acc) [] t
    |> List.rev

  let get_pkg_p4os t =
    List.fold_left (fun acc -> function Pkg_p4o t -> t :: acc | _ -> acc) [] t
    |> List.rev

  let get_customs t =
    List.fold_left (fun acc -> function Custom t -> t :: acc | _ -> acc) [] t
    |> List.rev

  type resolver = [`Destdir of string | `Pkgs of string list] -> string

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
          | Unit u ->
            let d' = Unit.deps u in
            aux acc (d' @ d)
          | Lib l ->
            let d' = Lib.deps l in
            aux acc (d' @ d)
          | _ -> Hashtbl.replace deps h 1; aux (h :: acc) t
        )
    in
    aux [] ts

end

and Unit: sig
  type t
  val name: t -> string
  val dir: t -> string option
  val deps: t -> Dep.t list
  val lib: t -> Lib.t option
  val build_dir: t -> string option
  val add_deps: t -> Dep.t list -> t
  val with_lib: t -> Lib.t -> t
  val with_build_dir: t -> string -> t
  val create: ?dir:string -> ?deps:Dep.t list -> string -> t
  val cmi: t -> string
  val cmo: t -> string
  val cmx: t -> string
  val o: t -> string
  val generated_files: t -> (Env.Flag.t list * string) list
  val compflags: t -> Dep.resolver -> string list
  val p4oflags: t -> Dep.resolver -> string list
end = struct

  type t = {
    dir: string option;
    build_dir: string option;
    deps: Dep.t list;
    name: string;
    lib: Lib.t option;
  }

  let dir t = t.dir

  let build_dir t = t.build_dir

  let name t = t.name

  let deps t = t.deps

  let lib t = t.lib

  let with_lib t lib =
    { t with lib = Some lib; build_dir = Some (Lib.name lib) }

  let with_build_dir t build_dir =
    { t with build_dir = Some build_dir }

  let add_deps t deps =
    { t with deps = t.deps @ deps }

  let create ?dir ?(deps=[]) name =
    { dir; lib=None; build_dir=None; deps; name }

  let p4oflags t (resolver:Dep.resolver) =
    let local = Dep.get_p4os (Unit.deps t) in
    let global = Dep.get_pkg_p4os (Unit.deps t) in
    match local, global with
    | []   , []     -> []
    | local, global ->
      let local = conmap (fun l ->
          ["-I"; resolver (`Destdir (Lib.name l)); resolver (`Destdir (Lib.cma l))]
        ) local in
      let global = [
        resolver (`Pkgs global)
      ] in
      local @ global

  let compflags t (resolver:Dep.resolver) =
    let local = Dep.get_libs (Unit.deps t) in
    let local = conmap (fun l ->
        ["-I";  resolver (`Destdir (Lib.name l))]
      )local in
    let global = Dep.get_pkgs (Unit.deps t) in
    let global = match global with
      | [] -> []
      | l  -> [ resolver (`Pkgs global) ] in
    local @ global

  let file t ext =
    Unit.build_dir t // Unit.name t ^ ext

  let cmi t =
    file t ".cmi"

  let cmo t =
    file t ".cmo"

  let cmx t =
    file t ".cmx"

  let o t =
    file t ".o"

  let generated_files t = [
    []               , cmi t;
    []               , cmo t;
    [Env.Flag.native], o t;
    [Env.Flag.native], cmx t;
  ]

end

and Lib: sig
  type t
  val name: t -> string
  val filename: t -> string
  val set_filename: t -> string -> unit
  val units: t -> Unit.t list
  val flags: t -> Env.Flag.t list
  val create: ?flags:Env.Flag.t list -> ?deps:Dep.t list -> Unit.t list -> string -> t
  val cma: t -> string
  val cmxa: t -> string
  val a: t -> string
  val cmxs: t -> string
  val generated_files: t -> (Env.Flag.t list * string) list
  val deps: t -> Dep.t list
end = struct

  type t = {
    name: string;
    mutable units: Unit.t list;
    mutable filename: string;
    flags: Env.Flag.t list;
    deps : Dep.t list;
  }

  let name t = t.name

  let units t = t.units

  let filename t = t.filename

  let set_filename t f =
    t.filename <- f

  let flags t = t.flags

  let create ?(flags=[]) ?(deps=[]) units name =
    let units = List.map (fun u -> Unit.add_deps u deps) units in
    let filename = name in
    let t = { name; units; flags; filename; deps } in
    let units = List.map (fun u -> Unit.with_lib u t) units in
    t.units <- units;
    t

  let file t ext =
    Lib.name t / Lib.name t ^ ext

  let cma t =
    file t ".cma"

  let cmxa t =
    file t ".cmxa"

  let cmxs t =
    file t ".cmxs"

  let a t =
    file t ".a"

  let generated_files t = [
    t.flags                           , cma  t;
    Env.Flag.native :: t.flags        , cmxa t;
    Env.Flag.native :: t.flags        , a t;
    Env.Flag.native_dynlink :: t.flags, cmxs t
  ]
    @ conmap (fun u -> Unit.generated_files u) t.units

  let deps t =
    Lib.units t
    |> List.map Unit.deps
    |> List.concat

end

module Top = struct

  type t = {
    deps  : Dep.t list;
    name  : string;
    custom: bool;
    flags : Env.Flag.t list;
  }

  let flags t = t.flags

  let create ?(flags=[]) ?(custom=false) ?(deps=[]) name =
    { deps; flags; name; custom }

  let name t = t.name

  let deps t = t.deps

  let custom t = t.custom

  let byte t =
    t.name / t.name

  let generated_files t =
    [ [], byte t ]

end

module Bin = struct

  type t = {
    deps: Dep.t list;
    name: string;
    flags: Env.Flag.t list;
  }

  let name t = t.name

  let deps t = t.deps

  let flags t = t.flags

  let create ?(flags=[]) ?(deps=[]) name =
    { deps; flags; name }

  let byte t =
    t.name / t.name ^ ".byte"

  let native t =
    t.name / t.name ^ ".native"

  let generated_files t =
    [
      []               , byte t;
      [Env.Flag.native], native t;
    ]

end

type t = {
  name: string;
  libs: Lib.t list;
  bins: Bin.t list;
  tops: Top.t list;
}

let name t = t.name

let libs t = t.libs

let bins t = t.bins

let tops t = t.tops

let create ?(libs=[]) ?(bins=[]) ?(tops=[]) name =
  List.iter (fun l ->
      if Lib.name l <> name then
        Lib.set_filename l (name ^ "." ^ Lib.name l)
    ) libs;
  { name; libs; bins; tops }

(* dedup a list *)
let dedup l =
  let l = List.sort compare l in
  let rec aux acc = function
    | []             -> List.rev acc
    | [x]            -> List.rev (x :: acc)
    | x::(y::_ as t) ->
      if x=y then aux acc t
      else aux (x::acc) t in
  aux [] l

let flags t =
  let libs = conmap Lib.flags t.libs in
  let tops = conmap Top.flags t.tops in
  let bins = conmap Bin.flags t.bins in
  dedup (libs @ tops @ bins)
