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

module Flag = struct
  type name = string
  type t = name * bool
  let name = fst
  let value = snd
  let create name value = (name, value)
end

module Conf = struct
  type t = {
    native: bool;
    native_dynlink: bool;
    flags: Flag.t list;
    comp: string list;
    bytcomp: string list;
    natcomp: string list;
    link: string list;
    bytlink: string list;
    natlink: string list;
    p4: string list;
    destdir: string;
  }

  let create
      ?(native=true)
      ?(native_dynlink=true)
      ?(flags=[])
      ?(comp=[])
      ?(bytcomp=[])
      ?(natcomp=[])
      ?(link=[])
      ?(bytlink=[])
      ?(natlink=[])
      ?(p4=[])
      ?(destdir="_build")
      () =
    { native; native_dynlink; flags; comp; bytcomp; natcomp;
      link; bytlink; natlink; p4; destdir }

  let destdir t = t.destdir
  let native t = t.native
  let native_dynlink t = t.native && t.native_dynlink
  let flags t = t.flags

  let comp t = t.comp
  let bytcomp t = t.bytcomp @ t.comp
  let natcomp t = t.natcomp @ t.comp

  let link t = t.link
  let bytlink t = t.bytlink @ t.link
  let natlink t = t.natlink @ t.link

  let p4 t = t.p4

  let default = {
    native = true;
    native_dynlink = true;
    flags = [];
    comp = [];
    bytcomp = [];
    natcomp = [];
    link = [];
    bytlink = [];
    natlink = [];
    p4 = [];
    destdir = "_build";
  }

  let enable t flags =
    List.for_all (fun f ->
        try List.assoc f t.flags
        with Not_found -> false
      ) flags


end

let (/) x y = Filename.concat x y

let (//) x y =
  match x with
  | None   -> y
  | Some x -> Filename.concat x y

module rec Dep: sig
  type t
  type custom = {
    inputs: string list;
    outputs: string list;
    recipe: string list
  }
  val unit: Unit.t -> t
  val lib: Lib.t -> t
  val findp4o : string -> t
  val findlib: string -> t
  val custom: custom -> t
  val units: t list -> Unit.t list
  val libs: t list -> Lib.t list
  val findp4os: t list -> string list
  val findlibs: t list -> string list
  val customs: t list -> custom list
end = struct

  type custom = {
    inputs: string list;
    outputs: string list;
    recipe: string list
  }

  type t =
    | Unit of Unit.t
    | Lib of Lib.t
    | Findp4o of string
    | Findlib of string
    | Custom of custom

  let unit t = Unit t

  let lib t = Lib t

  let findp4o t = Findp4o t

  let findlib t = Findlib t

  let custom t = Custom t

  let units t =
    List.fold_left (fun acc -> function Unit t -> t :: acc | _ -> acc) [] t
    |> List.rev

  let libs t =
    List.fold_left (fun acc -> function Lib t -> t :: acc | _ -> acc) [] t
    |> List.rev

  let findp4os t =
    List.fold_left (fun acc -> function Findp4o t -> t :: acc | _ -> acc) [] t
    |> List.rev

  let findlibs t =
    List.fold_left (fun acc -> function Findlib t -> t :: acc | _ -> acc) [] t
    |> List.rev

  let customs t =
    List.fold_left (fun acc -> function Custom t -> t :: acc | _ -> acc) [] t
    |> List.rev

end

and Unit: sig
  type t
  val name: t -> string
  val dir: t -> string option
  val deps: t -> Dep.t list
  val flags: t -> Flag.name list
  val with_dir: t -> string option -> t
  val create: ?dir:string -> ?deps:Dep.t list -> ?flags:Flag.name list -> string -> t
  val generated_files: t -> Conf.t -> string list
  val compflags: t -> Conf.t -> string list
  val p4flags: t -> Conf.t -> string list
end = struct

  type t = {
    dir: string option;
    deps: Dep.t list;
    name: string;
    flags: Flag.name list;
  }

  let dir t = t.dir

  let name t = t.name

  let deps t = t.deps

  let flags t = t.flags

  let with_dir t dir = { t with dir }

  let create ?dir ?(deps=[]) ?(flags=[])name =
    { dir; deps; name; flags }


  let p4flags t conf =
    let deps = Dep.findp4os (Unit.deps t) in
    match List.rev deps, Conf.p4 conf with
    | []  , [] -> []
    | deps, p4 ->
      let deps = String.concat " " deps in
      let deps =
        sprintf
          "$(shell ocamlfind query %s -r -predicates byte,syntax -format \"-I %%d %%a\")" deps
      in
      p4 @ [deps]

  let compflags t conf =
    let incls = Dep.libs (Unit.deps t) in
    let incls = List.fold_left (fun acc l ->
        "-I" :: Conf.destdir conf / Lib.name l :: acc
      ) [] incls in
    let libs = Dep.findlibs (Unit.deps t) @ Dep.findp4os (Unit.deps t) in
    let libs = match libs with
      | [] -> []
      | l  ->
        let libs = String.concat " "  (List.rev l) in
        [sprintf " $(shell ocamlfind query %s -r -predicates byte -format \"-I %%d\")" libs] in
    let global = Conf.comp conf in
    global @ incls @ libs

  let generated_files t conf =
    if Conf.enable conf t.flags then
      let file ext =
        Conf.destdir conf / (t.dir // t.name ^ ext) in
      let byte = [ file ".cmi"; file ".cmo" ] in
      let native = [ file ".o"; file ".cmx" ] in
      let native_dynlink = [ file ".cmxs" ] in
      byte
      @ (if Conf.native conf then native else [])
      @ (if Conf.native_dynlink conf then native_dynlink else [])
    else
      []

end

and Lib: sig
  type t
  val name: t -> string
  val units: t -> Unit.t list
  val create: ?flags:Flag.name list -> Unit.t list -> string -> t
  val generated_files: t -> Conf.t -> string list
end = struct

  type t = {
    name: string;
    units: Unit.t list;
    flags: Flag.name list;
  }

  let name t = t.name

  let units t = t.units

  let create ?(flags=[]) units name =
    { name; units; flags }

  let generated_files t conf =
    if Conf.enable conf t.flags then
      let file ext =
        Conf.destdir conf / t.name ^ ext in
      let byte = [ file ".cma" ] in
      let native = [ file ".cmxa" ] in
      let units = List.concat (List.map (fun u ->
          Unit.generated_files u conf
        )  t.units) in
      byte
      @ (if Conf.native conf then native else [])
      @ units
    else
      []

end

type t = {
  libs: Lib.t list;
  conf: Conf.t;
}

let libs t = t.libs

let conf t = t.conf

let create ~libs conf = { libs; conf }
