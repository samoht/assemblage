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
open Cmdliner

let global_option_section = "COMMON OPTIONS"
let help_sections = [
  `S global_option_section;
  `P "These options are common to all commands.";
  `S "AUTHORS";
  `P "Thomas Gazagnaire <thomas@gazagnaire.org>";
  `S "BUGS";
  `P "Check bug reports at https://github.com/samoht/ocaml-tools/issues.";
]

type global = {
  mutable verbose: bool;
}

let global = {
  verbose = false;
}

let debug fmt =
  ksprintf (fun str ->
      printf "+ %s\n" str
    ) fmt

let global =
  let verbose =
    let doc =
      Arg.info ~docs:global_option_section ~doc:"Verbose mode." ["v";"verbose"] in
    Arg.(value & flag & doc) in
  let help =
    let doc =
      Arg.info ~docs:global_option_section ~doc:"Display help." ["h";"help"] in
    Arg.(value & flag & doc) in
  let create verbose help man_format =
    if help then `Help (man_format, None)
    else (
      global.verbose <- verbose;
      `Ok ()
    ) in
  Term.(ret (pure create $ verbose $ help $ Term.man_format))

let mk (fn:'a): 'a Term.t =
  Term.(pure (fun () -> fn) $ global)

module Flag = struct
  type t = {
    name: string;
    doc: string;
  }
  let name t = t.name
  let doc t = t.doc
  let create ~doc name = { name; doc }
  let parse t =
    let enable =
    let d = Arg.info ~doc:(sprintf "Enable %s" t.doc)
        ["enable-" ^ t.name] in
    Arg.(value & flag & d) in
    let disable =
      let d = Arg.info ~doc:(sprintf "Disable %s" t.doc)
          ["disable-" ^ t.name] in
      Arg.(value & flag & d) in
    let create enable disable =
      let v = match enable, disable with
        | true , false -> Some true
        | false, true  -> Some false
        | false, false -> None
        | true , true  -> failwith "Invalid flag" in
      (t, v) in
    Term.(mk create $ enable $ disable)

end

module Conf = struct
  type t = {
    native: bool;
    native_dynlink: bool;
    flags: (Flag.t * bool) list;
    comp: string list;
    bytcomp: string list;
    natcomp: string list;
    link: string list;
    bytlink: string list;
    natlink: string list;
    p4o: string list;
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
      ?(p4o=[])
      ?(destdir="_build")
      () =
    { native; native_dynlink; flags; comp; bytcomp; natcomp;
      link; bytlink; natlink; p4o; destdir }

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

  let p4o t = t.p4o

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
    p4o = [];
    destdir = "_build";
  }

  let enable t flags =
    List.for_all (fun f ->
        try List.assoc f t.flags
        with Not_found -> false
      ) flags

  let term_of_list default list =
    let aux acc h = Term.(pure (fun (f,b) t -> (f, default b) :: t) $ h $ acc) in
    List.fold_left aux (Term.pure []) list

  let parse flags =
    let default d = function
      | None   -> d
      | Some b -> b in
    let flags = term_of_list (default false) (List.map Flag.parse flags) in
    let native = Flag.(parse (create ~doc:"native code compilation." "native")) in
    let native_dynlink =
      Flag.(parse (create ~doc:"native plugins for native code." "native-dynlink")) in
    let comp =
      let doc = Arg.info
          ~doc:"Additional options passed to both the native and bytecode the \
                compilers."
          ~docv:"OPTIONS" ["comp"] in
      Arg.(value & opt (some string) None & doc) in
    let link =
      let doc = Arg.info
          ~doc:"Additional options passed to both the native and bytecode the \
                linkers."
          ~docv:"OPTIONS"["link"] in
      Arg.(value & opt (some string) None & doc) in
    let p4o =
      let doc = Arg.info
          ~doc:"Additional options passed to the camlp4o pre-processor."
          ~docv:"OPTIONS" ["p4o"] in
      Arg.(value & opt (some string) None & doc) in
    let destdir =
      let doc = Arg.info
          ~doc:"The name of the directory where built artifacts are created."
          ~docv:"DIR" ["destdir"] in
      Arg.(value & opt string "_build" & doc) in
    let list = function
      | None   -> []
      | Some l -> [l] in

    let create (_,native) (_,native_dynlink) flags comp link p4o destdir = {
      native = default true native;
      native_dynlink = default true native;
      flags;
      comp = list comp;
      bytcomp = [];
      natcomp = [];
      link = list link;
      bytlink = [];
      natlink = [];
      p4o = list p4o;
      destdir;
    } in
    Term.(mk create $ native $ native_dynlink $ flags $ comp $ link $ p4o $ destdir)

end

let (/) x y = Filename.concat x y

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
  type resolver = string list -> string list
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

  type resolver = string list -> string list

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
  val flags: t -> Flag.t list
  val lib: t -> Lib.t option
  val add_deps: t -> Dep.t list -> t
  val with_lib: t -> Lib.t -> t
  val create: ?dir:string -> ?deps:Dep.t list -> ?flags:Flag.t list -> string -> t
  val generated_files: t -> Conf.t -> string list
  val compflags: t -> Conf.t -> Dep.resolver -> string list
  val p4oflags: t -> Conf.t -> Dep.resolver -> string list
end = struct

  type t = {
    dir: string option;
    deps: Dep.t list;
    name: string;
    flags: Flag.t list;
    lib: Lib.t option;
  }

  let dir t = t.dir

  let name t = t.name

  let deps t = t.deps

  let flags t = t.flags

  let lib t = t.lib

  let with_lib t lib = { t with lib = Some lib }

  let add_deps t deps =
    { t with deps = t.deps @ deps }

  let create ?dir ?(deps=[]) ?(flags=[])name =
    { dir; lib=None; deps; name; flags }

  let p4oflags t c resolver =
    let local = Dep.get_p4os (Unit.deps t) in
    let global = Dep.get_pkg_p4os (Unit.deps t) in
    match local, global, Conf.p4o c with
    | [], [], [] -> []
    | local, global, conf ->
      let local = List.map (fun l -> Conf.destdir c / Lib.name l ^ ".cma") local in
      let global = resolver global in
      conf @ local @ global

  let compflags t conf resolver =
    let local = Dep.get_libs (Unit.deps t) in
    let local = List.fold_left (fun acc l ->
        "-I" :: Conf.destdir conf / Lib.name l :: acc
      ) [] local in
    let global = Dep.get_pkgs (Unit.deps t) in
    let global = match global with
      | [] -> []
      | l  -> resolver global in
    let conf = Conf.comp conf in
    conf @ local @ global

  let generated_files t conf =
    if Conf.enable conf t.flags then
      let file ext = match t.lib with
        | None   -> Conf.destdir conf / t.name ^ ext
        | Some l -> Conf.destdir conf / Lib.name l / t.name ^ ext in
      let byte = [ file ".cmi"; file ".cmo" ] in
      let native = [ file ".o"; file ".cmx" ] in
      byte
      @ (if Conf.native conf then native else [])
    else
      []

end

and Lib: sig
  type t
  val name: t -> string
  val units: t -> Unit.t list
  val create: ?flags:Flag.t list -> ?deps:Dep.t list -> Unit.t list -> string -> t
  val generated_files: t -> Conf.t -> string list
  val deps: t -> Dep.t list
end = struct

  type t = {
    name: string;
    mutable units: Unit.t list;
    flags: Flag.t list;
    deps : Dep.t list;
  }

  let name t = t.name

  let units t = t.units

  let create ?(flags=[]) ?(deps=[]) units name =
    let units = List.map (fun u -> Unit.add_deps u deps) units in
    let t = { name; units; flags; deps } in
    let units = List.map (fun u -> Unit.with_lib u t) units in
    t.units <- units;
    t

  let generated_files t conf =
    if Conf.enable conf t.flags then
      let file ext =
        Conf.destdir conf / t.name / t.name ^ ext in
      let byte = [ file ".cma" ] in
      let native = [ file ".cmxa" ] in
      let native_dynlink = [ file ".cmxs" ] in
      let units = List.concat (List.map (fun u ->
          Unit.generated_files u conf
        )  t.units) in
      byte
      @ (if Conf.native conf then native else [])
      @ (if Conf.native_dynlink conf then native_dynlink else [])
      @ units
    else
      []

  let deps t =
    Lib.units t
    |> List.map Unit.deps
    |> List.concat

end

module Top = struct

  type t = {
    libs: Lib.t list;
    name: string;
    custom: bool;
  }

  let create ?(custom=false) libs name =
    { libs; name; custom }

  let name t = t.name

  let libs t = t.libs

  let custom t = t.custom

  let generated_files t conf =
    [Conf.destdir conf / t.name / t.name]

end

module Bin = struct

  type t = {
    libs: Lib.t list;
    units: Unit.t list;
    name: string;
  }

  let name t = t.name

  let libs t = t.libs

  let units t = t.units

  let create libs units name =
    { libs; units; name }

  let generated_files t conf =
    [Conf.destdir conf / t.name / t.name]

end

type t = {
  name: string option;
  version: string option;
  libs: Lib.t list;
  bins: Bin.t list;
  tops: Top.t list;
  conf: Conf.t;
}

let version t = t.version

let name t = t.name

let with_name t name = { t with name = Some name }

let with_version t version = { t with version = Some version }

let libs t = t.libs

let bins t = t.bins

let tops t = t.tops

let conf t = t.conf

let parse ?version flags =
  let doc = "opam-configure - helpers to manage and configure OCaml projects." in
  let man = [
    `S "DESCRIPTION";
    `P "opam-configure is part of OCaml-tools, a collection of tools to \
        manage and configure OCaml projects.";
  ] in
  let git_version = match Git.version () with
    | None   -> ""
    | Some v -> v in
  let version = match version with
    | None   -> git_version
    | Some v -> v ^ git_version in
  let info = Term.info "opam-configure"
      ~version
      ~sdocs:global_option_section
      ~doc
      ~man in
  match Term.eval ((Conf.parse flags), info) with
  | `Ok conf -> conf
  | `Version -> failwith "version"
  | `Help    -> failwith "help"
  | `Error _ -> exit 1

let create
    ?name
    ?version
    ?flags
    ?conf
    ?(libs=[])
    ?(bins=[])
    ?(tops=[])
    () =
  let conf = match conf with
    | Some c -> c
    | None   ->
      let flags = match flags with
        | None   -> []
        | Some f -> f in
      parse ?version flags in
  { name; version; libs; bins; tops; conf }
