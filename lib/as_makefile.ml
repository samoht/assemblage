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

let (|>) x f = f x

let conmap f l = List.concat (List.map f l)

module StringSet = struct
  include Set.Make (String)
  let of_list ss = List.fold_left (fun acc s -> add s acc) empty ss
end
let dedup l = StringSet.(elements (of_list l))

module Variable = struct

  type assign = string

  type t = {
    name  : string;
    assign: assign;
    value : contents;
  }

  and guard = (t * string) list

  and contents =
    [ `String of string
    | `Strings of string list
    | `Case of (guard * contents) list ]

  let compare v1 v2 = String.compare v1.name v2.name

  let (=:=) name value: t =
    { name; value; assign = ":="; }

  let (=+=) name value: t =
    { name; value; assign = "+="; }

  let (=?=) name value: t =
    { name; value; assign = "?="; }

  let subst t name ~input ~output : t =
    { name; assign = "=";
      value = `String (sprintf "$(${%s}:%s=%s)" t.name input output);
    }

  let name t =
    sprintf "$(%s)" t.name

  let is_empty t = match t.value with
    | `String ""
    | `Strings [] -> true
    | `Strings l  -> List.for_all ((=) "") l
    | `Case _
    | `String _   -> false

  let has_feature f =
    let var = String.uppercase (As_features.name f) in
    let var = String.map (function '-' -> '_' | x -> x) var in
    ("HAS_" ^ var) =?= `String (if As_features.default f then "1" else "0")

  (* build the guard pattern *)
  let guard features =
    match As_features.cnf features with
    | `Conflict -> failwith "invalid handler case"
    | `And l    ->
      List.map (function
          | `P f -> has_feature f, "1"
          | `N f -> has_feature f, "0"
        ) l

  (* build the full handler cases *)
  let case available cs =
    let cs = List.filter (fun (f,_) ->
        As_features.(cnf (available &&& f)) <> `Conflict
      ) cs in
    `Case (List.map (fun (f, c) -> guard f, c) cs)

  type no_internal_cases = (guard * t list) list

  (* Move the guard conditions outside of the value declarations. *)
  let move_guards (t:t): no_internal_cases =
    match t.value with
    | `Case cases ->
      List.map (fun (guard, contents) ->
          guard, [{ t with value = contents }]
        ) cases
    | _ -> [[], [t]]

  let string_of_guard g =
    List.map (fun (k,v) -> k.name ^ "=" ^ v) g
    |> String.concat " "
    |> sprintf "{%s}"

  let _string_of_guards g =
    String.concat ", "
      (List.map (fun (g, _) -> string_of_guard g) g)

  let compare_guards o1 o2 =
    let mk l =
      List.map (fun (g,tl) ->
          string_of_guard g ^ ":" ^
          String.concat "-" (List.map (fun t -> t.name) tl)
        ) l
      |> String.concat "|" in
    String.compare (mk o1) (mk o2)

  let merge_guards ts =
    let ts = List.sort compare_guards ts in
    let return cur acc =
      let ret l =
        List.rev_map (List.map (fun (g, vs) -> g, List.sort compare vs)) l in
      match cur with
      | None   -> ret acc
      | Some g -> ret (g::acc) in
    let rec aux cur acc ts = match ts with
      | []     -> return cur acc
      | h :: t ->
        match cur with
        | None   -> aux (Some h) acc t
        | Some g ->
          if compare_guards g h = 0 then
            let g = List.map2 (fun (g1, v1) (_, v2) -> g1, v1 @ v2) h g in
            aux (Some g) acc t
          else
            aux None (g :: acc) ts in
    aux None [] ts

  let generate buf ~simplify ?(align=false) (ts:t list) =
    let align = if not align then 0 else
        List.fold_left (fun acc t -> max acc (String.length t.name)) 0 ts in
    let gen_string v str =
      bprintf buf "%-*s %s %s\n" align v.name v.assign str in
    let rec gen_variable v = match v.value with
      | `Case _      -> gen_case (move_guards v)
      | `String s    -> gen_string v s
      | `Strings []  -> gen_string v ""
      | `Strings [s] -> gen_string v s
      | `Strings ss  ->
        let sep = " \\\n" ^ String.make 4 ' ' in
        gen_string v (String.concat sep (""::ss))
    and gen_case = function
      | [guards, ts] ->
        (* A single case handler *)
        begin match guards with
          | [] -> List.iter gen_variable ts
          | _  ->
            bprintf buf "ifeq (";
            List.iter (fun (k, v) -> bprintf buf "$(%s:%s=)" k.name v) guards;
            bprintf buf ",)\n";
            List.iter gen_variable ts;
            bprintf buf "endif\n"
        end
      | cases ->
        (* A full case handler *)
        let rec aux = function
          | []                -> ()
          | (guards, ts) :: rest ->
            match guards with
            | [] ->
              (* we assume that's the default case hanlder *)
              if rest <> [] then failwith "invalid default case";
              List.iter gen_variable ts;
              bprintf buf "endif\n"
            | _ ->
              bprintf buf "ifeq (";
              List.iter (fun (k, v) -> bprintf buf "$(%s:%s=)" k.name v) guards;
              bprintf buf ",)\n";
              List.iter gen_variable ts;
              if List.length rest <= 1 then bprintf buf "else\n"
              else bprintf buf "else ";
              aux rest;
        in
        aux cases in
    if simplify then
      ts
      |> List.map move_guards
      |> merge_guards
      |> List.iter gen_case
    else
      List.iter gen_variable ts

  let shell name command =
    { name; assign = "=";
      value = `String (sprintf "$(shell %s)" command)
    }

  let files name ~dir ~ext =
    { name; assign = "=";
      value = `String (sprintf "$(wildcard %s/*.%s)" dir ext) }

  type stanza = {
    doc      : string list;
    align    : bool;
    simplify : bool;
    variables: t list;
  }

  let stanza ?(align=false) ?(simplify=true) ?(doc=[]) variables =
    { doc; align; simplify; variables }

end

module Rule = struct

  type t = {
    ext: bool;
    targets: string list;
    prerequisites:string list;
    order_only_prerequisites:string list;
    recipe:string list;
  }

  let create ?(ext=false) ~targets ~prereqs ?(order_only_prereqs=[]) recipe =
    { ext; targets;
      prerequisites=prereqs;
      order_only_prerequisites=order_only_prereqs;
      recipe }

  let generate buf t =
    bprintf buf "%s%s %s%s\n"
      (String.concat " " t.targets)
      (if t.ext then "::" else ":")
      (String.concat " " t.prerequisites)
      (match t.order_only_prerequisites with
       | []  -> ""
       | l   -> sprintf " | %s" (String.concat " " l));
    let () = match t.recipe with
      | [] -> bprintf buf "\t@\n"
      | l  -> List.iter (bprintf buf "\t%s\n") l
    in
    bprintf buf "\n"

  let target = "$@"
  let target_member = "$%"
  let prereq = "$<"
  let changed_prereqs = "$?"
  let prereqs = "$^"
  let dedup_prereqs = "$+"
  let stem = "$*"

end

type t = {
  makefile: string;
  header: string list;
  phony: string list;
  variables: Variable.stanza list;
  rules: Rule.t list;
}

let create ?(header=[]) ?(phony=[]) makefile variables rules =
  { makefile; phony; header; variables; rules }

let write t =
  printf "\027[36m+ write %s\027[m\n" t.makefile;
  let buf = Buffer.create 1024 in
  bprintf buf "# Generated by Assemblage.\n";
  bprintf buf "# Run `make help' to get the list of targets.\n\n";
  List.iter (fun s ->
      Buffer.add_string buf s;
      Buffer.add_string buf "\n\n";
    ) t.header;
  let () = match t.phony with
    | [] -> ()
    | l  -> bprintf buf ".PHONY: %s\n\n" (String.concat " " l)
  in
  List.iter (fun { Variable.doc; align; simplify; variables } ->
      List.iter (bprintf buf "# %s\n") doc;
      Variable.generate buf ~simplify ~align variables;
      if doc <> [] || variables <> [] then bprintf buf "\n";
    ) t.variables;
  bprintf buf "\n";
  List.iter (Rule.generate buf) t.rules;
  bprintf buf "-include Makefile.assemble\n\n";
  let oc = open_out t.makefile in
  output_string oc (Buffer.contents buf);
  close_out oc

(******************************************************************************)

let echo_prereqs name =
  sprintf "@echo '%s %s %s'"
    (As_shell.color `underline name)
    (As_shell.color `yellow "=>")
    Rule.prereqs

let resolver =
  As_ocamlfind.resolver `Makefile
    ~ocamlc:"$(OCAMLC)"
    ~ocamlopt:"$(OCAMLOPT)"
    ~build_dir:"$(BUILDIR)"

let native_dynlink_f = As_features.(native_dynlink &&& native)

let byte_f = As_features.byte
let native_f = As_features.native
let js_f = As_features.js

let lib_dir =
  lazy (List.hd (As_shell.exec_output "ocamlfind printconf destdir"))

let comp_byte   = "comp-byte"
let comp_native = "comp-native"
let comp_c      = "comp-c"
let link_byte   = "link-byte"
let link_native = "link-native"
let link_shared = "link-shared"
let pp_byte     = "pp-byte"
let pp_native   = "pp-native"
let deps_byte   = "deps-byte"
let deps_native = "deps-native"
let deps_shared = "deps-shared"
let deps_c      = "deps-c"
let deps_js     = "deps-js"

module rec U: sig
  val rules    : As_project.Unit.t -> Rule.t list
  val variables: As_project.Unit.t -> Variable.stanza
end = struct

  let pp suffix varlib varbin fn t =
    let var = As_project.Unit.id t ^ "." ^ suffix in
    let lib = match As_project.Unit.container t with
    | None          -> None
    | Some (`Lib l) -> Some (varlib l)
    | Some (`Bin b) -> Some (varbin b)
    | Some (`Dir _) -> None
    in
    match lib, fn (As_project.Unit.flags t resolver) with
    | None  , [] -> None
    | Some l, [] -> if Variable.is_empty l then None else Some l
    | None  , u  -> Some (Variable.(var =?= `Strings u))
    | Some l, u  ->
      if Variable.is_empty l then
        Some (Variable.(var =?= `Strings u))
      else
        Some (Variable.(var =?= `Strings (Variable.name l :: u)))

  let flag suffix varlib varbin fn t =
    let var    = As_project.Unit.id t ^ "." ^ suffix in
    let global = match As_project.Unit.container t with
    | None          -> []
    | Some (`Lib l) -> [Variable.name (varlib l)]
    | Some (`Bin b) -> [Variable.name (varbin b)]
    | Some (`Dir _) -> []
    in
    let flags = fn (As_project.Unit.flags t resolver) @ global in
    Variable.(var =?= `Strings flags)

  let pp_byte =
    pp pp_byte L.pp_byte B.pp_byte As_flags.(get `Pp `Byte)

  let pp_native =
    pp pp_native L.pp_native B.pp_native As_flags.(get `Pp `Native)

  let comp_byte =
    flag comp_byte L.comp_byte B.comp_byte As_flags.(get `Compile `Byte)

  let comp_native =
    flag comp_native L.comp_native B.comp_native As_flags.(get `Compile `Native)

  let comp_c =
    flag comp_c L.comp_c B.comp_c As_flags.(get `Compile `C)

  let prereqs t = function
  | `Byte   -> As_project.Unit.id t ^ "." ^ deps_byte
  | `Native -> As_project.Unit.id t ^ "." ^ deps_native
  | `C      -> As_project.Unit.id t ^ "." ^ deps_c
  | `Js     -> As_project.Unit.id t ^ "." ^ deps_js

  let prereqs_var t mode =
    sprintf "$(%s)" (prereqs t mode)

  let variables t =
    let name = match As_project.Unit.dir t with
    | None   -> As_project.Unit.name t
    | Some d -> d / As_project.Unit.name t in
    match As_project.Unit.kind t with
    | `OCaml ->
        Variable.stanza
          ~doc:[sprintf "Compilation unit: %s" name]
          (Variable.(prereqs t `Byte =?=
                     `Strings (As_project.Component.prereqs
                                 (`Unit t) resolver `Byte `Compile))
           :: Variable.(prereqs t `Native =?=
                        `Strings (As_project.Component.prereqs
                                    (`Unit t) resolver `Native `Compile))
           :: comp_byte t
           :: comp_native t
           :: (match pp_byte t with
             | None   -> []
             | Some v -> [v])
           @  (match pp_native t with
             | None   -> []
             | Some v -> [v]))
    | `C ->
        Variable.stanza
          ~doc:[sprintf "C file: %s" name]
          [Variable.(prereqs t `C =?=
                     `Strings (As_project.Component.prereqs
                                 (`Unit t) resolver `C `Compile));
           comp_c t]
    | `Js ->
        Variable.stanza
          ~doc:[sprintf "JS file: %s" name]
          [Variable.(prereqs t `Js =?=
                     `Strings (As_project.Component.prereqs
                                 (`Unit t) resolver `Js `Compile))]

  (* XXX: handle native pp *)
  let rec rules t =
    match As_project.Unit.kind t with
    | `Js -> []
    | `C  ->
        let source = As_project.Unit.source t ".c" in
        let symlink = As_project.Unit.c t resolver in
        [ Rule.create
            ~targets:[As_project.Unit.id t]
            ~prereqs:[As_project.Unit.dll_so t resolver]
            [];
          Rule.create
            ~targets:[symlink]
            ~prereqs:[source]
            [sprintf "mkdir -p %s" (Filename.dirname symlink);
             sprintf "ln -sf $(shell pwd)/%s %s" source symlink];
          Rule.create
            ~targets:[As_project.Unit.o t resolver]
            ~prereqs:[symlink; prereqs t `C]
            [sprintf "cd %s && $(OCAMLC) -c -I %s %s %s"
               (Filename.dirname symlink)
               (Lazy.force lib_dir)
               (Filename.basename symlink)
               (Variable.name (comp_c t))];
          Rule.create
            ~targets:[As_project.Unit.dll_so t resolver]
            ~prereqs:[As_project.Unit.o t resolver]
            [sprintf "$(OCAMLMKLIB) -o %s %s %s"
               (As_project.Unit.file t resolver "")
               Rule.prereqs
               (Variable.name (comp_c t))]
        ];
    | `OCaml ->
        let pp = match pp_byte t with
        | None   -> ""
        | Some v -> sprintf "-pp '$(CAMLP4O) %s' " (Variable.name v) in
        let for_pack = match As_project.Unit.for_pack t with
        | None   -> ""
        | Some p -> sprintf "-for-pack %s " p in
        let flags = for_pack ^ pp in
        let target ext = As_project.Unit.file t resolver ext in
        let source ext = As_project.Unit.source t ext in
        match As_project.Unit.unpack t with
        | [] -> (* Normal compilation unit. *)
            let ln = (* link source file to target directory *)
              match As_project.Unit.generated t with
              | false ->
                  let aux exists ext =
                    let source = source ext in
                    let target = target ext in
                    if exists t then
                      [Rule.create ~targets:[target] ~prereqs:[source] [
                          sprintf "mkdir -p %s" (As_project.Unit.build_dir t resolver);
                          sprintf "ln -sf $(shell pwd)/%s %s" source target
                        ]]
                    else [] in
                  aux As_project.Unit.(has `Ml) ".ml"
                  @ aux As_project.Unit.(has `Mli) ".mli"
              | true -> [] in
            let cmi = (* generate cmis *)
              let targets, prereqs =
                if As_project.Unit.has `Mli t then [target ".cmi"], [target ".mli"]
                else if As_project.Unit.has `Ml t then [target ".cmo"; target ".cmi"],
                                                       [target ".ml"]
                else [], [] in
              [Rule.create ~targets ~prereqs:(prereqs @ [prereqs_var t `Byte]) [
                  sprintf "$(OCAMLC) -c %s%s %s"
                    flags (Variable.name (comp_byte t)) Rule.prereq
                ]] in
            let cmo = (* Generate cmos *)
              if As_project.Unit.has `Mli t && As_project.Unit.has `Ml t then
                [Rule.create ~targets:[target ".cmo"]
                   ~prereqs:[target ".ml"; target ".cmi"; prereqs_var t `Byte]
                   [sprintf "$(OCAMLC) -c %s%s %s"
                      flags (Variable.name (comp_byte t)) Rule.prereq]]
              else
              [] in
            let cmx = (* Generate cmxs *)
              if As_project.Unit.has `Ml t then
                [Rule.create ~targets:[target ".cmx"]
                   ~prereqs:[target ".ml"; target ".cmi"; prereqs_var t `Native]
                   [sprintf "$(OCAMLOPT) -c %s%s %s"
                      flags (Variable.name (comp_native t)) Rule.prereq]]
              else
              []
            in
            ln @ cmi @ cmo @ cmx
        | units -> (* Packed units *)
            conmap rules units

end

and L: sig
  val rules      : As_project.Lib.t -> Rule.t list
  val variables  : As_project.Lib.t -> Variable.stanza list
  val comp_byte  : As_project.Lib.t -> Variable.t
  val comp_native: As_project.Lib.t -> Variable.t
  val comp_c     : As_project.Lib.t -> Variable.t
  val pp_byte    : As_project.Lib.t -> Variable.t
  val pp_native  : As_project.Lib.t -> Variable.t
end = struct

  let flag suffix with_glob fn t =
    let var   = As_project.Lib.id t ^ "." ^ suffix in
    let glob  = sprintf "$(%s)" suffix in
    let flags = fn (As_project.Lib.flags t resolver) in
    if with_glob then
      Variable.(var =?= `Strings (glob :: flags))
    else
      Variable.(var =?= `Strings flags)

  let pp_byte = flag pp_byte false As_flags.(get `Pp `Byte)
  let pp_native = flag pp_native false As_flags.(get `Pp `Native)
  let comp_byte = flag comp_byte true  As_flags.(get `Compile `Byte)
  let comp_native = flag comp_native true  As_flags.(get `Compile `Native)
  let comp_c = flag comp_c true As_flags.(get `Compile `C)
  let link_byte = flag link_byte true  As_flags.(get `Link `Byte)
  let link_native = flag link_native true  As_flags.(get `Link `Native)
  let link_shared = flag link_shared true As_flags.(get `Link `Shared)

  let prereqs t = function
    | `Byte   -> As_project.Lib.id t ^ "." ^ deps_byte
    | `Native -> As_project.Lib.id t ^ "." ^ deps_native
    | `Shared -> As_project.Lib.id t ^ "." ^ deps_shared

  let prereqs_var t mode =
    sprintf "$(%s)" (prereqs t mode)

  let variables t =
    let vars =
      let cma  = As_project.Lib.cma t resolver in
      let cmxa = As_project.Lib.cmxa t resolver in
      let cmxs = As_project.Lib.cmxs t resolver in
      let byte = [ byte_f, `Strings [cma] ] in
      let native = [ native_f, `Strings [cma; cmxa] ] in
      let native_dynlink = [ native_dynlink_f, `Strings [cmxs] ] in
      Variable.(
        As_project.Lib.id t =:= case (As_project.Lib.available t) byte)
      :: Variable.(
          As_project.Lib.id t =+= case (As_project.Lib.available t) native)
      :: Variable.(
          As_project.Lib.id t =+= case (As_project.Lib.available t) native_dynlink)
      :: comp_byte t
      :: comp_native t
      :: pp_byte t
      :: pp_native t
      :: link_byte t
      :: link_native t
      :: link_shared t
      :: Variable.(
          prereqs t `Byte =?=
          `Strings (As_project.Component.prereqs (`Lib t) resolver `Byte `Link))
      :: Variable.(
          prereqs t `Native =?=
          `Strings (As_project.Component.prereqs (`Lib t) resolver `Native `Link))
      :: Variable.(
          prereqs t `Shared =?=
          `Strings (As_project.Component.prereqs (`Lib t) resolver `Shared `Link))
      :: [] in
    Variable.stanza
      ~doc:[sprintf "Library: %s" (As_project.Lib.name t)]
      vars
    :: List.map U.variables (As_project.Lib.units t)

  let rules t =
    let byte =
      Rule.create
        ~targets:[As_project.Lib.cma t resolver]
        ~prereqs:[prereqs_var t `Byte] [
        sprintf "$(OCAMLC) -a %s -o %s" (Variable.name (link_byte t)) Rule.target
      ] in
    let native mode =
      let file, arg = match mode with
        | `Shared -> As_project.Lib.cmxs t resolver, "-shared -linkall"
        | `Native -> As_project.Lib.cmxa t resolver, "-a" in
      let link = match mode with
        | `Shared -> link_shared t
        | `Native -> link_native t in
      Rule.create
        ~targets:[file]
        ~prereqs:[prereqs_var t mode] [
        sprintf "$(OCAMLOPT) %s %s -o %s"
         arg (Variable.name link) Rule.target
      ] in
    Rule.create
      ~targets:[As_project.Lib.id t]
      ~prereqs:[sprintf "$(%s)" (As_project.Lib.id t)]
      [echo_prereqs (As_project.Lib.id t)]
    :: byte
    :: native `Native
    :: native `Shared
    :: conmap U.rules (As_project.Lib.units t)

end

and B: sig
  val rules      : As_project.Bin.t -> Rule.t list
  val variables  : As_project.Bin.t -> Variable.stanza list
  val comp_byte  : As_project.Bin.t -> Variable.t
  val comp_native: As_project.Bin.t -> Variable.t
  val comp_c     : As_project.Bin.t -> Variable.t
  val pp_byte    : As_project.Bin.t -> Variable.t
  val pp_native  : As_project.Bin.t -> Variable.t
end = struct

  let flag suffix with_glob fn t =
    let var   = As_project.Bin.id t ^ "." ^ suffix in
    let glob  = sprintf "$(%s)" suffix in
    let flags = fn (As_project.Bin.flags t resolver) in
    if with_glob then
      Variable.(var =?= `Strings (glob :: flags))
    else
      Variable.(var =?= `Strings flags)

  let pp_byte = flag pp_byte false As_flags.(get `Pp `Byte)
  let pp_native = flag pp_native false As_flags.(get `Pp `Native)
  let comp_byte = flag comp_byte true As_flags.(get `Compile `Byte)
  let comp_native = flag comp_native true As_flags.(get `Compile `Native)
  let comp_c = flag comp_c true As_flags.(get `Compile `C)
  let link_byte = flag link_byte true As_flags.(get `Link `Byte)
  let link_native = flag link_native true As_flags.(get `Link `Native)

  let prereqs t = function
    | `Byte   -> As_project.Bin.id t ^ "." ^ deps_byte
    | `Native -> As_project.Bin.id t ^ "." ^ deps_native

  let prereqs_var t mode =
    sprintf "$(%s)" (prereqs t mode)

  let variables t =
    let vars =
      let byte = As_project.Bin.byte t resolver in
      let native = As_project.Bin.native t resolver in
      let js = As_project.Bin.js t resolver in
      let byte = [ byte_f, `Strings [byte] ] in
      let native = [ native_f, `Strings [native] ] in
      let js = [ js_f, `Strings [js] ] in
      Variable.(
        As_project.Bin.id t =:= case (As_project.Bin.available t) byte)
      :: Variable.(
          As_project.Bin.id t =+= case (As_project.Bin.available t) native)
      :: Variable.(
          As_project.Bin.id t =+= case (As_project.Bin.available t) js)
      :: comp_byte t
      :: comp_native t
      :: pp_byte t
      :: pp_native t
      :: link_byte t
      :: link_native t
      :: Variable.(prereqs t `Byte   =?=
                   `Strings (As_project.Component.prereqs (`Bin t) resolver `Byte `Link))
      :: Variable.(prereqs t `Native =?=
                   `Strings (As_project.Component.prereqs (`Bin t) resolver `Native `Link))
      :: [] in
    Variable.stanza
      ~doc:[sprintf "Binary: %s" (As_project.Bin.name t)]
      vars
    :: List.map U.variables (As_project.Bin.units t)

  let rules t =
    Rule.create
      ~targets:[As_project.Bin.id t]
      ~prereqs:[sprintf "$(%s)" (As_project.Bin.id t)]
      [echo_prereqs (As_project.Bin.id t)]
    ::
    Rule.create
      ~targets:[As_project.Bin.byte t resolver]
      ~prereqs:[prereqs_var t `Byte] [
      sprintf "mkdir -p %s" (As_project.Bin.build_dir t resolver);
      sprintf "$(OCAMLC) %s -o %s" (Variable.name (link_byte t)) Rule.target;
    ]
    ::
    Rule.create
      ~targets:[As_project.Bin.native t resolver]
      ~prereqs:[prereqs_var t `Native] [
      sprintf "mkdir -p %s" (As_project.Bin.build_dir t resolver);
      sprintf "$(OCAMLOPT) %s -o %s" (Variable.name (link_native t)) Rule.target;
    ]
    :: (if As_project.Bin.has_js t then
      [Rule.create
         ~targets:[As_project.Bin.js t resolver]
         ~prereqs:(As_project.Component.prereqs (`Bin t) resolver `Js `Link) [
         sprintf "$(JS_OF_OCAML) %s %s"
           (String.concat " "
              (As_flags.(get `Link `Js) (As_project.Bin.flags t resolver)))
           Rule.prereq]
      ] else [])
    @ conmap U.rules (As_project.Bin.units t)

end

and O : sig
  val rules    : As_project.Other.t -> Rule.t list
  val variables: As_project.Other.t -> Variable.stanza
end = struct

  let variables _ = Variable.stanza []

  let rules t =
    List.map (fun (kinds, actions) ->
        let targets =
          List.map (As_project.Other.file_of_kind t resolver) kinds
          |> dedup in
        let prereqs =
          (* FIXME: which mode and phase ? *)
          As_project.Component.prereqs (`Other t) resolver `Byte `Other in
        Rule.create ~targets ~prereqs actions
      ) (As_project.Other.actions t resolver)

end

module T = struct

  let variables ts =
    [Variable.("test" =:= `String (String.concat " " (List.map As_project.Test.id ts)))]

  let rules ts =
    Rule.create ~targets:["test"] ~prereqs:["$(test)"] []
    :: List.map (fun t ->
        Rule.create
          ~targets:[As_project.Test.id t]
          ~prereqs:(As_project.Component.prereqs (`Test t) resolver `Byte `Test) (
          let dir = match As_project.Test.dir t with
            | None   -> ""
            | Some d -> sprintf "cd %s &&" d in
          List.map (function
              | `Shell cmd            -> String.concat " " [dir; cmd]
              | `Bin (`Bin bin, args) ->
                let args = args (fun c ->
                    Sys.getcwd () / As_project.Component.build_dir c resolver
                  ) in
                let bin = Sys.getcwd () / As_project.Bin.byte bin resolver in
                String.concat " " [dir; bin; String.concat " " args]
            ) (As_project.Test.commands t)
        )
      ) ts

end

module D = struct

  let target l =
    "doc-" ^ As_project.Lib.id l

  let full_target l =
    "full-doc-" ^ As_project.Lib.id l

  let variables libs =
    [Variable.("doc"      =:= `String (String.concat " " (List.map target libs)));
     Variable.("full-doc" =:= `String (String.concat " " (List.map full_target libs)));]

  let rules ?css ?intro ~dir public libs =
    let aux ~full_doc l =
      let targets = [if full_doc then full_target l else target l] in
      Rule.create
        ~targets
        ~prereqs:[As_project.Lib.id l] [
        sprintf "mkdir -p %s" dir;
        let files =
          As_project.Lib.units l
          |> conmap (fun u ->
              let name = As_project.Unit.name u in
              if full_doc || List.mem name public then
                ["$(BUILDIR)" / As_project.Lib.id l /
                 if As_project.Unit.has `Mli u then name ^ ".mli" else name ^ ".ml"]
              else
                [])
          |> String.concat " " in
        let deps = As_project.Lib.deps l |> As_project.Component.closure ~link:true in
        let libs =
          deps
          |> As_project.Component.(filter lib_ocaml)
          |> (fun d -> l :: d)
          |> List.map (fun l -> sprintf "-I %s"
                          (As_resolver.build_dir resolver
                             (As_project.Lib.id l)))
          |> String.concat " " in
        let pkgs =
          deps
          |> As_project.Component.(filter pkg_ocaml)
          |> List.map As_project.Pkg.name
          |> (fun pkgs ->
              As_flags.(get `Compile `Byte) (As_resolver.pkgs resolver pkgs))
          |> String.concat " " in
        let css = match css with
          | None   -> ""
          | Some f -> sprintf "-css-style %s " f in
        let intro = match intro with
          | None   -> ""
          | Some i -> sprintf "-intro %s " i in
        sprintf "$(OCAMLDOC) %s %s %s -short-functors \
                \  %s%s-colorize-code -html -d %s"
          pkgs libs files css intro dir
      ] in
    Rule.create ~targets:["doc"] ~prereqs:["$(doc)"] []
    :: List.map (aux ~full_doc:true) libs
    @  List.map (aux ~full_doc:false) libs

end

module C = struct

  let filter = List.filter (fun u -> As_project.Unit.kind u = `C)

  let variables l =
    let l =  filter l in
    [Variable.("c" =:= `String (String.concat " " (List.map As_project.Unit.id l)))]

  let rules () =
    Rule.create ~targets:["c"] ~prereqs:["$(c)"] []

end

module J = struct

  let filter = List.filter As_project.Bin.has_js

  let variables l =
    let l = filter l in
    let id t = As_project.Bin.js t resolver in
    [Variable.("js" =:= `String (String.concat " " (List.map id l)))]

  let rules () =
    Rule.create ~targets:["js"] ~prereqs:["$(js)"] []

end

let global_variables flags =
  let debug = Variable.has_feature As_features.debug_atom in
  let annot = Variable.has_feature As_features.annot_atom in
  let warn_error = Variable.has_feature As_features.warn_error_atom in
  let mk fn n = match fn flags with
    | [] -> []
    | l  -> [Variable.(n =:= `Strings l)] in
  let vars =
    mk As_flags.(get `Compile `Byte) comp_byte
    @ mk As_flags.(get `Compile `Native) comp_native
    @ mk As_flags.(get `Link `Byte) link_byte
    @ mk As_flags.(get `Link `Native) link_native
    @ mk As_flags.(get `Link `Shared) link_shared
    @ [
      Variable.(comp_byte =+= `Case [
          [debug, "1"], `Strings As_flags.(get `Compile `Byte debug)
        ]);
      Variable.(comp_byte =+= `Case [
          [annot, "1"], `Strings As_flags.(get `Compile `Byte annot)
        ]);
      Variable.(comp_byte =+= `Case [
          [warn_error, "1"], `Strings As_flags.(get `Compile `Byte warn_error)
        ]);
      Variable.(link_byte =+= `Case [
          [debug, "1"], `Strings As_flags.(get `Link `Byte debug)
        ]);
      Variable.(comp_native =+= `Case [
          [debug, "1"], `Strings As_flags.(get `Compile `Native debug)
        ]);
      Variable.(comp_native =+= `Case [
          [annot, "1"], `Strings As_flags.(get `Compile `Native annot)
        ]);
      Variable.(comp_native =+= `Case [
          [warn_error, "1"], `Strings As_flags.(get `Compile `Native warn_error)
        ]);
      Variable.(link_native =+= `Case [
          [debug, "1"], `Strings As_flags.(get `Link `Native debug)
        ]);
    ] in
  Variable.stanza ~align:true ~simplify:true vars

let of_project ?(buildir="_build") ?(makefile="Makefile") ~flags ~features t =
  let components = As_project.components t in
  let libs = As_project.Component.(filter lib_ocaml components) in
  let pps = As_project.Component.(filter lib_ocaml_pp components) in
  let bins = As_project.Component.(filter bin components) in
  let tests = As_project.Component.(filter test components) in
  let others = As_project.Component.(filter other components) in
  let units = As_project.Component.(filter unit components) in
  let targets =
    List.map (fun l -> `Lib l) (libs @ pps)
    @ List.map (fun b -> `Bin b) bins in
  let all =
    let targets = List.map As_project.Component.id targets in
    Variable.("all" =:= `String (String.concat " " targets)) in
  let global_variables =
    global_variables flags in
  let project_features =
    As_project.features t
    |> As_features.Set.elements in
  let feature_variables =
    project_features
    |> (fun t ->
        List.map (fun elt ->
            if List.mem_assoc elt features then
              As_features.with_default elt (List.assoc elt features)
            else elt
          ) t)
    |> List.map Variable.has_feature in
  let variables =
    let tool name =
      let native = [ (Variable.has_feature As_features.native_atom, "1") ] in
      `Case [ (native, `String (name ^ ".opt")); ([], `String name) ] in
    Variable.stanza ~doc:[""; "Main project configuration"; ""] []
    :: Variable.stanza
      ~align:true
      Variable.([
          ("BUILDIR"     =?= `String buildir);
          ("OCAMLOPT"    =?= tool "ocamlopt");
          ("OCAMLC"      =?= tool "ocamlc");
          ("CAMLP4O"     =?= `String "camlp4o");
          ("OCAMLDOC"    =?= tool "ocamldoc");
          ("JS_OF_OCAML" =?= `String "js_of_ocaml");
          ("OCAMLMKLIB"  =?= `String "ocamlmklib");
        ])
    :: Variable.stanza ~align:true feature_variables
    :: Variable.stanza ~doc:[""; "Global targets"; ""] []
    :: Variable.stanza ~align:true (all :: D.variables libs
                                    @ T.variables tests
                                    @ C.variables units
                                    @ J.variables bins)
    :: Variable.stanza ~doc:[""; "Global variables"; ""] []
    :: global_variables
    :: Variable.stanza
      ~doc:["";
            "Component configuration.";
            "";
            "Each component has variables associated to the different phases of the build:";
            "";
            " - <NAME>.comp-native controls the compilation options for native code.";
            " - <NAME>.comp-byte controls the compilation options for bytecode.";
            " - <NAME>.link-native controls the compilation options for linking native core";
            " - <NAME>.link-byte controls the compilation options for linking byte code.";
            " - <NAME>.dep-native is the list of dependencies for the component in native mode.";
            " - <NAME>.dep-byte is the list of dependencies for the component in byte code.";
            "";
           ] []
    :: (List.map U.variables units)
    @  (conmap L.variables (libs @ pps))
    @  (conmap B.variables bins)
    @  (List.map O.variables others)
  in
  let rules =
    conmap U.rules units
    @ conmap L.rules libs
    @ conmap L.rules pps
    @ conmap B.rules bins
    @ conmap O.rules others
    @ T.rules tests
    @ D.rules
      ?css:(As_project.doc_css t)
      ?intro:(As_project.doc_intro t)
      ~dir:(As_project.doc_dir t)
      (As_project.doc_public t) libs
    @ [C.rules (); J.rules ()]
  in
  let main = Rule.create ~ext:true ~targets:["all"] ~prereqs:[] [
      sprintf "@echo '%s %s ${all}'"
        (As_shell.color `underline "all")
        (As_shell.color `yellow "=>");
      sprintf "@$(MAKE) $(all)";
      sprintf "@if [ \"x${HAS_JS}\" = \"x1\" ]; then $(MAKE) js; fi";
      sprintf "@if [ \"x${HAS_TEST}\" = \"x1\" ]; then $(MAKE) test; fi";
      sprintf "@if [ \"x${HAS_DOC}\" = \"x1\" ]; then $(MAKE) doc; fi";
      sprintf "@if [ \"x${HAS_FULL_DOC}\" = \"x1\" ]; then $(MAKE) full-doc; fi";
      sprintf "@echo '\027[32m== Done!\027[m'";
    ] in
  let clean = Rule.create ~ext:true ~targets:["clean"] ~prereqs:[] [
      "rm -f *~ **/*~";
      sprintf "rm -rf $(BUILDIR)";
    ] in
  let distclean = Rule.create ~ext:true ~targets:["distclean"] ~prereqs:["clean"] (
      sprintf "rm -f %s %s.install META %s/*.html"
        makefile  (As_project.name t) (As_project.doc_dir t)
      ::
      List.map (fun file ->
          sprintf "rm -rf %s.ml" file
        ) (As_project.files_of_generators t resolver)
    ) in
  let install = Rule.create ~ext:true ~targets:["install"] ~prereqs:["all"] [
      sprintf "@opam-installer --prefix $(shell opam config var prefix) \
               %s.install" (As_project.name t)
    ] in
  let help = Rule.create ~targets:["help"] ~prereqs:[]
      ([sprintf "@echo 'The following targets are available (use \"make %s\"):'"
          (As_shell.color `underline "<target>");
        "@echo";
        sprintf "@echo ' - %s -- build all the active targets.'"
          (As_shell.color `underline "all")]
       @ List.map (fun c ->
           sprintf "@echo ' - %s -- build the %s %s.'"
             (As_shell.color `underline (As_project.Component.id c))
             (match c with `Lib _ -> "library"
                         | `Bin _ -> "executable"
                         | _ -> "component")
             (As_project.Component.name c)
         ) targets
       @ [sprintf "@echo ' - %s -- build the documentation.'"
            (As_shell.color `underline "doc");
          sprintf "@echo ' - %s -- build and run the test.'"
            (As_shell.color `underline "test");
          sprintf "@echo ' - %s -- build the js_of_ocaml targets.'"
            (As_shell.color `underline "js");
          sprintf "@echo ' - %s -- clean the build artefacts.'"
            (As_shell.color `underline "clean");
          sprintf "@echo ' - %s -- clean the project to prepare the release.'"
            (As_shell.color `underline "distclean");
          "@echo";
          "@echo";
          sprintf "@echo 'Current configuration (use \"make %s\" to modify):'"
            (As_shell.color `underline "VAR=val");
          "@echo"; ]
       @ List.map (fun f ->
           let v = Variable.has_feature f in
           let k_v = v.Variable.name ^ "=" ^ Variable.name v in
           sprintf "@echo ' - %s -- %s'"
             (As_shell.color `underline k_v) (As_features.doc f)
         ) project_features
       @ [ "@echo" ]
      ) in
  create
    ~phony:["all"; "clean"; "test"; "doc"; "distclean"; "js"; "help"]
    makefile
    variables
    (main :: clean :: distclean :: install :: help :: rules)
