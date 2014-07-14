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

module Project = As_project
module Features = As_features
module Flags = As_flags
module Ocamlfind = As_ocamlfind
module Resolver = As_resolver
module Shell = As_shell

let (/) x y = Filename.concat x y

let (//) x y =
  match x with
  | None   -> y
  | Some x -> Filename.concat x y

let (|>) x f = f x

let conmap f l = List.concat (List.map f l)

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
    let var = String.uppercase (Features.name f) in
    let var = String.map (function '-' -> '_' | x -> x) var in
    ("HAS_" ^ var) =?= `String (if Features.default f then "1" else "0")

  (* build the guard pattern *)
  let guard features =
    match Features.cnf features with
    | `Conflict -> failwith "invalid handler case"
    | `And l    ->
      List.map (function
          | `P f -> has_feature f, "1"
          | `N f -> has_feature f, "0"
        ) l

  (* build the full handler cases *)
  let case available cs =
    let cs = List.filter (fun (f,_) ->
        Features.(cnf (available &&& f)) <> `Conflict
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

  let compare_guards o1 o2 =
    let mk l =
      List.map (fun (g, _) -> string_of_guard g) l
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

let buildir = "$(BUILDIR)"

let echo_prereqs name =
  sprintf "@echo '%s %s %s'"
    (Shell.color `underline name)
    (Shell.color `yellow "=>")
    Rule.prereqs

let resolver =
  Ocamlfind.resolver `Makefile buildir

let native_dynlink_f = Features.(native_dynlink &&& native)

let native_f = Features.native

let comp_byte   = "comp-byte"
let comp_native = "comp-native"
let link_byte   = "link-byte"
let link_native = "link-native"
let link_shared = "link-shared"
let pp_byte     = "pp-byte"
let pp_native   = "pp-native"
let deps_byte   = "deps-byte"
let deps_native = "deps-native"
let deps_shared = "deps-shared"

module rec U: sig
  val rules    : Project.CU.t -> Rule.t list
  val variables: Project.CU.t -> Variable.stanza
end = struct

  let pp suffix varlib varbin fn t =
    let var = Project.CU.id t ^ "." ^ suffix in
    let lib = match Project.CU.container t with
      | None          -> None
      | Some (`Lib l) -> Some (varlib l)
      | Some (`Bin b) -> Some (varbin b) in
    match lib, fn (Project.CU.flags t resolver) with
    | None  , [] -> None
    | Some l, [] -> if Variable.is_empty l then None else Some l
    | None  , u  -> Some (Variable.(var =?= `Strings u))
    | Some l, u  ->
      if Variable.is_empty l then
        Some (Variable.(var =?= `Strings u))
      else
        Some (Variable.(var =?= `Strings (Variable.name l :: u)))

  let flag suffix varlib varbin fn t =
    let var    = Project.CU.id t ^ "." ^ suffix in
    let global = match Project.CU.container t with
      | None          -> []
      | Some (`Lib l) -> [Variable.name (varlib l)]
      | Some (`Bin b) -> [Variable.name (varbin b)] in
    let flags = fn (Project.CU.flags t resolver) @ global in
    Variable.(var =?= `Strings flags)

  let pp_byte     = pp   pp_byte   L.pp_byte     B.pp_byte     Flags.pp_byte
  let pp_native   = pp   pp_native L.pp_native   B.pp_native   Flags.pp_native
  let comp_byte   = flag comp_byte L.comp_byte   B.comp_byte   Flags.comp_byte
  let comp_native =
    flag comp_native L.comp_native B.comp_native Flags.comp_native

  let prereqs t = function
    | `Byte   -> Project.CU.id t ^ "." ^ deps_byte
    | `Native -> Project.CU.id t ^ "." ^ deps_native

  let prereqs_var t mode =
    sprintf "$(%s)" (prereqs t mode)

  let variables t =
    Variable.stanza
      ~doc:[sprintf "Compilation unit %s" (Project.CU.dir t // Project.CU.name t)]
      (Variable.(prereqs t `Byte =?= `Strings (Project.CU.prereqs t resolver `Byte))
       :: Variable.(prereqs t `Native =?= `Strings (Project.CU.prereqs t resolver `Native))
       :: comp_byte t
       :: comp_native t
       :: (match pp_byte t with
           | None   -> []
           | Some v -> [v])
       @  (match pp_native t with
           | None   -> []
           | Some v -> [v]))

  (* XXX: handle native pp *)
  let rules t =
    let pp = match pp_byte t with
      | None   -> ""
      | Some v -> sprintf "-pp '$(CAMLP4O) %s' " (Variable.name v) in
    let for_pack = match Project.CU.for_pack t with
      | None   -> ""
      | Some p -> sprintf "-for-pack %s " p in
    let flags = for_pack ^ pp in
    let target ext = Project.CU.file t resolver ext in
    let source ext = Project.CU.(dir t // name t ^ ext) in

    match Project.CU.unpack t with
    | [] -> (* Normal compilation unit. *)
      let ln = (* link source file to target directory *)
        match Project.CU.generated t with
        | false ->
          let aux exists ext =
            let source = source ext in
            let target = target ext in
            if exists t then
              [Rule.create ~targets:[target] ~prereqs:[source] [
                  sprintf "mkdir -p %s" (Project.CU.build_dir t resolver);
                  sprintf "ln -sf $(shell pwd)/%s %s" source target
                ]]
            else [] in
          aux Project.CU.ml ".ml" @ aux Project.CU.mli ".mli"
        | true -> [] in
      let cmi = (* generate cmis *)
        let targets, prereqs =
          if Project.CU.mli t then [target ".cmi"], [target ".mli"]
          else if Project.CU.ml t then [target ".cmo"; target ".cmi"], [target ".ml"]
          else [], [] in
        [Rule.create ~targets ~prereqs:(prereqs @ [prereqs_var t `Byte]) [
            sprintf "$(OCAMLC) -c %s%s %s"
              flags (Variable.name (comp_byte t)) Rule.prereq
          ]] in
      let cmo = (* Generate cmos *)
        if Project.CU.mli t && Project.CU.ml t then
          [Rule.create ~targets:[target ".cmo"]
             ~prereqs:[target ".ml"; target ".cmi"; prereqs_var t `Byte]
             [sprintf "$(OCAMLC) -c %s%s %s"
                flags (Variable.name (comp_byte t)) Rule.prereq]]
        else
          [] in
      let cmx = (* Generate cmxs *)
        [Rule.create ~targets:[target ".cmx"]
           ~prereqs:[target ".ml"; target ".cmi"; prereqs_var t `Native]
           [sprintf "$(OCAMLOPT) -c %s%s %s"
              flags (Variable.name (comp_native t)) Rule.prereq]]
      in
      ln @ cmi @ cmo @ cmx

    | units -> (* Packed units *)
      let byte =
        let cmo = List.map (fun u -> Project.CU.cmo u resolver) units in
        Rule.create ~targets:[target ".cmo"; target ".cmi"] ~prereqs:cmo [
          sprintf "$(OCAMLC) -pack %s%s -o %s"
            flags Rule.prereq Rule.target_member
        ] in
      let native =
        let cmx = List.map (fun u -> Project.CU.cmx u resolver) units in
        Rule.create ~targets:[target ".cmx"] ~prereqs:cmx [
          sprintf "$(OCAMLOPT) -pack %s%s -o %s"
            flags Rule.prereq Rule.target_member
        ] in
      byte :: native :: conmap U.rules units

end

and L: sig
  val rules      : Project.Lib.t -> Rule.t list
  val variables  : Project.Lib.t -> Variable.stanza
  val comp_byte  : Project.Lib.t -> Variable.t
  val comp_native: Project.Lib.t -> Variable.t
  val pp_byte    : Project.Lib.t -> Variable.t
  val pp_native  : Project.Lib.t -> Variable.t
end = struct

  let flag suffix with_glob fn t =
    let var   = Project.Lib.id t ^ "." ^ suffix in
    let glob  = sprintf "$(%s)" suffix in
    let flags = fn (Project.Lib.flags t resolver) in
    if with_glob then
      Variable.(var =?= `Strings (glob :: flags))
    else
      Variable.(var =?= `Strings flags)

  let comp_byte   = flag comp_byte   true  Flags.comp_byte
  let comp_native = flag comp_native true  Flags.comp_native
  let pp_byte     = flag pp_byte     false Flags.pp_byte
  let pp_native   = flag pp_native   false Flags.pp_native
  let link_byte   = flag link_byte   true  Flags.link_byte
  let link_native = flag link_native true  Flags.link_native
  let link_shared = flag link_shared true Flags.link_shared

  let prereqs t = function
    | `Byte   -> Project.Lib.id t ^ "." ^ deps_byte
    | `Native -> Project.Lib.id t ^ "." ^ deps_native
    | `Shared -> Project.Lib.id t ^ "." ^ deps_shared

  let prereqs_var t mode =
    sprintf "$(%s)" (prereqs t mode)

  let variables t =
    let vars =
      let cma  = Project.Lib.cma t resolver in
      let cmxa = Project.Lib.cmxa t resolver in
      let cmxs = Project.Lib.cmxs t resolver in
      let native = [ native_f, `Strings [cma; cmxa] ] in
      let native_dynlink = [ native_dynlink_f, `Strings [cmxs] ] in
      Variable.(Project.Lib.id t =:= `Strings [cma])
      :: Variable.(Project.Lib.id t =+= case (Project.Lib.available t) native)
      :: Variable.(Project.Lib.id t =+= case (Project.Lib.available t) native_dynlink)
      :: comp_byte t
      :: comp_native t
      :: pp_byte t
      :: pp_native t
      :: link_byte t
      :: link_native t
      :: link_shared t
      :: Variable.(prereqs t `Byte =?= `Strings (Project.Lib.prereqs t resolver `Byte))
      :: Variable.(prereqs t `Native =?= `Strings (Project.Lib.prereqs t resolver `Native))
      :: [] in
    Variable.stanza
      ~doc:[sprintf "Library %s" (Project.Lib.name t)]
      vars

  let rules t =
    let byte =
      Rule.create
        ~targets:[Project.Lib.cma t resolver]
        ~prereqs:[prereqs_var t `Byte] [
        sprintf "$(OCAMLC) -a %s -o %s" (Variable.name (link_byte t)) Rule.target
      ] in
    let native mode =
      let file, arg = match mode with
        | `Shared -> Project.Lib.cmxs t resolver, "-shared"
        | `Native -> Project.Lib.cmxa t resolver, "-a" in
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
      ~targets:[Project.Lib.id t]
      ~prereqs:[sprintf "$(%s)" (Project.Lib.id t)]
      [echo_prereqs (Project.Lib.id t)]
    :: byte
    :: native `Native
    :: native `Shared
    :: []

end

and B: sig
  val rules      : Project.Bin.t -> Rule.t list
  val variables  : Project.Bin.t -> Variable.stanza
  val comp_byte  : Project.Bin.t -> Variable.t
  val comp_native: Project.Bin.t -> Variable.t
  val pp_byte    : Project.Bin.t -> Variable.t
  val pp_native  : Project.Bin.t -> Variable.t
end = struct

  let flag suffix with_glob fn t =
    let var   = Project.Bin.id t ^ "." ^ suffix in
    let glob  = sprintf "$(%s)" suffix in
    let flags = fn (Project.Bin.flags t resolver) in
    if with_glob then
      Variable.(var =?= `Strings (glob :: flags))
    else
      Variable.(var =?= `Strings flags)

  let comp_byte   = flag comp_byte   true  Flags.comp_byte
  let comp_native = flag comp_native true  Flags.comp_native
  let link_byte   = flag link_byte   true  Flags.link_byte
  let link_native = flag link_native true  Flags.link_native
  let pp_byte     = flag pp_byte     false Flags.pp_byte
  let pp_native   = flag pp_native   false Flags.pp_native

  let prereqs t = function
    | `Byte   -> Project.Bin.id t ^ "." ^ deps_byte
    | `Native -> Project.Bin.id t ^ "." ^ deps_native

  let prereqs_var t mode =
    sprintf "$(%s)" (prereqs t mode)

  let variables t =
    let vars =
      let cs = [
        native_f      , `Strings [Project.Bin.byte t resolver; Project.Bin.native t resolver];
        Features.true_, `String  (Project.Bin.byte t resolver);
      ] in
      Variable.(Project.Bin.id t =?= case (Project.Bin.available t) cs)
      :: comp_byte t
      :: comp_native t
      :: pp_byte t
      :: pp_native t
      :: link_byte t
      :: link_native t
      :: Variable.(prereqs t `Byte   =?= `Strings (Project.Bin.prereqs t resolver `Byte))
      :: Variable.(prereqs t `Native =?= `Strings (Project.Bin.prereqs t resolver `Native))
      :: [] in
    Variable.stanza
      ~doc:[sprintf "Binary %s" (Project.Bin.name t)]
      vars

  let rules t =
    Rule.create
      ~targets:[Project.Bin.id t]
      ~prereqs:[sprintf "$(%s)" (Project.Bin.id t)]
      [echo_prereqs (Project.Bin.id t)]
    ::
    Rule.create
      ~targets:[Project.Bin.byte t resolver]
      ~prereqs:[prereqs_var t `Byte] [
      sprintf "mkdir -p %s" (Project.Bin.build_dir t resolver);
      sprintf "$(OCAMLC) %s -o %s" (Variable.name (link_byte t)) Rule.target;
    ]
    ::
    Rule.create
      ~targets:[Project.Bin.native t resolver]
      ~prereqs:[prereqs_var t `Native] [
      sprintf "mkdir -p %s" (Project.Bin.build_dir t resolver);
      sprintf "$(OCAMLOPT) %s -o %s" (Variable.name (link_native t)) Rule.target;
    ]
    :: []

end

and G: sig
  val rules    : Project.Gen.t -> Rule.t list
  val variables: Project.Gen.t -> Variable.stanza
end = struct

  (* XXX: improve the generated variables and rules *)
  let variables _ =
    Variable.stanza []

  let rules t =
    [
      Rule.create
        ~targets:(Project.Gen.files t resolver)
        ~prereqs:(Project.Gen.prereqs t resolver `Byte)
        (Project.Gen.actions t resolver)
    ]
end

module T = struct

  let variables ts =
    [Variable.("test" =:= `String (String.concat " " (List.map Project.Test.id ts)))]

  let rules ts =
    Rule.create ~targets:["test"] ~prereqs:["$(test)"] []
    :: List.map (fun t ->
        Rule.create
          ~targets:[Project.Test.id t]
          ~prereqs:(Project.Test.prereqs t resolver `Byte) (
          let dir = match Project.Test.dir t with
            | None   -> ""
            | Some d -> sprintf "cd %s &&" d in
          List.map (function
              | `Shell cmd            -> String.concat " " [dir; cmd]
              | `Bin (`Bin bin, args) ->
                let args = args (fun c ->
                    Sys.getcwd () / Project.Component.build_dir c resolver
                  ) in
                let bin = Sys.getcwd () / Project.Bin.byte bin resolver in
                String.concat " " [dir; bin; String.concat " " args]
            ) (Project.Test.commands t)
        )
      ) ts

end

module D = struct

  let target l =
    "doc-" ^ Project.Lib.id l

  let full_target l =
    "full-doc-" ^ Project.Lib.id l

  let variables libs =
    [Variable.("doc"      =:= `String (String.concat " " (List.map target libs)));
     Variable.("full-doc" =:= `String (String.concat " " (List.map full_target libs)));]

  let rules ?css ?intro ~dir public libs =
    let aux ~full_doc l =
      let targets = [if full_doc then full_target l else target l] in
      Rule.create
        ~targets
        ~prereqs:[Project.Lib.id l] [
        sprintf "mkdir -p %s" dir;
        let files =
          Project.Lib.compilation_units l
          |> conmap (fun u ->
              let name = Project.CU.name u in
              if full_doc || List.mem name public then
                ["$(BUILDIR)" / Project.Lib.id l /
                 if Project.CU.mli u then name ^ ".mli" else name ^ ".ml"]
              else
                [])
          |> String.concat " " in
        let deps = Project.Lib.deps l |> Project.Component.closure in
        let libs =
          deps
          |> Project.Component.(filter lib)
          |> (fun d -> l :: d)
          |> List.map (fun l -> sprintf "-I %s"
                          (Resolver.build_dir resolver (Project.Lib.id l)))
          |> String.concat " " in
        let pkgs =
          deps
          |> Project.Component.(filter pkg)
          |> (fun pkgs -> Flags.comp_byte (Resolver.pkgs resolver pkgs))
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

module J = struct

  let variables l =
    [Variable.("js" =:= `String (String.concat " " (List.map Project.JS.id l)))]

  let rules jss =
    Rule.create ~targets:["js"] ~prereqs:["$(js)"] []
    :: List.map (fun j ->
        Rule.create
          ~targets:[Project.JS.id j]
          ~prereqs:(Project.JS.prereqs j resolver `Byte) [
          sprintf "$(JS_OF_OCAML) %s %s"
            (String.concat " " (Flags.link_byte (Project.JS.flags j resolver)))
            Rule.prereq
        ]) jss

end

module C = struct

  let variables l =
    [Variable.("c" =:= `String (String.concat " " (List.map Project.C.id l)))]

  let rules cs r =
    (* XXX: handle the case where opam is not installed *)
    let lib_dir = List.hd (Shell.exec_output "opam config var lib") in
    Rule.create ~targets:["c"] ~prereqs:["$(c)"] []
    :: conmap (fun c -> [
          (* XXX: add symlink rule *)
          Rule.create
            ~targets:[Project.C.id c]
            ~prereqs:[Project.C.dll_so c r]
            [];
          Rule.create
            ~targets:[Project.C.o c r]
            ~prereqs:[Project.C.symlink_c c r]
            [sprintf "cd %s && $(OCAMLC) -c -I %s %s %s"
               (Filename.dirname (Project.C.symlink_c c r))
               lib_dir
               (Filename.basename (Project.C.symlink_c c r))
               (String.concat " " (
                   List.map (sprintf "-ccopt %s") (Project.C.link_flags c)))];
          Rule.create
            ~targets:[Project.C.dll_so c r]
            ~prereqs:[Project.C.o c r]
           [sprintf "$(OCAMLMKLIB) -o %s %s %s"
              (Project.C.file c resolver "")
              Rule.prereqs
              (String.concat " " (Project.C.link_flags c))
           ];
        ]
      ) cs

end

let global_variables flags =
  let debug = Variable.has_feature Features.debug_elt in
  let annot = Variable.has_feature Features.annot_elt in
  let warn_error = Variable.has_feature Features.warn_error_elt in
  let mk fn n = match fn flags with
    | [] -> []
    | l  -> [Variable.(n =:= `Strings l)] in
  let vars =
    mk Flags.comp_byte   comp_byte
    @ mk Flags.comp_native comp_native
    @ mk Flags.link_byte   link_byte
    @ mk Flags.link_native link_native
    @ mk Flags.link_shared link_shared
    @ [
      Variable.(comp_byte =+= `Case [
          [debug, "1"], `Strings Flags.(comp_byte debug)
        ]);
      Variable.(comp_byte =+= `Case [
          [annot, "1"], `Strings Flags.(comp_byte annot)
        ]);
      Variable.(comp_byte =+= `Case [
          [warn_error, "1"], `Strings Flags.(comp_byte warn_error)
        ]);
      Variable.(link_byte =+= `Case [
          [debug, "1"], `Strings Flags.(link_byte debug)
        ]);
      Variable.(comp_native =+= `Case [
          [debug, "1"], `Strings Flags.(comp_native debug)
        ]);
      Variable.(comp_native =+= `Case [
          [annot, "1"], `Strings Flags.(comp_native annot)
        ]);
      Variable.(comp_native =+= `Case [
          [warn_error, "1"], `Strings Flags.(comp_native warn_error)
        ]);
      Variable.(link_native =+= `Case [
          [debug, "1"], `Strings Flags.(link_native debug)
        ]);
    ] in
  Variable.stanza ~align:true ~simplify:true vars

let of_project ?(buildir="_build") ?(makefile="Makefile") ~flags ~features t =
  let components = Project.components t in
  let libs  = Project.Component.(filter lib components) in
  let pps   = Project.Component.(filter pp components) in
  let bins  = Project.Component.(filter bin components) in
  let tests = Project.Component.(filter test components) in
  let jss   = Project.Component.(filter js components) in
  let gens  = Project.Component.(filter gen components) in
  let cs    = Project.Component.(filter c components) in
  let cus   = Project.Component.(
      (components
       @ List.map (fun x -> `CU x) (conmap Project.Lib.compilation_units (libs @ pps))
       @ List.map (fun x -> `CU x) (conmap Project.Bin.compilation_units bins))
      |> Graph.of_list
      |> Graph.to_list
      |> filter cu) in
  let targets =
    List.map (fun l -> `Lib l) (libs @ pps)
    @ List.map (fun b -> `Bin b) bins in
  let all =
    let targets = List.map Project.Component.id targets in
    Variable.("all" =:= `String (String.concat " " targets)) in
  let global_variables =
    global_variables flags in
  let project_features =
    Project.features t
    |> Features.Set.elements in
  let feature_variables =
    project_features
    |> (fun t ->
        List.map (fun elt ->
            if List.mem_assoc elt features then
              Features.with_default elt (List.assoc elt features)
            else elt
          ) t)
    |> List.map Variable.has_feature in
  let variables =
    let tool name =
      let native = [ (Variable.has_feature Features.native_elt, "1") ] in
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
                                    @ C.variables cs
                                    @ J.variables jss)
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
    :: (List.map U.variables cus)
    @  (List.map L.variables (libs @ pps))
    @  (List.map B.variables bins)
    @  (List.map G.variables gens)
  in
  let rules =
    conmap U.rules cus
    @ conmap L.rules libs
    @ conmap L.rules pps
    @ conmap B.rules bins
    @ conmap G.rules gens
    @ C.rules cs resolver
    @ T.rules tests
    @ D.rules
      ?css:(Project.doc_css t)
      ?intro:(Project.doc_intro t)
      ~dir:(Project.doc_dir t)
      (Project.doc_public t) libs
    @ J.rules jss
  in
  let main = Rule.create ~ext:true ~targets:["all"] ~prereqs:[] [
      sprintf "@echo '%s %s ${all}'"
        (Shell.color `underline "all")
        (Shell.color `yellow "=>");
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
        makefile  (Project.name t) (Project.doc_dir t)
      ::
      List.map (fun file ->
          sprintf "rm -rf %s.ml" file
        ) (Project.files_of_generators t resolver)
    ) in
  let install = Rule.create ~ext:true ~targets:["install"] ~prereqs:["all"] [
      sprintf "@opam-installer --prefix $(shell opam config var prefix) \
               %s.install" (Project.name t)
    ] in
  let help = Rule.create ~targets:["help"] ~prereqs:[]
      ([sprintf "@echo 'The following targets are available (use \"make %s\"):'"
          (Shell.color `underline "<target>");
        "@echo";
        sprintf "@echo ' - %s -- build all the active targets.'" (Shell.color `underline "all")]
       @ List.map (fun c ->
           sprintf "@echo ' - %s -- build the %s %s.'"
             (Shell.color `underline (Project.Component.id c))
             (match c with `Lib _ -> "library" | `Bin _ -> "executable" | _ -> "component")
             (Project.Component.name c)
         ) targets
       @ [sprintf "@echo ' - %s -- build the documentation.'" (Shell.color `underline "doc");
          sprintf "@echo ' - %s -- build and run the test.'" (Shell.color `underline "test");
          sprintf "@echo ' - %s -- build the js_of_ocaml targets.'" (Shell.color `underline "js");
          sprintf "@echo ' - %s -- clean the build artefacts.'" (Shell.color `underline "clean");
          sprintf "@echo ' - %s -- clean the project to prepare the release.'"
            (Shell.color `underline "distclean");
          "@echo";
          "@echo";
          sprintf "@echo 'Current configuration (use \"make %s\" to modify):'"
            (Shell.color `underline "VAR=val");
          "@echo"; ]
       @ List.map (fun f ->
           let v = Variable.has_feature f in
           let k_v = v.Variable.name ^ "=" ^ Variable.name v in
           sprintf "@echo ' - %s -- %s'" (Shell.color `underline k_v) (Features.doc f)
         ) project_features
       @ [ "@echo" ]
      ) in
  create
    ~phony:["all"; "clean"; "test"; "doc"; "distclean"; "js"; "help"]
    makefile
    variables
    (main :: clean :: distclean :: install :: help :: rules)
