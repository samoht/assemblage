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
  let to_list = elements
  let of_list ss = List.fold_left (fun acc s -> add s acc) empty ss
end
let dedup l = StringSet.(to_list (of_list l))

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
    (As_shell.color `Underline name)
    (As_shell.color `Yellow "=>")
    Rule.prereqs

let resolver =
  As_ocamlfind.resolver `Makefile
    ~ocamlc:"$(OCAMLC)"
    ~ocamlopt:"$(OCAMLOPT)"
    ~ocamldep:"$(OCAMLDEP)"
    ~ocamlmklib:"$(OCAMLMKLIB)"
    ~ocamldoc:"$(OCAMLDOC)"
    ~camlp4o:"$(CAMLP4O)"
    ~js_of_ocaml:"$(JS_OF_OCAML)"
    ~ln:"$(LN)"
    ~mkdir:"$(MKDIR)"
    ~build_dir:"$(BUILDIR)"
    ~lib_dir:"$(LIBDIR)"
    ~root_dir:"$(ROOTDIR)"
    ()

let native_dynlink_f = As_features.(native_dynlink &&& native)

let byte_f = As_features.byte
let native_f = As_features.native
let js_f = As_features.js

let lib_dir =
  lazy (List.hd (As_shell.exec_output "ocamlfind printconf destdir"))

let mk_flags phase t =
  let suffix = As_flags.string_of_phase phase in
  let fn = As_flags.get phase in
  let var = As_project.Component.id t ^ "." ^ suffix in
  let global = match As_project.Component.container t with
  | None   -> [sprintf "$(%s)" suffix]
  | Some c -> [sprintf "$(%s.%s)" (As_project.Container.id ~all:false c) suffix]
  in
  let flags = global @ fn (As_project.Component.flags t resolver) in
  Variable.(var =?= `Strings flags)

(* Replace all flags value with a flag variable in which the value is
   stored. *)
let meta_flags t =
  let phases = As_project.Component.phases t in
  List.fold_left (fun acc phase ->
      let open As_flags in
      acc @@@
      v phase [sprintf "$(%s.%s)"
                 (As_project.Component.id t)
                 (As_flags.string_of_phase phase)]
    ) As_flags.empty phases

let mk_rule t rule =
  let targets = As_project.Rule.files t resolver rule.As_action.targets in
  let prereqs, order_only_prereqs =
    List.partition (function
      | `Self `Dir | `N (_, `Dir) -> false
      | _ -> true
      ) rule.As_action.prereqs
  in
  let prereqs = As_project.Rule.files t resolver prereqs in
  let order_only_prereqs = As_project.Rule.files t resolver order_only_prereqs in
  let action = As_action.run rule.As_action.action t resolver (meta_flags t) in
  let echo result =
    let color = match result with `Ok -> `Green | `Error -> `Red in
    sprintf "echo '%-45s %-20s %s'"
      (As_shell.color color (As_project.Component.id t))
      (As_shell.color `Bold ((As_flags.string_of_phase rule.As_action.phase)))
      (String.concat " " (List.map Filename.basename targets))
  in
  let action = match action with
  | []   -> []
  | h::t ->
      ("$(QUIET)(" ^ h ^ " \\")
      :: List.map (fun x -> "&&" ^ x ^ "\\") t
      @ [ sprintf " && %s) \\" (echo `Ok);
          "|| (" ^ echo `Error ^ " && exit 2)" ]

  in
  Rule.create ~targets ~prereqs ~order_only_prereqs action

module type S = sig
  type t
  val rules    : t list -> Rule.t list
  val variables: t list -> Variable.stanza list
end

module Unit: S with type t = As_project.comp_unit = struct

  type t = As_project.comp_unit

  let variable t =
    let c = `Unit t in
    let name = match As_project.Unit.source_dir t with
    | None   -> As_project.Unit.name t
    | Some d -> d / As_project.Unit.name t in
    let phases = As_project.Component.phases c in
    match As_project.Unit.kind t with
    | `OCaml ->
        Variable.stanza
          ~doc:[sprintf "Compilation unit: %s" name]
          (List.map (fun phase -> mk_flags phase c) phases)
    | `C ->
        Variable.stanza
          ~doc:[sprintf "C file: %s" name]
          [mk_flags (`Compile `C) (`Unit t)]
    | `Js ->
        Variable.stanza
          ~doc:[sprintf "JS file: %s" name]
          [mk_flags (`Compile `Js) (`Unit t)]

  let variables ts = List.map variable ts
  let rule t = List.map (mk_rule (`Unit t)) (As_project.Unit.rules t)
  let rules ts = conmap rule ts
end

module Other: S with type t = As_project.other = struct
  type t = As_project.other
  let variables _ = []
  let rules _ = []
end

module Pkg: S with type t = As_project.pkg = struct
  type t = As_project.pkg
  let variables _ = []
  let rules _ = []
end

module Lib: S with type t = As_project.lib = struct

  type t = As_project.lib

  let variable t =
    Variable.stanza
      ~doc:[sprintf "Library: %s" (As_project.Lib.name t)]
      (let c = `Lib t in
       let cma = As_project.Component.file c resolver `Cma in
       let a = As_project.Component.file c resolver `A in
       let cmxa = As_project.Component.file c resolver `Cmxa in
       let cmxs = As_project.Component.file c resolver `Cmxs in
       let byte = [ byte_f, `Strings [cma] ] in
       let native = [ native_f, `Strings [a; cmxa] ] in
       let native_dynlink = [ native_dynlink_f, `Strings [cmxs] ] in
       let id = As_project.Lib.id t in
       let phases = As_project.Component.phases c
       in Variable.(id =:= case (As_project.Lib.available t) byte)
       :: Variable.(id =+= case (As_project.Lib.available t) native)
       :: Variable.(id =+= case (As_project.Lib.available t) native_dynlink)
       :: List.map (fun phase -> mk_flags phase c) phases)

  let variables ts =
    let targets = List.map As_project.Lib.id ts in
    Variable.stanza [Variable.("lib" =:= `Strings targets)]
    :: List.map variable ts

  let rule t =
    let id = As_project.Component.id (`Lib t) in
    Rule.create ~targets:[id] ~prereqs:[sprintf "$(%s)" id] []
    :: List.map (mk_rule (`Lib t)) (As_project.Lib.rules t)

  let rules ts =
    Rule.create ~targets:["lib"] ~prereqs:["$(lib)"] []
    :: conmap rule ts

end

module Bin: S with type t = As_project.bin = struct

  type t = As_project.bin

  let variable t =
    let c = `Bin t in
    Variable.stanza
      ~doc:[sprintf "Binary: %s" (As_project.Bin.name t)]
      (let byte = As_project.Component.file c resolver `Byte in
       let native = As_project.Component.file c resolver `Native in
       let js = As_project.Component.file c resolver `Js in
       let byte = [ byte_f, `Strings [byte] ] in
       let native = [ native_f, `Strings [native] ] in
       let js = [ js_f, `Strings [js] ] in
       let id = As_project.Bin.id t in
       let phases = As_project.Component.phases c
       in Variable.(id =:= case (As_project.Bin.available t) byte)
       :: Variable.(id =+= case (As_project.Bin.available t) native)
       :: Variable.(id =+= case (As_project.Bin.available t) js)
       :: List.map (fun phase -> mk_flags phase c) phases)

  let variables ts =
    let targets = List.map As_project.Bin.id ts in
    let js_targets =
      List.filter As_project.Bin.js ts
      |> List.map As_project.Bin.id in
    Variable.stanza [Variable.("bin" =:= `Strings targets)]
    :: Variable.stanza [Variable.("js" =:= `Strings js_targets)]
    :: List.map variable ts

  let rule t =
    let id = As_project.Component.id (`Bin t) in
    Rule.create ~targets:[id] ~prereqs:[sprintf "$(%s)" id] []
    :: List.map (mk_rule (`Bin t)) (As_project.Bin.rules t)

  let rules ts =
    Rule.create ~targets:["bin"] ~prereqs:["$(bin)"] []
    :: Rule.create ~targets:["js"] ~prereqs:["$(js)"] []
    :: conmap rule ts

end

module Dir: S with type t = As_project.dir = struct
  type t = As_project.dir
  let variable t =
    let c = `Dir t in
    let phases = As_project.Component.phases c in
    Variable.stanza
      ~doc:[sprintf "Directory: %s" (As_project.Dir.name t)]
      (List.map (fun phase -> mk_flags phase c) phases)
  let variables = List.map variable
  let rules _ = []
end

module Test: S with type t = As_project.test = struct

  type t = As_project.test

  let variables ts =
    let targets = List.map As_project.Test.id ts in
    [Variable.stanza [Variable.("test" =:= `Strings targets)]]

  let rule t =
    let id = As_project.Component.id (`Test t) in
    Rule.create ~targets:[id] ~prereqs:[sprintf "$(%s)" id] []
    :: List.map (mk_rule (`Test t)) (As_project.Test.rules t)

  let rules ts =
    Rule.create ~targets:["test"] ~prereqs:["$(test)"] []
    :: conmap rule ts

end

module Doc: S with type t = As_project.doc = struct

  type t = As_project.doc

  let variable t =
    let c = `Doc t in
    Variable.stanza
      ~doc:[sprintf "Documentation: %s" (As_project.Doc.name t)]
      (let id = As_project.Doc.id t in
       let phony = [As_project.Component.available c, `String (id ^ ".phony")]
       in Variable.(id =:= case (As_project.Doc.available t) phony)
       :: mk_flags `Doc c
       :: [])

  let variables ts =
    let targets = List.map As_project.Doc.id ts in
    Variable.stanza [Variable.("doc" =:= `Strings targets)]
    :: List.map variable ts

  let rule t =
    let id = As_project.Component.id (`Doc t) in
    Rule.create ~targets:[id] ~prereqs:[sprintf "$(%s)" id] []
    :: List.map (mk_rule (`Doc t)) (As_project.Doc.rules t)

  let rules ts =
    Rule.create ~targets:["doc"] ~prereqs:["$(doc)"] []
    :: conmap rule ts

end

let variables ts =
  let open As_project.Component in
  Lib.variables (filter lib ts) @
  Bin.variables (filter bin ts) @
  Test.variables (filter test ts) @
  Doc.variables (filter doc ts) @
  Dir.variables (filter dir ts) @
  Unit.variables (filter unit ts) @
  Other.variables (filter other ts) @
  Pkg.variables (filter pkg ts)

let rules ts =
  let open As_project.Component in
  Lib.rules (filter lib ts) @
  Bin.rules (filter bin ts) @
  Test.rules (filter test ts) @
  Doc.rules (filter doc ts) @
  Unit.rules (filter unit ts) @
  Other.rules (filter other ts) @
  Pkg.rules (filter pkg ts)

let global_variables flags =
  let debug = As_features.debug_atom, As_flags.debug in
  let annot = As_features.annot_atom, As_flags.annot in
  let warn_error = As_features.warn_error_atom, As_flags.warn_error in
  let init phase = match As_flags.get phase flags with
  | [] -> Variable.(As_flags.string_of_phase phase =:= `Strings [])
  | l  -> Variable.(As_flags.string_of_phase phase =:= `Strings l) in
  let add phase (feature, flags) =
    let feature = Variable.has_feature feature in
    let var = As_flags.string_of_phase phase in
    Variable.(var =+= `Case [
        [feature, "1"], `Strings (As_flags.get phase flags)
      ]) in
  let vars =
    init (`Compile `Byte)
    :: init (`Compile `Native)
    :: init (`Link `Byte)
    :: init (`Link `Native)
    :: add (`Compile `Byte) debug
    :: add (`Compile `Byte) annot
    :: add (`Compile `Byte) warn_error
    :: add (`Compile `Native) debug
    :: add (`Link `Byte) debug
    :: add (`Link `Native) debug
    :: []
  in
  Variable.stanza ~align:true ~simplify:true vars

let of_project ?(buildir="_build") ?(makefile="Makefile") ~flags ~features t =
  let components = As_project.components t in
  let all = Variable.("all" =:= `String "lib bin doc test") in
  let global_variables = global_variables flags in
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
    let check name =
      let native = [ (Variable.has_feature As_features.native_atom, "1") ] in
      `Case [ (native, `String (name ^ ".opt")); ([], `String name) ] in
    Variable.stanza ~doc:[""; "Main project configuration"; ""] []
    :: Variable.stanza
      ~align:true
      Variable.([
          ("QUIET"       =?= `String "@");
          ("BUILDIR"     =?= `String buildir);
          ("LIBDIR"      =?= `String (As_resolver.lib_dir resolver));
          Variable.shell "ROOTDIR" "pwd";
          ("OCAMLOPT"    =?= check "ocamlopt");
          ("OCAMLC"      =?= check "ocamlc");
          ("OCAMLDEP"    =?= check "ocamldep");
          ("OCAMLMKLIB"  =?= `String "ocamlmklib");
          ("CAMLP4O"     =?= `String "camlp4o");
          ("OCAMLDOC"    =?= check "ocamldoc");
          ("JS_OF_OCAML" =?= `String "js_of_ocaml");
          ("LN"          =?= `String "ln -sf");
          ("MKDIR"       =?= `String "mkdir -p");
        ])
    :: Variable.stanza ~align:true feature_variables
    :: Variable.stanza ~doc:[""; "Global variables"; ""] [all]
    :: global_variables
    :: Variable.stanza
      ~doc:["";
            "Component configuration.";
            "";
            "Each component has variables associated to the different phases of the build.";
            "<NAME>.<PHASE> controls the compilation options for the component <NAME>,";
            "during the phase <PHASE>.";
            "";
           ] []
    :: variables components
  in
  let rules = rules components in
  let main =
    Rule.create ~ext:true ~targets:["all"] ~prereqs:[] [
      sprintf "@echo '%s %s ${all}'"
        (As_shell.color `Underline "all")
        (As_shell.color `Yellow "=>");
      sprintf "@$(MAKE) $(all)";
      sprintf "@if [ \"x${HAS_JS}\" = \"x1\" ]; then $(MAKE) js; fi";
      sprintf "@if [ \"x${HAS_TEST}\" = \"x1\" ]; then $(MAKE) test; fi";
      sprintf "@if [ \"x${HAS_DOC}\" = \"x1\" ]; then $(MAKE) doc; fi";
      sprintf "@if [ \"x${HAS_FULL_DOC}\" = \"x1\" ]; then $(MAKE) full-doc; fi";
      sprintf "@echo '\027[32m== Done!\027[m'";
    ] in
  let clean =
    Rule.create ~ext:true ~targets:["clean"] ~prereqs:[] [
      "rm -f *~ **/*~";
      sprintf "rm -rf $(BUILDIR)";
    ] in
  let distclean =
    Rule.create ~ext:true ~targets:["distclean"] ~prereqs:["clean"] [
      sprintf "rm -f %s %s.install META" makefile  (As_project.name t)
    ] in
  let install =
    Rule.create ~ext:true ~targets:["install"] ~prereqs:["all"] [
      sprintf "@opam-installer --prefix $(shell opam config var prefix) \
               %s.install" (As_project.name t)
    ] in
  let help =
    Rule.create ~targets:["help"] ~prereqs:[]
      ([sprintf "@echo 'The following targets are available (use \"make %s\"):'"
          (As_shell.color `Underline "<target>");
        "@echo";
        sprintf "@echo ' - %s -- build all the active targets.'"
          (As_shell.color `Underline "all")]
       @ (List.fold_left (fun acc -> function
         | `Lib _ | `Bin _ as c ->
             sprintf "@echo ' - %s -- build the %s %s.'"
               (As_shell.color `Underline (As_project.Component.id c))
               (match c with `Lib _ -> "library"
                           | `Bin _ -> "executable"
                           | _ -> "component")
               (As_project.Component.name c) :: acc
         | _ -> acc
         ) [] components
          |> List.rev)
       @ [sprintf "@echo ' - %s -- build the documentation.'"
            (As_shell.color `Underline "doc");
          sprintf "@echo ' - %s -- build and run the test.'"
            (As_shell.color `Underline "test");
          sprintf "@echo ' - %s -- build the js_of_ocaml targets.'"
            (As_shell.color `Underline "js");
          sprintf "@echo ' - %s -- clean the build artefacts.'"
            (As_shell.color `Underline "clean");
          sprintf "@echo ' - %s -- clean the project to prepare the release.'"
            (As_shell.color `Underline "distclean");
          "@echo";
          "@echo";
          sprintf "@echo 'Current configuration (use \"make %s\" to modify):'"
            (As_shell.color `Underline "VAR=val");
          "@echo"; ]
       @ List.map (fun f ->
           let v = Variable.has_feature f in
           let k_v = v.Variable.name ^ "=" ^ Variable.name v in
           sprintf "@echo ' - %s -- %s'"
             (As_shell.color `Underline k_v) (As_features.doc_of f)
         ) project_features
       @ [ "@echo" ]
      ) in
  let phony =
    ["all"; "clean"; "lib"; "bin"; "test"; "doc";
     "distclean"; "js"; "help"] @
    (List.fold_left (fun acc c -> match c with
       | `Lib _ | `Bin _ | `Doc _ | `Test _ -> As_project.Component.id c :: acc
       | _ -> acc
       ) [] components
     |> List.rev)
  in
  create
    ~phony
    makefile
    variables
    (main :: clean :: distclean :: install :: help :: rules)
