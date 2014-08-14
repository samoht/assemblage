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

module StringSet = Set.Make (String)

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

  let bool_true = "true"
  let bool_false = "false"
  let has_feature f =
    let var = String.uppercase (As_features.name f) in
    let var = String.map (function '-' -> '_' | x -> x) var in
    let bool_val = if As_features.default f then bool_true else bool_false in
    ("HAS_" ^ var) =?= `String bool_val

  (* build the guard pattern *)
  let guard features =
    match As_features.cnf features with
    | `Conflict -> failwith "invalid handler case"
    | `And l    ->
      List.map (function
          | `P f -> has_feature f, bool_true
          | `N f -> has_feature f, bool_false
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
      List.map (fun (g,_) -> string_of_guard g) l
      |> String.concat "|" in
    String.compare (mk o2) (mk o1)

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

  let stanza ?(align=false) ?(simplify=false) ?(doc=[]) variables =
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
  headers: string list;
  includes: string list;
  opt_includes: (string list * string list) list;
  phony: string list;
  variables: Variable.stanza list;
  rules: Rule.t list;
}

let create
    ?(headers=[])
    ?(includes=[])
    ?(opt_includes=[])
    ?(phony=[])
    makefile variables rules =
  let opt_includes = ([], ["Makefile.assemble"]) :: opt_includes in
  { makefile; phony; headers; variables; rules; includes; opt_includes }

let write t =
  printf "%s write %s\n" (As_shell.color `Green "==>") t.makefile;
  let buf = Buffer.create 1024 in
  List.iter (Buffer.add_string buf) t.headers;
  Buffer.add_string buf "\n";
  bprintf buf "# Run `make help' to get the list of targets.\n\n";
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
  if t.opt_includes <> [] then (
    let with_guards, no_guards =
      List.partition (function (g,_) -> g<>[]) t.opt_includes
    in
    let incl files = bprintf buf "-include %s\n" (String.concat " " files) in
    incl (conmap snd no_guards);
    List.iter (fun (guards, files) ->
        bprintf buf "ifneq ($(filter-out %s,$(MAKECMDGOALS)),)\n" (String.concat " " guards);
        incl files;
        bprintf buf "endif\n"
      ) with_guards
  );
  if t.includes <> [] then
    bprintf buf "include %s\n" (String.concat " " t.includes);
  let oc = open_out t.makefile in
  output_string oc (Buffer.contents buf);
  close_out oc

(******************************************************************************)

let native_dynlink_f = As_features.(native_dynlink &&& native)

let byte_f = As_features.byte
let native_f = As_features.native
let js_f = As_features.js

let mk_flags r phase t =
  let suffix = As_flags.string_of_phase phase in
  let fn = As_flags.get phase in
  let var = As_project.Component.id t ^ "." ^ suffix in
  let global = match As_project.Component.parent t with
  | None   -> [sprintf "$(%s)" suffix]
  | Some p -> [sprintf "$(%s.%s)" (As_project.Component.id ~all:false p) suffix]
  in
  let flags = global @ fn (As_project.Component.flags t r) in
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

let mk_rule resolver t rule =
  let targets = As_project.Rule.files t resolver rule.As_action.targets in
  let compute_deps = match rule.As_action.targets with
  | [ `Self (`Dep _) ] -> true
  | _ -> false  in
  let prereqs, order_only_prereqs =
    List.partition (function
      | `Self `Dir | `N (_, `Dir) -> false
      | `N (`Unit _, (`Ml|`Mli)) when compute_deps -> false
      | _ -> true
      ) rule.As_action.prereqs
  in
  let prereqs = As_project.Rule.files t resolver prereqs in
  let order_only_prereqs = As_project.Rule.files t resolver order_only_prereqs in
  let action = As_action.run rule.As_action.action t resolver (meta_flags t) in
  let long = String.concat " " action in
  let short =
    sprintf "%-45s %-30s %s"
      (As_shell.color `Yellow (As_project.Component.id t))
      (As_shell.color `Bold ((As_flags.string_of_phase rule.As_action.phase)))
      (String.concat " " (List.map Filename.basename targets))
  in
  let action = match action with
  | [] -> []
  | _  ->
      sprintf "@if test -n \"$$VERBOSE\"; then echo '%s'; else echo '%s'; fi"
        long short
      :: (List.map (fun x -> "@" ^ x) action)
  in
  Rule.create ~targets ~prereqs ~order_only_prereqs action

module type S = sig
  type t
  val rules    : As_resolver.t -> t list -> Rule.t list
  val variables: As_resolver.t -> t list -> Variable.stanza list
end

module Unit: S with type t = As_project.comp_unit = struct

  type t = As_project.comp_unit

  let variable r t =
    let c = `Unit t in
    let name = match As_project.Unit.source_dir t with
    | None   -> As_project.Component.name (`Unit t)
    | Some d -> d / As_project.Component.name (`Unit t) in
    let phases = As_project.Component.phases c in
    match As_project.Unit.kind t with
    | `OCaml ->
        Variable.stanza
          ~doc:[sprintf "Compilation unit: %s" name]
          (List.map (fun phase -> mk_flags r phase c) phases)
    | `C ->
        Variable.stanza
          ~doc:[sprintf "C file: %s" name]
          [mk_flags r (`Compile `C) (`Unit t)]
    | `Js ->
        Variable.stanza
          ~doc:[sprintf "JS file: %s" name]
          [mk_flags r (`Compile `Js) (`Unit t)]

  let variables r ts = List.map (variable r) ts

  let rule r t =
    let c = `Unit t in
    List.map (mk_rule r c) (As_project.Component.rules c)

  let rules r ts = conmap (rule r) ts
end

module Other: S with type t = As_project.other = struct
  type t = As_project.other
  let variables _ _ = []
  let rules _ _ = []
end

module Pkg: S with type t = As_project.pkg = struct
  type t = As_project.pkg
  let variables _ _ = []
  let rules _ _ = []
end

module Lib: S with type t = As_project.lib = struct

  type t = As_project.lib

  let variable r t =
    Variable.stanza
      ~doc:[sprintf "Library: %s" (As_project.Component.name (`Lib t))]
      (let c = `Lib t in
       let cma = As_project.Component.file c r `Cma in
       let a = As_project.Component.file c r `A in
       let cmxa = As_project.Component.file c r `Cmxa in
       let cmxs = As_project.Component.file c r `Cmxs in
       let byte = [ byte_f, `Strings [cma] ] in
       let native = [ native_f, `Strings [a; cmxa] ] in
       let native_dynlink = [ native_dynlink_f, `Strings [cmxs] ] in
       let id = As_project.Component.id c in
       let phases = As_project.Component.phases c
       in Variable.(id =:= `Strings [])
       :: Variable.(id =+= case (As_project.Component.available c) byte)
       :: Variable.(id =+= case (As_project.Component.available c) native)
       :: Variable.(id =+= case (As_project.Component.available c) native_dynlink)
       :: List.map (fun phase -> mk_flags r phase c) phases)

  let variables r ts =
    let targets = List.map (fun t -> As_project.Component.id (`Lib t)) ts in
    Variable.stanza [Variable.("lib" =:= `Strings targets)]
    :: List.map (variable r) ts

  let rule r t =
    let c = `Lib t in
    let id = As_project.Component.id c in
    Rule.create ~targets:[id] ~prereqs:[sprintf "$(%s)" id] []
    :: List.map (mk_rule r c) (As_project.Component.rules c)

  let rules r ts =
    Rule.create ~targets:["lib"] ~prereqs:["$(lib)"] []
    :: conmap (rule r) ts

end

module Bin: S with type t = As_project.bin = struct

  type t = As_project.bin

  let variable r t =
    let c = `Bin t in
    Variable.stanza
      ~doc:[sprintf "Binary: %s" (As_project.Component.name c)]
      (let byte = As_project.Component.file c r `Byte in
       let native = As_project.Component.file c r `Native in
       let js = As_project.Component.file c r `Js in
       let byte = [ byte_f, `Strings [byte] ] in
       let native = [ native_f, `Strings [native] ] in
       let js = [ js_f, `Strings [js] ] in
       let id = As_project.Component.id c in
       let phases = As_project.Component.phases c
       in Variable.(id =:= `Strings [])
       :: Variable.(id =+= case (As_project.Component.available c) byte)
       :: Variable.(id =+= case (As_project.Component.available c) native)
       :: Variable.(id =+= case (As_project.Component.available c) js)
       :: List.map (fun phase -> mk_flags r phase c) phases)

  let variables r ts =
    let targets = List.map (fun t -> As_project.Component.id (`Bin t)) ts in
    let js_targets =
      List.filter As_project.Bin.js ts
      |> List.map (fun t -> As_project.Component.id (`Bin t)) in
    Variable.stanza [Variable.("bin" =:= `Strings targets)]
    :: Variable.stanza [Variable.("js" =:= `Strings js_targets)]
    :: List.map (variable r) ts

  let rule r t =
    let c = `Bin t in
    let id = As_project.Component.id c in
    Rule.create ~targets:[id] ~prereqs:[sprintf "$(%s)" id] []
    :: List.map (mk_rule r c) (As_project.Component.rules c)

  let rules r ts =
    Rule.create ~targets:["bin"] ~prereqs:["$(bin)"] []
    :: Rule.create ~targets:["js"] ~prereqs:["$(js)"] []
    :: conmap (rule r) ts

end

module Container: S with type t = As_project.container = struct
  type t = As_project.container
  let variable r t =
    let c = `Container t in
    let phases = As_project.Component.phases c in
    Variable.stanza
      ~doc:[sprintf "Directory: %s" (As_project.Component.name c)]
      (List.map (fun phase -> mk_flags r phase c) phases)
  let variables r = List.map (variable r)

  let rule r t =
    let c = `Container t in
    List.map (mk_rule r c) (As_project.Component.rules c)

  let rules r ts = conmap (rule r) ts
end

module Test: S with type t = As_project.test = struct

  type t = As_project.test

  let variables _r ts =
    let targets = List.map (fun t -> As_project.Rule.phony_run (`Test t)) ts in
    [Variable.stanza [Variable.("test" =:= `Strings targets)]]

  let rule r t =
    let c = `Test t in
    let id = As_project.Component.id c in
    Rule.create ~targets:[id] ~prereqs:[sprintf "$(%s)" id] []
    :: List.map (mk_rule r c) (As_project.Component.rules c)

  let rules r ts =
    Rule.create ~targets:["test"] ~prereqs:["$(test)"] []
    :: conmap (rule r) ts

end

module Doc: S with type t = As_project.doc = struct

  type t = As_project.doc

  let variable r t =
    let c = `Doc t in
    Variable.stanza
      ~doc:[sprintf "Documentation: %s" (As_project.Component.name c)]
      [mk_flags r `Doc c]

  let variables r ts =
    let add_docs = List.map (fun d ->
        let c = `Doc d in
        Variable.("doc" =+= case As_features.true_
                    [As_project.Component.available c,
                     `String (As_project.Component.id c)])
      ) ts in
    Variable.stanza (Variable.("doc" =:= `Strings []) :: add_docs)
    :: List.map (variable r) ts

  let rule r t =
    let c = `Doc t in
    List.map (mk_rule r c) (As_project.Component.rules c)

  let rules r ts =
    Rule.create ~targets:["doc"] ~prereqs:["$(doc)"] []
    :: conmap (rule r) ts

end

let variables r ts =
  let open As_project.Component in
  Lib.variables r (filter lib ts) @
  Bin.variables r (filter bin ts) @
  Test.variables r (filter test ts) @
  Doc.variables r (filter doc ts) @
  Container.variables r (filter container ts) @
  Unit.variables r (filter unit ts) @
  Other.variables r (filter other ts) @
  Pkg.variables r (filter pkg ts)

let rules r ts =
  let open As_project.Component in
  Lib.rules r (filter lib ts) @
  Bin.rules r (filter bin ts) @
  Test.rules r (filter test ts) @
  Doc.rules r (filter doc ts) @
  Container.rules r (filter container ts) @
  Unit.rules r (filter unit ts) @
  Other.rules r (filter other ts) @
  Pkg.rules r (filter pkg ts)

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
        [feature, bool_true], `Strings (As_flags.get phase flags)
      ]) in
  let vars =
    Variable.("all" =:= `String "lib bin")
    :: init (`Compile `Byte)
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

let of_project ?(buildir="_build") ?(makefile="Makefile") ~flags ~features ~dumpast t =
  let preprocessor = if dumpast then (
      if not (As_shell.try_exec "ocaml-dumpast -v") then
        As_shell.fatal_error 1
          "ocaml-dumpast is not installed. Use `assemblage setup --dumpast=false` \
           to configure your project without it.";
      Some "$(DUMPAST) camlp4o"
    ) else None in
  let resolver =
    As_ocamlfind.resolver `Makefile
      ~ocamlc:"$(OCAMLC)"
      ~ocamlopt:"$(OCAMLOPT)"
      ~ocamldep:"$(OCAMLDEP)"
      ~ocamlmklib:"$(OCAMLMKLIB)"
      ~ocamldoc:"$(OCAMLDOC)"
      ~preprocessor
      ~js_of_ocaml:"$(JS_OF_OCAML)"
      ~ln:"$(LN)"
      ~mkdir:"$(MKDIR)"
      ~build_dir:"$(BUILDIR)"
      ~lib_dir:"$(LIBDIR)"
      ~root_dir:"$(ROOTDIR)"
      ()
  in
  let headers =
    [ sprintf "# Generated by Assemblage v%s.\n" (As_project.version t)]
  in
  let components = As_project.components t in
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
      let native = [ (Variable.has_feature As_features.native_atom,
                      Variable.bool_true) ]
      in
      `Case [ (native, `String (name ^ ".opt")); ([], `String name) ] in
    Variable.stanza ~doc:[""; "Main project configuration"; ""] []
    :: Variable.stanza
      ~align:true ~simplify:true
      Variable.([
          ("BUILDIR"     =?= `String buildir);
          ("LIBDIR"      =?= `String (As_resolver.lib_dir resolver));
          Variable.shell "ROOTDIR" "pwd";
          ("OCAMLOPT"    =?= check "ocamlopt");
          ("OCAMLC"      =?= check "ocamlc");
          ("OCAMLDEP"    =?= check "ocamldep");
          ("OCAMLMKLIB"  =?= `String "ocamlmklib");
          ("DUMPAST"     =?= `String "ocaml-dumpast");
          ("OCAMLDOC"    =?= check "ocamldoc");
          ("JS_OF_OCAML" =?= `String "js_of_ocaml");
          ("LN"          =?= `String "ln -sf");
          ("MKDIR"       =?= `String "mkdir -p");
        ])
    :: Variable.stanza ~align:true feature_variables
    :: Variable.stanza ~doc:[""; "Global variables"; ""] []
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
    :: variables resolver components
  in
  let rules = rules resolver components in
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
      ([sprintf "@echo 'Use %s to show the full commands.'"
          (As_shell.color `Underline "VERBOSE=1");
        sprintf "@echo 'The following targets are available (use \"make %s\"):'"
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
    (List.fold_left (fun acc c ->
         let id = As_project.Component.id c in
         match c with
         | `Lib _ | `Bin _  | `Doc _  -> id :: acc
         | `Test _ -> id :: As_project.Rule.phony_run c :: acc
         | _ -> acc
       ) [] components
     |> List.rev)
  in
  let opt_includes =
    let units = As_project.Component.(filter unit) components in
    [["clean"; "help"; "distclean"],
     conmap (fun u ->
         let mk f =
           if not (As_project.Unit.has (f:>As_action.file) u) then []
           else [As_project.Component.file (`Unit u) resolver (`Dep f)]
         in
         mk `Ml @ mk `Mli
       ) units]
  in
  create
    ~headers
    ~phony
    ~opt_includes
    makefile
    variables
    (main :: clean :: distclean :: install :: help :: rules)
