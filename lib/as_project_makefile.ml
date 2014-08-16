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

let native_dynlink_f = As_features.(native_dynlink &&& native)
let byte_f = As_features.byte
let native_f = As_features.native
let js_f = As_features.js

let bool_true = "true"
let bool_false = "false"
let has_feature f =
  let var = String.uppercase (As_features.name f) in
  let var = String.map (function '-' -> '_' | x -> x) var in
  let bool_val = if As_features.default f then bool_true else bool_false in
  As_makefile.Var.("HAS_" ^ var =?= `String bool_val)

let case available cs = (* build the full handler cases *)
  let guard features = match As_features.cnf features with
  | `Conflict -> failwith "invalid handler case"
  | `And l    ->
      List.map (function
        | `P f -> has_feature f, bool_true
        | `N f -> has_feature f, bool_false
        ) l
  in
  let cs = List.filter (fun (f,_) ->
      As_features.(cnf (available &&& f)) <> `Conflict
    ) cs in
  `Case (List.map (fun (f, c) -> guard f, c) cs)

let mk_flags r phase t =
  let suffix = As_flags.string_of_phase phase in
  let fn = As_flags.get phase in
  let var = As_project.Component.id t ^ "." ^ suffix in
  let global = match As_project.Component.parent t with
  | None   -> [sprintf "$(%s)" suffix]
  | Some p -> [sprintf "$(%s.%s)" (As_project.Component.id p) suffix]
  in
  let flags = global @ fn (As_project.Component.flags ~all:false t r) in
  As_makefile.Var.(var =?= `Strings flags)

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
  let prereqs, oo_prereqs =
    List.partition (function
      | `Self `Dir | `N (_, `Dir) -> false
      | `N (`Unit _, (`Ml|`Mli)) when compute_deps -> false
      | _ -> true
      ) rule.As_action.prereqs
  in
  let prereqs = As_project.Rule.files t resolver prereqs in
  let order_only_prereqs = As_project.Rule.files t resolver oo_prereqs in
  let action = As_action.run rule.As_action.action t resolver (meta_flags t) in
  let long = String.concat " " action in
  let short =
    sprintf "%-25s %s %s %s"
      (String.concat " " (List.map Filename.basename targets))
      (As_shell.color `Green "<=")
      (As_shell.color `Bold ((As_flags.string_of_phase rule.As_action.phase)))
      (As_project.Component.id t)
  in
  let action = match action with
  | [] -> []
  | _  ->
      sprintf "@if test -n \"$$VERBOSE\"; \\\n\
               \        then echo '%s'; \\\n\
               \        else echo '%s'; fi"
        long short
      :: (List.map (fun x -> "@" ^ x) action)
  in
  As_makefile.Rule.create ~targets ~prereqs ~order_only_prereqs action

module type S = sig
  type t
  val rules    : As_resolver.t -> t list -> As_makefile.Rule.t list
  val variables: As_resolver.t -> t list -> As_makefile.Var.stanza list
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
        As_makefile.Var.stanza
          ~doc:[sprintf "Compilation unit: %s" name]
          (List.map (fun phase -> mk_flags r phase c) phases)
    | `C ->
        As_makefile.Var.stanza
          ~doc:[sprintf "C file: %s" name]
          [mk_flags r (`Compile `C) (`Unit t)]
    | `Js ->
        As_makefile.Var.stanza
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

  let mk_flags r pkg phase =
    let suffix = As_flags.string_of_phase phase in
    let var = As_project.Component.id pkg ^ "." ^ suffix in
    let flags = As_project.Component.flags ~all:false pkg r in
    As_makefile.Var.(var =?= `Strings (As_flags.get phase flags))

  let variable r p =
    let p = `Pkg p in
    let doc = [sprintf "Package: %s" (As_project.Component.name p)] in
    let phases = As_project.Component.phases p in
    let vars = List.map (fun phase -> mk_flags r p phase) phases in
    As_makefile.Var.stanza ~doc vars

  let variables r pkgs  = List.map (variable r) pkgs
  let rules _ _ = []
end

module Lib: S with type t = As_project.lib = struct

  type t = As_project.lib

  let variable r t =
    As_makefile.Var.stanza
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
       let phases = As_project.Component.phases c in
       let open As_makefile.Var in
       (id =:= `Strings [])
       :: (id =+= case (As_project.Component.available c) byte)
       :: (id =+= case (As_project.Component.available c) native)
       :: (id =+= case (As_project.Component.available c) native_dynlink)
       :: (List.map (fun phase -> mk_flags r phase c) phases))

  let variables r ts =
    let targets = List.map (fun t -> As_project.Component.id (`Lib t)) ts in
    As_makefile.Var.(stanza ["lib" =:= `Strings targets])
    :: List.map (variable r) ts

  let rule r t =
    let c = `Lib t in
    let id = As_project.Component.id c in
    As_makefile.Rule.create ~targets:[id] ~prereqs:[sprintf "$(%s)" id] []
    :: List.map (mk_rule r c) (As_project.Component.rules c)

  let rules r ts =
    As_makefile.Rule.create ~targets:["lib"] ~prereqs:["$(lib)"] []
    :: conmap (rule r) ts

end

module Bin: S with type t = As_project.bin = struct

  type t = As_project.bin

  let variable r t =
    let c = `Bin t in
    As_makefile.Var.stanza
      ~doc:[sprintf "Binary: %s" (As_project.Component.name c)]
      (let byte = As_project.Component.file c r `Byte in
       let native = As_project.Component.file c r `Native in
       let js = As_project.Component.file c r `Js in
       let byte = [ byte_f, `Strings [byte] ] in
       let native = [ native_f, `Strings [native] ] in
       let js = [ js_f, `Strings [js] ] in
       let id = As_project.Component.id c in
       let phases = As_project.Component.phases c in
       let open As_makefile.Var in
       (id =:= `Strings [])
       :: (id =+= case (As_project.Component.available c) byte)
       :: (id =+= case (As_project.Component.available c) native)
       :: (id =+= case (As_project.Component.available c) js)
       :: List.map (fun phase -> mk_flags r phase c) phases)

  let variables r ts =
    let targets = List.map (fun t -> As_project.Component.id (`Bin t)) ts in
    let js_targets =
      List.filter As_project.Bin.js ts
      |> List.map (fun t -> As_project.Component.id (`Bin t))
    in
    let open As_makefile.Var in
    (stanza ["bin" =:= `Strings targets])
    :: (stanza ["js" =:= `Strings js_targets])
    :: List.map (variable r) ts

  let rule r t =
    let c = `Bin t in
    let id = As_project.Component.id c in
    As_makefile.Rule.create ~targets:[id] ~prereqs:[sprintf "$(%s)" id] []
    :: List.map (mk_rule r c) (As_project.Component.rules c)

  let rules r ts =
    As_makefile.Rule.create ~targets:["bin"] ~prereqs:["$(bin)"] []
    :: As_makefile.Rule.create ~targets:["js"] ~prereqs:["$(js)"] []
    :: conmap (rule r) ts

end

module Container: S with type t = As_project.container = struct
  type t = As_project.container
  let variable r t =
    let c = `Container t in
    let phases = As_project.Component.phases c in
    As_makefile.Var.stanza
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
    As_makefile.Var.([stanza ["test" =:= `Strings targets]])

  let rule r t =
    let c = `Test t in
    let id = As_project.Component.id c in
    As_makefile.Rule.create ~targets:[id] ~prereqs:[sprintf "$(%s)" id] []
    :: List.map (mk_rule r c) (As_project.Component.rules c)

  let rules r ts =
    As_makefile.Rule.create ~targets:["test"] ~prereqs:["$(test)"] []
    :: conmap (rule r) ts

end

module Doc: S with type t = As_project.doc = struct

  type t = As_project.doc

  let variable r t =
    let c = `Doc t in
    As_makefile.Var.stanza
      ~doc:[sprintf "Documentation: %s" (As_project.Component.name c)]
      [mk_flags r `Doc c]

  let variables r ts =
    let add_docs = List.map (fun d ->
        let c = `Doc d in
        As_makefile.Var.("doc" =+= case As_features.true_
                                [As_project.Component.available c,
                                 `String (As_project.Component.id c)])) ts
    in
    As_makefile.Var.(stanza (("doc" =:= `Strings []) :: add_docs))
    :: List.map (variable r) ts

  let rule r t =
    let c = `Doc t in
    List.map (mk_rule r c) (As_project.Component.rules c)

  let rules r ts =
    As_makefile.Rule.create ~targets:["doc"] ~prereqs:["$(doc)"] []
    :: conmap (rule r) ts
end

let variables r ts =
  let open As_project.Component in
  Pkg.variables r (filter_map pkg ts) @
  Lib.variables r (filter_map lib ts) @
  Bin.variables r (filter_map bin ts) @
  Test.variables r (filter_map test ts) @
  Doc.variables r (filter_map doc ts) @
  Container.variables r (filter_map container ts) @
  Unit.variables r (filter_map unit ts) @
  Other.variables r (filter_map other ts)

let rules r ts =
  let open As_project.Component in
  Lib.rules r (filter_map lib ts) @
  Bin.rules r (filter_map bin ts) @
  Test.rules r (filter_map test ts) @
  Doc.rules r (filter_map doc ts) @
  Container.rules r (filter_map container ts) @
  Unit.rules r (filter_map unit ts) @
  Other.rules r (filter_map other ts) @
  Pkg.rules r (filter_map pkg ts)

let global_variables flags =
  let debug = As_features.debug_atom, As_flags.debug in
  let annot = As_features.annot_atom, As_flags.annot in
  let warn_error = As_features.warn_error_atom, As_flags.warn_error in
  let init phase = match As_flags.get phase flags with
  | [] -> As_makefile.Var.(As_flags.string_of_phase phase =:= `Strings [])
  | l  -> As_makefile.Var.(As_flags.string_of_phase phase =:= `Strings l)
  in
  let add phase (feature, flags) =
    let feature = has_feature feature in
    let var = As_flags.string_of_phase phase in
    As_makefile.Var.(var =+= `Case [
        [feature, bool_true], `Strings (As_flags.get phase flags)
      ]) in
  let vars =
    As_makefile.Var.("all" =:= `String "lib bin")
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
  As_makefile.Var.stanza ~align:true ~simplify:true vars

let of_project ?(buildir="_build") ?(makefile="Makefile") ~flags ~features
    ~dumpast t =
  let preprocessor = if dumpast then (
      if not (As_shell.try_exec "ocaml-dumpast -v") then
        As_shell.fatal_error 1
          "ocaml-dumpast is not installed. \
           Use `assemblage setup --dumpast=false` \
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
    [ sprintf "# Generated by Assemblage for %s %s.\n"
        (As_project.name t)
        (As_project.version t);
      "\n";
      "# Run `make help' to get the list of targets.\n\n"; ]
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
    |> List.map has_feature in
  let variables =
    let check name =
      let native = [ (has_feature
                        As_features.native_toolchain_atom,
                      bool_true) ]
      in
      `Case [ (native, `String (name ^ ".opt")); ([], `String name) ] in
    As_makefile.Var.stanza ~doc:[""; "Main project configuration"; ""] []
    :: As_makefile.Var.stanza
      ~align:true ~simplify:true
      As_makefile.Var.([
          ("BUILDIR"     =?= `String buildir);
          ("LIBDIR"      =?= `String (As_resolver.lib_dir resolver));
          shell "ROOTDIR" "pwd";
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
    :: As_makefile.Var.stanza ~align:true feature_variables
    :: As_makefile.Var.stanza ~doc:[""; "Global variables"; ""] []
    :: global_variables
    :: As_makefile.Var.stanza
      ~doc:["";
            "Component configuration.";
            "";
            "Each component has variables associated to the different \
             phases of the build.";
            "<NAME>.<PHASE> controls the compilation options for the \
             component <NAME>,";
            "during the phase <PHASE>.";
            "";
           ] []
    :: variables resolver components
  in
  let rules = rules resolver components in
  let main =
    As_makefile.Rule.create ~ext:true ~targets:["all"] ~prereqs:[] [
      sprintf "@echo '%s %s ${all}'"
        (As_shell.color `Underline "all")
        (As_shell.color `Blue "=>");
      sprintf "@$(MAKE) $(all)";
      sprintf "@if [ \"x${HAS_JS}\" = \"x1\" ]; then $(MAKE) js; fi";
      sprintf "@if [ \"x${HAS_TEST}\" = \"x1\" ]; then $(MAKE) test; fi";
      sprintf "@if [ \"x${HAS_DOC}\" = \"x1\" ]; then $(MAKE) doc; fi";
      sprintf "@if [ \"x${HAS_FULL_DOC}\" = \"x1\" ]; then $(MAKE) full-doc; fi";
      sprintf "@echo '%s Done!'" (As_shell.color `Green "==>");
    ] in
  let clean =
    As_makefile.Rule.create ~ext:true ~targets:["clean"] ~prereqs:[] [
      "rm -f *~ **/*~";
      sprintf "rm -rf $(BUILDIR)";
    ] in
  let distclean =
    As_makefile.Rule.create ~ext:true ~targets:["distclean"]
      ~prereqs:["clean"] [
      sprintf "rm -f %s %s.install META" makefile  (As_project.name t)
    ] in
  let install =
    As_makefile.Rule.create ~ext:true ~targets:["install"] ~prereqs:["all"] [
      sprintf "@opam-installer --prefix $(shell opam config var prefix) \
               %s.install" (As_project.name t)
    ] in
  let help =
    As_makefile.Rule.create ~targets:["help"] ~prereqs:[]
      ([sprintf "@echo 'Use %s to show the full commands.'"
          (As_shell.color `Underline "VERBOSE=true");
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
            (As_shell.color `Underline "VAR=BOOL");
          "@echo"; ]
       @ List.map (fun f ->
           let v = has_feature f in
           let k_v = As_makefile.Var.(name v ^ "=" ^ ref v) in
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
    let units = As_project.Component.(filter_map unit) components in
    ([], ["Makefile.assemble"]) ::
    [["clean"; "help"; "distclean"],
     conmap (fun u ->
         let mk f =
           if not (As_project.Unit.has (f:>As_action.file) u) then []
           else [As_project.Component.file (`Unit u) resolver (`Dep f)]
         in
         mk `Ml @ mk `Mli
       ) units]
  in
  As_makefile.create ~headers ~phony ~opt_includes variables
    (main :: clean :: distclean :: install :: help :: rules)
