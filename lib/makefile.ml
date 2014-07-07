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

open Project
open Printf

let (/) x y = Filename.concat x y

let (//) x y =
  match x with
  | None   -> y
  | Some x -> Filename.concat x y

let conmap f l = List.concat (List.map f l)

module Variable = struct

  type assign = string

  type t = {
    name  : string;
    assign: assign;
    value : contents;
  }

  and contents =
    [ `String of string
    | `Strings of string list
    | `Case of ((t * string) list * contents) list ]

  let (=:=) name value =
    { name; value; assign = ":=" }

  let (=+=) name value =
    { name; value; assign = "+=" }

  let (=?=) name value =
    { name; value; assign = "?=" }

  let subst t name ~input ~output =
    { name; assign = "=";
      value = `String (sprintf "$(${%s}:%s=%s)" t.name input output)
    }

  let name t =
    sprintf "$(%s)" t.name

  let is_empty t =
    match t.value with
    | `String ""
    | `Strings [] -> true
    | `Strings l  -> List.for_all ((=) "") l
    | `Case _
    | `String _   -> false

  let has_feature f =
    let var = String.uppercase (Feature.name f) in
    for i = 0 to String.length var - 1 do
      match var.[i] with
      | '-' -> var.[i] <- '_'
      | _   -> ()
    done;
    ("HAS_" ^ var) =?= `String (if Feature.default f then "1" else "0")

  (* build one handler case *)
  let one_case features contents =
    match Feature.cnf features with
    | `Conflict -> failwith "invalid handler case"
    | `And l    ->
      List.map (function
          | `P f -> has_feature f, "1"
          | `N f -> has_feature f, "0"
        ) l,
      contents

  (* build the full handler cases *)
  let case available cs: contents =
    let cs = List.filter (fun (f,_) ->
        Feature.(cnf (available && f)) <> `Conflict
      ) cs in
    `Case (List.map (fun (f, c) -> one_case f c) cs)

  let generate buf ?(size=0) t =
    let string tab c =
      bprintf buf "%s%-.*s %s %s\n"
        tab (size - String.length tab) t.name t.assign c in
    let rec contents tab (t:contents) = match t with
      | `String s   -> string tab s
      | `Strings ss ->
        let sep = " \\\n" ^ String.make (size + 4) ' ' in
        string tab (String.concat sep ss)
      | `Case []        -> ()
      | `Case [vars, c] ->
        (* A single case handler *)
        begin match vars with
          | []   -> contents tab c
          | vars ->
            bprintf buf "ifeq (";
            List.iter (fun (var, b) ->
                bprintf buf "$(%s:%s=)" var.name b) vars;
            bprintf buf ",)\n";
            contents (tab ^ "  ") c;
            bprintf buf "endif\n"
        end
      | `Case cases ->
        (* A full case handler *)
        let rec aux = function
          | []                -> ()
          | (vars, c) :: rest ->
            match vars with
            | []  ->
              (* we assume that's the default case hanlder *)
              if rest <> [] then failwith "invalid default case";
              contents (tab ^ "  ") c;
              bprintf buf "endif\n"

            | vars ->
              bprintf buf "ifeq (";
              List.iter (fun (var, b) ->
                  bprintf buf "$(%s:%s=)" var.name b) vars;
              bprintf buf ",)\n";
              contents (tab ^ "  ") c;
              if List.length rest <= 1 then bprintf buf "else\n"
              else bprintf buf "else ";
              aux rest;
        in
        aux cases in
    contents "" t.value

  let generates buf ts =
    let size = List.fold_left (fun acc t ->
        max acc (String.length t.name)
      ) 0 ts in
    List.iter (generate buf ~size) ts

  let shell name command =
    { name; assign = "=";
      value = `String (sprintf "$(shell %s)" command)
    }

  let files name ~dir ~ext =
    { name; assign = "=";
      value = `String (sprintf "$(wildcard %s/*.%s)" dir ext) }

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
  variables: Variable.t list;
  rules: Rule.t list;
}

let create ?(header=[]) ?(phony=[]) makefile variables rules =
  { makefile; phony; header; variables; rules }

let write t =
  printf "\027[36m+ write %s\027[m\n" t.makefile;
  let buf = Buffer.create 1024 in
  bprintf buf "# Generated by ocaml-tools\n\n";
  List.iter (fun s ->
      Buffer.add_string buf s;
      Buffer.add_string buf "\n\n";
    ) t.header;
  let () = match t.phony with
    | [] -> ()
    | l  -> bprintf buf ".PHONY: %s\n\n" (String.concat " " l)
  in
  Variable.generates buf t.variables;
  bprintf buf "\n";
  List.iter (Rule.generate buf) t.rules;
  let oc = open_out t.makefile in
  output_string oc (Buffer.contents buf);
  close_out oc

(******************************************************************************)

let buildir = "$(BUILDIR)"

let echo_prereqs =
  sprintf "@echo '\027[36m== Building %s\027[m'" Rule.prereqs

let resolver =
  Ocamlfind.resolver `Makefile buildir

let native_dynlink_f = Feature.(native_dynlink && native)

let native_f = Feature.native

let comp_byte = "comp-byte"
let comp_opt  = "comp-opt"
let link_byte = "link-byte"
let link_opt  = "link-opt"
let pp_byte   = "pp-byte"
let pp_opt    = "pp-opt"
let deps_byte = "deps-byte"
let deps_opt  = "deps-opt"

module rec U: sig
  val rules    : comp -> Rule.t list
  val variables: comp -> Variable.t list
end = struct

  let pp suffix varlib varbin fn t =
    let var = Comp.id t ^ "." ^ suffix in
    let lib = match Comp.container t with
      | None          -> None
      | Some (`Lib l) -> Some (varlib l)
      | Some (`Bin b) -> Some (varbin b) in
    match lib, fn (Comp.flags t resolver) with
    | None  , [] -> None
    | Some l, [] -> if Variable.is_empty l then None else Some l
    | None  , u  -> Some (Variable.(var =?= `Strings u))
    | Some l, u  ->
      if Variable.is_empty l then
        Some (Variable.(var =?= `Strings u))
      else
        Some (Variable.(var =?= `Strings (Variable.name l :: u)))

  let flag suffix varlib varbin fn t =
    let var    = Comp.id t ^ "." ^ suffix in
    let global = match Comp.container t with
      | None          -> []
      | Some (`Lib l) -> [Variable.name (varlib l)]
      | Some (`Bin b) -> [Variable.name (varbin b)] in
    let flags = fn (Comp.flags t resolver) @ global in
    Variable.(var =?= `Strings flags)

  let pp_byte     = pp   pp_byte   L.pp_byte     B.pp_byte     Flags.pp_byte
  let pp_native   = pp   pp_opt    L.pp_native   B.pp_native   Flags.pp_native
  let comp_byte   = flag comp_byte L.comp_byte   B.comp_byte   Flags.comp_byte
  let comp_native = flag comp_opt  L.comp_native B.comp_native Flags.comp_native

  let prereqs t = function
    | `Byte   -> Comp.id t ^ "." ^ deps_byte
    | `Native -> Comp.id t ^ "." ^ deps_opt

  let prereqs_var t mode =
    sprintf "$(%s)" (prereqs t mode)

  let variables t =
    Variable.(prereqs t `Byte =?= `Strings (Comp.prereqs t resolver `Byte))
    :: Variable.(prereqs t `Native =?= `Strings (Comp.prereqs t resolver `Native))
    :: comp_byte t
    :: comp_native t
    :: (match pp_byte t with
        | None   -> []
        | Some v -> [v])
    @  (match pp_native t with
        | None   -> []
        | Some v -> [v])

  (* XXX: handle native pp *)
  let rules t =
    let pp = match pp_byte t with
      | None   -> ""
      | Some v -> sprintf "-pp '$(CAMLP4O) %s' " (Variable.name v) in
    let for_pack = match Comp.for_pack t with
      | None   -> ""
      | Some p -> sprintf "-for-pack %s " p in
    let flags = for_pack ^ pp in
    let target ext = Comp.file t resolver ext in
    let source ext = Comp.dir t // Comp.name t ^ ext in

    match Comp.unpack t with
    | [] -> (* Normal compilation unit. *)
      let ln = (* link source file to target directory *)
        match Comp.generated t with
        | false ->
          let aux exists ext =
            let source = source ext in
            let target = target ext in
            if exists t then
              [Rule.create ~targets:[target] ~prereqs:[source] [
                  sprintf "mkdir -p %s" (Comp.build_dir t resolver);
                  sprintf "ln -sf $(shell pwd)/%s %s" source target
                ]]
            else [] in
          aux Comp.ml ".ml" @ aux Comp.mli ".mli"
        | true -> [] in
      let cmi = (* generate cmis *)
        let targets, prereqs =
          if Comp.mli t then [target ".cmi"], [target ".mli"]
          else if Comp.ml t then [target ".cmo"; target ".cmi"], [target ".ml"]
          else [], [] in
        [Rule.create ~targets ~prereqs:(prereqs @ [prereqs_var t `Byte]) [
            sprintf "$(OCAMLC) -c %s%s %s"
              flags (Variable.name @@ comp_byte t) Rule.prereq
          ]] in
      let cmo = (* Generate cmos *)
        if Comp.mli t && Comp.ml t then
          [Rule.create ~targets:[target ".cmo"]
             ~prereqs:[target ".ml"; target ".cmi"; prereqs_var t `Byte]
             [sprintf "$(OCAMLC) -c %s%s %s"
                flags (Variable.name @@ comp_byte t) Rule.prereq]]
        else
          [] in
      let cmx = (* Generate cmxs *)
        [Rule.create ~targets:[target ".cmx"]
           ~prereqs:[target ".ml"; target ".cmi"; prereqs_var t `Native]
           [sprintf "$(OCAMLOPT) -c %s%s %s"
              flags (Variable.name @@ comp_native t) Rule.prereq]]
      in
      ln @ cmi @ cmo @ cmx

    | units -> (* Packed units *)
      let byte =
        let cmo = List.map (fun u -> Comp.cmo u resolver) units in
        Rule.create ~targets:[target ".cmo"; target ".cmi"] ~prereqs:cmo [
          sprintf "$(OCAMLC) -pack %s%s -o %s"
            flags Rule.prereq Rule.target_member
        ] in
      let native =
        let cmx = List.map (fun u -> Comp.cmx u resolver) units in
        Rule.create ~targets:[target ".cmx"] ~prereqs:cmx [
          sprintf "$(OCAMLOPT) -pack %s%s -o %s"
            flags Rule.prereq Rule.target_member
        ] in
      byte :: native :: conmap U.rules units

end

and L: sig
  val rules      : Lib.t -> Rule.t list
  val variables  : Lib.t -> Variable.t list
  val comp_byte  : Lib.t -> Variable.t
  val comp_native: Lib.t -> Variable.t
  val pp_byte    : Lib.t -> Variable.t
  val pp_native  : Lib.t -> Variable.t
end = struct

  let flag suffix with_glob fn t =
    let var   = Lib.id t ^ "." ^ suffix in
    let glob  = sprintf "$(%s)" suffix in
    let flags = fn (Lib.flags t resolver) in
    if with_glob then
      Variable.(var =?= `Strings (glob :: flags))
    else
      Variable.(var =?= `Strings flags)

  let comp_byte   = flag comp_byte true  Flags.comp_byte
  let comp_native = flag comp_opt  true  Flags.comp_native
  let pp_byte     = flag pp_byte   false Flags.pp_byte
  let pp_native   = flag pp_opt    false Flags.pp_native
  let link_byte   = flag link_byte true  Flags.link_byte
  let link_native = flag link_opt  true  Flags.link_native

  let prereqs t = function
    | `Byte   -> Lib.id t ^ "." ^ deps_byte
    | `Native -> Lib.id t ^ "." ^ deps_opt

  let prereqs_var t mode =
    sprintf "$(%s)" (prereqs t mode)

  let variables t =
    let cma  = Lib.cma t resolver in
    let cmxa = Lib.cmxa t resolver in
    let cmxs = Lib.cmxs t resolver in
    let cs = [
        native_dynlink_f, `Strings [cma; cmxa; cmxs];
        native_f        , `Strings [cma; cmxa];
        Feature.true_   , `Strings [cma];
      ] in
    Variable.(Lib.id t =?= case (Lib.available t) cs)
    :: comp_byte t
    :: comp_native t
    :: pp_byte t
    :: pp_native t
    :: link_byte t
    :: link_native t
    :: Variable.(prereqs t `Byte =?= `Strings (Lib.prereqs t resolver `Byte))
    :: Variable.(prereqs t `Native =?= `Strings (Lib.prereqs t resolver `Native))
    :: conmap U.variables (Lib.comps t)

  let rules t =
    let byte =
      Rule.create
        ~targets:[Lib.cma t resolver]
        ~prereqs:[prereqs_var t `Byte] [
        sprintf "$(OCAMLC) -a %s -o %s" (Variable.name @@ link_byte t) Rule.target
      ] in
    let native mode =
      let file, mode = match mode with
        | `shared  -> Lib.cmxs t resolver, "-shared"
        | `archive -> Lib.cmxa t resolver, "-a" in
      Rule.create
        ~targets:[file]
        ~prereqs:[prereqs_var t `Native] [
        sprintf "$(OCAMLOPT) %s %s -o %s"
          mode (Variable.name @@ link_native t) Rule.target
      ] in
    Rule.create
      ~targets:[Lib.id t]
      ~prereqs:[sprintf "$(%s)" (Lib.id t)]
      [echo_prereqs]
    :: byte
    :: native `archive
    :: native `shared
    :: conmap U.rules (Lib.comps t)

end

and B: sig
  val rules      : Bin.t -> Rule.t list
  val variables  : Bin.t -> Variable.t list
  val comp_byte  : Bin.t -> Variable.t
  val comp_native: Bin.t -> Variable.t
  val pp_byte    : Bin.t -> Variable.t
  val pp_native  : Bin.t -> Variable.t
end = struct

  let flag suffix with_glob fn t =
    let var   = Bin.id t ^ "." ^ suffix in
    let glob  = sprintf "$(%s)" suffix in
    let flags = fn (Bin.flags t resolver) in
    if with_glob then
      Variable.(var =?= `Strings (glob :: flags))
    else
      Variable.(var =?= `Strings flags)

  let comp_byte   = flag comp_byte true  Flags.comp_byte
  let comp_native = flag comp_opt  true  Flags.comp_native
  let link_byte   = flag link_byte true  Flags.link_byte
  let link_native = flag link_opt  true  Flags.link_native
  let pp_byte     = flag pp_byte   false Flags.pp_byte
  let pp_native   = flag pp_opt    false Flags.pp_native

  let prereqs t = function
    | `Byte   -> Bin.id t ^ "." ^ deps_byte
    | `Native -> Bin.id t ^ "." ^ deps_opt

  let prereqs_var t mode =
    sprintf "$(%s)" (prereqs t mode)

  let variables t =
    let cs = [
      native_f     , `Strings [Bin.byte t resolver; Bin.native t resolver];
      Feature.true_, `String  (Bin.byte t resolver);
    ] in
    Variable.(Bin.id t =?= case (Bin.available t) cs)
    :: comp_byte t
    :: comp_native t
    :: pp_byte t
    :: pp_native t
    :: link_byte t
    :: link_native t
    :: Variable.(prereqs t `Byte   =?= `Strings (Bin.prereqs t resolver `Byte))
    :: Variable.(prereqs t `Native =?= `Strings (Bin.prereqs t resolver `Native))
    :: conmap U.variables (Bin.comps t)

  let rules t =
    Rule.create
      ~targets:[Bin.id t]
      ~prereqs:[sprintf "$(%s)" (Bin.id t)]
      [echo_prereqs]
    ::
    Rule.create
      ~targets:[Bin.byte t resolver]
      ~prereqs:[prereqs_var t `Byte] [
      sprintf "mkdir -p %s" (Bin.build_dir t resolver);
      sprintf "$(OCAMLC) %s -o %s" (Variable.name @@ link_byte t) Rule.target;
    ]
    ::
    Rule.create
      ~targets:[Bin.native t resolver]
      ~prereqs:[prereqs_var t `Native] [
      sprintf "mkdir -p %s" (Bin.build_dir t resolver);
      sprintf "$(OCAMLOPT) %s -o %s" (Variable.name @@ link_native t) Rule.target;
    ]
    :: conmap U.rules (Bin.comps t)

end

and G: sig
  val rules: gen -> Rule.t list
  val variables: gen -> Variable.t list
end = struct
  let rules _ = failwith "TODO"
  let variables _ = failwith "TODO"
end

module T = struct

  let variables ts =
    let has_test = Variable.has_feature Feature.test_t in
    [Variable.("test" =:= `Case [
         [has_test, "1"], `String (String.concat " " (List.map Test.id ts));
         []             , `Strings [];

       ])]

  let rules ts =
    Rule.create ~targets:["test"] ~prereqs:["$(test)"] []
    :: List.map (fun t ->
        Rule.create
          ~targets:[Test.id t]
          ~prereqs:(Test.prereqs t resolver `Byte) (
          let dir = match Test.dir t with
            | None   -> ""
            | Some d -> sprintf "cd %s &&" d in
          List.map (function
              | `Shell cmd       -> String.concat " " [dir; cmd]
              | `Bin (bin, args) ->
                let bin = Sys.getcwd () / Bin.byte bin resolver in
                String.concat " " [dir; bin; String.concat " " args]
            ) (Test.commands t)
        )
      ) ts

end

module D = struct

  let target l =
    "doc-" ^ Lib.id l

  let variables libs =
    let has_doc = Variable.has_feature Feature.doc_t in
    [Variable.("doc" =:= `Case [
         [has_doc, "1"], `String (String.concat " " (List.map target libs));
         []            , `Strings [];
       ])]

  let rules ?css ?intro ~dir libs =
    Rule.create ~targets:["doc"] ~prereqs:["$(doc)"] []
    :: List.map (fun l ->
        Rule.create ~targets:[target l] ~prereqs:[Lib.id l] [
          sprintf "mkdir -p %s" dir;
          let files =
            Lib.comps l
            |> List.map (fun u ->
                "$(BUILDIR)" / Lib.id l /
                if Comp.mli u then Comp.name u ^ ".mli"
                else Comp.name u ^ ".ml")
            |> String.concat " " in
          let deps = Lib.deps l |> Dep.closure in
          let libs =
            deps
            |> Dep.(filter lib)
            |> (fun d -> l :: d)
            |> List.map (fun l -> sprintf "-I %s"
                          (Resolver.build_dir resolver (Lib.id l)))
            |> String.concat " " in
          let pkgs =
            deps
            |> Dep.(filter pkg)
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
        ]) libs

end

module J = struct

  let variables = function
    | []  -> [Variable.("js" =:= `Strings [])]
    | jss ->
      let has_js = Variable.has_feature Feature.js_t in
      [Variable.("js" =:= `Case [
           [has_js, "1"], `String (String.concat " " (List.map JS.id jss));
           []           , `Strings [];
         ])]

  let rules jss =
    Rule.create ~targets:["js"] ~prereqs:["$(js)"] []
    :: List.map (fun j ->
        Rule.create ~targets:[JS.id j] ~prereqs:(JS.prereqs j resolver `Byte) [
          sprintf "$(JS_OF_OCAML) %s %s"
            (String.concat " " (Flags.link_byte (JS.flags j resolver)))
            Rule.prereq
        ]) jss

end

module C = struct

  let _variables cs r =
    [Variable.("c" =:= `Strings (List.map (fun t -> C.dll_so t r) cs))]

  let _rules cs r =
    Rule.create ~targets:["c"] ~prereqs:["$(c)"] []
    :: List.map (fun c ->
        Rule.create ~targets:[C.dll_so c r] ~prereqs:[]
          [sprintf "$(OCAMLMKLIB) -o %s" Rule.target]
      ) cs

end

(* dedup while keeping the initial order *)
let dedup l =
  let saw = Hashtbl.create (List.length l) in
  let rec aux acc = function
    | []   -> List.rev acc
    | h::t ->
      if Hashtbl.mem saw h then aux acc t
      else (
        Hashtbl.add saw h true;
        aux (h :: acc) t
      ) in
  aux [] l

let global_variables flags =
  let debug = Variable.has_feature Feature.debug_t in
  let annot = Variable.has_feature Feature.annot_t in
  let warn_error = Variable.has_feature Feature.warn_error_t in
  let mk fn n = match fn flags with
    | [] -> []
    | l  -> [Variable.(n =:= `Strings l)] in
  mk Flags.comp_byte     comp_byte
  @ mk Flags.comp_native comp_opt
  @ mk Flags.link_byte   link_byte
  @ mk Flags.link_native link_opt
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
    Variable.(comp_opt =+= `Case [
        [debug, "1"], `Strings Flags.(comp_native debug)
      ]);
    Variable.(comp_opt =+= `Case [
        [annot, "1"], `Strings Flags.(comp_native annot)
      ]);
    Variable.(comp_opt =+= `Case [
        [warn_error, "1"], `Strings Flags.(comp_native warn_error)
      ]);
    Variable.(link_opt =+= `Case [
        [debug, "1"], `Strings Flags.(link_native debug)
      ]);
  ]

let of_project ?(buildir="_build") ?(makefile="Makefile") ~flags ~features t =
  let global_variables = global_variables flags in
  let contents = Project.contents t in
  let libs = Dep.(filter lib contents) in
  let pps = Dep.(filter pp contents) in
  let bins = Dep.(filter bin contents) in
  let tests = Dep.(filter test contents) in
  let jss = Dep.(filter js contents) in
  let gens = Dep.(filter gen contents) in
  let features =
    Project.features t
    |> Feature.Set.elements
    |> (fun t ->
        List.map (fun elt ->
            if List.mem_assoc elt features then
              Feature.with_default elt (List.assoc elt features)
            else elt
          ) t)
    |> List.map Variable.has_feature in
  let variables =
    dedup (
      Variable.(   "BUILDIR"     =?= `String buildir)
      :: Variable.("OCAMLOPT"    =?= `String "ocamlopt")
      :: Variable.("OCAMLC"      =?= `String "ocamlc")
      :: Variable.("CAMLP4O"     =?= `String "camlp4o")
      :: Variable.("OCAMLDOC"    =?= `String "ocamldoc")
      :: Variable.("JS_OF_OCAML" =?= `String "js_of_ocaml")
      :: features
      @  global_variables
      @  conmap L.variables libs
      @  conmap L.variables pps
      @  conmap B.variables bins
      @  conmap G.variables gens
      @  T.variables tests
      @  D.variables libs
      @  J.variables jss
    ) in
  let rules =
    dedup (
      conmap L.rules libs
      @ conmap L.rules pps
      @ conmap B.rules bins
      @ conmap G.rules gens
      @ T.rules tests
      @ D.rules
        ?css:(Project.doc_css t)
        ?intro:(Project.doc_intro t)
        ~dir:(Project.doc_dir t) libs
      @ J.rules jss
    ) in
  let main = Rule.create ~ext:true ~targets:["all"] ~prereqs:[] [
      sprintf "@echo '\027[32m== %s\027[m'"
        (String.concat " " (List.map (fun v ->
             sprintf "%s=%s" v.Variable.name (Variable.name v)
           ) features));
      sprintf "@$(MAKE) %s test"
        (String.concat " " (List.map Lib.id libs @ List.map Bin.id bins));
      sprintf "@echo '\027[32m== Done!\027[m'";
    ] in
  let clean = Rule.create ~ext:true ~targets:["clean"] ~prereqs:[] [
      "rm -f *~ **/*~";
      sprintf "rm -rf $(BUILDIR)";
    ] in
  let distclean = Rule.create ~ext:true ~targets:["distclean"] ~prereqs:[] (
      sprintf "rm -f %s %s.install META %s/*.html"
        makefile  (Project.name t) (Project.doc_dir t)
      ::
      List.map (fun file ->
          sprintf "rm -rf %s.ml" file
        ) (Project.generated_from_custom_generators t resolver)
    ) in
  let install = Rule.create ~ext:true ~targets:["install"] ~prereqs:["all"] [
      sprintf "@opam-installer --prefix $(shell opam config var prefix) \
               %s.install" (Project.name t)
    ] in
  create
    ~phony:["all"; "clean"; "test"; "doc"; "distclean"; "js"]
    makefile variables (main :: clean :: distclean :: install :: rules)
