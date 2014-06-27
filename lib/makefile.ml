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
    name    : string;
    assign  : assign;
    contents: contents;
  }

  and contents =
    [ `String of string
    | `Strings of string list
    | `Case of ((t * string) list * contents) list ]

  let (=:=) name contents =
    { name; contents; assign = ":=" }

  let (=+=) name contents =
    { name; contents; assign = "+=" }

  let (=?=) name contents =
    { name; contents; assign = "?=" }

  let subst t name ~input ~output =
    { name; assign = "=";
      contents = `String (sprintf "$(${%s}:%s=%s)" t.name input output)
    }

  let name t =
    sprintf "$(%s)" t.name

  let contents t = t.contents

  let is_contents_empty t =
    match t.contents with
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
    match Feature.normalize features with
    | `False  -> failwith "invalid handler case"
    | `And l  ->
      List.map (function
          | `P f -> has_feature f, "1"
          | `N f -> has_feature f, "0"
        ) l,
      contents

  (* build the full handler cases *)
  let case available cs: contents =
    let cs = List.filter (fun (f,_) ->
        Feature.(normalize (available && f)) <> `False
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
    contents "" t.contents

  let generates buf ts =
    let size = List.fold_left (fun acc t ->
        max acc (String.length t.name)
      ) 0 ts in
    List.iter (generate buf ~size) ts

  let shell name command =
    { name; assign = "=";
      contents = `String (sprintf "$(shell %s)" command)
    }

  let files name ~dir ~ext =
    { name; assign = "=";
      contents = `String (sprintf "$(wildcard %s/*.%s)" dir ext) }

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
  header: string list;
  phony: string list;
  variables: Variable.t list;
  rules: Rule.t list;
}

let create ?(header=[]) ?(phony=[]) variables rules =
  { phony; header; variables; rules }

let write ?(file="Makefile") t =
  printf "\027[36m+ write %s\027[m\n" file;
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
  let oc = open_out file in
  output_string oc (Buffer.contents buf);
  close_out oc

(******************************************************************************)

let buildir = "$(BUILDIR)"

let echo_prereqs =
  sprintf "@echo '\027[36m== Building %s\027[m'" Rule.prereqs

let resolver =
  Ocamlfind.resolver (fun x -> buildir / x)

let native_dynlink_f = Feature.(atom native_dynlink && atom native)

let native_f = Feature.(atom native)

module rec D: sig
  val variables: Dep.t -> Variable.t list
  val rules    : Dep.t -> Rule.t list
end = struct

  let variables = function
    | `Unit u -> U.variables u
    | `Lib l
    | `Pp l   -> L.variables l
    | `Bin b  -> B.variables b
    | `Pkg_pp _
    | `Pkg _  -> []

  let rules = function
    | `Unit u -> U.rules u
    | `Lib l
    | `Pp l   -> L.rules l
    | `Bin b  -> B.rules b
    | `Pkg_pp _
    | `Pkg _  -> []

end

and U: sig
  val rules      : Unit.t -> Rule.t list
  val variables  : Unit.t -> Variable.t list
  val pp_byte    : Unit.t -> Variable.t option
  val pp_native  : Unit.t -> Variable.t option
  val comp_byte  : Unit.t -> Variable.t
  val comp_native: Unit.t -> Variable.t
end = struct

  let pp prefix varlib varbin fn t =
    let var    = prefix ^ Unit.id t in
    let lib = match Unit.container t with
      | None          -> None
      | Some (`Lib l) -> Some (varlib l)
      | Some (`Bin b) -> Some (varbin b) in
    match lib, fn (Unit.flags t resolver) [] with
    | None  , [] -> None
    | Some l, [] -> if Variable.is_contents_empty l then None else Some l
    | None  , u  -> Some (Variable.(var =?= `Strings u))
    | Some _, u  -> Some (Variable.(var =?= `Strings (fn (Unit.flags t resolver) u)))

  let flag prefix varlib varbin fn t =
    let var    = prefix ^ Unit.id t in
    let global = match Unit.container t with
      | None          -> []
      | Some (`Lib l) -> [Variable.name (varlib l)]
      | Some (`Bin b) -> [Variable.name (varbin b)] in
    let flags = fn (Unit.flags t resolver) global in
    Variable.(var =?= `Strings flags)

  let pp_byte     = pp   "PP__B_" L.pp_byte     B.pp_byte     Flags.pp_byte
  let pp_native   = pp   "PP__O_" L.pp_native   B.pp_native   Flags.pp_native
  let comp_byte   = flag "COMPB_" L.comp_byte   B.comp_byte   Flags.comp_byte
  let comp_native = flag "COMPO_" L.comp_native B.comp_native Flags.comp_native

  let variables t =
    comp_byte t
    :: comp_native t
    :: (match pp_byte t with
        | None   -> []
        | Some v -> [v])
    @  (match pp_native t with
        | None   -> []
        | Some v -> [v])

  let preds t =
    Dep.units (Unit.unpack t) @ Unit.deps t

  (* XXX: handle native pp *)
  let rec rules t =
    let pp = match pp_byte t with
      | None   -> ""
      | Some v -> sprintf "-pp '$(CAMLP4O) %s' " (Variable.name v) in
    let for_pack = match Unit.for_pack t with
      | None   -> ""
      | Some p -> sprintf "-for-pack %s " p in
    let flags = for_pack ^ pp in
    let target ext = Unit.file t resolver ext in
    let source ext = Unit.dir t // Unit.name t ^ ext in

    match Unit.unpack t with
    | [] -> (* Normal compilation unit. *)
      let ln = (* link source file to target directory *)
        let aux exists ext =
          let source = source ext in
          let target = target ext in
          if exists t then
            [Rule.create[target] [source] [
                sprintf "mkdir -p %s" (Unit.build_dir t resolver);
                sprintf "ln -sf $(shell pwd)/%s %s" source target
              ]]
          else [] in
        aux Unit.ml ".ml" @ aux Unit.mli ".mli" in
      let cmi = (* generate cmis *)
        let targets, prereqs =
          if Unit.mli t then [target ".cmi"], [target ".mli"]
          else if Unit.ml t then [target ".cmo"; target ".cmi"], [target ".ml"]
          else [], [] in
        [Rule.create targets (prereqs @ Unit.prereqs t resolver`Byte) [
            sprintf "$(OCAMLC) -c %s%s %s"
              flags (Variable.name @@ comp_byte t) Rule.prereq
          ]] in
      let cmo = (* Generate cmos *)
        if Unit.mli t && Unit.ml t then
          [Rule.create [target ".cmo"]
             (target ".ml" :: target ".cmi" :: Unit.prereqs t resolver `Byte)
             [sprintf "$(OCAMLC) -c %s%s %s"
                flags (Variable.name @@ comp_byte t) Rule.prereq]]
        else
          [] in
      let cmx = (* Generate cmxs *)
        [Rule.create [target ".cmx"]
           (target ".ml" :: target ".cmi" :: Unit.prereqs t resolver `Native)
           [sprintf "$(OCAMLOPT) -c %s%s %s"
              flags (Variable.name @@ comp_native t) Rule.prereq]]
      in
      ln @ cmi @ cmo @ cmx

    | units -> (* Packed units *)
      let byte =
        let cmo = List.map (fun u -> Unit.cmo u resolver) units in
        Rule.create [target ".cmo"; target ".cmi"] cmo [
          sprintf "$(OCAMLC) -pack %s%s -o %s" flags Rule.prereq Rule.target_member
        ] in
      let native =
        let cmx = List.map (fun u -> Unit.cmx u resolver) units in
        Rule.create [target ".cmx"] cmx [
          sprintf "$(OCAMLOPT) -pack %s%s -o %s" flags Rule.prereq Rule.target_member
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
  val link_byte  : Lib.t -> Variable.t
  val link_native: Lib.t -> Variable.t
end = struct

  let flag prefix fn t =
    let var   = prefix ^ Lib.id t in
    let flags = fn (Lib.flags t resolver) [] in
    Variable.(var =?= `Strings flags)

  let comp_byte   = flag "COMPB_" Flags.comp_byte
  let comp_native = flag "COMPO_" Flags.comp_native
  let pp_byte     = flag "PP__B_" Flags.pp_byte
  let pp_native   = flag "PP__O_" Flags.pp_native
  let link_byte   = flag "LINKB_" Flags.link_byte
  let link_native = flag "LINKO_" Flags.link_native

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
    :: conmap U.variables (Lib.units t)

  let rules t =
    let byte =
      Rule.create [Lib.cma t resolver] (Lib.prereqs t resolver `Byte) [
        sprintf "$(OCAMLC) -a %s -o %s" (Variable.name @@ link_byte t) Rule.target
      ] in
    let native mode =
      let file, mode = match mode with
        | `shared  -> Lib.cmxs t resolver, "-shared"
        | `archive -> Lib.cmxa t resolver, "-a" in
      Rule.create [file] (Lib.prereqs t resolver `Native) [
        sprintf "$(OCAMLOPT) %s %s -o %s"
          mode (Variable.name @@ link_native t) Rule.target
      ] in
    Rule.create [Lib.id t] [sprintf "$(%s)" (Lib.id t)] [echo_prereqs]
    :: byte
    :: native `archive
    :: native `shared
    :: conmap U.rules (Lib.units t)

end

and B: sig
  val rules      : Bin.t -> Rule.t list
  val variables  : Bin.t -> Variable.t list
  val comp_byte  : Bin.t -> Variable.t
  val comp_native: Bin.t -> Variable.t
  val pp_byte    : Bin.t -> Variable.t
  val pp_native  : Bin.t -> Variable.t
  val link_byte  : Bin.t -> Variable.t
  val link_native: Bin.t -> Variable.t
end = struct

  let flag prefix fn t =
    let var   = prefix ^ Bin.id t in
    let flags = fn (Bin.flags t resolver) [] in
    Variable.(var =?= `Strings flags)

  let comp_byte   = flag "COMPB_" Flags.comp_byte
  let comp_native = flag "COMPO_" Flags.comp_native
  let link_byte   = flag "LINKB_" Flags.link_byte
  let link_native = flag "LINKO_" Flags.link_native
  let pp_byte     = flag "PP__B_" Flags.pp_byte
  let pp_native   = flag "PP__O_" Flags.pp_native

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
    :: conmap U.variables (Bin.units t)

  let rules t =
    Rule.create [Bin.id t] [sprintf "$(%s)" (Bin.id t)] [echo_prereqs]
    ::
    Rule.create [Bin.byte t resolver] (Bin.prereqs t resolver `Byte) [
      sprintf "mkdir -p %s" (Bin.build_dir t resolver);
      sprintf "$(OCAMLC) %s -o %s" (Variable.name @@ link_byte t) Rule.target;
    ]
    ::
    Rule.create [Bin.native t resolver] (Bin.prereqs t resolver `Native) [
      sprintf "mkdir -p %s" (Bin.build_dir t resolver);
      sprintf "$(OCAMLOPT) %s -o %s" (Variable.name @@ link_native t) Rule.target;
    ]
    :: conmap U.rules (Bin.units t)

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

let of_project ?(buildir="_build") t =
  let libs = Project.libs t in
  let pps = Project.pps t in
  let bins = Project.bins t in
  let features =
    Project.features t
    |> Feature.Set.elements
    |> List.map Variable.has_feature in
  let variables =
    dedup (
      Variable.(   "BUILDIR"    =?= `String buildir)
      :: Variable.("OCAMLOPT"   =?= `String "ocamlopt")
      :: Variable.("OCAMLC"     =?= `String "ocamlc")
      :: Variable.("CAMLP4O"    =?= `String "camlp4o")
      :: features
      @  conmap L.variables libs
      @  conmap L.variables pps
      @  conmap B.variables bins
    ) in
  let rules =
    dedup (
      conmap L.rules libs
      @ conmap L.rules pps
      @ conmap B.rules bins
    ) in
  let main = Rule.create ~ext:true ~targets:["all"] ~prereqs:[] [
      sprintf "@echo '\027[32m== %s\027[m'"
        (String.concat " " (List.map (fun v ->
             sprintf "%s=%s" v.Variable.name (Variable.name v)
           ) features));
      sprintf "@$(MAKE) %s"
        (String.concat " " (List.map Lib.id libs @ List.map Bin.id bins));
      sprintf "@echo '\027[32m== Done!\027[m'";
    ] in
  let clean = Rule.create ~ext:true ~targets:["clean"] ~prereqs:[] [
      "rm -f *~ **/*~";
      sprintf "rm -rf $(BUILDIR)";
    ] in
  let install = Rule.create ~ext:true ~targets:["install"] ~prereqs:["all"] [
      sprintf "@opam-installer --prefix $(shell opam config var prefix) %s.install"
        (Project.name t)
    ] in
  create
    ~phony:["all"; "clean"]
    variables
    (main :: clean :: install :: rules)
