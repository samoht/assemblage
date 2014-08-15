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

module Var = struct

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

  let raw_name t = t.name
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
        let sep = " \\\n    " in
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
      value = `String (sprintf "$(shell %s)" command) }

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
  type t =
    { ext : bool;
      targets : string list;
      prereqs : string list;
      order_only_prereqs : string list;
      recipe : string list; }

  let create ?(ext = false) ~targets ~prereqs ?(order_only_prereqs=[]) recipe =
    { ext; targets; prereqs; order_only_prereqs; recipe }

  let generate buf t =
    let pp_reqs reqs =
      if List.length reqs < 2 then String.concat " " reqs else
      String.concat " \\\n    " ("" :: reqs)
    in
    let targets = String.concat " " t.targets in
    let ext = if t.ext then "::" else ":" in
    let prereqs = pp_reqs t.prereqs in
    let oo_prereqs = match t.order_only_prereqs with
    | [] -> "" | reqs -> sprintf " \\\n    | %s" (pp_reqs reqs)
    in
    let generate_recipe = function
    | [] -> bprintf buf "\t@\n"
    | l -> List.iter (bprintf buf "\t%s\n") l
    in
    bprintf buf "%s%s %s%s\n" targets ext prereqs oo_prereqs;
    generate_recipe t.recipe;
    bprintf buf "\n"

  let target = "$@"
  let target_member = "$%"
  let prereq = "$<"
  let changed_prereqs = "$?"
  let prereqs = "$^"
  let dedup_prereqs = "$+"
  let stem = "$*"
end

type t =
  { headers : string list;
    phony : string list;
    variables : Var.stanza list;
    rules : Rule.t list;
    includes : string list;
    opt_includes : (string list * string list) list; }

let create ?(headers = []) ?(includes = []) ?(opt_includes = []) ?(phony = [])
    variables rules =
  { headers; phony; variables; rules; includes; opt_includes }

let to_string t =
  let buf = Buffer.create 1024 in
  let pr_headers buf = List.iter (Buffer.add_string buf) t.headers in
  let pr_phony buf = match t.phony with
  | [] -> ()
  | l  -> bprintf buf ".PHONY: %s\n\n" (String.concat " " l)
  in
  let pr_variables buf =
    let pr_variable { Var.doc; align; simplify; variables }  =
      List.iter (bprintf buf "# %s\n") doc;
      Var.generate buf ~simplify ~align variables;
      if doc <> [] || variables <> [] then bprintf buf "\n";
    in
    List.iter pr_variable t.variables;
  in
  let pr_rules buf = List.iter (Rule.generate buf) t.rules in
  let pr_opt_includes buf = match t.opt_includes with
  | [] -> ()
  | includes ->
      let has_guard (g, _) = g <> [] in
      let with_guards, no_guards = List.partition has_guard includes in
      let pr_incl files =
        let files =
          if List.length files < 2 then String.concat " " files else
          String.concat " \\\n    " ("" :: files)
        in
        bprintf buf "-include %s\n" files
      in
      let pr_guarded (guards, files) =
        bprintf buf "ifneq ($(filter-out %s,$(MAKECMDGOALS)),)\n"
          (String.concat " " guards);
        pr_incl files;
        bprintf buf "endif\n"
      in
      pr_incl (conmap snd no_guards);
      List.iter pr_guarded with_guards
  in
  let pr_includes buf = bprintf buf
      "include %s\n" (String.concat " " t.includes)
  in
  pr_headers buf;
  pr_phony buf;
  pr_variables buf;
  bprintf buf "\n";
  pr_rules buf;
  pr_opt_includes buf;
  pr_includes buf;
  Buffer.contents buf

let write_file makefile t =
  let oc = open_out makefile in
  output_string oc (to_string t);
  close_out oc
