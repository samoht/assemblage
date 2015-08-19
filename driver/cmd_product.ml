(*
 * Copyright (c) 2014 Daniel C. Bünzli
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

open Assemblage
open Assemblage.Private

let str = Printf.sprintf

(* FIXME this is a proof of concept. Things that
   should be improved: basic globbing for product selection
   Highlighting using pp_style, better formatting (don't
   use the library's pp). Well thought command line UI. etc. *)

type index =
  { inputs : (part_kind Part.t * Action.t) list Path.Map.t;
    outputs : (part_kind Part.t * Action.t) list Path.Map.t; }

let index proj =
  let add_index part act acc p = match Path.Map.find p acc with
  | None -> Path.Map.add p [part, act] acc
  | Some occs -> Path.Map.add p ((part, act) :: occs) acc
  in
  let add_part acc part =
    let add_action (i, o) act =
      List.fold_left (add_index part act) i (Action.inputs act),
      List.fold_left (add_index part act) o (Action.outputs act)
    in
    List.fold_left add_action acc (Project.eval proj (Part.actions part))
  in
  let init = Path.Map.empty, Path.Map.empty in
  let inputs, outputs = Part.list_fold add_part init (Project.parts proj) in
  {inputs; outputs}

let find_refs index kind selection =
  let merge _ i o = match i, o with
  | None, Some l | Some l, None -> Some l
  | Some i, Some o -> Some (List.rev_append (List.rev i) o)
  | None, None -> assert false
  in
  let inputs = Path.Map.dom index.inputs in
  let outputs = Path.Map.dom index.outputs in
  let sel = match selection with
  | [] -> Path.Set.union inputs outputs
  | l -> Path.Set.of_list (List.rev_map Path.v (* TODO FIXME *) selection)
  in
  let sel = match kind with
  | `Any -> sel
  | `Source -> Path.Set.inter sel (Path.Set.diff inputs outputs)
  | `Input -> Path.Set.inter sel inputs
  | `Output -> Path.Set.inter sel outputs
  in
  let mi = Path.Map.filter (fun k _ -> Path.Set.mem k sel) index.inputs in
  let mo = Path.Map.filter (fun k _ -> Path.Set.mem k sel) index.outputs in
  Path.Map.merge merge mi mo

let refs proj kind selection details =
  let index = index proj in
  let refs = Path.Map.bindings (find_refs index kind selection) in
  let pp_ref key ppf (part, act) =
    Fmt.pf ppf "%s %a - %s"
      (Part.name part) Part.pp_kind (Part.kind part) (Path.to_string key)
  in
  let pp_ref_details key ppf (part, act) =
      Fmt.pf ppf "@[<v>%s@,    part: @[%a %s@]@, @[%a@]@]@,"
        (Path.to_string key) Part.pp_kind (Part.kind part)
        (Part.name part) (Action.pp (Project.conf proj)) act
  in
  let pp_ref = if details then pp_ref_details else pp_ref in
  let pp_refs ppf (k, refs) = Fmt.pf ppf "%a" (Fmt.list (pp_ref k)) refs in
  Fmt.pr "@[<v>%a@]@." (Fmt.list pp_refs) refs;
  `Ok ()

let list proj kind =
  let products = Path.Set.elements (Project.products ~kind proj) in
  let pp_product ppf p = Path.pp ppf p in
  Fmt.pr "@[<v>%a@]@." (Fmt.list pp_product) products;
  `Ok ()

let product cmd kind selection details p = match cmd with
| `List -> list p kind
| `Refs -> refs p kind selection details

(* Command line interface *)

open Cmdliner

let kind =
  let doc = "Select only source products." in
  let src = Arg.info ~doc ["source"] in
  let doc = "Select only build products." in
  let build = Arg.info ~doc ["build"] in
  let doc = "Select only input products." in
  let input = Arg.info ~doc ["input"] in
  Arg.(value & vflag `Any [`Source, src; `Output, build; `Input, input ])

let subcmd =
  let conv_sub = Arg.enum ["list", `List; "references", `Refs] in
  let doc = "The command to apply on products." in
  Arg.(value & pos 0 conv_sub `List & info [] ~doc ~docv:"COMMAND")

let selection =
  let doc = "The products to select." in
  Arg.(value & pos_right 0 string [] & info [] ~doc ~docv:"PRODUCT")

let details =
  let doc = "Show reference details" in
  Arg.(value & flag & info ["d"; "details"] ~doc)

let cmd =
  let doc = "information about project products" in
  let man =
    [ `S "DESCRIPTION";
      `P "The $(b,product) command outputs information about products
          known to assemblage in a given configuration." ]
  in
  let product = Term.(pure product $ subcmd $ kind $ selection $ details) in
  Cmd_base.cmd_with_project "product" product ~doc ~man ~see_also:["setup"]
