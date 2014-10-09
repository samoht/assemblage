(*
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(* Conditions *)

type atom = { name : string; default : bool; doc : string; }
type t =
  | True
  | False
  | Atom of atom
  | Not of t
  | And of t * t
  | Or of t * t

let create ?(default = true) name ~doc = Atom { name; default; doc }
let true_ = True
let false_ = False
let neg ?(on = true) c =
  if not on then True else
  match c with
  | True -> False
  | False -> True
  | Not t -> t
  | t  -> Not t

let ( &&& ) c0 c1 = match c0, c1 with
| True, c | c, True  -> c
| False, _ | _, False -> False
| _ -> And (c0, c1)

let ( ||| ) c0 c1 = match c0, c1 with
| True, _ | _, True  -> True
| False, c | c, False -> c
| _ -> Or (c0, c1)

(* Built-in conditions *)

let byte =
  create "byte" ~default:true ~doc:"Byte code OCaml compilation is available."

let native =
  create "native" ~default:true
    ~doc:"Native code OCaml compilation is available."

let native_dynlink =
  create "native-dynlink" ~default:true
    ~doc:"Native code OCaml dynamic linking is available."

let native_tools =
  create "native-tools" ~default:true
    ~doc:"The native compiled OCaml toolchain is available (.opt tools)."

let js =
  create "js" ~default:false
    ~doc:"JavaScript code OCaml compilation with js_of_ocaml is available."

let annot =
  create "annot" ~default:true  ~doc:"Build OCaml binary annotations files."

let debug =
  create "debug" ~default:true ~doc:"Build with debugging support."

let warn_error =
  create "warn-error" ~default:false  ~doc:"Build with warnings as errors."

let test =
  create "test" ~default:false  ~doc:"Build the tests."

let doc =
  create "doc" ~default:false ~doc:"Build the documentation."

(* Atomic conditions *)

let name a = a.name
let default a = a.default
let atom_doc a = a.doc
let rec eval tbl = function
| True -> true
| False -> false
| Atom a -> (try List.assoc a tbl with Not_found -> default a)
| Not c -> not (eval tbl c)
| And (c0, c1) -> (eval tbl c0) && (eval tbl c1)
| Or (c0, c1)  -> (eval tbl c0) || (eval tbl c1)

(* Sets of atomic condition *)

module Atom = struct
  type t = atom
  let compare a0 a1 = String.compare a0.name a1.name
end

module Set = Set.Make (Atom)

let atoms c =
  let rec loop acc = function
  | [] -> acc
  | True :: cs -> loop acc cs
  | False :: cs -> loop acc cs
  | Atom a :: cs -> loop (Set.add a acc) cs
  | Not c :: cs ->  loop acc (c :: cs)
  | And (c0, c1) :: cs -> loop acc (c0 :: c1 :: cs)
  | Or (c0, c1) :: cs -> loop acc (c0 :: c1 :: cs)
  in
  loop Set.empty [c]

let builtin =
  let add acc = function Atom a -> Set.add a acc | _ -> assert false in
  List.fold_left add Set.empty
    [ byte; native; native_dynlink; native_tools; js; annot; warn_error; debug;
      test; doc; ]

(* Conditions in conjunctive normal form *)

type clause = [ `P of atom | `N of atom ] list
type cnf = [ `True | `False | `And of clause list ]

let cnf_disjunct_clauses cl0 cl1 = (* cl0 v cl1 *)
  let add (pos, neg) = function
  | `P a -> Set.add a pos, neg
  | `N a -> pos, Set.add a neg
  in
  let pos, neg = List.fold_left add (Set.empty, Set.empty) cl0 in
  let pos, neg = List.fold_left add (pos, neg) cl1 in
  if not Set.(is_empty (inter pos neg)) then (* A v ~A *) [] else
  let clause = Set.fold (fun a acc -> `N a :: acc) neg [] in
  let clause = Set.fold (fun a acc -> `P a :: acc) pos clause in
  clause

let cnf_conjunct c0 c1 = match c0, c1 with (* c0 ^ c1 *)
| `False, _ | _, `False -> `False
| `True, c | c, `True -> c
| `And d0s, `And d1s -> `And (d0s @ d1s)

let cnf_disjunct c0 c1 = match c0, c1 with (* c0 v c0 *)
| `False, c | c, `False -> c
| `True, _ | _, `True -> `True
| `And d0s, `And d1s ->
    (* (A0 ^ ... ^ An) v (B0 ^ ... ^ Bn) ->
       (A0 v B1) ^ ... (A0 v Bn) ^ (A1 v B1) ^ ... ^ (A1 v Bn) ^ ... *)
    let distribute acc d0 =
      let add_clause acc d1 = match cnf_disjunct_clauses d0 d1 with
      | [] -> (* True *) acc
      | clause -> clause :: acc
      in
      List.fold_left add_clause acc d1s
    in
    `And (List.fold_left distribute [] d0s)

let rec cnf = function
| True -> `True
| False -> `False
| Atom a -> `And [[`P a]]
| And (c0, c1) -> cnf_conjunct (cnf c0) (cnf c1)
| Or (c0, c1) -> cnf_disjunct (cnf c0) (cnf c1)
| Not c ->
    match c with
    | True -> `False
    | False -> `True
    | Atom a -> `And [[`N a]]
    | Not c -> cnf c                            (* double negation *)
    | And (c0, c1) -> cnf (Or (Not c0, Not c1)) (* De Morgan *)
    | Or (c0, c1) -> cnf (And (Not c0, Not c1)) (* De Morgan *)
