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

(* Setup environments *)

type setup =
  { auto_load : bool; (* [true] to add assemblage libs includes to includes. *)
    includes : string list;         (* includes to add to toploop execution. *)
    assemble_file : string;                              (* file to execute. *)
    exec_status :                    (* execution status of [assemble_file]. *)
      [ `Error | `Ok | `No_cmd | `No_file]; }

(* The following does a pre-parse of of the command line to look for
   options that will influence the execution of the assemble.ml file. *)

let parse_opt short long args =        (* parses an option as cmdliner does. *)
  let starts_with ~pre s =
    try
      if String.length s < String.length pre then false else
      begin
        for i = 0 to String.length pre - 1 do
          if pre.[i] <> s.[i] then raise Exit
        done;
        true
      end
    with Exit -> false
  in
  let cut i s = String.sub s i (String.length s - i) in
  match args with
  | opt :: arg :: args' when opt = short || opt = long ->
      Some (arg, args')
  | sopt :: args' when starts_with ~pre:short sopt ->
      Some ((cut (String.length short) sopt), args')
  | lopt :: args' when starts_with ~pre:long lopt &&
                       String.length lopt > String.length long ->
      if lopt.[String.length long] <> '=' then None else
      Some ((cut (String.length long + 1) lopt), args')
  | _ -> None

let setup = ref None
let get_setup () = !setup
let parse_setup () =
  let env = { auto_load = true; includes = []; assemble_file = "assemble.ml";
              exec_status = `Ok }
  in
  let args = Array.to_list Sys.argv in
  let rec parse env = function
  | [] -> { env with includes = List.rev env.includes }
  | "--" :: _ -> parse env [] (* it's all positional after that, stop *)
  | args ->
      match parse_opt "-I" "--includes" args with
      | Some (i, args') -> parse { env with includes = i :: env.includes } args'
      | None ->
          match parse_opt "-f" "--file" args with
          | Some (f, args') -> parse { env with assemble_file = f } args'
          | None ->
              match args with
              | "--disable-auto-load" :: args' ->
                  parse { env with auto_load = false } args'
              | _ :: args' -> parse env args'
              | [] -> assert false
  in
  let env = parse env args in
  setup := Some env;
  env

(* Command runtime environement *)

type t =
  { setup : setup option;         (* None if not run by assemblage. *)
    verbose : bool; }

let get_bool e ~default =
  try match Sys.getenv e with
  | "" | "false" | "0" -> false
  | _ -> true
  with
  Not_found -> default

let var_verbose = "ASSEMBLAGE_VERBOSE"

let created = ref false
let create setup verbose =
  created := true;
  let verbose = get_bool var_verbose ~default:verbose in
  { setup; verbose; }

let created () = !created
let variable_docs =
  [ var_verbose, "See option $(b,--verbose)."; ]
