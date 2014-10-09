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

(* The assemblage command line tool.

   The users's assemble.ml handles the whole program and especially
   its command line interface by calling Assemble.assemble.

   The assemblage tool just prepares the environment so that
   assemble.ml can be interpeted correctly. *)

open Assemblage

let str = Printf.sprintf
let err_no_file file = `Error (str "missing %s." file)
let err_loading file = `Error (str "while loading %s." file)
let err_multiple_projects file =
  `Error (str "Multiple projects are registered. \
               Did you call Assemblage.assemble more than once in %s ?" file)

let err_no_cmd file =
  `Error (str "No command ran. Did you call Assemblage.assemble in %s ?" file)

let err_no_ocamlfind =
  `Error (str "ocamlfind not found in your $PATH. Invoke with \
               --auto-load=false and \n\
               \       specify the path to the assemblage library with -I.")

let show_run_start file auto_load =
  let pp_auto_load ppf () =
    if auto_load then () else
    Fmt.(pp ppf "[auto-load: %a]" (pp_styled `Magenta pp_bool)) auto_load
  in
  Log.show "%a Loading %s %a"
    Fmt.(pp_styled `Cyan pp_rarrow) () file pp_auto_load ()

let auto_includes () =
  if not (Asd_shell.has_cmd "ocamlfind") then err_no_ocamlfind else
  `Ok (Asd_shell.exec_output "ocamlfind query -r assemblage")

let error setup_env status =
  let setup_env = { setup_env with Assemblage_env.exec_status = status } in
  Assemblage_cmd.assemble_no_project setup_env

let run () =
  let setup_env = Assemblage_env.parse_setup () in
  let includes =
    if not setup_env.Assemblage_env.auto_load
    then `Ok setup_env.Assemblage_env.includes
    else
    match auto_includes () with
    | `Ok auto_incs -> `Ok (setup_env.Assemblage_env.includes @ auto_incs)
    | `Error _ as err -> err
  in
  match includes with
  | `Error _ as err -> error setup_env err
  | `Ok includes ->
      let file = setup_env.Assemblage_env.assemble_file in
      if not (Sys.file_exists file) then error setup_env (err_no_file file)
      else
      begin
        Toploop.initialize_toplevel_env ();
        Toploop.set_paths ();
        List.iter Topdirs.dir_directory includes;
        show_run_start file setup_env.Assemblage_env.auto_load;
        match Toploop.use_silently Format.err_formatter file with
        | false -> error setup_env (err_loading file)
        | true ->
            (* FIXME review *)
            match Assemblage.projects () with
            | [] -> error setup_env (err_no_cmd file)
            | [p] -> Assemblage_cmd.assemble p
            | l -> error setup_env (err_multiple_projects file)
                     (* FIXME list project names *)
      end

let () = run ()
