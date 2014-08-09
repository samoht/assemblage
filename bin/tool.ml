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

(* The assemblage command line tool. *)

let show_run_start file auto_load =
  let file = As_shell.color `Bold file in
  let auto_load =
    if auto_load then "" else
    Printf.sprintf "[auto-load: %s]"
      (As_shell.color `Magenta (string_of_bool auto_load))
  in
  As_shell.show "Loading %s. %s" file auto_load

let run ?(file = "assemble.ml") () =
  let sys_argl = Array.to_list Sys.argv in
  let auto_load = List.for_all ((<>) "--disable-auto-load") sys_argl in
  let includes () =
    let rec cmdline_includes acc = function
    | [] -> List.rev acc
    | "-I" :: h :: t -> cmdline_includes (h::acc) t
    | _ :: t -> cmdline_includes acc t
    in
    let auto_load_includes =
      if not auto_load then [] else
      As_shell.exec_output "ocamlfind query -r assemblage"
    in
    (cmdline_includes [] sys_argl) @ auto_load_includes
  in
  show_run_start file auto_load;
  Toploop.initialize_toplevel_env ();
  Toploop.set_paths ();
  List.iter Topdirs.dir_directory (includes ());
  if not (Sys.file_exists file)
  then As_shell.fatal_error 1 "missing %s." file
  else
  match Toploop.use_silently Format.err_formatter file with
  | false -> As_shell.fatal_error 1 "while loading %s." file
  | true ->
      if As_cmd.did_run () then () else
      As_shell.fatal_error 1
        "no command ran. Did you call Assemblage.assemble on your \
         project in %s" file

let () = run ()
