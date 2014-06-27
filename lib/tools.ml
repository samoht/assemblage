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
open Project

let process ?(file="configure.ml") ?(auto_include=true) ?(includes=[]) fn =
  Shell.show "Loading %s [auto-include=%b includes=%s]"
    (Shell.color `bold file) auto_include (String.concat ", " includes);
  Toploop.initialize_toplevel_env ();
  let includes =
    if auto_include then
      includes @ Shell.exec_output "ocamlfind query -r tools"
    else
      includes in
  List.iter Topdirs.dir_directory includes;
  if not (Sys.file_exists file) then
    Shell.fatal_error 1 "missing %s." file
  else match Toploop.use_silently Format.std_formatter file with
    | false -> Shell.fatal_error 1 "while loading `%s'." file
    | true  ->
      match Project.list () with
      | [] -> Shell.fatal_error 2 "No projects are registered in `%s'." file
      | ts ->
        let features = List.fold_left (fun acc t ->
            Feature.Set.union (Project.features t) acc
          ) Feature.Set.empty ts in
        let env = Build_env.parse features in
        List.iter fn ts

let generate t `Makefile =
  Makefile.(write @@ of_project t);
  Ocamlfind.META.(write @@ of_project t);
  Opam.Install.(write @@ of_project t)

let describe t =
  let deps = function
    | [] -> ""
    | ds -> sprintf "[%s]" (String.concat " " (List.map Dep.id ds)) in
  let unit u =
    printf "    |- %s %s\n" (Unit.id u) (deps @@ Unit.deps u) in
  let lib l =
    printf "  | %s %s\n" (Shell.color `blue (Lib.id l)) (deps @@ Lib.deps l);
    List.iter unit (Lib.units l) in
  let pps l =
    printf "  | %s %s\n" (Shell.color `magenta (Lib.id l)) (deps @@ Lib.deps l);
    List.iter unit (Lib.units l) in
  let bin b =
    printf "  | %s %s\n" (Shell.color `yellow (Bin.id b)) (deps @@ Bin.deps b);
    List.iter unit (Bin.units b)
  in
  List.iter lib (Project.libs t);
  List.iter pps (Project.pps t);
  List.iter bin (Project.bins t)
