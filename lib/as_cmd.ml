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


let sys_argl = Array.to_list Sys.argv

let project_list = ref []
let projects () = !project_list
let add_project p = project_list := p :: !project_list

let configure `Make t env =
  let features = As_build_env.features env in
  let flags = As_build_env.flags env in
  let makefile = "Makefile" in
  let build_dir = As_build_env.build_dir env in
  As_makefile.(write (of_project t ~features ~flags ~makefile));
  As_ocamlfind.META.(write (of_project t));
  As_opam.Install.(write (of_project ~build_dir t))

let describe t env =
  let open Printf in
  let print_deps x =
    let bold_name pkg = As_shell.color `Bold (As_project.Pkg.name pkg) in
    let pkgs = As_project.Component.(filter pkg x) in
    match String.concat " " (List.map bold_name pkgs) with
    | "" -> ""
    | pkgs -> sprintf "  ├─── [%s]\n" pkgs
  in
  let print_modules last modules =
    let aux i n m =
      printf "  %s %s\n"
        (if last && i = n then "└───" else "├───") (As_shell.color `Blue m) in
    let n = List.length modules - 1 in
    List.iteri (fun i m -> aux i n m) modules in
  let print_units units =
    let aux i n u =
        let mk f ext =
          if f u then (As_shell.color `Cyan (As_project.Unit.name u ^ ext)) else
          ""
        in
        let ml = mk As_project.Unit.(has `Ml) ".ml" in
        let mli = mk As_project.Unit.(has `Mli) ".mli" in
        let modules =
          if As_project.Unit.generated u then ["--generated--"]
          else
          let build_dir = As_build_env.build_dir env in
          As_OCaml.modules ~build_dir u in
        printf "  %s %-25s%-25s\n"
          (if modules = [] && i = n then "└─" else "├─") ml mli;
        print_modules (i=n) modules
    in
    let n = List.length units - 1 in
    List.iteri (fun i u -> aux i n u) units in
  let print cs =
    let aux c =
      let open As_project.Component in
      printf "└─┬─ %s\n%s"
        (As_shell.color `Magenta (id c)) (print_deps (deps c));
        print_units (filter unit (contents c)) in
    List.iter aux cs in
  printf "\n%s %s %s\n\n"
    (As_shell.color `Yellow "==>")
      (As_shell.color `Underline (As_project.name t)) (As_project.version t);
  let components =
    As_project.components t
    |> List.filter (function `Lib _ | `Bin _ -> true | _ -> false)
  in
  print components

let check t =
  (* check that all non-dep packages are actually installed. *)
  let pkgs = As_project.Component.(filter pkg) (As_project.components t) in
    let not_installed = List.fold_left (fun acc pkg ->
      let opt = As_project.Pkg.opt pkg in
        let name = As_project.Component.name (`Pkg pkg) in
      if not opt && not (As_shell.try_exec "ocamlfind query %s" name) then
          name :: acc
      else acc
    ) [] pkgs in
  match not_installed with
  | []   -> ()
  | [h]  -> As_shell.fatal_error 1
                "The ocamlfind package %s is not installed, stopping." h
  | h::t -> As_shell.fatal_error 1
              "The ocamlfind packages %s and %s are not installed, stopping."
              (String.concat " " t) h

let auto_load () =
  List.for_all ((<>) "--disable-auto-load") sys_argl

let includes auto_load =
  let rec cmdline_includes acc = function
  | []             -> List.rev acc
  | "-I" :: h :: t -> cmdline_includes (h::acc) t
  | _ :: t         -> cmdline_includes acc t
  in
  let auto_load_includes =
    if not auto_load then [] else
    As_shell.exec_output "ocamlfind query -r assemblage"
  in
  (cmdline_includes [] sys_argl) @ auto_load_includes

let run ?(file = "assemble.ml") () =
  let show_run_start file auto_load =
    let file = As_shell.color `Bold file in
    let auto_load =
      if auto_load then "" else
      Printf.sprintf "[auto-load: %s]"
        (As_shell.color `Magenta (string_of_bool auto_load))
      in
      As_shell.show "Loading %s. %s" file auto_load
  in
  let auto_load = auto_load () in
  show_run_start file auto_load;
  Toploop.initialize_toplevel_env ();
  Toploop.set_paths ();
  List.iter Topdirs.dir_directory (includes auto_load);
  if not (Sys.file_exists file)
  then As_shell.fatal_error 1 "missing %s." file
  else
  match Toploop.use_silently Format.err_formatter file with
  | false -> As_shell.fatal_error 1 "while loading %s." file
  | true -> ((* TODO error if we see nothing ran *))

let process ?(file = "assemble.ml") name fn =
  let auto_load = auto_load () in
  As_shell.show "Loading %s. %s"
    (As_shell.color `Bold file)
    (if auto_load then "" else
     Printf.sprintf "[auto-load: %s]"
       (As_shell.color `Magenta (string_of_bool auto_load)));
  Toploop.initialize_toplevel_env ();
  Toploop.set_paths ();
  let includes = includes auto_load in
  List.iter Topdirs.dir_directory includes;
  if not (Sys.file_exists file) then
    As_shell.fatal_error 1 "missing %s." file
  else match Toploop.use_silently Format.err_formatter file with
  | false -> As_shell.fatal_error 1 "while loading `%s'." file
  | true  ->
      match projects () with
      | [] -> As_shell.fatal_error 2 "No projects are registered in `%s'." file
      | ts ->
            let features = List.fold_left (fun acc t ->
              As_features.Set.union (As_project.features t) acc
            ) As_features.Set.empty ts in
            let features =
              As_features.Set.fold (fun f acc ->
                  As_features.(acc ||| atom f)
                ) features As_features.false_ in
            let env = As_build_env.parse name features in
            List.iter (fun t ->
                check t;
                fn t env
              ) ts
