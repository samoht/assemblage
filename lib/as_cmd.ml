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

let (|>) x f = f x

(* Command environment (as opposed to *build* environment) *)

type env = { verbose : bool }

let env_parsed = ref false
let did_run () = !env_parsed

let env verbose = env_parsed := true; { verbose }

(* Project processors *)

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

let configure _ `Make p build_env =
  let features = As_build_env.features build_env in
  let flags = As_build_env.flags build_env in
  let makefile = "Makefile" in
  let build_dir = As_build_env.build_dir build_env in
  check p;
  As_makefile.(write (of_project p ~features ~flags ~makefile));
  As_ocamlfind.META.(write (of_project p));
  As_opam.Install.(write (of_project ~build_dir p))

let describe _ p build_env =
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
          let build_dir = As_build_env.build_dir build_env in
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
      (As_shell.color `Underline (As_project.name p)) (As_project.version p);
  let components =
    As_project.components p
    |> List.filter (function `Lib _ | `Bin _ -> true | _ -> false)
  in
  print components

(* Command line interface *)

open Cmdliner;;

let help _ man_format cmds topic = match topic with
| None -> `Help (`Pager, None) (* help about the program. *)
| Some topic ->
    let topics = "topics" :: cmds in
    let conv, _ = Cmdliner.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
    match conv topic with
    | `Error e -> `Error (false, e)
    | `Ok t when t = "topics" -> List.iter print_endline topics; `Ok ()
    | `Ok t when List.mem t cmds -> `Help (man_format, Some t)
    | `Ok _ -> assert false

let global_option_section = "COMMON OPTIONS"
let help_sections = [
  `S global_option_section;
  `P "These options are common to all commands.";
  `S "AUTHORS";
  `P "Thomas Gazagnaire <thomas@gazagnaire.org>"; `Noblank;
  `P "Daniel C. Buenzli <daniel.buenzl i@erratique.ch>";
  `S "BUGS";
  `P "Check bug reports at https://github.com/samoht/assemblage/issues.";
]

let env =
  let docs = global_option_section in
  let verbose_opt =
    let doc = "Give verbose output." in
    Arg.(value & flag & info ["v"; "verbose"] ~doc ~docs)
  in
  Term.(pure env $ verbose_opt)

let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  let doc = "display help about assemblage and assemblage commands" in
  let man =
    [ `S "DESCRIPTION";
      `P "Prints help about assemblage commands and other subjects..."] @
    help_sections
  in
  Term.(ret (pure help $ env $ Term.man_format $ Term.choice_names $ topic)),
  Term.info "help" ~doc ~sdocs:global_option_section ~man

let configure_cmd p build_env =
  let doc = "configure an OCaml project" in
  let man =
    [ `S "DESCRIPTION";
      `P "TODO"; ] @
    help_sections;
  in
  Term.(pure configure $ env $ pure `Make $ pure p $ build_env),
  Term.info "configure" ~doc ~sdocs:global_option_section ~man

let describe_cmd p build_env =
  let doc = "describe an OCaml project" in
  let man =
    [ `S "DESCRIPTION";
      `P "TODO"; ] @
    help_sections;
  in
  Term.(pure describe $ env $ pure p $ build_env),
  Term.info "describe" ~doc ~sdocs:global_option_section ~man

let default_cmd =
  let doc = "configure, manage and use OCaml projects" in
  let man =
    [ `S "DESCRIPTION";
      `P "Assemblage provides an OCaml API and a command line tool \
          to configure, manage and use OCaml projects."; ] @
    help_sections
  in
  let err = "No command specified." in
  Term.(ret (pure (fun _ -> `Error (true, err)) $ env)),
  Term.info "assemblage" ~version:"%%VERSION%%" ~sdocs:global_option_section
    ~doc ~man

let assemble p =
  let features = As_project.features p in
  let build_env = As_build_env.term features in
  let cmds =
    [ help_cmd;
      configure_cmd p build_env;
      describe_cmd p build_env; ]
  in
  match Term.eval_choice default_cmd cmds with
  | `Ok () | `Version | `Help -> exit 0
  | `Error _ -> exit 1
