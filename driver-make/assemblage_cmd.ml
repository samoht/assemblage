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

open Assemblage

let str = Printf.sprintf

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

(* Command line interface *)

open Cmdliner;;

let global_option_section = "COMMON OPTIONS"
let help_sections =
  let env_docs = Assemblage_env.variable_docs in
  [ `S global_option_section;
    `P "These options are common to all commands.";
    `S "ENVIRONMENT";
    `P "$(mname) commands make use of the following environment variables:";
  ] @
  (List.map (fun (v, doc) -> `I (str "$(i,%s)" v, doc)) env_docs) @
  [ `S "AUTHORS";
    `P "Thomas Gazagnaire <thomas@gazagnaire.org>"; `Noblank;
    `P "Daniel C. Buenzli <daniel.buenzl i@erratique.ch>";
    `S "BUGS";
    `P "Check bug reports at https://github.com/samoht/assemblage/issues."; ]

let setup_env_opts setup_env =
  (* These are the same options that are parsed by
     Assemblage_env.parse_setup ().
     We need them if assemblage is being run otherwise they will be
     unrecognized options and will lead to errors. If a standalone
     assemble.ml file is run we don't expose them as they don't make sense. *)
  let docs = global_option_section in
  let includes_opt =
    let doc = "List of directories to includes when loading assemble.ml." in
    if setup_env = None then Term.pure [] else
    Arg.(value & opt_all string [] &
         info ["I"; "include"] ~docv:"DIR" ~doc ~docs)
  in
  let auto_load_opt =
    let doc = "Automatically include the paths $(b,`ocamlfind query \
               -r assemblage`) before loading assemble.ml."
    in
    if setup_env = None then Term.pure false else
    Arg.(value & opt bool true & info ["auto-load"] ~doc ~docs ~docv:"BOOL")
  in
  let assemble_file_opt =
    let doc = "Read $(docv) as the assemble.ml file." in
    if setup_env = None then Term.pure "assemble.ml" else
    Arg.(value & opt non_dir_file "assemble.ml" &
         info [ "f"; "file"] ~docv:"FILE" ~doc ~docs)
  in
  let setup_env _ _ _ = (* just so that the term has the opts *) setup_env in
  Term.(pure setup_env $ auto_load_opt $ includes_opt $
        assemble_file_opt)

let env_opts setup_env =
  let docs = global_option_section in
  let verbose_opt =
    let doc = "Give verbose output." in
    Arg.(value & flag & info ["v"; "verbose"] ~doc ~docs)
  in
  let color_opt =
    let doc = "Colorize the output. $(docv) must one of `always`, `never' \
               or `auto'."
    in
    let color_tri_state = ["auto", `Auto; "always", `Always; "never", `Never] in
    Arg.(value & opt (enum color_tri_state) `Auto & info ["color"] ~doc
           ~docv:"WHEN" ~docs)
  in
  Term.(pure Assemblage_env.create $ setup_env_opts setup_env $ verbose_opt $
        color_opt)

let setup_env_opts p =
  let cond_atoms = match p with
  | None -> Cond.builtin
  | Some p -> Project.cond_atoms p
  in
  Asd_setup_env.term cond_atoms

let no_project setup = match setup with
| None -> assert false
| Some setup ->
    match setup.Assemblage_env.exec_status with
    | `Ok () -> assert false
    | `Error msg -> Log.err "%s" msg; exit 1

let help_cmd setup_env =
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
  Term.(ret (pure help $ env_opts setup_env $ Term.man_format $
             Term.choice_names $ topic)),
  Term.info "help" ~doc ~sdocs:global_option_section ~man

let setup_cmd p setup_env =
  let doc = "setup an OCaml project" in
  let man =
    [ `S "DESCRIPTION";
      `P "The $(b,setup) command generates a build system to build the \
          components defined in an assemble.ml file. A default configuration \
          for the build system can be specified on the command line using \
          the flags described below. The configuration can be changed on \
          build system invocations without having to $(b,setup) again."; ] @
    help_sections;
  in
  let setup = match p with
  | None -> fun _ _ _ _ _ -> no_project setup_env
  | Some p ->
      (* FIXME let the user specify on the command line *)
      let version = Asd_project_version.get () in
      fun s b d make merlin -> Asd_setup.setup ~version p s b d make ~merlin
  in
  let dumpast_opt =
    let doc = "Dump the AST during build (optimisation). FIXME" in
    Arg.(value & opt bool true & info ["dumpast"] ~doc ~docv:"BOOL")
  in
  let merlin_opt =
    let doc = "Generate a .merlin file." in
    Arg.(value & opt bool true & info ["merlin"] ~doc ~docv:"BOOL")
  in
  Term.(ret (pure setup $ env_opts setup_env $ setup_env_opts p $ dumpast_opt $
             pure `Make $ merlin_opt)),
  Term.info "setup" ~doc ~sdocs:global_option_section ~man

let describe_cmd p setup_env =
  let doc = "describe an OCaml project" in
  let man =
    [ `S "DESCRIPTION";
      `P "The $(b,describe) command outputs a summary of the components \
          defined by an assemble.ml file.";
    ] @ help_sections;
  in
  let describe = match p with
  | None -> fun _ _ -> no_project setup_env
  | Some p ->
      (* FIXME let the user specify on the command line *)
      let version = Asd_project_version.get () in
      Asd_describe.describe ~version p
  in
  Term.(ret (pure describe $ env_opts setup_env $ setup_env_opts p)),
  Term.info "describe" ~doc ~sdocs:global_option_section ~man

let default_cmd setup_env =
  let doc = "setup, manage and use OCaml projects" in
  let man =
    [ `S "DESCRIPTION";
      `P "Assemblage provides an OCaml API and a command line tool \
          to setup, manage and use OCaml projects.";
      `P "Use '$(mname) help $(i,COMMAND)' for information about \
          $(i,COMMAND).";
    ] @ help_sections
  in
  let exec_name = Filename.basename Sys.argv.(0) in
  let no_cmd_err _ = `Error (true, "No command specified.") in
  Term.(ret (pure no_cmd_err $ env_opts setup_env)),
  Term.info exec_name ~version:"%%VERSION%%" ~sdocs:global_option_section
    ~doc ~man

let assemble setup_env p =
  let cmds =
    [ help_cmd setup_env;
      setup_cmd p setup_env;
      describe_cmd p setup_env; ]
  in
  match Term.eval_choice (default_cmd setup_env) cmds with
  | `Ok () | `Version | `Help -> exit 0
  | `Error _ -> exit 1

let assemble_no_project setup_env = assemble (Some setup_env) None
let assemble p = assemble (Assemblage_env.get_setup ()) (Some p)
