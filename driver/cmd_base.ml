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
open Assemblage.Private
open Assemblage_driver
open Cmdliner

let err_multi_project ~using =
  strf "Unsupported: multiple projects assembled in the loaded files. \
        Using the project named `%s' and ignoring the others."
    (Project.name using)

let err_no_project files =
  "No project assembled in any of the loaded files. Did you call \
   Assemblage.assemble with your project value ?"

let common_option_section = "COMMON OPTIONS"

let help_sections =
  [ `S common_option_section;
    `P "These options are common to all commands.";
    `S "ENVIRONMENT";
    `P "$(mname) commands make use of the following environment variables:";
  ] @ Driver.man_vars () @
  [ `S "BUGS";
    `P "Check bug reports at https://github.com/samoht/assemblage/issues.";
    `S "AUTHORS";
    `P "Thomas Gazagnaire <thomas@gazagnaire.org>"; `Noblank;
    `P "Daniel C. Buenzli <daniel.buenzl i@erratique.ch>"; ]

let see_also_section cmds =
  if cmds = [] then [] else
  let see_also = List.map (strf "$(b,$(mname)-%s)(1)") cmds in
  let see_also = String.concat ~sep:", " ("$(b,$(mname))(1)" :: see_also) in
  [ `S "SEE ALSO"; `P see_also ]

let default_cmd init =
  let doc = "assemble software projects" in
  let man =
    [ `S "DESCRIPTION";
      `P "Assemblage provides an OCaml API and a command line tool \
          to setup, build and manage software projects.";
      `P "Use '$(mname) help $(i,COMMAND)' for information about \
          $(i,COMMAND).";
    ] @ help_sections
  in
  let exec_name = Filename.basename Sys.argv.(0) in
  let no_cmd_err _ = `Error (true, "No command specified.") in
  Term.(ret (pure no_cmd_err $ init)),
  Term.info exec_name ~version:"%%VERSION%%" ~sdocs:common_option_section
    ~doc ~man

type 'a cmd =
  [ `With_project of
      unit Term.t -> Project.t option -> Conf.t Term.t -> Manpage.block list ->
      'a Term.t * Term.info
  | `With_project_no_config of
      unit Term.t -> Project.t option -> 'a Term.t * Term.info
  | `No_project of unit Term.t -> 'a Term.t * Term.info ]

let cmd name cmd ~doc ~man ~see_also =
  let man = man @ help_sections @ see_also_section see_also in
  let wrap init cmd = cmd in
  `No_project begin fun init ->
    Term.(ret (pure wrap $ init $ cmd)),
    Term.info name ~doc ~sdocs:common_option_section ~man
  end

let cmd_with_project ?(config = true) name cmd ~doc ~man ~see_also : 'a cmd =
  let wrap init p conf cmd =
    let p = match p with None -> assert false | Some p -> p in
    cmd (Project.with_conf p conf)
  in
  let wrap_no_config init p cmd =
    let p = match p with None -> assert false | Some p -> p in
    cmd p
  in
  if config then
    `With_project begin fun init p conf_opts conf_man ->
      let man = man @ conf_man @ help_sections @ see_also_section see_also in
      Term.(ret (pure wrap $ init $ pure p $ conf_opts $ cmd)),
      Term.info name ~doc ~sdocs:common_option_section ~man
    end
  else
    `With_project_no_config begin fun init p ->
      let man = man @ help_sections @ see_also_section see_also in
      Term.(ret (pure wrap_no_config $ init $ pure p $ cmd)),
      Term.info name ~doc ~sdocs:common_option_section ~man
    end

let project_conf p =
  (* Project base configuration, overriden by configuration scheme,
     overriden by command line options. The driver also uses keys
     that may not be used by the project's actions we also add them
     to the mix. *)
  let driver_keys =
    Conf.Key.(Set.singleton (hide_type Conf.mkdir)
              |> Set.add (hide_type Conf.build_dir))
  in
  let base_keys = match p with
  | None -> driver_keys
  | Some p -> Conf.Key.Set.union (Project.deps p) driver_keys
  in
  let base = Conf.of_keys base_keys in
  let schemes = match p with None -> [] | Some p -> Project.schemes p in
  let scheme = Conf_spec.scheme_ui schemes in
  let opts = Conf_spec.ui base in
  let merge base scheme opts = match scheme with
  | None -> Conf.merge base opts
  | Some (_, (_, scheme)) -> Conf.merge (Conf.merge base scheme) opts
  in
  let conf = Term.(pure merge $ pure base $ scheme $ opts) in
  let man = Conf_spec.(scheme_man schemes @ man base) in
  conf, man

let cmds_terms init p cmds =
  let conf, conf_man = project_conf p in (* Do it only once *)
  let add_cmd = function
  | `With_project cmd -> cmd init p conf conf_man
  | `With_project_no_config cmd -> cmd init p
  | `No_project cmd -> cmd init
  in
  default_cmd init :: List.map add_cmd cmds

let terms cmds =
  let init, t = Driver.init ~version_opt:true ~docs:common_option_section () in
  match init with
  | None -> cmds_terms t None cmds
  | Some (_, l) ->
      let err msg = Term.(ret (pure (fun () -> `Error (false, msg)) $ t)) in
      match Project.list () with
      | [] -> cmds_terms (err (err_no_project l.Loader.files)) None cmds
      | [p] -> cmds_terms t (Some p) cmds
      |  ps ->
          let p = List.hd ps in
          Log.warn "%a" Fmt.pp_text (err_multi_project ~using:p);
          cmds_terms t (Some p) cmds
