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
open Cmdliner

(* Configuration *)

module Conf_spec = struct

  (* Configuration specification *)

  let uppercase = function
  | None -> None
  | Some s -> Some (String.Ascii.uppercase s)

  let value_converter_of_converter (parse, _) =
    let parse s = match parse s with
    | Ok v -> `Ok (Some (Conf.const v)) | Error (`Msg msg) -> `Error msg
    in
    let print = Fmt.nop (* config is needed for accurate default values so
                           don't print anything *)
    in
    parse, print

  let ui c =
    let add (Conf.Key.V k) (names, acc) =
      if not (Conf.Key.public k) then (names, acc) else
      let name =
        let name = Conf.Key.name k in
        if not (String.Set.mem name names) then name else
        begin
          Log.warn "%a" Conf.pp_key_dup (Conf.Key.V k);
          String.make_unique_in names name
        end
      in
      let names' = String.Set.add name names in
      let c = value_converter_of_converter (Conf.Key.converter k) in
      (* We suffix the name to avoid end-user clashes with other options *)
      let opt_name = strf "%s-key" name in
      let doc = Conf.Key.doc k in
      let docs = uppercase (Conf.Key.docs k) in
      let docv = Conf.Key.docv k in
      let i = Arg.info [opt_name] ?doc ?docv ?docs in
      let opt = Arg.(value (opt c None & i)) in
      let set acc k = function None -> acc | Some v -> Conf.set acc k v in
      let acc' = Term.(pure set $ acc $ pure k $ opt) in
      (names', acc')
    in
    let acc = (String.Set.empty, Cmdliner.Term.pure Conf.empty) in
    snd (Conf.Key.Set.fold add (Conf.domain c) acc)

  let builtin_sections =
    let open Conf in
    [ docs_project, doc_project;
      docs_build_properties, doc_build_properties;
      docs_build_directories, doc_build_directories;
      docs_ocaml_system, doc_ocaml_system;
      docs_c_system, doc_c_system;
      docs_machine_information, doc_machine_information;
      docs_system_utilities, doc_system_utilities; ]

  let man_empty =
    [ `S "CONFIGURATION KEYS";
      `P "There are no known configuration keys. This may be due to one of the
          following reasons: the project uses no configuration keys, there was
          an error during assemble file loading, you are consulting the static
          version of $(mname)'s man page." ]

  let man c =            (* We only add builtin_sections that appear in [c]. *)
    let conf_sections =
      let add (Conf.Key.V k) acc = match Conf.Key.docs k with
      | None -> acc | Some sec -> String.Set.add sec acc
      in
      Conf.Key.Set.fold add (Conf.domain c) String.Set.empty
    in
    let add_section acc (title, doc) =
      if not (String.Set.mem title conf_sections) then acc else
      `P doc :: `S (String.Ascii.uppercase title) :: acc
    in
    if Conf.is_empty c then man_empty else
    List.rev (List.fold_left add_section [] builtin_sections)

  (* Configuration scheme selection *)

  let docs = "CONFIGURATION SCHEMES"

  let scheme_ui sl =
    if sl = [] then Term.(pure None) else
    let names = List.map (fun (name, _) -> (name, name)) sl in
    let conv = Arg.(some ~none:"none" (Arg.enum names)) in
    let vopt = Some (fst (List.hd names)) in
    let doc_names = Arg.doc_alts_enum names in
    let scheme_name =
      Arg.(value & opt ~vopt conv None & info ["s"; "scheme"]
             ~docs ~docv:"SCHEME"
             ~doc:(strf
                     "Use the given configuration scheme. $(docv) must be %s."
                     doc_names))
    in
    let select name = match name with
    | None -> None
    | Some name -> Some (name, (List.assoc name sl))
    in
    Term.(pure select $ scheme_name)

  let scheme_man sl =
    if sl = [] then [] else
    let sl = List.sort compare sl in
    let add_scheme acc (name, (doc, _)) = `I (name, doc) :: acc in
    (`S docs) ::
    (`P "The following configuration schemes are available.") ::
    (List.rev (List.fold_left add_scheme [] sl))
end

(* Library preferences *)

module Lib_prefs = struct

  (* Enums *)

  let color_enum = ["auto", `Auto; "always", `Always; "never", `Never]
  let color_doc = Arg.doc_alts_enum color_enum
  let color_conv = Arg.enum color_enum

  let log_level_enum =
    [ "quiet", None; "error", Some Log.Error; "warning", Some Log.Warning;
      "info", Some Log.Info; "debug", Some Log.Debug; ]
  let log_level_doc = Arg.doc_alts_enum log_level_enum
  let log_level_conv = Arg.enum log_level_enum

  let vcs_kind_enum = [ "git", `Git; "hg", `Hg ]
  let vcs_kind_doc = Arg.doc_alts_enum vcs_kind_enum
  let vcs_kind_conv = Arg.enum vcs_kind_enum

  (* Preferences *)

  type t =
    { fmt_utf8_enabled : bool;
      fmt_style_tags : [ `Ansi | `None ];
      log_level : Log.level option;
      cmd_vcs_override_kind : Vcs.t option;
      cmd_vcs_override_exec : string option; }

  let pp ppf p =
    let none ppf () = Fmt.string ppf "none" in
    let pp_style_tags ppf = function
    | `Ansi -> Fmt.string ppf "ansi"
    | `None -> Fmt.string ppf "none"
    in
    Fmt.pf ppf "@[<v>@[fmt_utf8_enabled@ = @[%b@]@]@,\
                     @[fmt_style_tags@ = @[%a@]@]@,\
                     @[log_level@ = @[%a@]@]@,\
                     @[cmd_vcs_override_kind@ = @[%a@]@]@,\
                     @[cmd_vcs_override_exec@ = @[%a@]@]@]"
      p.fmt_utf8_enabled
      pp_style_tags p.fmt_style_tags
      (snd log_level_conv)  p.log_level
      Fmt.(option ~none (snd vcs_kind_conv)) p.cmd_vcs_override_kind
      Fmt.(option ~none string) p.cmd_vcs_override_exec

  let set c =
    Fmt.set_utf_8_enabled c.fmt_utf8_enabled;
    Fmt.set_style_tags c.fmt_style_tags;
    Log.set_level c.log_level;
    Vcs.set_override_kind c.cmd_vcs_override_kind;
    Vcs.set_override_exec c.cmd_vcs_override_exec;
    ()

  let get () =
    { fmt_utf8_enabled = Fmt.utf_8_enabled ();
      fmt_style_tags = Fmt.style_tags ();
      log_level = Log.level ();
      cmd_vcs_override_kind = Vcs.override_kind ();
      cmd_vcs_override_exec = Vcs.override_exec (); }

  (* Environment variables *)

  let env_bool e = match OS.Env.var e with
  | None -> None
  | Some v ->
      match String.Ascii.lowercase v with
      | "" | "false" | "0" -> Some false
      | _ -> Some true

  let env_enum e enum_def = match OS.Env.var e with
  | None -> None
  | Some v ->
      let v = String.Ascii.lowercase v in
      try Some (List.assoc v enum_def) with Not_found -> None

  let var_color = "ASSEMBLAGE_COLOR"
  let var_utf8_msgs = "ASSEMBLAGE_UTF8_MSGS"
  let var_vcs_kind = "ASSEMBLAGE_VCS_KIND"
  let var_vcs = "ASSEMBLAGE_VCS"
  let var_verbose = "ASSEMBLAGE_VERBOSE"

  let man_vars =
    let doc var doc = `I (strf "$(i,%s)" var, doc) in
    [ doc var_color "See option $(b,--color).";
      doc var_utf8_msgs "Use UTF-8 characters in $(mname) messages.";
      doc var_vcs_kind (strf "Override assemblage's VCS discovery. Use %s."
                          vcs_kind_doc);
      doc var_vcs (strf "Specify the VCS executable to use, only used if $(i,%s)
                         is defined." var_vcs_kind);
      doc var_verbose "See option $(b,--verbose)."; ]

  (* Command line and environment interface *)

  let color_opt docs =
    let doc = strf "Colorize the output. $(docv) must be %s." color_doc in
    Arg.(value & opt color_conv `Auto & info ["color"] ~doc ~docv:"WHEN" ~docs)

  let verbose_opts docs =
    let verbose =
      Arg.(value & opt ~vopt:(Some Log.Info) log_level_conv (Some Log.Warning) &
           info ["v"; "verbose"] ~docs ~docv:"LEVEL"
           ~doc:(strf "Be more or less verbose. $(docv) must be %s."
                   log_level_doc))
    in
    let quiet =
      let doc = "Be quiet. Takes over $(b,--verbose)." in
      Arg.(value & flag & info ["q"; "quiet"] ~doc ~docs)
    in
    let choose quiet verbose_opt = if quiet then None else verbose_opt in
    Term.(pure choose $ quiet $ verbose)

  let ui color verb =
    (* Override command line with environment variables *)
    let override value ~on = match on with None -> value | Some v -> v in
    let fmt_utf8_enabled = override true ~on:(env_bool var_utf8_msgs) in
    let fmt_style_tags =
      match override color ~on:(env_enum var_color color_enum) with
      | `Auto (* FIXME when Unix.is_atty stdout, $TERM  *) -> `Ansi
      | `Always -> `Ansi
      | `Never -> `None
    in
    let log_level = override verb ~on:(env_enum var_verbose log_level_enum) in
    let cmd_vcs_override_kind = env_enum var_vcs vcs_kind_enum in
    let cmd_vcs_override_exec = OS.Env.var var_vcs in
    { fmt_utf8_enabled; fmt_style_tags; log_level; cmd_vcs_override_kind;
      cmd_vcs_override_exec }

  let ui ~docs = Term.(pure ui $ color_opt docs $ verbose_opts docs)
end

(* Assemble file loader *)

module Loader = struct

  (* Loader *)

  type kind = [ `Toplevel ]

  type t =
    { kind : [ `Toplevel ];
      ocamlfind_exec : string;
      auto_lib : bool;
      includes : string list;
      files : Path.t list; }

  let pp_kind ppf k = match k with `Toplevel -> Fmt.string ppf "toplevel"
  let pp ppf l =
    Fmt.pf ppf "@[<v>@[kind@ = @[%a@]@,\
                     @[ocamlfind_exec@ = @[%s@]@]@,\
                     @[auto_lib@ = @[%b@]@]@,\
                     @[includes@ = @[%a@]@]@,\
                     @[files@ = @[%a@]@]@]"
      pp_kind l.kind
      l.ocamlfind_exec
      l.auto_lib
      Fmt.(list ~sep:sp string) l.includes
      Fmt.(list ~sep:sp Path.pp) l.files


  let header = "LOADER" (* logging header *)

  let err_missing file = R.msgf "%s: no such file to load" (Path.to_string file)
  let err_loading file = R.msgf "%s: error while loading" (Path.to_string file)
  let err_no_ocamlfind exec =
    strf "ocamlfind command not found (%s was used). Use the \
          ASSEMBLAGE_OCAMLFIND environment variable to specify the path to \
          ocamlfind or invoke the driver with --auto-lib=false and use -I to \
          indicate the path to the assemblage library."
      (Cmdliner.Arg.doc_quote exec)

  let check_ocamlfind exec =
    OS.Cmd.exists exec >>= fun exists ->
    if exists then Ok () else R.error_msg (err_no_ocamlfind exec)

  let all_incs l =
    if not l.auto_lib then Ok l.includes else
    check_ocamlfind l.ocamlfind_exec
    >>= fun () ->
    OS.Cmd.exec_read_lines l.ocamlfind_exec ["query"; "-r"; "assemblage" ]
    |> R.reword_error_msg
      (fun _ -> R.msg "ocamlfind lookup for package `assemblage' failed.")
    >>= fun auto_incs -> Ok (l.includes @ auto_incs)

  let toplevel_load level l =
    let add_include inc =
      Log.debug ~header "include: %s" inc; Topdirs.dir_directory inc
    in
    let rec loop = function
    | [] -> Ok ()
    | f :: fs ->
        OS.File.exists f >>= fun exists ->
        if not exists then Error (err_missing f) else
        let file = Path.to_string f in
        Log.msg level "Loading file %s" file;
        match Toploop.use_silently Format.err_formatter file with
        | false -> Error (err_loading f)
        | true -> loop fs
    in
    Toploop.initialize_toplevel_env ();
    Toploop.set_paths ();
    all_incs l >>= fun incs -> List.iter add_include incs; loop l.files

  let load ?(level = Log.Info) l =
    Log.info ~header "%a with auto-lib: %b" pp_kind l.kind l.auto_lib;
    match l.kind with `Toplevel -> toplevel_load level l

  (* Environment variables *)

  let var_ocamlfind = "ASSEMBLAGE_OCAMLFIND"

  let man_vars ?kinds () =
    let doc var doc = `I (strf "$(i,%s)" var, doc) in
    [ doc var_ocamlfind "Specify the ocamlfind executable to use when loading
                         assemble files. Note that this is different from
                         the ocamlfind configuration key used to configure a
                         project." ]

  (* Command line and environment interface *)

  let path_arg =
    let parse s = match Path.of_string s with
    | None -> `Error (strf "%a: not a path" String.pp s)
    | Some p -> `Ok p
    in
    parse, Path.pp

  let auto_lib_opt docs =
    let doc = "Use ocamlfind to automatically lookup the assemblage library
               for loading assemble files. See also the
               $(i,ASSEMBLAGE_OCAMLFIND) environment variable."
    in
    Arg.(value & opt bool true & info ["auto-lib"] ~doc ~docs ~docv:"BOOL")

  let includes_opt docs =
    let doc = "Add $(docv) to the driver include directories" in
    Arg.(value & opt_all string [] &
         info ["I"; "include"] ~docv:"DIR" ~doc ~docs)

  let files_opt docs =
    let doc = "Load the OCaml source $(docv) in the driver. The option can
               be repeated. If absent looks for a file named `assemble.ml'
               in the current directory."
    in
    Arg.(value & opt_all path_arg [Path.v "assemble.ml"] &
         info [ "f"; "file"] ~docv:"FILE" ~doc ~docs)

  let ui auto_lib includes files =
    let override value ~on = match on with None -> value | Some v -> v in
    let ocamlfind_exec = override "ocamlfind" ~on:(OS.Env.var var_ocamlfind) in
    let kind = `Toplevel in
    { kind; auto_lib; ocamlfind_exec; includes; files }

  let ui ?kinds ?files ~docs () =
    let files = match files with None -> files_opt docs | Some files -> files in
    Term.(pure ui $ auto_lib_opt docs $ includes_opt docs $ files)
end

module Driver = struct

  type init = (Lib_prefs.t * Loader.t) option

  let init ?(version_opt = false) ?kinds ~docs () =
    let lib_prefs = Lib_prefs.ui ~docs in
    let loader = Loader.ui ?kinds ~docs () in
    let t = Term.(pure (fun a b -> a, b) $ lib_prefs $ loader) in
    let ret r = Term.(ret (pure (fun _ -> r) $ t (* get the cl spec in *))) in
    match Term.eval_peek_opts ~version_opt t with
    | None, _ -> None, ret (`Ok ())
    | Some (lib_prefs, loader as v), res ->
        Lib_prefs.set lib_prefs;
        match Loader.load loader with
        | Ok ()  -> Some v, ret (`Ok ())
        | Error (`Msg msg) -> None, ret (`Error (false, msg))

  let man_vars ?kinds () =
    List.sort compare (Lib_prefs.man_vars @ Loader.man_vars ?kinds ())
end
