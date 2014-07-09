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

open Cmdliner
open Printf

let headers =
  let doc = Arg.info
      ~docv:"FILE"
      ~doc:"List of C headers to include in the generated C stubs."
      ["headers"] in
  Arg.(value & opt (list string) [] & doc)

let project_name =
  let doc = Arg.info
      ~docv:"MODULE"
      ~doc:"The name of the library for which the user wrote some binding."
      [] in
  Arg.(required & pos 0 (some string) None & doc)

let default_name = "$(b,NAME)"

type t = {
  default: string;
  mk: string -> string;
  term: string Term.t;
}

let ml_stubs =
  let mk = sprintf "%s_stubs.ml" in
  let default = mk default_name in
  let doc = Arg.info
      ~docv:"FILE"
      ~doc:"The name of the stub `.ml` file which will be generated."
      ["ml-stubs"] in
  let term = Arg.(value & opt string default & doc) in
  { default; mk; term }

let c_stubs =
  let mk = sprintf "%s_stubs.c" in
  let default = mk default_name in
  let doc = Arg.info
      ~docv:"FILE"
      ~doc:"The name of the stub `.c` file which will be generated."
      ["c-stubs"] in
  let term = Arg.(value & opt string default & doc) in
  { default; mk; term }

let funct =
  let mk x = sprintf "%s_bindings.Make" (String.capitalize x) in
  let default = mk default_name in
  let doc = Arg.info
      ~docv:"FUNCTOR"
      ~doc:"The name of the functor which defined the Ctypes bindings."
      ["functor"] in
  let term = Arg.(value & opt string default & doc) in
  { default; mk; term }

let generator =
  let mk = sprintf "%s_generator.ml" in
  let default = mk default_name in
  let doc = Arg.info
      ~docv:"FILE"
      ~doc:"The name of the generator source file."
      ["generator"] in
  let term = Arg.(value & opt string default & doc) in
  { default; mk; term }

let library =
  let mk x = x ^ ".ml" in
  let default = mk default_name in
  let doc = Arg.info
      ~docv:"FILE"
      ~doc:"The name of the library file."
      ["library"] in
  let term = Arg.(value & opt string default & doc) in
  { default; mk; term }

let output_generator_ml
    headers ml_stubs_ c_stubs_ funct_ generator_ library_ name =
  let mk t x = if x = t.default then t.mk name else x in
  let ml_stubs = mk ml_stubs ml_stubs_ in
  let c_stubs  = mk c_stubs c_stubs_ in
  let funct = mk funct funct_ in
  let generator = mk generator generator_ in
  let library = mk library library_ in
  let buf = Buffer.create 1024 in
  let p fmt = bprintf buf (fmt ^^ "\n") in
  p "(* Generated by Assemblage *)";
  p "let c_headers = [";
  List.iter (p "  \"#include <%s.h>\";") headers;
  p "]";
  p "";
  p "let main () =";
  if Filename.dirname ml_stubs <> "." then
    p "  let _ = Sys.command \"mkdir -p %s\" in" (Filename.dirname ml_stubs);
  if Filename.dirname ml_stubs <> Filename.dirname c_stubs then
    p "  let _ = Sys.command \"mkdir -p %s\" in" (Filename.dirname c_stubs);
  p "  let ml_out = open_out \"%s\"" ml_stubs;
  p "  and c_out = open_out \"%s\" in" c_stubs;
  p "  let ml_fmt = Format.formatter_of_out_channel ml_out";
  p "  and c_fmt = Format.formatter_of_out_channel c_out in";
  p "  List.iter (Format.fprintf c_fmt \"%%s@\\n\") c_headers;";
  p "  Cstubs.write_c c_fmt ~prefix:\"%s_stub_\" (module %s);" name funct;
  p "  Cstubs.write_ml ml_fmt ~prefix:\"%s_stub_\" (module %s);" name funct;
  p "  Format.pp_print_flush ml_fmt ();";
  p "  Format.pp_print_flush c_fmt ();";
  p "  close_out ml_out;";
  p "  close_out c_out";
  p "";
  p "let () = main ()";
  p "";

  let oc = open_out generator in
  output_string oc (Buffer.contents buf);
  close_out oc;
  let bindings =
    try
      let i = String.index funct '.' in
      String.sub funct 0 i
    with Not_found ->
      Shell.fatal_error 1 "%s is not a functor!" funct in
  let stubs = String.capitalize Filename.(basename (chop_extension ml_stubs)) in
  let oc = open_out library in
  output_string oc "(* Generated by Assemblage. *)\n\n";
  output_string oc (sprintf "include %s\n" bindings);
  output_string oc (sprintf "include %s(%s)\n" funct stubs);
  close_out oc

let () =
  let term =
    Term.(pure output_generator_ml
          $ headers $ ml_stubs.term $ c_stubs.term
          $ funct.term $ generator.term $ library.term
          $ project_name) in
  let info = Term.info
      ~version:"0.1"
      ~doc:"Helper for Ctype stubs generation."
      ~man:[]
      "ctype-gen" in
  match Term.eval (term, info) with
  | `Ok conf -> conf
  | `Version -> exit 0
  | `Help    -> exit 0
  | `Error _ -> exit 1
