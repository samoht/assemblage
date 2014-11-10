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

open Makefile.Infix
open Assemblage
open Assemblage.Private

let str = Format.asprintf

type gen =
  { proj : Project.t;   (* The project to generate. *)
    dirs : Path.Set.t;  (* Set of directories that need to exist. *)
    rmk : Makefile.t;   (* Reversed makefile definition. *) }

let generator proj =
  { proj; dirs = Path.Set.empty;
    rmk =
      `Blank ::
      (`Comment "Run `make help` to get the list of targets.") ::
      (`Comment (Project.watermark_string proj)) :: []; }

let keys gen =
  (* FIXME example, this is similar to how we used to variablify build actions
     but I'm not sure whether doing this is actually a good idea
     since it could potentially trip other derived configuration values
     that are e.g. testing strings. Also if we keep this it should be
     done in dep order. *)
  let alter k var (rmk, conf as no_alter) =
    if not (Conf.mem conf k) then no_alter else
    let v = Conf.(eval conf (get conf k)) in
    let conf = Conf.set conf k (Conf.const (str "$(%s)" var)) in
    (var =?= [str "%a" Conf.(printer (Key.converter k)) v]) :: rmk, conf
  in
  let rmk, conf =
    (`Blank :: gen.rmk, Project.conf (gen.proj))
    |> alter Conf.ocamlc "OCAMLC"
    |> alter Conf.ocamlopt "OCAMLOPT"
  in
  { gen with proj = Project.with_conf gen.proj conf; rmk }

let mk_recipe cmds =
  let add_cmd acc cmd =
    let redirect op file acc = match file with
    | None -> acc | Some file -> (Path.to_string file) :: op :: acc
    in
    let cmdline =
      [cmd.Action.exec]
      |> List.rev_append cmd.Action.args
      |> redirect "<" cmd.Action.stdin
      |> redirect "1>" cmd.Action.stdout
      |> redirect "2>" cmd.Action.stderr
      |> List.rev
    in
    cmdline :: acc
  in
  List.rev (List.fold_left add_cmd [] cmds)

let mk_action args gen action =
  if not (Project.eval gen.proj (Action.cond action)) then gen else
  let inputs = Project.eval gen.proj (Action.inputs action) in
  let outputs = Project.eval gen.proj (Action.outputs action) in
  let dirs = List.rev_map Path.dirname outputs in
  let order_only_prereqs = List.rev_map Path.to_string dirs in
  let prereqs = List.rev_map Path.to_string (List.rev inputs) in
  let targets = List.rev_map Path.to_string (List.rev outputs) in
  let cmds = Action.eval_cmds (Project.conf gen.proj) action args in
  let recipe = mk_recipe cmds in
  let dirs = List.fold_left (fun set d -> Path.Set.add d set) gen.dirs dirs in
  let rule = Makefile.rule ~order_only_prereqs ~targets ~prereqs ~recipe () in
  let rmk = rule :: gen.rmk in
  { gen with dirs; rmk; }

let mk_part gen p =
  if not (Project.eval gen.proj (Part.cond p)) then gen else
  match Part.actions p with
  | [] -> gen
  | actions ->
      let name = Part.name p in
      let kind = Part.kind p in
      let comment = str "%a-%s rules" Part.pp_kind kind name in
      let gen = { gen with rmk = `Comment comment :: `Blank :: gen.rmk } in
      List.fold_left (mk_action (Part.args p)) gen actions

let mk_gen_dirs gen =
  let add_dir dir gen =
    (* Fake action to use portable Action.mkdir to make the recipe *)
    let nil = Conf.const [] in
    let cmds = Action.mkdir (Conf.const dir) in
    let action = Action.v ~ctx:Ctx.empty ~inputs:nil ~outputs:nil cmds in
    let cmds = Action.eval_cmds (Project.conf gen.proj) action Args.empty in
    let recipe = mk_recipe cmds in
    let prereqs = [] in
    let targets = [Path.to_string dir] in
    let rule = Makefile.rule ~targets ~prereqs ~recipe () in
    let rmk = rule :: gen.rmk in
    { gen with rmk }
  in
  let rmk = `Comment ("Build directories rules") :: `Blank :: gen.rmk in
  Path.Set.fold add_dir gen.dirs { gen with rmk }

let of_project ~setup_files proj =
  let gen = generator proj in
  let gen = keys gen in
  let gen = List.fold_left mk_part gen (Project.parts gen.proj) in
  let gen = mk_gen_dirs gen in
  List.rev gen.rmk
