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

(* Products *)

type product = As_path.rel As_conf.value
type products = As_path.rel list As_conf.value

let products_keep pred ps = As_conf.(const (List.filter pred) $ ps)
let products_keep_ext ext ps = products_keep (As_path.has_ext ext) ps
let products_keep_exts exts ps =
  let pred f = List.exists (fun e -> As_path.has_ext e f) exts in
  products_keep pred ps

(* Build commands *)

type cmd_spec =
  { exec_spec : string As_conf.key;
    args_spec : string list As_conf.value;
    stdin_spec : As_path.rel As_conf.value option;
    stdout_spec : As_path.rel As_conf.value option;
    stderr_spec : As_path.rel As_conf.value option; }

let cmd_deps cmd =
  let union = As_conf.Key.Set.union in
  let opt_deps v = match v with
  | None -> As_conf.Key.Set.empty
  | Some v -> As_conf.deps v
  in
  As_conf.Key.(Set.singleton (V cmd.exec_spec))
  |> union (As_conf.deps cmd.args_spec)
  |> union (opt_deps cmd.stdin_spec)
  |> union (opt_deps cmd.stdout_spec)
  |> union (opt_deps cmd.stderr_spec)

type cmds = cmd_spec list

let cmd ?stdin ?stdout ?stderr exec args =
  [{ exec_spec = exec;
     args_spec = args;
     stdin_spec = stdin;
     stdout_spec = stdout;
     stderr_spec = stderr; }]

let seq cmds cmds' =  List.rev_append (List.rev cmds) cmds'
let (<*>) = seq
let cmds_deps cmds =
  let add_cmd acc cmd = As_conf.Key.Set.union acc (cmd_deps cmd) in
  List.fold_left add_cmd As_conf.Key.Set.empty cmds

(* Portable system utility invocations *)

let add v acc = v :: acc
let add_if b v acc = if b then v :: acc else acc
let path_arg p = As_path.to_string p
let paths_args_rev ps = List.rev_map As_path.to_string ps
let paths_args ps = List.rev (paths_args_rev ps)

(* FIXME IMPORTANT copy and move on Win don't support multiple files, except if
   they are wildcards. For this reason ~src is not ~srcs. If we transform
   the type cmds to a value we can generate a sequence of commands for
   the moves. *)

let dev_null =
  let dev_null = function
  | "Win32" -> As_path.file "NUL"
  | _ -> As_path.(root / "dev" / "null")
  in
  As_conf.(const dev_null $ value host_os)

let ln ?stdout ?stderr ~src ~dst () =
  let args os src dst = match os with
  | "Win32" ->
      As_log.warn "Symbolic@ links@ unsupported@ copying@ instead.";
      [ "/Y"; path_arg src; path_arg dst; ]
  | _ -> [ "-s"; "-f"; path_arg src; path_arg dst; ]
  in
  let args = As_conf.(const args $ (value host_os) $ src $ dst) in
  cmd As_conf.ln args ?stdout ?stderr

let cp ?stdout ?stderr ~src ~dst () =
  let args os src dst = match os with
  | "Win32" -> [ "/Y"; path_arg src; path_arg dst; ]
  | _ -> [ path_arg src; path_arg dst; ]
  in
  let args = As_conf.(const args $ (value host_os) $ src $ dst) in
  cmd As_conf.cp args ?stdout ?stderr

let mv ?stdout ?stderr ~src ~dst () =
  let args os src dst = match os with
  | "Win32" -> [ "/Y"; path_arg src; path_arg dst; ]
  | _ -> [ path_arg src; path_arg dst; ]
  in
  let args = As_conf.(const args $ (value host_os) $ src $ dst) in
  cmd As_conf.cp args ?stdout ?stderr

let rm_files ?stdout ?stderr ?(f = As_conf.false_) paths =
  let args os f paths = match os with
  | "Win32" -> add_if f "/F" @@ add "/Q" @@ paths_args paths
  | _ -> add_if f "-f" @@ paths_args paths
  in
  let args = As_conf.(const args $ (value host_os) $ f $ paths) in
  cmd As_conf.rm args ?stdout ?stderr

let rm_dirs ?stdout ?stderr ?(f = As_conf.false_) ?(r = As_conf.false_) paths =
  let args os f r paths = match os with
  | "Win32" -> add_if f "/F" @@ add_if r "/S" @@ add "/Q" @@ paths_args paths
  | _ -> add_if f "-f" @@ add_if r "-r" @@ paths_args paths
  in
  let args = As_conf.(const args $ (value host_os) $ f $ r $ paths) in
  cmd As_conf.rmdir args ?stdout ?stderr

let mkdir ?stdout ?stderr dir =
  let args os dir = match os with
  | "Win32" -> [ path_arg dir ]
  | _ -> [ "-p"; path_arg dir ]
  in
  let args = As_conf.(const args $ (value host_os) $ dir) in
  cmd As_conf.mkdir args ?stdout ?stderr

(* Actions *)

type t =
  { cond : bool As_conf.value;
    ctx : As_ctx.t;
    inputs : As_path.rel list As_conf.value;
    outputs : As_path.rel list As_conf.value;
    cmds : cmds; }

let v ?(cond = As_conf.true_) ~ctx ~inputs ~outputs cmds =
  { cond; ctx; inputs; outputs; cmds }

let cond a = a.cond
let ctx a = a.ctx
let inputs a = a.inputs   (* FIXME should we thread r.cond here ? *)
let outputs a = a.outputs (* FIXME should we thread r.cond here ? *)
let cmds a = a.cmds
let deps a =
  let union = As_conf.Key.Set.union in
  cmds_deps a.cmds
  |> union (As_conf.deps a.cond)
  |> union (As_conf.deps a.inputs)
  |> union (As_conf.deps a.outputs)

(* Action evaluation *)

type cmd =
  { exec : string;
    args : string list;
    stdin : As_path.rel option;
    stdout : As_path.rel option;
    stderr : As_path.rel option; }

let eval_cmds conf a args =
  let eval_opt_fd fd = match fd with
  | None -> None
  | Some v -> Some (As_conf.eval conf v)
  in
  let add_cmd acc cmd =
    let ctx = As_ctx.add (`Cmd cmd.exec_spec) a.ctx in
    let exec = As_conf.eval conf (As_conf.value cmd.exec_spec) in
    let injected = As_args.eval_for_ctx conf args ctx in
    let args = injected @ (As_conf.eval conf cmd.args_spec) in
    let stdin = eval_opt_fd cmd.stdin_spec in
    let stdout = eval_opt_fd cmd.stdout_spec in
    let stderr = eval_opt_fd cmd.stderr_spec in
    { exec; args; stdin; stdout; stderr } :: acc
  in
  List.rev (List.fold_left add_cmd [] a.cmds)

(* Action specification combinators *)

module Spec = struct

  (* List configuration values *)

  type 'a list_v = 'a list As_conf.value

  let atom v = As_conf.(const [v])
  let atoms v = As_conf.(const v)

  let addl l l' = List.rev_append (List.rev l) l'
  let addl_if c l l' = if c then addl l l' else l'

  let add l l' = As_conf.(const addl $ l $ l')
  let add_if c l l' = As_conf.(const addl_if $ c $ l $ l')
  let add_if_key c l l' = add_if (As_conf.value c) l l'

  (* Path and products *)

  let path p ~ext:e =
    let change_ext p = As_path.(as_rel (change_ext p e)) in
    As_conf.(const change_ext $ p)

  let path_base p = As_conf.(const As_path.basename $ p)
  let path_dir p = As_conf.(const (fun p -> As_path.(as_rel (dirname p))) $ p)
  let path_arg ?opt p =
    let make_arg p =
      let p = As_path.to_string p in
      match opt with None -> [p] | Some opt -> [opt; p]
    in
    As_conf.(const make_arg $ p)

  let paths_args ?opt ps =
    let make_args ps =
      let add = match opt with
      | None -> fun acc p -> As_path.to_string p :: acc
      | Some opt -> fun acc p -> As_path.to_string p :: opt :: acc
      in
      List.rev (List.fold_left add [] ps)
    in
    As_conf.(const make_args $ ps)

  let product ?ext p =
    let p = match ext with None -> p | Some ext -> path p ~ext in
    As_conf.(const (fun p -> [p]) $ p)

  (* Commands *)
  let ( <*> ) = ( <*> )
end

let link ?stdout ?stderr ~src ~dst () =
  let open Spec in
  let add_if_some v acc = match v with
  | None -> acc
  | Some fd -> add (product fd) acc
  in
  let ctx = As_ctx.v [ `Link ] in
  let inputs = product src in
  let outputs = add_if_some stdout @@ add_if_some stderr @@ product dst in
  let cmd =
    (** FIXME here we really mean link src to dst when seen from
        the root directory. We are using `..` but As_path.t are
        not supposed to have such segments. Really need to sort out
        paths. *)
    let rec relativize src dst = match As_path.(as_rel (dirname dst)) with
    | d when As_path.is_current d -> src
    | d -> relativize (As_path.(as_rel (dir ".." // src))) d
    in
    let src = As_conf.(const relativize $ src $ dst) in
    ln ?stdout ?stderr ~src ~dst ()
  in
  v ~ctx ~inputs ~outputs cmd
