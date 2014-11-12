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

type product = As_path.t As_conf.value
type products = As_path.t list As_conf.value

(* Build commands *)

type cmd =
  { cmd_key : string As_conf.key;      (* Remember the key for the context. *)
    cmd : string;                     (* This is the evaluation of cmd_key. *)
    args : string list;                               (* command arguments. *)
    stdin : As_path.t option;                (* stdin redirection (if any). *)
    stdout : As_path.t option;              (* stdout redirection (if any). *)
    stderr : As_path.t option; }            (* stderr redirection (if any). *)

let cmd_cmd c = c.cmd
let cmd_args c = c.args
let cmd_stdin c = c.stdin
let cmd_stdout c = c.stdout
let cmd_stderr c = c.stderr

let cmd_ctx ctx c = As_ctx.add (`Cmd c.cmd_key) ctx
let cmd_args_with_ctx conf ctx args c =
  let injected = As_args.for_ctx conf (cmd_ctx ctx c) args in
  List.rev_append (List.rev injected) c.args

type cmds = cmd list As_conf.value
type cmd_gen =
  ?stdin:As_path.t -> ?stdout:As_path.t -> ?stderr:As_path.t ->
  string list -> cmd

let cmd c =
  let build_cmd cmd_key cmd = fun ?stdin ?stdout ?stderr args ->
    { cmd_key; cmd; args; stdin; stdout; stderr }
  in
  As_conf.(const build_cmd $ const c $ (value c))

let cmd_exec ?stdin ?stdout ?stderr c args =
  let stdin = As_conf.Option.wrap stdin in
  let stdout = As_conf.Option.wrap stdout in
  let stderr = As_conf.Option.wrap stderr in
  let build_cmds cmd_key cmd args stdin stdout stderr =
    [{ cmd_key; cmd; args; stdin; stdout; stderr }]
  in
  As_conf.(const build_cmds $ const c $ (value c) $ args $ stdin $ stdout $
           stderr)

let seq cmds cmds' = As_conf.List.(rev_append (rev cmds) cmds')
let (<*>) = seq

(* Portable system utility invocations *)

let add v acc = v :: acc
let add_if b v acc = if b then v :: acc else acc
let path_arg p = As_path.to_string p               (* FIXME quoting issues. *)
let paths_args_rev ps = List.rev_map As_path.to_string ps
let paths_args ps = List.rev (paths_args_rev ps)

let dev_null =
  let dev_null os = match os with
  | "Win32" -> As_path.file "NUL"
  | _ -> As_path.(root / "dev" / "null")
  in
  As_conf.(const dev_null $ value host_os)

let ln =
  let make_cmd os (exec : cmd_gen) = match os with
  | "Win32" ->
      As_log.warn "Symbolic@ links@ unsupported@ copying@ instead.";
      fun src dst -> exec [ "/Y"; path_arg src; path_arg dst;]
  | _ ->
      fun src dst -> exec [ "-s"; "-f"; path_arg src; path_arg dst;]
  in
  As_conf.(const make_cmd $ (value host_os) $ (cmd As_conf.ln ))

let ln_rel =
  (* FIXME here we really mean link src to dst when seen from
     the empty relative directory. We are using `..` but As_path.t are
     not supposed to have such segments. Really need to sort out
     paths. *)
  let see src ~from:dst = (* src as seen from dst, as short as possible *)
    let parent = Filename.parent_dir_name in
    let rec loop src dst = match As_path.Rel.dirname dst with
    | d when As_path.Rel.is_empty d -> src
    | d -> loop (As_path.Rel.(base parent // src)) d
    in
    match As_path.to_rel src, As_path.to_rel dst with
    | Some src, Some dst ->
        let pre = As_path.Rel.find_prefix src dst in
        let rem p = match As_path.Rel.rem_prefix pre p with
        | Some p -> p | None -> assert false
        in
        As_path.(of_rel (loop (rem src) (rem dst)))
    | _ -> src
  in
  let make_cmd ln src dst = ln (see src ~from:dst) dst in
  let ln_cmd = ln in
  As_conf.(const make_cmd $ ln_cmd)

let cp =
  let make_cmd os (exec : cmd_gen) = match os with
  | "Win32" -> fun src dst -> exec [ "/Y"; path_arg src; path_arg dst;]
  | _ -> fun src dst -> exec [ path_arg src; path_arg dst;]
  in
  As_conf.(const make_cmd $ value host_os $ cmd As_conf.cp)

let mv =
  let make_cmd os (exec : cmd_gen) = match os with
  | "Win32" -> fun src dst -> exec [ "/Y"; path_arg src; path_arg dst;]
  | _ -> fun src dst -> exec [ path_arg src; path_arg dst;]
  in
  As_conf.(const make_cmd $ value host_os $ cmd As_conf.mv)

let rm_files =
  let make_cmd os (exec : cmd_gen) = match os with
  | "Win32" ->
      fun ?(f = false) paths ->
        exec (add_if f "/F" @@ add "/Q" @@ paths_args paths)
  | _ ->
      fun ?(f = false) paths ->
        exec (add_if f "-f" @@ paths_args paths)
  in
  As_conf.(const make_cmd $ value host_os $ cmd As_conf.rm)

let rm_dirs =
  let make_cmd os (exec : cmd_gen) = match os with
  | "Win32" ->
      fun ?(f = false) ?(r = false) paths ->
        exec (add_if f "/F" @@ add_if r "/S" @@ add "/Q" @@ paths_args paths)
  | _ ->
      fun ?(f = false) ?(r = false) paths ->
        exec (add_if f "-f" @@ add_if r "-r" @@ paths_args paths)
  in
  As_conf.(const make_cmd $ value host_os $ cmd As_conf.rmdir)

let mkdir =
  let make_cmd os (exec : cmd_gen) = match os with
  | "Win32" -> fun dir -> exec [ path_arg dir ]
  | _ -> fun dir -> exec [ "-p"; path_arg dir ]
  in
  As_conf.(const make_cmd $ value host_os $ cmd As_conf.mkdir)

(* Actions *)

type t =
  { cond : bool As_conf.value;             (* [true] if available in config. *)
    args : As_args.t;               (* argument bundle to use on evaluation. *)
    ctx : As_ctx.t;                         (* context to use on evaluation. *)
    inputs : products;       (* inputs that need to exist and be up to date. *)
    outputs : products;          (* outputs that need to be touched by cmds. *)
    cmds : cmds; }                                       (* action commands. *)

let v ?(cond = As_conf.true_) ~ctx ~inputs ~outputs cmds =
  { cond; args = As_args.empty; ctx; inputs; outputs; cmds }

let cond a = a.cond
let args a = a.args
let ctx a = a.ctx
let inputs a = a.inputs   (* FIXME should we thread r.cond here ? *)
let outputs a = a.outputs (* FIXME should we thread r.cond here ? *)
let cmds a = a.cmds
let deps a =
  let union = As_conf.Key.Set.union in
  (As_conf.deps a.cond)
  |> union (As_args.deps a.args)
  |> union (As_conf.deps a.inputs)
  |> union (As_conf.deps a.outputs)
  |> union (As_conf.deps a.cmds)

let add_inputs ps a = { a with inputs = As_conf.List.append ps a.inputs }
let add_ctx_args ctx args a =
  { a with ctx = As_ctx.union ctx a.ctx; args = As_args.append args a.args }

(* Action specification combinators

   FIXME this needs reviewing e.g. w.r.t. As_conf.List
*)

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
    let change_ext p = As_path.(change_ext p e) in
    As_conf.(const change_ext $ p)

  let path_base p = As_conf.(const As_path.basename $ p)
  let path_dir p = As_conf.(const As_path.dirname $ p)
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

let link ~src ~dst () =
  let open Spec in
  let ctx = As_ctx.v [ `Link ] in
  let inputs = product src in
  let outputs = product dst in
  let cmd = As_conf.(List.singleton (ln_rel $ src $ dst)) in
  v ~ctx ~inputs ~outputs cmd
