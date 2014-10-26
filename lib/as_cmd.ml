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

let str = Printf.sprintf

(* Command results *)

type 'a result = [ `Ok of 'a | `Error of string ]

let ret v = `Ok v
let error e = `Error e
let on_error ?(level = As_log.Error) ~use r = match r with
| `Ok v -> v
| `Error msg -> As_log.kmsg (fun () -> use) level "@[%a@]" As_fmt.pp_text msg

let bind v f = match v with `Ok v -> f v | `Error _ as e -> e
let map v f = match v with `Ok v -> `Ok (f v) | `Error _ as e -> e

let ( >>= ) = bind
let ( >>| ) = map

module Infix = struct
  let ( >>= ) = ( >>= )
  let ( >>| ) = ( >>| )
end

(* Working with files *)

module File = struct
  let exists file =
    let file = As_path.to_string file in
    try ret (Sys.file_exists file && not (Sys.is_directory file)) with
    | Sys_error e -> error e

  let null =
    let f = match Sys.os_type with "Win32" -> "NUL" | _ -> "/dev/null" in
    As_path.of_string f

  let apply f x ~finally y =
    let result = try f x with
    | e -> try finally y; raise e with _ -> raise e
    in
    finally y;
    result

  let with_inf f inf v =
    try
      let inf = As_path.to_string inf in
      let ic = if inf <> "" then open_in_bin inf else stdin in
      let close ic = if inf <> "" then close_in ic else () in
      apply (f ic) v ~finally:close ic
    with
    | Sys_error e -> error e

  let with_outf f outf v =
    try
      let outf = As_path.to_string outf in
      let oc = if outf <> "" then open_out_bin outf else stdout in
      let close oc = if outf <> "" then close_out oc else () in
      apply (f oc) v ~finally:close oc
    with
    | Sys_error e -> error e

  let input file =
    let input ic () =
      let len = in_channel_length ic in
      let s = String.create len in
      really_input ic s 0 len; ret s
    in
    with_inf input file ()

  let input_lines file = input file >>| (As_string.split ~sep:"\n")

  let output file contents =
    let output oc contents = output_string oc contents; ret () in
    with_outf output file contents

  let output_lines file lines = output file (String.concat "\n" lines)

  let output_subst vars file contents =
    let output_subst oc contents =                     (* man that's ugly. *)
      let s = contents in
      let start = ref 0 in
      let last = ref 0 in
      let len = String.length s in
      while (!last < len - 4) do
        if not (s.[!last] = '%' && s.[!last + 1] = '%') then incr last else
        begin
          let start_subst = !last in
          let last_id = ref (!last + 2) in
          let stop = ref false in
          while (!last_id < len - 1 && not !stop) do
            if not (s.[!last_id] = '%' && s.[!last_id + 1] = '%') then begin
              if s.[!last_id] <> ' '
              then (incr last_id)
              else (stop := true; last := !last_id)
            end else begin
              let id_start = start_subst + 2 in
              let id = String.sub s (id_start) (!last_id - id_start) in
              try
                let subst = List.assoc id vars in
                Pervasives.output oc s !start (start_subst - !start);
                output_string oc subst;
                stop := true;
                start := !last_id + 2;
                last := !last_id + 2;
              with Not_found ->
                stop := true;
                last := !last_id
            end
          done
        end
      done;
      Pervasives.output oc s !start (len - !start); ret ()
    in
    with_outf output_subst file contents

  let delete ?(maybe = false) file =
    exists file >>= fun exists ->
    if maybe && not exists then ret () else
    let file = As_path.to_string file in
    try ret (Sys.remove file) with Sys_error e -> error e

  let temp suff =
    try
      let f = As_path.of_string (Filename.temp_file "assemblage" suff) in
      at_exit (fun () -> ignore (delete f));
      ret f
    with Sys_error e -> error e
end

(* Working with directories. *)

module Dir = struct
  let exists dir =
    let dir = As_path.to_string dir in
    try ret (Sys.file_exists dir && Sys.is_directory dir)
    with Sys_error e -> error e

  let getcwd () =
    try ret (As_path.of_string (Sys.getcwd ())) with Sys_error e -> error e

  let chdir dir =
    try ret (Sys.chdir (As_path.to_string dir)) with Sys_error e -> error e

  let fold_files_rec ?(skip = []) f acc paths =
    let is_dir d = try Sys.is_directory d with Sys_error _ -> false in
    let readdir d = try Array.to_list (Sys.readdir d) with Sys_error _ -> [] in
    let keep p = not (List.exists (fun s -> Filename.check_suffix p s) skip) in
    let process acc file = acc >>= fun acc -> f file acc in
    let rec aux f acc = function
    | (d :: ds) :: up ->
        let paths = List.rev_map (Filename.concat d) (readdir d) in
        let paths = List.find_all keep paths in
        let dirs, files = List.partition is_dir paths in
        begin match List.fold_left process acc files with
        | `Error _ as e -> e
        | `Ok _ as acc -> aux f acc (dirs :: ds :: up)
        end
    | [] :: [] -> acc
    | [] :: up -> aux f acc up
    | _ -> assert false
    in
    let paths = List.find_all keep paths in
    let dirs, files = List.partition is_dir paths in
    let acc = List.fold_left process (`Ok acc) files in
    aux f acc (dirs :: [])
end

(* Commands *)

let trace = ref false
let set_trace b = trace := b
let trace () = !trace


let exists cmd =
  let null = As_path.to_string File.null in
  let test = match Sys.os_type with "Win32" -> "where" | _ -> "type" in
  try ret (Sys.command (str "%s %s 1>%s 2>%s" test cmd null null) = 0)
  with Sys_error e -> error e

(* FIXME

   1. Shouldn't we quote args ? In fact we should take the decision
   whether exec represents more an Unix.execv like call or rather a
   Unix.system call (which would allow e.g. redirects). It seems
   Window's cmd.exe supports that but maybe we should limit it through
   combinators (e.g. input below) and thus always quote the args.

   2. Should we always redirect output on Sys.command. E.g.
      Sys.command "doesntexist" makes spurious outputs. OTOH
      when command fail it can give useful hints.

   3. Should we treat exit code 127 specially, see if it's the
      case on windows. *)

let do_trace cmd =
  if not (trace ()) then () else
  As_log.show "%a @[%a@]" (As_fmt.pp_styled_str `Blue) "[EXEC]"
    As_fmt.pp_text cmd

let mk_cmd cmd args = String.concat " " (cmd :: args)
let execute cmd = do_trace cmd; Sys.command cmd
let exec_ret cmd args = ret (execute (mk_cmd cmd args))
let handle_ret cmd = match execute cmd with
| 0 -> ret ()
| c -> error (str "invocation `%s' exited with code %d" cmd c)

let exec cmd args = handle_ret (mk_cmd cmd args)
let input ?(trim = true) cmd args =
  let cmd = mk_cmd cmd args in
  File.temp "input"
  >>= fun file -> handle_ret (str "%s > %s" cmd (As_path.to_string file))
  >>= fun () -> File.input file
  >>= fun v -> ret (if trim then As_string.trim v else v)

let input_lines cmd args = input cmd args >>| As_string.split ~sep:"\n"

let output cmd args file =
  let cmd = mk_cmd cmd args in
  handle_ret (str "%s > %s" cmd (As_path.to_string file))

(* Working with version control systems *)

module Vcs = struct
  type t = [ `Git | `Hg ]

  let dirtify id = id ^ "-dirty"

  (* VCS detection and executable override *)

  let override = ref None
  let override_exec = ref None
  let set_override v = override := v
  let set_override_exec exec = override_exec := exec
  let override () = !override
  let override_exec () = !override_exec

  (* Git *)

  let git_dir = ".git"
  let git_exists root = match override () with
  | Some `Git -> ret true
  | Some _ | None -> Dir.exists As_path.(root / git_dir)

  let git root args =
    let git = match override_exec () with
    | Some exec when override () = Some `Git -> exec
    | _ -> "git"
    in
    let dir = As_path.(to_string (root / git_dir)) in
    input git ("--git-dir" :: dir :: args)

  let git_head mark_dirty root =
    git root [ "show-ref"; "HEAD"; "--hash" ] >>= fun hash ->
    if not mark_dirty then ret hash else
    git root [ "status"; "--porcelain" ] >>= function
    | "" -> ret hash
    | _ -> ret (dirtify hash)

  let git_describe mark_dirty root =
    let opt = if mark_dirty then [ "--dirty" ] else [] in
    git root ([ "describe"; "--always"; ] @ opt)

  (* Hg *)

  let hg_dir = ".hg"
  let hg_exists root = match override () with
  | Some `Hg -> ret true
  | Some _ | None  -> Dir.exists As_path.(root / hg_dir)

  let hg root args =
    let hg = match override_exec () with
    | Some exec when override () = Some `Hg -> exec
    | _ -> "hg"
    in
    let dir = As_path.to_string root in
    input hg ("--repository" :: dir :: args)

  let hg_id root =
    hg root [ "id"; "-i" ] >>= fun id ->
    let is_dirty = String.length id > 0 && id.[String.length id - 1] = '+' in
    let id = if is_dirty then As_string.slice ~stop:(-1) id else id in
    ret (id, is_dirty)

  let hg_head mark_dirty root =
    hg_id root >>= fun (id, is_dirty) ->
    ret @@ if is_dirty && mark_dirty then dirtify id else id

  let hg_describe mark_dirty root =
    let get_distance s = try ret (int_of_string s) with
    | Failure _ -> error "could not parse hg tag distance"
    in
    let hg_parent template = hg root [ "parent"; "--template"; template ] in
    hg_parent "\"{latesttagdistance}\"" >>= get_distance
    >>= begin function
    | 1 -> hg_parent "\"{latesttag}\""
    | n -> hg_parent "\"{latesttag}-{latesttagdistance}-{node|short}\""
    end
    >>= fun descr ->
    if not mark_dirty then ret descr else
    hg_id root >>= fun (_, is_dirty) ->
    ret @@ if is_dirty then dirtify descr else descr

  (* VCS detection *)

  let exists root = function
  | `Git -> git_exists root
  | `Hg -> hg_exists root

  let find root = match override () with
  | Some override -> ret (Some override)
  | None ->
      git_exists root >>= function
      | true -> ret (Some `Git)
      | false ->
          hg_exists root >>= function
          | true -> ret (Some `Hg)
          | false -> ret None

  let get root =
    find root >>= function
    | Some vcs -> ret vcs
    | None -> error "No VCS found"

  (* VCS commands *)

  let head ?(dirty = true) root = function
  | `Git -> git_head dirty root
  | `Hg -> hg_head dirty root

  let describe ?(dirty = true) root = function
  | `Git -> git_head dirty root
  | `Hg -> hg_head dirty root
end
