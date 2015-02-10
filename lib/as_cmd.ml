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

(* Command and IO operation results *)

let err_error msg = str "result value is (`Error %S)" msg

type 'a result = [ `Ok of 'a | `Error of string ]

let ret v = `Ok v
let error e = `Error e
let bind v f = match v with `Ok v -> f v | `Error _ as e -> e
let map v f = match v with `Ok v -> `Ok (f v) | `Error _ as e -> e
let get = function `Ok v -> v | `Error msg -> invalid_arg (err_error msg)
let ignore_error ~use r = match r with `Ok v -> v | `Error _ -> use
let on_error ?(level = As_log.Error) ~use r = match r with
| `Ok v -> v
| `Error msg -> As_log.kmsg (fun () -> use) level "@[%a@]" As_fmt.pp_text msg

let reword_error ?(replace = false) msg r = match r with
| `Ok _ as r -> r
| `Error _ when replace -> `Error msg
| `Error old -> `Error (str "%s\n%s" msg old)

let exn_msg bt _ _ = Printexc.raw_backtrace_to_string bt
let exn_error ?(msg = exn_msg) f v = try `Ok (f v) with
| e ->
    let bt = Printexc.get_raw_backtrace () in
    `Error (msg bt e v)

let ( >>= ) = bind
let ( >>| ) = map

module Infix = struct
  let ( >>= ) = ( >>= )
  let ( >>| ) = ( >>| )
end

(* Path operations *)

type path = As_path.t (* to avoid assemblage.mli confusion *)

let path_str = As_path.to_string

module Path = struct
  let exists p = try ret (Sys.file_exists (path_str p))
  with Sys_error e -> error e

  let err_move src dst =
    let src, dst = (path_str src), (path_str dst) in
    error (str "move %s to %s: destination exists" src dst)

  let move ?(force = false) src dst =
    (if force then ret false else exists dst) >>= fun don't ->
    if don't then err_move src dst else
    try ret (Sys.rename (path_str src) (path_str dst)) with
    | Sys_error e -> error e
end

(* File operations *)

module File = struct

  let apply f x ~finally y =
    let result = try f x with
    | e -> try finally y; raise e with _ -> raise e
    in
    finally y;
    result

  let is_dash = As_path.is_dash

  (* Files *)

  let dev_null = match Sys.os_type with
  (* Using Sys.os_type, because that's really for the driver. *)
  | "Win32" -> As_path.file "NUL"
  |  _ -> As_path.(root /"dev"/"null")

  let exists file =
    let file = path_str file in
    try ret (Sys.file_exists file && not (Sys.is_directory file)) with
    | Sys_error e -> error e

  let delete ?(maybe = false) file =
    exists file >>= fun exists ->
    if maybe && not exists then ret () else
    try ret (Sys.remove (path_str file)) with Sys_error e -> error e

  let temp ?dir suff =
    try
      let temp_dir = match dir with
      | None -> None
      | Some d -> Some (As_path.to_string d)
      in
      let f = Filename.temp_file ?temp_dir "assemblage" suff in
      let f = As_path.of_string f in
      at_exit (fun () -> ignore (delete f));
      ret f
    with Sys_error e -> error e

  (* Input *)

  let with_inf f file v =
    try
      let ic = if is_dash file then stdin else open_in_bin (path_str file) in
      let close ic = if is_dash file then () else close_in ic in
      apply (f ic) v ~finally:close ic
    with
    | Sys_error e -> error e

  let read file =
    let input ic () =
      let len = in_channel_length ic in
      let s = Bytes.create len in
      really_input ic s 0 len; ret s
    in
    with_inf input file ()

  let read_lines file = read file >>| (As_string.split ~sep:"\n")

  (* Output *)

  let with_outf f file v =
    try
      let oc = if is_dash file then stdout else open_out_bin (path_str file) in
      let close oc = if is_dash file then () else close_out oc in
      apply (f oc) v ~finally:close oc
    with
    | Sys_error e -> error e

  let write file contents =
    let write oc contents = output_string oc contents; ret () in
    if is_dash file then with_outf write file contents else
    temp ~dir:(As_path.dirname file) "write"
    >>= fun tmpf -> with_outf write tmpf contents
    >>= fun () -> Path.move ~force:true tmpf file

  let write_lines file lines = write file (String.concat "\n" lines)

  let write_subst vars file contents =
    let write_subst oc contents =                     (* man that's ugly. *)
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
    if is_dash file then with_outf write_subst file contents else
    temp ~dir:(As_path.dirname file) "write"
    >>= fun tmpf -> with_outf write_subst tmpf contents
    >>= fun () -> Path.move ~force:true tmpf file
end

(* Directory operations *)

module Dir = struct
  let exists dir =
    let dir = path_str dir in
    try ret (Sys.file_exists dir && Sys.is_directory dir)
    with Sys_error e -> error e

  let getcwd () =
    try ret (As_path.of_string (Sys.getcwd ())) with Sys_error e -> error e

  let chdir dir =
    try ret (Sys.chdir (path_str dir)) with Sys_error e -> error e

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

(* Environment variables lookup *)

let env var = try Some (Sys.getenv var) with Not_found -> None
let get_env var = try `Ok (Sys.getenv var) with
| Not_found -> `Error (str "environment variable `%s' undefined" var)

(* Commands *)

(* FIXME in these functions should [cmd] and [args] be quoted ? *)

let exists cmd =
  let null = path_str File.dev_null in
  (* Using Sys.os_type, because that's really for the driver. *)
  let test = match Sys.os_type with "Win32" -> "where" | _ -> "type" in
  try ret (Sys.command (str "%s %s 1>%s 2>%s" test cmd null null) = 0)
  with Sys_error e -> error e

let trace cmd = As_log.info ~header:"EXEC" "@[<2>%a@]" As_fmt.pp_text cmd
let mk_cmd cmd args = String.concat " " (cmd :: args)

let execute cmd = trace cmd; Sys.command cmd
let exec_ret cmd args = execute (mk_cmd cmd args)
let handle_ret cmd = match execute cmd with
| 0 -> ret ()
| c -> error (str "Exited with code: %d `%s'" c cmd)

let exec cmd args = handle_ret (mk_cmd cmd args)
let read ?(trim = true) cmd args =
  let cmd = mk_cmd cmd args in
  File.temp "cmd-read"
  >>= fun file -> handle_ret (str "%s > %s" cmd (path_str file))
  >>= fun () -> File.read file
  >>= fun v -> ret (if trim then As_string.trim v else v)

let read_lines cmd args = read cmd args >>| As_string.split ~sep:"\n"

let write cmd args file =
  let cmd = mk_cmd cmd args in
  File.temp "cmd-write"
  >>= fun tmpf -> handle_ret (str "%s > %s" cmd (path_str tmpf))
  >>= fun () -> Path.move ~force:true tmpf file

(* Version control systems operations *)

module Vcs = struct
  type t = [ `Git | `Hg ]

  let dirtify id = id ^ "-dirty"

  (* VCS detection and executable override *)

  let override_kind = ref None
  let override_exec = ref None
  let set_override_kind v = override_kind := v
  let set_override_exec exec = override_exec := exec
  let override_kind () = !override_kind
  let override_exec () = !override_exec

  (* Git *)

  let git_dir = ".git"
  let git_exists root_dir = match override_kind () with
  | Some `Git -> ret true
  | Some _ | None -> Dir.exists As_path.(root_dir / git_dir)

  let git root_dir args =
    let git = match override_exec () with
    | Some exec when override_kind () = Some `Git -> exec
    | _ -> "git"
    in
    let dir = As_path.(to_string (root_dir / git_dir)) in
    read git ("--git-dir" :: dir :: args)

  let git_head mark_dirty root_dir =
    git root_dir [ "show-ref"; "HEAD"; "--hash" ] >>= fun hash ->
    if not mark_dirty then ret hash else
    git root_dir [ "status"; "--porcelain" ] >>= function
    | "" -> ret hash
    | _ -> ret (dirtify hash)

  let git_describe mark_dirty root_dir =
    let opt = if mark_dirty then [ "--dirty" ] else [] in
    git root_dir ([ "describe"; "--always"; ] @ opt)

  (* Hg *)

  let hg_dir = ".hg"
  let hg_exists root_dir = match override_kind () with
  | Some `Hg -> ret true
  | Some _ | None  -> Dir.exists As_path.(root_dir / hg_dir)

  let hg root_dir args =
    let hg = match override_exec () with
    | Some exec when override_kind () = Some `Hg -> exec
    | _ -> "hg"
    in
    let dir = path_str root_dir in
    read hg ("--repository" :: dir :: args)

  let hg_id root_dir =
    hg root_dir [ "id"; "-i" ] >>= fun id ->
    let is_dirty = String.length id > 0 && id.[String.length id - 1] = '+' in
    let id = if is_dirty then As_string.slice ~stop:(-1) id else id in
    ret (id, is_dirty)

  let hg_head mark_dirty root_dir =
    hg_id root_dir >>= fun (id, is_dirty) ->
    ret @@ if is_dirty && mark_dirty then dirtify id else id

  let hg_describe mark_dirty root_dir =
    let get_distance s = try ret (int_of_string s) with
    | Failure _ -> error "could not parse hg tag distance"
    in
    let hg_parent template = hg root_dir [ "parent"; "--template"; template ] in
    hg_parent "\"{latesttagdistance}\"" >>= get_distance
    >>= begin function
    | 1 -> hg_parent "\"{latesttag}\""
    | n -> hg_parent "\"{latesttag}-{latesttagdistance}-{node|short}\""
    end
    >>= fun descr ->
    if not mark_dirty then ret descr else
    hg_id root_dir >>= fun (_, is_dirty) ->
    ret @@ if is_dirty then dirtify descr else descr

  (* VCS detection *)

  let exists root_dir = function
  | `Git -> git_exists root_dir
  | `Hg -> hg_exists root_dir

  let find root_dir = match override_kind () with
  | Some kind -> ret (Some kind)
  | None ->
      git_exists root_dir >>= function
      | true -> ret (Some `Git)
      | false ->
          hg_exists root_dir >>= function
          | true -> ret (Some `Hg)
          | false -> ret None

  let get root_dir =
    find root_dir >>= function
    | Some vcs -> ret vcs
    | None -> error "No VCS found"

  (* VCS commands *)

  let head ?(dirty = true) root_dir = function
  | `Git -> git_head dirty root_dir
  | `Hg -> hg_head dirty root_dir

  let describe ?(dirty = true) root_dir = function
  | `Git -> git_describe dirty root_dir
  | `Hg -> hg_describe dirty root_dir
end
