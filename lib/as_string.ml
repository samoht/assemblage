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

(* String functions *)

include String

let split ~sep s =
  let sep_max = String.length sep - 1 in
  if sep_max < 0 then invalid_arg "As_string.split: empty separator" else
  let s_max = String.length s - 1 in
  if s_max < 0 then [""] else
  let acc = ref [] in
  let sub_start = ref 0 in
  let k = ref 0 in
  let i = ref 0 in
  while (!i + sep_max <= s_max) do
    if String.unsafe_get s !i <> String.unsafe_get sep 0 then incr i else
    begin
      (* Check remaining [sep] chars match, access to unsafe s (!i + !k) is
         guaranteed by loop invariant. *)
      k := 1;
      while (!k <= sep_max &&
             String.unsafe_get s (!i + !k) = String.unsafe_get sep !k)
      do incr k done;
      if !k <= sep_max then (* no match *) incr i else begin
        let new_sub_start = !i + sep_max + 1 in
        let sub_end = !i - 1 in
        let sub_len = sub_end - !sub_start + 1 in
        acc := String.sub s !sub_start sub_len :: !acc;
        sub_start := new_sub_start;
        i := new_sub_start;
      end
    end
  done;
  List.rev (String.sub s !sub_start (s_max - !sub_start + 1) :: !acc)

let rsplit ~sep s =
  let sep_max = length sep - 1 in
  if sep_max < 0 then invalid_arg "As_string.rsplit: empty separator" else
  let s_max = length s - 1 in
  if s_max < 0 then [""] else
  let acc = ref [] in
  let sub_end = ref s_max in
  let k = ref 0 in
  let i = ref s_max in
  while (!i >= sep_max) do
    if unsafe_get s !i <> unsafe_get sep sep_max then decr i else begin
      (* Check remaining [sep] chars match, access to unsafe_get
           s (sep_start + !k) is guaranteed by loop invariant. *)
      let sep_start = !i - sep_max in
      k := sep_max - 1;
      while (!k >= 0 && unsafe_get s (sep_start + !k) = unsafe_get sep !k)
      do decr k done;
      if !k >= 0 then (* no match *) decr i else begin
        let new_sub_end = sep_start - 1 in
        let sub_start = !i + 1 in
        let sub_len = !sub_end - sub_start + 1 in
        acc := sub s sub_start sub_len :: !acc;
        sub_end := new_sub_end;
        i := new_sub_end;
      end
    end
  done;
  sub s 0 (!sub_end + 1) :: !acc

let cut ~sep s =
  let sep_max = String.length sep - 1 in
  if sep_max < 0 then invalid_arg "As_string.cut: empty separator" else
  let s_max = String.length s - 1 in
  if s_max < 0 then None else
  let k = ref 0 in
  let i = ref 0 in
  try
    while (!i + sep_max <= s_max) do
      (* Check remaining [sep] chars match, access to unsafe s (!i + !k) is
           guaranteed by loop invariant. *)
      if String.unsafe_get s !i <> String.unsafe_get sep 0 then incr i else
      begin
        k := 1;
        while (!k <= sep_max &&
               String.unsafe_get s (!i + !k) = String.unsafe_get sep !k)
        do incr k done;
        if !k <= sep_max then (* no match *) incr i else raise Exit
      end
    done;
    None (* no match in the whole string. *)
  with
  | Exit -> (* i is at the beginning of the separator *)
      let left_end = !i - 1 in
      let right_start = !i + sep_max + 1 in
      Some (String.sub s 0 (left_end + 1),
            String.sub s right_start (s_max - right_start + 1))

let rcut ~sep s =
  let sep_max = String.length sep - 1 in
  if sep_max < 0 then invalid_arg "As_string.rcut: empty separator" else
  let s_max = String.length s - 1 in
  if s_max < 0 then None else
  let k = ref 0 in
  let i = ref s_max in
  try
    while (!i >= sep_max) do
      if String.unsafe_get s !i <> String.unsafe_get sep sep_max
      then decr i
      else begin
        (* Check remaining [sep] chars match, access to String.unsafe_get
             s (sep_start + !k) is guaranteed by loop invariant. *)
        let sep_start = !i - sep_max in
        k := sep_max - 1;
        while (!k >= 0 &&
               String.unsafe_get s (sep_start + !k) = String.unsafe_get sep !k)
        do decr k done;
        if !k >= 0 then (* no match *) decr i else raise Exit
      end
    done;
    None (* no match in the whole string. *)
  with
  | Exit -> (* i is at the end of the separator *)
      let left_end = !i - sep_max - 1 in
      let right_start = !i + 1 in
      Some (String.sub s 0 (left_end + 1),
            String.sub s right_start (s_max - right_start + 1))

let slice ?(start = 0) ?stop s =
  let len = String.length s in
  let clip i = if i < 0 then 0 else if i > len then len else i in
  let start = clip (if start < 0 then len + start else start) in
  let stop = match stop with None -> len | Some stop -> stop in
  let stop = clip (if stop < 0 then len + stop else stop) in
  if start >= stop then "" else
  String.sub s start (stop - start)

let tokens s =
  let acc = ref [] in
  let start = ref 0 in
  let max = String.length s - 1 in
  for i = 0 to max do match s.[i] with
  | ' ' | '\n' | '\r' | '\t' ->
      if !start = i then (incr start) else
      (acc := String.sub s !start (i - !start) :: !acc; start := (i + 1))
  | _ -> ()
  done;
  if !start > max then (List.rev !acc) else
  List.rev (String.sub s !start (max - !start + 1) :: !acc)

(* String sets and maps *)

module Set = struct
  include Set.Make (String)
  let of_list = List.fold_left (fun acc s -> add s acc) empty
end

let make_unique_in ?(suff = "~") set elt =
  let candidate = ref elt in
  try
    for i = 1 to max_int do
      if not (Set.mem !candidate set) then raise Exit else
      candidate := Printf.sprintf "%s%s%d" elt suff i
    done;
    None
  with Exit -> Some !candidate

let uniquify ss =
  let add (seen, ss as acc) v =
    if Set.mem v seen then acc else (Set.add v seen, v :: ss)
  in
  List.rev (snd (List.fold_left add (Set.empty, []) ss))

module Map = struct
  include Map.Make (String)
  let dom m = fold (fun k _ acc -> Set.add k acc) m Set.empty
end
