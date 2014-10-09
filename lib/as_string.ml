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

(* String sets *)

module Set = struct
  include Set.Make (String)
  let of_list = List.fold_left (fun acc s -> add s acc) empty
end
