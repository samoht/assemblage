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

module Cli = struct

  let value_converter_of_converter (parse, _) =
    let parse s = match parse s with
    | `Ok v -> `Ok (Conf.const v) | `Error _ as e -> e
    in
    let print = Fmt.nop (* config needed be accurate, use describe cmd *) in
    parse, print

  let term_of_conf c =             (* term constructed in dependency order. *)
    let rec loop acc names seen = function
    | ((Conf.Key.V kt) :: _ as todo) :: more_todo ->
        if not (Conf.Key.public kt)
        then loop acc names seen (List.tl todo :: more_todo)
        else
        let deps = Conf.deps (Conf.Key.default kt) in
        loop acc names seen ((Conf.Key.Set.elements deps) :: todo :: more_todo)
    | [] :: ((Conf.Key.V kt as k) :: todo) :: more_todo  ->
        if Conf.Key.Set.mem k seen
        then loop acc names seen (todo :: more_todo) else
        if As_string.Set.mem (Conf.Key.name kt) names
        then (Log.warn "%a" (Conf.pp_key_dup c) k;
              loop acc names seen (todo :: more_todo))
        else
        let v = Conf.Key.default kt in
        let c = value_converter_of_converter (Conf.Key.converter kt) in
        let doc = Conf.Key.doc kt in
        let docs = Conf.Key.docs kt in
        let docv = Conf.Key.docv kt in
        let i = Cmdliner.Arg.info [Conf.Key.name kt] ?doc ?docv ?docs in
        let opt = Cmdliner.Arg.(value (opt c v & i)) in
        let acc' = Cmdliner.Term.(pure Conf.set $ acc $ pure kt $ opt) in
        let names' = As_string.Set.add (Conf.Key.name kt) names in
        let seen' = Conf.Key.Set.add k seen in
        loop acc' names' seen' (todo :: more_todo)
    | [] :: [] :: todo -> loop acc names seen todo
    | [] :: [] -> acc
    | [] -> assert false
    in
    loop (Cmdliner.Term.pure c) As_string.Set.empty
      Conf.Key.Set.empty (Conf.Key.Set.elements (Conf.domain c) :: [])
end
