open Sexplib.Std

type t = {
  foo: int;
} with sexp, compare

type y =
  | Foo: int -> y
with sexp

let x = Xmlm.input
