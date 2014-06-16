#!/bin/sh

set -ex

ocamlbuild lib/project.cmo lib/ocamlfind.cmo lib/makefile.cmo -package cmdliner
ocamlmktop -I $(opam config var cmdliner:lib) cmdliner.cma -I _build/lib project.cmo ocamlfind.cmo makefile.cmo -o configure.top
./configure.top -I _build/lib configure.ml
