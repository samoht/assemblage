#!/bin/sh

set -ex

ocamlbuild lib/git.cmo lib/env.cmo lib/project.cmo \
    lib/ocamlfind.cmo lib/opam.cmo lib/makefile.cmo \
    lib/tools.cmo -package cmdliner -package opam

ocamlmktop \
    $(ocamlfind query -r unix cmdliner opam -predicates byte -format "-I %d %a")  \
    -I _build/lib git.cmo env.cmo project.cmo ocamlfind.cmo opam.cmo makefile.cmo \
    tools.cmo -o configure.top

./configure.top -I _build/lib configure.ml
