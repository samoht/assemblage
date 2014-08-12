#!/bin/sh

set -ex

OCAMLFIND=${OCAMLFIND:="ocamlfind"}

BDIR="_build/bootstrap"
LIBDIR="lib"
PKGS="-package cmdliner,ocamlgraph"
OPTCOMP="-syntax camlp4o -package camlp4,optcomp"

UNITS="as_shell as_git as_features as_flags as_resolver as_action
       as_build_env as_project as_ocamlfind as_OCaml_incl as_OCaml
       as_opam as_makefile as_env as_tool as_cmd assemblage"

CMOS=""

# Make sure $BDIR is clean
rm -rf $BDIR
mkdir -p $BDIR

major=`ocamlc -version | cut -d. -f 1`
xminor=`ocamlc -version | cut -d. -f 2`
if [ $major -ge 4 ] && [ $xminor -le 01 ]; then minor=01; else minor=$xminor; fi

# Build the assemblage's library compilation units in $BDIR
for u in $UNITS; do
    case $u in
    "as_OCaml_incl") f=$major$minor/$u; UPKGS="$PKGS,compiler-libs.bytecomp"; OPTS="" ;;
    "as_OCaml") f="$u"; UPKGS="$PKGS,compiler-libs.bytecomp"; OPTS="$OPTS $OPTCOMP" ;;
    *)          f="$u"; UPKGS="$PKGS"; OPTS="" ;;
    esac

    CMI="$BDIR/$u.cmi"
    CMO="$BDIR/$u.cmo"
    CMOS="$CMOS $CMO"

    if [ -f $LIBDIR/$f.mli ]; then $OCAMLFIND ocamlc -c -I $BDIR $UPKGS $OPTS -o $CMI $LIBDIR/$f.mli; fi
    $OCAMLFIND ocamlc -c -I $BDIR $UPKGS $OPTS -o $CMO $LIBDIR/$f.ml
done

# Build the assemblage command line tool
UPKGS="$PKGS,compiler-libs.toplevel"
OPTS=""
$OCAMLFIND ocamlc $OPTS $UPKGS -I $BDIR -c -o $BDIR/tool.cmo bin/tool.ml
$OCAMLFIND ocamlc $OPTS $UPKGS -I $BDIR -linkpkg $CMOS $BDIR/tool.cmo \
    -o $BDIR/assemblage.boot

$BDIR/assemblage.boot setup --disable-auto-load -I $BDIR \
    --enable-warn-error --disable-test --disable-dumpast
