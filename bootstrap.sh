#!/bin/sh

set -ex

OCAMLFIND=${OCAMLFIND:="ocamlfind"}

BDIR="_build/bootstrap"
LIBDIR="lib"
PKGS="-package cmdliner"

UNITS="as_shell as_git as_makefile as_features as_flags as_resolver as_action
       as_build_env as_component as_project as_ocamlfind as_project_makefile
       as_OCaml_incl as_OCaml as_opam as_merlin as_env as_tool as_cmd assemblage"

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
    "as_OCaml") f="$u"; UPKGS="$PKGS,compiler-libs.bytecomp"; OPTS="$OPTS" ;;
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

# Run it on assemblage's assemblage.ml
$BDIR/assemblage.boot setup --auto-load=false -I $BDIR \
    --warn-error=false --test=false --dumpast=false --merlin=false
