#!/bin/sh

set -ex

OCAMLFIND=${OCAMLFIND:="ocamlfind"}

BDIR="_build/bootstrap"
PKGS="-package bytes -package rresult -package astring -package fmt \
      -package bos -package cmdliner"
CMOS=""

# Make sure $BDIR is clean
rm -rf $BDIR
mkdir -p $BDIR

# FIXME if get rid of attic/ this can be removed
major=`ocamlc -version | cut -d. -f 1`
xminor=`ocamlc -version | cut -d. -f 2`
if [ $major -ge 4 ] && [ $xminor -le 01 ]; then minor=01; else minor=$xminor; fi

build_srcs ()
{
    SDIR=$1
    SRCS=`ocamldep -sort $SDIR/*`
    for SRC in $SRCS; do
        UNIT=`basename $SRC | cut -d. -f1`
        OPTS="-bin-annot"
        case $UNIT in
            "asd_ocaml_incl") # FIXME if get rid of attic/ can be removed
                SRC=$SDIR/$major$minor/$u;
                UPKGS="$PKGS,compiler-libs.bytecomp";;
            "assemblage_driver")
                UPKGS="$PKGS,compiler-libs.bytecomp";;
            *)
                UPKGS="$PKGS";;
        esac

        case $SRC in
            *.mli)
                CMI="$BDIR/$UNIT.cmi"
                $OCAMLFIND ocamlc -g -c -I $BDIR $UPKGS $OPTS -o $CMI $SRC ;;
            *.ml)
                CMO="$BDIR/$UNIT.cmo"
                $OCAMLFIND ocamlc -g -c -I $BDIR $UPKGS $OPTS -o $CMO $SRC
                CMOS="$CMOS $CMO" ;;
        esac
    done
}

# Build everything in $BDIR
build_srcs "lib"
build_srcs "lib-driver"
build_srcs "driver"

# Build the assemblage command line tool
UPKGS="$PKGS,compiler-libs.toplevel"
OPTS="-bin-annot"
$OCAMLFIND ocamlc $OPTS $UPKGS -I $BDIR -g -linkpkg $CMOS \
           -o $BDIR/assemblage.boot

# Run it on assemblage's assemblage.ml
$BDIR/assemblage.boot setup --auto-lib=false -I $BDIR --merlin=false
