#!/bin/sh

set -ex

OCAMLFIND=${OCAMLFIND:="ocamlfind"}

BDIR="_build/bootstrap"
LIBDIR="lib"
PKGS="-package cmdliner"

LIB="as_string as_path as_fmt as_log as_cmd as_conf \
     as_context as_args as_product as_env as_rule as_part \
     as_project assemblage"

LIB_CMDLINER="assemblage_cmdliner"

DRIVER="asd_cstubs asd_merlin asd_ocaml_incl asd_ocaml \
        asd_pkg_config \
        asd_opam asd_ocamlfind asd_makefile asd_project_makefile \
        assemblage_env asd_setup asd_describe driver_cmd main"

CMOS=""

# Make sure $BDIR is clean
rm -rf $BDIR
mkdir -p $BDIR

major=`ocamlc -version | cut -d. -f 1`
xminor=`ocamlc -version | cut -d. -f 2`
if [ $major -ge 4 ] && [ $xminor -le 01 ]; then minor=01; else minor=$xminor; fi

build_units ()
{
    UNITS_DIR=$1
    UNITS=$2
    for u in $UNITS; do
        case $u in
            "asd_ocaml_incl")
                f=$major$minor/$u;
                UPKGS="$PKGS,compiler-libs.bytecomp";
                OPTS="" ;;
            "asd_ocaml"|"main")
                f="$u";
                UPKGS="$PKGS,compiler-libs.bytecomp";
                OPTS="" ;;
            *)
                f="$u";
                UPKGS="$PKGS";
                OPTS="" ;;
        esac

        CMI="$BDIR/$u.cmi"
        CMO="$BDIR/$u.cmo"

        if [ -f $UNITS_DIR/$f.mli ]; then
            $OCAMLFIND ocamlc -g -c -I $BDIR $UPKGS $OPTS -o $CMI \
                              $UNITS_DIR/$f.mli;
        fi

        if [ -f $UNITS_DIR/$f.ml ]; then
            $OCAMLFIND ocamlc -g -c -I $BDIR $UPKGS $OPTS -o $CMO \
                              $UNITS_DIR/$f.ml;
            CMOS="$CMOS $CMO"
        fi
    done
}

# Build everything in $BDIR
build_units "lib" "$LIB"
build_units "libs-driver" "$LIB_CMDLINER"
build_units "driver" "$DRIVER"

# Build the assemblage command line tool
UPKGS="$PKGS,compiler-libs.toplevel"
OPTS=""
$OCAMLFIND ocamlc $OPTS $UPKGS -I $BDIR -g -linkpkg $CMOS \
           -o $BDIR/assemblage.boot

# Run it on assemblage's assemblage.ml

$BDIR/assemblage.boot setup --auto-load=false -I $BDIR \
                      --test=false --merlin=false
