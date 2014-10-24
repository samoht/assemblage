#!/bin/sh

# usage: ./bootstrap.sh [-b]

set -e

OCAMLFIND=${OCAMLFIND:="ocamlfind"}
PKGS="-package cmdliner,compiler-libs.bytecomp"

BDIR=_build/bootstrap-doc

# Make sure $BDIR is clean
rm -rf $BDIR
mkdir -p $BDIR

# Gather .mli and compile their cmis.
for f in `ls lib/*.mli driver-make/*.mli`; do
    ln -s "../../$f" $BDIR/`basename $f`
done

MLIS=`ocamldep -sort $BDIR/*.mli`

for f in $MLIS; do
    $OCAMLFIND ocamlc $PKGS -I $BDIR $f
done

# Generate doc
MLIS=`ls $BDIR/*.mli | sort -f`
$OCAMLFIND ocamldoc $PKGS -I $BDIR -d $BDIR -html -colorize-code \
 -charset utf-8 $MLIS
cp doc/style.css $BDIR

if [ "$1" = "-b" ]; then
    reload-browser file://$PWD/$BDIR/
fi
