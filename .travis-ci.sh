OPAM_DEPENDS="ocamlfind ocamlgraph cmdliner optcomp \
              sexplib comparelib xmlm ezjsonm ctypes" # For the tests

install_on_linux () {
    # Install OCaml and OPAM PPAs
    case "$OCAML_VERSION,$OPAM_VERSION" in
	4.00.1,1.1.0) ppa=avsm/ocaml40+opam11 ;;
	4.01.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
	4.02.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
	*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
    esac

    echo "yes" | sudo add-apt-repository ppa:$ppa
    sudo apt-get update -qq
    sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam time
}

install_on_osx () {
    curl -OL "http://xquartz.macosforge.org/downloads/SL/XQuartz-2.7.6.dmg"
    sudo hdiutil attach XQuartz-2.7.6.dmg
    sudo installer -verbose -pkg /Volumes/XQuartz-2.7.6/XQuartz.pkg -target /
    brew install opam
}

case $TRAVIS_OS_NAME in
    osx)   install_on_osx ;;
    linux) install_on_linux ;;
    *)     echo Unknown $TRAVIS_OS_NAME; exit 1
esac

echo OCaml version
ocaml -version

export OPAMYES=1
export OPAMVERBOSE=1

opam init git://github.com/ocaml/opam-repository >/dev/null 2>&1

case $TRAVIS_OS_NAME in
    osx)   brew install libffi             ;;
    linux) sudo apt-get install libffi-dev ;;
    *)     echo Unknown $TRAVIS_OS_NAME; exit 1
esac

case "$TRAVIS_OS_NAME,$OCAML_VERSION" in
    osx,4.00.1) opam switch 4.00.1 ;;
    *,4.02.0)   opam switch 4.02.0+trunk ;;
esac

opam install ${OPAM_DEPENDS}

eval `opam config env`
./bootstrap.sh
make
make test
make install
make distclean
