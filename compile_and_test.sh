#!/bin/bash

mkdir -p docs

function conf_doc_build_install()
{
    echo "===[ Build & Install $1 ]==="


# Cannot do haddock with Buildable: False
# therefore configure without examples or test flag
    cd $1
    cabal clean

    cabal configure
    cabal haddock

    cabal configure -fexamples
    cabal build
    cabal install
    cp -r dist/doc/html/* ../docs/

    cd ..
    echo "===[ Done $1 ]==="
}

conf_doc_build_install craftwerk
conf_doc_build_install craftwerk-gloss
conf_doc_build_install craftwerk-cairo
conf_doc_build_install craftwerk-gtk

