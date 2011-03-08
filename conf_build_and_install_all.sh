#!/bin/bash

mkdir -p docs


function conf_doc_build_install()
{

    echo -e "\e[0;32m===[ Build & Install $1 ]===\e[0m"

    cd $1
    cabal clean

# Cannot do haddock with Buildable: False
# therefore configure without examples or test flag
    cabal configure
    cabal haddock

    cabal configure -fexamples
    cabal build
    cabal install
    cp -r dist/doc/html/* ../docs/

    if [ "$2" ]
    then
	mkdir -p ../bin/$1
	for prg in "${@}"; do
	    cp dist/build/${prg}/${prg} ../bin/$1/ &> /dev/null
	done
    fi

    cd ..
    echo -e "\e[0;32m===[ Done $1 ]===\e[0m"
}

conf_doc_build_install craftwerk example1

conf_doc_build_install craftwerk-gloss

#conf_doc_build_install craftwerk-cairo

conf_doc_build_install craftwerk-gtk example1

