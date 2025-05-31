#!/bin/bash


BIN_NAME="pattern-renamer"
# compile
pushd lib;
ocamlfind ocamlopt -c types.ml
ocamlfind ocamlopt -c utils.ml
ocamlfind ocamlopt -c global.ml
ocamlfind ocamlopt -package spectrum -linkpkg -c log.ml
ocamlfind ocamlopt -c transform.ml
ocamlfind ocamlopt -package str -linkpkg -c file.ml
ocamlfind ocamlopt -c core.mli
ocamlfind ocamlopt -package cmdliner -linkpkg -c core.ml
ocamlfind ocamlopt -package cmdliner -linkpkg -c command.ml
ocamlfind ocamlopt -c renamer.ml

# /lib
ocamlfind ocamlopt -package cmdliner,spectrum,str -a types.cmx utils.cmx global.cmx log.cmx transform.cmx file.cmx core.cmx command.cmx renamer.cmx -o renamer.cmxa

# dynamic linked build
ocamlfind ocamlopt -g -package cmdliner,unix,spectrum,str -linkpkg renamer.cmxa ../bin/main.ml -o ../$BIN_NAME
# static linked build
ocamlfind ocamlopt -g \
    -package cmdliner,unix,spectrum,str -linkpkg \
    -ccopt "-static -L$HOME/.opam/default/lib/pcre -l:pcre.a" \
    renamer.cmxa ../bin/main.ml -o ../$BIN_NAME.static

rm *.o *.cm* *.a;
popd;
rm bin/*.cmi bin/*.cmx bin/*.o;