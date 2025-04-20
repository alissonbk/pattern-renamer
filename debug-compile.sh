#!/bin/zsh

# compile
pushd lib;
ocamlfind ocamlc -c types.ml;
ocamlfind ocamlc -c utils.ml;
ocamlfind ocamlc -c transform.ml;
ocamlfind ocamlc -c file.ml;
ocamlfind ocamlc -c core.mli;
ocamlfind ocamlc -package cmdliner -linkpkg core.ml -c core.ml;
ocamlfind ocamlc -package cmdliner -linkpkg command.ml -c command.ml;
ocamlfind ocamlc -c renamer.ml;

ocamlfind ocamlc -package cmdliner -a types.cmo utils.cmo transform.cmo file.cmo core.cmo command.cmo renamer.cmo -o renamer.cma;
ocamlfind ocamlc -g -package cmdliner,unix -linkpkg renamer.cma ../bin/main.ml -o ../main.debug;

rm *.cm*;
popd;

# run the debugger
ocamldebug main.debug -r -i "db.go" "textFile" "csvFile";

rm main.debug;