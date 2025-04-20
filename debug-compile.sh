#!/bin/zsh

pushd lib;
ocamlfind ocamlc -c types.ml;
ocamlfind ocamlc -c utils.ml;
ocamlfind ocamlc -c transform.ml;
ocamlfind ocamlc -c file.ml;
ocamlfind ocamlc -c core.ml;
ocamlfind ocamlc -c command.ml;
ocamlfind ocamlc -c renamer.ml;
popd lib;

ocamlfind ocamlc -package cmdliner -a lib/types.cmo lib/utils.cmo lib/core.cmo lib/command.cmo lib/renamer.cmo -o lib/renamer.cma
ocamlfind ocamlc -package cmdliner -linkpkg lib/renamer.cma bin/main.ml -o main.debug
