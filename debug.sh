#!/bin/bash


dune build;
ocamldebug _build/default/bin/main.exe -r -i "db.go" "textFile" "csvFile";