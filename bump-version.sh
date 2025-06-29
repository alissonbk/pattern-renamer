#!/bin/bash

perl -i -pe '
  s/(~version:"v\d+\.\d+\.)(\d+)"/$1 . ($2 + 1) . "\""/e
' lib/command.ml;

perl -i -pe '
  s/(pkgver=\d+\.\d+\.)(\d+)/$1 . ($2 + 1) . ""/e
' PKGBUILD