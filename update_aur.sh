#!/bin/bash

DEST=~/projects/aur/pattern-renamer/
sh bump-cmdliner-version.sh
makepkg --printsrcinfo > .SRCINFO
cp PKGBUILD $DEST
cp .SRCINFO $DEST
cp LICENSE $DEST
cp README.md $DEST
