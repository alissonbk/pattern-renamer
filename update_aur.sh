#!/bin/bash

DEST=~/projects/aur/pattern-renamer/
makepkg --printsrcinfo > .SRCINFO
cp PKGBUILD $DEST
cp .SRCINFO $DEST
cp LICENSE $DEST
cp README.md $DEST
