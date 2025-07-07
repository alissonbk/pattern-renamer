#!/bin/bash

DEST=~/projects/aur/pattern-renamer/

while true; do
    read -rp "Bump version? [y/n]: " response
    case "$response" in
        [Yy]) 
            sh bump-version.sh
            break
            ;;
        [Nn])             
            break
            ;;
        *) 
            echo "Please enter y or n."
            ;;
    esac
done
makepkg --printsrcinfo > .SRCINFO
cp PKGBUILD $DEST
cp .SRCINFO $DEST
cp LICENSE $DEST
cp README.md $DEST
