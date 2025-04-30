#!/bin/bash

sh compile.sh;
sudo cp main /usr/bin/pattern-renamer;
rm main main.static;