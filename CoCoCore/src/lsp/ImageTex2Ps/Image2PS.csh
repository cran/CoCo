#!/bin/sh
cat image.tex | sed -e 's/\.0000//g' > Image.tex
latex indpakning.tex
dvips indpakning.dvi -o Image.ps


