#!/bin/sh
cat $1 | sed -e 's/\.0000//g' > Image.tex
latex indpakning.tex
dvips indpakning.dvi -o $2


