#!/bin/bash

# Generate all images for the post
#
# THIS REQUIRES THE DOT EXECUTABLE
#
# see http://www.graphviz.org/ for
# more information

for fgv in *.gv
do
    dot -Tpng $fgv -o ${fgv%gv}png
done
