#!/usr/bin/env bash

# Martin Kersner, m.kersner@gmail.com
# 2016/09/29

# Plot two-dimensional data.

# Data should be in column format where the first two columns represent
# coordinates and the third column determines label of data.

CLASSIFIED=$(mktemp /tmp/lisp-ml-dataset.XXXXXX)
exec 3>"$CLASSIFIED"
rm "$CLASSIFIED"

echo "$2 $3 $4" > "$CLASSIFIED"

cat << EOF | gnuplot -p
set nokey
plot "$1" using 1:2:3 with points pt 7 ps 2 palette
replot "$CLASSIFIED" using 1:2:3 with circles palette fill solid border lt 2
EOF
