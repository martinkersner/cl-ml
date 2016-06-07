#!/usr/bin/env bash

# Martin Kersner, m.kersner@gmail.com
# 2016/06/05

# Plot two-dimensional data (with 2 classes) and and their decision boundary.

# Data should be in column format where the first two columns represent
# coordinates and the third column determines label of data.

cat << EOF | gnuplot -p
set arrow from $2,$3 to $4,$5 nohead
plot "$1" using 1:2:3 with points pt 7 ps 2 palette
EOF
