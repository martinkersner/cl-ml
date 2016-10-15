#!/usr/bin/env bash

# Martin Kersner, m.kersner@gmail.com
# 2016/10/15

# Plot two-dimensional data and and their linear regresion line. 

# Data should be in column format where the first two columns represent
# coordinates and the third column determines label of data.

cat << EOF | gnuplot -p
set arrow from $2,$3 to $4,$5 nohead
plot "$1" using 1:2 with points pt 7 ps 2
EOF
