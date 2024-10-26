#!/bin/ksh

SRC_NAME=ORCA025-G70

typeset -Z2 nb

for grid in gridT gridU gridV gridW
do
  echo ""
  ((nb = 1))

  for filename in *${grid}.nc
  do

    echo "${nb} - ${SRC_NAME}_${nb}_${grid}.nc -> ${filename}"
    ln -s ${filename}  ${SRC_NAME}_${nb}_${grid}.nc
    ((nb=nb+1))
  done
done
