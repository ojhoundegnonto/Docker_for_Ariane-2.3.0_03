#!/bin/ksh

SRC_NAME=Iroise15KM_avg_M

typeset -Z2 nb

echo ""
((nb = 1))
((nbo = 1))

for count in ${SRC_NAME}*.nc
do
    filename=${SRC_NAME}${nbo}.nc
    echo "${nb} - ${SRC_NAME}_${nb}.nc -> ${filename}"
    ln -s ${filename}  ${SRC_NAME}_${nb}.nc
    ((nb=nb+1))
    ((nbo=nbo+1))
done
