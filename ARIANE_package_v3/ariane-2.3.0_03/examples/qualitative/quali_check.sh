#!/bin/sh

#test $# = 0 && { echo "name of directory must be provided..." ; exit ;}
#test -d $1 || mkdir $1
NAMEDIR=results
test -d ${NAMEDIR} || mkdir ${NAMEDIR}

grep -hv '^@' initial_positions > initial_positions.txt

/bin/mv initial_positions.txt ${NAMEDIR}
cd ${NAMEDIR}

ln -fs ../namelist_check namelist
ln -fs ../../../src/ariane/ariane ariane

# We assume that the ariane executable is in the PATH environment variable !
# or a link exists.
./ariane

