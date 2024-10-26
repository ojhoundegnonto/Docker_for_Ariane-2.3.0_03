#!/bin/sh

test $# = 0 && { echo "name of directory must be provided..." ; exit ;}
test -d $1 || mkdir $1

grep -hv '^@' initial_positions > initial_positions.txt

/bin/mv initial_positions.txt $1
cd $1

ln -fs ../namelist namelist

# We assume that the ariane executable is in the PATH environment variable !
# or a link exists.
type ariane
if (($? != 0))
then
    echo "!!! ariane is not present in you PATH !!!"
    echo "Please update your PATH environment variable:"
    echo ""
    echo '        setenv PATH /your_dir/bin:${PATH}    (csh)'
    echo '        export PATH=/your_dir/bin:${PATH}    (ksh)'
    echo ""
    echo "- STOP -"
    exit
fi

# Let's go
ariane

