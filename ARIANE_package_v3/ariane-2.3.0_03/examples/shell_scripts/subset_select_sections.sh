#!/bin/bash
####################
## N.G. June 2007 ##
###############################################################
# We assume that nco commands are available on your computer ##
###############################################################
## This script extracts only particules which     ##
## have reach a selected section and stores their ##
## indices in a subset file using by ariane for   ##
## a futur quantitative simulation.               ##
####################################################
# Usage:
#  subset_select_sections.sh [section_number ...]
#
# Example:
#  subset_select_sections.sh 1 3 6
###################################################

## Test
## number of arguments
##echo "Number of arguments = $#"
##nc=1
##for ii in $*
##do
##  echo " arg ${nc} = ${ii}"
##  ((nc=nc+1))
##done
##
##exit

if [[ -f final_section.txt ]]
then
  mv final_section.txt final_section.txt_old
fi

if [[ -f subset.txt ]]
then
  mv subset.txt subset.txt_old
fi

#-V or --var_val = Print variable values only.
#     Do not print variable and dimension names, indices,
#     and coordinate values when printing arrays. 
ncks --trd -V -v final_section -C -H ariane_initial.nc > final_section.txt

# Remove empty lines
sed -i '/^$/d' final_section.txt

# Add a number in front of each lines
cat -n final_section.txt >  dummy.txt
rm final_section.txt
mv dummy.txt final_section.txt

# to select only section 2
# cat final_section.txt | grep '2$'

if [[ $# == 0 ]]
then
  sed -e /-1/d final_section.txt > subset.txt
else
  nc=1
  for ii in $*
  do
    echo " arg ${nc} = ${ii}"
    cat final_section.txt | grep "${ii}$" >> subset0.txt
    ((nc=nc+1))
  done
fi

cat subset0.txt | sort -n > subset.txt
rm subset0.txt

size=`wc -l subset.txt`
size=${size% *}
echo "${size} particules for the subset..."
