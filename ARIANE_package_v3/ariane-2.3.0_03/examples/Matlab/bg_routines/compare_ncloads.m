clear all; close all;
ncFileName='roms_avg_Y21M1.nc';

tic
ncload(ncFileName);
toc
whos

tic
incload(ncFileName);
toc
whos

tic
ncload_st(ncFileName);
toc
whos

tic
incload_st(ncFileName);
toc
whos