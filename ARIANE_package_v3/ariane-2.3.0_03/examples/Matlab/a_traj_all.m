%%%%%%%%%%%%%%%%%%%%%%
%% Qualitative mode %%
%%%%%%%%%%%%%%%%%%%%%%

%% N. Grima July 2007

%% To select a region %% NG 27 may 2010
%% clear all; close all;
%% max_lont_reg_inc=3.65; min_lont_reg_inc=3.0;  
%% min_latt_reg_inc=42.5; max_latt_reg_inc=43.10;
%% max_lonp_reg_inc=3.65; min_lonp_reg_inc=3.0;  
%% min_latp_reg_inc=42.5; max_latp_reg_inc=43.10;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addpath(genpath(fullfile(pwd,'bg_routines')));

%% Depth
a_traj_depth;

%% Time
a_traj_time;

%% Temperature
a_traj_temp;

%% Salinity
a_traj_salt;

%% Density
a_traj_dens;

