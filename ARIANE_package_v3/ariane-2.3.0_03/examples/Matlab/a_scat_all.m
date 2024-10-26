%% a_scat_all Qualitative mode
%
% N. Grima July 2007

addpath(genpath(fullfile(pwd,'bg_routines')));

%% Point size
sz_pt=3;

%% Depth
a_scat_depth;

%% Time
a_scat_time;

if strcmp(key_alltracers,'.TRUE.')

  %% Temperature
  a_scat_temp;

  %% Salinity
  a_scat_salt;

  %% Density
  a_scat_dens;

end
