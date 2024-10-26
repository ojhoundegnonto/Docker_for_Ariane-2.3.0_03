%% N. Grima July 2007 %%
%%

addpath(genpath(fullfile(pwd,'bg_routines')));

%% Load NetCDF data
if ( ~exist('traj_lon')   ||...
     ~exist('traj_lat')   ||...
     ~exist('traj_depth'))
  ncload('ariane_trajectories_qualitative.nc');
end

%% Mask invalid values %%
traj_lon(find(traj_lon     >  1.e19)) = NaN;
traj_lat(find(traj_lat     >  1.e19)) = NaN;
traj_depth(find(traj_depth >  1.e19)) = NaN;

%% Lon and Lat min and max 
if (~exist('max_traj_lon') ||...
    ~exist('min_traj_lon') ||...
    ~exist('max_traj_lat') ||...
    ~exist('min_traj_lat'))

  max_traj_lon=max(max(traj_lon));
  min_traj_lon=min(min(traj_lon));
  max_traj_lat=max(max(traj_lat));
  min_traj_lat=min(min(traj_lat));
end

%%Lon and Lat min and max increase by a factor inc
inc=0.04;
delta_lon = ((max_traj_lon - min_traj_lon) * inc);
delta_lat = ((max_traj_lat - min_traj_lat) * inc);
min_traj_lon_inc=min_traj_lon - delta_lon;
max_traj_lon_inc=max_traj_lon + delta_lon;
min_traj_lat_inc=min_traj_lat - delta_lat;
max_traj_lat_inc=max_traj_lat + delta_lat;

clear delta_lon;
clear delta_lat;

%%%%%%%%%%%%
%% Figure %%
%%%%%%%%%%%%
fid=figure;

%% read grid
a_ncreadgrid;

%% Initialize the map projection. 
%% This first step is needed to use m_map routines.
a_projection;

%% land mask
a_mask_land

%% autorize to plot again on the same figure
hold on;

%% Bathymetry
a_bathy;

%% plot trajectories
m_plot(traj_lon(:,:),traj_lat(:,:));

%% plot initial positions
m_plot(traj_lon(1,:),traj_lat(1,:), 'kx','LineWidth',2);

%% Title and axe labels
title({'Particle trajectories'}, 'fontweight', 'b');
xlabel('longitude', 'fontweight', 'b');
ylabel('latitude', 'fontweight', 'b');

%% save the figure
print -dtiff traj.tif;
