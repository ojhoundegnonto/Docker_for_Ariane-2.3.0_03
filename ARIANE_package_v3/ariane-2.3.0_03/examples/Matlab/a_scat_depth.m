%% N. Grima July 2007 %%
%%

addpath(genpath(fullfile(pwd,'bg_routines')));

disp(' ');
disp('Reading lon, lat and depth values.');
%% Load Neta_scat_temp;CDF data
if ( ~exist('traj_lon')   ||...
     ~exist('traj_lat')   ||...
     ~exist('traj_depth'))
  ncload('ariane_trajectories_qualitative.nc');
end
disp('Reading is done.');

%% Size point
if ~exist('sz_pt')
  sz_pt = 3;
end

%% Mask data where values are > 1.e19 
%% NetCDF mask value = 1.e20
traj_lon(find(traj_lon     >  1.e19)) = NaN;
traj_lat(find(traj_lat     >  1.e19)) = NaN;
traj_depth(find(traj_depth >  1.e19)) = NaN;

%%Lon and Lat min and max 
if (~exist('max_traj_lon') ||...
    ~exist('min_traj_lon') ||...
    ~exist('max_traj_lat') ||...
    ~exist('min_traj_lat'))

  max_traj_lon=max(max(traj_lon));
  min_traj_lon=min(min(traj_lon));
  max_traj_lat=max(max(traj_lat));
  min_traj_lat=min(min(traj_lat));
end

%% Number of indices in i and j
nb_i=size(traj_lon, 1);
nb_j=size(traj_lon, 2);

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

%%%%%%%%%%%%%%%%%%
%% Figure Depth %%
%%%%%%%%%%%%%%%%%%
fid_depth=figure;

disp(' ');
disp('Computing depth and color values.');
%% Define colorbar
c_depth=jet;
szc=size(c_depth,1);
min_depth=min(min(traj_depth));
max_depth=max(max(traj_depth));
delta_depth= ( max_depth - min_depth) / (szc-2);
ind_depth = ones(size(traj_depth(:,:)),'int32');
ind_depth(:,:) = int32( (traj_depth(:,:) - min_depth)/delta_depth) + 2;
disp('Computing is done.');

%% read grid
if ~exist('xt')
  a_ncreadgrid;
end

%% Initialize the map projection. 
%% This first step is needed to use m_map routines.
a_projection;

%% land mask
a_mask_land

%% autorize to plot again on the same figure
hold on;

%% Bathymetry %%
a_bathy

if (nb_j > 500)
  disp('  ');
  disp('---');
  disp(['--- Warning: the number of trajectories is big: ',...
        num2str(nb_j)]);
  disp('--- Warning:       --PLEASE WAIT--');
  disp('---');
end

%% plot trajectories
for j=1:nb_j
  for i=1:nb_i 
    m_plot(traj_lon(i,j), traj_lat(i,j),'.',...
	  'MarkerEdgeColor',c_depth(ind_depth(i,j),:),...
          'MarkerFaceColor',[0 0 0],...
          'MarkerSize',sz_pt);
  end
end

%% Colorbar
set(gca,'CLim',[min_depth max_depth]);
colorbar;

%% plot initial positions
m_plot(traj_lon(1,:),traj_lat(1,:), 'kx','LineWidth',2);
%% Title and axe labels
title({'Particle trajectories - Depth \rm(m)'}, 'fontweight', 'b');
xlabel('longitude', 'fontweight', 'b');
ylabel('latitude', 'fontweight', 'b');

print -dtiff traj_depth.tif;

