%% N. Grima August 2007 %%
%%

addpath(genpath(fullfile(pwd,'bg_routines')));

%% Load NetCDF data
if ( ~exist('traj_lon')   ||...
     ~exist('traj_lat')   ||...
     ~exist('traj_depth') ||...
     ~exist('traj_depth'))
  ncload('ariane_trajectories_qualitative.nc');
end

%% Mask invalid values %%
traj_lon(find(traj_lon     >  1.e19)) = NaN;
traj_lat(find(traj_lat     >  1.e19)) = NaN;
traj_depth(find(traj_depth >  1.e19)) = NaN;
traj_depth(find(traj_depth   >  1.e19)) = NaN;

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

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Number of particules %%
%%%%%%%%%%%%%%%%%%%%%%%%%%
nb_traj = size(traj_depth,2);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Colorbar for depth data %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c_depth=jet;
szc=size(c_depth,1);
min_depth=min(min(traj_depth(:,:)));
max_depth=max(max(traj_depth(:,:)));

delta_depth= ( max_depth - min_depth) / (szc-1);

ind_depth = ones(size(traj_depth(:,:)),'int32');

ind_depth(:,:) = int32( (traj_depth(:,:) - min_depth)/delta_depth) + 1;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compute the positions of basic lines %% 
%% centered to each point               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LON %%
%%%%%%%%%
sz_traj_lon=size(traj_lon, 1);
traj_lon_plot=zeros(sz_traj_lon + 1, size(traj_lon, 2));
traj_lon_plot(1,:)=traj_lon(1,:);
traj_lon_plot(sz_traj_lon+1,:)=traj_lon(sz_traj_lon,:);
for is=2:sz_traj_lon
  traj_lon_plot(is,:) = (traj_lon(is-1,:) + traj_lon(is,:)) * 0.5;
end

%%%%%%%%%
%% LAT %%
%%%%%%%%%
sz_traj_lat=size(traj_lat, 1);
traj_lat_plot=zeros(sz_traj_lat + 1, size(traj_lat, 2));
traj_lat_plot(1,:)=traj_lat(1,:);
traj_lat_plot(sz_traj_lat+1,:)=traj_lat(sz_traj_lat,:);
for is=2:sz_traj_lat
  traj_lat_plot(is,:) = (traj_lat(is-1,:) + traj_lat(is,:)) * 0.5;
end


%%%%%%%%%%%%
%% Figure %%
%%%%%%%%%%%%
fid_depth=figure;

%% read grid
if ~exist('xt')
  a_ncreadgrid;
end

%% Initialize the map projection. 
%% This first step is needed to use m_map routines.
a_projection

%% land mask
a_mask_land

%% autorize to plot again on the same figure
hold on;

%% Bathymetry %%
a_bathy

for j=1:nb_traj
  for i=1:sz_traj_lon
    if ( m_lldist(traj_lon_plot(i:i+1,j), traj_lat_plot(i:i+1,j)) < 2000. )
      m_plot(traj_lon_plot(i:i+1,j), traj_lat_plot(i:i+1,j),'-',...
	    'LineWidth', 2,'Color', c_depth(ind_depth(i,j),:));
    end
  end
end

%% plot initial positions
m_plot(traj_lon(1,:),traj_lat(1,:), 'kx','LineWidth',2);

colorbar;
set(gca,'CLim',[min_depth max_depth]);

title({'Particle trajectories - Depth'}, 'fontweight', 'b');
xlabel('longitude', 'fontweight', 'b');
ylabel('latitude', 'fontweight', 'b');

%% save the figure
print -dtiff traj_depth.tif;