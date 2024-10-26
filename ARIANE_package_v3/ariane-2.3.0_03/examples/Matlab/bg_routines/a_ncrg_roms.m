% 
% Read NetCDF ROMS grid.
%

if strcmp(nc_var_lon_rho_roms,'lon_rho') &...
   strcmp(nc_var_lon_u_roms,'lon_u') &...
   strcmp(nc_var_lat_rho_roms,'lat_rho') &...
   strcmp(nc_var_lat_v_roms,'lat_v')

  nc_var_lat_u_roms='lat_u';
  nc_var_lon_v_roms='lon_v';
  nc_var_lon_psi_roms='lon_psi';
  nc_var_lat_psi_roms='lat_psi';

else
  disp(' ');
disp('The Ariane Matlab package doesn t know how to read grid coordinates.');
disp('Please edit bg_routines/a_ncrg_roms.m to set your coordinate data names.');
  disp(' ');
  stop
end

ncload([dir_grd_roms '/' fn_grd_roms],...
       nc_var_lon_rho_roms,...
       nc_var_lon_u_roms,...
       nc_var_lat_rho_roms,...
       nc_var_lat_v_roms,...
       nc_var_lon_psi_roms,...
       nc_var_lat_psi_roms,...
       nc_var_lat_u_roms,...
       nc_var_lon_v_roms );

xu = eval(nc_var_lon_u_roms)';
yu = eval(nc_var_lat_u_roms)';
xv = eval(nc_var_lon_v_roms)';
yv = eval(nc_var_lat_v_roms)';

xt = eval(nc_var_lon_rho_roms)'; % T grid  longitudes
xp = eval(nc_var_lon_psi_roms)'; % F grid (psi) longitudes
yt = eval(nc_var_lat_rho_roms)'; % T grid latitudes
yp = eval(nc_var_lat_psi_roms)'; % F grid (psi) latitudes

if exist('ariane_statistics_quantitative.nc')
  ncload('ariane_statistics_quantitative.nc','tmask');
  tmask_reg = squeeze(tmask(1,:,:))';
else
  tmask_reg=ones(size(lon_rho'));
end

clear eval(nc_var_lon_u_roms);
clear eval(nc_var_lat_u_roms);
clear eval(nc_var_lon_v_roms);
clear eval(nc_var_lat_v_roms);
clear eval(nc_var_lon_rho_roms);
clear eval(nc_var_lon_psi_roms);
clear eval(nc_var_lat_rho_roms);
clear eval(nc_var_lat_psi_roms);

% Periodicity (0 = No periodicity)
%% We assume that there is no periodicity with
%% ROMS simulation.
iperio = 0;
jperio = 0;
