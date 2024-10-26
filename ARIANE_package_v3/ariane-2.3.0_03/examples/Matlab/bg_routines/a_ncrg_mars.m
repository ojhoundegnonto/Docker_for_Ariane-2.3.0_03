% 
% Read NetCDF mars grid.
%

if strcmp(nc_var_lon_t_mars,'lon_T') &...
   strcmp(nc_var_lon_u_mars,'lon_U') &...
   strcmp(nc_var_lat_t_mars,'lat_T') &...
   strcmp(nc_var_lat_v_mars,'lat_V')

  nc_var_lat_u_mars='lat_U';
  nc_var_lon_v_mars='lon_V';
  nc_var_lon_f_mars='lon_F';
  nc_var_lat_f_mars='lat_F';

else
  disp(' ');
disp('The Ariane Matlab package doesn t know how to read grid coordinates.');
disp('Please edit bg_routines/a_ncrg_mars.m to set your coordinate data names.');
  disp(' ');
  stop
end

ncload([dir_grd_mars '/' fn_grd_mars],...
       nc_var_lon_t_mars,...
       nc_var_lon_u_mars,...
       nc_var_lat_t_mars,...
       nc_var_lat_v_mars,...
       nc_var_lon_f_mars,...
       nc_var_lat_f_mars,...
       nc_var_lat_u_mars,...
       nc_var_lon_v_mars );

xu = eval(nc_var_lon_u_mars)';
yu = eval(nc_var_lat_u_mars)';
xv = eval(nc_var_lon_v_mars)';
yv = eval(nc_var_lat_v_mars)';

xt = eval(nc_var_lon_t_mars)'; % T grid  longitudes
xp = eval(nc_var_lon_f_mars)'; % F grid (f) longitudes
yt = eval(nc_var_lat_t_mars)'; % T grid latitudes
yp = eval(nc_var_lat_f_mars)'; % F grid (f) latitudes

if exist('ariane_statistics_quantitative.nc')
  ncload('ariane_statistics_quantitative.nc','tmask');
  tmask_reg = squeeze(tmask(1,:,:))';
else
  tmask_reg=ones(size(lon_T'));
end

clear eval(nc_var_lon_u_mars);
clear eval(nc_var_lat_u_mars);
clear eval(nc_var_lon_v_mars);
clear eval(nc_var_lat_v_mars);
clear eval(nc_var_lon_t_mars);
clear eval(nc_var_lon_f_mars);
clear eval(nc_var_lat_t_mars);
clear eval(nc_var_lat_f_mars);

% Periodicity (0 = No periodicity)
%% We assume that there is no periodicity in
%% mars simulation.
iperio = 0;
jperio = 0;
