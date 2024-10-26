% 
% Read NetCDF Symphonie grid.
%

ncload([dir_grd_symp '/' fn_grd_symp],...
       nc_var_lon_t_symp,...
       nc_var_lon_u_symp,...
       nc_var_lat_t_symp,...
       nc_var_lat_v_symp,...
       nc_var_depth_t_symp);

xt = eval(nc_var_lon_t_symp)'; % T grid  longitudes
yt = eval(nc_var_lat_t_symp)'; % T grid latitudes

xu=eval(nc_var_lon_u_symp)';
yu=eval(nc_var_lat_t_symp)';
xv=eval(nc_var_lon_t_symp)';
yv=eval(nc_var_lat_v_symp)';

imt=size(xu,1);
jmt=size(xu,2);

xp=xu;
yp=yv;

for ix = 1:imt
  for jy = 1:jmt-1
    xp(ix,jy+1)=xu(ix,jy)+((xu(ix,jy+1)-xu(ix,jy))*0.5);
  end
end
for ix = 1:imt
  for jy = 1:jmt-1
    yp(ix,jy+1)=yu(ix,jy)+((yu(ix,jy+1)-yu(ix,jy))*0.5);
  end
end

for ix=1:imt-1
  xp(ix,1)=xv(ix,1)+((xu(ix+1,1)-xv(ix,1))*0.5);
  yp(ix,1)=yv(ix,1)+((yv(ix+1,1)-yv(ix,1))*0.5);
end

clear lo2;
clear la2;
clear lo3;
clear la3;

if exist('ariane_statistics_quantitative.nc')
  ncload('ariane_statistics_quantitative.nc','tmask');
  tmask_reg = squeeze(tmask(1,:,:))';
else
  tmask_reg=ones(size(xt));
end

% Periodicity (0 = No periodicity)
iperio = 0;
jperio = 0;
