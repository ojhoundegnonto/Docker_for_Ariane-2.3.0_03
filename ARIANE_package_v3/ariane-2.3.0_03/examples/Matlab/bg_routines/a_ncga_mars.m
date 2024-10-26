%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Read global attributes which correspond to namelist parameters
% in the Ariane NetCDF outputs for MARS3D model.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Dimensions
x_t    = netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'x_t');
y_t    = netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'y_t');
sigma_t= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'sigma_t');
time   = netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'time');

if strcmp(mode,'qualitative')
  if strcmp(key_region,'.FALSE.')
    imt_reg_start     = 1;
    imt_reg_end       = x_t-1;
    imt_reg           = imt_reg_end-imt_reg_start+1;
    jmt_reg_start     = 1;
    jmt_reg_end       = y_t-1;
    jmt_reg           = jmt_reg_end-jmt_reg_start+1;
    kmt_reg_start     = 1;
    kmt_reg_end       = sigma_t;
    kmt_reg           = kmt_reg_end-kmt_reg_start+1;
  end
else
  if strcmp(key_reducmem,'.FALSE.')
    imt_reg_start     = 1;
    imt_reg_end       = x_t-1;
    imt_reg           = imt_reg_end-imt_reg_start+1;
    jmt_reg_start     = 1;
    jmt_reg_end       = y_t-1;
    jmt_reg           = jmt_reg_end-jmt_reg_start+1;
    kmt_reg_start     = 1;
    kmt_reg_end       = sigma_t;
    kmt_reg           = kmt_reg_end-kmt_reg_start+1;
  end
end

% ZETA file(s)
c_dir_ze= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'c_dir_ze');
c_prefix_ze= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'c_prefix_ze');
ind0_ze= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'ind0_ze');
indn_ze= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'indn_ze');
maxsize_ze= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'maxsize_ze');
c_suffix_ze= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'c_suffix_ze');
nc_var_ze= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_ze');
nc_att_mask_ze= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_att_mask_ze');

% MARS3D grid file and variable names
dir_grd_mars= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'dir_grd_mars');

if strcmp( dir_grd_mars(size(dir_grd_mars,2)-1:size(dir_grd_mars,2)),'\0')
  dir_grd_mars = dir_grd_mars(1:size(dir_grd_mars,2)-2);
end

fn_grd_mars= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'fn_grd_mars');
nc_var_lon_t_mars= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_lon_t_mars');
nc_var_lon_u_mars= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_lon_u_mars');
nc_var_lat_t_mars= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_lat_t_mars');
nc_var_lat_v_mars= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_lat_v_mars');
nc_var_hc= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_hc');
nc_var_sc_w= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_sc_w');
nc_var_Cs_w= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_Cs_w');
nc_var_bathy_t_mars= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_bathy_t_mars');
nc_var_mask_t_mars= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_mask_t_mars');


