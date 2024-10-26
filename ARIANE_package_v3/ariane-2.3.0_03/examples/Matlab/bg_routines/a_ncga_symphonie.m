%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Read global attributes which correspond to namelist parameters
% in the Ariane NetCDF outputs for Symphonie model.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addpath(genpath(fullfile(pwd,'bg_routines')));

% Dimensions
x_dim= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'x_dim');
y_dim= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'y_dim');
z_dim= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'z_dim');
time= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'time');

if strcmp(mode,'qualitative')
  if strcmp(key_region,'.FALSE.')
    imt_reg_start     = 1;
    imt_reg_end       = x_dim;
    imt_reg           = x_dim;
    jmt_reg_start     = 1;
    jmt_reg_end       = y_dim;
    jmt_reg           = y_dim;
    kmt_reg_start     = 1;
    kmt_reg_end       = z_dim;
    kmt_reg           = z_dim;
  end
end

% SSE file(s)
c_dir_sse= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'c_dir_sse');
c_prefix_sse= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'c_prefix_sse');
ind0_sse= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'ind0_sse');
indn_sse= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'indn_sse');
maxsize_sse= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'maxsize_sse');
c_suffix_sse= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'c_suffix_sse');
nc_var_sse= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_sse');
nc_att_mask_sse= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_att_mask_sse');

% Symphonie grid file and variable names
dir_grd_symp= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'dir_grd_symp');
fn_grd_symp= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'fn_grd_symp');
nc_var_lon_t_symp= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_lon_t_symp');
nc_var_lon_u_symp= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_lon_u_symp');
nc_var_lat_t_symp= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_lat_t_symp');
nc_var_lat_v_symp= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_lat_v_symp');
nc_var_depth_t_symp= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_depth_t_symp');
