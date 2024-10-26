%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Read global attributes which correspond to namelist parameters
% in the Ariane NetCDF outputs for ROMS model.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Dimensions
xi_rho= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'xi_rho');
eta_rho= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'eta_rho');
s_w= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'s_w');
time= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'time');

if strcmp(mode,'qualitative')
  if strcmp(key_region,'.FALSE.')
    imt_reg_start     = 1;
    imt_reg_end       = xi_rho-1;
    imt_reg           = imt_reg_end-imt_reg_start+1;
    jmt_reg_start     = 1;
    jmt_reg_end       = eta_rho-1;
    jmt_reg           = jmt_reg_end-jmt_reg_start+1;
    kmt_reg_start     = 1;
    kmt_reg_end       = s_w;
    kmt_reg           = kmt_reg_end-kmt_reg_start+1;
  end
else
  if strcmp(key_reducmem,'.FALSE.')
    imt_reg_start     = 1;
    imt_reg_end       = xi_rho-1;
    imt_reg           = imt_reg_end-imt_reg_start+1;
    jmt_reg_start     = 1;
    jmt_reg_end       = eta_rho-1;
    jmt_reg           = jmt_reg_end-jmt_reg_start+1;
    kmt_reg_start     = 1;
    kmt_reg_end       = s_w;
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

% ROMS global attributes
dir_glbatt= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'dir_glbatt');
fn_glbatt= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'fn_glbatt');
nc_glbatt_hc= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_glbatt_hc');
nc_glbatt_sc_w= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_glbatt_sc_w');
nc_glbatt_Cs_w= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_glbatt_Cs_w');

% ROMS grid file and variable names
dir_grd_roms= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'dir_grd_roms');

if strcmp( dir_grd_roms(size(dir_grd_roms,2)-1:size(dir_grd_roms,2)),'\0')
  dir_grd_roms = dir_grd_roms(1:size(dir_grd_roms,2)-2);
end

fn_grd_roms= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'fn_grd_roms');
nc_var_lon_rho_roms= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_lon_rho_roms');
nc_var_lon_u_roms= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_lon_u_roms');
nc_var_lat_rho_roms= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_lat_rho_roms');
nc_var_lat_v_roms= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_lat_v_roms');
nc_var_pm_roms= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_pm_roms');
nc_var_pn_roms= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_pn_roms');
nc_var_h_roms= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_h_roms');
nc_var_mask_rho_roms= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_mask_rho_roms');

