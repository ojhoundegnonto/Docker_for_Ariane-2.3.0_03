%% a_ncga_opa.M - Read in Ariane file the namelist values
%% record as global attributes
%-----------------------------------------------------
% Nicolas.Grima@univ-brest.fr
% 2009 July
%-----------------------------------------------------
imt = netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'imt');
jmt = netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'jmt');
kmt = netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'kmt');
lmt = netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'lmt');

key_computew= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'key_computew');
key_partialsteps= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'key_partialsteps');

key_jfold= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'key_jfold');

%if strcmp(key_jfold,'.TRUE.') && strcmp(key_reducmem,'.FALSE.')
if strcmp(key_jfold,'.TRUE.')
  jperio = 1;
  pivot= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'pivot');
else
  jperio = 0;
end

%pivot = nc.pivot(:);
key_periodic= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'key_periodic');
%if strcmp(key_periodic,'.TRUE.') && strcmp(key_reducmem,'.FALSE.')
if strcmp(key_periodic,'.TRUE.')
  iperio = 1;
else
  iperio = 0;
end

%% OLD OLD OLD OLD OLD OLD
%% Old global attributs %%
%= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'');
%= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'');
% key_sigma        = nc.key_sigma(:);
key_computesigma= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'key_computesigma');
%zsigma           = nc.zsigma(:);
zsigma= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'zsigma');

if strcmp(key_alltracers,'.TRUE.')
  if strcmp(key_computesigma,'.FALSE.')
    % Density file(s)
    c_dir_de= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'c_dir_de');
    c_prefix_de= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'c_prefix_de');
    ind0_de= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'ind0_de');
    indn_de= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'indn_de');
    maxsize_de= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'maxsize_de');
    c_suffix_de= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'c_suffix_de');
    nc_var_de= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_de');
    nc_att_mask_de= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_att_mask_de');
  end
end
%% OLD OLD OLD OLD 0LD 

if strcmp(mode,'qualitative')
  if strcmp(key_region,'.FALSE.')
    imt_reg_start     = 1;
    imt_reg_end       = imt;
    imt_reg           = imt;
    jmt_reg_start     = 1;
    jmt_reg_end       = jmt;
    jmt_reg           = jmt;
    kmt_reg_start     = 1;
    kmt_reg_end       = kmt;
    kmt_reg           = kmt;
  end
else
  if strcmp(key_reducmem,'.FALSE.')
    imt_reg_start     = 1;
    imt_reg_end       = imt;
    imt_reg           = imt;
    jmt_reg_start     = 1;
    jmt_reg_end       = jmt;
    jmt_reg           = jmt;
    kmt_reg_start     = 1;
    kmt_reg_end       = kmt;
    kmt_reg           = kmt;
  end
end

% ORCA grid file and variable names
dir_mesh= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'dir_mesh');
if strcmp( dir_mesh(size(dir_mesh,2)-1:size(dir_mesh,2)),'\0')
  dir_mesh = dir_mesh(1:size(dir_mesh,2)-2);
end

fn_mesh= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'fn_mesh');
nc_var_xx_tt= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_xx_tt');
nc_var_xx_uu= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_xx_uu');
nc_var_zz_ww= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_zz_ww');
nc_var_e2u= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_e2u');
nc_var_e1v= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_e1v');
nc_var_e1t= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_e1t');
nc_var_e2t= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_e2t');
nc_var_e3t= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_e3t');
nc_var_tmask= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_tmask');
nc_mask_val= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_mask_val');

