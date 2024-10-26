%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Read global attributes which correspond to namelist parameters
% in the Ariane NetCDF outputs.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nbga = 1;

disp(sprintf('\n'));
disp('Reading Global attributes in Ariane output file:')
if exist('ariane_trajectories_qualitative.nc')
  disp('    - ariane_trajectories_qualitative.nc')
  nc = netcdf.open('ariane_trajectories_qualitative.nc','NC_NOWRITE');
elseif exist('ariane_statistics_quantitative.nc')
  disp('    - ariane_statistics_quantitative.nc')
  nc = netcdf.open('ariane_statistics_quantitative.nc','NC_NOWRITE');
else
  disp('ERROR: no correct Ariane NetCDF input file.');
  disp('    - ariane_trajectories_qualitative.nc');
  disp('or  - ariane_statistics_quantitative.nc');
  stop;
end

%  = netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),);
key_roms = netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'key_roms');
key_mars = netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'key_mars');
key_symphonie= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'key_symphonie');
key_sequential= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'key_sequential');
key_ascii_outputs= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'key_ascii_outputs');
key_read_age= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'key_read_age');
mode= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'mode');
forback= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'forback');
bin= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'bin');
init_final= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'init_final');
nmax= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nmax');
tunit= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'tunit');
ntfic= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'ntfic');
tcyc= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'tcyc');
key_approximatesigma= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'key_approximatesigma');
key_computesigma= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'key_computesigma');
zsigma = netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'zsigma');
key_alltracers= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'key_alltracers');

if strcmp(key_sequential,'.TRUE.')
  key_interp_temporal = netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'key_interp_temporal');
  maxcycles= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'maxcycles');
end

if strcmp(mode,'qualitative')
   delta_t    = netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'delta_t');
   frequency  = netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'frequency');
   mask       = netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'mask');
   key_region = netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'key_region');
   nb_output  = netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nb_output');
            
  %% To correct a bug in Ariane
  if isempty(key_region)
    key_region = netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'key_region');
  end
  if strcmp(key_region,'.TRUE.')
    imt_reg_start = netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'imt_reg_start');
    imt_reg_end= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'imt_reg_end');
    imt_reg= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'imt_reg');
    jmt_reg_start= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'jmt_reg_start');
    jmt_reg_end= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'jmt_reg_end');
    jmt_reg= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'jmt_reg');
    kmt_reg_start= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'kmt_reg_start');
    kmt_reg_end= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'kmt_reg_end');
    kmt_reg= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'kmt_reg');
    imt_reg_start = double(imt_reg_start);
    imt_reg_end   = double(imt_reg_end);
    imt_reg       = double(imt_reg);
    jmt_reg_start = double(jmt_reg_start);
    jmt_reg_end   = double(jmt_reg_end);
    jmt_reg       = double(jmt_reg);
    kmt_reg_start = double(kmt_reg_start);
    kmt_reg_end   = double(kmt_reg_end);
    kmt_reg       = double(kmt_reg);
  end
else
  key_2dquant= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'key_2dquant');
  key_eco= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'key_eco');
  key_reducmem= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'key_reducmem');
  
  if strcmp( key_reducmem(size(key_reducmem,2)-1:size(key_reducmem,2)),'\0')
    key_reducmem = netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'key_reducmem');
  end
  if strcmp(key_reducmem,'.TRUE.')
    imt_reg_start = netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'imt_reg_start');
    imt_reg_end= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'imt_reg_end');
    imt_reg= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'imt_reg');
    jmt_reg_start= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'jmt_reg_start');
    jmt_reg_end= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'jmt_reg_end');
    jmt_reg= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'jmt_reg');
    kmt_reg_start= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'kmt_reg_start');
    kmt_reg_end= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'kmt_reg_end');
    kmt_reg= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'kmt_reg');
    imt_reg_start = double(imt_reg_start);
    imt_reg_end   = double(imt_reg_end);
    imt_reg       = double(imt_reg);
    jmt_reg_start = double(jmt_reg_start);
    jmt_reg_end   = double(jmt_reg_end);
    jmt_reg       = double(jmt_reg);
    kmt_reg_start = double(kmt_reg_start);
    kmt_reg_end   = double(kmt_reg_end);
    kmt_reg       = double(kmt_reg);
  end
  key_unitm3= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'key_unitm3');
  key_nointerpolstats= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'key_nointerpolstats');
  max_transport = netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'max_transport');
  lmin= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'lmin');
  lmax= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'lmax');
end


% Zonal current file(s)
c_dir_zo= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'c_dir_zo');
c_prefix_zo= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'c_prefix_zo');
ind0_zo= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'ind0_zo');
indn_zo= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'indn_zo');
maxsize_zo= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'maxsize_zo');
c_suffix_zo= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'c_suffix_zo');
nc_var_zo= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_zo');
nc_var_eivu= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_eivu');
nc_att_mask_zo= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_att_mask_zo');

% Meridional current file(s)
c_dir_me= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'c_dir_me');
c_prefix_me= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'c_prefix_me');
ind0_me= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'ind0_me');
indn_me= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'indn_me');
maxsize_me= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'maxsize_me');
c_suffix_me= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'c_suffix_me');
nc_var_me= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_me');
nc_var_eivv= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_eivv');
nc_att_mask_me= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_att_mask_me');

if exist('key_computew')
    
  if strcmp(key_computew,'.FALSE.')
    c_dir_ve= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'c_dir_ve');
    c_prefix_ve= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'c_prefix_ve');
    ind0_ve= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'ind0_ve');
    indn_ve= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'indn_ve');
    maxsize_ve= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'maxsize_ve');
    c_suffix_ve= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'c_suffix_ve');
    nc_var_ve= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_ve');
    nc_var_eivw= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_eivw');
    nc_att_mask_ve= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_att_mask_ve');
  end
end


if strcmp(key_alltracers,'.TRUE.')
  % Temperature file(s)
  c_dir_te= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'c_dir_te');
  c_prefix_te= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'c_prefix_te');
  ind0_te= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'ind0_te');
  indn_te= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'indn_te');
  maxsize_te= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'maxsize_te');
  c_suffix_te= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'c_suffix_te');
  nc_var_te= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_te');
  nc_att_mask_te= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_att_mask_te');

  % Salinity file(s)
  c_dir_sa= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'c_dir_sa');
  c_prefix_sa= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'c_prefix_sa');
  ind0_sa= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'ind0_sa');
  indn_sa= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'indn_sa');
  maxsize_sa= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'maxsize_sa');
  c_suffix_sa= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'c_suffix_sa');
  nc_var_sa= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_var_sa');
  nc_att_mask_sa= netcdf.getAtt(nc,netcdf.getConstant('NC_GLOBAL'),'nc_att_mask_sa');
  
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

if strcmp(key_roms,'.TRUE.')
  disp('    - results computed with ROMS data')
  a_ncga_roms;
elseif strcmp(key_mars,'.TRUE.')
  disp('    - results computed with MARS3D data')
  a_ncga_mars;
elseif strcmp(key_symphonie,'.TRUE.')
  disp('    - results computed with SYMPHONIE data')
  a_ncga_symphonie;  
else
  disp('    - results computed with OPA data')
  a_ncga_opa;
end

clear nc;

disp('Reading is done.')
