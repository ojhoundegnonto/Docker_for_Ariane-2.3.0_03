%%%%%%%%%%%%%%%%%%%%%%%%%
%% Plot the bathymetry %%
%%%%%%%%%%%%%%%%%%%%%%%%%
%% N. Grima April 2008 %%
%%%%%%%%%%%%%%%%%%%%%%%%%

bathy_levels=[-100 -200 -500 -1000 -2000 -3000 -4000];

if ~exist('xt')
  a_ncreadgrid;
end

a_projection;

if strcmp(key_roms,'.TRUE.')
  ncload([dir_grd_roms '/' fn_grd_roms],'h');
  bathy=-h';
  clear h;
  m_contour(xt,yt,bathy,bathy_levels,'b');
elseif strcmp(key_symphonie,'.TRUE.')
  ncload([dir_grd_symp '/' fn_grd_symp],'bathy_m');
  bathy=-bathy_m';
  clear bathy_m;
  m_contour(xt,yt,bathy,bathy_levels,'b');
else
  m_elev('contour',bathy_levels,'edgecolor','b');
end
