%% Read xy_zonal and xy_meridionnal, store correctly and mask them.
ncload('ariane_statistics_quantitative.nc',...
       'xy_zonal', 'xy_mer','xy_uh', 'xy_vh');

nb_dims=ndims(xy_zonal);

if (nb_dims == 3)
  nb_loop = size(xy_zonal,1);
elseif (nb_dims == 2)
  nb_loop = 1;
else
  errordlg('ERROR: in ariane_statistics_quantitative.nc xy_zonal has more than 3 dims',...
           'Error');
  return;
end

%% Read the value of the global atribute all_tracers %%
%NG 20 12 2012: nc = netcdf('ariane_statistics_quantitative.nc');
%NG 20 12 2012: key_alltracers        = nc.key_alltracers(:);
%NG 20 12 2012: clear nc;
key_alltracers = ncreadatt('ariane_statistics_quantitative.nc','/','key_alltracers')

if exist('param_psi')

  if (strcmp(key_alltracers,'.TRUE.'))

    if (strcmp(param_psi,'salt'))
      ncload('ariane_statistics_quantitative.nc',...
         'xy_suh', 'xy_svh');

    elseif (strcmp(param_psi,'temp'))
      ncload('ariane_statistics_quantitative.nc',...
         'xy_tuh', 'xy_tvh');


    elseif (strcmp(param_psi,'dens'))
      ncload('ariane_statistics_quantitative.nc',...
         'xy_ruh', 'xy_rvh');

    else
      errordlg('Error: a_xy0','Error');
      return
    end
    
  elseif (strcmp(param_psi,'depth'))
    ncload('ariane_statistics_quantitative.nc',...
         'xy_zuh', 'xy_zvh');

  else
    errordlg('Thers is no tracer in the results. Please submit only a_psi','Error');
    return;
  end
end

if ~exist('segind')
  a_readsec;
end

% Create masks needed to compute psi
for ind = 0: nb_loop
  toto = abs(segind-ind);
  [y,ii]=min(toto);
  pind(ind+1)=ii;
end

% Read grid parameters and data if need
if ~exist('tmask_reg')
  a_ncreadgrid;
end

tmask2 = tmask_reg;
mtseg=zeros(size(tmask_reg));

for is = 1:nb_sec,
  if (segind(is) > 0) & (i1_reg(is) == i2_reg(is))
    for j = j1_reg(is):j2_reg(is),
      if tmask_reg(i1_reg(is),j) == 1
        tmask2(i1_reg(is),j) = 0;
        mtseg(i1_reg(is),j) = 1;
      end
    end
  end
end

for is = 1:nb_sec,
  if (segind(is) > 0) & (j1_reg(is) == j2_reg(is))
    for i = i1_reg(is):i2_reg(is),
      if tmask_reg(i,j1_reg(is)) == 1
        tmask2(i,j1_reg(is)) = 0;
        mtseg(i,j1_reg(is)) = 1;
      end
    end
  end
end

for i_loop=1:nb_loop

  if ( nb_loop > 1) 
    sq_xy_zonal=squeeze(xy_zonal(i_loop,:,:));
    sq_xy_zonal=sq_xy_zonal';

    sq_xy_mer=squeeze(xy_mer(i_loop,:,:));
    sq_xy_mer=sq_xy_mer';

    if iperio == 1
      sq_xy_zonal(1,:) = sq_xy_zonal(1,:) + sq_xy_zonal(imt_reg-1,:);
      sq_xy_zonal(2,:) = sq_xy_zonal(2,:) + sq_xy_zonal(imt_reg,:);
      sq_xy_zonal(imt_reg-1,:) = sq_xy_zonal(1,:);
      sq_xy_zonal(imt_reg,:)   = sq_xy_zonal(2,:);

      sq_xy_mer(1,:) = sq_xy_mer(1,:) + sq_xy_mer(imt_reg-1,:);
      sq_xy_mer(2,:) = sq_xy_mer(2,:) + sq_xy_mer(imt_reg,:);
      sq_xy_mer(imt_reg-1,:) = sq_xy_mer(1,:);
      sq_xy_mer(imt_reg,:)   = sq_xy_mer(2,:);
    end

    if jperio == 1

      for i=1:imt/2
        sq_xy_zonal(i,jmt_reg-2) = ...
          sq_xy_zonal(i,jmt_reg-2) - sq_xy_zonal(imt_reg-i+1,jmt_reg);
        sq_xy_zonal(imt_reg-i+1,jmt_reg)   = -sq_xy_zonal(i,jmt_reg-2);

        sq_xy_zonal(i,jmt_reg-1) = ...
          sq_xy_zonal(i,jmt_reg-1) - sq_xy_zonal(imt_reg-i+1,jmt_reg-1);
        sq_xy_zonal(imt_reg-i+1,jmt_reg-1) = -sq_xy_zonal(i,jmt_reg-1);

        sq_xy_zonal(i,jmt_reg) = ...
          sq_xy_zonal(i,jmt_reg) - sq_xy_zonal(imt_reg-i+1,jmt_reg-2);
        sq_xy_zonal(imt_reg-i+1,jmt_reg-2) = -sq_xy_zonal(i,jmt_reg);
      end

      for i=2:imt/2
        sq_xy_mer(i,jmt_reg-3)=...
          sq_xy_mer(i,jmt_reg-3) - sq_xy_mer(imt_reg-i+2,jmt_reg);
        sq_xy_mer(imt_reg-i+2, jmt_reg)   = -sq_xy_mer(i, jmt_reg-3);

        sq_xy_mer(i,jmt_reg-2)=...
          sq_xy_mer(i,jmt_reg-2) - sq_xy_mer(imt_reg-i+2,jmt_reg-1);
        sq_xy_mer(imt_reg-i+2, jmt_reg-1) = -sq_xy_mer(i, jmt_reg-2);

        sq_xy_mer(i,jmt_reg-1)=...
          sq_xy_mer(i,jmt_reg-1) - sq_xy_mer(imt_reg-i+2,jmt_reg-2);
        sq_xy_mer(imt_reg-i+2, jmt_reg-2) = -sq_xy_mer(i, jmt_reg-1);

        sq_xy_mer(i,jmt_reg)=...
          sq_xy_mer(i,jmt_reg) - sq_xy_mer(imt_reg-i+2,jmt_reg-3);
        sq_xy_mer(imt_reg-i+2, jmt_reg-3) = -sq_xy_mer(i, jmt_reg);
      end

    end

    sq_xy_uh=squeeze(xy_uh(i_loop,:,:));
    sq_xy_uh=sq_xy_uh';

    sq_xy_vh=squeeze(xy_vh(i_loop,:,:));
    sq_xy_vh=sq_xy_vh';

  else
    sq_xy_zonal = squeeze(xy_zonal)';
    sq_xy_mer   = squeeze(xy_mer)';
    sq_xy_uh    = squeeze(xy_uh)';
    sq_xy_vh    = squeeze(xy_vh)';
  end

  if ((min(min(sq_xy_zonal)) == 0  & ...
       max(max(sq_xy_zonal)) == 0) | ...
      (min(min(sq_xy_mer))   == 0  & ...
       max(max(sq_xy_mer))   == 0) )

    disp(' ');
    disp(['psi number:',num2str(i_loop),': all data are null...']);
  else

    disp(' ');
    disp(['psi number:', num2str(i_loop)]);

    sq_xy_zonal_msk=sq_xy_zonal;
%    sq_xy_zonal_msk(find(sq_xy_zonal==0.)) = NaN;

    sq_xy_mer_msk=sq_xy_mer;
%    sq_xy_mer_msk(find(sq_xy_mer==0.)) = NaN;

    sq_xy_uh_msk=sq_xy_uh;
%    sq_xy_uh_msk(find(sq_xy_uh==0.)) = NaN;

    sq_xy_vh_msk=sq_xy_vh;
 %   sq_xy_vh_msk(find(sq_xy_vh==0.)) = NaN;

  if exist('param_psi')

    if (strcmp(param_psi,'salt'))

      if (nb_dims == 3) 
        sq_xy_suh=squeeze(xy_suh(i_loop,:,:));
        sq_xy_suh=sq_xy_suh';

        sq_xy_svh=squeeze(xy_svh(i_loop,:,:));
        sq_xy_svh=sq_xy_svh';
      else
        sq_xy_suh=xy_suh';
        sq_xy_svh=xy_svh';
      end

      sq_xy_suh_msk=sq_xy_suh;
      sq_xy_suh_msk(find(sq_xy_suh==0)) = NaN;

      sq_xy_svh_msk=sq_xy_svh;
      sq_xy_svh_msk(find(sq_xy_svh==0)) = NaN;

    elseif (strcmp(param_psi,'temp'))

      if (nb_dims == 3) 
        sq_xy_tuh=squeeze(xy_tuh(i_loop,:,:));
        sq_xy_tuh=sq_xy_tuh';

        sq_xy_tvh=squeeze(xy_tvh(i_loop,:,:));
        sq_xy_tvh=sq_xy_tvh';
      else
        sq_xy_tuh=xy_tuh';
        sq_xy_tvh=xy_tvh';
      end

      sq_xy_tuh_msk=sq_xy_tuh;
      sq_xy_tuh_msk(find(sq_xy_tuh==0)) = NaN;

      sq_xy_tvh_msk=sq_xy_tvh;
      sq_xy_tvh_msk(find(sq_xy_tvh==0)) = NaN;
  
    elseif (strcmp(param_psi,'depth'))

      if (nb_dims == 3) 
        sq_xy_zuh=squeeze(xy_zuh(i_loop,:,:));
        sq_xy_zuh=sq_xy_zuh';

        sq_xy_zvh=squeeze(xy_zvh(i_loop,:,:));
        sq_xy_zvh=sq_xy_zvh';
      else
        sq_xy_zuh=xy_zuh';
        sq_xy_zvh=xy_zvh';
      end

      sq_xy_zuh_msk=sq_xy_zuh;
      sq_xy_zuh_msk(find(sq_xy_zuh==0)) = NaN;

      sq_xy_zvh_msk=sq_xy_zvh;
      sq_xy_zvh_msk(find(sq_xy_zvh==0)) = NaN;

    elseif (strcmp(param_psi,'dens'))

      if (nb_dims == 3) 
        sq_xy_ruh=squeeze(xy_ruh(i_loop,:,:));
        sq_xy_ruh=sq_xy_ruh';

        sq_xy_rvh=squeeze(xy_rvh(i_loop,:,:));
        sq_xy_rvh=sq_xy_rvh';
      else
        sq_xy_ruh=xy_ruh';
        sq_xy_rvh=xy_rvh';
      end

      sq_xy_ruh_msk=sq_xy_ruh;
      sq_xy_ruh_msk(find(sq_xy_ruh==0)) = NaN;

      sq_xy_rvh_msk=sq_xy_rvh;
      sq_xy_rvh_msk(find(sq_xy_rvh==0)) = NaN;

    else
      errordlg('Error: a_xy0','Error');
      return;
    end
  end

  tmask_xy_reg=zeros(imt_reg,jmt_reg)+1.;
  pmask_xy_reg=tmask_xy_reg;

  for j=1:jmt_reg-1
    for i=1:imt_reg-1
      if ( (sq_xy_zonal_msk(i,j)==0.)   &&...
       	   (sq_xy_zonal_msk(i+1,j)==0.) &&...
	   (sq_xy_mer_msk(i,j)==0.)     &&...
	   (sq_xy_mer_msk(i,j+1)==0.) )
	 tmask_xy_reg(i,j)=NaN;
      end

      if ( (sq_xy_zonal_msk(i,j)==0.)   &&...
       	   (sq_xy_zonal_msk(i,j+1)==0.) &&...
	   (sq_xy_mer_msk(i,j)==0.)     &&...
	   (sq_xy_mer_msk(i+1,j)==0.) )
	 pmask_xy_reg(i,j)=NaN;
      end
    end
  end

  a_compute_psi;

  end

end


