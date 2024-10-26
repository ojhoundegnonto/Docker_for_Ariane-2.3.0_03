%% Nicolas.Grima@univ-brest.fr
%% june 2019

%% Read xy_zonal and xy_meridionnal, store correctly and mask them.
ncload('ariane_statistics_quantitative.nc',...
       'yz_mer', 'yz_vert');

nb_dims=ndims(yz_mer);

if (nb_dims == 3)
  nb_loop = size(yz_mer,1);
  jmt_reg = size(yz_mer,3);
  kmt_reg = size(yz_mer,2);
elseif (nb_dims == 2)
  nb_loop = 1;
  jmt_reg = size(yz_mer,2);
  kmt_reg = size(yz_mer,1);
else
  errordlg('ERROR: in ariane_statistics_quantitative.nc yz_mer has more than 3 dims',...
           'Error');
  return;
end

% lit les sections qui peuvent ne pas etre rectilignes
if ~exist('segind')
  a_readsec;
end


% Déduire le masque de la variable yz_vert
% car pas forcément rectiligne !!
tmask2 = zeros(jmt_reg,kmt_reg);

for k=1:kmt_reg,
  for j=1:jmt_reg,
    if (yz_vert(1,k,j) ~= 0) || (yz_mer(1,k,j) ~= 0)
      tmask2(j,k) = 1.;
    end
  end
end

% tmask 2 reprend également les section avec des 0 !!
for is = 1:nb_sec
  if (segind(is) > 0) && (j1_reg(is) == j2_reg(is))
    for k = k1_reg(is):k2_reg(is)
      if tmask2(j1_reg(is),k) == 1
        tmask2(j1_reg(is),k) = 0;
        %mtseg(j1_reg(is),k)  = 1;
      end
    end
  end
end

for is = 1:nb_sec
  if (segind(is) > 0) && (k1_reg(is) == k2_reg(is))
    for j = j1_reg(is):j2_reg(is)
      if tmask2(j,k1_reg(is)) == 1
        tmask2(j,k1_reg(is)) = 0;
        %mtseg(j,k1_reg(is))  = 1;
      end
    end
  end
end

%% Calcul de PSI

for i_loop=1:nb_loop

  if ( nb_loop > 1) 
    sq_yz_mer=squeeze(yz_mer(i_loop,:,:));
    sq_yz_mer=sq_yz_mer';

    sq_yz_vert=squeeze(yz_vert(i_loop,:,:));
    sq_yz_vert=sq_yz_vert';

  else
    sq_yz_mer  = squeeze(yz_mer)';
    sq_yz_vert = squeeze(yz_vert)';
  end

  if ((min(min(sq_yz_mer))  == 0.  & ...
       max(max(sq_yz_mer))  == 0.) | ...
      (min(min(sq_yz_vert)) == 0.  & ...
       max(max(sq_yz_vert)) == 0.) )

    disp(' ');
    disp(['psi number:',num2str(i_loop),': all data are null...']);
    
  else

    disp(' ');
    disp(['psi number:', num2str(i_loop)]);

    sq_yz_mer_msk  = sq_yz_mer;
    sq_yz_vert_msk = sq_yz_vert;

    tmask_yz_reg=zeros(jmt_reg,kmt_reg)+1.;
    pmask_yz_reg=tmask_yz_reg;

    for k=1:kmt_reg-1
      for j=1:jmt_reg-1
        
        if ( (sq_yz_mer_msk(j  ,k  )==0.)  &&...
       	     (sq_yz_mer_msk(j+1,k  )==0.)  &&...
	         (sq_yz_vert_msk(j ,k  )==0.)  &&...
	         (sq_yz_vert_msk(j ,k+1)==0.) )
        
	      tmask_yz_reg(j,k)=NaN;
        
        end

        if ( (sq_yz_mer_msk(j   ,k  )==0.)  &&...
       	     (sq_yz_mer_msk(j   ,k+1)==0.)  &&...
	         (sq_yz_vert_msk(j  ,k  )==0.)  &&...
	         (sq_yz_vert_msk(j+1,k  )==0.) )
        
	      pmask_xy_reg(j,k)=NaN;
        
        end
        
      end
    end
    
    a_compute_psi_yz;
    
  end
end

