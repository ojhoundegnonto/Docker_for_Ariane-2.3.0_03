%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Compute corresponding i/j from lon/lat %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Usefull to initialized particle        %%
%% positions in qualitative experiment.   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Nicolas.Grima@univ-brest.fr %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% IDL source code %%
%%%%%%%%%%%%%%%%%%%%%
%% for n=0,nn-1 do begin $
%%  toto=min(abs(lon(n)-xu),itoto) & ii=itoto mod imt & jj=(itoto-ii)/imt &$
%%  if xu(ii,jj) lt lon(n) then begin $
%%   fi(n)=1+ii+(lon(n)-xu(ii,jj))/(xu(ii+1,jj)-xu(ii,jj)) & end else begin $
%%   fi(n)=1+ii-(lon(n)-xu(ii,jj))/(xu(ii-1,jj)-xu(ii,jj))
%% 
%% for n=0,nn-1 do begin $ (BUG - BUG)
%%  toto=min(abs(lat(n)-yv),itoto) & ii=itoto mod imt & jj=(itoto-ii)/imt &$
%%  if yv(ii,jj) lt lat(n) then begin $
%%   fj(n)=1+jj+(lat(n)-yv(ii,jj))/(yv(ii,jj+1)-yv(ii,jj)) & end else begin $
%%   fj(n)=1+jj-(lat(n)-yv(ii,jj))/(yv(ii,jj-1)-yv(ii,jj))
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Note: le calcul de l'indice vertical fw est un peu plus compliqué que les'
%% indices horizontaux. Si tu veux l'implémenter, tu peux utiliser la relation'
%% suivante (établie à partir des niveaux verticaux FIXES du maillage, cad
%% obtenus pour zeta=0)
%% 
%% ; point T immediatement au sud et a l'est'
%% i=fix(fi+.5)-1 & j=fix(fj+.5)-1
%% a=-(fi-1.)+.5+i & b=-(fj-1.)+.5+j
%% zz=fltarr(kmt,nn)
%% for n=0,nn-1 do begin $
%%  zz(*,n)=a(n)*b(n)*zw(i(n),j(n),*)+$
%%          a(n)*(1.-b(n))*zw(i(n),j(n)+1,*)+$
%%          (1.-a(n))*b(n)*zw(i(n)+1,j(n),*)+$
%%          (1.-a(n))*(1.-b(n))*zw(i(n)+1,j(n)+1,*)
%% for n=0,nn-1 do begin $
%%  toto=min(abs(dep(n)-zz(*,n)),kk) &$
%%  if zz(kk,n) lt dep(n) then begin $
%%   fk(n)=1+kk-(dep(n)-zz(kk,n))/(zz(kk-1,n)-zz(kk,n)) & end else begin $
%%   fk(n)=1+kk+(dep(n)-zz(kk,n))/(zz(kk+1,n)-zz(kk,n))  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addpath(genpath(fullfile(pwd,'bg_routines')));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%             If needed                %%
%% Read the longitude and latidude data %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if (~exist('xu') ||...
    ~exist('yv') ||...
    ~exist('zz'))
  
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %% Dialog box to select the grid file %%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if exist('PathName_grd')
    PathSearch=strcat(PathName_grd,'*.nc');
  else
    PathSearch=('*.nc');
  end
  [FileName,PathName_grd] = uigetfile(PathSearch,'Select the GRID file');
  grd_fn=strcat(PathName_grd,FileName);
  clear FileName;
  disp(['Grid file: ', grd_fn])
  
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %% Question box to choose the model %%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  Default='OPA/NEMO';
  [model] = questdlg('Please select the model corresponding to the grid file selected',...
                     'Model selection',...
                     Default,'ROMS','Symphonie',Default);
  clear Default;
  disp(['Model    : ', model])
  
  if strcmp(model,'ROMS')
    ncload(grd_fn,'lon_u','lat_v');
    xu = lon_u'; %'
    yv = lat_v'; %'
whos xu yv
    clear lon_u lat_v;
    
    PathSearch=strcat(PathName_grd,'*.nc');
    [data_FileName,PathName] = uigetfile(PathSearch,'Select a DATA file');
    data_fn=strcat(PathName,data_FileName);
    clear PathName PathSearch;
    disp(['Data file: ', data_fn]);
    
    zz3d=roms_get_depths(data_fn,grd_fn,1,'w');

  elseif strcmp(model,'Symphonie')

    ncload(grd_fn,'lon2','lat3');
    xu = lon2'; %'
    yv = lat3'; %'
    clear lon2 lat3;

    PathSearch=strcat(PathName_grd,'*.nc');
    [data_FileName,PathName] = uigetfile(PathSearch,'Select a DATA file');
    data_fn=strcat(PathName,data_FileName);
    clear PathName PathSearch;
    disp(['Data file: ', data_fn]);
    nc     = netcdf(data_fn,'nowrite');
    depthT = nc{'level_dpth'}(:);
    sse    = nc{'surfelev'}(:);
    close(nc);

    [kmax, jmax, imax] = size(depthT);

    zw0   = zeros(kmax, jmax, imax);
    zz_ww = zeros(kmax, jmax, imax);

    for kk = 2:kmax
      for jj = 1:jmax
        for ii = 1:imax
     	  zw0(kk, jj, ii) = 2.*depthT(kmax-kk+2, jj, ii) - zw0(kk-1, jj, ii);
        end
      end
    end

    max_depth = max(abs(zw0(:,:,:)));

    for kk = 1:kmax
      for jj = 1:jmax
        for ii = 1:imax
	  if (max_depth(1,jj,ii) ~= 0.)
	    zz_ww(kk, jj, ii) = sse(jj,ii) + zw0(kk,jj,ii) *...
	    (1.0 + ( sse(jj,ii)/max_depth(1,jj,ii) ) );
          end
        end
      end
    end

    zz3d=zz_ww;

    clear zz_ww zw0 depthT see max_depth;

  else 
    %% OPA/NEMO
    ncload(grd_fn,'glamu','gphiv','gdepw');
    xu   = glamu'; %'
    yv   = gphiv'; %'
    zz3d = gdepw'; %'
    clear glamu gphiv gdepw;
  end
  
  zz3d=-abs(zz3d);
  
  clear grd_fn;
  
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Array size in i and j %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
[nb_iu,nb_ju]=size(xu);
[nb_iv,nb_jv]=size(yv);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Min and Max of the longitude and latitude arrays %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
min_lon=min(min(xu));
min_lat=min(min(yv));
min_depth=min(min(min(zz3d)));

max_lon=max(max(xu));
max_lat=max(max(yv));
max_depth=max(max(max(zz3d)));

resp='Yes';

fid = fopen('initial_positions.txt','a');

lon_old   = (min_lon+max_lon)/2.;
lat_old   = (min_lat+max_lat)/2.;
depth_old = (min_depth+max_depth)/2.;

while ( strcmp(resp,'Yes') || strcmp(resp,'No'))

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %% Dialog box to enter longitude and latitude %%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  lon   = min_lon-1.;
  lat   = min_lat-1.;
  depth = min_depth-1.;
  
  while ( (lon < min_lon) || (lon > max_lon) ||...
          (lat < min_lat) || (lat > max_lat) ||...
          (depth < min_depth) || (depth > max_depth)   )
    prompt = {['Enter longitude (min=' num2str(min_lon) ' max=' num2str(max_lon),')'],...
              ['Enter latitude  (min=' num2str(min_lat) ' max=' num2str(max_lat),')'],...
              ['Enter depth     (min=' num2str(min_depth) ' max=' num2str(max_depth),')']};
    dlg_title = 'Ariane tool: lon/lat/depth => i/j/k';
    num_lines = 1;
    def       = {num2str(lon_old),...
                 num2str(lat_old),...
                 num2str(depth_old)};
    answer    = inputdlg(prompt,dlg_title,num_lines,def);

    if (isempty(answer))
      %%%%%%%%%%%%%%%%%%%%%
      %% Exit while loop %%
      %%%%%%%%%%%%%%%%%%%%%
      break;
    else
      [lon     status_lon] = str2num(answer{1});
      [lat     status_lat] = str2num(answer{2});
      [depth status_depth] = str2num(answer{3});
    end
    
    if (status_lon && status_lat && status_depth)
      disp('');
    else
      disp(' ');
      disp('!!! Please enter a real value for longitude and latitude !!!');
      lon   = min_lon-1.;
      lat   = min_lat-1.;
      depth = min_depth-1.;
    end

  end
  
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %% Exit the main while loop   %%
  %% Finish this Matlab routine %%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if (isempty(answer))
    break;
  end
  
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %% Treatment of the longitude %%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  [min_lon_diff, ind_min_lon_diff]=min(abs(xu(:)-lon));
  ii=int32(mod(ind_min_lon_diff,nb_iu))   + 1;
  jj=int32(((ind_min_lon_diff-ii)/nb_iu)) + 1;

  if ((xu(ii,jj) < lon) && (ii ~= nb_iu))
    fi=double(ii)+(lon-xu(ii,jj))/(xu(ii+1,jj)-xu(ii,jj));
  elseif ((xu(ii,jj) > lon) && (ii ~= 1))
    fi=double(ii)-(lon-xu(ii,jj))/(xu(ii-1,jj)-xu(ii,jj));
  else
    fi=double(ii);
  end

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ù
  %% Treatment of the latitude %%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%EC start
  yv_1d=yv(1,:);
  [min_lat_diff, ind_min_lat_diff]=min(abs(yv_1d(:)-lat));
  jj=int32(mod(ind_min_lat_diff,nb_jv));
  ii=int32(((ind_min_lat_diff-jj)/nb_jv)) + 1;
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%EC end
  if ((yv(ii,jj) < lat) && (jj ~= nb_jv))
    fj=double(jj)+(lat-yv(ii,jj))/(yv(ii,jj+1)-yv(ii,jj))
  elseif ((yv(ii,jj) > lat) && (jj ~= 1))
    fj=double(jj)-(lat-yv(ii,jj))/(yv(ii,jj-1)-yv(ii,jj))
  else
    fj=double(jj);
  end

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %% Treatment of the depth %%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  if strcmp(model,'ROMS')
    
    zz=squeeze(zz3d(:,jj,ii));
    
    ii=max(fix(fi+.5),1);
    jj=max(fix(fj+.5),1);
  
    coef_a = -fi+.5+double(ii);
    coef_b = -fj+.5+double(jj);
    
    zz = ...
        coef_a      *     coef_b  * zz3d(:,jj  ,ii  )+...
        coef_a      * (1.-coef_b) * zz3d(:,jj+1,ii  )+...
        (1.-coef_a) *     coef_b  * zz3d(:,jj  ,ii+1)+...
        (1.-coef_a) * (1.-coef_b) * zz3d(:,jj+1,ii+1);
    zz=flipud(zz);



    
  elseif strcmp(model,'Symphonie')

    zz=squeeze(zz3d(:,jj,ii));
    
    ii=max(fix(fi+.5),1);
    jj=max(fix(fj+.5),1);
  
    coef_a = -fi+.5+double(ii);
    coef_b = -fj+.5+double(jj);
    
    zz = ...
        coef_a      *     coef_b  * zz3d(:,jj  ,ii  )+...
        coef_a      * (1.-coef_b) * zz3d(:,jj+1,ii  )+...
        (1.-coef_a) *     coef_b  * zz3d(:,jj  ,ii+1)+...
        (1.-coef_a) * (1.-coef_b) * zz3d(:,jj+1,ii+1);
    zz=flipud(zz);

  else 
    %% OPA/NEMO
    zz=zz3d;
  end
  
  [kmt]=size(zz,1);
  
  [min_depth_diff, ind_min_depth_diff]=min(abs(zz(:)-depth));
  
  kk=ind_min_depth_diff;
 
  if ( (zz(kk) < depth) && (kk ~= 1)) 
    fk=double(kk)-(depth-zz(kk))/(zz(kk-1)-zz(kk));
  elseif  ( (zz(kk) > depth) && (kk ~= kmt))
    fk=double(kk)+(depth-zz(kk))/(zz(kk+1)-zz(kk));
  else
    fk=double(kk);
  end

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %% Question: record or not data %%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  disp(' ');
  disp(['lon  :',num2str(lon)  ,' => fi: ',num2str(fi)])
  disp(['lat  :',num2str(lat)  ,' => fj: ',num2str(fj)])
  disp(['depth:',num2str(depth),' => fk: ',num2str(fk)])
  Default='No';
  
  lon_old=lon;
  lat_old=lat;
  depth_old=depth;

  resp = questdlg({['Do you want to record this position?'],...
                   ['lon:',num2str(lon),' => fi: ',num2str(fi)],...
                   ['lat:',num2str(lat),' => fj: ',num2str(fj)],...
                   ['depth:',num2str(depth),' => fk: ',num2str(fk)]},...
                  'Record or not the results?',...
                  'Yes', Default, 'Exit', Default);

  if strcmp(resp,'Yes')
    fprintf(fid, '%6.3f %6.3f %6.3f %6.3f %6.3f\n', ...
            fi, fj, fk,1.,1.);
  end

end
  
fclose(fid);
