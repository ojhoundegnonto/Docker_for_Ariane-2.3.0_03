%%%%%%%%%%%%%%%%%%%%%%%%%
%% Readgrid parameters %%
%%%%%%%%%%%%%%%%%%%%%%%%%

if ~exist('key_roms')
  a_ncglobatt;
end

disp(' ');
disp('Reading GRID');

if strcmp(key_roms,'.TRUE.')
  a_ncrg_roms;
elseif strcmp(key_mars,'.TRUE.')
  a_ncrg_mars;
elseif strcmp(key_symphonie,'.TRUE.')
  a_ncrg_symphonie;
else
  a_ncrg_opa;
end

disp('  - compute data on limited region if needed');
%%%%%%%%%%%%%%%%%%%%
%% LIMITED REGION %%
%%%%%%%%%%%%%%%%%%%%
%%------------------------------------------------%%
%% Test if there is a periodicity in i (OPA/NEMO) %%
%%------------------------------------------------%%
if (iperio == 1) & (imt_reg_end < imt_reg_start)
  xt=circshift(xt,[-imt_reg_end 0]);
  xt(xt > 360) = xt(xt > 360) - 360;
  yt=circshift(yt,[-imt_reg_end 0]);
  xp=circshift(xp,[-imt_reg_end 0]);
  xp(xp > 360) = xp(xp > 360) - 360;
  yp=circshift(yp,[-imt_reg_end 0]);
  xu=circshift(xu,[-imt_reg_end 0]);
  xu(xu > 360) = xu(xu > 360) - 360;
  yu=circshift(yu,[-imt_reg_end 0]);
  xv=circshift(xv,[-imt_reg_end 0]);
  xv(xv > 360) = xv(xv > 360) - 360;
  yv=circshift(yv,[-imt_reg_end 0]);
  
  istart_reg = imt_reg_start-imt_reg_end;
  iend_reg   = imt;
  iperio_reg=0;
else
  iperio_reg=1;
  istart_reg = imt_reg_start;
  iend_reg   = imt_reg_end;
end

%%-----------------------------------%%
%% regional longitudes and latitudes %%
%%-----------------------------------%%
% T
xt_reg=xt(istart_reg:iend_reg,jmt_reg_start:jmt_reg_end);
yt_reg=yt(istart_reg:iend_reg,jmt_reg_start:jmt_reg_end);
% F
xp_reg=xp(istart_reg:iend_reg,jmt_reg_start:jmt_reg_end);
yp_reg=yp(istart_reg:iend_reg,jmt_reg_start:jmt_reg_end);
% U
xu_reg=xu(istart_reg:iend_reg,jmt_reg_start:jmt_reg_end);
yu_reg=yu(istart_reg:iend_reg,jmt_reg_start:jmt_reg_end);
% V
xv_reg=xv(istart_reg:iend_reg,jmt_reg_start:jmt_reg_end);
yv_reg=yv(istart_reg:iend_reg,jmt_reg_start:jmt_reg_end);

if ~exist('tmask_reg')
  tmask_reg=tmask(istart_reg:iend_reg,jmt_reg_start:jmt_reg_end);
end

if ~exist('umask_reg')
  umask_reg=tmask_reg*0.;
  for j=1:jmt_reg
    for i=1:imt_reg-1
      if (tmask_reg(i,j) + tmask_reg(i+1,j)) >= 1 
        umask_reg(i,j)=1;
      end
    end
  end
%%  umask_reg=umask(imt_reg_start:imt_reg_end,jmt_reg_start:jmt_reg_end);
end

if ~exist('vmask_reg')  
  vmask_reg=tmask_reg*0.;
  for j=1:jmt_reg-1
    for i=1:imt_reg
      if (tmask_reg(i,j) + tmask_reg(i,j+1)) >= 1 
        vmask_reg(i,j)=1;
      end
    end
  end

  if (jperio==1)
    for i=2:imt_reg
      vmask_reg(imt_reg-i+2,jmt_reg) = vmask_reg(i,jmt_reg-2);
    end
  end
  %%vmask_reg=vmask(imt_reg_start:imt_reg_end,jmt_reg_start:jmt_reg_end);
end

disp('  - mask data');
%% masked
% T
xt_reg_msk=xt_reg;
xt_reg_msk(find(tmask_reg < 0.5)) = NaN;
yt_reg_msk=yt_reg;
yt_reg_msk(find(tmask_reg < 0.5)) = NaN;
% F
xp_reg_msk=xp_reg;
xp_reg_msk(find(tmask_reg < 0.5)) = NaN;
yp_reg_msk=yp_reg;
yp_reg_msk(find(tmask_reg < 0.5)) = NaN;

%% Longitude and Latidue Min and max
%T
min_lont_reg=min(min(xt_reg_msk));
max_lont_reg=max(max(xt_reg_msk));
min_latt_reg=min(min(yt_reg_msk));
max_latt_reg=max(max(yt_reg_msk));
% F
min_lonp_reg=min(min(xp_reg_msk));
max_lonp_reg=max(max(xp_reg_msk));
min_latp_reg=min(min(yp_reg_msk));
max_latp_reg=max(max(yp_reg_msk));

%%%%%%%%%%%%%%%%%%%%
%% ALL THE DOMAIN %%
%%%%%%%%%%%%%%%%%%%%%% masked
%% Longitude and Latidue Min and max
%T
min_lont=min(min(xt));
max_lont=max(max(xt));
min_latt=min(min(yt));
max_latt=max(max(yt));
% F
min_lonp=min(min(xp));
max_lonp=max(max(xp));
min_latp=min(min(yp));
max_latp=max(max(yp));

% To increase by a factor
inc=0.04;
delta_lon = ((max_lont_reg - min_lont_reg) * inc);
delta_lat = ((max_latt_reg - min_latt_reg) * inc);


% T
if (~exist('min_lont_reg_inc') ||...
    ~exist('max_lont_reg_inc') ||...
    ~exist('min_latt_reg_inc') ||...
    ~exist('max_latt_reg_inc'))
  min_lont_reg_inc=min_lont_reg - delta_lon;
  if (min_lont_reg_inc < min(xt(:))); min_lont_reg_inc=min(xt(:));end
  max_lont_reg_inc=max_lont_reg + delta_lon;
  if (max_lont_reg_inc > max(xt(:))); max_lont_reg_inc=max(xt(:));end
  min_latt_reg_inc=min_latt_reg - delta_lat;
  if (min_latt_reg_inc < min(yt(:))); min_latt_reg_inc=min(yt(:));end
  max_latt_reg_inc=max_latt_reg + delta_lat;
  if (max_latt_reg_inc > max(yt(:))); max_latt_reg_inc=max(yt(:));end
end

% F
if (~exist('min_lonp_reg_inc') ||...
    ~exist('max_lonp_reg_inc') ||...
    ~exist('min_latp_reg_inc') ||...
    ~exist('max_latp_reg_inc'))
  min_lonp_reg_inc=min_lonp_reg - delta_lon;
  if (min_lonp_reg_inc < min(xp(:))); min_lonp_reg_inc=min(xp(:));end
  max_lonp_reg_inc=max_lonp_reg + delta_lat;
  if (max_lonp_reg_inc > max(xp(:))); max_lonp_reg_inc=max(xp(:));end
  min_latp_reg_inc=min_latp_reg - delta_lon;
  if (min_latp_reg_inc < min(yp(:))); min_latp_reg_inc=min(yp(:));end
  max_latp_reg_inc=max_latp_reg + delta_lat;
  if (max_latp_reg_inc > max(yp(:))); max_latp_reg_inc=max(yp(:));end
end

clear delta_lon;
clear delta_lat;

disp('Reading GRID is done');
