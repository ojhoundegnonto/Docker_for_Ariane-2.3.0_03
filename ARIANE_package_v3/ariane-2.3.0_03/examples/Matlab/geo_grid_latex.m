%% input
%% -----
%% lon_init
%% lat_init
%% depth_init
%% model [opa, ROMS, Symphonie]
%% 
%% output
%% -----
%% fi
%% fj
%% fk

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Array size in i and j %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
[nb_iu,nb_ju]=size(xu);
[nb_iv,nb_jv]=size(yv);

dist=100000000000000000.;

for jj = 2: nb_ju
    for ii = 2: nb_iu
        
        dist1=m_lldist([xt(ii,jj)  lon_init],[yt(ii,jj) lat_init]);
        
        if (dist1 < dist)
            dist=dist1;
            fi_dT=ii;
            fj_dT=jj;
        end
        
    end
end

%%%%% Solution Mathematica utilisant les points F (psi) %%%%%
%- x et y sont les coordonnées du point dont on cherche 
% les indices gi et gj sur le maillage Ariane.
%- on fait l'hypothèse que l'on connaît le point central de la maille T(i,j)
%- on note xa et ya les coordonnées du point F(i-1,j-1) ["en bas à gauche"]
%- on note xb et yb les coordonnées du point F(i,j-1) ["en bas à droite"]
%- on note xc et yc les coordonnées du point F(i-1,j) ["en haut à gauche"]
%- on note xd et yd les coordonnées du point F(i,j) ["en haut à droite"]
% 
% Alors tu peux tester les indices gi=i-1+alpha et gj=j-1+beta où (alpha,beta)
% est l'un des 2 couples solutions donnés dans le document joint.
% 
% Un seul des 2 couples doit effectivement vérifier 0 <= alpha <= 1 
% et 0 <= beta <= 1
% C'est celui qu'il faut conserver...
x = lon_init;
y = lat_init;
xa = xf(fi_dT-1,fj_dT-1);
xb = xf(fi_dT  ,fj_dT-1);
xc = xf(fi_dT-1,fj_dT  );
xd = xf(fi_dT  ,fj_dT  );

ya = yf(fi_dT-1,fj_dT-1);
yb = yf(fi_dT  ,fj_dT-1);
yc = yf(fi_dT-1,fj_dT  );
yd = yf(fi_dT  ,fj_dT  );

a1 = (xa*y - xb*y - xc*y + xd*y - x*ya + 2*xc*ya - ...
    xd*ya + x*yb - xc*yb + x*yc - 2*xa*yc + xb*yc - x*yd + xa*yd +...
    sqrt(4*(xb*(-y + ya) + xa*(y - yb) + x*(-ya + yb))*(xd*(ya - yc) +...
    xb*(-ya + yc) + (xa - xc)*(yb - yd)) + (xc*y - xd*y + x*ya +...
    xd*ya - x*yb - xc*yb - x*yc + xb*(y - 2*ya + yc) + x*yd -...
    xa*(y*- 2*yb + yd))^2)) / ...
    (2*(xc*(ya - yb) + xd*(-ya + yb) - (xa - xb)*(yc - yd)));
 
 b1 =-(-xa*y + xb*y + xc*y - xd*y + x*ya - 2*xb*ya + ...
    xd*ya - x*yb + 2*xa*yb - xc*yb - x*yc + xb*yc + x*yd - xa*yd + ...
    sqrt (4*(xb*(-y + ya) + xa*(y - yb) + x*(-ya + yb))*(xd*(ya - yc) + ...
    xb*(-ya + yc) + (xa - xc)*(yb - yd)) + (xc*y - xd*y + x*ya + ...
    xd*ya - x*yb - xc*yb - x*yc + xb*(y - 2*ya + yc) + x*yd - ...
    xa*(y - 2*yb + yd))^2)) / ...
    (2*(xb*(ya - yc) + xd*(-ya + yc) - (xa - xc)*(yb - yd)));
 
% disp('a1 b1:'); disp([a1 b1]);
 
a2 = -(-xa*y + xb*y + xc*y - xd*y + x*ya - 2*xc*ya + ...
    xd*ya - x*yb + xc*yb - x*yc + 2*xa*yc - xb*yc + x*yd - xa*yd + ...
    sqrt (4*(xb*(-y + ya) + xa*(y - yb) + x*(-ya + yb))*(xd*(ya - yc) +...
    xb*(-ya + yc) + (xa - xc)*(yb - yd)) + (xc*y - xd*y + x*ya + ...
    xd*ya - x*yb - xc*yb - x*yc + xb*(y - 2*ya + yc) + x*yd - ...
    xa*(y - 2*yb + yd))^2)) / ...
    (2*(xc*(ya - yb) + xd*(-ya + yb) - (xa - xb)*(yc - yd)));
 
 b2 =(xa*y - xb*y - xc*y + xd*y - x*ya + 2*xb*ya - ...
     xd*ya + x*yb - 2*xa*yb + xc*yb + x*yc - xb*yc - x*yd + xa*yd + ...
     sqrt (4*(xb*(-y + ya) + xa*(y - yb) + x*(-ya + yb))*(xd*(ya - yc) +...
     xb*(-ya + yc) + (xa - xc)*(yb - yd)) + (xc*y - xd*y + x*ya + ...
     xd*ya - x*yb - xc*yb - x*yc + xb*(y - 2*ya + yc) + x*yd - ...
     xa*(y - 2*yb + yd))^2)) / ...
     (2*(xb*(ya - yc) + xd*(-ya + yc) - (xa - xc)*(yb - yd)));

% disp('a2 b2:'); disp([a2 b2]);

if ( (a1 >= 0.) & (a1 <= 1.) & (b1 >= 0.) & (b1 <= 1.))
    fi = fi_dT - 1. + a1;
    fj = fj_dT - 1. + b1;
    
elseif ((a2 >= 0.) & (a2 <= 1.) & (b2 >= 0.) & (b2 <= 1.))
    
    fi = fi_dT - 1. + a2;
    fj = fj_dT - 1. + b2;
    
else
    stop
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
    %zz=flipud(zz);

  else 
    %% OPA/NEMO
    zz=zz3d;
  end
  
  [kmt]=size(zz,1);
  
  [min_depth_diff, ind_min_depth_diff]=min(abs(zz(:)-depth_init));
  
  kk=ind_min_depth_diff;
 
  if ( (zz(kk) < depth_init) && (kk ~= 1)) 
    fk=double(kk)-(depth_init-zz(kk))/(zz(kk-1)-zz(kk));
  elseif  ( (zz(kk) > depth_init) && (kk ~= kmt))
    fk=double(kk)+(depth_init-zz(kk))/(zz(kk+1)-zz(kk));
  else
    fk=double(kk);
  end

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %% Question: record or not data %%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
