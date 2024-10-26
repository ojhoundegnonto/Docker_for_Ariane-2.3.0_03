%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% You can set "a_cstep" to specify the LevelStep contour %%
%% You can set "a_title" to add a comment in the title    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

disp(' ');
disp('---');
disp('--- Please wait, it could take a long time to plot ---');
disp('---');
disp(' ');

if ~exist('psi')
  a_xy0;
end

%% Minimum transport
if ~exist('min_transport')
   min_transport=0.;
end

%% change dimension SV in m3/s
if (min_transport < 100.)
  min_transport = min_transport * 1.e6;
end

%%colormap('hot');


figid=figure;
hold on;

%% Initialize the map projection. 
%% This first step is needed to use m_map routines.
a_projection;

%% Fill salinity field
max_salt=0.;
min_salt=100.;

%%%%%%%%%
%-- V --%
%%%%%%%%%
display('  - patch salt_v')
    
sq_xy_vh_msk(sq_xy_vh_msk < min_transport) = NaN;
  
salt_v=zeros(size(sq_xy_svh_msk,1), size(sq_xy_svh_msk,2));
salt_v(1:end-1,:)=sq_xy_svh_msk(2:end,:)./sq_xy_vh_msk(2:end,:);
tt=salt_v(1:end-1,1:end-1)';
vv=tt(:);
  
Xpt=zeros(4,(imt_reg-1)*(jmt_reg-1));
Ypt=zeros(4,(imt_reg-1)*(jmt_reg-1));
  
ict=1;

for ix = 1:imt_reg-1
  for jy = 1:jmt_reg-1
    
    Xpt(:,ict)=[xp_reg_msk(ix,jy) xt_reg_msk(ix+1,jy+1) ...
		xp_reg_msk(ix+1,jy) xt_reg_msk(ix+1,jy)];
    Ypt(:,ict)=[yp_reg_msk(ix,jy) yt_reg_msk(ix+1,jy+1) ...
		yp_reg_msk(ix+1,jy) yt_reg_msk(ix+1,jy)];
    
    ict=ict+1;
  end
  
end

[m_X,m_Y]=m_ll2xy(Xpt,Ypt);

patch(m_X,m_Y,vv');shading flat;

%%%%%%%%%
%-- U --%
%%%%%%%%%
display('  - patch salt_u')

sq_xy_uh_msk(sq_xy_uh_msk < min_transport) = NaN;

salt_u=zeros(size(sq_xy_suh_msk,1), size(sq_xy_suh_msk,2));
salt_u(:,1:end-1)=sq_xy_suh_msk(:,2:end)./sq_xy_uh_msk(:,2:end);
tt=salt_u(1:end-1,1:end-1)';
uu=tt(:);

Xpt=zeros(4,(imt_reg-1)*(jmt_reg-1));
Ypt=zeros(4,(imt_reg-1)*(jmt_reg-1));

ict=1;

for ix = 1:imt_reg-1
  for jy = 1:jmt_reg-1

    Xpt(:,ict)=[xt_reg_msk(ix,jy+1) xp_reg_msk(ix,jy+1) ...
		xt_reg_msk(ix+1,jy+1) xp_reg_msk(ix,jy)];

    Ypt(:,ict)=[yt_reg_msk(ix,jy+1) yp_reg_msk(ix,jy+1) ...
		yt_reg_msk(ix+1,jy+1) yp_reg_msk(ix,jy)];
    
    ict=ict+1;
    
  end
end

hold on;
[m_X,m_Y]=m_ll2xy(Xpt,Ypt);

patch(m_X,m_Y,uu');shading flat;


%% Set the color bar
min_saltu = min(uu(~isnan(uu)));
min_saltv = min(vv(~isnan(vv)));
min_salt  = min([ min_saltu min_saltv]);

max_saltu = max(uu(~isnan(uu)));
max_saltv = max(vv(~isnan(vv)));
max_salt  = max([ max_saltu max_saltv]);

caxis([min_salt max_salt]);
colorbar;

%% Contour psi (black contour, dashed lines for negative values and solid 
%%	       lines for positive values).
if exist('a_cstep')
  [c,h]=m_contour(xp_reg,yp_reg,squeeze(psi(:,:)),...
		  'showtext','on','LineWidth',1.2, ...
		  'LevelStep', a_cstep);
else
  [c,h]=m_contour(xp_reg,yp_reg,squeeze(psi(:,:)),'Visible', 'off',...
		  'LineColor','w' );

  levels = get(h,'LevelList');

  levels_pos=levels(find(levels >= 0.));
  levels_neg=levels(find(levels < 0.));

  if (size(levels_pos) == 1)
    levels_pos=[levels_pos levels_pos];
  end

  if (size(levels_neg) == 1)
    levels_neg=[levels_neg levels_neg];
  end

  [cn,hn]=m_contour(xp_reg,yp_reg,squeeze(psi(:,:)),levels_neg,...
		    'Visible', 'on',...
		    'showtext','on',...
		    'LineWidth',1.2,...
		    'LineStyle','--',...
		    'LineColor','k');

  [cp,hp]=m_contour(xp_reg,yp_reg,squeeze(psi(:,:)),levels_pos,...
		    'Visible', 'on',...
		    'showtext','on',...
		    'LineWidth',1.2,...
		    'LineColor','k');

end

%% Print sections
for is = 1:nb_sec,
  if ((i1_reg(is) > 0) && (j1_reg(is) > 0) && ...
      (i2_reg(is) > 0) && (j2_reg(is) > 0))
    m_line([xp_reg(i1_reg(is),j1_reg(is)) xp_reg(i2_reg(is),j2_reg(is))], ...
           [yp_reg(i1_reg(is),j1_reg(is)) yp_reg(i2_reg(is),j2_reg(is))], ...
           'Color','k','LineWidth',2)
  end
end

%% print land mask
a_mask_land;

%% Print psi ref = 0.
m_text(xref_psi,yref_psi,'psi=0');

%% Title and axe labels
if exist('a_title')
  title({'PSI \rm(in Sv) and Salinity',[a_title]},...
	'FontWeight','bold','FontSize',14);
else
  title('PSI \rm(in Sv) and Salinity','FontWeight','bold','FontSize',14);
end
xlabel('longitudes','FontWeight','bold','FontSize',12);
ylabel('latitudes','FontWeight','bold','FontSize',12);

%% Save fig in jpeg format
%% print -djpeg psi_pmt.jpeg

fig_fn=strcat('psi_salt_',num2str(i_loop),'.tif')
print('-dtiff',fig_fn)

%%  fig_fn=strcat('psi_salt_',num2str(i_loop),'.jpeg')
%%  print('-djpeg',fig_fn)


