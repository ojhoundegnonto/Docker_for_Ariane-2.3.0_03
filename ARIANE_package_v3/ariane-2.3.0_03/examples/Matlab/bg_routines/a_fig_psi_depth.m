%You can set "min_transport" 

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

figure;
hold on;

a_projection;

%% Fill salinity field
max_depth=-100.;
min_depth= 100.;


%%%%%%%%%
%-- V --%
%%%%%%%%%
display('  - patch depth_v')

sq_xy_vh_msk(sq_xy_vh_msk < min_transport) = NaN;

depth_v=zeros(size(sq_xy_zvh_msk,1), size(sq_xy_zvh_msk,2));
depth_v(1:end-1,:)=sq_xy_zvh_msk(2:end,:)./sq_xy_vh_msk(2:end,:);
tt=depth_v(1:end-1,1:end-1)';
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

display('  - patch depth_u')

sq_xy_uh_msk(sq_xy_uh_msk < min_transport) = NaN;

depth_u=zeros(size(sq_xy_zuh_msk,1), size(sq_xy_zuh_msk,2));
depth_u(:,1:end-1)=sq_xy_zuh_msk(:,2:end)./sq_xy_uh_msk(:,2:end);
tt=depth_u(1:end-1,1:end-1)';
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
min_depthu = min(uu(~isnan(uu)));
min_depthv = min(vv(~isnan(vv)));
min_depth  = min([ min_depthu min_depthv]);

max_depthu = max(uu(~isnan(uu)));
max_depthv = max(vv(~isnan(vv)));
max_depth  = max([ max_depthu max_depthv]);

caxis([min_depth max_depth]);
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

  [cn,hn]=m_contour(xp_reg,yp_reg,squeeze(psi(:,:)),levels_neg,'Visible', 'on',...
		    'showtext','on','LineWidth',1.2,'LineStyle','--','LineColor','k');

  [cp,hp]=m_contour(xp_reg,yp_reg,squeeze(psi(:,:)),levels_pos,'Visible', 'on',...
		    'showtext','on','LineWidth',1.2,'LineColor','k');

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
  title({'PSI \rm(in Sv) and Depth',[a_title]},...
	'FontWeight','bold','FontSize',14);
else
  %%     title('PSI \rm(in Sv) and Depth','FontWeight','bold','FontSize',14);
end
xlabel('longitudes','FontWeight','bold','FontSize',12);
ylabel('latitudes','FontWeight','bold','FontSize',12);

%% Save fig in jpeg format
%% print -dtiff  psi_depth.tiff
%% print -djpeg psi_depth.jpeg

fig_fn=strcat('psi_depth_',num2str(i_loop),'.tif')
print('-dtiff',fig_fn)

%%  fig_fn=strcat('psi_depth_',num2str(i_loop),'.jpeg')
%%  print('-djpeg',fig_fn)


