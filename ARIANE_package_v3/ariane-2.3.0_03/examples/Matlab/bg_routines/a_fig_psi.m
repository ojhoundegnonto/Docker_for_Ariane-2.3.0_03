%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% You can set "a_cstep" to specify the LevelStep contour %%
%% You can set "a_title" to add a comment in the title    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if ~exist('psi')
  a_xy0;
end

%%colormap('hot');

figid=figure;
hold on;

%% Initialize the map projection. 
%% This first step is needed to use m_map routines.
a_projection

if exist('a_cstep')
  [c,h]=m_contour(xp_reg,yp_reg,psi,...
            'showtext','on','LineWidth',1, ...
            'LevelStep', a_cstep);
else
  [c,h]=m_contour(xp_reg,yp_reg,psi,...
            'showtext','on','LineWidth',1);
end

%% Print sections
for is = 1:1
  if ((i1_reg(is) > 0) && (j1_reg(is) > 0) && ...
     (i2_reg(is) > 0) && (j2_reg(is) > 0))
    m_line([xt_reg(i1_reg(is),j1_reg(is)) xt_reg(i2_reg(is),j2_reg(is))], ...
           [yt_reg(i1_reg(is),j1_reg(is)) yt_reg(i2_reg(is),j2_reg(is))], ...
           'Color','g','LineWidth',2)
  end
end
for is = 2:nb_sec
  if ((i1_reg(is) > 0) && (j1_reg(is) > 0) && ...
     (i2_reg(is) > 0) && (j2_reg(is) > 0))
    m_line([xt_reg(i1_reg(is),j1_reg(is)) xt_reg(i2_reg(is),j2_reg(is))], ...
           [yt_reg(i1_reg(is),j1_reg(is)) yt_reg(i2_reg(is),j2_reg(is))], ...
           'Color','r','LineWidth',2)
  end
end

%% print land mask
a_mask_land;

%% Print psi ref = 0.
m_text(xref_psi,yref_psi,'psi=0');

%% Title and axe labels
if exist('a_title')
  title({'PSI \rm(in Sv)',[a_title]},...
  'FontWeight','bold','FontSize',14);
else
  title(['PSI \rm(in Sv) - ' segname(pind(i_loop),:)],...
         'FontWeight','bold','FontSize',14);
end
xlabel('longitudes','FontWeight','bold','FontSize',12);
ylabel('latitudes','FontWeight','bold','FontSize',12);

fig_fn=strcat('psi_',num2str(i_loop),'.tif')
print('-dtiff',fig_fn)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Nicolas.Grima@univ-brest.fr
%% 2007-2008

