%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% You can set "a_cstep" to specify the LevelStep contour %%
%% You can set "a_title" to add a comment in the title    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if ~exist('psi')
  a_yz0;
end

%%colormap('hot');

figid=figure;
hold on;

[X,Y]=meshgrid(yt_reg(1,:),zt);

mask_psi=mp;
mask_psi(find(mp==1))=NaN;
pcolor(X,Y,mask_psi'); shading flat;

if exist('a_cstep')
  [c,h]=contour(X,Y,psi',...
         'showtext','on','LineWidth',1, ...
         'LevelStep', a_cstep);
else
  [c,h]=contour(X,Y,psi',...
         'showtext','on','LineWidth',1);
end

%colorbar(c)
axis ij
grid;

%% Title and axe labels
if exist('a_title')
  title({'PSI \rm(in Sv)',[a_title]},...
  'FontWeight','bold','FontSize',14);
else
  title(['PSI \rm(in Sv)'],...
         'FontWeight','bold','FontSize',14);
end
xlabel('latitudes','FontWeight','bold','FontSize',12);
ylabel('Depth [m]','FontWeight','bold','FontSize',12);

fig_fn=strcat('psi_yz',num2str(i_loop),'.tif')
print('-dtiff',fig_fn)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Nicolas.Grima@univ-brest.fr
%% 2019

