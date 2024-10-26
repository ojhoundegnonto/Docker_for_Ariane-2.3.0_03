if ~exist('tmask_reg')
  a_ncreadgrid;
end

fig=figure;
hold on

%% Initialize the map projection. 
%% This first step is needed to use m_map routines.
a_projection;

%%[cc hh]=contour(xt_reg,yt_reg,tmask_reg);
%%set(hh(:,:),'visible','off')


title('PLEASE CHOOSE REFERENCE (zero) FOR PSI...',...
      'FontWeight','bold','FontSize',12);

xlabel1='LEFT click: position    RIGHT click: final choice';
set(get(gca,'xlabel'),'string',xlabel1,...
      'FontWeight','bold','FontSize',12);

for j=2:jmt_reg,
 for i=1:imt_reg-1,
  if tmask_reg(i,j)+tmask_reg(i+1,j) == 1 
   if abs(xp_reg(i,j-1)-xp_reg(i,j)) <= 180.
     m_line([xp_reg(i,j-1) xp_reg(i,j)],[yp_reg(i,j-1) yp_reg(i,j)],...
            'Color','k','LineWidth',1)
   end
  end
 end
end

if iperio == 0
 for j=2:jmt_reg
  if tmask_reg(1,j)+tmask_reg(2,j) == 1
    m_line([xp_reg(1,j-1);xp_reg(1,j)],[yp_reg(1,j-1);yp_reg(1,j)],...
	   'Color','k','LineWidth',1)
  end
 end
end

for j=1:jmt_reg-1
 for i=2:imt_reg
  if tmask_reg(i,j)+tmask_reg(i,j+1) == 1
   if abs(xp_reg(i-1,j)-xp_reg(i,j)) <= 180.
     m_line([xp_reg(i-1,j);xp_reg(i,j)],[yp_reg(i-1,j);yp_reg(i,j)],...
             'Color','k','LineWidth',1)
   end
  end
 end
end

%%axis([min_lont_reg max_lont_reg min_latt_reg max_latt_reg]);
%%caxe=axis;
% lignes style _._._
%%m_line([caxe(1),caxe(2)],[0,0],'Color','r')
%%for lon=-360:180:360,
%%  m_line([lon,lon],[caxe(3),caxe(4)],'Color','r')
%%end

%% sections %%
for j=1:jmt_reg
 for i=1:imt_reg
  if mtseg(i,j) == 1
   m_line([xt_reg(i,j);xt_reg(i,j)],[yt_reg(i,j);yt_reg(i,j)],...
	  'Color','r','Marker','*');
  end
 end
end

m_grid('fancy');

%% Select a reference psi point (why ???)
pb=1;
while pb == 1
 [m_xref_psi,m_yref_psi,mbutton] = ginput(1);
 [xref_psi,yref_psi]=m_xy2ll(m_xref_psi,m_yref_psi);
 dist = (xp_reg-xref_psi).^2 + (yp_reg-yref_psi).^2;
 [dmin,jref_psi]=min(min(dist));
 [dmin,iref_psi]=min(min(dist'));
 disp(sprintf('lon: %4.1f lat: %4.1f - i:%d j:%d',...
               xref_psi,yref_psi,iref_psi,jref_psi));

 if mbutton == 3
  pb = 0;
%  if tmask_reg(iref_psi,jref_psi) == 0
%    pb = 0;
%    disp('All rigth this is a Land point...!');
%  else
%    disp('Ocean Point... Please select a Land point for psi=0.!');
%  end

 end

end

close(fig);
