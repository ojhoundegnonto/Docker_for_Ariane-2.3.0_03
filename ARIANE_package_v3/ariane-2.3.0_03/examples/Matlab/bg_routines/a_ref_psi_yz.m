fig=figure;
hold on

%% Initialize the map projection. 
%% This first step is needed to use m_map routines.
a_projection;


title('PLEASE CHOOSE REFERENCE (zero) FOR PSI...',...
      'FontWeight','bold','FontSize',12);

xlabel1='LEFT click: position    RIGHT click: final choice';
set(get(gca,'xlabel'),'string',xlabel1,...
      'FontWeight','bold','FontSize',12);

for k=2:kmt_reg
 for j=1:jmt_reg-1
  if tmask2(j,k)+tmask2(j+1,k) == 1 
     m_line([yp_reg(j,k-1) yp_reg(j,k)],[zp_reg(j,k-1) zp_reg(j,k)],...
            'Color','k','LineWidth',1)
  end
 end
end

 for k=2:kmt_reg
  if tmask2(1,k)+tmask2(2,k) == 1
    m_line([yp_reg(1,k-1);yp_reg(1,k)],[zp_reg(1,k-1);zp_reg(1,k)],...
	   'Color','k','LineWidth',1)
  end
 end

for k=1:kmt_reg-1
 for j=2:jmt_reg
  if tmask2(j,k)+tmask2(j,k+1) == 1
     m_line([yp_reg(j-1,k);yp_reg(j,k)],[zp_reg(j-1,k);zp_reg(j,k)],...
             'Color','k','LineWidth',1)
  end
 end
end


%% sections %%
for k=1:kmt_reg
 for j=1:jmt_reg
  if mtseg(j,k) == 1
   m_line([yt_reg(j,k);yt_reg(j,k)],[zt_reg(j,k);zt_reg(j,k)],...
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
