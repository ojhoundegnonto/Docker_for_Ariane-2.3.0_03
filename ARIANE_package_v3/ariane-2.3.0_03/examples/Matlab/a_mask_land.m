%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Plot the land mask on a map %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% N. Grima 2007 %%
%%%%%%%%%%%%%%%%%%%

addpath(genpath(fullfile(pwd,'bg_routines')));

if ~exist('xt_reg')
    a_ncreadgrid;
end

if ~exist('flag_projection')
    a_projection;
end

if exist('map_HR')
    
    if ~exist('land_mask.mat','file') & ~exist('a_no_save_mask')
        disp(' ');
        disp('Is creating a high resolution land mask... please wait');
        disp('(If it is to long please submit again with a_no_save_mask=1)');
        m_gshhs_f('save','land_mask');
    end
    
    if exist('land_mask.mat','file') & ~exist('a_no_save_mask')
        m_usercoast('land_mask.mat','patch',[0.7 0.7 0.7],...
            'edgecolor','k');
        m_grid('box','fancy');
    else
        m_coast('patch',[0.7 0.7 0.7],'edgecolor','k');
        m_grid('box','fancy');
    end
    
else
    disp(' ');
    disp('Tmask for land mask, use map_HR=1 to have High Resolution map');
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
        for j=2:jmt_reg,
            if tmask_reg(1,j)+tmask_reg(2,j) == 1
                m_line([xp_reg(1,j-1);xp_reg(1,j)],[yp_reg(1,j-1);yp_reg(1,j)],...
                    'Color','k','LineWidth',1)
            end
        end
    end
    
    for j=1:jmt_reg-1,
        for i=2:imt_reg,
            if tmask_reg(i,j)+tmask_reg(i,j+1) == 1
                if abs(xp_reg(i-1,j)-xp_reg(i,j)) <= 180.
                    m_line([xp_reg(i-1,j);xp_reg(i,j)],[yp_reg(i-1,j);yp_reg(i,j)],...
                        'Color','k','LineWidth',1)
                end
            end
        end
    end
    
    
    
end

