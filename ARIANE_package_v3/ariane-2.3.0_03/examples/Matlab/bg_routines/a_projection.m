%%%%%%%%%%%%%%%%%%%%%%
%% M_map projection %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Nicolas.Grima@univ-brest.fr 2008 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if ~exist('projection')
  projection='mercator';
end

if strcmp(projection,'mercator')
  m_proj(projection, ...
         'longitudes',[min_lont_reg_inc max_lonp_reg_inc], ...
         'latitudes' ,[min_latt_reg_inc max_latp_reg_inc]);

elseif strcmp(projection,'stereographic')
  m_proj(projection,'lat',max_latp_reg_inc,'long',0,'radius',25);

elseif strcmp(projection,'lamber')
  m_proj(projection,...
	 'longitudes',[min_lont_reg_inc max_lonp_reg_inc],...
	 'latitudes', [min_latt_reg_inc max_latp_reg_inc]);

elseif strcmp(projection,'oblique')
  m_proj(projection,...
	 'latitudes' ,[min_latt_reg_inc max_latp_reg_inc],...
	 'longitudes',[min_lont_reg_inc max_lonp_reg_inc],...
	 'aspect',.8);

elseif strcmp(projection,'miller')
  m_proj(projection,'lat',max_latp_reg_inc);

elseif strcmp(projection,'UMT')
  m_proj(projection,...
	 'latitudes' ,[min_latt_reg_inc max_latp_reg_inc],...
	 'longitudes',[min_lont_reg_inc max_lonp_reg_inc]);

elseif strcmp(projection,'hammer')
  m_proj(projection,'clong',max_lonp_reg_inc);

%% elseif strcmp(projection,'')

else
  disp(' ');
  disp([' The map projection is not correct: ', projection]);
  disp('Please choose a corect projection before to resubmit your routine');
  disp('  - projection=mercator');
  disp('  - projection=stereographic');
  disp('  - projection=lamber');
  disp('  - projection=oblique');
  disp('  - projection=miller');
  disp('  - projection=UMT');
  disp('  - projection=hammer');
  disp(' ');
  stop;
end

flag_projection=1;
