%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Time dependant plots of the transport at the initial section.%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% N. Grima July 2007 %%
%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Load data if necessary %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if ~exist('init_transp') || ...
   ~exist('init_x     ') || ...
   ~exist('init_y     ') || ...
   ~exist('init_z     ') || ...
   ~exist('init_t'     ) || 
  caca;
  ncload('ariane_positions_quantitative.nc');
end

%%%%%%%%%%%%%%%%%%%%%%%
%% Open a new figure %%
%%%%%%%%%%%%%%%%%%%%%%%
fid1=figure;

%%%%%%
%% R14: Scatter command is a little bit to long...
%% scatter3(init_x,init_y, init_z,init_transp);
%%%%%

%%%%%%%%%%%%%%%%%%%%%
%% Colorbar + size %%
cb=hot;
sz_cb=size(cb,1);

%%%%%%%%%%%%%%%%%%%
%% Transport max %%
max_transp = max(max(init_transp));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Delta transport for representation %%
delta_transp = max_transp / sz_cb;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Build a scale transport array %%
scale_transp=zeros(sz_cb);

for is=0:sz_cd-1;
  scale_transp(is) = is * delta_transp;
end

%%%%%
%% 
