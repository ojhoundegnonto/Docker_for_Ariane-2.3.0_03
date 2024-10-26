%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Plot init and final Temperature salinity diagrams %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% You can change number of the bins by a_bins %%
%% ex.:                                        %%
%%   >> a_bins=50.;                            %%
%%   >> a_diag_ts;                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% N. Grima 2007 %%
%%%%%%%%%%%%%%%%%%%

addpath(genpath(fullfile(pwd,'bg_routines')));


if ~exist('init_temp' ) || ...
   ~exist('init_salt' ) || ...
   ~exist('final_temp') || ...
   ~exist('final_salt')
  caca;
  ncload('ariane_positions_quantitative.nc');
end

%%%%%%%%%%%%%%%%%%%%%%%
%% Open a new figure %%
%%%%%%%%%%%%%%%%%%%%%%%
fid1=figure;
nb_x=2;
nb_y=1;

%%%%%%%%%%%%
%% FIGURE %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Initial temperature and salinity diagram %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
s1=subplot(nb_y,nb_x,[1]);
plot(init_salt,init_temp,'.','Color',[0.8 0.8 0.8]);
h1=gca;
xlim1=get(h1, 'XLim');
ylim1=get(h1, 'YLim');

title('Initial positions','FontWeight','bold','FontSize',14);
xlabel('Salinity \rm(psu)','FontWeight','bold','FontSize',12);
ylabel('Temperature \rm(\circC)','FontWeight','bold','FontSize',12);
grid on;

%%%%%%%%%%%%
%% FIGURE %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Final temperature and salinity diagram %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
s2=subplot(nb_y,nb_x,[2]);
plot(final_salt,final_temp,'.','Color',[0.8 0.8 0.8]);
h2=gca;
xlim2=get(h2, 'XLim');
ylim2=get(h2, 'YLim');

title('Final positions','FontWeight','bold','FontSize',14);
xlabel('Salinity \rm(psu)','FontWeight','bold','FontSize',12);
ylabel('Temperature \rm(\circC)','FontWeight','bold','FontSize',12);
grid on;

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% To have the same axes %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
xlim=xlim1;
xlim(1)=min(xlim1(1),xlim2(1));
xlim(2)=max(xlim1(2),xlim2(2));

ylim=ylim1;
ylim(1)=min(ylim1(1),ylim2(1));
ylim(2)=max(ylim1(2),ylim2(2));

set(h1,'Xlim',xlim,'Ylim',ylim);
set(h2,'Xlim',xlim,'Ylim',ylim);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if ~exist('a_bins'),
  a_bins=100.;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% delta in T and delta in S %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delta_t=(ylim(2)-ylim(1))/a_bins;
delta_s=(xlim(2)-xlim(1))/a_bins;
area_ts=delta_t * delta_s;

%% Initialize 2 2D arrays
init_2Dar=zeros(a_bins,a_bins);
final_2Dar=zeros(a_bins,a_bins);

%%%%%%%%%%%%%%%%%%%%%%%%
%% Fill the 2D arrays %%
%%%%%%%%%%%%%%%%%%%%%%%%
max_init_transp =max(init_transp(:));
max_final_transp=max(final_transp(:));

nb_part=size(init_temp,1);

for is = 1:nb_part
  ind_t = round((init_temp(is)-ylim(1)) / delta_t) + 1;
  ind_s = round((init_salt(is)-xlim(1)) / delta_s) + 1;
  init_2Dar(ind_s,ind_t) = init_2Dar(ind_s,ind_t) + ...
                           ((init_transp(is)/max_init_transp) * 100.);

  ind_t = round((final_temp(is)-ylim(1)) / delta_t) + 1;
  ind_s = round((final_salt(is)-xlim(1)) / delta_s) + 1;
  final_2Dar(ind_s,ind_t) = final_2Dar(ind_s,ind_t) + ...
                            ((final_transp(is)/max_final_transp) * 100.);
end

%% Results are ponderated by the surface of each cells (area_ts);
init_2Dar(:,:) = init_2Dar(:,:) / area_ts;
final_2Dar(:,:) = final_2Dar(:,:) / area_ts;


%% Max of concentration
max_init=max(max(init_2Dar));
max_final=max(max(final_2Dar));

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Axes of the 2D arrays %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
x_2Daxe=zeros(a_bins,a_bins);
y_2Daxe=zeros(a_bins,a_bins);

for is=1:a_bins;
  for js=1:a_bins;
    x_2Daxe(is,js)= xlim(1) + (is-1)*delta_s;
    y_2Daxe(is,js)= ylim(1) + (js-1)*delta_t;
  end
end

%%%%%%%%%%%%%%%%%%%%%%
%% Replace 0 by NaN %%
%%%%%%%%%%%%%%%%%%%%%%
%init_2Dar(find(init_2Dar   == 0))=NaN;
%final_2Dar(find(final_2Dar == 0))=NaN;

%%%%%%%%%%%%%%%%%%%%%%%%
%% Inverse hot colors %%
%%%%%%%%%%%%%%%%%%%%%%%%
hot_=hot;
invhot=hot;
for is = 1:size(hot_,1);
  invhot(size(hot_,1)-is+1,1)=hot_(is,1);
  invhot(size(hot_,1)-is+1,2)=hot_(is,2);
  invhot(size(hot_,1)-is+1,3)=hot_(is,3);
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%
%% FIGURE %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Initial temperature and salinity diagram %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sub1=subplot(nb_y,nb_x,[1]);
hold on;
[Xi,Yi]=meshgrid(x_2Daxe(:,1),y_2Daxe(1,:));
zi=interp2(Xi,Yi,init_2Dar,Xi,Yi,'nearest');
contour(x_2Daxe,y_2Daxe,zi,'LineWidth',2);
h1=gca;

title({'Initial positions'},'FontWeight','bold','FontSize',14);

xlabel('Salinity \rm(psu)','FontWeight','bold','FontSize',12);
ylabel('Temperature \rm(\circC)','FontWeight','bold','FontSize',12);
grid on;
colormap(invhot);

%%%%%%%%%%%%
%% FIGURE %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Final temperature and salinity diagram %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sub2=subplot(nb_y,nb_x,[2]);
hold on;
zf=interp2(Xi,Yi,final_2Dar,Xi,Yi,'nearest');
contour(x_2Daxe,y_2Daxe,zf,'LineWidth',2);
h2=gca;

title({'Final positions'},'FontWeight','bold','FontSize',14);
xlabel('Salinity \rm(psu)','FontWeight','bold','FontSize',12);
ylabel('Temperature \rm(\circC)','FontWeight','bold','FontSize',12);
grid on;
colormap(invhot);
set(h2,'Xlim',xlim,'Ylim',ylim);

%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%
%% s5=subplot(nb_y,nb_x,[7 8]);
%% set(axes,'Visible','off');
%% hc=colorbar('north');

if exist('a_figname'),
   print -dtiff a_figname;
   clear a_figname;
else
   print -dtiff diag_TS.tif;
end
