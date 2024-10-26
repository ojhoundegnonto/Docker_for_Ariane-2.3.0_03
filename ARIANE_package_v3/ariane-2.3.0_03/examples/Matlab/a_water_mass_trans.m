%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Plot the water mass transformation in Temperature and salinity %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% N. Grima July 2007 %%
%%%%%%%%%%%%%%%%%%%%%%%%

addpath(genpath(fullfile(pwd,'bg_routines')));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Load data if necessary %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if ~exist('final_temp') || ...
   ~exist('final_salt') || ...
   ~exist('init_temp' ) || ...
   ~exist('init_salt' )
  caca;
  ncload('ariane_positions_quantitative.nc');
end

%%%%%%%%%%%%%%%%%%%%%%%%%
%% Number of particles %%
%%%%%%%%%%%%%%%%%%%%%%%%%
nb_part=size(init_temp,1);

%%%%%%%%%%%%%%%%%%%%%%%
%% Open a new figure %%
%%%%%%%%%%%%%%%%%%%%%%%
fid1=figure;
nb_x=2;
nb_y=1;

%%%%%%%%%%%%%%%%%%%%
%% Number of bins %%
%%%%%%%%%%%%%%%%%%%%
if ~exist('a_bins')
  a_bins=100;
end

%%%%%%%%%%%%%%%
%% Transport %%
%%%%%%%%%%%%%%%
%% We assume that init and final transport are equal 
%% (This is the true in Ariane) %%
max_init_transp = max(max(init_transp(:)));
pond= 1./max_init_transp;

%%%%%%%%%%%%%%%%%
%% TEMPERATURE %%
%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Min and max in temperature %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
min_init_temp=min(init_temp);
min_final_temp=min(final_temp);
min_temp=min(min_init_temp,min_final_temp);

max_init_temp=max(init_temp);
max_final_temp=max(final_temp);
max_temp=max(max_init_temp,max_final_temp);

delta_temp= (max_temp - min_temp)/(a_bins -1);

x_hist_temp=zeros(1,a_bins);
x_hist_temp(a_bins)=max_temp - (delta_temp/2.);
x_diff_temp=zeros(1,a_bins);
x_diff_temp(a_bins)=max_temp;
for is = a_bins-1:-1:1
  x_hist_temp(is) = x_hist_temp(is+1) - delta_temp;
  x_diff_temp(is) = x_diff_temp(is+1) - delta_temp;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Compute histograms for temperature %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cumul_init_temp=zeros(1,a_bins);
cumul_final_temp=zeros(1,a_bins);

for is = 1:nb_part
  ind_cumul_init=round((init_temp(is)-min_temp) / delta_temp) + 1;
  cumul_init_temp(1,ind_cumul_init) = cumul_init_temp(1,ind_cumul_init) + (init_transp(is) * pond);

  ind_cumul_final=round((final_temp(is)-min_temp) / delta_temp) + 1;
  cumul_final_temp(1,ind_cumul_final) = cumul_final_temp(1,ind_cumul_final) + (final_transp(is) * pond);
end

diff_cumul_temp  = cumul_final_temp - cumul_init_temp;

diff_cumul_temp_pos = diff_cumul_temp;
diff_cumul_temp_pos(find(diff_cumul_temp <= 0.)) = NaN;

diff_cumul_temp_neg = diff_cumul_temp;
diff_cumul_temp_neg(find(diff_cumul_temp >= 0.)) = NaN;

%%%%%%%%%%%%%%%%%%%%%%
%% plot temperature %%
%%%%%%%%%%%%%%%%%%%%%%
s1=subplot(nb_y,nb_x,[1]);
hold on;
bar(x_diff_temp, diff_cumul_temp_pos,'m','EdgeColor',[1 0 1]);
bar(x_diff_temp, diff_cumul_temp_neg,'g','EdgeColor',[0 1 0]);
stairs(x_hist_temp,-cumul_init_temp,'b');
stairs(x_hist_temp,cumul_final_temp,'r');

set(gca,'Xlim',[min_temp max_temp]);

title({['Histogram \rm(bin =' int2str(a_bins) ')']},'FontWeight','bold','FontSize',14);
xlabel('Binned Temperature \rm(\circC)','FontWeight','bold','FontSize',12);
ylabel('Number of particles ponderated by transport','FontWeight','bold','FontSize',12);
%l1=legend('initial','final','created','consumed');
grid on;


%%%%%%%%%%
%% SALT %%
%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Min and max in salinity %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
min_init_salt=min(init_salt);
min_final_salt=min(final_salt);
min_salt=min(min_init_salt,min_final_salt);

max_init_salt=max(init_salt);
max_final_salt=max(final_salt);
max_salt=max(max_init_salt,max_final_salt);

delta_salt= (max_salt - min_salt)/(a_bins-1);

x_hist_salt=zeros(1,a_bins);
x_hist_salt(a_bins)=max_salt - (delta_salt/2);
x_diff_salt=zeros(1,a_bins);
x_diff_salt(a_bins)=max_salt;
for is = a_bins-1:-1:1
  x_hist_salt(is) = x_hist_salt(is+1) - delta_salt;
  x_diff_salt(is) = x_diff_salt(is+1) - delta_salt;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Compute histograms for salt %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cumul_init_salt=zeros(1,a_bins);
cumul_final_salt=zeros(1,a_bins);

for is = 1:nb_part
  ind_cumul_init=round((init_salt(is)-min_salt) / delta_salt) + 1;
  cumul_init_salt(1,ind_cumul_init) = cumul_init_salt(1,ind_cumul_init) + (init_transp(is) * pond);

  ind_cumul_final=round((final_salt(is)-min_salt) / delta_salt) + 1;
  cumul_final_salt(1,ind_cumul_final) = cumul_final_salt(1,ind_cumul_final) + (final_transp(is) * pond);
end

diff_cumul_salt  = cumul_final_salt - cumul_init_salt;

diff_cumul_salt_pos = diff_cumul_salt;
diff_cumul_salt_pos(find(diff_cumul_salt <= 0.)) = NaN;

diff_cumul_salt_neg = diff_cumul_salt;
diff_cumul_salt_neg(find(diff_cumul_salt >= 0.)) = NaN;

%%%%%%%%%%%%%%%%%%%
%% plot salinity %%
%%%%%%%%%%%%%%%%%%%
s1=subplot(nb_y,nb_x,[2]);
hold on;
bar(x_diff_salt, diff_cumul_salt_pos,'m','EdgeColor',[1 0 1]);
bar(x_diff_salt, diff_cumul_salt_neg,'g','EdgeColor',[0 1 0]);
stairs(x_hist_salt,-cumul_init_salt,'b');
stairs(x_hist_salt,cumul_final_salt,'r');

set(gca,'Xlim',[min_salt max_salt]);

title({['Histogram \rm(bin =' int2str(a_bins) ')']},'FontWeight','bold','FontSize',14);
xlabel('Binned Salinity \rm(psu)','FontWeight','bold','FontSize',12);
ylabel('Number of particles ponderated by transport','FontWeight','bold','FontSize',12);
%l2=legend('init. salt','final salt','created','consumed');

grid on;

print -dtiff histograms_TS.tif;
