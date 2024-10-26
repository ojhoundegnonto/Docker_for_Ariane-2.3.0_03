%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Plot the stream function PSI %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Nicolas.Grima@univ-brest.fr %%
%% April 2008                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Modified by Bruno B. july 2008%%

fprintf('\n\n');
disp('Computing divergence...');

if ~exist('xy_zonal')
  error('Please submit a_xy0 and not a_compute_psi...');
end

%%%%%%%%%%%%%%%%%%%%%%%%
%% Compute Divergence %%
%%%%%%%%%%%%%%%%%%%%%%%%
%Initialize array
div=zeros(size(sq_xy_zonal_msk));
%Compute divergence
for j=2:jmt_reg,
 for i=2:imt_reg,
  div(i,j)=sq_xy_zonal_msk(i,j)-sq_xy_zonal_msk(i-1,j)+ ...
             sq_xy_mer_msk(i,j)-sq_xy_mer_msk(i,j-1);
 end
end


%Divergence max
divmax=max(max(max(abs(div))));

% Special for OPA-ORCA
if iperio == 1
 div(1,:)=div(imt_reg-1,:);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% reperage d un point psi "actif" %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
disp(sprintf('\n'));
disp('Search an active psi point:');
ip0=0;jp0=0;
for j=1:jmt_reg-1,
  for i=1:imt_reg-1,

   if abs(div(  i,  j)) <= divmax && ...
      abs(div(i+1,  j)) <= divmax && ...
      abs(div(  i,j+1)) <= divmax && ...
      abs(div(i+1,j+1)) <= divmax
  
     if ((sq_xy_zonal_msk(i,j) * sq_xy_zonal_msk(i,j+1))~=0. || ...
         (sq_xy_mer_msk(i,j) * sq_xy_mer_msk(i+1,j))~=0.)
         disp(sprintf('%d %d',i,j)); 
       ip0=i;
       jp0=j;
       break;
     end
   end
 end
 if (ip0 ~= 0) && (jp0 ~= 0)
  break
 end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% certains points posent probleme (comme l amerique centrale) %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
disp(sprintf('\n'));
disp('computing diagnonal land connections...');
ipb=zeros(imt_reg,jmt_reg);
for j=1:jmt_reg-1,
  for i=1:imt_reg-1,
    if (tmask_reg(i,j) == tmask_reg(i+1,j+1)) && ...
       (tmask_reg(i,j+1) == tmask_reg(i+1,j))
      if tmask_reg(i,j)+ tmask_reg(i+1,j) == 1
        disp(sprintf('%d %d ',i,j));
        ipb(i,j)=1;
      end
    end
  end
end

% OPA_ORCA
if iperio == 1
 ipb(1,:)     = max(ipb(1,:),ipb(imt_reg-1,:));
 ipb(2,:)     = max(ipb(2,:),ipb(imt_reg,:));
 ipb(imt_reg-1,:) = ipb(1,:);
 ipb(imt_reg,:)   = ipb(2,:);
end

if jperio == 1
 for i=1:imt_reg/2,
  ipb(i,jmt_reg-3)       = max(ipb(i,jmt_reg-3),ipb(imt_reg-i+1,jmt_reg));
  ipb(i,jmt_reg-2)       = max(ipb(i,jmt_reg-2),ipb(imt_reg-i+1,jmt_reg-1));
  ipb(i,jmt_reg-1)       = max(ipb(i,jmt_reg-1),ipb(imt_reg-i+1,jmt_reg-2));
  ipb(i,jmt_reg)         = max(ipb(i,jmt_reg)  ,ipb(imt_reg-i+1,jmt_reg-3));
  ipb(imt_reg-i+1,jmt_reg)   = ipb(i,jmt_reg-3);
  ipb(imt_reg-i+1,jmt_reg-1) = ipb(i,jmt_reg-2);
  ipb(imt_reg-i+1,jmt_reg-2) = ipb(i,jmt_reg-1);
  ipb(imt_reg-i+1,jmt_reg-3) = ipb(i,jmt_reg);
 end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% une fois trouve un point psi actif, construction du masque de psi %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mp=zeros(imt_reg,jmt_reg);
mp(ip0,jp0)=1;

fprintf('\n\n');
disp('computing psi mask...');
mpold=0;
totmp=1;
while totmp > mpold

 mpold=totmp;

  for j=1:jmt_reg-1
    for i=1:imt_reg-1
      if ipb(i,j) == 0 && mp(i,j) == 1
        if mp(i+1,j) == 0 && (tmask2(i+1,j) == 1 || tmask2(i+1,j+1) == 1)
          mp(i+1,j)=1;
        end
        if mp(i,j+1) == 0 && (tmask2(i,j+1) == 1 || tmask2(i+1,j+1) == 1)
          mp(i,j+1)=1;
        end
      end
    end
  end

  for j=2:jmt_reg-1,
    for i=2:imt_reg-1,
      if ipb(i,j) == 0 && mp(i,j) == 1
        if mp(i-1,j) == 0 && (tmask2(i,j) == 1 || tmask2(i,j+1) == 1)
          mp(i-1,j)=1;
        end
        if mp(i,j-1) == 0 && (tmask2(i,j) == 1 || tmask2(i+1,j) == 1)
          mp(i,j-1)=1;
        end
      end
    end
  end

 for j=jmt_reg-1:-1:1,
  for i=1:imt_reg-1,
   if ipb(i,j) == 0 && mp(i,j) == 1
    if mp(i+1,j) == 0 && (tmask2(i+1,j) == 1 || tmask2(i+1,j+1) == 1)
     mp(i+1,j)=1;
    end
    if mp(i,j+1) == 0 && (tmask2(i,j+1) == 1 || tmask2(i+1,j+1) == 1)
     mp(i,j+1)=1;
    end
   end
  end
 end

 for j=jmt_reg-1:-1:2,
  for i=2:imt_reg-1,
   if ipb(i,j) == 0 && mp(i,j) == 1
    if mp(i-1,j) == 0 && (tmask2(i,j) == 1 || tmask2(i,j+1) == 1)
     mp(i-1,j)=1;
    end
    if mp(i,j-1) == 0 && (tmask2(i,j) == 1 || tmask2(i+1,j) == 1)
     mp(i,j-1)=1;
    end
   end
  end
 end

 for j=1:jmt_reg-1,
  for i=imt_reg-1:-1:1,
   if ipb(i,j) == 0 && mp(i,j) == 1
    if mp(i+1,j) == 0 && (tmask2(i+1,j) == 1 || tmask2(i+1,j+1) == 1)
     mp(i+1,j)=1;
    end
    if mp(i,j+1) == 0 && (tmask2(i,j+1) == 1 || tmask2(i+1,j+1) == 1)
     mp(i,j+1)=1;
    end
   end
  end
 end

 for j=2:jmt_reg-1,
  for i=imt_reg-1:-1:2,
   if ipb(i,j) == 0 && mp(i,j) == 1
    if mp(i-1,j) == 0 && (tmask2(i,j) == 1 || tmask2(i,j+1) == 1)
     mp(i-1,j)=1;
    end
    if mp(i,j-1) == 0 && (tmask2(i,j) == 1 || tmask2(i+1,j) == 1)
     mp(i,j-1)=1;
    end
   end
  end
 end

 for j=jmt_reg-1:-1:1,
  for i=imt_reg-1:-1:1,
   if ipb(i,j) == 0 && mp(i,j) == 1
    if mp(i+1,j) == 0 && (tmask2(i+1,j) == 1 || tmask2(i+1,j+1) == 1)
     mp(i+1,j)=1;
    end
    if mp(i,j+1) == 0 && (tmask2(i,j+1) == 1 || tmask2(i+1,j+1) == 1)
     mp(i,j+1)=1;
    end
   end
  end
 end

 for j=jmt_reg-1:-1:2,
  for i=imt_reg-1:-1:2,
   if ipb(i,j) == 0 && mp(i,j) == 1
    if mp(i-1,j) == 0 && (tmask2(i,j) == 1 || tmask2(i,j+1) == 1)
     mp(i-1,j)=1;
    end
    if mp(i,j-1) == 0 && (tmask2(i,j) == 1 || tmask2(i+1,j) == 1)
     mp(i,j-1)=1;
    end
   end
  end
 end

 if iperio == 1
  mp(1,:)         = max(mp(1,:),mp(imt_reg-1,:));
  mp(2,:)         = max(mp(2,:),mp(imt_reg,:));
  mp(imt_reg-1,:) = mp(1,:);
  mp(imt_reg,:)   = mp(2,:);
 end

 if jperio == 1
  for i=1:imt_reg/2,
   mp(i,jmt_reg-3)           = max(mp(i,jmt_reg-3),mp(imt_reg-i+1,jmt_reg));
   mp(i,jmt_reg-2)           = max(mp(i,jmt_reg-2),mp(imt_reg-i+1,jmt_reg-1));
   mp(i,jmt_reg-1)           = max(mp(i,jmt_reg-1),mp(imt_reg-i+1,jmt_reg-2));
   mp(i,jmt_reg)             = max(mp(i,jmt_reg),mp(imt_reg-i+1,jmt_reg-3));
   mp(imt_reg-i+1,jmt_reg)   = mp(i,jmt_reg-3);
   mp(imt_reg-i+1,jmt_reg-1) = mp(i,jmt_reg-2);
   mp(imt_reg-i+1,jmt_reg-2) = mp(i,jmt_reg-1);
   mp(imt_reg-i+1,jmt_reg-3) = mp(i,jmt_reg);
  end
 end

 totmp=sum(sum(mp)); % totmp=size(find(mp==1),1);

end

fprintf('%d %d \n',mpold,totmp)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% maintenant que l'on connait le masque pour psi, %%
%%  on peut commencer l'integration.               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
psi=zeros(imt_reg,jmt_reg)-1.e12;
%%psi(:,:)=NaN;
ipsi=zeros(imt_reg,jmt_reg);

disp(sprintf('\n'));
disp('setting reference value for psi...');
iref=zeros(imt_reg,jmt_reg);

if ~exist('jref_psi') || ~exist('iref_psi')
  a_ref_psi;
end

iref(iref_psi,jref_psi)=1;
irefold=0;
totiref=1;

while totiref ~= irefold

 disp(sprintf('%d %d ',irefold,totiref));

 irefold=totiref;

 for j=1:jmt_reg-1,
  for i=1:imt_reg-1,
   if iref(i,j) == 1
    if iref(i+1,j) == 0 && tmask2(i+1,j+1)==0 && tmask2(i+1,j)==0
     iref(i+1,j)=1;
    end
    if iref(i,j+1) == 0 && tmask2(i+1,j+1)==0 && tmask2(i,j+1)==0
     iref(i,j+1)=1;
    end
   end
  end
 end

 for j=2:jmt_reg-1,
  for i=2:imt_reg-1
   if iref(i,j) == 1
    if iref(i-1,j) == 0 && tmask2(i,j+1)==0 && tmask2(i,j)==0
     iref(i-1,j)=1;
    end
    if iref(i,j-1) == 0 && tmask2(i+1,j)==0 && tmask2(i,j)==0
     iref(i,j-1)=1;
    end
   end
  end
 end

 for j=jmt_reg-1:-1:1,
  for i=1:imt_reg-1,
   if iref(i,j) == 1
    if iref(i+1,j) == 0 && tmask2(i+1,j+1)==0 && tmask2(i+1,j)==0
     iref(i+1,j)=1;
    end
    if iref(i,j+1) == 0 && tmask2(i+1,j+1)==0 && tmask2(i,j+1)==0
     iref(i,j+1)=1;
    end
   end
  end
 end

 for j=jmt_reg-1:-1:2,
  for i=2:imt_reg-1,
   if iref(i,j) == 1
    if iref(i-1,j) == 0 && tmask2(i,j+1)==0 && tmask2(i,j)==0
     iref(i-1,j)=1;
    end
    if iref(i,j-1) == 0 && tmask2(i+1,j)==0 && tmask2(i,j)==0
     iref(i,j-1)=1;
    end
   end
  end
 end

 for j=1:jmt_reg-1,
  for i=imt_reg-1:-1:1,
   if iref(i,j) == 1
    if iref(i+1,j) == 0 && tmask2(i+1,j+1)==0 && tmask2(i+1,j)==0
     iref(i+1,j)=1;
    end
    if iref(i,j+1) == 0 && tmask2(i+1,j+1)==0 && tmask2(i,j+1)==0
     iref(i,j+1)=1;
    end
   end
  end
 end

 for j=2:jmt_reg-1,
  for i=imt_reg-1:-1:2,
   if iref(i,j) == 1
    if iref(i-1,j) == 0 && tmask2(i,j+1)==0 && tmask2(i,j)==0
     iref(i-1,j)=1;
    end
    if iref(i,j-1) == 0 && tmask2(i+1,j)==0 && tmask2(i,j)==0
     iref(i,j-1)=1;
    end
   end
  end
 end

 for j=jmt_reg-1:-1:1,
  for i=imt_reg-1:-1:1,
   if iref(i,j) == 1
    if iref(i+1,j) == 0 && tmask2(i+1,j+1)==0 && tmask2(i+1,j)==0
     iref(i+1,j)=1;
    end
    if iref(i,j+1) == 0 && tmask2(i+1,j+1)==0 && tmask2(i,j+1)==0
     iref(i,j+1)=1;
    end
   end
  end
 end

 for j=jmt_reg-1:-1:2,
  for i=imt_reg-1:-1:2,
   if iref(i,j) == 1
    if iref(i-1,j) == 0 && tmask2(i,j+1)==0 && tmask2(i,j)==0
     iref(i-1,j)=1;
    end
    if iref(i,j-1) == 0 && tmask2(i+1,j)==0 && tmask2(i,j)==0
     iref(i,j-1)=1;
    end
   end
  end
 end

 if iperio == 1
  iref(1,:)         = max(iref(1,:),iref(imt_reg-1,:));
  iref(2,:)         = max(iref(2,:),iref(imt_reg,:));
  iref(imt_reg-1,:) = iref(1,:);
  iref(imt_reg,:)   = iref(2,:);
 end

 if jperio == 1
  for i=1:imt_reg/2,
   iref(i,jmt_reg-3)           = max(iref(i,jmt_reg-3),iref(imt_reg-i+1,jmt_reg));
   iref(i,jmt_reg-2)           = max(iref(i,jmt_reg-2),iref(imt_reg-i+1,jmt_reg-1));
   iref(i,jmt_reg-1)           = max(iref(i,jmt_reg-1),iref(imt_reg-i+1,jmt_reg-2));
   iref(i,jmt_reg)             = max(iref(i,jmt_reg),iref(imt_reg-i+1,jmt_reg-3));
   iref(imt_reg-i+1,jmt_reg)   = iref(i,jmt_reg-3);
   iref(imt_reg-i+1,jmt_reg-1) = iref(i,jmt_reg-2);
   iref(imt_reg-i+1,jmt_reg-2) = iref(i,jmt_reg-1);
   iref(imt_reg-i+1,jmt_reg-3) = iref(i,jmt_reg);
  end
 end

 totiref=sum(sum(iref)); % totiref=size(find(iref==1),1);

end

disp(sprintf('%d %d ',irefold,totiref));

ip1=0;
jp1=0;
for j=1:jmt_reg,
 for i=1:imt_reg,
  if iref(i,j) == 1 && mp(i,j) == 1 && ip1 == 0
   ip1=i;
   jp1=j;
  end
 end
end

if ip1 <= 0
 disp('reference value for psi does not match active domain bounds...');
 stop
end

%%%%%%%%%%%%%%%%%
%% Compute Psi %%
%%%%%%%%%%%%%%%%%
psi(ip1,jp1)=0.;
ipsi(ip1,jp1)=1;
totipsi=1;

disp(sprintf('\n'));
disp('computing psi...');
while totipsi < mpold

 disp(sprintf('%d %d ',totipsi,mpold));

 for j=1:jmt_reg,
  for i=1:imt_reg-1,
   if ipsi(i,j) == 1 && ipb(i,j) == 0
    if ipsi(i+1,j) == 0 && mp(i+1,j) == 1 && vmask_reg(i+1,j) == 1
     psi(i+1,j)=psi(i,j)+sq_xy_mer(i+1,j);
     ipsi(i+1,j)=1;
    end
   end
  end
 end
 for j=1:jmt_reg,
  for i=2:imt_reg,
   if ipsi(i,j) == 1 && ipb(i,j) == 0
    if ipsi(i-1,j) == 0 && mp(i-1,j) == 1 && vmask_reg(i,j) == 1
     psi(i-1,j)=psi(i,j)-sq_xy_mer(i,j);
     ipsi(i-1,j)=1;
    end
   end
  end
 end
 for j=1:jmt_reg-1,
  for i=1:imt_reg,
   if ipsi(i,j) == 1 && ipb(i,j) == 0
    if ipsi(i,j+1) == 0 && mp(i,j+1) == 1 && umask_reg(i,j+1) == 1
     psi(i,j+1)=psi(i,j)-sq_xy_zonal(i,j+1);
     ipsi(i,j+1)=1;
    end
   end
  end
 end
 for j=2:jmt_reg,
  for i=1:imt_reg,
   if ipsi(i,j) == 1 && ipb(i,j) == 0
    if ipsi(i,j-1) == 0 && mp(i,j-1) == 1 && umask_reg(i,j) == 1
     psi(i,j-1)=psi(i,j)+sq_xy_zonal(i,j);
     ipsi(i,j-1)=1;
    end
   end
  end
 end
 for j=jmt_reg:-1:1,
  for i=1:imt_reg-1,
   if ipsi(i,j) == 1 && ipb(i,j) == 0
    if ipsi(i+1,j) == 0 && mp(i+1,j) == 1 && vmask_reg(i+1,j) == 1
     psi(i+1,j)=psi(i,j)+sq_xy_mer(i+1,j);
     ipsi(i+1,j)=1;
    end
   end
  end
 end
 for j=jmt_reg:-1:1,
  for i=2:imt_reg,
   if ipsi(i,j) == 1 && ipb(i,j) == 0
    if ipsi(i-1,j) == 0 && mp(i-1,j) == 1 && vmask_reg(i,j) == 1
     psi(i-1,j)=psi(i,j)-sq_xy_mer(i,j);
     ipsi(i-1,j)=1;
    end
   end
  end
 end
 for j=jmt_reg-1:-1:1,
  for i=1:imt_reg,
   if ipsi(i,j) == 1 && ipb(i,j) == 0
    if ipsi(i,j+1) == 0 && mp(i,j+1) == 1 && umask_reg(i,j+1) == 1
     psi(i,j+1)=psi(i,j)-sq_xy_zonal(i,j+1);
     ipsi(i,j+1)=1;
    end
   end
  end
 end
 for j=jmt_reg:-1:2,
  for i=1:imt_reg,
   if ipsi(i,j) == 1 && ipb(i,j) == 0
    if ipsi(i,j-1) == 0 && mp(i,j-1) == 1 && umask_reg(i,j) == 1
     psi(i,j-1)=psi(i,j)+sq_xy_zonal(i,j);
     ipsi(i,j-1)=1;
    end
   end
  end
 end
 for j=jmt_reg:-1:1,
  for i=imt_reg-1:-1:1,
   if ipsi(i,j) == 1 && ipb(i,j) == 0
    if ipsi(i+1,j) == 0 && mp(i+1,j) == 1 && vmask_reg(i+1,j) == 1
     psi(i+1,j)=psi(i,j)+sq_xy_mer(i+1,j);
     ipsi(i+1,j)=1;
    end
   end
  end
 end
 for j=jmt_reg:-1:1,
  for i=imt_reg:-1:2,
   if ipsi(i,j) == 1 && ipb(i,j) == 0
    if ipsi(i-1,j) == 0 && mp(i-1,j) == 1 && vmask_reg(i,j) == 1
     psi(i-1,j)=psi(i,j)-sq_xy_mer(i,j);
     ipsi(i-1,j)=1;
    end
   end
  end
 end
 for j=jmt_reg-1:-1:1,
  for i=imt_reg:-1:1,
   if ipsi(i,j) == 1 && ipb(i,j) == 0
    if ipsi(i,j+1) == 0 && mp(i,j+1) == 1 && umask_reg(i,j+1) == 1
     psi(i,j+1)=psi(i,j)-sq_xy_zonal(i,j+1);
     ipsi(i,j+1)=1;
    end
   end
  end
 end
 for j=jmt_reg:-1:2,
  for i=imt_reg:-1:1,
   if ipsi(i,j) == 1 && ipb(i,j) == 0
    if ipsi(i,j-1) == 0 && mp(i,j-1) == 1 && umask_reg(i,j) == 1
     psi(i,j-1)=psi(i,j)+sq_xy_zonal(i,j);
     ipsi(i,j-1)=1;
    end
   end
  end
 end
 for j=1:jmt_reg,
  for i=imt_reg-1:-1:1,
   if ipsi(i,j) == 1 && ipb(i,j) == 0
    if ipsi(i+1,j) == 0 && mp(i+1,j) == 1 && vmask_reg(i+1,j) == 1
     psi(i+1,j)=psi(i,j)+sq_xy_mer(i+1,j);
     ipsi(i+1,j)=1;
    end
   end
  end
 end
 for j=1:jmt_reg,
  for i=imt_reg:-1:2,
   if ipsi(i,j) == 1 && ipb(i,j) == 0
    if ipsi(i-1,j) == 0 && mp(i-1,j) == 1 && vmask_reg(i,j) == 1
     psi(i-1,j)=psi(i,j)-sq_xy_mer(i,j);
     ipsi(i-1,j)=1;
    end
   end
  end
 end
 for j=1:jmt_reg-1,
  for i=imt_reg:-1:1,
   if ipsi(i,j) == 1 && ipb(i,j) == 0
    if ipsi(i,j+1) == 0 && mp(i,j+1) == 1 && umask_reg(i,j+1) == 1
     psi(i,j+1)=psi(i,j)-sq_xy_zonal(i,j+1);
     ipsi(i,j+1)=1;
    end
   end
  end
 end
 for j=2:jmt_reg,
  for i=imt_reg:-1:1,
   if ipsi(i,j) == 1 && ipb(i,j) == 0
    if ipsi(i,j-1) == 0 && mp(i,j-1) == 1 && umask_reg(i,j) == 1
     psi(i,j-1)=psi(i,j)+sq_xy_zonal(i,j);
     ipsi(i,j-1)=1;
    end
   end
  end
 end

 if iperio == 1
  for j=1:jmt_reg,
   if ipsi(1,j) == 0 && ipsi(imt_reg-1,j) == 1
    ipsi(1,j)=1;
    psi(1,j)=psi(imt_reg-1,j);
   end
   if ipsi(1,j) == 1 && ipsi(imt_reg-1,j) == 0
    ipsi(imt_reg-1,j)=1;
    psi(imt_reg-1,j)=psi(1,j);
   end
   if ipsi(2,j) == 0 && ipsi(imt_reg,j) == 1
    ipsi(2,j)=1;
    psi(2,j)=psi(imt_reg,j);
   end
   if ipsi(2,j) == 1 && ipsi(imt_reg,j) == 0
    ipsi(imt_reg,j)=1;
    psi(imt_reg,j)=psi(2,j);
   end
  end
 end

 if jperio == 1
  for i=1:imt_reg/2,
   if ipsi(i,jmt_reg-3) == 0 && ipsi(imt_reg-i+1,jmt_reg) == 1
    ipsi(i,jmt_reg-3)=1;
    psi(i,jmt_reg-3)=psi(imt_reg-i+1,jmt_reg);
   end
   if ipsi(i,jmt_reg-3) == 1 && ipsi(imt_reg-i+1,jmt_reg) == 0
    ipsi(imt_reg-i+1,jmt_reg)=1;
    psi(imt_reg-i+1,jmt_reg)=psi(i,jmt_reg-3);
   end
   if ipsi(i,jmt_reg-2) == 0 && ipsi(imt_reg-i+1,jmt_reg-1) == 1
    ipsi(i,jmt_reg-2)=1;
    psi(i,jmt_reg-2)=psi(imt_reg-i+1,jmt_reg-1);
   end
   if ipsi(i,jmt_reg-2) == 1 && ipsi(imt_reg-i+1,jmt_reg-1) == 0
    ipsi(imt_reg-i+1,jmt_reg-1)=1;
    psi(imt_reg-i+1,jmt_reg-1)=psi(i,jmt_reg-2);
   end
   if ipsi(i,jmt_reg-1) == 0 && ipsi(imt_reg-i+1,jmt_reg-2) == 1
    ipsi(i,jmt_reg-1)=1;
    psi(i,jmt_reg-1)=psi(imt_reg-i+1,jmt_reg-2);
   end
   if ipsi(i,jmt_reg-1) == 1 && ipsi(imt_reg-i+1,jmt_reg-2) == 0
    ipsi(imt_reg-i+1,jmt_reg-2)=1;
    psi(imt_reg-i+1,jmt_reg-2)=psi(i,jmt_reg-1);
   end
   if ipsi(i,jmt_reg) == 0 && ipsi(imt_reg-i+1,jmt_reg-3) == 1
    ipsi(i,jmt_reg)=1;
    psi(i,jmt_reg)=psi(imt_reg-i+1,jmt_reg-3);
   end
   if ipsi(i,jmt_reg) == 1 && ipsi(imt_reg-i+1,jmt_reg-3) == 0
    ipsi(imt_reg-i+1,jmt_reg-3)=1;
    psi(imt_reg-i+1,jmt_reg-3)=psi(i,jmt_reg);
   end
  end
 end

 totipsi=sum(sum(ipsi));

end %while

psi(find(psi < -1.e10 ))=NaN;

psi=psi/1.e6;

psi = psi.* pmask_xy_reg; %% We mask psi values by the psi mask.

pmax=max(max(psi(:,:)));
pmin=min(min(psi(:,:)));

disp(sprintf('\n'));
disp(sprintf('%12s %12s %12s %18s','#(p)','minimum','maximum','section'));
disp(sprintf('%12d %12.2f %12.2f %18s', ...
               i_loop,pmin,pmax,segname(pind(i_loop),:)));


if exist('param_psi')
  if (strcmp(param_psi,'salt'))
    a_fig_psi_salt;
  elseif (strcmp(param_psi,'temp'))
    a_fig_psi_temp;
  elseif (strcmp(param_psi,'depth'))
    a_fig_psi_depth;
  elseif (strcmp(param_psi,'dens'))
    a_fig_psi_dens;
  else
    error('Problem !!!');
  end
else
  a_fig_psi;
  
end

