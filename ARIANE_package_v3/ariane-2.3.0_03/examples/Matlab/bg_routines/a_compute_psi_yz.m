%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Compute the stream function PSI %% YZ
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Nicolas.Grima@univ-brest.fr %%
%% June 2019                   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fprintf('\n\n');
disp('Computing divergence...');

if ~exist('yz_mer')
  error('Please submit a_yz0 and not a_compute_psi_yz...');
end

%%%%%%%%%%%%%%%%%%%%%%%%
%% Compute Divergence %%
%%%%%%%%%%%%%%%%%%%%%%%%
%Initialize array
div=zeros(size(sq_yz_mer_msk));

%Compute divergence
for k=1:kmt_reg-1,
  for j=2:jmt_reg,
    div(j,k)= sq_yz_mer_msk(j,k) -  sq_yz_mer_msk(j-1,k) + ...
             sq_yz_vert_msk(j,k) - sq_yz_vert_msk(j,k+1);
  end
end

%Divergence max
divmax=max(max(max(abs(div))));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% reperage d un point psi "actif" %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
disp(sprintf('\n'));
disp('Search an active psi point:');
jp0=0;
kp0=0;

for k=2:kmt_reg
  for j=1:jmt_reg-1
    if ( sq_yz_mer_msk(j,k) ~=0. && sq_yz_mer_msk(j,k-1)~=0. && ...
         sq_yz_vert_msk(j,k)~=0. && sq_yz_vert_msk(j+1,k)~=0.)
      disp(sprintf('%d %d',j,k)); 
      jp0=j;
      kp0=k;
      break;
    end
  end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% une fois trouve un point psi actif, construction du masque de psi %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ipb=zeros(jmt_reg,kmt_reg);
mp =zeros(jmt_reg,kmt_reg);
mp(jp0,kp0)=1;

fprintf('\n\n');
disp('computing psi mask...');

mpold=0;
totmp=1;

while totmp > mpold

  mpold=totmp;
  
  for k=2:kmt_reg-1
    for j=2:jmt_reg-1
      if ipb(j,k) == 0 && mp(j,k) == 1
        if mp(j+1,k) == 0 && (tmask2(j+1,k) == 1 || tmask2(j+1,k-1) == 1)
          mp(j+1,k)=1;
        end
        if mp(j-1,k) == 0 && (tmask2(j,k) == 1 || tmask2(j,k-1) == 1)
          mp(j-1,k)=1;
        end
        if mp(j,k-1) == 0 && (tmask2(j,k-1) == 1 || tmask2(j+1,k-1) == 1)
          mp(j,k-1)=1;
        end
        if mp(j,k+1) == 0 && (tmask2(j,k) == 1 || tmask2(j+1,k) == 1)
          mp(j,k+1)=1;
        end
      end
    end
  end
  
  %initVal:step:endVal — Increment index by the value step on each iteration, or decrements index when step is negative.

  for k=kmt_reg-1:-1:2
    for j=2:jmt_reg-1
      if ipb(j,k) == 0 && mp(j,k) == 1
        if mp(j+1,k) == 0 && (tmask2(j+1,k) == 1 || tmask2(j+1,k-1) == 1)
          mp(j+1,k)=1;
        end
        if mp(j-1,k) == 0 && (tmask2(j,k) == 1 || tmask2(j,k-1) == 1)
          mp(j-1,k)=1;
        end
        if mp(j,k-1) == 0 && (tmask2(j,k-1) == 1 || tmask2(j+1,k-1) == 1)
          mp(j,k-1)=1;
        end
        if mp(j,k+1) == 0 && (tmask2(j,k) == 1 || tmask2(j+1,k) == 1)
          mp(j,k+1)=1;
        end
      end
    end
 end

 for k=2:kmt_reg-1
   for j=jmt_reg-1:-1:2
     if ipb(j,k) == 0 && mp(j,k) == 1
       if mp(j+1,k) == 0 && (tmask2(j+1,k) == 1 || tmask2(j+1,k-1) == 1)
         mp(j+1,k)=1;
       end
       if mp(j-1,k) == 0 && (tmask2(j,k) == 1 || tmask2(j,k-1) == 1)
         mp(j-1,k)=1;
       end
       if mp(j,k-1) == 0 && (tmask2(j,k-1) == 1 || tmask2(j+1,k-1) == 1)
         mp(j,k-1)=1;
       end
       if mp(j,k+1) == 0 && (tmask2(j,k) == 1 || tmask2(j+1,k) == 1)
         mp(j,k+1)=1;
       end
     end
    end
 end

 for k=kmt_reg-1:-1:2
   for j=jmt_reg-1:-1:2
     if ipb(j,k) == 0 && mp(j,k) == 1
       if mp(j+1,k) == 0 && (tmask2(j+1,k) == 1 || tmask2(j+1,k-1) == 1)
         mp(j+1,k)=1;
       end
       if mp(j-1,k) == 0 && (tmask2(j,k) == 1 || tmask2(j,k-1) == 1)
         mp(j-1,k)=1;
       end
       if mp(j,k-1) == 0 && (tmask2(j,k-1) == 1 || tmask2(j+1,k-1) == 1)
         mp(j,k-1)=1;
       end
       if mp(j,k+1) == 0 && (tmask2(j,k) == 1 || tmask2(j+1,k) == 1)
         mp(j,k+1)=1;
       end
     end
    end
 end

 totmp=sum(sum(mp)); 

end

fprintf('%d %d \n',mpold,totmp)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% maintenant que l'on connait le masque pour psi, %%
%%  on peut commencer l'integration.               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
psi =zeros(jmt_reg,kmt_reg)-1.e12;
ipsi=zeros(jmt_reg,kmt_reg);

disp(sprintf('\n'));
disp('setting reference value for psi...');
jref=zeros(jmt_reg,kmt_reg);

%if ~exist('jref_psi') || ~exist('kref_psi')
%  a_ref_psi_yz;
%end
jref_psi = jp0;
kref_psi = kp0;

jref(jref_psi,kref_psi)=1;
jrefold=0;
totjref=1;

while totjref ~= jrefold

 disp(sprintf('%d %d ',jrefold,totjref));

 jrefold=totjref;

 for k=2:kmt_reg-1
  for j=2:jmt_reg-1
   if jref(j,k) == 1
    if jref(j+1,k) == 0 && tmask2(j+1,k-1)==0 && tmask2(j+1,k)==0
     jref(j+1,k)=1;
    end
    if jref(j-1,k) == 0 && tmask2(j,k-1)==0 && tmask2(j,k)==0
     jref(j-1,k)=1;
    end
    if jref(j,k-1) == 0 && tmask2(j+1,k-1)==0 && tmask2(j,k-1)==0
     jref(j,k-1)=1;
    end
    if jref(j,k+1) == 0 && tmask2(j+1,k)==0 && tmask2(j,k)==0
     jref(j,k+1)=1;
    end
   end
  end
 end

 for k=kmt_reg-1:-1:2
  for j=2:jmt_reg-1
   if jref(j,k) == 1
    if jref(j+1,k) == 0 && tmask2(j+1,k-1)==0 && tmask2(j+1,k)==0
     jref(j+1,k)=1;
    end
    if jref(j-1,k) == 0 && tmask2(j,k-1)==0 && tmask2(j,k)==0
     jref(j-1,k)=1;
    end
    if jref(j,k-1) == 0 && tmask2(j+1,k-1)==0 && tmask2(j,k-1)==0
     jref(j,k-1)=1;
    end
    if jref(j,k+1) == 0 && tmask2(j+1,k)==0 && tmask2(j,k)==0
     jref(j,k+1)=1;
    end
   end
  end
 end

 for k=2:kmt_reg-1
  for j=jmt_reg-1:-1:2
   if jref(j,k) == 1
    if jref(j+1,k) == 0 && tmask2(j+1,k-1)==0 && tmask2(j+1,k)==0
     jref(j+1,k)=1;
    end
    if jref(j-1,k) == 0 && tmask2(j,k-1)==0 && tmask2(j,k)==0
     jref(j-1,k)=1;
    end
    if jref(j,k-1) == 0 && tmask2(j+1,k-1)==0 && tmask2(j,k-1)==0
     jref(j,k-1)=1;
    end
    if jref(j,k+1) == 0 && tmask2(j+1,k)==0 && tmask2(j,k)==0
     jref(j,k+1)=1;
    end
   end
  end
 end

 for k=kmt_reg-1:-1:2
  for j=jmt_reg-1:-1:2
   if jref(j,k) == 1
    if jref(j+1,k) == 0 && tmask2(j+1,k-1)==0 && tmask2(j+1,k)==0
     jref(j+1,k)=1;
    end
    if jref(j-1,k) == 0 && tmask2(j,k-1)==0 && tmask2(j,k)==0
     jref(j-1,k)=1;
    end
    if jref(j,k-1) == 0 && tmask2(j+1,k-1)==0 && tmask2(j,k-1)==0
     jref(j,k-1)=1;
    end
    if jref(j,k+1) == 0 && tmask2(j+1,k)==0 && tmask2(j,k)==0
     jref(j,k+1)=1;
    end
   end
  end
 end

 %BBL
 for j=2:jmt_reg-1
   if jref(j,kmt_reg) == 1
     if jref(j+1,kmt_reg) == 0 && tmask2(j+1,kmt_reg-1)==0 && tmask2(j+1,kmt_reg)==0
       jref(j+1,kmt_reg)=1
     end
     if jref(j-1,kmt_reg) == 0 && tmask2(j,kmt_reg-1)==0 && tmask2(j,kmt_reg)==0
       jref(j-1,kmt_reg)=1
     end
     if jref(j,kmt_reg-1) == 0 && tmask2(j+1,kmt_reg-1)==0 && tmask2(j,kmt_reg-1)==0
       jref(j,kmt_reg-1)=1
     end
   end
 end
 
 for j=2:jmt_reg-1
   if jref(j,kmt_reg) == 1
     if jref(j+1,kmt_reg) == 0 && tmask2(j+1,kmt_reg-1)==0 && tmask2(j+1,kmt_reg)==0
       jref(j+1,kmt_reg)=1
     end
     if jref(j-1,kmt_reg) == 0 && tmask2(j,kmt_reg-1)==0 && tmask2(j,kmt_reg)==0
       jref(j-1,kmt_reg)=1
     end
     if jref(j,kmt_reg-1) == 0 && tmask2(j+1,kmt_reg-1)==0 && tmask2(j,kmt_reg-1)==0
       jref(j,kmt_reg-1)=1
     end
   end
 end
 
 for j=jmt_reg-1:-1:2
   if jref(j,kmt_reg) == 1
     if jref(j+1,kmt_reg) == 0 && tmask2(j+1,kmt_reg-1)==0 && tmask2(j+1,kmt_reg)==0
       jref(j+1,kmt_reg)=1
     end
     if jref(j-1,kmt_reg) == 0 && tmask2(j,kmt_reg-1)==0 && tmask2(j,kmt_reg)==0
       jref(j-1,kmt_reg)=1
     end
     if jref(j,kmt_reg-1) == 0 && tmask2(j+1,kmt_reg-1)==0 && tmask2(j,kmt_reg-1)==0
       jref(j,kmt_reg-1)=1
     end
   end
 end
 
 for j=jmt_reg-1:-1:2
   if jref(j,kmt_reg) == 1
     if jref(j+1,kmt_reg) == 0 && tmask2(j+1,kmt_reg-1)==0 && tmask2(j+1,kmt_reg)==0
       jref(j+1,kmt_reg)=1
     end
     if jref(j-1,kmt_reg) == 0 && tmask2(j,kmt_reg-1)==0 && tmask2(j,kmt_reg)==0
       jref(j-1,kmt_reg)=1
     end
     if jref(j,kmt_reg-1) == 0 && tmask2(j+1,kmt_reg-1)==0 && tmask2(j,kmt_reg-1)==0
       jref(j,kmt_reg-1)=1
     end
   end
 end
 %BBL

 totjref=sum(sum(jref)); 

end

disp(sprintf('%d %d ',jrefold,totjref));

jp1=-1;
kp1=-1;
for k=1:kmt_reg
 for j=1:jmt_reg
  if jref(j,k) == 1 && mp(j,k) == 1
   jp1=j;
   kp1=k;
   break;
  end
 end
end

if jp1 <= 0
 disp('reference value for psi does not match active domain bounds...');
 stop
end

%%%%%%%%%%%%%%%%%
%% Compute Psi %%
%%%%%%%%%%%%%%%%%
psi(jp1,kp1)=0.;
ipsi(jp1,kp1)=1;
totipsi=1;

disp(sprintf('\n'));
disp('computing psi...');

while totipsi < mpold

 disp(sprintf('%d %d ',totipsi,mpold));

 for k=2:kmt_reg-1
  for j=2:jmt_reg-1
   if ipsi(j,k) == 1
    if ipsi(j+1,k) == 0 && mp(j+1,k) == 1
     psi(j+1,k)=psi(j,k)+sq_yz_vert(j+1,k);
     ipsi(j+1,k)=1;
    end
    if ipsi(j-1,k) == 0 && mp(j-1,k) == 1
     psi(j-1,k)=psi(j,k)-sq_yz_vert(j,k);
     ipsi(j-1,k)=1;
    end
    if ipsi(j,k-1) == 0 && mp(j,k-1) == 1
     psi(j,k-1)=psi(j,k)-sq_yz_mer(j,k-1);
     ipsi(j,k-1)=1;
    end 
    if ipsi(j,k+1) == 0 && mp(j,k+1) == 1
     psi(j,k+1)=psi(j,k)+sq_yz_mer(j,k);
     ipsi(j,k+1)=1;
    end
   end
  end
 end
 
 for k=kmt_reg-1:-1:2
  for j=2:jmt_reg-1
   if ipsi(j,k) == 1
    if ipsi(j+1,k) == 0 && mp(j+1,k) == 1
     psi(j+1,k)=psi(j,k)+sq_yz_vert(j+1,k);
     ipsi(j+1,k)=1;
    end
    if ipsi(j-1,k) == 0 && mp(j-1,k) == 1
     psi(j-1,k)=psi(j,k)-sq_yz_vert(j,k);
     ipsi(j-1,k)=1;
    end
    if ipsi(j,k-1) == 0 && mp(j,k-1) == 1
     psi(j,k-1)=psi(j,k)-sq_yz_mer(j,k-1);
     ipsi(j,k-1)=1;
    end
    if ipsi(j,k+1) == 0 && mp(j,k+1) == 1
     psi(j,k+1)=psi(j,k)+sq_yz_mer(j,k);
     ipsi(j,k+1)=1;
    end
   end
  end
 end

 for k=2:kmt_reg-1
  for j=jmt_reg-1:-1:2
   if ipsi(j,k) == 1
    if ipsi(j+1,k) == 0 && mp(j+1,k) == 1
     psi(j+1,k)=psi(j,k)+sq_yz_vert(j+1,k);
     ipsi(j+1,k)=1;
    end
    if ipsi(j-1,k) == 0 && mp(j-1,k) == 1
     psi(j-1,k)=psi(j,k)-sq_yz_vert(j,k);
     ipsi(j-1,k)=1;
    end
    if ipsi(j,k-1) == 0 && mp(j,k-1) == 1
     psi(j,k-1)=psi(j,k)-sq_yz_mer(j,k-1);
     ipsi(j,k-1)=1;
    end
    if ipsi(j,k+1) == 0 && mp(j,k+1) == 1
     psi(j,k+1)=psi(j,k)+sq_yz_mer(j,k);
     ipsi(j,k+1)=1;
    end
   end
  end
 end
 
 for k=kmt_reg-1:-1:2
   for j=jmt_reg-1:-1:2
    if ipsi(j,k) == 1
      if ipsi(j+1,k) == 0 && mp(j+1,k) == 1
       psi(j+1,k)=psi(j,k)+sq_yz_vert(j+1,k);
       ipsi(j+1,k)=1;
      end
      if ipsi(j-1,k) == 0 && mp(j-1,k) == 1
       psi(j-1,k)=psi(j,k)-sq_yz_vert(j,k);
       ipsi(j-1,k)=1;
      end
      if ipsi(j,k-1) == 0 && mp(j,k-1) == 1
       psi(j,k-1)=psi(j,k)-sq_yz_mer(j,k-1);
       ipsi(j,k-1)=1;
      end
      if ipsi(j,k+1) == 0 && mp(j,k+1) == 1
       psi(j,k+1)=psi(j,k)+sq_yz_mer(j,k);
       ipsi(j,k+1)=1;
      end
    end
   end
 end
 
 %BBL2
 
 for j=2,jmt_reg-1
   if ipsi(j,kmt_reg) == 1
     if ipsi(j+1,kmt_reg) == 0 && mp(j+1,kmt_reg) == 1
       psi(j+1,kmt_reg)=psi(j,kmt_reg)+sq_yz_vert(j+1,kmt_reg);
       ipsi(j+1,kmt_reg) = 1
     end
     if ipsi(j-1,kmt_reg) == 0 && mp(j-1,kmt_reg) == 1
       psi(j-1,kmt_reg)=psi(j,kmt_reg)-sq_yz_vert(j,kmt_reg);
       ipsi(j-1,kmt_reg) = 1
     end
     if ipsi(j,kmt_reg-1) == 0 && mp(j,kmt_reg-1) == 1
       psi(j,kmt_reg-1)=psi(j,kmt_reg)-sq_yz_mer(j,kmt_reg-1);
       ipsi(j,kmt_reg-1) = 1
     end
   end
 end
 
 for j=2,jmt_reg-1
   if ipsi(j,kmt_reg) == 1
     if ipsi(j+1,kmt_reg) == 0 && mp(j+1,kmt_reg) == 1
       psi(j+1,kmt_reg)=psi(j,kmt_reg)+sq_yz_vert(j+1,kmt_reg);
       ipsi(j+1,kmt_reg) = 1
     end
     if ipsi(j-1,kmt_reg) == 0 && mp(j-1,kmt_reg) == 1
       psi(j-1,kmt_reg)=psi(j,kmt_reg)-sq_yz_vert(j,kmt_reg);
       ipsi(j-1,kmt_reg) = 1
     end
     if ipsi(j,kmt_reg-1) == 0 && mp(j,kmt_reg-1) == 1
       psi(j,kmt_reg-1)=psi(j,kmt_reg)-sq_yz_mer(j,kmt_reg-1);
       ipsi(j,kmt_reg-1) = 1
     end
   end
 end
 
 for j=jmt_reg-1:-1:2
   if ipsi(j,kmt_reg) == 1
     if ipsi(j+1,kmt_reg) == 0 && mp(j+1,kmt_reg) == 1
       psi(j+1,kmt_reg)=psi(j,kmt_reg)+sq_yz_vert(j+1,kmt_reg);
       ipsi(j+1,kmt_reg) = 1
     end
     if ipsi(j-1,kmt_reg) == 0 && mp(j-1,kmt_reg) == 1
       psi(j-1,kmt_reg)=psi(j,kmt_reg)-sq_yz_vert(j,kmt_reg);
       ipsi(j-1,kmt_reg) = 1
     end
     if ipsi(j,kmt_reg-1) == 0 && mp(j,kmt_reg-1) == 1
       psi(j,kmt_reg-1)=psi(j,kmt_reg)-sq_yz_mer(j,kmt_reg-1);
       ipsi(j,kmt_reg-1) = 1
     end
   end
 end
 
 for j=jmt_reg-1:-1:2
   if ipsi(j,kmt_reg) == 1
     if ipsi(j+1,kmt_reg) == 0 && mp(j+1,kmt_reg) == 1
       psi(j+1,kmt_reg)=psi(j,kmt_reg)+sq_yz_vert(j+1,kmt_reg);
       ipsi(j+1,kmt_reg) = 1
     end
     if ipsi(j-1,kmt_reg) == 0 && mp(j-1,kmt_reg) == 1
       psi(j-1,kmt_reg)=psi(j,kmt_reg)-sq_yz_vert(j,kmt_reg);
       ipsi(j-1,kmt_reg) = 1
     end
     if ipsi(j,kmt_reg-1) == 0 && mp(j,kmt_reg-1) == 1
       psi(j,kmt_reg-1)=psi(j,kmt_reg)-sq_yz_mer(j,kmt_reg-1);
       ipsi(j,kmt_reg-1) = 1
     end
   end
 end
 
 %BBL2

 totipsi=sum(sum(ipsi));

end %while

psi(find(psi < -1.e10 ))=NaN;

psi=psi/1.e6;

psi = psi.* pmask_yz_reg; %% We mask psi values by the psi mask.

pmax=max(max(psi(:,:)));
pmin=min(min(psi(:,:)));

%disp(sprintf('\n'));
%disp(sprintf('%12s %12s %12s %18s','#(p)','minimum','maximum','section'));
%disp(sprintf('%12d %12.2f %12.2f %18s',i_loop,pmin,pmax,segname(pind(i_loop),:)));

a_fig_psi_yz;
