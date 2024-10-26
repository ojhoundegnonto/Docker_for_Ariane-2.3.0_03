%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Read values in the Ariane file sections.txt %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Comment
disp(sprintf('\n'));
disp('Reading Ariane file section.txt');

% First step: open file to count the number of line
fid=fopen('sections.txt','r');

nb_sec=0;

while 1
  tline = fgetl(fid);
  if ~ischar(tline)
    break
  elseif ~isempty(tline) 
   nb_sec = nb_sec + 1;
  end
end
fclose(fid);

% Second step: open again file to read data
% Support white space in section name
fid=fopen('sections.txt','r');

segname='';

for is = 1:nb_sec,
  segind(is)=fscanf(fid,'%d',1);
  i10=fscanf(fid,'%d',1);
  i1(is)=abs(i10);
  i20=fscanf(fid,'%d',1);
  i2(is)=abs(i20);
  j10=fscanf(fid,'%d',1);
  j1(is)=abs(j10);
  j20=fscanf(fid,'%d',1);
  j2(is)=abs(j20);
  k10=fscanf(fid,'%d',1);
  k1(is)=abs(k10);
  k20=fscanf(fid,'%d',1);
  k2(is)=abs(k20);
  sname=fscanf(fid,'%s',1);
  while ~strcmp(sname(size(sname,2)),'"')
    str=fscanf(fid,'%s',1);
    sname=strcat(sname,str);
  end
  segname=strvcat(segname,sname);
  if i10 < 0 | j10 < 0
    segor(is) = -1;
  end
  disp(sprintf('%i %i %i %i %i %i %i %s',segind(is),i1(is),i2(is),j1(is),j2(is),k1(is),k2(is),sname));

end

fclose(fid);

% Read grid parameters and data if need
if ~exist('imt_reg_start')
  a_ncreadgrid;
end

if (iperio == 1) && (imt_reg_end < imt_reg_start)
  i1(i1 < imt_reg_start) = i1(i1 < imt_reg_start) + 180;
  i2(i2 < imt_reg_start) = i2(i2 < imt_reg_start) + 180;
end

i1_reg=i1-imt_reg_start+1;
i2_reg=i2-imt_reg_start+1;

j1_reg=j1-jmt_reg_start+1;
j2_reg=j2-jmt_reg_start+1;

for j=1:size(j1_reg,2)
    if j1_reg(j) < 0 
        j1_reg(j) = 1;
    end
    if j2_reg(j) > jmt_reg
        j2_reg(j) = jmt_reg;
    end
end

k1_reg=k1-kmt_reg_start+1;
k2_reg=k2-kmt_reg_start+1;

for k=1:size(k1_reg,2)
    if k1_reg(j) <= 0 
        k1_reg(j) = 1;
    end
    if k2_reg(j) <= 0 
        k2_reg(j) = kmt_reg;
    end
end
