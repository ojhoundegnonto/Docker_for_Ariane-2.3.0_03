function theResult = ncload(theNetCDFFile, varargin)

% ncload -- Load NetCDF variables.
%  ncload('theNetCDFFile', 'var1', 'var2', ...) loads the
%   given variables of 'theNetCDFFile' into the Matlab
%   workspace of the "caller" of this routine.  If no names
%   are given, all variables are loaded.  The names of the
%   loaded variables are returned or assigned to "ans".
%   No attributes are loaded.
 
% Copyright (C) 1997 Dr. Charles R. Denham, ZYDECO.
%  All Rights Reserved.
%   Disclosure without explicit written consent from the
%    copyright owner does not constitute publication.
 
% Version of 18-Aug-1997 10:13:57.

%NG Version of 30-Aug-2012
%NG 
%NG "ncload" function modified to work with 
%NG  the official Matlab netcdf toolbox.
%NG
%NG - tested with Matlab 2011b -
%NG
%NG Nicolas.Grima@univ-brest.fr
%NG

if nargin < 1, help(mfilename), return, end

result = [];
if nargout > 0, theResult = result; end

%NG f = netcdf(theNetCDFFile, 'nowrite');
ncid = netcdf.open(theNetCDFFile,'NC_NOWRITE');

%NG if isempty(f), return, end
if isempty(ncid)
    return
end

%NG if isempty(varargin), varargin = ncnames(var(f)); end
if isempty(varargin)
    varargin = ncnames(ncid); 
end

for ii = 1:length(varargin)
   %NG if ~isstr(varargin{i}), varargin{i} = inputname(i+1); end
   %NG assignin('caller', varargin{i}, f{varargin{i}}(:))
   varid = netcdf.inqVarID(ncid,varargin{ii});
   [varname, xtype, dimids, numatts] = netcdf.inqVar(ncid,varid);
   %NG display(['Is reading: ', varargin{ii}]);
   if ((xtype > 2) && (xtype < 6))
     
     if (length(dimids) > 1)
       assignin('caller', varargin{ii}, ...
         double(permute(ncread(theNetCDFFile,varargin{ii}),length(dimids):-1:1)));
     else
       assignin('caller', varargin{ii}, double(ncread(theNetCDFFile,varargin{ii})));
     end
     
   else
       
     if (length(dimids) > 1)
       assignin('caller', varargin{ii}, ...
         permute(ncread(theNetCDFFile,varargin{ii}),length(dimids):-1:1) );
     else
       assignin('caller', varargin{ii}, ncread(theNetCDFFile,varargin{ii}));
     end
   end
end

%NG close(f)
netcdf.close(ncid)

result = varargin;

if nargout > 0
   theResult = result;
else
   ncans(result)
end

%NG end
