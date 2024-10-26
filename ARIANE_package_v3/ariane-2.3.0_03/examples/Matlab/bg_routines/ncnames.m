%NG function theResult = NCNames(theNCItems)
function theResult = NCNames(theNCID)

% NCNames -- List of ncitem names.
%  NCNames(theNCItems) returns the list
%   of names corresponding to theNCItems,
%   a cell-array of NetCDF objects derived
%   from the "ncitem" class.
 
% Copyright (C) 1997 Dr. Charles R. Denham, ZYDECO.
%  All Rights Reserved.
%   Disclosure without explicit written consent from the
%    copyright owner does not constitute publication.
 
% Version of 21-Apr-1997 09:23:56.

if nargin < 1, help(mfilename), return, end

%NG if ~iscell(theNCItems), theNCItems = {theNCItems}; end

%NG theNCNames = cell(size(theNCItems));
[numdims, numvars, numglobalatts, unlimdimID] = netcdf.inq(theNCID);
theNCNames = cell(1,numvars);

%NG for i = 1:length(theNCItems)
%NG    if isa(theNCItems{i}, 'ncitem')
%NG       theNCNames{i} = name(theNCItems{i});
%NG    end
%NG end

for ii = 1:numvars
  [theNCNames{ii}, xtype, dimids, numatts] = netcdf.inqVar(theNCID,ii-1);
end

if nargin > 0
   theResult = theNCNames;
  else
   disp(theNCNames)
end
