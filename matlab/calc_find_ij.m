function dum_ij = calc_find_ij(dum_lon,dum_lat,dum_lon_offset,dum_imax,dum_jmax)
% calc_grid_distance
%
%   ***********************************************************************
%   *** Calculate i,j location of a specified lat,lon point ***************
%   ***********************************************************************
%
%
%   ***********************************************************************
%   *** HISTORY ***********************************************************
%   ***********************************************************************
%
%   10/07/06: added new dummy variables
%
%   ***********************************************************************

% \/\/\/ USER SETTINGS \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ %
% lon_offset = -180.0;     % LONGITUDE OFFSET
% imax = 36;               % i-dimension
% jmax = 36;               % j-dimension
% /\/\/\ USER SETTINGS /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ %

% *** INITIALIZE PARAMETERS & VARIABLES ********************************* %
%
% *********************************************************************** %
%
% set passed parameters
lon_offset = dum_lon_offset;
imax = dum_imax;
jmax = dum_jmax;
% LOCAL VARIABLES
loc_dlon = 360.0/imax;

% *** CALCULATE (i,j) *************************************************** %
%
% *********************************************************************** %
%
% CALCULATE 'i'
% precondition lon
if dum_lon >= (360.0 + lon_offset)
    dum_lon = dum_lon - 360.0;
end
if dum_lon < lon_offset
    dum_lon = dum_lon + 360.0;
end
% calculate 'i'
loc_i = int8((dum_lon - lon_offset)/loc_dlon + 0.5);
if (loc_i > imax), loc_i = imax; end
%
% CALCULATE 'j'
% check lat
if (dum_lat > 90) | (dum_lat < -90)
    disp(['lat out-of-range ...'])
end
% calculate 'j'
loc_sinlat = sin(pi*dum_lat/180.0);
loc_j = int8(jmax*0.5*(1.0 + loc_sinlat) + 0.5);
if (loc_j > jmax), loc_j = jmax; end

% *** RETURN RESULT ***************************************************** %
%
% *********************************************************************** %
%
dum_ij = [loc_i, loc_j];
