function [dum_lon, dum_lat] = get_grid_genie_lonlatedges(DUM_NI,DUM_NJ,DUM_OFFSET)
% GET_GRID_GENIE_LONLATEDGES
%
%   *********************************************************
%   *** Generate GENIE grid                               ***
%   *********************************************************
%
%   NOTE: GOLDSTEIn grid calculated according to gseto.f
%
%   *********************************************************

% START
disp(['START [get_grid_genie_lonlatedges] >>>'])
close all;

% INITIALIZE
% dummy variables
dum_n_i = DUM_NI;
dum_n_j = DUM_NJ;
dum_grid_lon_offset = DUM_OFFSET;
% local constants
par_grid_dsc = 5.0E3;
par_grid_ds  = (sin(pi/2.0) - sin(-pi/2.0))/dum_n_j;

% CONSTRUCT GRID
% lon
for i=1:dum_n_i+1,
    loc_axis_lon(i) = (i-1)*(360.0/dum_n_i) + dum_grid_lon_offset;
end
% lat
for j=1:dum_n_j+1,
    loc_grid_sv(j) = sin(-pi/2.0) + (j-1)*par_grid_ds;
end
for j=1:dum_n_j+1,
    loc_axis_lat(j)  = (180.0/pi)*asin(loc_grid_sv(j));
end

% RETURN RESULT
dum_lon = loc_axis_lon;
dum_lat = loc_axis_lat;

% END
disp(['<<< END [get_grid_genie_lonlatedges]'])

%   *********************************************************
