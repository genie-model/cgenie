function [dum_lon, dum_lat] = get_grid_genie(DUM_NI,DUM_NJ,DUM_NK)
% GET_GRID_GENIE
%
%   *********************************************************
%   *** Generate GENIE grid                               ***
%   *********************************************************
%
% NOTE: GOLDSTEIn variable values calculated according to gseto.f
%

%   *********************************************************

% START
disp(['START [get_grid_genie] >>>'])
close all;

% INITIALIZE
% dummy variables
dum_n_i = DUM_NI;
dum_n_j = DUM_NJ;
dum_n_k = DUM_NK;
% local constants
par_grid_dsc = 5.0E3;
par_grid_ds  = (sin(pi/2.0) - sin(-pi/2.0))/dum_n_j;
par_grid_lon_offset = -260.0;

% CONSTRUCT GRID - HORIZONTAL
% 
for j=1:dum_n_j,
    loc_grid_sv(j) = sin(-pi/2.0) + j*par_grid_ds;
    loc_grid_s(j)  = loc_grid_sv(j) - 0.5*par_grid_ds;
end
% lon
for i=1:dum_n_i,
    loc_axis_lon(i) = (360.0/dum_n_i)*(i-0.5) + par_grid_lon_offset;
    loc_axis_dlon(i) = (360.0/dum_n_i);
end
% lat
for j=1:1,
    loc_axis_lat(j)  = (180.0/pi)*asin(loc_grid_s(j));
    loc_axis_dlat(j) = (180.0/pi)*(asin(loc_grid_sv(j)) - asin(-1.0));
end
for j=2:dum_n_j,
    loc_axis_lat(j)  = (180.0/pi)*asin(loc_grid_s(j));
    loc_axis_dlat(j) = (180.0/pi)*(asin(loc_grid_sv(j)) - asin(loc_grid_sv(j-1)));
end
% create 2D lat & lon grids
[loc_grid_lat loc_grid_lon] = meshgrid(loc_axis_lat,loc_axis_lon);

% CONSTRUCT GRID - VERITCAL
%
%   % NOTE: for variable (exponential) loc_grid_dz use loc_grid_ez0 > 0,
%   %       else use loc_grid_ez0 < 0
%   loc_grid_ez0 = 0.1;
%   loc_grid_z1 = loc_grid_ez0*((1.0 + 1.0/loc_grid_ez0)^(1.0/real(dum_n_k)) - 1.0);
%   loc_grid_tv4 = loc_grid_ez0*((loc_grid_z1/loc_grid_ez0 + 1.0)^0.5 - 1.0);
%   loc_grid_tv2 = 0.0;
%   loc_grid_tv1 = 0.0;
%   loc_grid_dz(:) = 0.0;
%   for k=1:dum_n_k,
%      if loc_grid_ez0 > 0.0
%         loc_grid_tv3 = loc_grid_ez0*((loc_grid_z1/loc_grid_ez0 + 1.0)^k - 1.0);
%         loc_grid_dz(dum_n_k-k+1) = loc_grid_tv3 - loc_grid_tv2;
%         loc_grid_tv2 = loc_grid_tv3;
%         loc_grid_tv5 = loc_grid_ez0*((loc_grid_z1/loc_grid_ez0 + 1.0)^(real(k) + 0.5) - 1.0);
%         if k < dum_n_k
%            loc_grid_dza(n_k-k) = loc_grid_tv5 - loc_grid_tv4;
%         else
%            loc_grid_dza(k) = 0.0;
%         end
%         loc_grid_tv4 = loc_grid_tv5;
%         loc_grid_tv1 = loc_grid_tv1 + loc_grid_dz(n_k-k+1);
%      end
%   end
%   %
%   for k=1:dum_n_k,
%      loc_grid_Dbot(k) = sum(par_grid_dsc*loc_grid_dz(k:dum_n_k));
%      loc_grid_Dtop(k) = sum(par_grid_dsc*loc_grid_dz(k+1:dum_n_k+1));
%   end

% RETURN RESULT
dum_lon = loc_axis_lon;
dum_lat = loc_axis_lat;

% END
disp(['<<< END [get_grid_genie]'])

%   *********************************************************