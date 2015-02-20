function [glon, glonm, glat, glatm, gdep, gthick, garea] = ...
  gold_grid(imax, jmax, kmax);
% GOLD_GRID  Calculates properties for an equal area GOLDSTEIN grid
% 
%   Useage :
%
%   >> [glon, glonm, glat, glatm, gdep, gthick, garea] = ...
%         gold_grid(imax, jmax, kmax);
%
%   Where :
%
%   glon    = longitude edges / u-point position
%   glonm   = longitude midpoints / v-point position
%   glat    = latitude edges / v-point position
%   glatm   = latitude midpoints / u-point position
%   gdep    = depth interfaces
%   gthick  = thickness of depth levels
%   garea   = area of (equal area) grid cells
%   imax    = grid i (longitude) dimension
%   jmax    = grid j (latitude) dimension
%   kmax    = grid k (depth levels) dimension

% Longitude
glon = -180:(360/imax):180;
glonm = (-180+(180/imax)):(360/imax):(180-(180/imax));

% Latitude
t1 = -1:(2/jmax):1;
t2 = asin(t1);
t3 = (t2 / pi) * 180;
glat = t3;

t1 = -1+(1/jmax):(2/jmax):1-(1/jmax);
t2 = asin(t1);
t3 = (t2 / pi) * 180;
glatm = t3;

% Depth levels
gdep = get_gdep(kmax, 0.1, 5);
gthick = gdep(2:end) - gdep(1:(end-1));

% Grid area
rsc  = 6.37e6;
ds   = 2 / imax;
dphi = (2 * pi) / imax;
garea = rsc * rsc * ds * dphi;
