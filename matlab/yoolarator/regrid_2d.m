function [a, olx, oly] = regrid_2d(b, olat, olon, nlat, nlon, fland);
% REGRID_2D  Regrids a 2D field to user-specified grid
%
%	>> lowres = regrid_2d(hires, lat, lon, nlat, nlon);
%
%	Where,
%
%		lowres	= new gridded data
%		hires	= old gridded data
%		lat		= old latitude array (S to N)
%		lon		= old longitude array (W to E)
%		nlat	= new latitude array (S to N)
%		nlon	= new longitude array (W to E)
%
%	Longitude - input longitude arrays need to contain only 
%	the west-most edges of each cell (i.e. -180W to 179E)
%
%	Latitude - input latitude arrays need to contain
%	the south-most edges of each cell plus the north-most
%	edge of the most northerly cell (i.e. -90S to 90N)
%
%   Note : You can specify (as a final argument) the minimum
%   fraction of ocean in a grid cell for that cell to remain
%   as ocean in the regridded cell (default = 0.50).
%
%	Note : This code is *NOT* protected against you making 
%	stupid mistakes.  And don't be feeding me any NaN-padded 
%	arrays.
%
%   Andrew Yool (axy@soc.soton.ac.uk), 2000.

nargs = nargin;
if nargs == 5
    fland = 0.5;
end

% A new test for longitude
t1 = 360 + olon(1);
if t1 == olon(end), olon = olon(1:1:(end-1)); end
t1 = 360 + nlon(1);
if t1 == nlon(end), nlon = nlon(1:1:(end-1)); end

% Cosine of input latitude grid
t1 = olat(1:1:end-1);
t2 = olat(2:1:end);
t3 = (t2 - t1)/2;
t4 = t1 + t3;
colat2	= cos([t4] * pi / 180);
t5 = size(colat2);
if t5(1) == 1
	colat	= permute(colat2, [2 1]);
else
	colat	= colat2;
end
	
% Calculate an alternate (and more correct) colat
t1 = olat(1:1:end-1)* pi / 180;
t2 = olat(2:1:end) * pi / 180;
t3 = sin(t2) - sin(t1);
t4 = t3 ./ (t2 - t1);
t5 = size(t4);
if t5(1) == 1
	colat	= permute(t4, [2 1]);
else
	colat	= t4;
end

% Calculate overlapping areas of grids
t1 = max(size(nlon));
[overlapx, loverlapx] = rgii2(olon, nlon, t1);
[overlapy, loverlapy] = rgii2(olat, nlat);

% If you needed a mask, the next lines should include it
% (needless to say, it's not implemented here)
t1 = max(size(nlat)) - 1;
t2 = max(size(nlon));
mask(1:1:t1, 1:1:t2) = 0.0;

%whos b over* colat mask

% Regrid data
a = rg021(b, overlapx, overlapy, colat, mask, fland);

olx = overlapx;
oly = overlapy;
%col = colat;
