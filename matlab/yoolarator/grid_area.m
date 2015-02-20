function [a]=grid_area(glat, glon)
% GRID_AREA  Generates an array of cell areas
%
%	Given arrays of latitude and longitude cell limits,
%	this function returns a grid of cell areas.
%
%	>> area = grid_area([-90:10:90], [-180:10:180]);
%
%	In this example, both dimensions given cover the
%	whole global domain (i.e. all edges for every cell
%	are included).
%
%	Units are kilometres squared (= 1e6 m^2).
%
%	Total Earth area should be 5.1e8 km^2 (though
%       this is obviously a function of the assumed Earth
%       radius - 6371 km here).
%
%       Andrew Yool (axy@soc.soton.ac.uk), 14th February 2003.

% R = radius of the earth in metres
R=6371000;

% degrees to radians conversion
rad=(pi/180);
% Earth's radius squared
R2=R^2;

% Size of arrays
szlat = max(size(glat));
szlon = max(size(glon));

% Stop latitude from exceeding 90S or 90N (i.e. Hadley)
% (n.b. fragile if submitted grid has multiple cells > 90)
[t1, t2] = min(glat);
if t1 < -90
	fprintf('\n Latitude cells extend below 90S - correcting\n');
	glat(t2) = -90;
end
[t1, t2] = max(glat);
if t1 > 90
	fprintf('\n Latitude cells extend above 90N - correcting\n');
	glat(t2) = 90;
end

% Loops to calculatitudee surface area of each model box
for i=1:1:(szlat-1)
	for j=1:1:(szlon-1)
		latitude1	= glat(i+1)*rad;
		latitude2	= glat(i)*rad;
		longitude1	= glon(j+1)*rad;
		longitude2	= glon(j)*rad;
		w = (longitude1 - longitude2)*(sin(latitude1) - sin(latitude2));
		area(i,j)=w*R2;
	end
end

% Pad array with NaNs
area(szlat,:) = NaN;
area(:,szlon) = NaN;

% And the answer is ...
a=area ./ 1e6;
