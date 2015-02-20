function [gblocklon, gblocklat] = blocky(in_lon, in_lat, field);
% BLOCKY  Adds a blocky coastline to a figure
%
%    This overlays a blocky coastline map onto the current 
%    figure.  The plot axes are also adjusted and the divisions 
%    are set to standard increments.  The axes are also labelled.
%
%	 This version creates the blocky coastline on the fly, and
%	 can do so for any grid it is fed :
%
%	 >> [olon, olat] = blocky(ilon, ilat, field);
%
%	 Where,	ilon	= input field of longitude edges (x+1)
%			ilat	= input field of latitude edges (y+1)
%			field	= 2D data field (x,y or x+1,y+1)
%			olon	= output blocky longitude edges
%			olat	= output blocky latitude edges
%
%	 Alternatively, if you already have a blocky coastline that
%	 you'd like to use on a plot (e.g. a surface coastline that
%	 you want to plot with a seafloor/atmosphere dataset), use
%	 the following syntax instead :
%
%	 >> blocky(olon, olat);
%
%	 Where olon and olat have been generated as above.
%
%	 Note : because I can't be bothered, there are no protection
%	 mechanisms in this routine.  If you feed in inappropriate or
%	 incorrect fields, it won't work.  So there.
%
%    See also GATLAS, GATLAS2, ATLAS, COAST.
%
%    Andrew Yool (axy@soc.soton.ac.uk), 17th October 2003.

% Load up dummy data
% a = load ('Expt_8/0480.1');
% [t1, t2] = get_gold(a, [36 36 8], 2);
% tracer = t1(:,:,:,1);
% field = tracer(:,:,8);
% in_lon = ilon; in_lat = ilat;

% Need to insert some checks and balances in here

% Start routine here

if nargin == 3
    
    clon1 = max(size(in_lon)); clat1 = max(size(in_lat));
    clon = clon1 - 1; clat = clat1 - 1;
    maxdim = max([clon clat]);
    bignum = 10^(floor(log10(maxdim)) + 1);
    
    % Setup up the 4 cell edges
    a1lon(1:clat,1:clon,1) = repmat(1:clon, [clat 1]);
    a1lat(1:clat,1:clon,1) = repmat(permute(1:clat, [2 1]), [1 clon]);
    a1lon(1:clat,1:clon,2) = repmat(1:clon, [clat 1]);
    a1lat(1:clat,1:clon,2) = repmat(permute(2:clat1, [2 1]), [1 clon]);
    a1lon(1:clat,1:clon,3) = repmat(1:clon, [clat 1]);
    a1lat(1:clat,1:clon,3) = repmat(permute(1:clat, [2 1]), [1 clon]);
    a1lon(1:clat,1:clon,4) = repmat(2:clon1, [clat 1]);
    a1lat(1:clat,1:clon,4) = repmat(permute(1:clat, [2 1]), [1 clon]);
    a1lon(1:clat,1:clon,5) = repmat(1:clon, [clat 1]);
    a1lat(1:clat,1:clon,5) = repmat(permute(2:clat1, [2 1]), [1 clon]);
    a1lon(1:clat,1:clon,6) = repmat(2:clon1, [clat 1]);
    a1lat(1:clat,1:clon,6) = repmat(permute(2:clat1, [2 1]), [1 clon]);
    a1lon(1:clat,1:clon,7) = repmat(2:clon1, [clat 1]);
    a1lat(1:clat,1:clon,7) = repmat(permute(1:clat, [2 1]), [1 clon]);
    a1lon(1:clat,1:clon,8) = repmat(2:clon1, [clat 1]);
    a1lat(1:clat,1:clon,8) = repmat(permute(2:clat1, [2 1]), [1 clon]);
    
    % Sort 2D field out to get a land-sea mask (sea = NaN)
    t1 = field(1:clat,1:clon);
    t2a = isfinite(t1); t2b = isnan(t1);
    t1(t2a) = NaN; t1(t2b) = 0;
    
    % Repeat mask to get it same size as edge array above
    t2 = repmat(t1, [1 1 8]);
    
    % Apply mask to data
    b1lon = a1lon + t2;
    b1lat = a1lat + t2;
    
    % How many cells are in the grid?
    tcell = clat*clon;
    
    % Reshape edge array (so that it's long lines of edges)
    for i = 1:1:8
        q1 = b1lon(:,:,i);
        q2 = reshape(q1, [tcell 1]);
        c1lon(1:tcell,i) = q2;
        q1 = b1lat(:,:,i);
        q2 = reshape(q1, [tcell 1]);
        c1lat(1:tcell,i) = q2;
    end
    
    % Sort edge array to find out NaNs (i.e. sea cells)
    [q1, q2] = sort(c1lon(:,1));
    q3 = isnan(q1);
    q4 = min(find(q3 == 1));
    q5 = q4 - 1;
    
    % Get rid of said NaNs
    for i = 1:1:8
        p1 = c1lon(:,i);
        p2 = p1(q2);
        p3 = p2(1:q5);
        d1lon(1:q5,i) = p3(1:q5,1);
        p1 = c1lat(:,i);
        p2 = p1(q2);
        p3 = p2(1:q5);
        d1lat(1:q5,i) = p3(1:q5,1);
    end
    
    % Stack the edge arrays into one big array
    pos = 1;
    for i = 1:2:7
        bot = pos; top = bot + q5 - 1;
        e1lon(bot:top,1:2) = d1lon(1:q5,i:(i+1));
        e1lat(bot:top,1:2) = d1lat(1:q5,i:(i+1));
        pos = pos + q5;
    end
    
    % Make up clever numbers
    scl = [1 bignum bignum^2 bignum^3];
    flonlat1 = [e1lon(:,1)*scl(1) e1lon(:,2)*scl(2) e1lat(:,1)*scl(3) e1lat(:,2)*scl(4)]; 
    flonlat2 = sum(flonlat1, 2);
    
    % Now, cull them to unique list
    clear g1 g2 g3 g4 g5
    [g1, g2, g3] = unique(flonlat2);
    glist = 1:max(size(g2));
    g4 = hist(g3,glist);
    g5 = (g4 == 1);
    glonlat = g1(g5);
    
    % Now, reverse the clever process
    clear h1 h2 h3 h4
    h1 = rem(glonlat, scl(2));
    h2 = (rem(glonlat - h1, scl(3))) / scl(2);
    h3 = (rem(glonlat - h1 - (h2*scl(2)), scl(4))) / scl(3);
    h4 = (glonlat - h1 - (h2*scl(2)) - (h3*scl(3))) / scl(4);
    hlen = max(size(h1));
    
    % Now convert these indices to longitude and latitude
    clear i1lon1 i1lon2 i1lat1 i1lat2
    i1lon1 = in_lon(h1);
    i1lon2 = in_lon(h2);
    i1lat1 = in_lat(h3);
    i1lat2 = in_lat(h4);
    
    % Finally put them in a list that can be plotted
    clear j1lon j1lat
    j1lon(1:(hlen*3)) = NaN;
    j1lon(1:3:((hlen*3)-2)) = i1lon1(1:end);
    j1lon(2:3:((hlen*3)-1)) = i1lon2(1:end);
    j1lat(1:(hlen*3)) = NaN;
    j1lat(1:3:((hlen*3)-2)) = i1lat1(1:end);
    j1lat(2:3:((hlen*3)-1)) = i1lat2(1:end);
    
    % Set up output data
    gblocklon = j1lon;
    gblocklat = j1lat;
    
elseif nargin == 2
    
    if isempty(in_lon)
        % Set up dummy data (just in case!)
        gblocklon = NaN;
        gblocklat = NaN;
        in_lon = [-260 100];
        in_lat = [-90 90];
    else    
        % Set up pre-existing data
        gblocklon = in_lon;
        gblocklat = in_lat;
        in_lon = [min(in_lon) max(in_lon)];
        in_lat = [min(in_lat) max(in_lat)];
    end
    
elseif nargin == 1
    
    % Set up dummy data
    gblocklon = NaN;
    gblocklat = NaN;
    
    in_lon = [-260 100];
    in_lat = [-90 90];
    
end

% Plot them
hold on;
plot (gblocklon, gblocklat, 'k');
axis image;
axis ([in_lon(1) in_lon(end) in_lat(1) in_lat(end)]);
if in_lon(1) == -260
    set(gca, 'XTick', [-260 -215 -170 -125 -80 -35 10 55 100]);
elseif in_lon(1) == 0
    set(gca, 'XTick', [0 45 90 135 180 225 270 315 360]);
elseif in_lon(1) == -180
    set(gca, 'XTick', [-180 -135 -90 -45 0 45 90 135 180]);
end	
set(gca, 'YTick', [-90 -60 -30 0 30 60 90]);
xlabel ('Longitude [{\circ}E]');
ylabel ('Latitude [{\circ}N]');
box on;
