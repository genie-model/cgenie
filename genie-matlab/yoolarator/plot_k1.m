function [a] = plot_k1(glon, glat, gdep, fname);
% PLOT_K1  Plots a *.k1 file
%
%   Useage : 
%
%       >> plot_k1(glon, glat, gdep, 'worber.k1');
%
%   Andrew Yool (axy@soc.soton.ac.uk), 20th October 2004.

% Set up grid size
imax = max(size(glon)) - 1;
jmax = max(size(glat)) - 1;
kmax = max(size(gdep)) - 1;

% Load up a topography file, remove its edges and NaN pad it
% a = load ('workar.k1');
a = load (fname);
b = a(2:jmax+1, 2:imax+1);
c = flipud(b); c(jmax+1,:) = NaN; c(:,imax+1) = NaN;

bathy = c;
bathy(bathy == 91) = kmax+2; bathy(bathy == 92) = kmax+3;
bathy(bathy == 93) = kmax+4; bathy(bathy == 94) = kmax+1;

plotgdep = round((-1*gdep(1:kmax))*10)/10;
for i = 1:kmax
    tmpstr = sprintf('%1.1f',plotgdep(i));
    strplotgdep(i,1:3) = tmpstr(1:3);
end

ubathy = bathy + NaN; vbathy = bathy + NaN;
for j = 1:1:jmax
        for i = 1:1:imax
                if bathy(j,i) == kmax+1, vbathy(j,i) = 1; ubathy(j,i) = 0;
                elseif bathy(j,i) == kmax+2, ubathy(j,i) = 1;  vbathy(j,i) = 0;
                elseif bathy(j,i) == kmax+3, vbathy(j,i) = -1; ubathy(j,i) = 0;
                elseif bathy(j,i) == kmax+4, ubathy(j,i) = -1;  vbathy(j,i) = 0; 
                end
        end
end

% Scale vbathy to match grid
frac = 2.5;
t1 = glat(2:end) - glat(1:end-1); t1(end+1) = NaN;
t2 = repmat(permute(t1, [2 1]), [1 imax+1]); uscale = t2 / frac;
% AY (16/09/04) : new scaling factor to make equatorial arrows bigger
% Sinusoidal
% frac2 = 0.75;
% t1 = sin(((glat+90)/360)*2*pi); t2 = (t1(2:end) + t1(1:end-1))/2;
% t3 = (t2 * frac2) + 1; t3(end+1) = NaN; t4 = repmat(permute(t3, [2 1]), [1 37]);
% uscale2 = uscale .* t4;
% Linear
% t1 = (glat(2:end) + glat(1:end-1)) / 2; t2 = ((1 - abs(t1/90)) * frac2) + 1;
% t2(end+1) = NaN; t3 = repmat(permute(t2, [2 1]), [1 37]);
% uscale3 = uscale .* t3;
% Apply scaling (ignoring my new scaling factors ...)
%%%whos vbathy uscale;
vbathy = vbathy .* uscale;
ubathy = ubathy * ((glon(2) - glon(1)) / frac);

bathy2 = bathy; bathy2(bathy2 > kmax) = NaN;
glat2 = (glat(2:1:end) + glat(1:1:(end-1)))/2; glat2(end+1) = NaN;
glon2 = (glon(2:1:end) + glon(1:1:(end-1)))/2; glon2(end+1) = NaN;
oceanpal(kmax);

% figure(1); clf
%%%whos gl* *bathy;
h2 = quiver(glon2, glat2, ubathy, vbathy, 0, 'k');
% set(h2,'LineWidth',0.5);
hold on;
pcolor (glon, glat, bathy2); shading flat;
blocky(glon, glat, bathy2);
box on;
%axis ([-260.1 100.1 -90.1 90.1]);
caxis ([0.5 (kmax+0.5)]); h = colorbar ('horiz');
set(h,'XTick',1:kmax,'XTickLabel',strplotgdep);
pos = get(h, 'Position');
posx = pos(1) + (pos(3) / 2); posy = pos(2) + (1.5 * pos(4));
hc = get(h, 'Title');
set(hc, 'String', 'Depth [km]');
