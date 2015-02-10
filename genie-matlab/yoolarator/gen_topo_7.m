function [new_topo landsea2] = gen_topo_7(tblon, tblat, btopo, nlon, nlat, ndep, smpar, filename, oldmask);
%  GEN_TOPO_7
%
%   ***********************************************************************
%   *** Generates a GOLDSTEIN topography **********************************
%   ***********************************************************************
%
%	Returns a 'world.k1' file for GOLDSTEIN.  Restricted at 
%   the moment insofar as it still requires an the user to
%   somehow specify the land drainage pattern.
%
%	Usage :
%
%       >> new_lon  = -260:10:100;
%       >> new_lat  = -90:10:90;
%       >> new_dep  = get_gdep(8, 0.1, 5);
%       >> new_topo = gen_topo_5(old_lon, old_lat, old_topo ...
%                     new_lon, new_lat, new_dep, smooth, 'file');
%
%   Where :
%
%       old_lon     = existing topography's longitude array
%       old_lat     = existing topography's latitude array
%       old_topo	= existing topography file (e.g. etopo30)
%       new_lon     = new longitude array
%       new_lat     = new latitude array
%       new_dep		= depth levels for new topography
%       smooth      = smoothing parameter (0 to 1.1 = good range)
%       file		= filename for new topography file
%
%   Notes :
%
%   The 'new_dep' array should be in metres and start at the
%   bottom (i.e. some big negative number).  It should finish 
%   at 0.
%
%   Fast Fourier Transformation smoothing in this version of
%   the routine is performed at the length scale of the final
%   grid (specifically the longitudinal width of the cells in
%   the final grid).  For example, using the standard GOLDSTEIN 
%   grid would result in smoothing at a 10 degree length scale
%   (36 longitudinal cells, each 10 degrees wide).	
%
%   The above paragraph is now incorrect.  Smoothing is a bit
%   more pliable now allowing quite a range of range topography
%   results.  Values of the smoothing parameter < 0.75 return
%   almost unsmoothed grids, but values closer to 1 return much
%   more interesting grids.  Values > 1.1 can remove all
%   topography and result in a single layer ocean.  If this
%   happens, just repeat the process with a lower smoothing.
%
%   All longitude (and latitude) arrays should consist of the
%   western (and southern) edges of every cell, plus the
%   eastern (and northern) edge of the most easterly (and
%   northerly) cell, i.e. 0:10:360E, -180:10:180E, -90:10:90N.
%
%   Note that this routine lets you edit the land-sea mask to
%   correct distortions introduced by the regridding.  If you
%   would prefer to use an existing land-sea mask, add it's
%   filename (e.g. 'worber.k1') to the end of the argument list.
%   This also corrects the new *.k1 file for drainage patterns
%   where possible.  Otherwise, you'll need to use gold_paths
%   or hand-edit.
% 
%   See also GOLD_PATHS, RUNOFF, RUNOFF2.
%
%   Andrew Yool (axy@soc.soton.ac.uk), October 2004.
%
%   ***********************************************************************
%   *** HISTORY ***********************************************************
%   ***********************************************************************
%
%   14/02/09: renamed from: 'gen_topo_6'
%
%   ***********************************************************************

% ============================================================
%  Initialisation
% ============================================================

fprintf('\n');

% Check for arguments
% Need a minimum of new depth scale and original topography file
nargs = nargin;
if nargs < 8
    error (' - Error!  A few more inputs please');
elseif nargs == 8
    fprintf(' - All inputs supplied, thanks\n');
    oldgrid = 0;
elseif nargs == 9    
    fprintf(' - All inputs supplied, thanks\n');
    oldgrid = 1;
else
    error (' - Error!  Steady on, a few less inputs please');
end

% Is an old grid is offered, load it up and prepare it
if oldgrid == 1
    t1 = load (oldmask);
    t2 = t1(2:1:(end-1),2:1:(end-1));
    t3 = flipud(t2);
    landsea0 = t3; 
    landsea0(t3 > 90) = NaN; landsea0(t3 < 90) = 1;
    old_mask = t3;
    
    [jmaxo, imaxo] = size(old_mask);
    fprintf(' - Old grid successfully loaded\n');
end

% Which figures are wanted?
fig1 = 1; % base topography
fig2 = 1; % land-sea masks
fig3 = 1; % topographic slices
fig4 = 1; % final topography

topoedit = 1; % edit topography?

% Work out grid resolution
imax = max(size(nlon)) - 1; jmax = max(size(nlat)) - 1;
kmax = max(size(ndep)) - 1;
reslon = imax; reslat = jmax;
% Set up grid limits
newlon = nlon; newlat = nlat; resdep = ndep;
oldlon = tblon(1:end-1); oldlat = tblat;

if oldgrid == 1
    if imax ~= imaxo | jmax ~= jmaxo
        fprintf(' - Mismatch in old and new grid sizes.  Old grid ignored\n');
        oldgrid = 0;
    end
end

% Turn depth levels into metres if necessary
layers = kmax;
if max(abs(resdep)) < 10
    resdep = resdep * 1e3;
end

maxdep = min(resdep);

% Check if incoming topography has values over land
t1 = isfinite(btopo); t2 = max(btopo(t1));
if t2 == 0, zeroland = 1; else, zeroland = 0; end

% ============================================================
%  Plot base topography (if required)
% ============================================================

% Plot up incoming topography
blon = tblon(1:(end-1));
blat = tblat;
%tblon = blon; tblon(end+1) = tblon(end) + (blon(end) - blon(end-1));
tbtopo = btopo; tbtopo(end+1,:) = NaN; tbtopo(:,end+1) = NaN;
tbtopo(tbtopo >= 0) = NaN;
t1 = btopo(isfinite(btopo)); % maxdep = min(t1);

if fig1 == 1
    figure(1); clf
    oceanpal(20);
    pcolor (tblon, blat, tbtopo); shading flat;
    caxis ([maxdep 0]); blocky(tblon, blat, tbtopo);
    h = colorbar ('horiz');
    title ('Base topography [m]');
    stamp; drawnow;
    fname = sprintf('%s_pic_01',filename);
    paper; orient landscape; print ('-dpsc', '-painters', fname);
else
    fprintf(' - Base topography acceptable\n');
end

% ============================================================
%  Determine land-sea mask
% ============================================================

fprintf(' - Working out land-sea mask\n');

% Work out start and end land-sea masks
t1 = btopo; t1(btopo >= 0) = NaN; t1(btopo < 0) = 1;
landsea1 = t1;
b1 = btopo; b1(btopo >= 0) = NaN;
t1 = regrid_2d(b1, blat, blon, newlat, newlon(1:(end-1)), 0.5);
t2 = t1; t2(t1 >= 0) = 0; t2(t1 < 0) = 1;
landsea2 = t2;

% Calculate land/sea area
t1 = landsea1; t1(end+1,:) = NaN; t1(:,end+1) = NaN;
t1(isfinite(t1)) = 1; t1(isnan(t1)) = 0;
area1 = grid_area(blat, tblon); a1 = sum(area1(isfinite(area1)));
area1(t1 == 0) = NaN; a2 = sum(area1(isfinite(area1)));
t1 = landsea2; t1(end+1,:) = NaN; t1(:,end+1) = NaN;
t1(isfinite(t1)) = 1; t1(isnan(t1)) = 0;
area2 = grid_area(newlat, newlon); a3 = sum(area2(isfinite(area2)));
area2(t1 == 0) = NaN; a4 = sum(area2(isfinite(area2)));

% Report land/sea areas of grids
fprintf('\n - Pre-edit ocean fraction :\n');
fprintf(' - Total area of base grid : %6.4e\n',a1);
fprintf(' - Total area of new grid  : %6.4e\n',a3);
fprintf(' - Ocean fraction of base grid : %4.2f %% \n', (a2 / a1)*100);
fprintf(' - Ocean fraction of new grid  : %4.2f %% \n', (a4 / a3)*100);

% Impose old grid land-sea mask on new grid
if oldgrid == 1
    landsea2 = landsea0;
end

if topoedit == 1
    % ==================================================
    % Topography edit
    % ==================================================
    
    % Set up label for colorbar
    cbartxt(1,1:5) = 'land ';
    cbartxt(2,1:5) = 'ocean';
    
    % Do the topography alteration
    fprintf('\nTopography alteration procedure :\n');
    fprintf('- left button to turn grid cell to ocean\n');
    fprintf('- right button to turn grid cell to land\n');
    fprintf('- middle button to finish\n\n');
    cmap = [0.5 1 0.5; 0.5 0.5 1]; colormap (cmap);
    t1 = landsea2; t1(isfinite(t1)) = 1; t1(isnan(t1)) = 0;
    t1(end+1,:) = NaN; t1(:,end+1) = NaN;
    [sy,sx] = size(t1); sy = sy - 1; sx = sx - 1;
    flag = 0;
    while flag == 0
        figure(1); clf
        colormap(cmap);
        pcolor (t1); axis image;
        caxis ([-0.5 1.5]); h = colorbar ('horiz');
        set(h,'XTick',0:1,'XTickLabel',cbartxt);
        title ('Ocean land/sea mask');
        [x,y,button] = ginput(1);
        ix = floor(x); iy = floor(y);
        if ix < 1 | ix > sx | iy < 1 | iy > sy
            fprintf(' - Out of grid range\n');
        elseif button == 2
            flag = 1;
            fprintf('\n - Topography alteration complete\n');
        else
            if button == 1
                fprintf(' - Cell at (%d, %d) now ocean\n', ix, iy);
                t1(iy, ix) = 1;
            elseif button == 3
                fprintf(' - Cell at (%d, %d) now land\n', ix, iy);
                t1(iy, ix) = 0; 
            else
                fprintf('Cannot deepen/shallow cell at (%d, %d)\n',ix,iy);
            end
        end
        clf    
    end
    
    % Setup a new landsea mask based on the edit and the old one
    landsea2b = landsea2;
    landsea2 = t1(1:end-1,1:end-1);
    landsea2(landsea2 == 0) = NaN;
    
    % Plot up the old and new grids
    figure(2); clf
    cmap = [0.5 1 0.5; 0.5 0.5 1]; colormap (cmap);
    subplot (2,1,1);
    t1 = landsea2b; t1(end+1,:) = NaN; t1(:,end+1) = NaN;
    clear olon olat; [olon, olat] = blocky(newlon, newlat, t1);
    t1(isfinite(t1)) = 1; t1(isnan(t1)) = 0;
    pcolor (newlon, newlat, t1); shading flat; caxis ([0 1]); h = colorbar ('horiz');
    blocky(olon, olat);
    axis ([newlon(1) newlon(end) newlat(1) newlat(end)]);
    set(h, 'XTick',[0.25 0.76],'XTickLabel',char('Land', 'Sea'));
    titstr = sprintf('First draft %s topography land-sea mask', filename);
    title (titstr);
    subplot (2,1,2);
    t1 = landsea2; t1(end+1,:) = NaN; t1(:,end+1) = NaN;
    clear olon olat; [olon, olat] = blocky(newlon, newlat, t1);
    t1(isfinite(t1)) = 1; t1(isnan(t1)) = 0;
    pcolor (newlon, newlat, t1); shading flat; caxis ([0 1]); h = colorbar ('horiz');
    blocky(olon, olat);
    axis ([newlon(1) newlon(end) newlat(1) newlat(end)]);
    set(h, 'XTick',[0.25 0.76],'XTickLabel',char('Land', 'Sea'));
    titstr = sprintf('Redrafted %s topography land-sea mask', filename);
    title (titstr);
    stamp; drawnow;
    fname = sprintf('%s_pic_05',filename);
    paper; orient tall; print ('-dpsc', '-painters', fname);
end

t1 = landsea2; t1(end+1,:) = NaN; t1(:,end+1) = NaN;
t1(isfinite(t1)) = 1; t1(isnan(t1)) = 0;
area2 = grid_area(newlat, newlon); a3 = sum(area2(isfinite(area2)));
area2(t1 == 0) = NaN; a4 = sum(area2(isfinite(area2)));

% Report land/sea areas of grids
fprintf('\n - Post-edit ocean fraction :\n');
fprintf(' - Ocean fraction of base grid : %4.2f %% \n', (a2 / a1)*100);
fprintf(' - Ocean fraction of new grid  : %4.2f %% \n', (a4 / a3)*100);

if fig2 == 1
    figure(2); clf
    cmap = [0.5 1 0.5; 0.5 0.5 1]; colormap (cmap);
    subplot (2,1,1);
    t1 = landsea1; t1(end+1,:) = NaN; t1(:,end+1) = NaN;
    clear olon olat; [olon, olat] = blocky(tblon, blat, t1);
    t1(isfinite(t1)) = 1; t1(isnan(t1)) = 0;
    pcolor (tblon, blat, t1); shading flat; caxis ([0 1]); h = colorbar ('horiz');
    blocky(olon, olat);
    axis ([oldlon(1) oldlon(end) oldlat(1) oldlat(end)]);
    set(h, 'XTick',[0.25 0.76],'XTickLabel',char('Land', 'Sea'));
    title ('Old topography land-sea mask');
    subplot (2,1,2);
    t1 = landsea2; t1(end+1,:) = NaN; t1(:,end+1) = NaN;
    clear olon olat; [olon, olat] = blocky(newlon, newlat, t1);
    t1(isfinite(t1)) = 1; t1(isnan(t1)) = 0;
    pcolor (newlon, newlat, t1); shading flat; caxis ([0 1]); h = colorbar ('horiz');
    blocky(olon, olat);
    axis ([newlon(1) newlon(end) newlat(1) newlat(end)]);
    set(h, 'XTick',[0.25 0.76],'XTickLabel',char('Land', 'Sea'));
    titstr = sprintf('New %s topography land-sea mask', filename);
    title (titstr);
    stamp; drawnow;
    fname = sprintf('%s_pic_02',filename);
    paper; orient tall; print ('-dpsc', '-painters', fname);
end

% ============================================================
%  Smooth base topography (if required)
% ============================================================

% Is an initial smoothing to be used?
init_smooth = 1;
%init_smooth = 0;
if init_smooth == 1
    % Perform a smoothing operation on base topography
    fprintf(' - Smoothing base topography\n'); drawnow;
    t1 = topo_p1(btopo, smpar);
    if zeroland == 1, t1(t1 > 0) = 0; end
    % Plot up the effects of smoothing for the user
    [y1, i1] = max(btopo, [], 2); [y2, i2] = min(btopo, [], 2); [y3, i3] = max((y1 - y2));
    if fig3 == 1
        figure(3); clf
        subplot(3,1,1);
        q1 = btopo(i3,:)/1e3; q2 = t1(i3,:)/1e3;
        plot([blon(1) blon(end)], [0 0], 'k:', blon, q1, 'b-', blon, q2, 'r-'); axis ([blon(1) blon(end) -8 1]);
        % xlabel ('Longitude [{\circ}E]'); 
        ylabel ('Depth [km]');
        botlab = sprintf('Smoothed topography at %d{\\circ}N (blue = raw; red = smoothed)', round(blat(i3))); title (botlab);
        drawnow;
    end
    
    % Store these longitude slices for later
    slice1 = btopo(i3,:) / 1e3;
    slice2 = t1(i3,:) / 1e3;
    
    fprintf(' - Grid smoothing complete\n');
else
    t1 = btopo;
    
    % Store these longitude slices for later
    [y1, i1] = max(btopo, [], 2); [y2, i2] = min(btopo, [], 2); [y3, i3] = max((y1 - y2));
    slice1 = btopo(i3,:) / 1e3;
    slice2 = t1(i3,:) / 1e3;
    fprintf(' - No grid smoothing requested\n');
end

% Calculate and apply base land-sea mask to smoothed data
q1 = btopo; q1(btopo >= 0) = 0; q1(btopo < 0) = 1;
t1 = t1 .* q1;

% Set topographic heights above sea-level to zero
t1(t1 > 0) = 0;

% ============================================================
%  Regrid base topography to new grid
% ============================================================

% Regrid smoothed base topography to GOLDSTEIN grid
fprintf(' - Regridding base topography to GOLDSTEIN grid\n'); drawnow;
% The next parameter controls how much land a cell can contain
% before it becomes land in the final topography.  A sensible
% value for this is 0.5, but using a lower value is useful if
% combined with the land-sea mask calculated earlier.
land_frac = 0.5;
t1_ = t1;
t1(find(t1 == 0.0)) = NaN;
[t2] = make_regrid_lonlat(tblon, tblat, t1', newlon, newlat);
t2=t2';
[t2_, olx, oly] = regrid_2d(t1_, blat, blon, newlat, newlon(1:(end-1)), 0.5);
store_t2 = t2;

% Get regridded data slice
clear q1 q2 q3 q4
q1 = [newlat blat(i3)]; q2 = sort(q1); q3 = find(q2 == blat(i3));
if q3 > jmax
    q4 = jmax;
else
    q4 = q3;
end
clear p1 p2
p1 = t2(q4,:) / 1e3;
p2(1:2:((reslon*2)-1)) = p1(1:1:reslon);
p2(2:2:(reslon*2)) = p1(1:1:reslon);
slice3 = p2;

% Save topographic heights if required (e.g. for land surface)
% save mark_data_2 t2

% ============================================================
%  Do a smoothing operation on the regridded data?
% ============================================================

second_smooth = 1;
if second_smooth == 1
    
    % Feed the regridded data back through the FFT transformation
    t3 = topo_p1(t2, smpar);
    t2 = t3;
    
end

clear p1 p2
p1 = t2(q4,:) / 1e3;
p2(1:2:((reslon*2)-1)) = p1(1:1:reslon);
p2(2:2:(reslon*2)) = p1(1:1:reslon);
slice4 = p2;

% Data slice plotting information
newlon2(1) = newlon(1);
newlon2(2:2:((reslon*2)-2)) = newlon(2:1:reslon);
newlon2(3:2:((reslon*2)-1)) = newlon(2:1:reslon);
newlon2(reslon*2) = newlon(reslon+1);

figure(3);
subplot (3,1,2);
plot([newlon2(1) newlon2(end)], [0 0], 'k:', newlon2, slice3, 'b-', newlon2, slice4, 'r-'); axis ([newlon2(1) newlon2(end) -8 1]);
% xlabel ('Longitude [{\circ}E]'); 
ylabel ('Depth [km]');
toplab = sprintf('%s topography at %d{\\circ}N (blue = regridded; red = smoothed)', filename, round(blat(i3))); title (toplab);

% ============================================================
%  Truncate new grid's topography to specific depth levels
% ============================================================

fprintf(' - Truncating new topography to specified depth levels\n'); drawnow;
t4 = topo_p2(t2, resdep);

% ============================================================
%  Correct new topography to match land/sea boundary of old topography
% ============================================================

fprintf(' - Correcting land/sea grid and drainage patterns\n'); drawnow;
t5 = topo_p3(t4, landsea2, layers);
new_topo = t5;

% ============================================================
%  Show slice across old and new topography grids
% ============================================================

% Get regridded data slice
clear p1 p2 p3
p1 = new_topo(q4,:);
p2(1:2:((reslon*2)-1)) = p1(1:1:reslon);
p2(2:2:(reslon*2)) = p1(1:1:reslon);
p2(p2 > layers) = layers + 1;
p2(p2 == 0) = layers + 1;
p3 = resdep(p2);
slice5 = p3 / 1e3;

if fig3 == 1
    % Plot up the data slices
    figure(3);
    subplot (3,1,3);
    plot([newlon2(1) newlon2(end)], [0 0], 'k:', newlon2, slice4, 'b-', newlon2, slice5, 'r-'); axis ([newlon2(1) newlon2(end) -8 1]);
    % legend ('Regridded', 'Truncated', 4);
    xlabel ('Longitude [{\circ}E]'); ylabel ('Depth [km]');
    toplab = sprintf('%s topography at %d{\\circ}N (blue = smoothed; red = truncated)', filename, round(blat(i3))); title (toplab);
    stamp; drawnow;
    fname = sprintf('%s_pic_03',filename);
    paper; orient tall; print ('-dpsc', '-painters', fname);
end

% ============================================================
%  Apply old drainage map if required
% ============================================================

% Apply drainage map from old grid to new one
if oldgrid == 1
    % The first stage here corrects for alterations
    % the user might have made to the land-sea mask
    drain = isnan(landsea2);
    t5(drain) = 95;
    drain2 = isnan(landsea0);
    drain2(drain == 0) = 0;
    t5(drain2) = old_mask(drain2);
    newtopo = t5;
end

% ============================================================
%  Save new topography file
% ============================================================

% Turn new topography into a GOLDSTEIN input file
fprintf(' - Saving new topography as a GOLDSTEIN input file\n'); drawnow;
t6(2:1:(reslat+1),2:1:(reslon+1)) = t5(1:1:reslat,1:1:reslon);
t6(2:1:(reslat+1),1) = t5(1:1:reslat,reslon);
t6(2:1:(reslat+1),(reslon+2)) = t5(1:1:reslat,1);
t6((reslat+2),:) = 18;
t6(1,:) = t6(2,:);

% Now write it out to a file
namefile = sprintf('%s.k1', filename);
fid = fopen (namefile,'w');
fprintf(fid,' ');
for i = (reslat+2):-1:1
    for j = 1:1:(reslon+2)
        q1 = round(t6(i,j));
        if q1 < 10
            fprintf(fid, ' %d ', q1);
        else
            fprintf(fid, '%d ', q1);
        end
    end
    fprintf(fid,'\n ');
end
fclose(fid);
fprintf(' - File saved, it is called %s\n',namefile); drawnow;

% ============================================================
%  Show new topography grid
% ============================================================

% Plot up the topography for the user
ftopo = new_topo - 0.5; ftopo(ftopo > 90) = NaN;
ftopo(reslat+1,:) = NaN; ftopo(:,reslon+1) = NaN;

for i = 1:1:(layers+1)
    q1 = (round(abs(resdep(i)) / 1e2))/10;
    q2 = sprintf('%0.3g',q1);
    q3 = lenstr(q2);
    if q3 < 3
        q4 = sprintf('%s.0',q2);
    else
        q4 = q2;
    end
    laystr(i,1:3) = q4(1:3);
end

fig4 = 1;
if fig4 == 1
    figure(4); clf
    oceanpal(layers);
    pcolor (newlon, newlat, ftopo); shading flat;
    caxis ([0 layers]); blocky(newlon, newlat, ftopo);
    h = colorbar ('horiz');
    set(h,'XTick', 0.5:1:(layers - 0.5));
    set(h,'XTickLabel', laystr(1:(end-1),:));
    toplab = sprintf ('New %s topography [km]', filename);
    title (toplab);
    drawnow;
    stamp; drawnow;
    fname = sprintf('%s_pic_04',filename);
    paper; orient landscape; print ('-dpsc', '-painters', fname);
end

% Give user something to plot
q1 = new_topo; q1(q1 > 90) = NaN;
q2 = q1;
%for i = 1:1:layers
%	q2(q1 == i) = resdep(i);
%end
q2(reslat+1,:) = NaN; q2(:,reslon+1,:) = NaN;
new_topo = q2;

% ============================================================
%  Prepare ancillary data files (T & S only at present)
% ============================================================

% % Prepare 3D climatological fields of T & S for comparison with
% % model - these are used in diagend.f at the end of a model run
% gen_clim = 1;
% if gen_clim == 1
%     fprintf(' - Generating T and S climatology at new resolution\n'); drawnow;
%     
%     % Need to make an appropriate land mask
%     % There's a switch here :
%     %  0 = use bathymetry grid to delineate sea/land at depth
%     %  1 = use surface land mask to delineate sea/land at depth
%     use_surf = 1;
%     new_topo2 = new_topo(1:reslat,1:reslon);
%     for i = layers:-1:1
%         t1 = zeros(reslat,reslon) + NaN;
%         if use_surf == 0
%           t2 = (new_topo2 <= i);
%         else
%           t2 = (new_topo2 <= layers);
%         end
%         t1(t2) = 0;
%         mask(:,:,i) = t1;
%     end
%     mask2 = flipdim(mask, 3);
%     
%     % Load up and process T and S climatology
%     load woa_TS
%     fprintf(' - Processing temperature\n'); drawnow;
%     new_tmp = regrid_3d(woa_tmp(1:180,1:360,:), woa_lat, woa_lon, woa_dep, newlat, newlon, resdep, mask2);
%     fprintf(' - Processing salinity\n'); drawnow;
%     new_sal = regrid_3d(woa_sal(1:180,1:360,:), woa_lat, woa_lon, woa_dep, newlat, newlon, resdep, mask2);
%     
%     % Flip new grids
%     new_tmp = flipdim(new_tmp, 3);
%     new_sal = flipdim(new_sal, 3);
%     
%     % Write out T and S climatology
%     nelements = reslat * reslon * layers;
%     t1 = permute(new_tmp, [3 2 1]);	t2 = reshape(t1, [nelements 1]); t2(isnan(t2)) = -99.9999; 
%     namefile = sprintf('%sT.silo', filename); fid = fopen(namefile,'w'); fprintf(fid,'%6.4f\n',t2); fclose(fid);
%     t1 = permute(new_sal, [3 2 1]);	t2 = reshape(t1, [nelements 1]); t2(isnan(t2)) = -99.9999; 
%     namefile2 = sprintf('%sS.silo', filename); fid = fopen(namefile2,'w'); fprintf(fid,'%6.4f\n',t2); fclose(fid);
%     
%     fprintf(' - Climatology written as files %s and %s\n',namefile,namefile2); drawnow;
% end

% Is the data also saved as a Matlab file
save_matlab = 1;
if save_matlab == 1
    % This next bit ensures that the Matlab file's contents are
    % named after the filename itself - i.e. so that if you load
    % more than one of these files, they're all got different
    % names
    str_eval = sprintf('%s = new_topo;\n',filename)
    eval(str_eval);
    str_eval = sprintf('save %s %s\n',filename,filename);
    eval(str_eval);
    fprintf(' - Matlab version of the data also saved to %s.mat\n', filename); drawnow;
end

fprintf(' - Done and dusted\n\n'); drawnow;

new_topo = store_t2;

% ============================================================
% ------------------------------------------------------------
% ============================================================

function [new_grid] = topo_p1(old_grid, scale);
%	When fed a 2D grid of topography plus a cut-off percentage,
%	this command FFTs the grid, removes frequencies less than
%	the cut-off percentage of the maximum, and then IFFTs the
%	resulting field to return a smoothed topography.

clear t1 t2 t3 t4 t5

% Convert scale to fractions of 360 degrees
scale2 = scale / 360;

[mxlat, mxlon] = size(old_grid);

if mod(mxlat, 2) > 0
    q1 = old_grid(1:(end-1),:);
    old_grid = q1;
    mxlat = mxlat - 1;
end
if mod(mxlon, 2) > 0
    q1 = old_grid(:,1:(end-1));
    old_grid = q1;
    mxlon = mxlon - 1;
end

hflat = floor(mxlat / 2);
hflon = floor(mxlon / 2);

% Create frame to put data maxtrix into
t1((mxlat*2),(mxlon*2)) = 0;
% Put data matrix into centre of frame
t1((hflat+1):1:(hflat+mxlat),(hflon+1):1:(hflon+mxlon)) = old_grid(1:mxlat,1:mxlon);
% Repeat data matrix either side (i.e. where it wraps east-west) 
t1((hflat+1):1:(hflat+mxlat),1:1:hflon) = old_grid(1:mxlat,hflon+1:mxlon);
t1((hflat+1):1:(hflat+mxlat),(hflon+mxlon+1):1:(2*mxlon)) = old_grid(1:mxlat,1:1:hflon);
% FFT resulting matrix
t2 = fft2(t1);
% Get the real part
rt2 = real(t2);
% Get the imaginary part
it2 = imag(t2); 

% Now, need to do something clever to remove low amplitude/high frequency stuff
% Here I'm creating a circular mask
A = ones((mxlat*2),(mxlon*2));
X = cumsum(A,1) - mxlat - (1/2);
Y = cumsum(A,2) - mxlon - (1/2);
R = sqrt((X.^2) + (Y.^2));
% R0 is the radius of the circle within which FFT components are destroyed
% Note : in the equation below the multiplier on scale2 allows one to
% increase or decrease smoothing.  If you increase it, the radius of
% the "circle of destruction" decreases, with the result that topography
% is less smooth.  Decrease it, and topography is smoothed more.  The
% default is usually 2, but 1 and 4 usually have appreciable effects.
% R0 = mxlon - (mxlon * (scale2*2));
% R0 = mxlon + (mxlon * scale2);
R0 = mxlon * scale;
tanh_switch = 1;
if tanh_switch == 1
    lambda = (5/360)*mxlon; M = (tanh((R - R0)/lambda)+1)/2;
else
    M = A; M(R < R0) = 0;
end
% Apply this to the real and imaginary components
rt2 = rt2 .* M;
it2 = it2 .* M;
% Recombine real and imaginary components
nt2 = rt2 + (it2 * sqrt(-1));
% Inverse FFT the matrix
t3 = real(ifft2(nt2));
% De-frame the data matrix
t4 = t3((hflat+1):1:(hflat+mxlat),(hflon+1):1:(hflon+mxlon));

% Sorted
new_grid = t4;

% ============================================================
% ------------------------------------------------------------
% ============================================================

function [new_grid] = topo_p2(old_grid, new_gdep);
%	When fed a 2D grid of topography plus a gdep field,
%	this command returns the 2D grid with its topography
%	altered to match the gdep field.

layers = max(size(new_gdep)) - 1;
if max(abs(new_gdep)) < 10
    new_gdep = new_gdep * 1e3;
end
ngpos = (new_gdep(2:1:end) - new_gdep(1:1:(end-1)))/2 + new_gdep(1:1:(end-1));

new_grid = old_grid;

% Get rid of land cells
new_grid(old_grid > 0) = NaN;

for i = layers:-1:1
    new_grid(old_grid < ngpos(i)) = i;
end

% Catch grid cells that are below sea level but are missed above
new_grid(old_grid > ngpos(layers) & old_grid <= 0) = layers;

% ============================================================
% ------------------------------------------------------------
% ============================================================

function [new_grid] = topo_p3(old_grid, gold_grid, layers);
%	When fed a 2D grid of topography plus a standard GOLDSTEIN
%	2D topography grid (from a 'world.k1' file), this command 
%	corrects the new grid so that its land distribution matches 
%	that of the standard grid.  Where the new grid has land and 
%	the standard grid has ocean, the new grid is changed to 
%	assume the depth of the shallowest depth level.

% tmp_grid = flipud(gold_grid);

% Add on drainage patterns to new grid
old_grid(isnan(gold_grid)) = 95;

% Find new grid land points that should be ocean
t1 = (isnan(old_grid));
old_grid(t1) = layers;

arse = 0;
if arse == 1
    % Set all land points in the standard topography to NaNs
    tmp_grid = flipud(gold_grid);
    tmp_grid(gold_grid > 90) = NaN;
    
    % NaN-ify all of the land points in the new grid
    old_grid(isnan(tmp_grid)) = NaN;
    
    % Find new grid land points that should be ocean
    t1 = (old_grid >= 0);
    old_grid(t1) = layers;
    
    % Add on drainage patterns to new grid
    old_grid(gold_grid > 90) = gold_grid(gold_grid > 90);
end

new_grid = old_grid;

% ============================================================
% ------------------------------------------------------------
% ============================================================

% function [clim_t, clim_s] = topo_p4(topo, 

% ============================================================
% ------------------------------------------------------------
% ============================================================

function [a] = oceanpal(fine)
% OCEANPAL  Creates an ocean depth palette of user-specified size
%
%	This command builds on RAIN, but uses an "ocean depth"
%	palette.
%
%	>> oceanpal(12);
%
%	This will produce a palette with exactly 12 colours in
%	it.  The colours used will be drawn from the standard
%	ocean depth palette.

basefine = 200;

pal = [
    0.6000         0    0.7000;
    0.4500         0    0.6500;
    0.3000         0    0.6000;
    0.1000         0    0.6000;
    0.2286    0.1714    0.6429;
    0.2929    0.2571    0.6643;
    0.3571    0.3429    0.6857;
    0.4214    0.4286    0.7071;
    0.4857    0.5143    0.7286;
    0.5500    0.6000    0.7500;
    0.6000    0.6500    0.8000;
    0.6667    0.7667    0.8667;
    0.7333    0.8833    0.9333;];

clear pal2;
basefine=basefine + 1;

if basefine<0
    pal2=pal;
else
    pal2(1,:)=pal(1,:);
    for i=1:1:12
        pos=((i-1)*basefine)+1;
        stepr=(pal(i,1) - pal(i+1,1))/basefine;
        stepg=(pal(i,2) - pal(i+1,2))/basefine;
        stepb=(pal(i,3) - pal(i+1,3))/basefine;
        for j=1:1:basefine
            pal2(pos+j,1)=pal(i,1) - (stepr*j);
            pal2(pos+j,2)=pal(i,2) - (stepg*j);
            pal2(pos+j,3)=pal(i,3) - (stepb*j);
        end
    end
end

bigpal = max(size(pal2));

if fine < 2
    pal3 = pal2(1,:);
elseif fine == 2
    pal3(1,:) = pal2(1,:);
    pal3(fine,:) = pal2(end,:);
elseif fine > 200
    error (' Please be serious - do you really want such a large palette?');
else
    pal3(1,:) = pal2(1,:);
    pal3(fine,:) = pal2(end,:);
    
    t1 = bigpal - fine;
    t2 = (t1 / (fine - 1));
    pos = 1;
    for i = 2:1:(fine - 1)
        pos = pos + t2 + 1;
        pos2 = round(pos);
        pal3(i,:) = pal2(pos2,:);
    end
end

colormap(pal3);
a = pal3;
