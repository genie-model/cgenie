function [] = make_topo(PEXPID,PMAXI,PMAXJ,PMAXK,PNAME,POPT);
% make_topo
%
%   ***********************************************************************
%   *** Yool-a-rate [MAKE GENIE TOPO] *************************************
%   ***********************************************************************
%
%   make_topo(PEXPID,PMAXI,PMAXJ,PMAXK,PNAME,POPT)
%   takes 6 arguments:
%   PEXPID [STRING]
%   --> the GCM experiment name
%   PMAXI [INTEGER]
%   --> the i-dimension to be created
%   PMAXJ [INTEGER]
%   --> the j-dimension to be created
%   PMAXK [INTEGER]
%   --> the k-dimension to be created
%   PNAME [STRING] (e.g., 'p0067g_tmp')
%   --> the final topo name (suggested format: pyyyyz)
%              where yyyy is the time interval (Ma)
%   POPT [STRING] (e.g., 'topo_config')
%   --> the string for an alternative plotting parameter set
%   --> if an empty (i.e., '') value is passed to this parameter
%       then the default parameter set is used (make_topo_settings)
%
%   ***********************************************************************
%   *** HISTORY ***********************************************************
%   ***********************************************************************
%
%   14/01/04: renamed and generalized yoolarate_hadcm3l
%   14/02/09: changed to new re-gridding routine
%             also updated plotting
%             set opengl rendering to 'neverselect' to fix plotting bug
%
%   ***********************************************************************

% *********************************************************************** %
% *** INITIALIZE ******************************************************** %
% *********************************************************************** %
%
% close plot windows
% NOTE: don't clear variable space here ...
close all;
%
opengl neverselect;
% set dummy variables
imax = PMAXI;
jmax = PMAXJ;
kmax = PMAXK;
expid = PEXPID;
fname2 = PNAME;
% load plotting options
if isempty(POPT), POPT='make_topo_settings'; end
eval(POPT);
if opt_bathonly, opt_windonly=false; end
if (opt_bathonly || opt_windonly), opt_filtertopo=false; end
% add yoolrator library path
addpath(library_path);
% set date
str_date = [datestr(date,11), datestr(date,5), datestr(date,7)];
% set function name
str_function = 'make_topo';
%
% *********************************************************************** %

% *********************************************************************** %
% *** SET UP GRID ******************************************************* %
% *********************************************************************** %
%
% *** OPEN netCDF DATA FILE ********************************************* %
%
switch par_gcm
    case 'hadcm3l'
        ncid_topo = netcdf.open([gcm_path '/' par_um_topo '.nc'],'nowrite');
    case 'foam'
        %%%%%%
    otherwise
        disp(['ERROR: Unknown GCM name.']);
        return;
end
% read netCDf information
[ndims,nvars,ngatts,unlimdimid] = netcdf.inq(ncid_topo);
%
% *** load topo from netCDF ********************************************* %
%
switch par_gcm
    case 'hadcm3l'
        varid  = netcdf.inqVarID(ncid_topo,'bathsmooth');
        et2(:,:) = netcdf.getVar(ncid_topo,varid);
        % create land mask
        maskb = ones(size(et2));
        maskb(find(et2(:,:) <= 0.0)) = NaN;
        maskb(:,73) = [];
        maskb = flipdim(maskb',1);
        % flip array around diagonal to give (lon,lat) array orientation
        et2 = et2';
        et2 = double(et2);
        % set bathymetry = negative topography
        et2 = -et2;
        % make grid 97x73 so that everything works;
        % invent some topography at the South Pole
        % repeat topography at the 180E/180W
        et2(73,:)  = 0.0;
        et2(:,97) = et2(:,1);
        % flip up-down
        et2 = flipud(et2);
        % Set up some latitude and longitude arrays
        tlon = 0:3.75:360;
        tlat = -90:2.5:90;
        % load "Mask for Ocean Temperature" (73x96)
        varid  = netcdf.inqVarID(ncid_topo,'maskt');
        mask_tmp = netcdf.getVar(ncid_topo,varid);
        maskt = double(mask_tmp);
        maskt(:,:,2:end) = [];
        maskt(find(maskt(:,:) == -99999.0)) = NaN;
        maskt = flipdim(maskt',1);
        % load "Mask for Ocean Velocity" (72x96)
        varid  = netcdf.inqVarID(ncid_topo,'masku');
        mask_tmp = netcdf.getVar(ncid_topo,varid);
        maskv = double(mask_tmp);
        maskv(:,:,2:end) = [];
        maskv(find(maskv(:,:) == -99999.0)) = NaN;
        maskv = flipdim(maskv',1);
    case 'foam'
        %%%%%%
    otherwise
        disp(['ERROR: Unknown GCM name.']);
        return;
end
%
% *** close netCDF file ************************************************* %
%
netcdf.close(ncid_topo);
%
% *** plot raw topo ***************************************************** %
%
topopal;
figure(1); clf
pcolor (tlon, tlat, et2); shading flat;
axis ([-180 180 -90 90]); axis image;
caxis ([-6500 6500]); h = colorbar ('horiz');
h2 = get(h, 'Title'); set(h2, 'String', 'Height above sea level [m]');
title ('Topography');
%
% *********************************************************************** %

% *********************************************************************** %
% *** CREATE TOPOGRAPHY ************************************************* %
% *********************************************************************** %
%
% *** Set up output grid ************************************************ %
%
[gln, glnm, glt, gltm, gdp, gtk, gar] = gold_grid(imax, jmax, kmax);
%
% *** Generate *.k1 file (i.e. sort out land-sea mask) ****************** %
%
% Create topography.  At this stage the script lets you alter the
% land-sea mask of the grid your (low) resolution generates
%
% Regriding involves FFT smoothing of the topography produced,
% and this is controllable via a parameter (par_sm below). The
% value of this ranges 0 to approximately 1.1.  0 smoothing
% produces an unsmoothed grid.  1.1 smoothing produces some-
% thing very sine-wave like (but still a "reasonable" grid).
% A value between 0.5 and 0.6 produces a topography quite
% similar to that normally used.
% Original default = 0.55
% GENIE paleo topos used a value of 0.01
%
% When sorting out the land-sea mask, several issues frequently
% arise (for modern topography) :
% - need to close gap between north and south America
% - need to close (for two island grid) Indonesian Throughflow
% - need to fill in any enclosed seas (e.g. Baltic, Black, Caspian,
%   Red Sea, Great Lakes, Hudson Bay, etc.)
% - need to remove/connect-to-Africa Madagascar
% - need to open communication with Mediterranean (one grid cell)
% - ensure Antarctic land mass fills first grid cell row
% - ensure Americas and Asia don't touch North Pole
% - open Bering Straits
% - submerge any islands you don't want (that can't be connected
%   to nearby land; e.g. Japan; Madagascar; New Zealand; Hawaii)
%
% At this stage, one only gets to alter the land-sea mask, later
% stages allow alteration of ocean depth itself
%
fname = 'draft'; % output filename used for *.k1 file
[out gmask] = gen_topo_7(tlon, tlat, et2(1:end-1,1:end-1), gln, glt, gdp, par_sm, fname);
%
if opt_windonly,
    fname1 = sprintf('%s.k1', fname);
    fname2 = fname;
    fname5 = fname1;
elseif opt_bathonly
    fname1 = sprintf('%s.k1', fname);
    fname2 = fname;
    fname5 = fname1;
else
    %
    % *** Generate *.paths, *.psiles files; edit *.k1 file ********** %
    %
    % Tips :
    % - a lot of information is imparted in the Matlab window, so make
    %   sure you can read this
    % - when it comes to editing topography, paths, etc. it's best to
    %   maximise the plot window to make clicking on the right cells
    %   easier (this is the voice of much past frustration speaking)
    % - I recommend either letting the computer guess unspecified
    %   runoff, or doing nothing about it (allowing it to guess ALL
    %   runoff really isn't clever - though it can be amusing)
    % - when it comes to enumerating islands, the "closed Indonesian
    %   Throughflow" grid has 2
    % - when it comes to identifying islands, make sure the first one
    %   you point as is the one with the most complicated path around
    %   it (otherwise you'll have to work out this path!)
    % - any unconnected islands (e.g. Greenland) that aren't identified
    %   as separate islands at this stage are classed (for the purposes
    %   of psiles, etc. as being part of island 1 - this can be useful
    % - when it comes to finding edges, both automatic and manual modes
    %   word, but it's usually better to go manual
    % - the routine prints out some helpful plots along the way, and
    %   you might want to save these for later
    %
    fname1 = sprintf('%s.k1', fname);
    gold_paths_4(fname1, [imax jmax kmax], fname2);
    %
end
%
% *** Display resulting topography and runoff field ************* %
%
fname5 = sprintf('%s.k1', fname2);
figure(1); clf
plot_k1(gln, glt, gdp, fname5);
titstr = sprintf('New grid : %d x %d x %d', imax, jmax, kmax);
paper; orient landscape;
stamp;
plotstr = sprintf('%s_topo', fname2);
print ('-dpsc', '-painters',  plotstr)
%
% *** Generate random drainage basins *********************************** %
%
% Modify runoff patterns in *.k1 file
if opt_randomrunoff && ~opt_windonly && ~opt_bathonly,
    make_runoff_rnd(fname2);
    fprintf('- Runoff patterns calculated\n')
end
%
% *********************************************************************** %
%
% *** Filter topography ***************************************** %
%
if opt_filtertopo,
    make_topo_filtered(fname2);
    fprintf('- Topography filtered\n')
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** Create ancillary data to force GOLDSTEIN model ******************** %
% *********************************************************************** %
%
% Note : these routines "flood fill" data fields so that points
% that contain land contain values, since the code will not
% make use of these values (I think!)
%
if ~opt_bathonly,
    %
    % *** Create wind ancillary files *********************************** %
    %
    [wstr, wspd, g_wspd] = gold_winds(par_gcm,gcm_path,expid,maskb,maskt,maskv,gmask,gln,glnm,glt,gltm,fname2);
    %
    % *** Have a look at these ancillary fields ************************* %
    %
    % Load up and organise model grid
    clear t1 t2
    t1 = load (fname5);
    t2 = flipud(t1);
    t3 = t2(2:end-1, 2:end-1);
    t3(end+1,:) = NaN; t3(:,end+1) = NaN;
    t4 = t3; t4(t4 > kmax) = NaN;
    t5 = t4(1:end-1,1:end-1);
    % Wind stress, wind velocity, and wind speed
    uspd = wspd(:,:,1); vspd = wspd(:,:,2);
    tspd = ((uspd .^ 2) + (vspd .^ 2)) .^ 0.5;
    tspd(end+1,:) = NaN; tspd(:,end+1) = NaN;
    uspd(isnan(t5)) = NaN; vspd(isnan(t5)) = NaN; tspd(isnan(t4)) = NaN;
    ustr = mean(wstr(:,:,1:2), 3); vstr = mean(wstr(:,:,3:4), 3);
    tstr = ((ustr .^ 2) + (vstr .^ 2)) .^ 0.5;
    tstr(end+1,:) = NaN; tstr(:,end+1) = NaN;
    ustr(isnan(t5)) = NaN; vstr(isnan(t5)) = NaN; tstr(isnan(t4)) = NaN;
    g_wspd(end+1,:) = NaN; g_wspd(:,end+1) = NaN;
    % plot wind velocity
    figure(3); clf
    pcolor(gln, glt, tspd); hold on;
    quiver (glnm, gltm, uspd, vspd, 'k-');
    set(gca,'XTick', -180:90:180);
    axis([-180 180 -90 90]);
    caxis ([0 10.0]); colorbar ('horiz');
    title ('Wind velocity [m s^{-1}]');
    paper; orient landscape;
    plotstr = sprintf('%s_windvelocity', fname2);
    print ('-dpsc', '-painters',  plotstr)
    % plot wind speed
    figure(3); clf
    pcolor(gln, glt, g_wspd); hold on;
    set(gca,'XTick', -180:90:180);
    axis([-180 180 -90 90]);
    caxis ([0 10.0]); colorbar ('horiz');
    title ('Wind speed [m s^{-1}]');
    paper; orient landscape;
    plotstr = sprintf('%s_windspeed', fname2);
    print ('-dpsc', '-painters',  plotstr)
    % plot wind stress
    figure(3); clf
    pcolor(gln, glt, tstr); hold on;
    quiver (glnm, gltm, ustr, vstr, 'k-');
    set(gca,'XTick', -180:90:180);
    axis([-180 180 -90 90]);
    caxis ([0 0.15]); colorbar ('horiz');
    title ('Wind stress [N m^{-2}]');
    paper; orient landscape;
    plotstr = sprintf('%s_windstress', fname2);
    print ('-dpsc', '-painters',  plotstr)
    %
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** GENERATE SEDIMENT (ONLY) GRID ************************************* %
% *********************************************************************** %
%
if opt_bathonly,
    make_topo_sed(kmax,kmax,'draft.k1',[fname2 '.' num2str(imax) num2str(jmax) num2str(kmax)]);
    fprintf('- Sediment topo generated\n')
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** END *************************************************************** %
% *********************************************************************** %
%
fprintf('- END\n');
%
% *********************************************************************** %
