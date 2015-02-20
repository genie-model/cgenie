function [] = plot_fields_biogem_3d_i(PEXP1,PEXP2,PVAR1,PVAR2,PT1,PT2,PIK,PMASK,PCSCALE,PCMIN,PCMAX,PCN,PDATA,POPT,PNAME)
% plot_fields_biogem_3d_i
%
%   *******************************************************************   %
%   *** biogem i-SECTION (LAT-LAY) + INTEGRATED PLOTTING  *************   %
%   *******************************************************************   %
%
%   plot_fields_biogem_3d_i(PEXP1,PEXP2,PVAR1,PVAR2,PT1,PT2,PIK,PMASK,PCSCALE,PCMIN,PCMAX,PCN,PDATA,POPT,PNAME)
%   plots slices and zonally averaged vertical sections from the BIOGEM 3-D
%   netCDF data file 'fields_biogem_3d.nc' and takes 15 arguments:
%
%   PEXP1 [STRING] (e.g. 'preindustrial_spinup')
%   --> the (first) experiment name
%   PEXP2 [STRING] [OPTIONAL] (e.g. 'enhanced_export')
%   --> the experiment name of 2nd, optional, netCDF file
%   --> leave EXP2 blank, i.e., '', for no second netCDF file
%   PVAR1 [STRING] (e.g. 'ocn_PO4')
%   --> id the name of 1st variable to be plotted
%   --> all valid valiable names will be listed if an invalid name is given
%   PVAR2 [STRING] [OPTIONAL] (e.g. 'ocn_NO3')
%   --> id the name of 2nd, optional, variable
%   PT1 [REAL] (e.g. 1999.5)
%   --> the (first) time-slice year
%   --> all valid years will be listed if an invalid year is given
%   PT2 [REAL] [OPTIONAL] (e.g. 0.5)
%   --> the year of the 2nd, optional, time-slice
%   --> set PT2 to -1 for no second time-slice
%   PIK [INTEGER] (e.g. 32)
%   --> the meridional section to be plotted (the 'i' slice)
%       (in the absence of a mask being specified)
%   PMASK [STRING] (e.g. 'mask_worjh2_Indian.dat')
%   --> the filename containing the meridional mask to construct the zonal average
%   --> the full filename must be give, including any extensions
%   --> leave MASK blank, i.e., '', for no mask
%   PCSCALE [REAL] (e.g. 1.0)
%   --> the scale factor for the plot
%       e.g., to plot in units of micro molar (umol kg-1), enter: 1e-6
%             to plot in units of PgC, enter: 1e15
%             to plot negative values, enter: -1
%   --> the plot is auto-scaled if a value of zero (0.0) is entered
%   PCMIN [REAL] (e.g. 0.0)
%   --> the minimum scale value
%   PCMAX [REAL] (e.g. 100.0)
%   --> the maxumum scale value
%   PCN [INTEGER] (e.g. 20)
%   --> the number of (contor) intervals between minimum and maximum
%       scale values
%   PDATA [STRING] (e.g. 'obs_d13C.dat')
%   --> the filename containing any overlay data set,
%       which must be formatted as (space) seperated columns of:
%       lat, depth, value
%   --> the full filename must be give, including any extensions
%   --> leave PDATA blank, i.e., '', for no overlay data
%   POPT [STRING] (e.g., 'plotting_config_2')
%   --> the string for an alternative plotting parameter set
%   --> if an empty (i.e., '') value is passed to this parameter
%       then the default parameter set is used
%   PNAME [STRING] (e.g., 'my_plot')
%   --> the string for an alternative filename
%   --> if an empty (i.e., '') value is passed to this parameter
%       then a filename is automatically generated
%
%   Example
%           plot_fields_biogem_3d_i('experiment_1','','ocn_PO4','',1994.5,-1,1,'mask_worjh2_Pacific.dat',1e-6,0.0,2.0,20,'','','')
%           will plot the time-slice cenetered on a time of 1994.5,
%           of PO4 concentrations zonally averaged according to
%           the mask file 'mask_worjh2_Pacific.dat',
%           between 0 and 2 umol kg-1 in 20 contour intervals
%           experiment is 'experiment_1'
%
%   *******************************************************************   %

% *********************************************************************** %
% ***** HISTORY ********************************************************* %
% *********************************************************************** %
%
%   11/05/30: Added time-stamping
%   12/02/10: added in options for: anomoly plotting; data-only plotting
%             some code reorganisation / rationalization
%             added overturning streamfubnction contour plotting
%   12/06/28: moved streamfunction contour plotting code so that netCDF
%             parameters needed for primary data plotting not over-written
%   12/10/16: updated HELP text
%   12/10/18: reordered parameter list
%   12/12/10: updated color bar drawing
%   12/12/13: minor update to streamfunction plotting
%             added data simple overlay addition
%             also added option to create means
%   12/12/14: adjusted plotting of single contour overlay
%             got full (3D netCDF data) anomoly plotting going
%             revised filename string
%   12/12/27: bug-fix in non re-gridded obs data (wrong levtor length)
%   12/12/27: added in Taylor diagram and data anomoly plotting
%             (adapted from plot_fields_biogem_3d_k.m)
%   13/01/02: bug-fix to highlight contour
%             added dashed negative contour option
%             fixed small buglette in gridded data plotting
%   13/04/26: added ability to plot MOC only
%             => MOC overlay + data only options (but no data)
%   13/08/12: updated stats calculation and output
%   13/08/29: fixed OPSI plotting (vertical axis orientation issue)
%   13/09/09: fixes/improvements to difference plotting
%   13/09/19: adjusted anomoly plotting and added 'no stats' option
%             adjusted filename
%             added scatter plot
%   13/10/06: created alt anomoly colormap and adjusted anomoly plotting
%             added invert color bar option
%   13/11/12: REORGANIZED USER SETTINGS [AND PASSED PARAMETER LIST]
%   13/11/15: bug-fixes and potted changes to start to bring consistent
%             with the other function revisions
%   13/11/18: MUTLAB bug-fix(?) for nmax<10 not displaying scatter colors
%   13/11/23: bug-fix on variable name change
%   13/12/04: minor bug fix
%   13/12/09: added topography masking to streamfunction
%   13/12/10: bug-fix for calculating stats between 2 fields with a mask
%   13/12/19: disabled target diagram for now to simplify things
%   13/12/23: added file format selection for 'new' plotting
%             fixed bug in model vs. model plotting for an i-slice
%             fixed axis lables in cross-plot and added regression line
%   13/12/24: added depth coloring of cross plot and depth range filtering
%   13/12/25: added R2 calc for cross-plot
%             fixed up model-data capabilities (e.g. correct masking)
%   14/01/02: fixed 2nd-order poly equation in cross-plot; also 'n' count
%   14/04/07: adjusted cross-plot depth color limits
%   14/04/11: added cross-plot data output
%             corrected bug in calculating 'n' (cross-plotting)
%   14/04/09: cross-plot depth scale bug fix
%   14/04/14: more minor bug-fixing ...
%   14/04/15: added alt filename
%             added options of not plotting main or secondary figures
%   14/04/17: altered (fixed?) criteria for not ploting the color scale
%   14/06/18: corrected help example
%             removel old color-bar plotting option
%   14/08/20: added '%'s to ASCII data output headers
%   14/09/15: set data input consistent with 3D 'k' section plotting
%             adjusted global mean plotting setting (now: iplot == 0)
%             added alternative (external) plotting scale option
%   14/09/28: developed k-interval restriction on data plotting and stats
%             (parameters plot_kmin and plot_kmax)
%   14/09/29: minor bug-fix to the above
%   14/11/09: auto plot format
%   14/12/01: incorporated cbrewer.m colormap schemes (& removed anom map)
%   14/12/03: removed cbrewer.m ... :o)
%   14/12/03: incorporated new make_cmap5 function
%   14/12/07: fixed dim error in flipping color scale
%   14/12/16: adjusted MOC (only) plotting
%   14/12/30: removed cross-plotting and
%             instead now call a stand-alone function
%   14/12/30: added nmax to stats output for dual 3D data input
%   15/01/07: adjusted parameter names for restricting k space data 
%             (now: data_kmin, data_kmax)
%   15/01/09: revised to use new make_cmap5.m (adjusted number of colors) 
%   15/01/11: replaced make_cmap5 -> make_cmap
%   15/01/12: adjusted orientation of data_vector_2
%             added capability for fine-tuning data shapes and colors
%   15/01/13: bug-fix of recent changes
%
% *********************************************************************** %

% *********************************************************************** %
% *** INITIALIZE PARAMETERS & VARIABLES ********************************* %
% *********************************************************************** %
%
% close open windows
% NOTE: don't clear variable space here ...
close all;
% load plotting options
if isempty(POPT), POPT='plot_fields_settings'; end
eval(POPT);
% set axes
lat_min = -090;
lat_max = +090;
D_min   = 0000;
D_max   = 5000;
zt_min = 0;
zt_max = 5000;
% set passed parameters
exp_1 = PEXP1;
exp_2 = PEXP2;
timesliceid_1 = PT1;
timesliceid_2 = PT2;
dataid_1 = PVAR1;
dataid_2 = PVAR2; %%%
iplot = PIK;
data_scale = PCSCALE;
con_min = PCMIN;
con_max = PCMAX;
con_n = PCN;
maskid = PMASK;
overlaydataid = PDATA;
altfilename = PNAME;
% set default data scaling
if data_scale == 0.0
    data_scale = 1.0;
    clear con_min;
    clear con_max;
    con_n = 10;
end
% set global flag if no alt plotting scale is set
% NOTE: catch possibility of one axis being set, but the other @ default
%       (min and max with indetical values)
if ((plot_lat_min == plot_lat_max) && (plot_D_min == plot_D_max)),
    plot_global = true;
    plot_xy_scaling = 1.0;
else
    plot_global = false;
    if (plot_lat_min == plot_lat_max),
        plot_lat_min = lat_min;
        plot_lat_max = lat_max;
    end
    if (plot_D_min == plot_D_max),
        plot_D_min = D_min;
        plot_D_max = D_max;
    end
    plot_xy_scaling = ((plot_D_max - plot_D_min)/(D_max - D_min)) / ((plot_lat_max - plot_lat_min)/(lat_max - lat_min));
end
% set date
str_date = [datestr(date,11), datestr(date,5), datestr(date,7)];
% set function name
str_function = 'plot-fields-biogem-3d(i)';
% plot format
if ~isempty(plot_format), plot_format_old='n'; end
% plotting paths
addpath(library_path1);
if (plot_format_old == 'n'),
    addpath(library_path2);
    addpath(library_path3);
end
%
% *** OPTIONAL PLOTTING SCALE ******************************************* %
%
if ~isempty(contour_file),
    % load data
    contour_data = load(contour_file,'-ascii');
    % adjust if necessary so that contour_data(1) is the lowest value
    if (contour_data(1) > contour_data(end)), contour_data = flipud(contour_data); end
    % adjust number of contours
    % NOTE: remember that con_n is the number of intervals, not the number of contours (which  is con_n+1)
    con_n = length(contour_data) - 1;
    % set max,min limits
    con_min = contour_data(1);
    con_max = contour_data(end);
end
%
% *** DEFINE COLORS ***************************************************** %
%
% define contonental color
color_g = [0.75 0.75 0.75];
% define no-data color
color_b = [0.00 0.00 0.00];
%
% *********************************************************************** %

% *********************************************************************** %
% *** INITIALIZE ARRAYS ************************************************* %
% *********************************************************************** %
%
xm = [];
ym = [];
zm = [];
lonm = [];
lone = [];
lonw = [];
latm = [];
latn = [];
lats = [];
laym = [];
layt = [];
layb = [];
rawdata=[];
data_0=[];
data_1=[];
data_2=[];
%
% *********************************************************************** %

% *********************************************************************** %
% *** OPEN netCDF DATA & LOAD (OPTIONAL) MASK FILE ********************** %
% *********************************************************************** %
%
% open netCDF file
ncid_1=netcdf.open([data_path '/' exp_1 '/biogem/fields_biogem_3d.nc'],'nowrite');
% read netCDf information
[ndims,nvars,ngatts,unlimdimid] = netcdf.inq(ncid_1);
% load mask data
% NOTE: flip in j-direction to make consistent with netCDF grid
maskfile = maskid;
if ~isempty(maskid)
    mask = load(maskfile,'-ascii');
    mask = flipdim(mask,1);
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** SET UP GRID ******************************************************* %
% *********************************************************************** %
%
% load grid data
varid  = netcdf.inqVarID(ncid_1,'grid_level');
grid_k1 = netcdf.getVar(ncid_1,varid);
% flip array around diagonal to give (j,i) array orientation
grid_k1 = grid_k1';
% calculate grid dimensions
varid  = netcdf.inqVarID(ncid_1,'lat');
[dimname, dimlen] = netcdf.inqDim(ncid_1,varid);
jmax = dimlen;
varid  = netcdf.inqVarID(ncid_1,'lon');
[dimname, dimlen] = netcdf.inqDim(ncid_1,varid);
imax = dimlen;
varid  = netcdf.inqVarID(ncid_1,'zt');
[dimname, dimlen] = netcdf.inqDim(ncid_1,varid);
kmax = dimlen;
% load remaining grid information
varid  = netcdf.inqVarID(ncid_1,'zt');
grid_zt = netcdf.getVar(ncid_1,varid);
grid_zt = flipud(grid_zt);
varid  = netcdf.inqVarID(ncid_1,'zt_edges');
grid_zt_edges = netcdf.getVar(ncid_1,varid);
grid_zt_edges = flipud(grid_zt_edges);
% calculate topography
for i = 1:imax,
    for j = 1:jmax,
        if grid_k1(j,i) <= kmax
            topo(j,i) = -grid_zt_edges(grid_k1(j,i));
        else
            topo(j,i) = 0.0;
        end
    end
end
if ~isempty(maskid)
    topo = mask.*topo;
elseif (iplot == 0)
    mask = zeros(jmax,imax);
    mask(:,:) = 1.0;
    topo = mask.*topo;
else
    if ((iplot > imax) || (iplot < 0))
        disp([' ']);
        error('*WARNING*: Value of iplot out-of-range (0 to imax): ENDING ... ');
    end
    mask = zeros(jmax,imax);
    mask(:,iplot) = 1.0;
    topo = mask.*topo;
end
% load and calculate remaining grid information
varid  = netcdf.inqVarID(ncid_1,'lat');
grid_lat = netcdf.getVar(ncid_1,varid);
varid  = netcdf.inqVarID(ncid_1,'lon');
grid_lon = netcdf.getVar(ncid_1,varid);
[latm laym] = meshgrid(grid_lat,-grid_zt);
varid  = netcdf.inqVarID(ncid_1,'lat_edges');
grid_lat_edges = netcdf.getVar(ncid_1,varid);
varid  = netcdf.inqVarID(ncid_1,'lon_edges');
grid_lon_edges = netcdf.getVar(ncid_1,varid);
[lats layb] = meshgrid(grid_lat_edges(1:jmax),-grid_zt_edges(1:kmax));
[latn layt] = meshgrid(grid_lat_edges(2:jmax+1),-grid_zt_edges(2:kmax+1));
% create cell volume array; also depth
% NOTE: assume equal area grid, normaalized area
% NOTE: multiple by 1.0 becasue ... (?) (to ensure correct format?)
data_V = zeros(kmax,jmax,imax);
data_D = zeros(kmax,jmax,imax);
for k = 1:kmax,
    data_V(k,:,:) = 1.0*(grid_zt_edges(k) - grid_zt_edges(k+1));
    data_D(k,:,:) = 1.0*grid_zt(k);
end
%
grid_lon_origin = grid_lon_edges(1);
% set restricted k interval
if ((data_kmin ~= data_kmax) || (data_kmin ~= 0)),
    loc_kmin = data_kmin;
    loc_kmax = data_kmax;
else
    loc_kmin = 1;
    loc_kmax = kmax;
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** SET PRIMARY GRIDDED DATASET *************************************** %
% *********************************************************************** %
%
% check that the year exists
varid  = netcdf.inqVarID(ncid_1,'time');
timeslices = netcdf.getVar(ncid_1,varid);
[dimname, dimlen] = netcdf.inqDim(ncid_1,varid);
clear time;
while exist('time','var') == 0
    for n = 1:dimlen,
        if double(int32(100*timeslices(n)))/100 == timesliceid_1
            time = timesliceid_1;
            tid_1 = n;
        end
    end
    if exist('time','var') == 0
        disp('   > WARNING: Year #1 must be one of the following;');
        format long g;
        double(int32(100*timeslices(:)))/100
        format;
        timesliceid_1 = input('   > Time-slice year: ');
    end
end
% check that the variable name exists
varid_1 = [];
while isempty(varid_1)
    for n = 0:nvars-1,
        [varname,xtype,dimids,natts] = netcdf.inqVar(ncid_1,n);
        if strcmp(varname,dataid_1)
            varid_1 = n;
        end
    end
    if isempty(varid_1)
        disp('   > WARNING: Variable #1 name must be one of the following;');
        for n = 0:nvars-1,
            [varname,xtype,dimids,natts] = netcdf.inqVar(ncid_1,n);
            varname
        end
        dataid_1 = input('   > Variable name: ','s');
    end
end
% load data
% flip array around diagonal to give (j,i) array orientation
[varname,xtype,dimids,natts] = netcdf.inqVar(ncid_1,varid_1);
rawdata = netcdf.getVar(ncid_1,varid_1);
if length(dimids) == 4
    rawdata(1:imax,1:jmax,1:kmax) = rawdata(1:imax,1:jmax,1:kmax,tid_1);
    for n = 1:kmax,
        data_1(kmax - n + 1,1:jmax,1:imax) = rawdata(1:imax,1:jmax,n)';
    end
elseif length(dimids) == 3
    rawdata(1:imax,1:jmax,1:kmax) = rawdata(1:imax,1:jmax,1:kmax);
    for n = 1:kmax,
        data_1(kmax - n + 1,1:jmax,1:imax) = rawdata(1:imax,1:jmax,n)';
    end
elseif length(dimids) == 2
    rawdata(1:imax,1:jmax) = rawdata(1:imax,1:jmax);
    data_1(1:jmax,1:imax) = rawdata(1:imax,1:jmax)';
else
    data_1(:,:,:) = NaN;
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** SET ALTERNATIVE GRIDDED DATASET *********************************** %
% *********************************************************************** %
%
% *** ALT EXPERIMENT **************************************************** %
%
% open new netCDF file if necessary, else reuse 1st netCDF dataset
if ~isempty(exp_2)
    % open netCDF file
    ncid_2 = netcdf.open([data_path '/' exp_2 '/biogem/fields_biogem_3d.nc'],'nowrite');
    % read netCDf information
    [ndims,nvars,ngatts,unlimdimid] = netcdf.inq(ncid_2);
    % load data
    varid_2 = netcdf.inqVarID(ncid_2,dataid_2);
    [varname,xtype,dimids,natts] = netcdf.inqVar(ncid_2,varid);
    rawdata = netcdf.getVar(ncid_2,varid_2);
else
    ncid_2 = ncid_1;
end
%
% *** ALT TIME-SLICE **************************************************** %
%
if timesliceid_2 >= 0.0
    % check that the year exists
    varid  = netcdf.inqVarID(ncid_2,'time');
    timeslices = netcdf.getVar(ncid_2,varid);
    [dimname, dimlen] = netcdf.inqDim(ncid_2,varid);
    clear time;
    while exist('time','var') == 0
        for n = 1:dimlen,
            if double(int32(100*timeslices(n)))/100 == timesliceid_2
                time = timesliceid_2;
                tid_2 = n;
            end
        end
        if exist('time','var') == 0
            disp('   > WARNING: Year #2 must be one of the following;');
            format long g;
            double(int32(100*timeslices(:)))/100
            format;
            timesliceid_2 = input('   > Time-slice year: ');
        end
    end
else
    tid_2 = tid_1;
end
%
% *** ALT DATA FIELD **************************************************** %
%
if ~isempty(dataid_2)
    % check that the variable name exists
    varid_2 = [];
    while isempty(varid_2)
        for n = 0:nvars-1,
            [varname,xtype,dimids,natts] = netcdf.inqVar(ncid_2,n);
            if strcmp(varname,dataid_2)
                varid_2 = n;
            end
        end
        if isempty(varid_2)
            disp('   > WARNING: Variable #2 name must be one of the following;');
            for n = 0:nvars-1,
                [varname,xtype,dimids,natts] = netcdf.inqVar(ncid_2,n);
                varname
            end
            dataid_2 = input('   > Variable name: ','s');
        end
    end
else
    varid_2 = varid_1;
end
%
% *** SET DATA ********************************************************** %
%
if (~isempty(exp_2)) || (timesliceid_2 >= 0.0) || (~isempty(dataid_2)),
    % load data
    [varname,xtype,dimids,natts] = netcdf.inqVar(ncid_2,varid_2);
    rawdata = netcdf.getVar(ncid_2,varid_2);
    if length(dimids) == 4
        rawdata(1:imax,1:jmax,1:kmax) = rawdata(1:imax,1:jmax,1:kmax,tid_2);
        for n = 1:kmax,
            data_2(kmax - n + 1,1:jmax,1:imax) = rawdata(1:imax,1:jmax,n)';
        end
    elseif length(dimids) == 3
        rawdata(1:imax,1:jmax,1:kmax) = rawdata(1:imax,1:jmax,1:kmax);
        for n = 1:kmax,
            data_2(kmax - n + 1,1:jmax,1:imax) = rawdata(1:imax,1:jmax,n)';
        end
    elseif length(dimids) == 2
        rawdata(1:imax,1:jmax) = rawdata(1:imax,1:jmax);
        data_2(1:jmax,1:imax) = rawdata(1:imax,1:jmax)';
    else
        data_2(:,:,:) = NaN;
    end
    data_anomoly = 'y';
else
    data_2(:,:,:) = 0.0;
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** STREAMFUNCTION **************************************************** %
% *********************************************************************** %
%
% open streamfunction data (if selected)
if ~isempty(plot_opsi)
    % open netCDF file
    ncid_0=netcdf.open([data_path '/' exp_1 '/biogem/fields_biogem_2d.nc'],'nowrite');
    % read netCDf information
    [ndims,nvars,ngatts,unlimdimid] = netcdf.inq(ncid_0);
    % load grid information
    varid  = netcdf.inqVarID(ncid_0,'zt');
    [dimname, dimlen] = netcdf.inqDim(ncid_0,varid);
    zmax = dimlen;
    opsigrid_zt = flipud(netcdf.getVar(ncid_0,varid));
    varid  = netcdf.inqVarID(ncid_0,'lat');
    opsigrid_lat = netcdf.getVar(ncid_0,varid);
    [latm ztm] = meshgrid(grid_lat,-grid_zt);
    varid  = netcdf.inqVarID(ncid_0,'zt_edges');
    opsigrid_zt_edges = flipud(netcdf.getVar(ncid_0,varid));
    varid  = netcdf.inqVarID(ncid_0,'lat_edges');
    opsigrid_lat_edges = flipud(netcdf.getVar(ncid_0,varid));
    [opsilats zts] = meshgrid(opsigrid_lat_edges(1:jmax),opsigrid_zt_edges(1:zmax));
    [opsilatn ztn] = meshgrid(opsigrid_lat_edges(2:jmax+1),opsigrid_zt_edges(2:zmax+1));
    % set variable name
    switch plot_opsi
        case {'g'}
            varid  = netcdf.inqVarID(ncid_0,'phys_opsi');
        case 'a'
            varid  = netcdf.inqVarID(ncid_0,'phys_opsia');
        case 'p'
            varid  = netcdf.inqVarID(ncid_0,'phys_opsip');
        otherwise
            disp('Unknown opsi definition.')
    end
    % open data
    [varname,xtype,dimids,natts] = netcdf.inqVar(ncid_0,varid);
    rawdata = netcdf.getVar(ncid_0,varid);
    if length(dimids) == 3
        rawdata = rawdata(1:jmax,1:zmax,tid_1);
        data_0 = rawdata(1:jmax,1:zmax)';
    elseif length(dimids) == 2
        rawdata = rawdata(1:jmax,1:zmax);
        data_0 = rawdata(1:jmax,1:zmax)';
    else
        data_0 = NaN*data_0;
    end
    % invert data to match grid
    data_0=flipud(data_0);
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** SET OUTPUT FILESTRING ********************************************* %
% *********************************************************************** %
%
% create an output filestring for data and plot saving
if ~isempty(maskid)
    if (~isempty(exp_2)) || (timesliceid_2 >= 0.0) || (~isempty(dataid_2))
        filename = [exp_1, '.', 'y', num2str(timesliceid_1), '.', dataid_1, '_MINUS_', exp_2, '.', 'y', num2str(timesliceid_2), '.', dataid_2, '.', maskid];
    else
        filename = [exp_1, '.', 'y', num2str(timesliceid_1), '.', dataid_1, '.', maskid];
    end
else
    if (~isempty(exp_2)) || (timesliceid_2 >= 0.0) || (~isempty(dataid_2))
        filename = [exp_1, '.', 'y', num2str(timesliceid_1), '.', dataid_1, '_MINUS_', exp_2, '.', 'y', num2str(timesliceid_2), '.', dataid_2, '.', 'i', num2str(iplot)];
    else
        filename = [exp_1, '.', 'y', num2str(timesliceid_1), '.', dataid_1, '.', 'i', num2str(iplot)];
    end
end
if ~isempty(overlaydataid),
    filename = [filename, '_VS_', overlaydataid];
    if (data_anomoly == 'y'),
        filename = [filename, '.ANOM'];
    end
    if (data_only == 'y'),
        filename = [filename, '.DO'];
    end
end
if (~isempty(altfilename)), filename = altfilename; end
%
% *********************************************************************** %

% *********************************************************************** %
% *** FILTER & PROCESS RAW DATA ***************************************** %
% *********************************************************************** %
%
% *** INITALIZE ********************************************************* %
%
xm = latm;
ym = laym;
if ( (data_anomoly == 'y') || (~isempty(exp_2)) || (timesliceid_2 >= 0.0) || (~isempty(dataid_2)) ),
    data = data_1 - data_2;
else
    data = data_1;
    data_2 = data_1;
end
data = data - data_offset;
if ~isempty(plot_opsi)
    opsiym = ztm;
    opsidata = data_0;
end
% define initial array sizes
zm = zeros(kmax,jmax);
zm_count = zm;
zl = zeros(kmax,1);
zl_V = zl;
zz = zeros(kmax,jmax);
zz_V = zz;
n = 0;
%
% *** PROCESS MAIN DATASET ********************************************** %
%
for k = 1:kmax,
    for j = 1:jmax,
        for i = 1:imax,
            if (topo(j,i) < -grid_zt(k))
                if ((data(k,j,i) > -0.999E19) && (data(k,j,i) < 0.999E19))
                    zm(k,j) = zm(k,j) + data(k,j,i);
                    zm_count(k,j) = zm_count(k,j) + 1.0;
                    zl(k) = zl(k) + data_V(k,j,i)*data(k,j,i);
                    zl_V(k) = zl_V(k) + data_V(k,j,i);
                    zz(k,j) = zz(k,j) + data_V(k,j,i)*data(k,j,i);
                    zz_V(k,j) = zz_V(k,j) + data_V(k,j,i);
                else
                    data(k,j,i) = NaN;
                    data_1(k,j,i) = NaN;
                    data_2(k,j,i) = NaN;
                    data_D(k,j,i) = NaN;
                end
            else
                data(k,j,i) = NaN;
                data_1(k,j,i) = NaN;
                data_2(k,j,i) = NaN;
                data_D(k,j,i) = NaN;
            end
        end
        if (zm_count(k,j) > 0.0)
            if data_log10 == 'y'
                if (zm(k,j) > 0.0)
                    zm(k,j) = log10(zm(k,j)/data_scale/zm_count(k,j));
                else
                    zm(k,j) = NaN;
                end
            else
                zm(k,j) = zm(k,j)/data_scale/zm_count(k,j);
                if contour_noneg == 'y'
                    if (zm(k,j) < 0.0)
                        zm(k,j) = 0.0;
                    end
                end
            end
        else
            zm(k,j) = NaN;
        end
        if ~isnan(zm(k,j)), n = n + 1; end
        if (zz_V(k,j) > 0.0)
            zz(k,j) = zz(k,j)/data_scale/zz_V(k,j);
        else
            zz(k,j) = NaN;
        end
    end
    if (zl_V(k) > 0.0)
        zl(k) = zl(k)/data_scale/zl_V(k);
    else
        zl(k) = NaN;
    end
end
nmax = n;
% copy zm before it gets transformed ...
overlaydata_zm(:,:) = zm(:,:);
% set topography uniform in i-direction and equal to deepest point found
for j = 1:jmax,
    for i = 2:imax,
        if (topo(j,i) < topo(j,i-1))
            topo(j,1:i-1) = topo(j,i);
        else
            topo(j,i) = topo(j,i-1);
        end
    end
end
%
% *** PROCESS OVERTURNING STREAMFUNCTION (IF SELECTED) ****************** %
%
if ~isempty(plot_opsi)
    opsizm = zeros(zmax,jmax);
    for j = 1:jmax,
        for z = 1:zmax,
            if (opsidata(z,j) < -1.0E6) || (opsidata(z,j) > 1.0E30)
                opsizm(z,j) = NaN;
            else
                opsizm(z,j) = opsidata(z,j);
            end
            if isnan (zm(z,j)), opsizm(z,j) = NaN; end
        end
    end
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** LOAD (OPTIONAL) OVERLAY DATA ************************************** %
% *********************************************************************** %
%
if ~isempty(overlaydataid)
    if (data_ijk == 'y')
        disp([' ']);
        error('*WARNING*: Currently there is no facility for loading data in GENIE (i,j,k) format (it must be explicit in lon/lat/depth): ENDING ... ');
    end
    % load overlay data
    overlaydatafile = [overlaydataid];
    fid = fopen(overlaydatafile);
    if (data_shapecol == 'n'),
        % lon, lat, depth, value, LABEL
        C = textscan(fid, '%f %f %f %f %s', 'CommentStyle', '%');
        overlaydata_raw = cell2mat(C(1:4));
        CC = C(5);
        overlaylabel_raw = char(CC{1}(:));
    else
        % lon, lat, depth, value, LABEL, SHAPE, EDGE COLOR, FILL COLOR
        C = textscan(fid, '%f %f %f %f %s %s %s %s', 'CommentStyle', '%');
        overlaydata_raw = cell2mat(C(1:4));
        CC = C(5);
        overlaylabel_raw = char(CC{1}(:));
        CC = C(6);
        overlaydata_shape = char(CC{1}(:));
        CC = C(7);
        overlaydata_ecol = char(CC{1}(:));
        CC = C(8);
        overlaydata_fcol = char(CC{1}(:));
    end
    fclose(fid);
    overlaydata_size = size(overlaydata_raw);
    nmax=overlaydata_size(1);
    if (overlaydata_size(2) == 3)
        disp([' ']);
        error('*WARNING*: Longitude must be specified in the data (even if as dummmy values): ENDING ... ');
    end
    % determine equivalent (i,j,k)
    % NOTE: filter out masked
    overlaydata_ijk(:,:) = zeros(size(overlaydata_raw(:,:)));
    for n = 1:nmax,
        overlaydata_ijk(n,1:2) = calc_find_ij(overlaydata_raw(n,1),overlaydata_raw(n,2),grid_lon_origin,imax,jmax);
        overlaydata_ijk(n,3)   = calc_find_k(overlaydata_raw(n,3),kmax);
        if ( isnan(data(overlaydata_ijk(n,3),overlaydata_ijk(n,2),overlaydata_ijk(n,1))) ),
            overlaydata_raw(n,4) = NaN;
            overlaydata_ijk(n,4) = NaN;
        elseif ( (overlaydata_ijk(n,3) < loc_kmin) || (overlaydata_ijk(n,3) > loc_kmax) ),
            overlaydata_raw(n,4) = NaN;
            overlaydata_ijk(n,4) = NaN;
        else
            overlaydata_ijk(n,4)   = overlaydata_raw(n,4);
        end
    end
    % remove data in land cells
    if (data_land == 'n')
        for n = 1:nmax,
            if (isnan(overlaydata_zm(overlaydata_ijk(n,3),overlaydata_ijk(n,2))))
                overlaydata_raw(n,4) = NaN;
                overlaydata_ijk(n,4) = NaN;
            end
        end
    end
    overlaydata_raw(isnan(overlaydata_raw(:,4)),:) = [];
    overlaydata_ijk(isnan(overlaydata_ijk(:,4)),:) = [];
    % update value of nmax
    nmax=length(overlaydata_raw);
    % change: +vs depths to -vs for plotting
    overlaydata_raw(find(overlaydata_raw(:,3)>0.0),3) = -1.0*overlaydata_raw(find(overlaydata_raw(:,3)>0.0),3);
    % BLAH
    overlaylabel(:,:) = overlaylabel_raw(:,:);
    overlaydata(:,:) = overlaydata_raw(:,:);
    % grid (and average per cell) data if requested
    % NOTE: data vector length is re-calculated and the value of nmax reset
    if (data_ijk_mean == 'y')
        overlaydata_ijk_old(:,:) = overlaydata_ijk(:,:);
        overlaydata_ijk(:,:) = [];
        overlaydata(:,:)    = [];
        m=0;
        for k = loc_kmin:loc_kmax,
            for j = 1:jmax,
                if (~isnan(overlaydata_zm(k,j)))
                    samecell_locations = find((int32(overlaydata_ijk_old(:,3))==k)&(int32(overlaydata_ijk_old(:,2))==j));
                    samecell_n = size(samecell_locations);
                    if (samecell_n(1) > 0)
                        m=m+1;
                        samecell_mean = mean(overlaydata_ijk_old(samecell_locations,4));
                        overlaydata_ijk(m,:) = [0 j k samecell_mean];
                        overlaydata(m,1) = grid_lon_origin + 360.0*(overlaydata_ijk(m,1) - 0.5)/jmax;
                        overlaydata(m,2) = 180.0*asin(2.0*(overlaydata_ijk(m,2) - 0.5)/jmax - 1.0)/pi;
                        overlaydata(m,3) = double(laym(k,j));
                        overlaydata(m,4) = samecell_mean;
                    end
                end
            end
        end
        nmax=m;
    end
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** DATA PROCESSING AND STATS ***************************************** %
% *********************************************************************** %
%
% calculate stats needed for Taylor Diagram (and plot it!)
%
% *** 3D (GRIDDED) DATA ************************************************* %
%
if (~isempty(dataid_2))
    % transform data sets in vectors
    % NOTE: data_1 is format (k,j,i)
    if ((iplot > 0) && isempty(maskid)),
        data_vector_1 = reshape(data_1(:,:,iplot),jmax*kmax,1);
        data_vector_2 = reshape(data_2(:,:,iplot),jmax*kmax,1);
    else
        data_vector_1 = reshape(data_1(:,:,:),imax*jmax*kmax,1);
        data_vector_2 = reshape(data_2(:,:,:),imax*jmax*kmax,1);
    end
    % filter data
    data_vector_1(find(data_vector_1(:) < -1.0E6)) = NaN;
    data_vector_1(find(data_vector_1(:) > 0.9E36)) = NaN;
    data_vector_2(find(data_vector_2(:) < -1.0E6)) = NaN;
    data_vector_2(find(data_vector_2(:) > 0.9E36)) = NaN;
    if isempty(overlaydataid), nmax = length(data_vector_2); end
    % calculate stats
    % NOTE: STATM = allstats(Cr,Cf)
    % 	    STATM(1,:) => Mean
    % 	    STATM(2,:) => Standard Deviation (scaled by N)
    % 	    STATM(3,:) => Centered Root Mean Square Difference (scaled by N)
    % 	    STATM(4,:) => Correlation
    %       STATM(5,:) => N
    if (data_stats=='y'),
        STATM = calc_allstats(data_vector_1,data_vector_2);
        % plot Taylor diagram
        taylordiag_vargout = plot_taylordiag(STATM(2,1:2),STATM(3,1:2),STATM(4,1:2));
        print('-depsc2', [filename, '_TaylorDiagram.', str_date, '.eps']);
        %%%% plot Target diagram
        %%%targetdiag_vargout = plot_target(STATM(7,1:2),STATM(8,1:2),'r',1.0,[],[]);
        %%%print('-depsc2', [filename, '_TargetDiagram.', str_date, '.eps']);
    end
end
%
% *** DISCRETE DATA ***************************************************** %
%
% NOTE: no scale transformation has been appplied
%       to either gridded or % overlay data
% NOTE: valid only for data on a single depth level
if (~isempty(overlaydataid)),
    % set overlay data vector
    data_vector_1 = overlaydata(:,4)/data_scale;
    % populate the gridded dataset vector with values corresponding to
    % the overlay data locations
    % NOTE: !!! overlaydata_zm is (k,j) !!!
    % NOTE: re-orientate data_vector_2 to match data_vector_1
    for n = 1:nmax,
        data_vector_2(n) = overlaydata_zm(int32(overlaydata_ijk(n,3)),int32(overlaydata_ijk(n,2)));
    end
    data_vector_2 = data_vector_2';
    % filter data
    data_vector_2(find(data_vector_2(:) < -1.0E6)) = NaN;
    data_vector_2(find(data_vector_2(:) > 0.9E36)) = NaN;
    % calculate stats
    if (data_stats=='y'),
        STATM = calc_allstats(data_vector_1,data_vector_2);
        % plot Taylor diagram
        taylordiag_vargout = plot_taylordiag(STATM(2,1:2),STATM(3,1:2),STATM(4,1:2));
        print('-depsc2', [filename, '_TaylorDiagram.', str_date, '.eps']);
        %%%% plot Target diagram
        %%%targetdiag_vargout = plot_target(STATM(7,1:2),STATM(8,1:2),'r',1.0,[],[]);
        %%%print('-depsc2', [filename, '_TargetDiagram.', str_date, '.eps']);
    end
end
%
% *** SAVE STATS DATA *************************************************** %
%
if (data_stats=='y'),
    if (~isempty(dataid_2) || (~isempty(overlaydataid) && data_only=='n')),
        fid = fopen([filename '_STATS' '.dat'], 'wt');
        fprintf(fid, 'Number of data points, N                           : %4i \n', nmax);
        fprintf(fid, '\n');
        fprintf(fid, 'Stats summary: reference data');
        fprintf(fid, '\n');
        fprintf(fid, 'Mean                                               : %8.6e \n', STATM(1,1));
        fprintf(fid, 'Standard Deviation (scaled by N)                   : %8.6e \n', STATM(2,1));
        fprintf(fid, 'Centered Root Mean Square Difference (scaled by N) : %8.6e \n', STATM(3,1));
        fprintf(fid, 'Correlation                                        : %8.6e \n', STATM(4,1));
        fprintf(fid, '\n');
        fprintf(fid, 'Stats summary: model data');
        fprintf(fid, '\n');
        fprintf(fid, 'Mean                                               : %8.6e \n', STATM(1,2));
        fprintf(fid, 'Standard Deviation (scaled by N)                   : %8.6e \n', STATM(2,2));
        fprintf(fid, 'Centered Root Mean Square Difference (scaled by N) : %8.6e \n', STATM(3,2));
        fprintf(fid, 'Correlation                                        : %8.6e \n', STATM(4,2));
        fclose(fid);
    end
end
%
% *** SAVE EQUIVALENT MODEL DATA **************************************** %
%
% save model data at the data locations
if (~isempty(overlaydataid) && (data_only == 'n'))
    fid = fopen([filename '_MODELPOINTS', '.', str_date '.dat'], 'wt');
    fprintf(fid, '%% Model value at data locations');
    fprintf(fid, '\n');
    fprintf(fid, '%% Format: i, j, k, lon, lat, depth, value, label');
    fprintf(fid, '\n');
    for n = 1:nmax,
        fprintf(fid, '%d %d %d %8.3f %8.3f %8.3f %8.6e %s \n', int16(overlaydata_ijk(n,1)), int16(overlaydata_ijk(n,2)), int16(overlaydata_ijk(n,3)), overlaydata(n,1), 90.0*sin(overlaydata(n,2)), overlaydata(n,3), data_vector_2(n), overlaylabel(n,:));
    end
    fclose(fid);
end
%
% *** CREATE DATA VECTOR FOR HISTOGRAM ********************************** %
%
if iplot > 0
    data_vector = reshape(data(:,iplot,:),jmax*kmax,1);
    data_vector_V = reshape(data_V(:,iplot,:),jmax*kmax,1);
else
    data_vector = reshape(data(:,:,:),imax*jmax*kmax,1);
    data_vector_V = reshape(data_V(:,:,:),imax*jmax*kmax,1);
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** ANOMOLY PLOTTING DATA ADJUSTMENTS ********************************* %
% *********************************************************************** %
%
if ~isempty(overlaydataid)
    % calculate molde-data anomoly
    if (data_anomoly == 'y')
        overlaydata(:,4) = data_vector_2(:) - data_vector_1(:);
    end
end
% redefine model grid location values so as to all appear white
if (data_only == 'y')
    zm = zeros(size(zm(:,:)));
    zm(find(zm(:,:) == 0)) = NaN;
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** PLOT MAIN FIGURE ************************************************** %
% *********************************************************************** %
%
if (plot_main == 'y'),
    %
    % *** CONFIGURE AND CREATE PLOTTING WINDOW ************************** %
    %
    % create figure
    scrsz = get(0,'ScreenSize');
    figure('Position',[((1 - plot_dscrsz)/2)*plot_dscrsz*scrsz(3) (1 - plot_dscrsz)*plot_dscrsz*scrsz(4) plot_dscrsz*scrsz(3) 0.80*plot_dscrsz*scrsz(4)])
    clf;
    % define plotting regions
    if (plot_format_old == 'y')
        fh(1) = axes('Position',[0 0 1 1],'Visible','off');
        fh(2) = axes('Position',[0.10 0.10 0.60 0.80]);
        fh(3) = axes('Position',[0.80 0.27 0.20 0.46],'Visible','off');
    else
        fh(1) = axes('Position',[0 0 1 1],'Visible','off');
        fh(2) = axes('Position',[0.075 0.125 0.60 0.70]);
        fh(3) = axes('Position',[0.725 0.125 0.15 0.70],'Visible','off');
    end
    % define colormap
    cmap = make_cmap(colorbar_name,con_n+2);
    if (colorbar_inv == 'y'), cmap = flipdim(cmap,1); end,
    colormap(cmap);
    % date-stamp plot
    set(gcf,'CurrentAxes',fh(1));
    text(0.95,0.50,[str_function, ' / ', 'on: ', str_date],'FontName','Arial','FontSize',8,'Rotation',90.0,'HorizontalAlignment','center','VerticalAlignment','top');
    %
    % *** SET PLOT SCALE ************************************************ %
    %
    % set minimum contour value
    if exist('con_min','var') == 0
        con_min = min(min(zm));
    end
    % set maximum contour value
    if exist('con_max','var') == 0
        con_max = max(max(zm));
    end
    % ensure min and max are not identical ...
    if con_min == con_max
        if con_max == 0.0
            con_max = 1.0;
        else
            con_min = (1.00/1.01)*con_min;
            con_max = (1.01)*con_max;
        end
    end
    % if min > max, then reverse min and max
    if con_min > con_max
        con_min_TEMP = con_min;
        con_max_TEMP = con_max;
        con_min = con_max_TEMP;
        con_max = con_min_TEMP;
    end
    %
    % *** CREATE MAIN PLOT ********************************************** %
    %
    set(gcf,'CurrentAxes',fh(2));
    hold on;
    % set color and lat/lon axes and labels
    caxis([con_min-(con_max-con_min)/con_n con_max]);
    set(gca,'PlotBoxAspectRatio',[1.0 plot_xy_scaling*0.5 1.0]);
    if plot_global,
        axis([lat_min lat_max -D_max -D_min]);
        set(gca,'XLabel',text('String','Latitude','FontSize',15),'XTick',[-90 -60 -30 0 30 60 90]);
        set(gca,'YLabel',text('String','Depth (km)','FontSize',15),'YTick',[-D_max:1000:-D_min],'YTickLabel',{'5';'4';'3';'2';'1';'0'});
    else
        axis([plot_lat_min plot_lat_max -plot_D_max -plot_D_min]);
        set(gca,'XLabel',text('String','Latitude','FontSize',15),'XTick',[plot_lat_min plot_lat_max]);
        set(gca,'YLabel',text('String','Depth (km)','FontSize',15),'YTick',[-plot_D_max -plot_D_min],'YTickLabel',{num2str(-plot_D_max);num2str(-plot_D_min)});
    end
    set(gca,'TickDir','out');
    if ~isempty(plot_title)
        title(plot_title,'FontSize',18);
    else
        if ~isempty(maskid)
            title(['Data ID: ',strrep(dataid_1,'_','-'),' / mask = ', strrep(maskid,'_','-')],'FontSize',12);
        else
            title(['Data ID: ',strrep(dataid_1,'_','-'),' / i = ', num2str(iplot)],'FontSize',12);
        end
    end
    % assign dummy iplot value
    if (iplot == 0), iplot = 1; end
    % draw filled rectangles
    for j = jmax:-1:1,
        for k = 1:kmax,
            if topo(j,iplot) > ym(k,j)
                h = patch([lats(k,j) lats(k,j) latn(k,j) latn(k,j)],[layb(k,j) layt(k,j) layt(k,j) layb(k,j)],color_g);
                set(h,'EdgeColor',color_g);
            else
                if (isnan(zm(k,j)))
                    if (data_only == 'n'),
                        h = patch([lats(k,j) lats(k,j) latn(k,j) latn(k,j)],[layb(k,j) layt(k,j) layt(k,j) layb(k,j)],[1 1 1]);
                        set(h,'EdgeColor',[1 1 1]);
                    end
                else
                    if isempty(contour_file),
                        col = 1 + round(0.5+con_n*(zm(k,j)-con_min)/(con_max-con_min));
                        if col < 1, col = 1; end
                        if col > con_n+2, col = con_n+2; end
                    else
                        col = 1;
                        for c=1:con_n+1,
                            if (zm(k,j) > contour_data(c)), col = c+1; end
                        end
                    end
                    if (data_only == 'n'),
                        h = patch([lats(k,j) lats(k,j) latn(k,j) latn(k,j)],[layb(k,j) layt(k,j) layt(k,j) layb(k,j)],cmap(col,:));
                        set(h,'EdgeColor',cmap(col,:));
                    end
                end
            end
        end
    end
    %
    % *** PLOT CONTINENTAL OUTLINE ************************************** %
    %
    for k = 1:kmax,
        for j = 1:jmax-1,
            if topo(j,iplot) > ym(k,j)
                if topo(j+1,iplot) <= ym(k,j+1)
                    h = plot([latn(k,j) latn(k,j)],[layb(k,j) layt(k,j)],'k-');
                    set(h,'LineWidth',1.0);
                end
            end
        end
        for j = 2:jmax,
            if topo(j,iplot) > ym(k,j)
                if topo(j-1,iplot) <= ym(k,j-1)
                    h = plot([lats(k,j) lats(k,j)],[layb(k,j) layt(k,j)],'k-');
                    set(h,'LineWidth',1.0);
                end
            end
        end
    end
    for j = 1:jmax,
        for k = 2:kmax,
            if topo(j,iplot) < ym(k,j)
                if topo(j,iplot) > ym(k-1,j)
                    h = plot([lats(k,j) latn(k,j)],[layb(k,j) layb(k,j)],'k-');
                    set(h,'LineWidth',1.0);
                end
            end
        end
    end
    %
    % *** OVERLAY CONTOURS ********************************************** %
    %
    if (contour_plot == 'y') && (data_only == 'n') && (isempty(plot_opsi))
        if ((con_min) < 0.0 && (con_max > 0.0) && (contour_dashneg == 'y'))
            v = [0.0:(con_max-con_min)/(con_n/contour_mod):con_max];
            [C,h] = contour(xm,ym,zm,v,'k-');
            set(h,'LineWidth',0.25);
            v = [0.0:(con_max-con_min)/(con_n/contour_mod_label):con_max];
            [C,h] = contour(xm,ym,zm,v,'k-');
            set(h,'LineWidth',1.0);
            if ((contour_label == 'y') && (data_log10 ~= 'y'))
                clabel(C,h);
            end
            v = [con_min:(con_max-con_min)/(con_n/contour_mod):0.0];
            [C,h] = contour(xm,ym,zm,v,'k--');
            set(h,'LineWidth',0.25);
            v = [con_min:(con_max-con_min)/(con_n/contour_mod_label):0.0];
            [C,h] = contour(xm,ym,zm,v,'k--');
            set(h,'LineWidth',1.0);
            if ((contour_label == 'y') && (data_log10 ~= 'y'))
                clabel(C,h);
            end
        else
            v = [con_min:(con_max-con_min)/(con_n/contour_mod):con_max];
            [C,h] = contour(xm,ym,zm,v,'k-');
            set(h,'LineWidth',0.25);
            v = [con_min:(con_max-con_min)/(con_n/contour_mod_label):con_max];
            [C,h] = contour(xm,ym,zm,v,'k-');
            set(h,'LineWidth',1.0);
            if ((contour_label == 'y') && (data_log10 ~= 'y'))
                clabel(C,h);
            end
        end
        if (contour_hlt == 'y'),
            v = [-1.0E19+contour_hltval:1.0E19:1.0E19+contour_hltval];
            [C,h] = contour(xm,ym,zm,v,'w');
            set(h,'LineWidth',2.5);
        end
    end
    %
    % *** OVERLAY CONTOURS -- OVERTURNING STREAMFUNCTION (IF SELECTED) ** %
    %
    if ~isempty(plot_opsi)
        if (data_only == 'y')
            con_min = plot_opsi_min;
            con_max = plot_opsi_max;
            con_n = (plot_opsi_max - plot_opsi_min)/plot_opsi_dminor;
            % re-define colormap
            cmap = make_cmap5(colorbar_name,con_n);
            if (colorbar_inv == 'y'), cmap = flipdim(cmap,1); end,
            colormap(cmap);
            %
            caxis([con_min-(con_max-con_min)/con_n con_max]);
            v = [plot_opsi_min:plot_opsi_dminor:plot_opsi_max];
            [C,h] = contourf(xm,opsiym,opsizm,v);
            set(h,'LineWidth',0.01);
        end
        v = [0.0:plot_opsi_dminor:plot_opsi_max];
        [C,h] = contour(xm,opsiym,opsizm,v,'k-');
        set(h,'LineWidth',0.25);
        v = [plot_opsi_min:plot_opsi_dminor:0.0];
        [C,h] = contour(xm,opsiym,opsizm,v,'k:');
        set(h,'LineWidth',0.25);
        v = [0.0:plot_opsi_dmajor:plot_opsi_max];
        [C,h] = contour(xm,opsiym,opsizm,v,'k');
        set(h,'LineWidth',1.0);
        if contour_label == 'y'
            clabel(C,h);
        end
        v = [plot_opsi_min:plot_opsi_dmajor:0.0];
        [C,h] = contour(xm,opsiym,opsizm,v,'k:');
        set(h,'LineWidth',1.0);
        if contour_label == 'y'
            clabel(C,h);
        end
    end
    %
    % *** OVERLAY DATA ************************************************** %
    %
    if ~isempty(overlaydataid)
        % set uniform marker shape and color
        if (data_shapecol == 'n'),
            for n = 1:nmax,
                overlaydata_shape(n) = 'o';
                overlaydata_ecol(n) = data_sitecolor;
                overlaydata_fcol(n) = data_sitecolor;
            end
        end
        % plot overlay data
        if (data_siteonly == 'n'),
            scatter(overlaydata(:,2),overlaydata(:,3),4,overlaydata(:,4)/data_scale,overlaydata_shape(n),'Filled','LineWidth',data_sitelineth,'Sizedata',data_size,'MarkerEdgeColor',overlaydata_ecol(n));
        else
            if (overlaydata_fcol(n) == '-'),
                scatter(overlaydata(:,2),overlaydata(:,3),4,overlaydata_shape(n),'LineWidth',data_sitelineth,'Sizedata',data_size,'MarkerEdgeColor',overlaydata_ecol(n));
            else
                scatter(overlaydata(:,2),overlaydata(:,3),4,overlaydata_shape(n),'LineWidth',data_sitelineth,'Sizedata',data_size,'MarkerEdgeColor',overlaydata_ecol(n),'MarkerFaceColor',overlaydata_fcol(n));
            end
        end
    end
    %
    % *** PLOT BORDER *************************************************** %
    %
    h = plot([lat_min lat_max],[-D_max -D_max],'k-');
    set(h,'LineWidth',1.0);
    h = plot([lat_min lat_max],[-D_min -D_min],'k-');
    set(h,'LineWidth',1.0);
    h = plot([lat_min lat_min],[-D_max -D_min],'k-');
    set(h,'LineWidth',1.0);
    h = plot([lat_max lat_max],[-D_max -D_min],'k-');
    set(h,'LineWidth',1.0);
    %
    hold off;
    %
    % *** CREATE COLOR BAR ********************************************** %
    %
    if (~((data_only == 'y') && (data_siteonly == 'y')))
        %
        set(gcf,'CurrentAxes',fh(3));
        hold on;
        %
        set(gca,'XTick',[],'YTick',[]);
        axis([0 1 0 con_n+2]);
        % draw and label color bar rectangles
        % draw and label start triangle
        c = 1;
        h = fill([0.1 0.2 0.3],[c c-1.0 c],cmap(c,:));
        if isempty(contour_file),
            str = [num2str(con_min + (c-1)*(con_max-con_min)/con_n)];
        else
            str = num2str(contour_data(c));
        end
        textsize = 2+round(80/con_n);
        if textsize > 10, textsize = 10; end
        text(0.40,c,str,'FontName','Arial','FontSize',textsize);
        set(h,'LineWidth',0.5);
        set(h,'EdgeColor','k');
        % draw and label bars
        for c = 2:con_n+1,
            h = fill([0.1 0.1 0.3 0.3],[c-1.0 c c c-1.0],cmap(c,:));
            if isempty(contour_file),
                str = [num2str(con_min + (c-1)*(con_max-con_min)/con_n)];
            else
                str = num2str(contour_data(c));
            end
            textsize = 2+round(80/con_n);
            if textsize > 10, textsize = 10; end
            text(0.40,c,str,'FontName','Arial','FontSize',textsize);
            set(h,'LineWidth',0.5);
            set(h,'EdgeColor','k');
        end
        % draw end triangle
        c = con_n+2;
        h = fill([0.1 0.2 0.3],[c-1.0 c c-1.0],cmap(c,:));
        set(h,'LineWidth',0.5);
        set(h,'EdgeColor','k');
        %
        hold off;
        %
    end
    %
    % *** PRINT PLOT **************************************************** %
    %
    set(gcf,'CurrentAxes',fh(1));
    if (plot_format_old == 'y')
        print('-dpsc2', [filename '.' str_date '.ps']);
    else
        switch plot_format
            case 'png'
                export_fig([filename '.' str_date '.png'], '-png', '-r150', '-nocrop');
            case 'pngT'
                export_fig([filename '.' str_date '.png'], '-png', '-r150', '-nocrop', '-transparent');
            case 'jpg'
                export_fig([filename '.' str_date '.jpg'], '-jpg', '-r150', '-nocrop');
            otherwise
                export_fig([filename '.' str_date '.eps'], '-eps', '-nocrop');
        end
    end
    %
    % ******************************************************************* %
    %
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** SECONDARY FIGURES ************************************************* %
% *********************************************************************** %
%
if (plot_secondary == 'y'),
    %
    % *** SET PLOT SCALE ************************************************ %
    %
    % set minimum contour value
    if exist('con_min','var') == 0
        con_min = min(min(zz));
    end
    % set maximum contour value
    if exist('con_max','var') == 0
        con_max = max(max(zz));
    end
    % ensure min and max are not identical ...
    if con_min == con_max
        if con_max == 0.0
            con_max = 1.0;
        else
            con_min = (1.00/1.01)*con_min;
            con_max = (1.01)*con_max;
        end
    end
    % if min > max, then reverse min and max
    if con_min > con_max
        con_min_TEMP = con_min;
        con_max_TEMP = con_max;
        con_min = con_max_TEMP;
        con_max = con_min_TEMP;
    end
    %
    % *** PLOT FIGURE (profile) ***************************************** %
    %
    if (data_only == 'n'),
        %
        figure
        plot(zl(:),-grid_zt(:));
        hold on;
        scatter(zl(:),-grid_zt(:),25,'r');
        axis([con_min con_max -grid_zt_edges(1) -grid_zt_edges(kmax+1)]);
        xlabel(strrep(dataid_1,'_','-'));
        ylabel('Elevation (m)');
        if ~isempty(plot_title)
            title(plot_title,'FontSize',18);
        else
            if ~isempty(maskid)
                title(['Data ID: ',strrep(dataid_1,'_','-'),' / i = ', strrep(maskid,'_','-')],'FontSize',12);
            else
                title(['Data ID: ',strrep(dataid_1,'_','-'),' / i = ', num2str(iplot)],'FontSize',12);
            end
        end
        if (plot_format_old == 'y')
            print('-dpsc2', [filename '.PROFILE.' str_date '.ps']);
        else
            switch plot_format
                case 'png'
                    export_fig([filename '.PROFILE.' str_date '.png'], '-png', '-r150', '-nocrop');
                case 'pngT'
                    export_fig([filename '.PROFILE.' str_date '.png'], '-png', '-r150', '-nocrop', '-transparent');
                case 'jpg'
                    export_fig([filename '.PROFILE.' str_date '.jpg'], '-jpg', '-r150', '-nocrop');
                otherwise
                    export_fig([filename '.PROFILE.' str_date '.eps'], '-eps', '-nocrop');
            end
        end
        %
    end
    %
    % *** SAVE DATA (profile) ******************************************* %
    %
    if (data_only == 'n'), fprint_1D2_d([flipud(grid_zt(:)) flipud(zl(:))],[filename '.PROFILE.', str_date, '.res']); end
    %
    % *** PLOT FIGURE (surface zonal mean) ****************************** %
    %
    if (data_only == 'n'),
        %
        figure
        plot(grid_lat,zz(kmax,:));
        hold on;
        scatter(grid_lat,zz(kmax,:),25,'r');
        axis([-90.0 90.0 con_min con_max ]);
        xlabel('Latitude');
        ylabel(strrep(dataid_1,'_','-'));
        if (plot_format_old == 'y')
            print('-dpsc2', [filename '.ZONAL.' str_date '.ps']);
        else
            switch plot_format
                case 'png'
                    export_fig([filename '.ZONAL.' str_date '.png'], '-png', '-r150', '-nocrop');
                case 'pngT'
                    export_fig([filename '.ZONAL.' str_date '.png'], '-png', '-r150', '-nocrop', '-transparent');
                case 'jpg'
                    export_fig([filename '.ZONAL.' str_date '.jpg'], '-jpg', '-r150', '-nocrop');
                otherwise
                    export_fig([filename '.ZONAL.' str_date '.eps'], '-eps', '-nocrop');
            end
        end
        %
    end
    %
    % *** SAVE DATA (surface zonal mean) ******************************** %
    %
    if (data_only == 'n'), fprint_1Dn_d([flipud(grid_lat) rot90(zz(kmax,:),1)],[filename '.ZONAL.', str_date, '.res']); end
    %
    % *** PLOT FIGURE (cross-plot) ************************************** %
    %
    if ( ~isempty(dataid_2) || (~isempty(overlaydataid) && (data_only == 'n')) ),
        %
        if ~isempty(dataid_2),
            loc_x_data = reshape(data_1(loc_kmin:loc_kmax,:,:),1,[]);
            loc_y_data = reshape(data_2(loc_kmin:loc_kmax,:,:),1,[]);
            loc_D_data = reshape(data_D(loc_kmin:loc_kmax,:,:),1,[]);
            loc_x_label = [strrep(dataid_1,'_','-')];
            loc_y_label = [strrep(dataid_2,'_','-')];
            loc_D_label = ['Depth (m)'];
        elseif ~isempty(overlaydataid)
            loc_x_data = data_vector_1;
            loc_y_data = data_vector_2;
            loc_D_data = -overlaydata(:,3);
            loc_x_label = [strrep(overlaydataid,'_','-')];
            loc_y_label = [strrep(dataid_1,'_','-')];
            loc_D_label = ['Depth (m)'];
        end
        %
        plot_crossplotc(loc_x_data,loc_y_data,loc_D_data,loc_x_label,loc_y_label,loc_D_label,POPT,[filename '.CROSSPLOT']);
        %
    end
    %
    % *** SAVE DATA (cross-plot relationships) ************************** %
    %
    if ( ~isempty(dataid_2) || (~isempty(overlaydataid) && (data_only == 'n')) ), fprint_1Dn_d([loc_x_data' loc_y_data' loc_D_data'],[filename '.CROSSPLOT.', str_date, '.res']); end
    %
    % *** PLOT FIGURE (histogram) *************************************** %
    %
    % figure;
    % bar_bins = [con_min:(con_max-con_min)/(con_n/contour_mod):con_max];
    % %%%bar_data = histwcv_bin(data_vector(:),data_vector_V(:),bar_bins);
    % [histw, vinterval] = histwc(data_vector(:),data_vector_V(:),20);
    % bar(histw, vinterval);
    % %%%bar_data = hist(data_vector(:),bar_bins);
    % %%%bar(bar_bins,bar_data);
    %
    % ******************************************************************* %
    %
    % ******************************************************************* %
    %
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** END *************************************************************** %
% *********************************************************************** %
%
% close netCDF files
netcdf.close(ncid_1);
if ~isempty(exp_2)
    netcdf.close(ncid_2);
end
if ~isempty(plot_opsi)
    netcdf.close(ncid_0);
end
%
% *********************************************************************** %
