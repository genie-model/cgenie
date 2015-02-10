function [] = plot_fields_biogem_3d_k(PEXP1,PEXP2,PVAR1,PVAR2,PT1,PT2,PIK,PMASK,PCSCALE,PCMIN,PCMAX,PCN,PDATA,POPT,PNAME)
% plot_fields_biogem_3d_k
%
%   *******************************************************************   %
%   *** biogem k-SECTION (LON-LAT) DIFFERENCE PLOTTING ****************   %
%   *******************************************************************   %
%
%   plot_fields_biogem_3d_k(PEXP1,PEXP2,PVAR1,PVAR2,PT1,PT2,PIK,PMASK,PCSCALE,PCMIN,PCMAX,PCN,PDATA,POPT,PNAME)
%   plots a k-section through the BIOGEM 3-D netCDF data file (with
%   differencing/anomoly and water column integral options)
%   'fields_biogem_3d.nc' and takes 15 arguments:
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
%   PIK [INTEGER] (e.g. 16)
%   --> the level in the ocean model to be plotted (the 'k' slice)
%   --> a zero will result in a water column integral being plotted
%   --> a -1 will result in the benthic surface being plotted
%   PMASK [STRING] (e.g. 'mask_worjh2_Indian.dat')
%   --> the filename containing the mask (in the working directory)
%   PCSCALE [REAL] (e.g. 1.0)
%   --> the scale factor for the plot
%       e.g., to plot in units of micro molar (umol kg-1), enter: 1e-6
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
%       lon, lat, depth, value, label
%       or, of the option (below) data_ij = 'y', then as:
%       i, j, k, value, label
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
%           plot_fields_biogem_3d_k('experiment_1','','ocn_PO4','',1994.5,-1,16,'',1e-6,0,2,20,'','','')
%           will plot the time-slice cenetered on a time of 1994.5,
%           of PO4 concentrations at the ocean surface (k = 16),
%           between 0 and 2 umol kg-1 in 20 contour intervals
%           of 'experiment_1'
%
%   *******************************************************************   %

% *********************************************************************** %
% ***** HISTORY ********************************************************* %
% *********************************************************************** %
%
%   10/06/19: CREATED ...
%   10/07/01: Passed reference data stats to plot_taylordiag
%   10/07/05: cosmetic changes ...
%   10/07/05: name changed: allstats -> calc_allstats
%   10/07/06: adjustments to use calc_find_ij_v100706 (in place of find_ij)
%   10/07/06: sorted out confusion between (lon,lat) of the data and (j,i) of the model grid ...
%   10/07/06: added stats save
%   10/07/08: filtered overlay data to avoid plotting on land!
%   10/07/16: added option for inputting (i,j) data
%   11/01/30: re-formatting
%             addition of 3D data Taylor Diagram analysis
%   11/01/31: 3D data Taylor Diagram analysis debugging ...
%   11/05/31: Added time-stamping to Taylor plots
%   11/05/31: cosmetic changes
%   12/01/21: changed subroutine name: calc_find_ij_v100706 -> calc_find_ij
%   12/01/23: fixed bug in plot save string
%             added quiver plot option (for velocity field)
%             added cell averaging + depth layer filtering of overlay data
%   12/01/24: reorganized sequence of lon axis vs overlay data processing
%             added parameter to control vector length
%             rationalized 'user settings'
%   12/02/09: added in options for: anomoly plotting; data-only
%   12/05/23: changed order of streamfunction netCDF loading and grid setup
%             to prevent parameter value conflicts (with data netCDF
%             parameters)
%   12/10/09: added option for plotting data sites (no values)
%   12/10/16: updated HELP text
%   12/10/18: reordered parameter list
%   12/11/13: removed scale bar when data Site labelling selected
%   12/12/10: added highlight contour option
%   12/12/10: removed stats calc when data is plotted as 'site'
%             fully removed land data sites (rather than set as NaN value)
%             updated color bar drawing
%             added additional option for labelling sites
%             revision of data loading and processoing to avoid impicit
%               integer truncation, requiring DOUBLE<->INT32 conversions
%   12/12/14: adjusted plotting of single contour overlay
%             revised filename string
%   12/12/27: bug-fix in non re-gridded obs data (wrong levtor length)
%   13/01/02: fixed small buglette in gridded data plotting
%   13/01/14: changed a default [data anomoly]
%             fixed bug in alt date (time-slice) read-in
%   13/05/24: enabled mask-out regions ot be plotted in white
%   13/08/12: small tweak to site plotting (as per SEDGEM plotting)
%             changed data format
%             updated stats calculation and output
%   13/09/19: adjusted filename
%   13/10/06: created alt anomoly colormap and adjusted anomoly plotting
%             added invert color bar option
%             tidy-up of old code
%   13/11/08: added option for omitting stats saving
%             filled in data only points white (rather than empty)
%   13/11/10: added alternative figure format saving
%   13/11/11: [minor]
%   13/11/12: REORGANIZED USER SETTINGS [AND PASSED PARAMETER LIST]
%   13/11/15: minor
%   13/11/18: MUTLAB bug-fix(?) for nmax<10 not displaying scatter colors
%   13/11/22: bug-fix of data scatter plotting (now: value, not depth!!!)
%             minor alterations to facilitate data export
%   13/12/04: added data-only saving
%   13/12/19: disabled target diagram for now to simplify things
%   13/12/23: added file format selection for 'new' plotting
%   14/04/15: added alt filename
%   14/04/17: altered (fixed?) criteria for not ploting the color scale
%   14/04/19: removed old colorbar option
%   14/08/11: added addiitonal options for fine-tuning site markers
%   14/08/20: added '%'s to ASCII data output headers
%   14/09/11: added options for:
%             setting the min/max k-values for water column averages
%             whether to plot as averages or column inventory
%             whether to plot the min or max value anywhere in the column
%             (for the latter -- k value is identified but not used yet)
%   14/09/11: added option for uniform lat grid
%   14/09/12: completed uniform grid option
%             added option for setting shape and color of data points
%   14/09/15: fixed up setting shape and color of data points code
%             also: added the capabilty of finding min of max values across
%                   two different fields (rather than calc a difference)
%   14/09/17: renamed plot_lon_min -> plot_lon_origin
%             added options for plotting sub-regions
%   14/09/29: auto plot format
%   14/09/30: minor bug-fix of data location reported lat values
%   14/10/28: added auto identification of kmax (for input kplot > kmax)
%   14/12/01: incorporated cbrewer.m colormap schemes (& removed anom map)
%   14/12/03: removed cbrewer.m ... :o)
%   14/12/03: incorporated new make_cmap5 function
%   14/12/07: fixed dim error in flipping color scale
%   14/12/13: fixed highlight contour; adjusted line-widths (and color)
%   14/12/30: added cross-plotting
%             fixed missing nmax value for dual 3D data input
%   15/01/06: added some parameter (k-value) warnings
%             added benthic plotting and data extraction
%   15/01/07: bug-fixed grid shifting of depth arrays
%             adjusted kmin/kmax filtering, added it to benthic analysis
%             adjusted parameter names for restricting k space data 
%             (now: data_kmin, data_kmax)
%   15/01/09: revised to use new make_cmap5.m (adjusted number of colors) 
%   15/01/11: various bug-fixes on k space parameter changes
%   15/01/11: replaced make_cmap5 -> make_cmap
%   15/01/12: adjusted orientation of data_vector_2
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
lon_min = plot_lon_origin;
lon_max = lon_min+360;
lon_offset = 0;
% set passed parameters
exp_1 = PEXP1;
exp_2 = PEXP2;
timesliceid_1 = PT1;
timesliceid_2 = PT2;
dataid_1 = PVAR1;
dataid_2 = PVAR2;
kplot = PIK;
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
if ((plot_lat_min == plot_lat_max) && (plot_lon_min == plot_lon_max)),
    plot_global = true;
    plot_xy_scaling = 1.0;
else
    plot_global = false;
    if (plot_lat_min == plot_lat_max),
        plot_lat_min = lat_min;
        plot_lat_max = lat_max;
    end
    if (plot_lon_min == plot_lon_max),
        plot_lon_min = lon_min;
        plot_lon_max = lon_max;
    end
    plot_xy_scaling = ((plot_lat_max - plot_lat_min)/(lat_max - lat_min)) / ((plot_lon_max - plot_lon_min)/(lon_max - lon_min));
end
% set date
str_date = [datestr(date,11), datestr(date,5), datestr(date,7)];
% set function name
str_function = 'plot-fields-biogem-3d(k)';
% plot format
if ~isempty(plot_format), plot_format_old='n'; end
% plotting paths
addpath(library_path1);
if (plot_format_old == 'n'),
    addpath(library_path2);
    addpath(library_path3);
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
data_1=[];
data_2=[];
% additional
rawdata=[];
zm_count = [];
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
% identify kmax
if (kplot > kmax),
    kplot = kmax;
    disp(['WARNING: selected k value ', kplot, ' exceeds surface level (#',kmax,'). Setting to : ', kmax]);
elseif (kplot < -1),
    disp(['ERROR: a kplot value of ', kplot, ' is not value. [VALID OPTIONS:, between 1 and kmax, 0 (water column integral), -1 (benthic)]']);
    return;
end
% check data_k* settings
if (data_kmin > data_kmax), data_kmin = data_kmax; end
if (data_kmin == data_kmax), 
    data_kmin = 1;
    data_kmax = kmax;
end
% calculate topography
for i = 1:imax,
    for j = 1:jmax,
        if grid_k1(j,i) <= kmax
            topo(j,i) = -grid_zt_edges(grid_k1(j,i));
        else
            topo(j,i) = 0.0;
        end
        if (kplot > 0)
            laym(j,i) = -grid_zt(kplot);
            layb(j,i) = -grid_zt_edges(kplot);
            layt(j,i) = -grid_zt_edges(kplot+1);
        elseif (kplot == 0)
            laym(j,i) = 0.0;
            layb(j,i) = -grid_zt_edges(data_kmax);
            layt(j,i) = -grid_zt_edges(data_kmax+1);
        elseif (kplot == -1)
        if grid_k1(j,i) <= kmax
            laym(j,i) = 0.0;
            layb(j,i) = -grid_zt_edges(grid_k1(j,i));
            layt(j,i) = -grid_zt_edges(grid_k1(j,i)+1);    
        else
            laym(j,i) = 0.0;
            layb(j,i) = 0.0;
            layt(j,i) = 0.0; 
            topo(j,i) = 9.9E19;
        end        
        end
    end
end
% load and calculate remaining grid information
varid  = netcdf.inqVarID(ncid_1,'lat');
grid_lat = netcdf.getVar(ncid_1,varid);
varid  = netcdf.inqVarID(ncid_1,'lon');
grid_lon = netcdf.getVar(ncid_1,varid) + lon_offset;
[lonm latm] = meshgrid(grid_lon,grid_lat);
varid  = netcdf.inqVarID(ncid_1,'lat_edges');
grid_lat_edges = netcdf.getVar(ncid_1,varid);
varid  = netcdf.inqVarID(ncid_1,'lon_edges');
grid_lon_edges = netcdf.getVar(ncid_1,varid) + lon_offset;
[lonw lats] = meshgrid(grid_lon_edges(1:imax),grid_lat_edges(1:jmax));
[lone latn] = meshgrid(grid_lon_edges(2:imax+1),grid_lat_edges(2:jmax+1));
% calculate cell masses
% NOTE: assume equal area grid, normaalized area
data_M = zeros(kmax,jmax,imax);
for k = 1:kmax,
    data_M(k,:,:) = 1027.649*1.0*(grid_zt_edges(k) - grid_zt_edges(k+1));
end
% Non-uniform lat grid
if (plot_equallat == 'n'),
    lat_max = sin(pi*lat_max/180.0);
    lat_min = sin(pi*lat_min/180.0);
    latn = sin(pi*latn/180.0);
    lats = sin(pi*lats/180.0);
    plot_lat_max = sin(pi*plot_lat_max/180.0);
    plot_lat_min = sin(pi*plot_lat_min/180.0);
end
%i=1 longitude grid origin
grid_lon_origin = grid_lon_edges(1);
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
            tid = n;
        end
    end
    if exist('time','var') == 0
        disp('   > WARNING: Year must be one of the following;');
        format long g;
        double(int32(100*timeslices(:)))/100
        format;
        timesliceid_1 = input('   > Time-slice year: ');
    end
end
% check that the variable name exists
varid = [];
while isempty(varid)
    for n = 0:nvars-1,
        [varname,xtype,dimids,natts] = netcdf.inqVar(ncid_1,n);
        if strcmp(varname,dataid_1)
            varid = n;
        end
    end
    if isempty(varid)
        disp('   > WARNING: Variable name must be one of the following;');
        for n = 0:nvars-1,
            [varname,xtype,dimids,natts] = netcdf.inqVar(ncid_1,n);
            varname
        end
        dataid = input('   > Variable name: ','s');
    end
end
% load data
% flip array around diagonal to give (j,i) array orientation
[varname,xtype,dimids,natts] = netcdf.inqVar(ncid_1,varid);
rawdata = netcdf.getVar(ncid_1,varid);
if length(dimids) == 4
    rawdata(1:imax,1:jmax,1:kmax) = rawdata(1:imax,1:jmax,1:kmax,tid);
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
else
    ncid_2 = ncid_1;
end
%
% *** ALT TIME-SLICE **************************************************** %
%
if timesliceid_2 > 0.0
    % check that the year exists
    varid  = netcdf.inqVarID(ncid_2,'time');
    timeslices = netcdf.getVar(ncid_2,varid);
    [dimname, dimlen] = netcdf.inqDim(ncid_2,varid);
    clear time;
    while exist('time','var') == 0
        for n = 1:dimlen,
            if double(int32(100*timeslices(n)))/100 == timesliceid_2
                time = timesliceid_2;
                tid = n;
            end
        end
        if exist('time','var') == 0
            disp('   > WARNING: Year must be one of the following;');
            format long g;
            double(int32(100*timeslices(:)))/100
            format;
            timesliceid_2 = input('   > Time-slice year: ');
        end
    end
end
%
% *** ALT DATA FIELD **************************************************** %
%
if ~isempty(dataid_2)
    % check that the variable name exists
    varid = [];
    while isempty(varid)
        for n = 0:nvars-1,
            [varname,xtype,dimids,natts] = netcdf.inqVar(ncid_2,n);
            if strcmp(varname,dataid_2)
                varid = n;
            end
        end
        if isempty(varid)
            disp('   > WARNING: Variable name must be one of the following;');
            for n = 0:nvars-1,
                [varname,xtype,dimids,natts] = netcdf.inqVar(ncid_2,n);
                varname
            end
            dataid = input('   > Variable name: ','s');
        end
    end
else
    for n = 0:nvars-1,
        [varname,xtype,dimids,natts] = netcdf.inqVar(ncid_1,n);
        if strcmp(varname,dataid_1)
            varid = n;
        end
    end
end
%
% *** SET DATA ********************************************************** %
%
% NOTE: set data anomoly flag here
%
if (~isempty(exp_2)) || (timesliceid_2 > 0.0) || (~isempty(dataid_2))
    % load data
    [varname,xtype,dimids,natts] = netcdf.inqVar(ncid_2,varid);
    rawdata = netcdf.getVar(ncid_2,varid);
    if length(dimids) == 4
        rawdata(1:imax,1:jmax,1:kmax) = rawdata(1:imax,1:jmax,1:kmax,tid);
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
% *** OPTIONAL (u,v) VELOCITY DATA OPERLAY ****************************** %
% *********************************************************************** %
%
if (data_uv == 'y'),
    varid  = netcdf.inqVarID(ncid_1,'phys_u');
    [varname,xtype,dimids,natts] = netcdf.inqVar(ncid_1,varid);
    rawdata = netcdf.getVar(ncid_1,varid);
    if length(dimids) == 4
        rawdata(1:imax,1:jmax,1:kmax) = rawdata(1:imax,1:jmax,1:kmax,tid);
        for n = 1:kmax,
            data_u(kmax - n + 1,1:jmax,1:imax) = rawdata(1:imax,1:jmax,n)';
        end
    elseif length(dimids) == 3
        rawdata(1:imax,1:jmax,1:kmax) = rawdata(1:imax,1:jmax,1:kmax);
        for n = 1:kmax,
            data_u(kmax - n + 1,1:jmax,1:imax) = rawdata(1:imax,1:jmax,n)';
        end
    elseif length(dimids) == 2
        rawdata(1:imax,1:jmax) = rawdata(1:imax,1:jmax);
        data_u(1:jmax,1:imax) = rawdata(1:imax,1:jmax)';
    else
        data_u(:,:,:) = NaN;
    end
    varid  = netcdf.inqVarID(ncid_1,'phys_v');
    [varname,xtype,dimids,natts] = netcdf.inqVar(ncid_1,varid);
    rawdata = netcdf.getVar(ncid_1,varid);
    if length(dimids) == 4
        rawdata(1:imax,1:jmax,1:kmax) = rawdata(1:imax,1:jmax,1:kmax,tid);
        for n = 1:kmax,
            data_v(kmax - n + 1,1:jmax,1:imax) = rawdata(1:imax,1:jmax,n)';
        end
    elseif length(dimids) == 3
        rawdata(1:imax,1:jmax,1:kmax) = rawdata(1:imax,1:jmax,1:kmax);
        for n = 1:kmax,
            data_v(kmax - n + 1,1:jmax,1:imax) = rawdata(1:imax,1:jmax,n)';
        end
    elseif length(dimids) == 2
        rawdata(1:imax,1:jmax) = rawdata(1:imax,1:jmax);
        data_v(1:jmax,1:imax) = rawdata(1:imax,1:jmax)';
    else
        data_v(:,:,:) = NaN;
    end
else
    data_u(:,:,:) = zeros(size(data_1));
    data_v(:,:,:) = zeros(size(data_1));
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** SET OUTPUT FILESTRING ********************************************* %
% *********************************************************************** %
%
% create an output filestring for data and plot saving
%
if ~isempty(maskid)
    if (~isempty(exp_2)) || (timesliceid_2 >= 0.0) || (~isempty(dataid_2))
        filename = [exp_1, '.', 'y', num2str(timesliceid_1), '.', dataid_1, '_MINUS_', exp_2, '.', 'y', num2str(timesliceid_2), '.', dataid_2, '.', maskid];
    else
        filename = [exp_1, '.', 'y', num2str(timesliceid_1), '.', dataid_1, '.', maskid];
    end
else
    if (~isempty(exp_2)) || (timesliceid_2 >= 0.0) || (~isempty(dataid_2))
        filename = [exp_1, '.', 'y', num2str(timesliceid_1), '.', dataid_1, '_MINUS_', exp_2, '.', 'y', num2str(timesliceid_2), '.', dataid_2, '.', 'k', num2str(kplot)];
    else
        filename = [exp_1, '.', 'y', num2str(timesliceid_1), '.', dataid_1, '.', 'k', num2str(kplot)];
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
% set ocean grid value to give white when plotted
if strcmp(dataid_1,'grid_mask')
    data_1 = NaN;
end
if strcmp(dataid_2,'grid_mask')
    data_2 = NaN;
end
%
xm = lonm;
ym = latm;
% set data value:
% (1) if no anomoly, use only array #1
% (2) if anomoly, by default difference #1 and #2 unless ...
%     a minimum or maximum is requested: find min or max of the two arrays
%     and then reset the anomoly flag
% (3) added in any data offset to the values set for the data array
if (data_anomoly == 'y')
    if (plot_maxval == 'y'),
        data = max(data_1,data_2);
        data_anomoly = 'n';
    elseif (plot_minval == 'y')
        data = min(data_1,data_2);
        data_anomoly = 'n';
    else
        data = data_1 - data_2;
    end
else
    data = data_1;
end
data = data - data_offset;
% filter gridded data
n = 0;
if (kplot > 0),
    % process single depth layer
    zm(:,:) = data(kplot,:,:);
    z_u(:,:) = data_u(kplot,:,:);
    z_v(:,:) = data_v(kplot,:,:);
    speed = NaN(size(zm));
    for i = 1:imax,
        for j = 1:jmax,
            if topo(j,i) > layb(j,i)
                zm(j,i) = NaN;
                xm(j,i) = NaN;
                ym(j,i) = NaN;
                z_u(j,i) = NaN;
                z_v(j,i) = NaN;
            elseif (zm(j,i) < -1.0E6) || (zm(j,i) > 1.0E30)
                zm(j,i) = NaN;
                xm(j,i) = NaN;
                ym(j,i) = NaN;
                z_u(j,i) = NaN;
                z_v(j,i) = NaN;
            else
                if ~isempty(maskid)
                    if mask(j,i) == 0
                        zm(j,i) = NaN;
                    end
                end
                if data_log10 == 'y'
                    if (zm(j,i) > 0.0)
                        zm(j,i) = log10(zm(j,i)/data_scale);
                    else
                        if contour_noneg == 'y'
                            zm(j,i) = 0.0;
                        else
                            zm(j,i) = NaN;
                        end
                    end
                else
                    zm(j,i) = zm(j,i)/data_scale;
                end
                if (data_uv == 'y'), speed(j,i) = data_scale*(z_u(j,i)^2.0 + z_v(j,i)^2.0)^0.5; end
                if ~isnan(zm(j,i)), n = n + 1; end
            end
        end
    end
elseif (kplot == 0),
    % create water column integral
    zm = zeros(jmax,imax);
    zm_M = zeros(jmax,imax);
    zm_count = zeros(jmax,imax);
    zm_minval = zeros(jmax,imax) + 0.9E36;
    zm_maxval = zeros(jmax,imax) - 1.0E6;
    zm_mink = zeros(jmax,imax);
    zm_maxk = zeros(jmax,imax);
    % assume surface velocities
    if (data_uv == 'y')
        z_u(:,:) = data_u(kmax,:,:);
        z_v(:,:) = data_v(kmax,:,:);
        speed = NaN(size(zm));
    end
    %
    for j = 1:jmax,
        for i = 1:imax,
            if topo(j,i) == 0.0
                xm(j,i) = NaN;
                ym(j,i) = NaN;
                zm(j,i) = NaN;
                z_u(j,i) = NaN;
                z_v(j,i) = NaN;
            else
                if ~isempty(maskid)
                    if mask(j,i) == 0
                        zm(j,i) = NaN;
                    end
                end
                for k = data_kmin:data_kmax,
                    if ((data(k,j,i) > -1.0E6) && (data(k,j,i) < 0.9E36))
                        zm(j,i) = zm(j,i) + data_M(k,j,i)*data(k,j,i);
                        zm_M(j,i) = zm_M(j,i) + data_M(k,j,i);
                        zm_count(j,i) = zm_count(j,i) + 1;
                        if (data(k,j,i) > zm_maxval(j,i)),
                            zm_maxval(j,i)   = data(k,j,i);
                            zm_maxk(j,i) = k;
                        end
                        if (data(k,j,i) < zm_minval(j,i)),
                            zm_minval(j,i) = data(k,j,i);
                            zm_mink(j,i) = k;
                        end
                    end
                end
                if (plot_minval == 'y'),
                    % replace zm if requested -- minimum water column value
                    if (zm_minval(j,i) == 0.9E36), zm_minval(j,i) = NaN; end
                    zm(j,i) = zm_minval(j,i);
                elseif (plot_maxval == 'y'),
                    % replace zm if requested -- maximum water column value
                    if (zm_maxval(j,i) == -1.0E6), zm_maxval(j,i) = NaN; end
                    zm(j,i) = zm_maxval(j,i);
                elseif (plot_av_conc == 'y'),
                    % convert to concentration if requested
                    zm(j,i) = zm(j,i)/zm_M(j,i);
                end
                if (zm_count(j,i) > 0)
                    if data_log10 == 'y'
                        if (zm(j,i) > 0.0)
                            zm(j,i) = log10(zm(j,i)/data_scale);
                        else
                            if contour_noneg == 'y'
                                zm(j,i) = 0.0;
                            else
                                zm(j,i) = NaN;
                            end
                        end
                    else
                        zm(j,i) = zm(j,i)/data_scale;
                    end
                else
                    zm(j,i) = NaN;
                end
            end
            if (data_uv == 'y'), speed(j,i) = data_scale*(z_u(j,i)^2.0 + z_v(j,i)^2.0)^0.5; end
            if ~isnan(zm(j,i)), n = n + 1; end
        end
    end
elseif (kplot == -1),
    % benthic layer
    zm = zeros(jmax,imax);
    z_u(:,:) = zeros(jmax,imax);
    z_v(:,:) = zeros(jmax,imax);
    for j = 1:jmax,
        for i = 1:imax,
            loc_k = grid_k1(j,i);
            if ((loc_k > data_kmax) || (loc_k < data_kmin)),
                zm(j,i) = NaN;
            elseif (data(loc_k,j,i) < -1.0E6) || (data(loc_k,j,i) > 1.0E30)
                zm(j,i) = NaN;
            else
                zm(j,i) = data(loc_k,j,i);
                if ~isempty(maskid)
                    if mask(j,i) == 0
                        zm(j,i) = NaN;
                    end
                end
                if data_log10 == 'y'
                    if (zm(j,i) > 0.0)
                        zm(j,i) = log10(zm(j,i)/data_scale);
                    else
                        if contour_noneg == 'y'
                            zm(j,i) = 0.0;
                        else
                            zm(j,i) = NaN;
                        end
                    end
                else
                    zm(j,i) = zm(j,i)/data_scale;
                end
                if ~isnan(zm(j,i)), n = n + 1; end
            end
        end
    end
end
nmax = n;
% copy zm before it gets transformed ...
overlaydata_zm(:,:) = zm(:,:);
%
% *********************************************************************** %

% *********************************************************************** %
% *** LOAD (OPTIONAL) OVERLAY DATA ************************************** %
% *********************************************************************** %
%
if ~isempty(overlaydataid)
    % load overlay datafile
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
    overlaydata_size = size(overlaydata_raw(:,:));
    nmax=overlaydata_size(1);
    % create (i,j) from (lon,lat) and vice versa (depending on data input type)
    if (data_ijk == 'n')
        % convert (lon,lat) overlay data to (i,j)
        % NOTE: function 'calc_find_ij' takes input in order: (lon,lat)
        %       i.e., the same as the raw overlay data, which is (lon,lat) (i.e., (i,j)) format
        % NOTE: !!! gridded data is (j,i) !!!
        overlaydata_ijk(:,:) = zeros(size(overlaydata_raw(:,:)));
        for n = 1:nmax,
            overlaydata_ijk(n,1:2) = calc_find_ij(overlaydata_raw(n,1),overlaydata_raw(n,2),grid_lon_origin,imax,jmax);
            overlaydata_ijk(n,3)   = calc_find_k(overlaydata_raw(n,3),kmax);
        end
        if kplot > 0,
            % delete data lines with depth levels not equal to kplot
            wronglayer_locations = find(overlaydata_ijk(:,3)~=kplot);
            wronglayer_n = size(wronglayer_locations);
            overlaydata_ijk(wronglayer_locations,:) = [];
            overlaydata_raw(wronglayer_locations,:) = [];
            nmax = nmax-wronglayer_n(1);
            if (nmax == 0),
                disp([' ']);
                error('*WARNING*: No data corresponding to this ocean level: ENDING ... ');
            end
        end
        % convert to double and set overlay data
        overlaydata_ijk(:,:) = double(overlaydata_ijk(:,:));
        overlaydata_ijk(:,4) = overlaydata_raw(:,4);
    else
        % convert (i,j) overlay data to (lon,lat)
        % NOTE: save (i,j,k) data first
        overlaydata_ijk(:,:) = overlaydata_raw(:,:);
        overlaydata_raw(:,1) = grid_lon_origin + 360.0*(overlaydata_raw(:,1) - 0.5)/jmax;
        overlaydata_raw(:,2) = 180.0*asin(2.0*(overlaydata_raw(:,2) - 0.5)/jmax - 1.0)/pi;
    end
    % remove data in land cells (or for k == 0 selection)
    if (data_land == 'n')
        for n = 1:nmax,
            if (isnan(overlaydata_zm(int32(overlaydata_ijk(n,2)),int32(overlaydata_ijk(n,1)))) || (kplot == 0))
                overlaydata_raw(n,4) = NaN;
                overlaydata_ijk(n,4) = NaN;
            end
        end
    end
    overlaylabel_raw(isnan(overlaydata_raw(:,4)),:) = [];
    overlaydata_raw(isnan(overlaydata_raw(:,4)),:) = [];
    overlaydata_ijk(isnan(overlaydata_ijk(:,4)),:) = [];
    % update value of nmax
    overlaydata_size = size(overlaydata_raw(:,:));
    nmax=overlaydata_size(1);
    %
    overlaylabel(:,:) = overlaylabel_raw(:,:);
    overlaydata(:,:) = overlaydata_raw(:,:);
    % convert lat to sin(lat) for plotting
    if (plot_equallat == 'n'), overlaydata(:,2) = sin(pi*overlaydata_raw(:,2)/180.0); end
    % grid (and average per cell) data if requested
    % NOTE: data vector length is re-calculated and the value of nmax reset
    if (data_ijk_mean == 'y')
        overlaydata_ijk_old(:,:) = overlaydata_ijk(:,:);
        overlaydata_ijk(:,:) = [];
        overlaydata(:,:)    = [];
        m=0;
        for i = 1:imax,
            for j = 1:jmax,
                if (~isnan(overlaydata_zm(j,i)))
                    samecell_locations = find((int32(overlaydata_ijk_old(:,1))==i)&(int32(overlaydata_ijk_old(:,2))==j)&(int32(overlaydata_ijk_old(:,3))==kplot));
                    samecell_n = size(samecell_locations);
                    if (samecell_n(1) > 0)
                        m=m+1;
                        samecell_mean = mean(overlaydata_ijk_old(samecell_locations,4));
                        overlaydata_ijk(m,:) = [i j kplot samecell_mean];
                        overlaydata(m,1) = grid_lon_origin + 360.0*(overlaydata_ijk(m,1) - 0.5)/jmax;
                        overlaydata(m,2) = 2.0*(overlaydata_ijk(m,2) - 0.5)/jmax - 1.0;
                        overlaydata(m,3) = double(laym(j,i));
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
% *** TAYLOR DIAGRAM **************************************************** %
% *********************************************************************** %
%
% calculate stats needed for Taylor Diagram (and plot it!)
%
% *** 3D (GRIDDED) DATA ************************************************* %
%
if (~isempty(dataid_2))
    % transform data sets in vectors
    if kplot > 0
        data_vector_1 = reshape(data_1(kplot,:,:),imax*jmax,1);
        data_vector_2 = reshape(data_2(kplot,:,:),imax*jmax,1);
    elseif (kplot == 0)
        data_vector_1 = reshape(data_1(:,:,:),imax*jmax*kmax,1);
        data_vector_2 = reshape(data_2(:,:,:),imax*jmax*kmax,1);
    elseif (kplot == -1)
            
    end
    % filter data
    data_vector_1(find(data_vector_1(:) < -1.0E6)) = NaN;
    data_vector_1(find(data_vector_1(:) > 0.9E36)) = NaN;
    data_vector_2(find(data_vector_2(:) < -1.0E6)) = NaN;
    data_vector_2(find(data_vector_2(:) > 0.9E36)) = NaN;
    if isempty(overlaydataid), nmax = length(data_vector_2); end
    if (data_stats == 'y')
        % calculate stats
        % NOTE: STATM = allstats(Cr,Cf)
        % 	    STATM(1,:) => Mean
        % 	    STATM(2,:) => Standard Deviation (scaled by N)
        % 	    STATM(3,:) => Centered Root Mean Square Difference (scaled by N)
        % 	    STATM(4,:) => Correlation
        %       STATM(5,:) => N
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
% NOTE: no scale transformatoin has been appplied
%       to either gridded or % overlay data
% NOTE: valid only for data on a single depth level
if (~isempty(overlaydataid) && ((data_only == 'n') || (data_anomoly == 'y')))
    % set overlay data vector
    data_vector_1 = overlaydata(:,4);
    % populate the gridded dataset vector with values corresponding to
    % the overlay data locations
    % NOTE: !!! data is (k,j,i) !!! (=> swap i and j)
    % NOTE: re-orientate data_vector_2 to match data_vector_1
    for n = 1:nmax,
        if kplot > 0,
            data_vector_2(n) = data(kplot,int32(overlaydata_ijk(n,2)),int32(overlaydata_ijk(n,1)));
        elseif (kplot == 0)
            data_vector_2(n) = NaN;
        elseif (kplot == -1)
            
        end
    end
    data_vector_2 = data_vector_2';
    % filter data
    data_vector_2(find(data_vector_2(:) < -1.0E6)) = NaN;
    data_vector_2(find(data_vector_2(:) > 0.9E36)) = NaN;
    if (data_stats == 'y')
        % calculate stats
        STATM = calc_allstats(data_vector_1,data_vector_2);
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
if (data_stats == 'y')
    if (~isempty(dataid_2) | (~isempty(overlaydataid) && ((data_only == 'n') || (data_anomoly == 'y'))))
        fid = fopen([filename '_STATS' '.dat'], 'wt');
        fprintf(fid, 'Number of data points, N                           : %4i \n', nmax);
        fprintf(fid, '\n');
        fprintf(fid, 'Stats summary: reference data');
        fprintf(fid, '\n');
        fprintf(fid, 'Mean                                               : %8.6e \n', STATM(1,1));
        fprintf(fid, 'Standard Deviation (scaled by N)                   : %8.6e \n', STATM(2,1));
        fprintf(fid, 'Root Mean Square Difference (scaled by N)          : %8.6e \n', STATM(6,1));
        fprintf(fid, 'Centered Root Mean Square Difference (scaled by N) : %8.6e \n', STATM(3,1));
        fprintf(fid, 'Correlation                                        : %8.6e \n', STATM(4,1));
        fprintf(fid, '\n');
        fprintf(fid, 'Stats summary: model data');
        fprintf(fid, '\n');
        fprintf(fid, 'Mean                                               : %8.6e \n', STATM(1,2));
        fprintf(fid, 'Standard Deviation (scaled by N)                   : %8.6e \n', STATM(2,2));
        fprintf(fid, 'Root Mean Square Difference (scaled by N)          : %8.6e \n', STATM(6,2));
        fprintf(fid, 'Centered Root Mean Square Difference (scaled by N) : %8.6e \n', STATM(3,2));
        fprintf(fid, 'Correlation                                        : %8.6e \n', STATM(4,2));
        fclose(fid);
    end
end
%
% *** SAVE EQUIVALENT MODEL DATA **************************************** %
%
% save model data at the data locations
if (~isempty(overlaydataid) && (data_only == 'n')),
    fid = fopen([filename '_MODELPOINTS', '.', str_date '.dat'], 'wt');
    fprintf(fid, '%% Model value at data locations');
    fprintf(fid, '\n');
    fprintf(fid, '%% Format: i, j, k, lon, lat, depth, model value, data value, label');
    fprintf(fid, '\n');
    for n = 1:nmax,
        if (plot_equallat == 'y'),
            fprintf(fid, '%d %d %d %8.3f %8.3f %8.3f %8.6e %8.6e %s \n', int16(overlaydata_ijk(n,1)), int16(overlaydata_ijk(n,2)), int16(overlaydata_ijk(n,3)), overlaydata(n,1), overlaydata(n,2), overlaydata(n,3), data_vector_2(n), overlaydata(n,4), overlaylabel(n,:));
        else
            fprintf(fid, '%d %d %d %8.3f %8.3f %8.3f %8.6e %8.6e %s \n', int16(overlaydata_ijk(n,1)), int16(overlaydata_ijk(n,2)), int16(overlaydata_ijk(n,3)), overlaydata(n,1), 180.0*asin(overlaydata(n,2))/pi, overlaydata(n,3), data_vector_2(n), overlaydata(n,4), overlaylabel(n,:));
        end
    end
    fclose(fid);
elseif (~isempty(overlaydataid)),
    fid = fopen([filename '_DATAPOINTS', '.', str_date '.dat'], 'wt');
    fprintf(fid, 'Model value at data locations');
    fprintf(fid, '\n');
    fprintf(fid, 'Format: lon, lat, depth, data value, label');
    fprintf(fid, '\n');
    for n = 1:nmax,
        if (plot_equallat == 'y'),
            fprintf(fid, '%8.3f %8.3f %8.3f %8.6e %s \n', overlaydata(n,1), overlaydata(n,2), overlaydata(n,3), overlaydata(n,4), overlaylabel(n,:));
        else
            fprintf(fid, '%8.3f %8.3f %8.3f %8.6e %s \n', overlaydata(n,1), 180.0*asin(overlaydata(n,2))/pi, overlaydata(n,3), overlaydata(n,4), overlaylabel(n,:));
        end
    end
    fclose(fid);
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
    % redefine model grid location values so as to all appear white
    if (data_only == 'y')
        zm = zeros(size(zm(:,:)));
        zm(find(zm(:,:) == 0)) = NaN;
    end
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** TRANSFORM LON GRID ************************************************ %
% *********************************************************************** %
%
% extend gridded data in +/- longitude
xm_ex = [xm - 360.0 xm + 000.0 xm + 360.0];
ym_ex = [ym + 000.0 ym + 000.0 ym + 000.0];
zm_ex = [zm zm zm];
z_u_ex = [z_u z_u z_u];
z_v_ex = [z_v z_v z_v];
topo_ex = [topo topo topo];
lonm_ex = [lonm - 360.0 lonm + 000.0 lonm + 360.0];
lone_ex = [lone - 360.0 lone + 000.0 lone + 360.0];
lonw_ex = [lonw - 360.0 lonw + 000.0 lonw + 360.0];
layb_ex = [layb layb layb];
% shorten to conform to desired lon start value
lon_start = min(min(lonw));
i_start = round((lon_min-(lon_start-360.0))/(360.0/jmax)) + 1;
xm = xm_ex(:,i_start:i_start+imax-1);
ym = ym_ex(:,i_start:i_start+imax-1);
zm = zm_ex(:,i_start:i_start+imax-1);
z_u = z_u_ex(:,i_start:i_start+imax-1);
z_v = z_v_ex(:,i_start:i_start+imax-1);
topo = topo_ex(:,i_start:i_start+imax-1);
lonm = lonm_ex(:,i_start:i_start+imax-1);
lone = lone_ex(:,i_start:i_start+imax-1);
lonw = lonw_ex(:,i_start:i_start+imax-1);
layb = layb_ex(:,i_start:i_start+imax-1);
if ~isempty(overlaydataid)
    % force discrete data to lie within longitude plotting axis
    % (lon_min to lon_min + 360)
    for n = 1:nmax,
        if (overlaydata(n,1) < lon_min)
            overlaydata(n,1) = overlaydata(n,1) + 360.0;
        end
        if (overlaydata(n,1) > (lon_min + 360.0))
            overlaydata(n,1) = overlaydata(n,1) - 360.0;
        end
    end
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** PLOT MAIN FIGURE ************************************************** %
% *********************************************************************** %
%
if (plot_main == 'y'),
    %
    % *** CONFIGURE AND CREATE PLOTTING WINDOW ****************************** %
    %
    % create figure
    scrsz = get(0,'ScreenSize');
    figure('Position',[((1 - plot_dscrsz)/2)*plot_dscrsz*scrsz(3) (1 - plot_dscrsz)*plot_dscrsz*scrsz(4) plot_dscrsz*scrsz(3) 0.60*plot_dscrsz*scrsz(4)])
    clf;
    % define plotting regions
    if (plot_format_old == 'y')
        fh(1) = axes('Position',[0 0 1 1],'Visible','off');
        fh(2) = axes('Position',[0.10 0.05 0.65 0.90]);
        fh(3) = axes('Position',[0.80 0.27 0.20 0.46],'Visible','off');
    else
        fh(1) = axes('Position',[0 0 1 1],'Visible','off');
        fh(2) = axes('Position',[0.15 0.15 0.65 0.70]);
        fh(3) = axes('Position',[0.75 0.15 0.15 0.70],'Visible','off');
    end
    % define colormap
    cmap = make_cmap(colorbar_name,con_n+2);
    if (colorbar_inv == 'y'), cmap = flipdim(cmap,1); end,
    colormap(cmap);
    % date-stamp plot
    set(gcf,'CurrentAxes',fh(1));
    if (plot_format_old == 'y')
        text(0.95,0.50,[str_function, ' / ', 'on: ', str_date],'FontName','Arial','FontSize',8,'Rotation',90.0,'HorizontalAlignment','center','VerticalAlignment','top');
    else
        text(0.85,0.50,[str_function, ' / ', 'on: ', str_date],'FontName','Arial','FontSize',8,'Rotation',90.0,'HorizontalAlignment','center','VerticalAlignment','top');
    end
    %
    % *** SET PLOT SCALE **************************************************** %
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
    % *** CREATE MAIN PLOT ************************************************** %
    %
    set(gcf,'CurrentAxes',fh(2));
    hold on;
    % set color and lat/lon axes and labels
    caxis([con_min-(con_max-con_min)/con_n con_max]);
    set(gca,'PlotBoxAspectRatio',[1.0 plot_xy_scaling*0.5 1.0]);
    if plot_global,
        axis([lon_min lon_max lat_min lat_max]);
        set(gca,'XLabel',text('String','Longitude','FontSize',15),'XTick',[lon_min:plot_lon_delta:lon_max]);
        if (plot_equallat == 'n'),
            set(gca,'YLabel',text('String','Latitude','FontSize',15),'YTick',[-1 -0.866 -0.5 0 0.5 0.866 1], 'YTickLabel',{'-90';'-60';'-30';'0';'30';'60';'90'});
        else
            set(gca,'YLabel',text('String','Latitude','FontSize',15),'YTick',[-90.0 -60.0 -30.0 0 30.0 60.0 90.0], 'YTickLabel',{'-90';'-60';'-30';'0';'30';'60';'90'});
        end
    else
        axis([plot_lon_min plot_lon_max plot_lat_min plot_lat_max]);
        set(gca,'XLabel',text('String','Longitude','FontSize',15),'XTick',[plot_lon_min plot_lon_max]);
        if (plot_equallat == 'n'),
            set(gca,'YLabel',text('String','Latitude','FontSize',15),'YTick',[plot_lat_min plot_lat_max], 'YTickLabel',{num2str(180*asin(plot_lat_min)/pi);num2str(180*asin(plot_lat_max)/pi)});
        else
            set(gca,'YLabel',text('String','Latitude','FontSize',15),'YTick',[plot_lat_min plot_lat_max], 'YTickLabel',{num2str(plot_lat_min);num2str(plot_lat_max)});
        end
    end
    set(gca,'TickDir','out');
    if ~isempty(plot_title)
        title(plot_title,'FontSize',18);
    else
        title(['Data: ',strrep(dataid_1,'_',' '),' / Level (k) = ', num2str(kplot)],'FontSize',12);
    end
    % draw filled rectangles
    for i = 1:imax,
        for j = 1:jmax,
            if topo(j,i) > layb(j,i)
                h = patch([lonw(j,i) lonw(j,i) lone(j,i) lone(j,i)],[lats(j,i) latn(j,i) latn(j,i) lats(j,i)],color_g);
                set(h,'EdgeColor',color_g);
            else
                if (isnan(zm(j,i)))
                    h = patch([lonw(j,i) lonw(j,i) lone(j,i) lone(j,i)],[lats(j,i) latn(j,i) latn(j,i) lats(j,i)],[1 1 1]);
                    set(h,'EdgeColor',[1 1 1]);
                else
                    col = 1 + round(0.5+con_n*(zm(j,i)-con_min)/(con_max-con_min));
                    if col < 1, col = 1; end
                    if col > con_n+2, col = con_n+2; end
                    h = patch([lonw(j,i) lonw(j,i) lone(j,i) lone(j,i)],[lats(j,i) latn(j,i) latn(j,i) lats(j,i)],cmap(col,:));
                    set(h,'EdgeColor',cmap(col,:));
                end
            end
        end
    end
    %
    % *** PLOT CONTINENTAL OUTLINE ****************************************** %
    %
    % draw continental outline
    for j = 1:jmax,
        for i = 1:imax-1,
            if topo(j,i) > layb(j,i)
                if topo(j,i+1) <= layb(j,i+1)
                    h = plot([lone(j,i) lone(j,i)],[lats(j,i) latn(j,i)],'k-');
                    set(h,'LineWidth',1.0);
                end
            end
        end
        for i = 2:imax,
            if topo(j,i) > layb(j,i)
                if topo(j,i-1) <= layb(j,i-1)
                    h = plot([lonw(j,i) lonw(j,i)],[lats(j,i) latn(j,i)],'k-');
                    set(h,'LineWidth',1.0);
                end
            end
        end
    end
    for i = 1:imax,
        for j = 1:jmax-1,
            if topo(j,i) > layb(j,i)
                if topo(j+1,i) <= layb(j+1,i)
                    h = plot([lonw(j,i) lone(j,i)],[latn(j,i) latn(j,i)],'k-');
                    set(h,'LineWidth',1.0);
                end
            end
        end
        for j = 2:jmax,
            if topo(j,i) > layb(j,i)
                if topo(j-1,i) <= layb(j-1,i)
                    h = plot([lonw(j,i) lone(j,i)],[lats(j,i) lats(j,i)],'k-');
                    set(h,'LineWidth',1.0);
                end
            end
        end
    end
    %
    % *** OVERLAY CONTOURS ************************************************** %
    %
    if (contour_plot == 'y') && (data_only == 'n'),
        v = [con_min:(con_max-con_min)/(con_n/contour_mod):con_max];
        if (plot_equallat == 'n'),
            [C,h] = contour(xm_ex,sin(pi*ym_ex/180.0),zm_ex,v,'k-');
        else
            [C,h] = contour(xm_ex,ym_ex,zm_ex,v,'k-');
        end
        set(h,'LineWidth',0.25);
        v = [con_min:(con_max-con_min)/(con_n/contour_mod_label):con_max];
        if (plot_equallat == 'n'),
            [C,h] = contour(xm_ex,sin(pi*ym_ex/180.0),zm_ex,v,'k');
        else
            [C,h] = contour(xm_ex,ym_ex,zm_ex,v,'k');
        end
        set(h,'LineWidth',0.75);
        if data_log10 == 'y'
            %%%%%%%%
        elseif contour_label == 'y'
            clabel(C,h);
        end
        if (contour_hlt == 'y'),
            v = [contour_hltval];
            if (plot_equallat == 'n'),
                [C,h] = contour(xm_ex,sin(pi*ym_ex/180.0),zm_ex,v,'k');
            else
                [C,h] = contour(xm_ex,ym_ex,zm_ex,v,'k');
            end
            set(h,'LineWidth',1.5);
        end
    end
    %
    % *** OVERLAY VELOCITY FIELD ******************************************** %
    %
    % plot velocity field if requested
    if (data_uv == 'y'),
        if (plot_equallat == 'n'),
            [h] = scatter(reshape(xm(:,:),jmax*imax,1),sin(pi*reshape(ym(:,:),jmax*imax,1)/180.0),2.5,'filled','k');
            [h] = quiver(xm,sin(pi*ym/180.0),z_u,sin(pi*z_v/180.0),data_uv_scale,'k','MaxHeadSize',0.0);
        else
            [h] = scatter(reshape(xm(:,:),jmax*imax,1),reshape(ym(:,:),jmax*imax,1),2.5,'filled','k');
            [h] = quiver(xm,ym,z_u,z_v,data_uv_scale,'k','MaxHeadSize',0.0);
        end
        set(h,'LineWidth',0.75);
    end
    %
    % *** OVERLAY DATA ****************************************************** %
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
        for n = 1:nmax,
            if (data_siteonly == 'n'),
                scatter(overlaydata(n,1),overlaydata(n,2),4,overlaydata(n,4)/data_scale,overlaydata_shape(n),'Filled','LineWidth',data_sitelineth,'Sizedata',data_size,'MarkerEdgeColor',overlaydata_ecol(n));
            else
                if (overlaydata_fcol(n) == '-'),
                    scatter(overlaydata(n,1),overlaydata(n,2),4,overlaydata_shape(n),'LineWidth',data_sitelineth,'Sizedata',data_size,'MarkerEdgeColor',overlaydata_ecol(n));
                else
                    scatter(overlaydata(n,1),overlaydata(n,2),4,overlaydata_shape(n),'LineWidth',data_sitelineth,'Sizedata',data_size,'MarkerEdgeColor',overlaydata_ecol(n),'MarkerFaceColor',overlaydata_fcol(n));
                end
            end
        end
        if (data_sitelabel == 'y'),
            text(overlaydata(:,1)+(data_fontsz/10)*(data_size/15),overlaydata(:,2)+(data_fontsz/15)*(data_size/400),overlaylabel(:,:),'FontSize',data_fontsz,'Color',data_sitecolor);
        end
    end
    %
    % *** PLOT BORDER ******************************************************* %
    %
    % draw plot border
    h = plot([lon_min lon_max],[lat_min lat_min],'k-');
    set(h,'LineWidth',1.0);
    h = plot([lon_min lon_max],[lat_max lat_max],'k-');
    set(h,'LineWidth',1.0);
    h = plot([lon_min lon_min],[lat_min lat_max],'k-');
    set(h,'LineWidth',1.0);
    h = plot([lon_max lon_max],[lat_min lat_max],'k-');
    set(h,'LineWidth',1.0);
    %
    hold off;
    %
    % *** CREATE COLOR BAR ************************************************** %
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
    % *** PRINT PLOT ******************************************************** %
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
    % *** PLOT FIGURE (cross-plot) ************************************** %
    %
    if ( ~isempty(dataid_2) || (~isempty(overlaydataid) && (data_only == 'n')) ),
        %
        if ~isempty(dataid_2),
            loc_x_data = data_vector_1;
            loc_y_data = data_vector_2;
            loc_x_label = [strrep(dataid_1,'_','-')];
            loc_y_label = [strrep(dataid_2,'_','-')];
        elseif ~isempty(overlaydataid)
            loc_x_data = data_vector_1;
            loc_y_data = data_vector_2;
            loc_x_label = [strrep(overlaydataid,'_','-')];
            loc_y_label = [strrep(dataid_1,'_','-')];
        end
        %
        plot_crossplotc(loc_x_data,loc_y_data,[],loc_x_label,loc_y_label,'',POPT,[filename '.CROSSPLOT']);
        %
    end
    %
    % *** SAVE DATA (cross-plot relationships) ************************** %
    %
    if ( ~isempty(dataid_2) || (~isempty(overlaydataid) && (data_only == 'n')) ), fprint_1Dn_d([loc_x_data loc_y_data],[filename '.CROSSPLOT.', str_date, '.res']); end
    %
    % *** SAVE DATA (benthic) ******************************************* %
    %
    if (isempty(dataid_2) && isempty(overlaydataid) && (kplot == -1)),
        loc_x_data = reshape(zm(:,:),imax*jmax,1);
        loc_y_data = reshape(topo(:,:),imax*jmax,1);
        loc_x_label = [strrep(dataid_1,'_','-')];
        loc_y_label = ['depth'];
        fprint_1Dn_d([loc_x_data loc_y_data],[filename '.BENTHIC.', str_date, '.res']);
    end
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
%
% *********************************************************************** %
