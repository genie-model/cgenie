function [] = plot_sedcore(PEXP,PCORE,PMIN,PMAX,PREFAGE,PDATA1,PDATA2,POPT,PNAME)
% plot_sedcore
%
%   ***********************************************************************
%   *** PLOT SEDCORE OUTPUT [carbonate proxies] ***************************
%   ***********************************************************************
%
%   plot_sedcore(PEXP,PCORE,PMIN,PMAX,PREFAGE,PDATA1,PDATA2,POPT)
%   plots a SEDGEM sedcore from 'sedcore.nc' and takes 8 %arguments:
%
%   PEXP [STRING] (e.g. 'preindustrial_spinup')
%   --> the experiment name
%   PCORE [STRING] (e.g. '0102')
%   --> location id of the sedcore to be plotted
%   --> valid sedcore locations will be listed if an invalid name is given
%   PMIN [REAL] (e.g. -50.0)
%   --> *minimum* plotted height/time as:
%       (i) an anomoly compared to a reference [opt_relreflevel = true]
%       (ii) absolute relative to the suface [opt_relreflevel = false]
%       with +ve. values representing younger or stratigraphically higher
%       than the reference height/time
%   PMAX [REAL] (e.g. -50.0)
%   --> *maximum* plotted height/time as:
%       (i) an anomoly compared to a reference [opt_relreflevel = true]
%       (ii) absolute relative to the suface [opt_relreflevel = false]
%       with +ve. values representing younger or stratigraphically higher
%       than the reference height/time
%   PREFAGE [REAL] (e.g. 50.0)
%   --> reference age, usually the run length (age 0.0 is a valid value)
%   --> enter any value less than 0.0 (e.g. -1) to automatically identify
%       the run start and plot as a function of depth relative to this
%   PDATA1 [STRING] (e.g. 'sed_LiCO3_7Li')
%   --> variable name for additional data to plot
%   PDATA2 [STRING] (e.g. 'sed_CaCO3_44Ca')
%   --> variable name for additional data to plot
%   POPT [STRING] (e.g., 'plotting_config_2')
%   --> the string for an alternative plotting parameter set
%   --> if an empty (i.e., '') value is passed to this parameter
%       then the default parameter set is used (plot_sedcore_settings)
%   PNAME [STRING] (e.g., 'my_plot')
%   --> the string for an alternative filename
%   --> if an empty (i.e., '') value is passed to this parameter
%       then a filename is automatically generated
%
%   ***********************************************************************
%   *** HISTORY ***********************************************************
%   ***********************************************************************
%
%   13/06/24: CREATED [copied from 'plot_sedcore.m']
%   13/06/25: features populated to create a basic working function
%   13/06/29: completion of initial working version
%   13/07/xx: stuff ...
%   13/07/07: changed to a primary color tracer reference level basis
%   13/07/08: edited 'help'
%             extended wt% CaCO3 plotting threshold to additional tracers
%   13/08/01: generalized plotting for non-specific tracers
%   13/11/26: removed 2nd core (experiment) option throughout to simplify
%             changed function inputs
%             chnaged function name!
%   13/11/27: further development ...
%   13/11/28: further development ...
%             chnaged function name! (again!)
%             seperated out plotting parameters into seperate m-file
%             added fix for old sedcore output (with core break/buck-up)
%   13/11/29: fixed help text and ... further development ...
%   13/11/30: completed as-age plotting (as an alternative to depth)
%             added checking of entered parameter values
%             added seperate per depth / age plot saving
%             enabled plotting according to specified absolute limits
%             add check for zero ash in plotting interval
%             masked out (grey) non-carbonate intervals (CaCO3 age scale)
%   13/12/02: resolved x-label issues
%             some labelling adjustments
%   13/12/02: for by-age plots, NOW plot depth in panel 3 x-axis
%             scaled ash by det to avoid 'double peaks' during carbonate
%             dissolution events
%             further depth/age scaling changes
%             added option for manual setting of age model x-axis scale
%   13/12/03: minor tweaks ...
%   13/12/14: added threshold for finding ash peak
%             added plotting of time-series data as alternative to
%             optional sedcore variables #1 and #2
%   13/12/29: added file format selection for 'new' plotting
%             fix for plotting on a depth basis but selecting time-series
%   14/01/15: minor bug-fix
%   14/01/21: adjusted filenaming for det age model
%             simplified use of non CaCO3 age model
%   14/01/26: added parameter to set time-series column to plot
%   14/01/26: aded internal ash peak based age scale option
%   14/04/17: corrected det vs. ash age filenaming
%   14/04/18: adjusted ash vs. det vs. CaCO3 age parameter options
%             added age scale based on ash peak & assuming const. det acc.
%             added option to offset ash peak from the reference age
%   14/06/01: added option for fatter panel plots (if no time-series)
%   14/06/02: minor adjustment to panel sizes ... [should be an option!]
%   14/10/09: added optional al filename
%   14/10/17: auto plot format
%   14/11/09: fixed time-series time scale bug
%   14/11/16: fixed array 'start' bug
%             fixed int16 bug (for veyr long sedcores)
%             adjusted plotting @ low wt% CaCO3
%   14/11/16: specified renderer for postscript output
%   15/01/11: switched built-in color scale def for call to make_cmap
%
%   ***********************************************************************

% *********************************************************************** %
% *** INITIALIZE ******************************************************** %
% *********************************************************************** %
%
% close plot windows
close all;
% load plotting options
if isempty(POPT), POPT='plot_sedcore_settings'; end
eval(POPT);
% set dummy variables
expid = PEXP;
coreid = PCORE;
refage = PREFAGE;
data1 = PDATA1;
data2 = PDATA2;
axis_Dmin = PMIN;
axis_Dmax = PMAX;
axis_Amin = PMIN;
axis_Amax = PMAX;
altfilename = PNAME;
% extract i and j values
coreid_i = str2num(coreid(1:2));
coreid_j = str2num(coreid(3:4));
% set date
str_date = [datestr(date,11), datestr(date,5), datestr(date,7)];
% set function name
str_function = 'plot-fields-sedgem-sedcore';
% set filename
str_filename = [expid];
% determine age/depth or assume automatic reference level
if (refage >= 0.0),
    opt_refage=true;
else
    opt_refage=false;
end
% set age model
if (~opt_ashagemodel && ~opt_detagemodel),
    opt_CaCO3agemodel=true;
else
    opt_CaCO3agemodel=false;
end
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
% define grey color
color_g = [0.75 0.75 0.75];
% DEFINE wt% COLOR MAP
cmap = make_cmap('wt%',101);
%
% *** SET OUTPUT FILESTRING ********************************************* %
%
str_filename = [expid];
%
% *********************************************************************** %

% *********************************************************************** %
% *** OPEN netCDF DATA FILE ********************************************* %
% *********************************************************************** %
%
% open netCDF file
ncid=netcdf.open([data_path '/' expid '/sedgem/sedcore.nc'],'nowrite');
% read netCDf information
[ndims,nvars,ngatts,unlimdimid] = netcdf.inq(ncid);
%
% *********************************************************************** %

% *********************************************************************** %
% *** SET UP GRID ******************************************************* %
% *********************************************************************** %
%
% set number of layers
varid = netcdf.inqVarID(ncid,'grid_nrst');
grid_layn = netcdf.getVar(ncid,varid);
% set number of sedcores
coren_max = length(grid_layn);
% load (i,j) location data
varid  = netcdf.inqVarID(ncid,'grid_i');
grid_i = netcdf.getVar(ncid,varid);
varid  = netcdf.inqVarID(ncid,'grid_j');
grid_j = netcdf.getVar(ncid,varid);
%
% *********************************************************************** %

% *********************************************************************** %
% *** LOAD SEDCORE DATA ************************************************* %
% *********************************************************************** %
%
% *** SET SEDCORE ******************************************************* %
%
% check that the (i,j) core location exists
coren = [];
while isempty(coren)
    for n = 1:coren_max,
        if ((coreid_i == grid_i(n)) && (coreid_j == grid_j(n)))
            coren = n;
        end
    end
    if isempty(coren)
        disp('   > WARNING: Sedcore location does not exist. Re-input one of:');
        for n = 1:coren_max,
            if (grid_i(n) < 10)
                stri = ['0' num2str(grid_i(n))];
            else
                stri = [num2str(grid_i(n))];
            end
            if (grid_j(n) < 10)
                strj = ['0' num2str(grid_j(n))];
            else
                strj = [num2str(grid_j(n))];
            end
            disp(['(' stri ',' strj ')'])
        end
        coreid = input('   > Sedcore ID: ','s');
        coreid_i = str2num(coreid(1:2));
        coreid_j = str2num(coreid(3:4));
    end
end
%
% *** LOAD SEDCORE DATA ************************************************* %
%
% load data -- #m
varid = netcdf.inqVarID(ncid,'phys_layer');
[varname,xtype,dimids,natts] = netcdf.inqVar(ncid,varid);
rawdata = netcdf.getVar(ncid,varid);
data = rawdata(coren,:)';
data_maxm = length(data);
% load data -- dbs
varid = netcdf.inqVarID(ncid,'phys_depth');
[varname,xtype,dimids,natts] = netcdf.inqVar(ncid,varid);
rawdata = netcdf.getVar(ncid,varid);
data(:,2) = rawdata(coren,:)';
% load data -- CaCO3 age
varid = netcdf.inqVarID(ncid,'age_CaCO3');
[varname,xtype,dimids,natts] = netcdf.inqVar(ncid,varid);
rawdata = netcdf.getVar(ncid,varid);
data(:,3) = rawdata(coren,:)';
% load data -- wt% CaCO3
varid = netcdf.inqVarID(ncid,'sed_CaCO3');
[varname,xtype,dimids,natts] = netcdf.inqVar(ncid,varid);
rawdata = netcdf.getVar(ncid,varid);
data(:,4) = rawdata(coren,:)';
% load data -- wt% CaCO3 d13C
varid = netcdf.inqVarID(ncid,'sed_CaCO3_13C');
[varname,xtype,dimids,natts] = netcdf.inqVar(ncid,varid);
rawdata = netcdf.getVar(ncid,varid);
data(:,5) = rawdata(coren,:)';
% load data -- wt% ash
varid = netcdf.inqVarID(ncid,'sed_ash');
[varname,xtype,dimids,natts] = netcdf.inqVar(ncid,varid);
rawdata = netcdf.getVar(ncid,varid);
data(:,6) = rawdata(coren,:)';
% load data -- porosity
varid = netcdf.inqVarID(ncid,'phys_porosity');
[varname,xtype,dimids,natts] = netcdf.inqVar(ncid,varid);
rawdata = netcdf.getVar(ncid,varid);
data(:,7) = rawdata(coren,:)';
% load data -- layer thickness
varid = netcdf.inqVarID(ncid,'phys_thickness');
[varname,xtype,dimids,natts] = netcdf.inqVar(ncid,varid);
rawdata = netcdf.getVar(ncid,varid);
data(:,8) = rawdata(coren,:)';
% load data -- wt% det
varid = netcdf.inqVarID(ncid,'sed_det');
[varname,xtype,dimids,natts] = netcdf.inqVar(ncid,varid);
rawdata = netcdf.getVar(ncid,varid);
data(:,9) = rawdata(coren,:)';
tmp_data_det(:,1) = rawdata(coren,:)';
% load data -- ash age
varid = netcdf.inqVarID(ncid,'age_ash');
[varname,xtype,dimids,natts] = netcdf.inqVar(ncid,varid);
rawdata = netcdf.getVar(ncid,varid);
data(:,10) = rawdata(coren,:)';
% check which additional time tracer variables exist (and load)
% just in case: set opt_redrefage to false if red tracer does not exist
% NOTE: set age model to 'CaCO3' if det age tracer does not exist
% ash age
dataid = 'age_det';
var_age_det=false;
for n = 0:nvars-1,
    [varname,xtype,dimids,natts] = netcdf.inqVar(ncid,n);
    if strcmp(varname,dataid),
        var_age_det=true;
        varid = netcdf.inqVarID(ncid,'age_det');
        [varname,xtype,dimids,natts] = netcdf.inqVar(ncid,varid);
        rawdata = netcdf.getVar(ncid,varid);
        data(:,10) = rawdata(coren,:)';
        var_age_det=true;
        break
    else
        if (~opt_ashagemodel && opt_detagemodel),
            disp(['ERROR: No detrital age variable present.']);
            return;
        end
    end
end
if (~var_age_det), opt_CaCO3agemodel=true; end
% red CaCO3 tracer
dataid = 'sed_CaCO3_red';
var_sed_CaCO3_red=false;
for n = 0:nvars-1,
    [varname,xtype,dimids,natts] = netcdf.inqVar(ncid,n);
    if strcmp(varname,dataid),
        var_sed_CaCO3_red=true;
        varid = netcdf.inqVarID(ncid,'sed_CaCO3_red');
        [varname,xtype,dimids,natts] = netcdf.inqVar(ncid,varid);
        rawdata = netcdf.getVar(ncid,varid);
        data(:,11) = rawdata(coren,:)';
        var_sed_CaCO3_red=true;
        break
    end
end
if (~var_sed_CaCO3_red), opt_redrefage = false; end
% blue CaCO3 tracer
dataid = 'sed_CaCO3_blue';
var_sed_CaCO3_blue=false;
for n = 0:nvars-1,
    [varname,xtype,dimids,natts] = netcdf.inqVar(ncid,n);
    if strcmp(varname,dataid),
        var_sed_CaCO3_blue=true;
        varid = netcdf.inqVarID(ncid,'sed_CaCO3_blue');
        [varname,xtype,dimids,natts] = netcdf.inqVar(ncid,varid);
        rawdata = netcdf.getVar(ncid,varid);
        data(:,12) = rawdata(coren,:)';
        var_sed_CaCO3_blue=true;
        break
    end
end
% check which additional user-selected variables exist (and load)
% 1st tracer
if ~isempty(data1),
    dataid = data1;
    data1 = [];
    data1_ts = [];
    for n = 0:nvars-1,
        [varname,xtype,dimids,natts] = netcdf.inqVar(ncid,n);
        if strcmp(varname,dataid),
            varid = netcdf.inqVarID(ncid,dataid);
            [varname,xtype,dimids,natts] = netcdf.inqVar(ncid,varid);
            rawdata = netcdf.getVar(ncid,varid);
            data(:,13) = rawdata(coren,:)';
            data1 = dataid;
            break
        end
    end
    if (isempty(data1)),
        if (strcmp('.res',dataid(end-3:end))),
            file_data1 = [data_path '/' expid '/biogem/' dataid];
            if (exist(file_data1, 'file') == 2)
                data1_ts = load(file_data1,'ascii');
                data1 = dataid;
                if (~opt_refage), data1 = []; end
            else
                disp(['ERROR: BIOGEM time-series file ', file_data1, ' does not exist.']);
                return;
            end
        else
            disp(['ERROR: Variable ', dataid, ' does not exist.']);
        end
    end
end
% 2nd tracer
if ~isempty(data2),
    dataid = data2;
    data2 = [];
    data2_ts = [];
    for n = 0:nvars-1,
        [varname,xtype,dimids,natts] = netcdf.inqVar(ncid,n);
        if strcmp(varname,dataid),
            varid = netcdf.inqVarID(ncid,dataid);
            [varname,xtype,dimids,natts] = netcdf.inqVar(ncid,varid);
            rawdata = netcdf.getVar(ncid,varid);
            data(:,14) = rawdata(coren,:)';
            data2 = dataid;
            break
        end
    end
    if (isempty(data2)),
        if (strcmp('.res',dataid(end-3:end))),
            file_data2 = [data_path '/' expid '/biogem/' dataid];
            if (exist(file_data2, 'file') == 2)
                data2_ts = load(file_data2,'ascii');
                data2 = dataid;
                if (~opt_refage), data2 = []; end
            else
                disp(['ERROR: BIOGEM time-series file ', file_data2, ' does not exist.']);
                return;
            end
        else
            disp(['ERROR: Variable ', dataid, ' does not exist.']);
        end
    end
end
%
% *** FIX DATA ERRORS *************************************************** %
%
% scan down-core and fix any numerical/depth errors [for old GENIE version]
loc_m = 0;
for m=1:data_maxm,
    if (data(m,1) > loc_m),
        loc_m=m;
    else
        loc_m=m;
        break
    end
end
% test to see if there is a 'break' in the core
% => renumber layers and create new depth scale
if (loc_m < data_maxm),
    for m=loc_m:data_maxm,
        data(m,1) = m;
        data(m,2) = data(m-1,2) + 1.0;
    end
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** PROCESS SEDCORE DATA ********************************************** %
% *********************************************************************** %
%
% *** PROCESS SEDCORE DATA ********************************************** %
%
% clean up
data(find(data(:,:) > 9.999E9)) = NaN;
% replace CaCO3 age with det age
if (~opt_CaCO3agemodel), data(:,3) = data(:,10); end
% convert units
data(:,3) = data(:,3)/1000.0; % yr -> kyr
if (var_age_det), data(:,10) = data(:,10)/1000.0; end
if (var_sed_CaCO3_red), data(:,11) = 100.0*data(:,11); end
if (var_sed_CaCO3_blue), data(:,12) = 100.0*data(:,12); end
% set maximum sediment thickness
data_maxD = max(data(:,2)) + 0.5;
% check for out-of range plotting parameters
if opt_refage,
    if (refage > max(data(:,3))),
        disp(['ERROR: Requested reference age is greater than the oldest sediments (', num2str(max(data(:,3))), ' ka).', ' Check that did you enter in units of yr rather than kyr.']);
        return;
    end
    if (refage < min(data(:,3))),
        disp(['WARNING: Requested reference age is less than the youngest sediments (', num2str(min(data(:,3))), ' ka).']);
        %%%return;
    end
end
%
% *** FIND STRATIGRAPHIC MARKERS **************************************** %
%
% first: normalize ash to detrital content (to avoid double peaks)
data(:,6) = data(:,6)./data(:,9);
data(:,6) = data(:,6)/max(data(:,6)); % NORMALIZE!
% find uppermost stratigraphic marker (ash maximum)
% NOTE: start at the first full stack layer
% NOTE: add 1% threshold for fiding ash peak
loc_ash = 0.0;
for m=3:data_maxm-1,
    if (data(m,6) >= 1.01*loc_ash),
        loc_ash = data(m,6);
    elseif (data(m,6) < 0.99*loc_ash),
        data_maxashn = m-1;
        break
    end
end
% find secondary stratigraphic marker (ash maximum) [if selected]
% NOTE: find ash minimum following 1st maximum before searching for 2nd max
if ((par_ashmaxn == 2) || (par_ashmaxn == 3)),
    for m=data_maxashn+1:data_maxm-1,
        if (data(m,6) <= 0.99*loc_ash),
            loc_ash = data(m,6);
        elseif (data(m,6) > 1.01*loc_ash),
            data_minashn = m-1;
            break
        end
    end
    for m=data_minashn:data_maxm-1,
        if (data(m,6) >= 1.01*loc_ash),
            loc_ash = data(m,6);
        elseif (data(m,6) < 0.99*loc_ash),
            data_smaxashn = m-1;
            break
        end
    end
    % substitute secondary stratigraphic marker
    data_maxashn = data_smaxashn;
end
% find tertiary stratigraphic marker (ash maximum)
% NOTE: find ash minimum following 2nd maximum before searching for 3rd max
if (par_ashmaxn == 3),
    for m=data_maxashn+1:data_maxm-1,
        if (data(m,6) <= 0.99*loc_ash),
            loc_ash = data(m,6);
        elseif (data(m,6) > 1.01*loc_ash),
            data_minashn = m-1;
            break
        end
    end
    for m=data_minashn:data_maxm-1,
        if (data(m,6) >= 1.01*loc_ash),
            loc_ash = data(m,6);
        elseif (data(m,6) < 0.99*loc_ash),
            data_tmaxashn = m-1;
            break
        end
    end
    % substitute tertiary stratigraphic marker
    data_maxashn = data_tmaxashn;
end
% offset ash maximum level to account for bioturbation
data_maxashn = data_maxashn + par_ashoffsetn;
% find color tracer restart marker (if tracer selected)
if (var_sed_CaCO3_red),
    loc_red = 0.0;
    for m=data_maxashn:data_maxm-1,
        if (data(m,11) >= loc_red),
            loc_red = data(m,11);
        elseif ((loc_red > 0) && (data(m,11) == 0)),
            data_redn = m - (par_ref_coln + 2);
            break
        end
    end
end
%
% *** CREATE ASH AGE SCALE ********************************************** %
%
% NOTE: calculate mean sedimentation rate (cm kyr-1) down to ash peak
if (opt_ashagemodel),
    loc_SR = data(data_maxashn,2)/par_expduration;
    data(:,3) = data(:,2)/loc_SR;
end
%
% *** CREATE CONSTANT DETRITAL FLUX AGE MODEL *************************** %
%
if (opt_ashagemodel && opt_detagemodel),
    % first sum the total detrital content of the sediment column
    % (above the strat marker)
    % NOTE: calculated as: (1 - porosity) * layer thickness * detrital wt%
    loc_dettot = sum((1.0 - data(1:data_maxashn,7)).*data(1:data_maxashn,8).*data(1:data_maxashn,9));
    % replace detrital age model [AGE AT LAYER MIDPOINT]
    % NOTE: the average age of a layer include the accumulation of det for only half the layer thicknesws
    %       (hence why 0.5 fraction of current + 0.5 of previous layers are being dealt with)
    m=1;
    data(m,3) = refage*(1.0 - data(m,7))*(0.5*data(m,8))*data(m,9)/loc_dettot;
    for m=2:data_maxm-1,
        data(m,3) = data(m-1,3) + refage*(1.0 - data(m-1,7))*(0.5*data(m-1,8))*data(m-1,9)/loc_dettot + refage*(1.0 - data(m,7))*(0.5*data(m,8))*data(m,9)/loc_dettot;
    end
end
%
% *** CALCULATE SED RATE ************************************************ %
%
% calculate sedimentation rate
% #2 == mean depth
% #3 == SR in CaCO3 age scale
% #4 == mean age (CaCO3 age scale)
% NOTE: pad with dummy data_maxm values
for m=1:data_maxm-1,
    data_SR(m,2) = (data(m+1,2) + data(m,2))/2.0;
    data_SR(m,3) = (data(m+1,2) - data(m,2))/((data(m+1,3) - data(m,3)));
    data_SR(m,4) = (data(m+1,3) + data(m,3))/2.0;
end
data_SR(data_maxm,:) = 1.001*data_SR(data_maxm-1,:);
% filter sed rate for Inf
data_SR(find(isinf(data_SR(:,2))),2)=NaN;
data_SR(find(isinf(data_SR(:,3))),3)=NaN;
% remove CaCO3-tied tracers at low wt% CaCO3 data for plotting
data(find(data(:,4) < axis_Cthresh),5) = NaN;
if ~isempty(data1), data(find(data(:,4) < axis_Cthresh),13) = NaN; end
if ~isempty(data2), data(find(data(:,4) < axis_Cthresh),14) = NaN; end
if (var_sed_CaCO3_red), data(find(data(:,4) < axis_Cthresh),11) = NaN; end
if (var_sed_CaCO3_blue), data(find(data(:,4) < axis_Cthresh),12) = NaN; end
if ~isempty(data1), data(find(data(:,4) < axis_Cthresh),13) = NaN; end
if ~isempty(data2), data(find(data(:,4) < axis_Cthresh),14) = NaN; end
%
% *** SET REFERENCE LEVEL AND AGE *************************************** %
%
% set reference level number and depth in sedcore + corresponding age
if opt_refage,
    % set CaCO3 (and detrital) reference level
    % NOTE: do this *if* a reference age is set (non zero value of PREFAGE)
    if (refage > min(data(:,3))),
        loc_ref_min = min(abs(data(:,3) - refage));
        refage_CaCO3_n = int32(data(find(abs(data(:,3) - refage) == loc_ref_min),1));
        loc_refage_dA = data(refage_CaCO3_n,3) - refage;
        if (loc_refage_dA > 0.0),
            refage_D = data(refage_CaCO3_n,2) - loc_ref_min*data_SR(refage_CaCO3_n,3);
        else
            refage_D = data(refage_CaCO3_n,2) + loc_ref_min*data_SR(refage_CaCO3_n,3);
        end
        refage_n = refage_CaCO3_n;
    else
        refage_D = 0.0;
        refage_n = 1;
    end
else
    if (opt_relreflevel),
        % set ash stratigraphic marker & assign ash max age == exp run length
        refage_n = data_maxashn;
        refage_D = data(refage_n,2);
        refage = par_expduration;
    else
        refage_n = 1;
        refage_D = data(refage_n,2);
        refage = data(refage_n,3);
    end
end
%
% *** TRANSFORM DEPTH & AGE SCALES ************************************** %
%
% set age and depth reference levels
data_D0 = refage_D;
data_A0 = refage;
data_n0 = refage_n;
% truncate depth limits
% NOTE: do this *before* a relative reference has been set
if (axis_Dmax > data(data_n0,2)), axis_Dmax=data(data_n0,2); end
if (abs(axis_Dmin) > (max(data(:,2))-data(data_n0,2))), axis_Dmin=(data(data_n0,2)-max(data(:,2))); end
% shift & invert depth and age scales
if (opt_relreflevel),
    data(:,2) = data_D0 - data(:,2);
    data(:,3) = data_A0 - data(:,3);
    data_SR(:,2) = data_D0 - data_SR(:,2);
    data_SR(:,4) = data_A0 - data_SR(:,4);
    axis_D0 = 0.0;
    axis_A0 = 0.0;
else
    data(:,2) = -data(:,2);
    data(:,3) = -data(:,3);
    data_SR(:,2) = -data_SR(:,2);
    data_SR(:,4) = -data_SR(:,4);
    axis_D0 = data_D0;
    axis_A0 = data_A0;
end
%
% *** SET DEPTH & AGE AXIS LIMITS *************************************** %
%
% set the mix/max layer number limits for plotting
% NOTE: axis_Dmax is the depth *above* the reference horizon
%       axis_Dmin is the depth *below* the reference horizon
%       (hence why axis_Dmax is used to determine axis_n_min)
if (opt_refage),
    loc_refage_min = min(abs(data(:,3) - axis_Amax));
    axis_n_min = int32(data(find(abs(data(:,3) - axis_Amax) == loc_refage_min),1));
    if (length(axis_n_min) > 1), axis_n_min = axis_n_min(1); end
    loc_refage_max = min(abs(data(:,3) - axis_Amin));
    axis_n_max = int32(data(find(abs(data(:,3) - axis_Amin) == loc_refage_max),1));
    if (length(axis_n_max) > 1), axis_n_max = axis_n_max(end); end
else
    axis_n_min = max(1,data_n0-abs(int32(axis_Dmax)));
    axis_n_max = min(data_maxm,data_n0+abs(int32(axis_Dmin)));
end
% calculate mean sedimentation rate across plotting section
% NOTE: set default sed rate if no sed rate data exists
% NOTE: filter out the uppermost 10 layers (bioturbated)
data_SR_meanA = par_SR_mean_default;
data_SR_meanB = par_SR_mean_default;
loc_data_SR = data_SR(:,3);
loc_data_SR(1:10) = NaN;
loc_data_SRA = loc_data_SR(axis_n_min:data_n0);
loc_data_SRB = loc_data_SR(data_n0:axis_n_max);
loc_data_SRA(find(isnan(loc_data_SRA)))=[];
loc_data_SRB(find(isnan(loc_data_SRB)))=[];
if ~isempty(loc_data_SRA), data_SR_meanA = mean(loc_data_SRA(:)); end
if ~isempty(loc_data_SRB), data_SR_meanB = mean(loc_data_SRB(:)); end
% truncate (minimum) age limit according to actual core limts
% NOTE: axis_Amax is +vs
%       axis_Amin is -ve in the case of a relative (to reference) scale
% NOTE: remember that n counts downwards, so axis_n_min is the youngest,
%       but has the oldest relative age (to reference)
% NOTE: depth is truncated earlier (hance dummy assignment here)
if (opt_refage),
    axis_Amin = max(min(data(:,3)),axis_Amin);
    axis_Dmax = max(data(axis_n_min,2),(axis_Amin/(abs(axis_Amax) + abs(axis_Amin)))*(abs(axis_Amax) + abs(axis_Amin))*data_SR_meanA);
    axis_Dmin = max(data(axis_n_max,2),(axis_Amin/(abs(axis_Amax) + abs(axis_Amin)))*(abs(axis_Amax) + abs(axis_Amin))*data_SR_meanB);
    if (refage_D == 0.0), axis_Dmax = 0.0; end
    axis_ymax = axis_Amax;
    axis_ymin = axis_Amin;
else
    axis_Dmax = axis_Dmax;
    axis_Dmin = axis_Dmin;
    axis_Amax = max(data(axis_n_min,3),(axis_Dmax/(abs(axis_Dmax) + abs(axis_Dmin)))*(abs(axis_Dmax) + abs(axis_Dmin))/data_SR_meanA);
    axis_Amin = min(data(axis_n_max,3),(axis_Dmin/(abs(axis_Dmax) + abs(axis_Dmin)))*(abs(axis_Dmax) + abs(axis_Dmin))/data_SR_meanB);
    axis_ymax = axis_Dmax;
    axis_ymin = axis_Dmin;
end
axis_ashmax = data(data_maxashn,6);
%
if (var_sed_CaCO3_red),
    axis_redmax = max(data(1:(data_redn+par_ref_coln+2),11));
end
%
% *** SUBSAMPLING OF DATA *********************************************** %
%
% sample every plot_nratio layers (i.e. make other layes NaNs)
% NOTE: sample relative to the reference layer (data_n0)
%       (hence preserving ash marker maximum in sampling)
data(find(mod(int32(data(:,1))-data_n0,plot_nratio)~=0),3:end)=NaN;
data_SR(find(mod(int32(data(:,1))-data_n0,plot_nratio)~=0),3:end)=NaN;
%
% *********************************************************************** %

% *********************************************************************** %
% *** PLOT DATA: vs. DEPTH or AGE *************************************** %
% *********************************************************************** %
%
% create figure
scrsz = get(0,'ScreenSize');
figure('Position',[0 0 plot_dscrsz*scrsz(3) plot_dscrsz*scrsz(4)])
clf;
% define plotting regions
fh(1) = axes('Position',[0 0 1 1],'Visible','off');
fh(2) = axes('Position',[0.00 0.00 1.00 0.10],'Visible','off');
if ((~isempty(data1) && ~isempty(data2)) || ~opt_fatplot),
    fh(3) = axes('Position',[0.10 0.15 0.08 0.70]);
    fh(4) = axes('Position',[0.20 0.15 0.08 0.70]);
    fh(5) = axes('Position',[0.35 0.15 0.08 0.70]);
    fh(6) = axes('Position',[0.45 0.15 0.08 0.70]);
    fh(7) = axes('Position',[0.55 0.15 0.08 0.70]);
else
    fh(3) = axes('Position',[0.10 0.15 0.11 0.70]);
    fh(4) = axes('Position',[0.25 0.15 0.11 0.70]);
    fh(5) = axes('Position',[0.45 0.15 0.11 0.70]);
    fh(6) = axes('Position',[0.60 0.15 0.11 0.70]);
    fh(7) = axes('Position',[0.75 0.15 0.11 0.70]);
end
if ~isempty(data1),
    fh(8) = axes('Position',[0.70 0.15 0.08 0.70]);
else
    fh(8) = axes('Position',[0.70 0.15 0.08 0.70],'Visible','off');
end
if ~isempty(data2),
    fh(9) = axes('Position',[0.80 0.15 0.08 0.70]);
else
    fh(9) = axes('Position',[0.80 0.15 0.08 0.70],'Visible','off');
end
fh(10) = axes('Position',[0.90 0.00 0.08 1.00],'Visible','off');
fh(11) = axes('Position',[0.10 0.90 0.90 0.10],'Visible','off');
% date-stamp plot
set(gcf,'CurrentAxes',fh(1));
text(0.95,0.50,[str_function, ' : ', str_filename, ' : ', str_date],'FontName','Arial','FontSize',10,'Rotation',90.0,'HorizontalAlignment','center','VerticalAlignment','top');
% add title
set(gcf,'CurrentAxes',fh(11));
text(0.00,0.50,['Simulated downcore sediment profiles @ location ' coreid],'FontName','Arial','FontSize',15);
% set alternative y-axis choice
% NOTE: different indexing for SR array
if (opt_refage),
    loc_axis_y0 = axis_A0;
    if (opt_ashagemodel && ~opt_detagemodel),
        loc_ystr = 'Linear sed rate age relative to ref level (kyr)';
    elseif (opt_ashagemodel && opt_detagemodel),
        loc_ystr = 'Constant detrital flux age relative to ref level (kyr)';
    elseif (~opt_ashagemodel && opt_detagemodel),
        loc_ystr = 'Detrital age relative to ref level (kyr)';
    else
        loc_ystr = 'CaCO3 age relative to ref level (kyr)';
    end
    loc_ydata = data(:,3);
    loc_ydataSR = data_SR(:,4);
    % define limts of no carbonate age
    loc_ymin = min(loc_ydata(axis_n_min:axis_n_max));
    if (loc_ymin < axis_ymin), loc_ymin = axis_ymin; end
    loc_ymax = max(loc_ydata(axis_n_min:axis_n_max));
    if (loc_ymax < axis_ymax), loc_ymax = axis_ymax; end
else
    loc_axis_y0 = axis_D0;
    loc_ystr = 'Height relative to reference level (cm)';
    loc_ydata = data(:,2);
    loc_ydataSR = data_SR(:,2);
end
%
% *** CREATE BULK SEDIMENT PROPERTIES PLOT ****************************** %
%
set(gcf,'CurrentAxes',fh(3));
hold on;
% set axes and labels
if (axis_Cmin == axis_Cmax),
    axis_Cmin = min(data(axis_n_min:axis_n_max,4));
    axis_Cmax = max(data(axis_n_min:axis_n_max,4));
end
if (axis_Cmin == axis_Cmax), disp(['ERROR: Failed to autoscale carbonate content ... ']); return; end
if ((axis_Cmin == axis_Cmax) && (axis_Cmin == 0.0)),
    disp(['WARNING: No carbonate in requested interval ... there is not much to plot ...']);
    return;
end
axislabel_Cmin = str2num(num2str(axis_Cmin,plot_axislabel_prec));
axislabel_Cmax = str2num(num2str(axis_Cmax,plot_axislabel_prec));
axis([axis_Cmin axis_Cmax axis_ymin axis_ymax]);
% create fill pattern for background
% NOTE: to not plot NaN colors ... :)
% NOTE: reduce sed rate to ensure that the color blocks overlap
% NOTE: in SR based width scaling -- check for values of zero or less ...
% NOTE: color non-carbonate areas first
if (opt_refage),
    h = fill([axis_Cmin axis_Cmin axis_Cmax axis_Cmax],[loc_ymax axis_ymax axis_ymax loc_ymax],cmap(1,:),'LineStyle','none');
    h = fill([axis_Cmin axis_Cmin axis_Cmax axis_Cmax],[axis_ymin loc_ymin loc_ymin axis_ymin],cmap(1,:),'LineStyle','none');
end
for m=axis_n_min:axis_n_max,
    if (opt_refage),
        if (data_SR(m,3) > 0.0 && (~isnan(data(m,4)))), h = fill([axis_Cmin axis_Cmin axis_Cmax axis_Cmax],[(loc_ydata(m)-0.5*double(plot_nratio)/(plot_rSR*data_SR(m,3))) (loc_ydata(m)+0.5*double(plot_nratio)/(plot_rSR*data_SR(m,3))) (loc_ydata(m)+0.5*double(plot_nratio)/(plot_rSR*data_SR(m,3))) (loc_ydata(m)-0.5*double(plot_nratio)/(plot_rSR*data_SR(m,3)))],cmap(int32(data(m,4))+1,:),'LineStyle','none'); end
    else
        if (~isnan(data(m,4))), h = fill([axis_Cmin axis_Cmin axis_Cmax axis_Cmax],[(loc_ydata(m)-0.5*double(plot_nratio)) (loc_ydata(m)+0.5*double(plot_nratio)) (loc_ydata(m)+0.5*double(plot_nratio)) (loc_ydata(m)-0.5*double(plot_nratio))],cmap(int32(data(m,4))+1,:),'LineStyle','none'); end
    end
end
% over-print axes hidden by color fill
h = line([axis_Cmin axis_Cmin],[axis_ymin axis_ymax],'Color','k','LineWidth',0.5);
h = line([axis_Cmin axis_Cmax],[axis_ymin axis_ymin],'Color','k','LineWidth',0.5);
% plot data
scatter(data(axis_n_min:axis_n_max,4),loc_ydata(axis_n_min:axis_n_max),'o','Filled','Sizedata',plot_datasize,'MarkerFaceColor','y','MarkerEdgeColor','k');
% add on reference horizon
h = line([axis_Cmin axis_Cmax],[loc_axis_y0 loc_axis_y0],'Color','k','LineWidth',0.5);
% create axes
set(gca,'TickDir','out');
set(gca,'XLabel',text('String','wt%','FontSize',12),'XTick',[axis_Cmin:(axis_Cmax-axis_Cmin)/2.0:axis_Cmax],'XTickLabel',[axislabel_Cmin:(axislabel_Cmax-axislabel_Cmin)/2.0:axislabel_Cmax]);
if (axis_dy == 0.0),
    set(gca,'YLabel',text('String',[loc_ystr],'FontSize',12.0));
else
    set(gca,'YLabel',text('String',[loc_ystr],'FontSize',12.0),'YTick',[axis_ymin:axis_dy:axis_ymax]);
end
title(['CaCO_{3}'],'FontSize',15);
% create additional axes
axesPosition = get(gca,'Position');
ha = axes('Position',axesPosition,'Color','none','XAxisLocation','top','TickDir','out','XTick',[],'YAxisLocation','right','YTick',[],'YTickLabel','');
%
% *** CREATE BULK SEDIMENT PROPERTIES PLOT -- isotopes ****************** %
%
set(gcf,'CurrentAxes',fh(4));
hold on;
% set axes and labels
if (axis_d13Cmin == axis_d13Cmax),
    axis_d13Cmin = min(data(axis_n_min:axis_n_max,5));
    axis_d13Cmax = max(data(axis_n_min:axis_n_max,5));
end
axislabel_d13Cmin = str2num(num2str(axis_d13Cmin,plot_axislabel_prec-1));
axislabel_d13Cmax = str2num(num2str(axis_d13Cmax,plot_axislabel_prec-1));
if (axis_d13Cmin == axis_d13Cmax), disp(['ERROR: Failed to autoscale d13C ... ']); return; end
axis([axis_d13Cmin axis_d13Cmax axis_ymin axis_ymax]);
% mask non-carbonate area
if (opt_refage),
    h = fill([axis_d13Cmin axis_d13Cmin axis_d13Cmax axis_d13Cmax],[loc_ymax axis_ymax axis_ymax loc_ymax],color_g,'LineStyle','none');
    h = fill([axis_d13Cmin axis_d13Cmin axis_d13Cmax axis_d13Cmax],[axis_ymin loc_ymin loc_ymin axis_ymin],color_g,'LineStyle','none');
    h = line([axis_d13Cmin axis_d13Cmin],[axis_ymin axis_ymax],'Color','k','LineWidth',0.5);
end
% plot data
scatter(data(axis_n_min:axis_n_max,5),loc_ydata(axis_n_min:axis_n_max),'o','Filled','Sizedata',plot_datasize,'MarkerFaceColor','y','MarkerEdgeColor','k');
% add reference horizon
h = line([axis_d13Cmin axis_d13Cmax],[loc_axis_y0 loc_axis_y0],'Color','k','LineWidth',0.5);
if (var_sed_CaCO3_red), h = line([axis_d13Cmin axis_d13Cmax],[(data_D0-refage_D) (data_D0-refage_D)],'Color','k','LineWidth',0.5,'LineStyle','--'); end
% create axes
set(gca,'TickDir','out');
if (axis_dy == 0.0),
    set(gca,'YTickLabel','','XTick',[],'XTickLabel','');
else
    set(gca,'YTick',[axis_ymin:axis_dy:axis_ymax],'YTickLabel','','XTick',[],'XTickLabel','');
end
set(gca,'XLabel',text('String',['\delta^{13}C'],'FontSize',15.0));
% create additional axes
axesPosition = get(gca,'Position');
ha = axes('Position',axesPosition,'Color','none','XAxisLocation','top','TickDir','out','XLim',[axis_d13Cmin axis_d13Cmax],'YAxisLocation','right','YTick',[],'YTickLabel','');
set(ha,'XLabel',text('String',[char(8240)],'FontSize',12.0),'XTick',[axis_d13Cmin:(axis_d13Cmax-axis_d13Cmin)/2.0:axis_d13Cmax],'XTickLabel',[axislabel_d13Cmin:(axislabel_d13Cmax-axislabel_d13Cmin)/2.0:axislabel_d13Cmax]);
%
% *** CREATE BULK SEDIMENT PROPERTIES PLOT -- time (/age) *************** %
%
set(gcf,'CurrentAxes',fh(5));
hold on;
% set x-axis according to y-axis age vs. depth scale
if (opt_refage),
    loc_axis_xmin = axis_Dmin;
    loc_axis_xmax = axis_Dmax;
else
    loc_axis_xmin = axis_Amin;
    loc_axis_xmax = axis_Amax;
end
% set axes and labels
if (axis_agemodelmin == axis_agemodelmax),
    axis_agemodelmin = loc_axis_xmin;
    axis_agemodelmax = loc_axis_xmax;
end
axislabel_agemodelmin = str2num(num2str(axis_agemodelmin,plot_axislabel_prec-1));
axislabel_agemodelmax = str2num(num2str(axis_agemodelmax,plot_axislabel_prec-1));
% axis_Amin = str2num(num2str(axis_Amin,plot_axislabel_prec));
% axis_Amax = str2num(num2str(axis_Amax,plot_axislabel_prec));
axis([axis_agemodelmin axis_agemodelmax axis_ymin axis_ymax]);
% mask non-carbonate area
if (opt_refage ),
    h = fill([axis_agemodelmin axis_agemodelmin axis_agemodelmax axis_agemodelmax],[loc_ymax axis_ymax axis_ymax loc_ymax],color_g,'LineStyle','none');
    h = fill([axis_agemodelmin axis_agemodelmin axis_agemodelmax axis_agemodelmax],[axis_ymin loc_ymin loc_ymin axis_ymin],color_g,'LineStyle','none');
    h = line([axis_agemodelmin axis_agemodelmin],[axis_agemodelmax axis_agemodelmax],'Color','k','LineWidth',0.5);
end
% plot data
if (opt_refage),
    scatter(data(axis_n_min:axis_n_max,2),loc_ydata(axis_n_min:axis_n_max),'o','Filled','Sizedata',plot_datasize,'MarkerFaceColor','b','MarkerEdgeColor','k');
else
    scatter(data(axis_n_min:axis_n_max,3),loc_ydata(axis_n_min:axis_n_max),'o','Filled','Sizedata',plot_datasize,'MarkerFaceColor','b','MarkerEdgeColor','k');
end
% add main reference horizon
h = line([axis_agemodelmin axis_agemodelmax],[loc_axis_y0 loc_axis_y0],'Color','k','LineWidth',0.5);
% add zero time (OR DEPTH) line
h = line([0.0 0.0],[axis_ymin axis_ymax],'Color','k','LineWidth',0.5,'LineStyle','--');
% create axes
set(gca,'TickDir','out');
if (opt_refage),
    loc_str = 'Height (cm)';
else
    if (opt_CaCO3agemodel),
        loc_str = 'kyr (CaCO3)';
    else
        loc_str = 'kyr (ash/det)';
    end
end
set(gca,'XLabel',text('String',[loc_str],'FontSize',12),'XTick',[axis_agemodelmin axis_agemodelmax],'XTickLabel',[axislabel_agemodelmin axislabel_agemodelmax]);
if (axis_dy ~= 0.0), set(gca,'YTick',[axis_ymin:axis_dy:axis_ymax],'YTick',[axis_ymin:axis_dy:axis_ymax]); end
title(['Age model'],'FontSize',12.0,'FontWeight','bold');
% create additional axes
axesPosition = get(gca,'Position');
ha = axes('Position',axesPosition,'Color','none','XAxisLocation','top','TickDir','out','XTick',[],'YAxisLocation','right','YTick',[],'YTickLabel','');
%
% *** CREATE BULK SEDIMENT PROPERTIES PLOT -- sedimentation rate ******** %
%
set(gcf,'CurrentAxes',fh(6));
hold on;
% set axes and labels
if (axis_SRmin == axis_SRmax),
    axis_SRmin = axis_SRmin;
    axis_SRmax = max(data_SR(axis_n_min:axis_n_max,3));
end
axislabel_SRmin = str2num(num2str(axis_SRmin,plot_axislabel_prec-1));
axislabel_SRmax = str2num(num2str(axis_SRmax,plot_axislabel_prec-1));
if (axis_SRmin == axis_SRmax), disp(['ERROR: Failed to autoscale sedimentation rate ... ']); return; end
axis([axis_SRmin axis_SRmax axis_ymin axis_ymax]);
% mask non-carbonate area
if (opt_refage),
    h = fill([axis_SRmin axis_SRmin axis_SRmax axis_SRmax],[loc_ymax axis_ymax axis_ymax loc_ymax],color_g,'LineStyle','none');
    h = fill([axis_SRmin axis_SRmin axis_SRmax axis_SRmax],[axis_ymin loc_ymin loc_ymin axis_ymin],color_g,'LineStyle','none');
    h = line([axis_SRmin axis_SRmin],[axis_SRmax axis_SRmax],'Color','k','LineWidth',0.5);
end
% plot data
if (opt_refage),
    h = barh(loc_ydataSR(axis_n_min:axis_n_max),data_SR(axis_n_min:axis_n_max,3),'histc');
    delete(findobj('marker','*'))
else
    h = barh(loc_ydataSR(axis_n_min:axis_n_max),data_SR(axis_n_min:axis_n_max,3),double(plot_nratio),'FaceColor','b');
end
% add reference horizon
h = line([axis_SRmin axis_SRmax],[loc_axis_y0 loc_axis_y0],'Color','k','LineWidth',0.5);
% create axes
set(gca,'TickDir','out');
if (axis_dy == 0.0),
    set(gca,'YTickLabel','','XTick',[],'XTickLabel','');
else
    set(gca,'YTick',[axis_ymin:axis_dy:axis_ymax],'YTickLabel','','XTick',[],'XTickLabel','');
end
set(gca,'XLabel',text('String','Sed rate','FontSize',15.0));
% create additional axes
axesPosition = get(gca,'Position');
ha = axes('Position',axesPosition,'Color','none','XAxisLocation','top','TickDir','out','XLim',[axis_SRmin axis_SRmax],'YAxisLocation','right','YTick',[],'YTickLabel','');
set(ha,'XLabel',text('String',['cm kyr^{-1}'],'FontSize',12.0),'XTick',[axis_SRmin:(axis_SRmax-axis_SRmin)/2.0:axis_SRmax],'XTickLabel',[axislabel_SRmin:(axislabel_SRmax-axislabel_SRmin)/2.0:axislabel_SRmax]);
%
% *** CREATE BULK SEDIMENT PROPERTIES PLOT -- ash *********************** %
%
set(gcf,'CurrentAxes',fh(7));
hold on;
% normalize data for plotting
if (max(data(axis_n_min:axis_n_max,6)) > 0.0),
    loc_data = data(axis_n_min:axis_n_max,6)/max(data(axis_n_min:axis_n_max,6));
else
    loc_data = data(axis_n_min:axis_n_max,6);
end
% set axes and labels
axis_ashmax = max(loc_data(:));
if (axis_ashmax == 0.0), axis_ashmax = 1.0; end
axislabel_ashmax = str2num(num2str(max(loc_data(:)),plot_axislabel_prec));
if (0.0 == axis_ashmax), disp(['ERROR: Failed to autoscale ash concentration ... ']); return; end
axis([0.0 axis_ashmax axis_ymin axis_ymax]);
% mask non-carbonate area
if (opt_refage),
    h = fill([0.0 0.0 axis_ashmax axis_ashmax],[loc_ymax axis_ymax axis_ymax loc_ymax],color_g,'LineStyle','none');
    h = fill([0.0 0.0 axis_ashmax axis_ashmax],[axis_ymin loc_ymin loc_ymin axis_ymin],color_g,'LineStyle','none');
    h = line([0.0 0.0],[axis_ymin axis_ymax],'Color','k','LineWidth',0.5);
end
% plot data
scatter(loc_data(:),loc_ydata(axis_n_min:axis_n_max),'o','Filled','Sizedata',plot_datasize,'MarkerFaceColor','g','MarkerEdgeColor','k');
% add main reference horizon
h = line([0.0 axis_ashmax],[axis_D0 axis_D0],'Color','k','LineWidth',0.5);
if (var_sed_CaCO3_red), h = plot([0.0 axis_ashmax],[(data_D0-refage_D) (data_D0-refage_D)],'k--','LineWidth',0.5); end
% create axes
set(gca,'TickDir','out');
if (axis_dy == 0.0),
    set(gca,'YTickLabel','');
else
    set(gca,'YTick',[axis_ymin:axis_dy:axis_ymax],'YTickLabel','');
end
set(gca,'XLabel',text('String',['[normalized]'],'FontSize',12),'XTick',[0.0:(axis_ashmax-0.0)/2.0:axis_ashmax],'XTickLabel',[0.0:(axislabel_ashmax-0.0)/2.0:axislabel_ashmax]);
title(['strat. marker'],'FontSize',12,'FontWeight','bold');
% create additional axes
axesPosition = get(gca,'Position');
ha = axes('Position',axesPosition,'Color','none','XAxisLocation','top','TickDir','out','XTick',[],'YAxisLocation','right','YTick',[],'YTickLabel','');
%
% *** CREATE TRACER #1 PLOT ********************************************* %
%
if ~isempty(data1),
    set(gcf,'CurrentAxes',fh(8));
    hold on;
    if (isempty(data1_ts)),
        % set axes and labels
        if (axis_data1min == axis_data1max),
            axis_data1min = min(data(axis_n_min:axis_n_max,13));
            axis_data1max = max(data(axis_n_min:axis_n_max,13));
        end
        axislabel_data1min = str2num(num2str(axis_data1min,plot_axislabel_prec));
        axislabel_data1max = str2num(num2str(axis_data1max,plot_axislabel_prec));
        axis([axis_data1min axis_data1max axis_ymin axis_ymax]);
        % plot data
        scatter(data(axis_n_min:axis_n_max,13),loc_ydata(axis_n_min:axis_n_max),'o','Filled','Sizedata',plot_datasize,'MarkerFaceColor','r','MarkerEdgeColor','k');
    elseif (opt_refage),
        if (axis_data1min == axis_data1max),
            axis_data1min = min(data1_ts(:,plot_data1_ts_n));
            axis_data1max = max(data1_ts(:,plot_data1_ts_n));
        end
        axislabel_data1min = str2num(num2str(axis_data1min,plot_axislabel_prec));
        axislabel_data1max = str2num(num2str(axis_data1max,plot_axislabel_prec));
        axis([axis_data1min axis_data1max axis_ymin axis_ymax]);
        % plot data
        hl1 = line(data1_ts(:,plot_data1_ts_n),(data1_ts(:,1)/1.0e3)-(par_expduration-refage),'Color','r','LineWidth',2.0);
    end
    % add on reference horizon
    h = line([axis_data1min axis_data1max],[axis_D0 axis_D0],'Color','k','LineWidth',0.5);
    if (var_sed_CaCO3_red), h = line([axis_data1min axis_data1max],[(data_D0-refage_D) (data_D0-refage_D)],'Color','k','LineWidth',0.5,LineStyle','--'); end
    % create axes
    set(gca,'TickDir','out');
    if (isempty(plot_data1_units)), plot_data1_units = ['[units]']; end
    set(gca,'XLabel',text('String',[plot_data1_units],'FontSize',12),'XTick',[axis_data1min:(axis_data1max-axis_data1min)/2.0:axis_data1max],'XTickLabel',[axislabel_data1min:(axislabel_data1max-axislabel_data1min)/2.0:axislabel_data1max]);
    if (axis_dy ~= 0.0), set(gca,'YTick',[axis_ymin:axis_dy:axis_ymax]); end
    if (isempty(plot_data1_title)),
        plot_data1_title = data1;
        plot_data1_title(find(plot_data1_title(:)=='_')) = '.';
        title([plot_data1_title],'FontSize',11,'FontWeight','bold');
    else
        title([plot_data1_title],'FontSize',15);
    end
    % create additional axes
    axesPosition = get(gca,'Position');
    ha = axes('Position',axesPosition,'Color','none','XAxisLocation','top','TickDir','out','XTick',[],'YAxisLocation','right','YTick',[],'YTickLabel','');
end
%
% *** CREATE TRACER #2 PLOT ********************************************* %
%
if ~isempty(data2),
    set(gcf,'CurrentAxes',fh(9));
    hold on;
    if (isempty(data2_ts)),
        % set axes and labels
        if (axis_data2min == axis_data2max),
            axis_data2min = min(data(axis_n_min:axis_n_max,14));
            axis_data2max = max(data(axis_n_min:axis_n_max,14));
        end
        axislabel_data2min = str2num(num2str(axis_data2min,plot_axislabel_prec));
        axislabel_data2max = str2num(num2str(axis_data2max,plot_axislabel_prec));
        axis([axis_data2min axis_data2max axis_ymin axis_ymax]);
        % plot data
        scatter(data(axis_n_min:axis_n_max,14),loc_ydata(axis_n_min:axis_n_max),'o','Filled','Sizedata',plot_datasize,'MarkerFaceColor','r','MarkerEdgeColor','k');
    elseif (opt_refage),
        if (axis_data2min == axis_data2max),
            axis_data2min = min(data2_ts(:,plot_data2_ts_n));
            axis_data2max = max(data2_ts(:,plot_data2_ts_n));
        end
        axislabel_data2min = str2num(num2str(axis_data2min,plot_axislabel_prec));
        axislabel_data2max = str2num(num2str(axis_data2max,plot_axislabel_prec));
        axis([axis_data2min axis_data2max axis_ymin axis_ymax]);
        % plot data
        hl1 = line(data2_ts(:,plot_data2_ts_n),(data2_ts(:,1)/1.0e3)-(par_expduration-refage),'Color','r','LineWidth',2.0);
    end
    % add on reference horizon
    h = line([axis_data2min axis_data2max],[axis_D0 axis_D0],'Color','k','LineWidth',0.5);
    if (var_sed_CaCO3_red), h = line([axis_data2min axis_data2max],[(data_D0-refage_D) (data_D0-refage_D)],'Color','k','LineWidth',0.5,LineStyle','--'); end
    if (isempty(plot_data2_units)), plot_data2_units = ['[units]']; end
    % create axes
    set(gca,'TickDir','out');
    if (axis_dy == 0.0),
        set(gca,'YTickLabel','','XTick',[],'XTickLabel','');
    else
        set(gca,'YTick',[axis_ymin:axis_dy:axis_ymax],'YTickLabel','','XTick',[],'XTickLabel','');
    end
    if (isempty(plot_data2_title)),
        plot_data2_title = data2;
        plot_data2_title(find(plot_data2_title(:)=='_')) = '.';
        set(gca,'XLabel',text('String',[plot_data2_title],'FontSize',11,'FontWeight','bold'));
    else
        set(gca,'XLabel',text('String',[plot_data2_title],'FontSize',15));
    end
    % create additional axes
    axesPosition = get(gca,'Position');
    ha = axes('Position',axesPosition,'Color','none','XAxisLocation','top','TickDir','out','XLim',[axis_data2min axis_data2max],'YAxisLocation','right','YTick',[],'YTickLabel','');
    set(ha,'XLabel',text('String',[plot_data2_units],'FontSize',12.0),'XTick',[axis_data2min:(axis_data2max-axis_data2min)/2.0:axis_data2max],'XTickLabel',[axislabel_data2min:(axislabel_data2max-axislabel_data2min)/2.0:axislabel_data2max]);
end
%
% *** PRINT PLOT ******************************************************** %
%
set(gcf,'CurrentAxes',fh(1));
set(gcf,'renderer','painters');
if (opt_refage),
    if (~isempty(data1) && ~isempty(data2)),
        str_filename = [str_filename '.' 'sedcore' coreid '_vsA'];
    else
        str_filename = [str_filename '.' 'sedcoreonly' coreid '_vsA'];
    end
    if (opt_ashagemodel && ~opt_detagemodel),
        str_filename = [str_filename, 'ash'];
    elseif (opt_ashagemodel && opt_detagemodel),
        str_filename = [str_filename, 'ashdet'];
    elseif (~opt_ashagemodel && opt_detagemodel),
        str_filename = [str_filename, 'det'];
    end
else
    if (~isempty(data1) && ~isempty(data2)),
        str_filename = [str_filename '.' 'sedcore' coreid '_vsD'];
    else
        str_filename = [str_filename '.' 'sedcoreonly' coreid '_vsD'];
    end
end
if (~isempty(altfilename)), str_filename = altfilename; end
str_filename = [str_filename, '.', str_date];
if (plot_format_old == 'y')
    print('-dpsc2', [str_filename, '.ps']);
else
    switch plot_format
        case 'png'
            export_fig([str_filename '.png'], '-png', '-r150', '-nocrop');
        case 'pngT'
            export_fig([str_filename '.png'], '-png', '-r150', '-nocrop', '-transparent');
        case 'jpg'
            export_fig([str_filename '.jpg'], '-jpg', '-r150', '-nocrop');
        otherwise
            export_fig([str_filename '.eps'], '-eps', '-nocrop');
    end
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** END *************************************************************** %
% *********************************************************************** %
%
% close netCDF file
netcdf.close(ncid);
%
% *********************************************************************** %
