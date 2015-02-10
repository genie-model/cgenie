function [data_ALL data_ij] = read_netcdf_biogem_2d(PEXP,PVAR,PT,PI,PJ,PK,POPT)
% read_netcdf_biogem_2d
%
%   ***********************************************************************
%   *** GENIE netCDF results processing ***********************************
%   ***********************************************************************
%
%   read_netcdf_biogem_2d(PEXP,PVAR,PT,PI,PJ,PK)
%   processes BIOGEM 2-D netCDF data file 'fields_biogem_2d.nc' 
%   returning the extracted field and (optionally) the value at a point
%   read_netcdf_biogem_2d takes 5 arguments:
%
%   PEXP [STRING] (e.g. 'preindustrial_spinup')
%   --> the experiment name
%   PVAR [STRING] (e.g. 'ocn_PO4')
%   --> id the name of 1st variable to be plotted
%   --> all valid valiable names will be listed if an invalid name is given
%   PT [REAL] (e.g. 1999.5)
%   --> the (first) time-slice year
%   --> all valid years will be listed if an invalid year is given
%   PI [INTEGER] (e.g. 1)
%   PJ [INTEGER] (e.g. 1)
%   PK [INTEGER] (e.g. 1)
%   POPT [STRING] (e.g., 'plotting_config_2')
%   --> the string for an alternative plotting parameter set
%   --> if an empty (i.e., '') value is passed to this parameter
%       then the default parameter set is used
%
%   Example
%           read_netcdf_biogem_2d('experiment','ocn_PO4',1765.5,0,0,0)
%           will read in the GENIE netcdf field ocn_PO4
%
%   *******************************************************************   %

%   *******************************************************************   %
%   *** HISTORY *******************************************************   %
%   *******************************************************************   %
%
%   13/10/15: created
%   14/01/06: fixed grids
%   14/11/28: split into 2d and 3d varients
%             much further development
%             added to svn
%   14/11/29: added i,j point output
%   14/11/20: corrected for 2d!
%
%   *******************************************************************   %

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
% set passed parameters
expid = PEXP;
dataid = PVAR;
timesliceid = PT;
data_n_i = PI;
data_n_j = PJ;
data_n_k = PK; % NOTE: not used -- for completeness (consistency with read_netcdf_biogem_3d)
% set date
str_date = [datestr(date,11), datestr(date,5), datestr(date,7)];
% set function name
str_function = 'read_netcdf_biogem_2d';
% plot format
if ~isempty(plot_format), plot_format_old='n'; end
% plotting paths
addpath(library_path1);
if (plot_format_old == 'n'),
    addpath(library_path2);
    addpath(library_path3);
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** OPEN netCDF FILE ************************************************** %
% *********************************************************************** %
%
% open netCDF file
ncid=netcdf.open([data_path '/' expid '/biogem/fields_biogem_2d.nc'],'nowrite');
% read netCDf information
[ndims,nvars,ngatts,unlimdimid] = netcdf.inq(ncid);
%
% *********************************************************************** %

% *********************************************************************** %
% *** SET UP GRID ******************************************************* %
% *********************************************************************** %
%
% load grid data
varid  = netcdf.inqVarID(ncid,'grid_level');
grid_k1 = netcdf.getVar(ncid,varid);
% flip array around diagonal to give (j,i) array orientation
grid_k1 = grid_k1';
% determine grid axes dimensions
varid  = netcdf.inqVarID(ncid,'lat');
[dimname, dimlen] = netcdf.inqDim(ncid,varid);
jmax = dimlen;
varid  = netcdf.inqVarID(ncid,'lon');
[dimname, dimlen] = netcdf.inqDim(ncid,varid);
imax = dimlen;
% load grid axes
% NOTE: invert z-axis
varid  = netcdf.inqVarID(ncid,'lat');
grid_lat = netcdf.getVar(ncid,varid);
varid  = netcdf.inqVarID(ncid,'lon');
grid_lon = netcdf.getVar(ncid,varid);
varid  = netcdf.inqVarID(ncid,'lat_edges');
grid_lat_edges = netcdf.getVar(ncid,varid);
varid  = netcdf.inqVarID(ncid,'lon_edges');
grid_lon_edges = netcdf.getVar(ncid,varid);
% create 2D arrays of grid boundaries
[lonw lats] = meshgrid(grid_lon_edges(1:imax),grid_lat_edges(1:jmax));
[lone latn] = meshgrid(grid_lon_edges(2:imax+1),grid_lat_edges(2:jmax+1));
%
grid_lon_origin = grid_lon_edges(1);
%
% *********************************************************************** %

% *********************************************************************** %
% *** LOAD DATASET ****************************************************** %
% *********************************************************************** %
%
% check that the year exists
varid  = netcdf.inqVarID(ncid,'time');
timeslices = netcdf.getVar(ncid,varid);
[dimname, dimlen] = netcdf.inqDim(ncid,varid);
clear time;
while exist('time','var') == 0
    for n = 1:dimlen,
        if double(int32(100*timeslices(n)))/100 == timesliceid
            time = timesliceid;
            tid = n;
        end
    end
    if exist('time','var') == 0
        disp('   > WARNING: Year #1 must be one of the following;');
        format long g;
        double(int32(100*timeslices(:)))/100
        format;
        timesliceid = input('   > Time-slice year: ');
    end
end
% check that the variable name exists
varid = [];
while isempty(varid)
    for n = 0:nvars-1,
        [varname,xtype,dimids,natts] = netcdf.inqVar(ncid,n);
        if strcmp(varname,dataid)
            varid = n;
        end
    end
    if isempty(varid)
        disp('   > WARNING: Variable #1 name must be one of the following;');
        for n = 0:nvars-1,
            [varname,xtype,dimids,natts] = netcdf.inqVar(ncid,n);
            varname
        end
        dataid = input('   > Variable name: ','s');
    end
end
% load data
% NOTE: flip array around diagonal to give (j,i) array orientation
data(:,:) = zeros(jmax,imax);
[varname,xtype,dimids,natts] = netcdf.inqVar(ncid,varid);
rawdata = netcdf.getVar(ncid,varid);
if length(dimids) == 3
    rawdata(1:imax,1:jmax) = rawdata(1:imax,1:jmax,tid);
    data(1:jmax,1:imax) = rawdata(1:imax,1:jmax)';
elseif length(dimids) == 2
    rawdata(1:imax,1:jmax) = rawdata(1:imax,1:jmax);
    data(1:jmax,1:imax) = rawdata(1:imax,1:jmax)';
else
    data = NaN*data;
end
%
zm = [];
zm(:,:) = data(:,:);
%
% *********************************************************************** %


% *********************************************************************** &
% *** PLOTTING ********************************************************** &
% *********************************************************************** &
%
% *** QUICK DIAGNOSTIC PLOT ********************************************* %
%
if (plot_secondary == 'y'),
    % plot
    plot_2dgridded(zm,0.9E19,'',[expid '.' dataid],strrep([dataid ' from: ' expid],'_',' '));
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** END and CLOSE netCDF FILE ***************************************** %
% *********************************************************************** %
%
% export data
data_ALL = zm;
if ((data_n_i > 0) && (data_n_i <= imax) && (data_n_j > 0) && (data_n_j <= jmax)),
    data_ij = zm(data_n_j,data_n_i);
else
    data_ij = NaN;
end
% close netCDF file
netcdf.close(ncid);
%
% *********************************************************************** %
