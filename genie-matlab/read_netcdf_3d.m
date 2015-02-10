function [data_ALL lat_e lon_e depth_e] = read_netcdf_3d(PFILE,PVAR,PGRID)
% read_netcdf_3d
%
%   *******************************************************************   %
%   *** netCDF results processing *************************************   %
%   *******************************************************************   %
%
%   read_netcdf_3d(PFILE,PVAR)
%   processes 3-D, assumed Levitus grid netCDF data file
%   read_netcdf_3d takes 3 arguments:
%
%   PFILE [STRING] (e.g. 'woa13_all_p00_01.nc')
%   --> the netCDF filename
%   PVAR [STRING] (e.g. 'p_an')
%   --> the name of variable
%   PGRID [LOGICAL] (e.g. 'false')
%   --> create a Levitus compatable grid instead of reading it in
%
%   Example
%           read_netcdf_3d('woa13_all_p00_01.nc','p_an','false')
%           will read in the PO4 field
%
%   *******************************************************************   %

% *********************************************************************** %
% *** HISTORY *********************************************************** %
% *********************************************************************** %
%
%   14/12/12: CREATED
%
% *********************************************************************** %

% *********************************************************************** %
% *** INITIALIZE PARAMETERS & VARIABLES ********************************* %
% *********************************************************************** %
%
% close open windows
% NOTE: don't clear variable space here ...
close all;
% set passed parameters
fileid = PFILE;
dataid = PVAR;
opt_makegrid = PGRID;
%
% *********************************************************************** %

% *********************************************************************** %
% *** LOAD DATASET ****************************************************** %
% ***********************************************************************
%
% *** OPEN netCDF FILE ************************************************** %
%
% open netCDF file
ncid=netcdf.open([fileid],'NC_NOWRITE');
% read netCDf information
[ndims,nvars,ngatts,unlimdimid] = netcdf.inq(ncid);
%
% *** LOAD GRID INFORMATION ********************************************* %
%
if (opt_makegrid),
    n_lon = 360;
    n_lat = 180;
    par_grid_lon_offset = -180.0;
    % lon (west boundary)
    for i=1:n_lon,
        loc_axis_lonedge(i)  = (i-1)*(360.0/n_lon) + par_grid_lon_offset;
        loc_axis_dlon(i) = (360.0/n_lon);
    end
    loc_axis_lonedge(n_lon+1)  = (n_lon)*(360.0/n_lon) + par_grid_lon_offset;
    loc_axis_lonmid  = loc_axis_lonedge(1:n_lon) + 0.5*loc_axis_dlon;
    loc_axis_lonbnds(1:n_lon,1) = loc_axis_lonedge(1:n_lon);
    loc_axis_lonbnds(1:n_lon,2) = loc_axis_lonedge(2:n_lon+1);
    loc_axis_lonbnds = loc_axis_lonbnds';
    % lat (south boundary)
    for j=1:n_lat,
        loc_axis_latedge(j)  = (j-1)*(180/n_lat) + -90.0;
        loc_axis_dlat(j) = (180.0/n_lat);
    end
    loc_axis_latedge(n_lat+1)  = (n_lat)*(180/n_lat) + -90.0;
    loc_axis_latmid  = loc_axis_latedge(1:n_lat) + 0.5*loc_axis_dlat;
    loc_axis_latbnds(1:n_lat,1) = loc_axis_latedge(1:n_lat);
    loc_axis_latbnds(1:n_lat,2) = loc_axis_latedge(2:n_lat+1);
    loc_axis_latbnds = loc_axis_latbnds';
    % depth (upper boundary)
    loc_axis_Dedge = [0,10,20,30,50,75,100,125,150,200,250,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1750,2000,2500,3000,3500,4000,4500,5000,5500,6000];
    loc_axis_Dmid = [5,15,25,40,62.5,87.5,112.5,137.5,175,225,275,350,450,550,650,750,850,950,1050,1150,1250,1350,1450,1625,1875,2250,2750,3250,3750,4250,4750,5250,5750];
    n_D = length(loc_axis_Dmid);
    max_D = loc_axis_Dedge(end);
    loc_axis_Dbnds(1:n_D,1) = loc_axis_Dedge(1:n_D);
    loc_axis_Dbnds(1:n_D,2) = loc_axis_Dedge(2:n_D+1);
    loc_axis_Dbnds = loc_axis_Dbnds';
else
    % load grid edges
    varid  = netcdf.inqVarID(ncid,'lat_bnds');
    loc_axis_latedge = netcdf.getVar(ncid,varid);
    varid  = netcdf.inqVarID(ncid,'lon_bnds');
    loc_axis_lonedge = netcdf.getVar(ncid,varid);
    varid  = netcdf.inqVarID(ncid,'depth_bnds');
    loc_axis_Dedge = netcdf.getVar(ncid,varid);
end
%
% *** LOAD VARIABLE ***************************************************** %
%
varid = netcdf.inqVarID(ncid,dataid);
rawdata = netcdf.getVar(ncid,varid);
%
% *** CLOSE netCDF FILE ************************************************* %
%
netcdf.close(ncid);
%
% *********************************************************************** %

% *********************************************************************** %
% *** END *************************************************************** %
% *********************************************************************** %
%
data_ALL = rawdata;
lat_e = loc_axis_latedge;
lon_e = loc_axis_lonedge;
depth_e = loc_axis_Dedge;
%
% *********************************************************************** %
