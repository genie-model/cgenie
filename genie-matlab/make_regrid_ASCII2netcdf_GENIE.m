function [] = make_regrid_ASCII2netcdf_GENIE(PDATA,PNAME,PLNAME,PUNITS,PNLON,PNLAT)
% make_regrid_ASCII2netcdf_GENIE
%
%   ***********************************************************************
%   *** Transform gridded ASCII data to netCDF ****************************
%   ***********************************************************************
%
%   make_regrid_ASCII2netcdf_GENIE(PDATA,PNAME,PLNAME,PUNITS,PNLON,PNLAT)   
%   'make_regrid_ASCII2netcdf_GENIE.nc' and takes 6 arguments:
%
%   PDATA [STRING] (e.g. 'data_dust.dat')
%   --> the dataset name (including extension)
%   PNAME [STRING] (e.g., 'dust flux')
%   --> short data name
%   PLNAME [STRING] (e.g., 'regridded dust depositional flux')
%   --> long data name (description)
%   PUNITS [STRING] (e.g., 'kg m-2 yr-1')
%   --> data units
%   PNLON [INTEGER] (e.g. 36)
%   --> the number of increments in longitude
%   --> e.g. for standard GENIE grid
%   PNLAT [INTEGER] (e.g. 36)
%   --> the number of increments in latitude
%   --> e.g. for standard GENIE grid
%
%   Example
%           make_regrid_ASCII2netcdf_GENIE('data_dust.dat','dust flux','regridded dust depositional flux','kg m-2 yr-1',36,36);
%           will transform the 36x36 ASCII dust flux data file into netCDF
%           format
%
%   ***********************************************************************
%   *** HISTORY ***********************************************************
%   ***********************************************************************
%
%   14/08/13: CREATED -- adapted from make_regrid_data2netcdf.m
%   14/08/14: adjusted to be CF Version 1.6 compliant
%
%   ***********************************************************************

% *********************************************************************** %
% *** INITIALIZE PARAMETERS & VARIABLES ********************************* %
% *********************************************************************** %
%
% *** dummy variable processing ***************************************** %
%
% set passed parameters
str_data_filename = PDATA;
str_dataname = PNAME;
str_dataname_long = PLNAME;
str_data_units = PUNITS;
n_lon = PNLON;
n_lat = PNLAT;
% set data strings
if isempty(str_dataname), str_dataname = str_data_filename; end
if isempty(str_dataname_long), str_dataname_long = str_data_filename; end
if isempty(str_data_units), str_data_units = 'n/a'; end
%
% *** create grid ******************************************************* %
%
par_grid_lon_offset = -260.0;
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
    loc_axis_latedge(j)  = (180.0/pi)*asin((2*(j-1)/n_lat) - 1.0);
    loc_axis_latmid(j) = (180.0/pi)*asin(((1 + 2*(j-1))/n_lat) - 1.0);
end
loc_axis_latedge(n_lat+1)  = (180.0/pi)*asin((2*n_lat/n_lat) - 1.0);
loc_axis_latbnds(1:n_lat,1) = loc_axis_latedge(1:n_lat);
loc_axis_latbnds(1:n_lat,2) = loc_axis_latedge(2:n_lat+1);
loc_axis_latbnds = loc_axis_latbnds';
%
% *** misc initialization *********************************************** %
%
% set null/fill value
loc_nullvalue = -9.9E19;
% set date
str_date = [datestr(date,11), datestr(date,5), datestr(date,7)];
%
% *********************************************************************** %

% *********************************************************************** %
% *** READ IN & PROCESS DATA ******************************************** %
% *********************************************************************** %
%
% *** load data ********************************************************* %
%
if (exist(str_data_filename,'file') == 2),
    data = load(str_data_filename,'-ascii');
    data = rot90(data(:,:),3);
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** WRITE netCDF FILE ************************************************* %
% *********************************************************************** %
%
% create netCDF file
ncid = netcdf.create([str_data_filename, '.', str_date, '.nc'],'NC_WRITE');
NC_GLOBAL = netcdf.getConstant('NC_GLOBAL');
% define global attributes
netcdf.putAtt(ncid,NC_GLOBAL,'title','Gridded data');
netcdf.putAtt(ncid,NC_GLOBAL,'long_title','Regridded on a regular lon-lat, irregular in depth, grid');
netcdf.putAtt(ncid,NC_GLOBAL,'comments','conversion of GENIE ascii format 2D data by make_regrid_ASCII2netcdf_GENIE.m');
netcdf.putAtt(ncid,NC_GLOBAL,'history','version as of 14/08/14');
netcdf.putAtt(ncid,NC_GLOBAL,'Conventions','CF-1.6');
netcdf.putAtt(ncid,NC_GLOBAL,'CreationDate',datestr(now,'yyyy/mm/dd HH:MM:SS'));
netcdf.putAtt(ncid,NC_GLOBAL,'CreatedBy',[getenv('username'), '@', getenv('computername')]);
netcdf.putAtt(ncid,NC_GLOBAL,'MatlabSource','make_regrid_ASCII2netcdf_GENIE.m');
% define dimensions
dimid_lat = netcdf.defDim(ncid,'lat',n_lat);
dimid_lon = netcdf.defDim(ncid,'lon',n_lon);
di_bnds = netcdf.defDim(ncid,'nbounds',2);
di_time = netcdf.defDim(ncid,'time',1);
% define axes -- Y
varid = netcdf.defVar(ncid,'lat','double',dimid_lat);
netcdf.putAtt(ncid,varid,'standard_name','latitude');
netcdf.putAtt(ncid,varid,'long_name','latitude of grid centre');
netcdf.putAtt(ncid,varid,'units','degrees_north');
netcdf.putAtt(ncid,varid,'point_spacing','uneven');
netcdf.putAtt(ncid,varid,'Axis','Y');
netcdf.putAtt(ncid,varid,'_CoordinateAxisType','Lat');
netcdf.putAtt(ncid,varid,'bounds','lat_bnds');
varid_lat = varid;
% define axes -- Y bnds
varid = netcdf.defVar(ncid,'lat_bnds','double',[di_bnds, dimid_lat]);
varid_latbnds = varid;
% define axes -- X
varid = netcdf.defVar(ncid,'lon','double',dimid_lon);
netcdf.putAtt(ncid,varid,'standard_name','longitude');
netcdf.putAtt(ncid,varid,'long_name','longitude of grid centre');
netcdf.putAtt(ncid,varid,'units','degrees_east');
netcdf.putAtt(ncid,varid,'point_spacing','even');
netcdf.putAtt(ncid,varid,'Axis','X');
netcdf.putAtt(ncid,varid,'_CoordinateAxisType','Lon');
netcdf.putAtt(ncid,varid,'bounds','lon_bnds');
varid_lon = varid;
% define axes -- X bnds
varid = netcdf.defVar(ncid,'lon_bnds','double',[di_bnds, dimid_lon]);
varid_lonbnds = varid;
% define data variable -- data
varid = netcdf.defVar(ncid,str_dataname,'double',[dimid_lon, dimid_lat]);
netcdf.putAtt(ncid,varid,'name',str_dataname);
netcdf.putAtt(ncid,varid,'long_name',str_dataname_long);
netcdf.putAtt(ncid,varid,'missing_value',loc_nullvalue);
netcdf.putAtt(ncid,varid,'fillValue',loc_nullvalue);
netcdf.putAtt(ncid,varid,'units',str_data_units);
varid_data = varid;
% end definition
netcdf.endDef(ncid);
% write axes
netcdf.putVar(ncid,varid_lat,loc_axis_latmid);
netcdf.putVar(ncid,varid_lon,loc_axis_lonmid);
netcdf.putVar(ncid,varid_latbnds,loc_axis_latbnds);
netcdf.putVar(ncid,varid_lonbnds,loc_axis_lonbnds);
% write data
netcdf.putVar(ncid,varid_data,data);
% close netCDF file
netcdf.close(ncid);
%
% *********************************************************************** %

% *********************************************************************** %
% *** END *************************************************************** %
% *********************************************************************** %
%
% END
disp(['END ...'])
%
% *********************************************************************** %
