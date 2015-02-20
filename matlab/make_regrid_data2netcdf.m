function [] = make_regrid_data2netcdf(PDATA,PNAME,PLNAME,PUNITS,PNLON,PNLAT,PVERT)
% make_regrid_data2netcdf
%
%   ***********************************************************************
%   *** Regrid discrete data to netCDF format, regular grid ***************
%   ***********************************************************************
%
%   make_regrid_data2netcdf(PDATA,PNAME,PLNAME,PUNITS,PNLON,PNLAT,PVERT)
%   (equal lat increment grid)
%   'make_regrid_data2netcdf.nc' and takes 7 arguments:
%
%   PDATA [STRING] (e.g. 'data_d30Si.dat')
%   --> the dataset name (including extension)
%   PNAME [STRING] (e.g., 'd30Si')
%   --> short data name
%   PLNAME [STRING] (e.g., 'observed silicate isotope value in units of per mil')
%   --> long data name (description)
%   PUNITS [STRING] (e.g., '1')
%   --> data units
%   NOTE: for isotopes, the units name needs to be '1'
%   PNLON [INTEGER] (e.g. 360)
%   --> the number of increments in longitude
%   --> e.g. 360 for a 1 degree grid
%   PNLAT [INTEGER] (e.g. 180)
%   --> the number of increments in latitude
%   --> e.g. 180 for a 1 degree grid
%   PVERT [STRING] (e.g., 'WOA')
%   --> vertical grid option
%   --> default ('') is the WOA (Levitus) vertical grid
%       [NOTE: currently the only option is the WOA grid ...]
%
%   Example #1
%           make_regrid_data2netcdf('data_d30Si.dat','d30Si','observed water column silicon isotope value in units of per mil','1',360,180,'WOA');
%           will grid the d30Si data in file: 'data_d30Si.dat'
%           to a WOA-compatable grid (1 degree lon, lat)
%   Example #2
%           make_regrid_data2netcdf('dfe_data_mar2014.dat','DFe','Dissolved iron','nM',360,180,'');
%           will grid the dissolved Fe concentrations in the file: 'dfe_data_mar2014.dat' 
%           to a WOA-compatable grid (1 degree lon, lat)
%
%   ***********************************************************************
%   *** HISTORY ***********************************************************
%   ***********************************************************************
%
%   14/07/31: CREATED
%   14/08/01: further development ...
%   14/08/02: finally: initial basic working version
%   14/08/13: fixed error in grid defintion (North boundary of last cell)
%             added grid bnds
%   14/08/14: adjusted to be CF Version 1.6 compliant
%   14/09/29: added %s to end of data line
%             added additional lon-lat grid point finding filters
%             (for dealing with grid boundaries)
%
%   ***********************************************************************

% *********************************************************************** %
% *** INITIALIZE PARAMETERS & VARIABLES ********************************* %
% *********************************************************************** %
%
% *** START ************************************************************* %
% 
disp(['START [make_regrid_data2netcdf] >>>'])
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
str_depthgrid = PVERT;
% set data strings
if isempty(str_dataname), str_dataname = str_data_filename; end
if isempty(str_dataname_long), str_dataname_long = str_data_filename; end
if isempty(str_data_units), str_data_units = 'n/a'; end
%
% *** create grid ******************************************************* %
%
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
if ( isempty(str_depthgrid) || strcmp('WOA',str_depthgrid) || strcmp('woa',str_depthgrid) ),
    loc_axis_Dedge = [0,10,20,30,50,75,100,125,150,200,250,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1750,2000,2500,3000,3500,4000,4500,5000,5500,6000];
    loc_axis_Dmid = [5,15,25,40,62.5,87.5,112.5,137.5,175,225,275,350,450,550,650,750,850,950,1050,1150,1250,1350,1450,1625,1875,2250,2750,3250,3750,4250,4750,5250,5750];
    n_D = length(loc_axis_Dmid);
    max_D = loc_axis_Dedge(end);
else
    disp(['ERROR: ', str_depthgrid, ' is not a value depth grid choice.']);
    return;
end
loc_axis_Dbnds(1:n_D,1) = loc_axis_Dedge(1:n_D);
loc_axis_Dbnds(1:n_D,2) = loc_axis_Dedge(2:n_D+1);
loc_axis_Dbnds = loc_axis_Dbnds';
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
    fid = fopen(str_data_filename);
    C = textscan(fid, '%f %f %f %f %s', 'CommentStyle', '%');
    data_raw = cell2mat(C(1:4));
    fclose(fid);
    data_size = size(data_raw(:,:));
    nmax=data_size(1);
    if (length(find(isnan(data_raw))) > 0),
        disp(['WARNING: ', 'The data file: ', str_data_filename, ' does not contain numeric data in a consistent 4-column (lon,lat,depth,value), space-seperated format, or it contains NaNs. Recommend is to remove NaN containing lines.']);
    end
else
    disp(['ERROR: ', 'File: ', str_data_filename, ' does not exist anywhere in the MATLAB directory path.']);
    return;
end
%
% *** fliter data ******************************************************* %
%
% remove any data deeper that the bottom of the depth grid and update nmax
n = 1;
while (n <= nmax),
    if (data_raw(n,3) > max_D),
        data_raw(n,:) = [];
        nmax = nmax - 1;
    else
        n = n + 1;
    end
end
% filter lon,lat values
for n = 1:nmax,
    if (data_raw(n,1) < par_grid_lon_offset), data_raw(n,1) = data_raw(n,1) + 360.0; end
    if (data_raw(n,1) > (par_grid_lon_offset + 360.0)), data_raw(n,1) = data_raw(n,1) - 360.0; end
    if ( (data_raw(n,2) > 90.0) || (data_raw(n,2) < -90.0) ),
        disp(['ERROR: ', 'Something odd with the latitude values in file: ', str_data_filename, ' -- maybe latitude is not the 2nd data column as it should be?']);
        return;
    end
    if (data_raw(n,3) < 0.0),
        disp(['ERROR: ', 'Something odd with the depth values in file: ', str_data_filename, ' -- maybe depth is given as a negative height or depth is is not the 3rd data column as it should be?']);
        return;
    end
end
%
% *** re-grid data ****************************************************** %
%
% create empty data vectors
data_vector = zeros(nmax,4);
% create empty 3D data grids
data_gridded = zeros(n_lon,n_lat,n_D);
data_gridded = data_gridded + loc_nullvalue;
data_gridded_rawcount = zeros(n_lon,n_lat,n_D);
% create empty 2D data grid (for saving data density))
data_gridded_2Dcount = zeros(n_lon,n_lat);
% determine grid locations
% NOTE: subtract 1 from upper bound values of n for lon and lat
%       i.e. a point that lies in the cell with lon index n, fits criteria:
%       lat >= lat_edge(n)
%       lat < lat_edge(n+1)
% NOTE: deal with special cases of lon or lat grid boundary values
for n = 1:nmax,
    loc_lon_n = intersect(find(data_raw(n,1)>=loc_axis_lonedge(:)),find(data_raw(n,1)<loc_axis_lonedge(:))-1);
    if (abs(data_raw(n,1)) == abs(par_grid_lon_offset)), loc_lon_n = 1; end
    loc_lat_n = intersect(find(data_raw(n,2)>=loc_axis_latedge(:)),find(data_raw(n,2)<loc_axis_latedge(:))-1);
    if (data_raw(n,2) == -90), loc_lat_n = 1; end
    if (data_raw(n,2) == 90), loc_lat_n = n_lat; end
    loc_D_n   = intersect(find(data_raw(n,3)>=loc_axis_Dedge(:)),find(data_raw(n,3)<loc_axis_Dedge(:))-1);
    if (isempty(loc_lon_n*loc_lat_n*loc_D_n)),
        disp(['ERROR: ', 'Failed to regrid ... check lon,lat values and/or grid resolution choice. Error info:']);
        disp(['n = ',num2str(n),' : ',num2str(data_raw(n,4)),' @ ','(',num2str(data_raw(n,1)),',',num2str(data_raw(n,2)),',',num2str(data_raw(n,3)),')',' == ','(',num2str(loc_lon_n),',',num2str(loc_lat_n),',',num2str(loc_D_n),')']);
        return;
    end
    %disp(['n = ',num2str(n),' : ',num2str(data_raw(n,4)),' @ ','(',num2str(data_raw(n,1)),',',num2str(data_raw(n,2)),',',num2str(data_raw(n,3)),')',' == ','(',num2str(loc_lon_n),',',num2str(loc_lat_n),',',num2str(loc_D_n),')']);
    data_vector(n,:) = [loc_lon_n; loc_lat_n; loc_D_n; data_raw(n,4)]';
    data_gridded_rawcount(loc_lon_n,loc_lat_n,loc_D_n) = data_gridded_rawcount(loc_lon_n,loc_lat_n,loc_D_n) + 1;
end
% average data
% NOTE: remove duplicate locations (having added the value from there to the running average)
n = 1;
while (n <= nmax),
    loc_coordinate = data_vector(n,1:3);
    m = n + 1;
    n_dup = 0;
    while (m <= nmax),
        %disp(['n = ',num2str(n),' : ','m = ',num2str(m)])
        if (data_vector(m,1:3) == loc_coordinate),
            %disp(['Duplicate @ ','n = ',num2str(n),', ','m = ',num2str(m)])
            n_dup = n_dup + 1;
            data_vector(n,4) = (n_dup*data_vector(n,4) + data_vector(m,4))/(n_dup + 1);
            data_vector(m,:) = [];
            nmax = nmax - 1;
        else
            m = m + 1;
        end
    end
    n = n + 1;
end
% populate 3D grid
for n = 1:nmax,
    loc_lon_n = data_vector(n,1);
    loc_lat_n = data_vector(n,2);
    loc_D_n = data_vector(n,3);
    data_gridded(loc_lon_n,loc_lat_n,loc_D_n) = data_vector(n,4);
    data_gridded_2Dcount(loc_lon_n,loc_lat_n) = data_gridded_2Dcount(loc_lon_n,loc_lat_n) + 1;
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
netcdf.putAtt(ncid,NC_GLOBAL,'comments','conversion of ascii format data by make_regrid_data2netcdf.m');
netcdf.putAtt(ncid,NC_GLOBAL,'history','version as of 14/08/14');
netcdf.putAtt(ncid,NC_GLOBAL,'Conventions','CF-1.6');
netcdf.putAtt(ncid,NC_GLOBAL,'CreationDate',datestr(now,'yyyy/mm/dd HH:MM:SS'));
netcdf.putAtt(ncid,NC_GLOBAL,'CreatedBy',[getenv('username'), '@', getenv('computername')]);
netcdf.putAtt(ncid,NC_GLOBAL,'MatlabSource','make_regrid_data2netcdf.m');
% define dimensions
dimid_D = netcdf.defDim(ncid,'depth',n_D);
dimid_lat = netcdf.defDim(ncid,'lat',n_lat);
dimid_lon = netcdf.defDim(ncid,'lon',n_lon);
di_bnds = netcdf.defDim(ncid,'nbounds',2);
di_time = netcdf.defDim(ncid,'time',1);
% define axes -- Z
varid = netcdf.defVar(ncid,'depth','double',dimid_D);
netcdf.putAtt(ncid,varid,'standard_name','depth');
netcdf.putAtt(ncid,varid,'long_name','Vertical distance below the surface');
netcdf.putAtt(ncid,varid,'units','m');
netcdf.putAtt(ncid,varid,'point_spacing','uneven');
netcdf.putAtt(ncid,varid,'Axis','Z');
netcdf.putAtt(ncid,varid,'positive','down');
netcdf.putAtt(ncid,varid,'_CoordinateAxisType','Depth');
netcdf.putAtt(ncid,varid,'bounds','D_bnds');
varid_D = varid;
% define axes -- D bnds
varid = netcdf.defVar(ncid,'D_bnds','double',[di_bnds, dimid_D]);
varid_Dbnds = varid;
% define axes -- Y
varid = netcdf.defVar(ncid,'lat','double',dimid_lat);
netcdf.putAtt(ncid,varid,'standard_name','latitude');
netcdf.putAtt(ncid,varid,'long_name','latitude of grid centre');
netcdf.putAtt(ncid,varid,'units','degrees_north');
netcdf.putAtt(ncid,varid,'point_spacing','even');
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
% define data variable -- averaged data
varid = netcdf.defVar(ncid,str_dataname,'double',[dimid_lon, dimid_lat, dimid_D]);
netcdf.putAtt(ncid,varid,'name',str_dataname);
netcdf.putAtt(ncid,varid,'long_name',str_dataname_long);
netcdf.putAtt(ncid,varid,'missing_value',loc_nullvalue);
netcdf.putAtt(ncid,varid,'fillValue',loc_nullvalue);
netcdf.putAtt(ncid,varid,'units',str_data_units);
varid_data = varid;
% define data variable -- raw data density
varid = netcdf.defVar(ncid,'data_density','double',[dimid_lon, dimid_lat, dimid_D]);
netcdf.putAtt(ncid,varid,'name','data_density');
netcdf.putAtt(ncid,varid,'long_name','Density of data per grid cell prior to averaging');
netcdf.putAtt(ncid,varid,'missing_value',loc_nullvalue);
netcdf.putAtt(ncid,varid,'fillValue',loc_nullvalue);
netcdf.putAtt(ncid,varid,'units','m^-3');
varid_data_rawcount = varid;
% define data variable -- raw data density
varid = netcdf.defVar(ncid,'data_inventory','double',[dimid_lon, dimid_lat]);
netcdf.putAtt(ncid,varid,'name','data_inventory');
netcdf.putAtt(ncid,varid,'long_name','Inventory of processed data per grid point');
netcdf.putAtt(ncid,varid,'missing_value',loc_nullvalue);
netcdf.putAtt(ncid,varid,'fillValue',loc_nullvalue);
netcdf.putAtt(ncid,varid,'units','m^-2');
varid_data_2Dcount = varid;
% end definition
netcdf.endDef(ncid);
% write axes
netcdf.putVar(ncid,varid_D,loc_axis_Dmid);
netcdf.putVar(ncid,varid_lat,loc_axis_latmid);
netcdf.putVar(ncid,varid_lon,loc_axis_lonmid);
netcdf.putVar(ncid,varid_Dbnds,loc_axis_Dbnds);
netcdf.putVar(ncid,varid_latbnds,loc_axis_latbnds);
netcdf.putVar(ncid,varid_lonbnds,loc_axis_lonbnds);
% write data
netcdf.putVar(ncid,varid_data,data_gridded);
netcdf.putVar(ncid,varid_data_rawcount,data_gridded_rawcount);
netcdf.putVar(ncid,varid_data_2Dcount,data_gridded_2Dcount);
% close netCDF file
netcdf.close(ncid);
%
% *********************************************************************** %

% *********************************************************************** %
% *** END *************************************************************** %
% *********************************************************************** %
%
% END
disp(['<<< END [make_regrid_data2netcdf]']);
disp([' ']);
%
% *********************************************************************** %
