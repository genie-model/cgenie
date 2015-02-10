function [wstress, wspeed, g_wspd] = gold_winds(gcmid,pathid,expid,maskb,maskt,maskv,gmask,glon,glonm,glat,glatm,fname);
% GOLD_WINDS  Calculate field of wind stress/speed on user-grid
%
%   Useage :
%
%   >> [wstress, wspeed] = gold_winds(umid, glon, glonm, glat, glatm, fname);
%
%   Where :
%
%   wstress = x and y wind stress at u and v points
%   wspeed  = x and y wind speed at grid points
%   glon    = longitude edges / u-point position
%   glonm   = longitude midpoints / v-point position
%   glat    = latitude edges / v-point position
%   glatm   = latitude midpoints / u-point position
%   fname   = filename for fields (e.g. 'grid1')
%
%   wstress is organised :  level 1   tau_x at u point
%                           level 2   tau_x at v point
%                           level 3   tau_y at u point
%                           level 4   tau_y at v point
%
%   wspeed is organised :   level 1   x speed at grid point
%                           level 2   y speed at grid point
%
%   Although the regridded data are returned to the user
%   in the form above, they are also organised into a form
%   suitable for GOLDSTEIN and then saved to file using the
%   user-specified filename as a marker.
%
%   See also REGRID_2D, GOLD_GRID, GOLD_CLIM.

% *********************************************************************** %
% *** INITIALIZE ******************************************************** %
% *********************************************************************** %
%
% How big is the output grid?
imax = max(size(glon)) - 1;
jmax = max(size(glat)) - 1;
%
% *********************************************************************** %

% *********************************************************************** %
% *** SET UP GRID ******************************************************* %
% *********************************************************************** %
%
% *** OPEN netCDF DATA FILE ********************************************* %
%
switch gcmid
    case 'hadcm3l'
        ncid=netcdf.open([pathid '/' expid '/' expid 'a.pdclann' '.nc'],'nowrite');
    case 'foam'
        %%%%%%
    otherwise
        disp(['ERROR: Unknown GCM name.']);
        return;
end
% read netCDf information
[ndims,nvars,ngatts,unlimdimid] = netcdf.inq(ncid);
%
% *** load data ********************************************************* %
%
% NOTE: flip array around diagonal to give (lon,lat) array orientation
%       & ensure double
%
switch gcmid
    case 'hadcm3l'
        % load TAUX
        varid  = netcdf.inqVarID(ncid,'taux_mm_hyb');
        old_utau(:,:) = netcdf.getVar(ncid,varid);
        new_utau = double(old_utau);
        utau = maskv.*flipdim((new_utau'),1);
        % load TAUY
        varid  = netcdf.inqVarID(ncid,'tauy_mm_hyb');
        old_vtau(:,:) = netcdf.getVar(ncid,varid);
        new_vtau = double(old_vtau);
        vtau = maskv.*flipdim((new_vtau'),1);
        % load U
        varid  = netcdf.inqVarID(ncid,'u_mm_10m');
        old_uwspd(:,:) = netcdf.getVar(ncid,varid);
        new_uwspd = double(old_uwspd);
        uwspd = flipdim(new_uwspd',1);
        % load V
        varid  = netcdf.inqVarID(ncid,'v_mm_10m');
        old_vwspd(:,:) = netcdf.getVar(ncid,varid);
        new_vwspd = double(old_vwspd);
        vwspd = flipdim(new_vwspd',1);
        % load wind speed scalar
        varid  = netcdf.inqVarID(ncid,'wind_mm_10m');
        old_wspd(:,:) = netcdf.getVar(ncid,varid);
        new_wspd = double(old_wspd);
        wspd = maskv.*flipdim(new_wspd',1);
        % calculate alternative wind speed
        wspd2 = maskv.*((uwspd.^2 + vwspd.^2).^0.5);
    case 'foam'
        %%%%%%
    otherwise
        disp(['ERROR: Unknown GCM name.']);
        return;
end
%
% *** close netCDF file ************************************************* %
%
netcdf.close(ncid);
%
% *** define grid ******************************************************* %
%
switch gcmid
    case 'hadcm3l'
        lata = [-88.750:2.5:88.750];
        lona = [1.875:3.75:361.875];
        lats = [-88.750:2.5:88.750];
        lons = [1.875:3.75:361.875];
    case 'foam'
        %%%%%%
    otherwise
        disp(['ERROR: Unknown GCM name.']);
        return;
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** RE-GRID *********************************************************** %
% *********************************************************************** %
%
% Need to determine wind stress at u and v points of the Arakawa C
% grid ...
%
%   ----v----
%   |       |
%   |   r   u
%   |       |
%   ---------
%
% ... so need to specify clever limits for regrid_2d to use
% 
% NOTE : although there are 37 latitudinal interfaces (the first
% is the south pole, the last the north pole), there are only 36
% wind stress entries in the wind stress files.  I am assuming
% here that the first v point is centred at the top of the first
% row of grid cells (i.e. at 70S).  It's unlikely to make a big
% (well, noticeable) difference if this is wrong.
%
% Add arbitrarily small number to put an "upper boundary" on the
% area over which the wind stress data is averaged.
vlat = glatm; vlat(end+1) = glat(end);
% Create longitude array for u points
ulon = glonm; ulon(end+1) = ulon(end) + (ulon(end) - ulon(end-1));
%
% *** Put on a GOLDSTEIN grid ******************************************* %
% 
% -> wind stress
fprintf('- Regridding wind stress (tau x) to GOLDSTEIN u point\n');
g_taux_u = regrid_2d(utau(1:(length(lats)-1),1:(length(lons)-1)), lats, lons(1:end-1), glat, ulon(1:end-1), 0.01);
fprintf('- Regridding wind stress (tau x) to GOLDSTEIN v point\n');
g_taux_v = regrid_2d(utau(1:(length(lats)-1),1:(length(lons)-1)), lats, lons(1:end-1), vlat, glon(1:end-1), 0.01);
fprintf('- Regridding wind stress (tau y) to GOLDSTEIN u point\n');
g_tauy_u = regrid_2d(vtau(1:(length(lats)-1),1:(length(lons)-1)), lats, lons(1:end-1), glat, ulon(1:end-1), 0.01);
fprintf('- Regridding wind stress (tau y) to GOLDSTEIN v point\n');
g_tauy_v = regrid_2d(vtau(1:(length(lats)-1),1:(length(lons)-1)), lats, lons(1:end-1), vlat, glon(1:end-1), 0.01);
% -> wind velocity
fprintf('- Regridding wind speed (x) to GOLDSTEIN\n');
g_wspd_u = regrid_2d(uwspd(1:1:(length(lata)-1),1:(length(lona)-1)), lata, lona(1:end-1), glat, glon(1:end-1), 0.01);
fprintf('- Regridding wind speed (y) to GOLDSTEIN\n');
g_wspd_v = regrid_2d(vwspd(1:1:(length(lata)-1),1:(length(lona)-1)), lata, lona(1:end-1), glat, glon(1:end-1), 0.01);
% -> wind speed scalar
fprintf('- Regridding wind speed scalar to GOLDSTEIN\n');
g_wspd = regrid_2d(wspd(1:1:(length(lata)-1),1:(length(lona)-1)), lata, lona(1:end-1), glat, glon(1:end-1), 0.01);
g_wspd2 = regrid_2d(wspd2(1:1:(length(lata)-1),1:(length(lona)-1)), lata, lona(1:end-1), glat, glon(1:end-1), 0.01);
%
% *** Copy to output arrays ********************************************* %
%
% -> wind stress
wstress(:,:,1) = g_taux_u;
wstress(:,:,2) = g_taux_v;
wstress(:,:,3) = g_tauy_u;
wstress(:,:,4) = g_tauy_v;
% -> wind velocity
wspeed(:,:,1) = g_wspd_u;
wspeed(:,:,2) = g_wspd_v;
% -> wind speed
g_wspd = gmask.*g_wspd;
g_wspd2 = gmask.*g_wspd2;
% replace NaNs with zero
wstress(find(isnan(wstress))) = 0.0;
wspeed(find(isnan(wspeed))) = 0.0;
g_wspd(find(isnan(g_wspd))) = 0.0;
g_wspd2(find(isnan(g_wspd2))) = 0.0;
%
% *********************************************************************** %

% *********************************************************************** %
% *** SAVE DATA ********************************************************* %
% *********************************************************************** %
%
% Remove spaces from end of fname
a = lenstr(fname);
fname = fname(1:a);
%
% Save regridded data to file
% Taux at u point
outname = sprintf('%s_taux_u.interp', fname);
c = wstress(:,:,1); b = permute(c, [2 1]); a = reshape(b, [imax*jmax 1]);
save (outname, 'a', '-ascii');
fprintf('- Written tau x (u point) data to %s\n',outname);
% Taux at v point
outname = sprintf('%s_taux_v.interp', fname);
c = wstress(:,:,2); b = permute(c, [2 1]); a = reshape(b, [imax*jmax 1]);
save (outname, 'a', '-ascii');
fprintf('- Written tau x (v point) data to %s\n',outname);
% Tauy at u point
outname = sprintf('%s_tauy_u.interp', fname);
c = wstress(:,:,3); b = permute(c, [2 1]); a = reshape(b, [imax*jmax 1]);
save (outname, 'a', '-ascii');
fprintf('- Written tau y (u point) data to %s\n',outname);
% Tauy at v point
outname = sprintf('%s_tauy_v.interp', fname);
c = wstress(:,:,4); b = permute(c, [2 1]); a = reshape(b, [imax*jmax 1]);
save (outname, 'a', '-ascii');
fprintf('- Written tau y (v point) data to %s\n',outname);
% U wind speed 
outname = sprintf('%s_uncep.silo', fname);
c = wspeed(:,:,1); b = permute(c, [2 1]); a = reshape(b, [imax*jmax 1]);
save (outname, 'a', '-ascii');
fprintf('- Written u wind speed data to %s\n',outname);
% V wind speed
outname = sprintf('%s_vncep.silo', fname);
c = wspeed(:,:,2); b = permute(c, [2 1]); a = reshape(b, [imax*jmax 1]);
save (outname, 'a', '-ascii');
fprintf('- Written v wind speed data to %s\n',outname);
% Save 2-D ASCII wind speed scalar for BIOGEM
savevar = flipdim(g_wspd(1:imax,1:jmax),1);
save([fname '_windspeed.dat'],'savevar','-ascii');
fprintf('- BIOGEM windspeed file saved\n');
% Save 2-D ASCII wind speed scalar for BIOGEM
savevar = flipdim(g_wspd2(1:imax,1:jmax),1);
save([fname '_windspeed2.dat'],'savevar','-ascii');
fprintf('- BIOGEM windspeed file saved\n');
%
% save mean global wind data
fid = fopen([fname '_windspeed_stats.txt'], 'wt');
fprintf(fid, 'Windpseed stats \n');
fprintf(fid, '\n');
fprintf(fid, 'Mean wind-speed [directly derived] : %6.3f m s-1 \n', mean(g_wspd(find(~isnan(g_wspd)))));
fprintf(fid, 'Mean wind-speed [vector derived]   : %6.3f m s-1 \n', mean(g_wspd2(find(~isnan(g_wspd2)))));
fprintf(fid, '\n');
fprintf(fid, 'Mean square wind-speed [directly derived] : %6.3f m2 s-2 \n', mean(g_wspd(find(~isnan(g_wspd))).^2));
fprintf(fid, 'Mean square wind-speed [vector derived]   : %6.3f m2 s-2 \n', mean(g_wspd2(find(~isnan(g_wspd2))).^2));
fclose(fid);
%
% *********************************************************************** %
