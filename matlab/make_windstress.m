function [] = make_windstress(DUM_NI,DUM_NJ,DUM_NSWL)
% make_windstress
%
%   *********************************************************
%   *** CREATE ZONAL AVERAGE WINDSTRESS                   ***
%   *********************************************************
%
%   make_windstress(DUM_NI,DUM_NJ,DUM_NSWL)
%   creates a synthetic zonal average windstress and takes 3 arguments:
%
%   DUM_NI [INTEGER] (e.g. 36)
%   --> the i-dimension of the grid
%   DUM_NJ [INTEGER] (e.g. 36)
%   --> the j-dimension of the grid
%   DUM_NSWL [STRING] (e.g. 'nwsw')
%   --> the profile required in N and S hemispheres
%       valid options are:
%       'nwsw' == NH waterworld, SH waterworld
%       'nlsw' == NH land present, SH waterworld
%       'nwsl' == NH waterworld, SH land present
%       'nlsl' == NH land present, SH land present
%
%   *********************************************************
%
%   ***********************************************************************
%   *** HISTORY ***********************************************************
%   ***********************************************************************
%
%   14/09/25: CREATED
%   14/09/26: initial working version completed
%   14/10/07: extended for get tau on both u and v grid
%   14/10/28: added wind
%
%   ***********************************************************************

% *********************************************************************** %
% *** INITIALIZE PARAMETERS & VARIABLES ********************************* %
% *********************************************************************** %
%
% START
disp(['START [make_windstress] >>>'])
close all;
% process dummy parameters
dum_n_i = DUM_NI;
dum_n_j = DUM_NJ;
dum_nswl = DUM_NSWL;
% set GOLDSTEIn parameters :: drag coefficent
go_cd = 0.0013;
% set GOLDSTEIn parameters :: air density
go_rhoair = 1.25;
% set zonal profile parameters -- water world
par_ws_amp(1) = 0.105;
par_ws_power(1) = 1.5;
par_ws_offset(1) = 0.0;
par_ws_cycles(1) = 1.0;
% set zonal profile parameters -- with land
par_ws_amp(2) = -0.105;
par_ws_power(2) = 1.0;
par_ws_offset(2) = 0.0;
par_ws_cycles(2) = 3.0;
% generate grid point mid-point latitudes
[loc_lon, loc_lat] = get_grid_genie(dum_n_i,dum_n_j,0);
% generate grid point edge latitudes
[loc_lone, loc_late] = get_grid_genie_lonlatedges(dum_n_i,dum_n_j,0);
% create zonal output arrays
taux_u = zeros(dum_n_j,dum_n_i);
taux_u_1d = zeros(dum_n_j,1);
taux_v = zeros(dum_n_j,dum_n_i);
taux_v_1d = zeros(dum_n_j,1);
%
% *********************************************************************** %

% *********************************************************************** %
% *** CREATE ZONAL WIND-STRESS PROFILE ********************************** %
% *********************************************************************** %
%
% NOTE: TAKE CARE WITH C-GRID!!!
%
%   ----v----
%   |       |
%   |   r   u
%   |       |
%   ---------
%
% Southern hemisphere
for j=1:dum_n_j/2,
    switch dum_nswl,
        case {'nwsw', 'nlsw'}
            % ($L$5*3.1416*(90-ABS(C10))/90)^$L$3
            loc_tmp1 = (par_ws_cycles(1)*pi*(90-abs(loc_lat(j)))/90.0)^par_ws_power(1);
            loc_tmp1e = (par_ws_cycles(1)*pi*(90-abs(loc_late(j+1)))/90.0)^par_ws_power(1);
            % (3.1416*(90-ABS(C10))/90)
            loc_tmp2 = (pi*(90-abs(loc_lat(j)))/90.0);
            loc_tmp2e = (pi*(90-abs(loc_late(j+1)))/90.0);
            % $L$2*SIN($F10)*SIN($E10)+$L$4
            taux_u(j,:) = par_ws_amp(1)*sin(loc_tmp1)*sin(loc_tmp2)+par_ws_offset(1);
            taux_v(j,:) = par_ws_amp(1)*sin(loc_tmp1e)*sin(loc_tmp2e)+par_ws_offset(1);
        case {'nwsl', 'nlsl'}
            % ($L$5*3.1416*(90-ABS(C10))/90)^$L$3
            loc_tmp1 = (par_ws_cycles(2)*pi*(90-abs(loc_lat(j)))/90.0)^par_ws_power(2);
            loc_tmp1e = (par_ws_cycles(2)*pi*(90-abs(loc_late(j+1)))/90.0)^par_ws_power(2);
            % (3.1416*(90-ABS(C10))/90)
            loc_tmp2 = (pi*(90-abs(loc_lat(j)))/90.0);
            loc_tmp2e = (pi*(90-abs(loc_late(j+1)))/90.0);
            % $L$2*SIN($F10)*SIN($E10)+$L$4
            taux_u(j,:) = par_ws_amp(2)*sin(loc_tmp1)*sin(loc_tmp2)+par_ws_offset(2);
            taux_v(j,:) = par_ws_amp(2)*sin(loc_tmp1e)*sin(loc_tmp2e)+par_ws_offset(2);
        otherwise
            disp(['ERROR: <' dum_nswl '> is not a valid option for N/S ocean/land setting.']);
            return;
    end
end
% Northern hemisphere
for j=(dum_n_j/2+1):dum_n_j,
    switch dum_nswl,
        case {'nwsw', 'nwsl'}
            % ($L$5*3.1416*(90-ABS(C10))/90)^$L$3
            loc_tmp1 = (par_ws_cycles(1)*pi*(90-abs(loc_lat(j)))/90.0)^par_ws_power(1);
            loc_tmp1e = (par_ws_cycles(1)*pi*(90-abs(loc_late(j+1)))/90.0)^par_ws_power(1);
            % (3.1416*(90-ABS(C10))/90)
            loc_tmp2 = (pi*(90-abs(loc_lat(j)))/90.0);
            loc_tmp2e = (pi*(90-abs(loc_late(j+1)))/90.0);
            % $L$2*SIN($F10)*SIN($E10)+$L$4
            taux_u(j,:) = par_ws_amp(1)*sin(loc_tmp1)*sin(loc_tmp2)+par_ws_offset(1);
            taux_v(j,:) = par_ws_amp(1)*sin(loc_tmp1e)*sin(loc_tmp2e)+par_ws_offset(1);
        case {'nlsw', 'nlsl'}
            % ($L$5*3.1416*(90-ABS(C10))/90)^$L$3
            loc_tmp1 = (par_ws_cycles(2)*pi*(90-abs(loc_lat(j)))/90.0)^par_ws_power(2);
            loc_tmp1e = (par_ws_cycles(2)*pi*(90-abs(loc_late(j+1)))/90.0)^par_ws_power(2);
            % (3.1416*(90-ABS(C10))/90)
            loc_tmp2 = (pi*(90-abs(loc_lat(j)))/90.0);
            loc_tmp2e = (pi*(90-abs(loc_late(j+1)))/90.0);
            % $L$2*SIN($F10)*SIN($E10)+$L$4
            taux_u(j,:) = par_ws_amp(2)*sin(loc_tmp1)*sin(loc_tmp2)+par_ws_offset(2);
            taux_v(j,:) = par_ws_amp(2)*sin(loc_tmp1e)*sin(loc_tmp2e)+par_ws_offset(2);
        otherwise
            disp(['ERROR: not a valid option for N/S ocean/land setting.']);
            return;
    end
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** CREATE WINDS ****************************************************** %
% *********************************************************************** %
%
% derive wind speeds on t-grid (i.e. the v grid point for x)
% NOTE: from BIOGEM code :: fun_calc_u(i,j) = sqrt((sqrt(tv**2 + tv2**2))/(goldstein_rhoair*goldstein_cd))
wind_u = sign(taux_u(:,:)).*((taux_u(:,:).^2).^0.5/(go_rhoair*go_cd)).^0.5;
%
% *********************************************************************** %

% *********************************************************************** %
% *** OUTPUT RESULTS **************************************************** %
% *********************************************************************** %
%
str_filename = ['ws_', num2str(dum_n_i), num2str(dum_n_j), '_', dum_nswl, '_taux_u'];
str_filenamee = ['ws_', num2str(dum_n_i), num2str(dum_n_j), '_', dum_nswl, '_taux_v'];
str_filenamee_wind = ['ws_', num2str(dum_n_i), num2str(dum_n_j), '_', dum_nswl, '_u'];
%
% *** PLOT PROFILES ***************************************************** %
%
% calculate zonal mean (should be equal to any particular longitude!)
for j=1:dum_n_j,
    taux_u_1d(j) = mean(taux_u(j,:));
    taux_v_1d(j) = mean(taux_v(j,:));
end
% plot figure -- u grid
figure;
plot(loc_lat(:),taux_u_1d(:));
axis([-90 90 -0.1 0.2]);
hold on;
scatter(loc_lat(:),taux_u_1d(:));
% save figure
print('-dpsc2', [str_filename, '.ps']);
% plot figure -- v grid
figure;
plot(loc_late(2:end),taux_v_1d(:));
axis([-90 90 -0.1 0.2]);
hold on;
scatter(loc_late(2:end),taux_v_1d(:));
% save figure
print('-dpsc2', [str_filenamee, '.ps']);
%
% *** SAVE FILES ******************************************************** %
%
% open file
fid = fopen([str_filename, '.interp'],'w');
% write data
for j=1:dum_n_j,
    for i=1:dum_n_i,
        fprintf(fid,'  %d',taux_u(j,i));
        fprintf(fid,'\n');
    end
end
% close file
fclose(fid);
%
% open file
fid = fopen([str_filenamee, '.interp'],'w');
% write data
for j=1:dum_n_j,
    for i=1:dum_n_i,
        fprintf(fid,'  %d',taux_v(j,i));
        fprintf(fid,'\n');
    end
end
% close file
fclose(fid);
%
% open file
fid = fopen([str_filenamee_wind, '.silo'],'w');
% write data
for j=1:dum_n_j,
    for i=1:dum_n_i,
        fprintf(fid,'  %d',wind_u(j,i));
        fprintf(fid,'\n');
    end
end
% close file
fclose(fid);
%
% *********************************************************************** %

% *********************************************************************** %
% *** END *************************************************************** %
% *********************************************************************** %
%
% END
disp(['<<< END [make_windstress]'])
%
% *********************************************************************** %
