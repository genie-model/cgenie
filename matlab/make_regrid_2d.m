function [zo fao] = make_regrid_2d(PLONI,PLATI,PZI,PLONO,PLATO,PPLOT)
% make_regrid_2d
%
%   ***********************************************************************
%   *** re-grid between lon-lat grids *************************************
%   ***********************************************************************
%
%   make_regrid_2d(PLONI,PLATI,PZI,PLONO,PLATO)
%   regrids one (higher resolution) lon-lat grid to another (lower res)
%
%   PLONI [VECTOR,REAL]
%   --> vector of longitude edges (original grid) (W to E)
%   --> edges must include E and W walls
%   PLATI [VECTOR,REAL]
%   --> vector of latitude edges (original grid) (S to N)
%   --> edges must include N and S pole
%   PZI [ARRAY,REAL]
%   --> array of (z) values to be regridded (from original grid)
%   --> format: (lon,lat)
%   PLONO [VECTOR,REAL]
%   --> vector of longitude edges (out/output grid) (W to E)
%   PLATO [VECTOR,REAL]
%   --> vector of latitude edges (out/output grid) (S to N)
%   PPLOT [LOGICAL]
%   --> plot some output?
%
%   ***********************************************************************
%   *** HISTORY ***********************************************************
%   ***********************************************************************
%
%   14/01/28: CREATED
%   14/02/08: completed first draft
%   14/04/21: corrected output lon-lat ordering (in array definition)
%   14/12/12: RENAMED! make_regrid_lonlat -> make_regrid_2d
%   14/12/12: updated plotting output
%             set zero 'hit' output cells to NaN (rather than zero)
%             added fractional area function return
%
%   ***********************************************************************

% *********************************************************************** %
% *** INITIALIZE PARAMETERS & VARIABLES ********************************* %
% *********************************************************************** %
%
% process dummy parameters
lon_in = PLONI;
lat_in = PLATI;
lon_out = PLONO;
lat_out = PLATO;
gi_z = PZI;
opt_plot = PPLOT;
% report debuf information?
opt_debug=false;
%
% *** misc (local) parameters ******************************************* %
%
% constants
par_rEarth = 6371000.0;
par_yrtos = 365.25*24.0*3600.0;
% set date
str_date = [datestr(date,11), datestr(date,5), datestr(date,7)];
%
% *********************************************************************** %

% *********************************************************************** %
% *** RE-GRID *********************************************************** %
% *********************************************************************** %
%
% *** create grids and dimensions *************************************** %
%
n_lon_in  = length(lon_in) - 1;
n_lat_in  = length(lat_in) - 1;
n_lon_out = length(lon_out) - 1;
n_lat_out = length(lat_out) - 1;
% create output data grid
go_z  = zeros(n_lon_out,n_lat_out);
go_n  = zeros(n_lon_out,n_lat_out);
go_a  = zeros(n_lon_out,n_lat_out);
go_fa = zeros(n_lon_out,n_lat_out);
% create 2D lat & lon grids
[gi_latn gi_lone] = meshgrid(lat_in(2:end),lon_in(2:end));
[gi_lats gi_lonw] = meshgrid(lat_in(1:end-1),lon_in(1:end-1));
[go_latn go_lone] = meshgrid(lat_out(2:end),lon_out(2:end));
[go_lats go_lonw] = meshgrid(lat_out(1:end-1),lon_out(1:end-1));
% area (in)
gi_area = zeros(n_lon_in,n_lat_in);
gi_area(:,:) = 2.0*pi*(par_rEarth^2)*(sin((pi/180.0)*gi_latn) - sin((pi/180.0)*gi_lats)).*((gi_lone-gi_lonw)/360.0);
if (opt_debug),
    figure;
    plot_2dgridded(flipud(gi_area(:,:)'),0.9E19,'','gi_area','gi_area');
end
% area (out)
go_area = zeros(n_lon_out,n_lat_out);
go_area(:,:) = 4.0*pi*(par_rEarth^2)*(1.0/n_lon_out)*(1.0/n_lat_out);
if (opt_debug),
    figure;
    plot_2dgridded(flipud(go_area(:,:)'),0.9E19,'','go_area','go_area');
end
% create mask (and remove NaNs from input data)
gi_mask = zeros(n_lon_in,n_lat_in);
gi_mask = gi_mask + 1.0;
gi_mask(find(isnan(gi_z))) = 0.0;
gi_z(find(isnan(gi_z))) = 0.0;
% misc
gi_fa = zeros(n_lon_in,n_lat_in);
% create wrap-around input grid
gi_latn = [gi_latn; gi_latn; gi_latn];
gi_lats = [gi_lats; gi_lats; gi_lats];
gi_lone = [gi_lone-360.0; gi_lone; gi_lone+360.0];
gi_lonw = [gi_lonw-360.0; gi_lonw; gi_lonw+360.0];
gi_z    = [gi_z; gi_z; gi_z];
gi_area = [gi_area; gi_area; gi_area];
gi_a    = [gi_fa; gi_fa; gi_fa];
gi_fa   = [gi_fa; gi_fa; gi_fa];
gi_mask = [gi_mask; gi_mask; gi_mask];
%
icelln_le_ocelln = zeros(3*n_lon_in,n_lat_in);
icells_ge_ocells = zeros(3*n_lon_in,n_lat_in);
icelle_le_ocelle = zeros(3*n_lon_in,n_lat_in);
icellw_ge_ocellw = zeros(3*n_lon_in,n_lat_in);
icelln_gt_ocells = zeros(3*n_lon_in,n_lat_in);
icells_lt_ocelln = zeros(3*n_lon_in,n_lat_in);
icelle_gt_ocellw = zeros(3*n_lon_in,n_lat_in);
icellw_lt_ocelle = zeros(3*n_lon_in,n_lat_in);
%
% *** do the re-gridding ************************************************ %
%
% NOTE: coding does not come cruder than this ..
%       all possible orientations of new vs. old grid cells are
%       tediously and explicitly identified ...
% NOTE: approximaion for both grids being strictly lon-lat
%
% > START lon LOOP
for i=1:n_lon_out,
    % > START lat LOOP
    for j=1:n_lat_out,
        % print something
        if (opt_debug), disp([' > LOOP: (i,j) location = (', num2str(i), ',', num2str(j), ')']); end
        % initialize local grid match arrays
        icelln_le_ocelln = 0.0*icelln_le_ocelln;
        icells_ge_ocells = 0.0*icells_ge_ocells;
        icelle_le_ocelle = 0.0*icelle_le_ocelle;
        icellw_ge_ocellw = 0.0*icellw_ge_ocellw;
        icelln_gt_ocells = 0.0*icelln_gt_ocells;
        icells_lt_ocelln = 0.0*icells_lt_ocelln;
        icelle_gt_ocellw = 0.0*icelle_gt_ocellw;
        icellw_lt_ocelle = 0.0*icellw_lt_ocelle;
        % determine grid relative boundary relationships
        [icelln_le_ocelln_i icelln_le_ocelln_j] = find(gi_latn(1,:) <= go_latn(i,j));
        icelln_le_ocelln(:,icelln_le_ocelln_j) = 1.0;
        [icells_ge_ocells_i icells_ge_ocells_j] = find(gi_lats(1,:) >= go_lats(i,j));
        icells_ge_ocells(:,icells_ge_ocells_j) = 1.0;
        [icelle_le_ocelle_i icelle_le_ocelle_j] = find(gi_lone(:,1) <= go_lone(i,j));
        icelle_le_ocelle(icelle_le_ocelle_i,:) = 1.0;
        [icellw_ge_ocellw_i icellw_ge_ocellw_j] = find(gi_lonw(:,1) >= go_lonw(i,j));
        icellw_ge_ocellw(icellw_ge_ocellw_i,:) = 1.0;
        [icelln_gt_ocells_i icelln_gt_ocells_j] = find(gi_latn(1,:) > go_lats(i,j));
        icelln_gt_ocells(:,icelln_gt_ocells_j) = 1.0;
        [icells_lt_ocelln_i icells_lt_ocelln_j] = find(gi_lats(1,:) < go_latn(i,j));
        icells_lt_ocelln(:,icells_lt_ocelln_j) = 1.0;
        [icelle_gt_ocellw_i icelle_gt_ocellw_j] = find(gi_lone(:,1) > go_lonw(i,j));
        icelle_gt_ocellw(icelle_gt_ocellw_i,:) = 1.0;
        [icellw_lt_ocelle_i icellw_lt_ocelle_j] = find(gi_lonw(:,1) < go_lone(i,j));
        icellw_lt_ocelle(icellw_lt_ocelle_i,:) = 1.0;
        % case #1: old cells wholly within a new cell
        icell_ocell = gi_mask.*icelln_le_ocelln.*icells_ge_ocells.*icelle_le_ocelle.*icellw_ge_ocellw;
        loc_n = sum(sum(icell_ocell));
        if (loc_n > 0.0),
            loc_a = 1.0*gi_area;
            go_z(i,j) = go_z(i,j) + sum(sum(gi_area.*gi_z.*icell_ocell))/go_area(i,j);
            go_fa(i,j) = go_fa(i,j) + sum(sum(gi_area.*icell_ocell))/go_area(i,j);
            go_n(i,j) = go_n(i,j) + loc_n;
            if (opt_debug),
                go_a(i,j) = go_a(i,j) + sum(sum(gi_area.*icell_ocell));
                gi_fa = gi_fa + loc_a.*icell_ocell()./gi_area;
            end
        end
        % case #2a: old cells spanning a new cell N boundary (only)
        icell_ocell = gi_mask.*(1.0-icelln_le_ocelln).*icells_lt_ocelln.*icelle_le_ocelle.*icellw_ge_ocellw;
        loc_n = sum(sum(icell_ocell));
        if (loc_n > 0.0),
            loc_lat_i = mean(gi_lats(find(icell_ocell == 1.0)));
            loc_lat_o = go_latn(i,j);
            loc_as = 2.0*pi*(par_rEarth^2)*(sin((pi/180.0)*loc_lat_o) - sin((pi/180.0)*loc_lat_i))*(1.0/n_lon_in);
            go_z(i,j) = go_z(i,j) + sum(sum(loc_as*gi_z.*icell_ocell))/go_area(i,j);
            go_fa(i,j) = go_fa(i,j) + sum(sum(loc_as*icell_ocell))/go_area(i,j);
            go_n(i,j) = go_n(i,j) + loc_n;
            if (opt_debug),
                go_a(i,j) = go_a(i,j) + sum(sum(loc_as*icell_ocell));
                gi_fa = gi_fa + loc_as*icell_ocell()./gi_area;
            end
        end
        % case #2b: old cells spanning a new cell S boundary (only)
        icell_ocell = gi_mask.*(1.0-icells_ge_ocells).*icelln_gt_ocells.*icelle_le_ocelle.*icellw_ge_ocellw;
        loc_n = sum(sum(icell_ocell));
        if (loc_n > 0.0),
            loc_lat_i = mean(gi_latn(find(icell_ocell == 1.0)));
            loc_lat_o = go_lats(i,j);
            loc_as = 2.0*pi*(par_rEarth^2)*(sin((pi/180.0)*loc_lat_i) - sin((pi/180.0)*loc_lat_o))*(1.0/n_lon_in);
            go_z(i,j) = go_z(i,j) + sum(sum(loc_as*gi_z.*icell_ocell))/go_area(i,j);
            go_fa(i,j) = go_fa(i,j) + sum(sum(loc_as*icell_ocell))/go_area(i,j);
            go_n(i,j) = go_n(i,j) + loc_n;
            if (opt_debug),
                go_a(i,j) = go_a(i,j) + sum(sum(loc_as*icell_ocell));
                gi_fa = gi_fa + loc_as*icell_ocell()./gi_area;
            end
        end
        % case #3a: old cells spanning a new cell E boundary (only)
        icell_ocell = gi_mask.*icelln_le_ocelln.*icells_ge_ocells.*(1.0-icelle_le_ocelle).*icellw_lt_ocelle;
        loc_n = sum(sum(icell_ocell));
        if (loc_n > 0.0),
            loc_lon_i = mean(gi_lonw(find(icell_ocell == 1.0)));
            loc_lon_o = go_lone(i,j);
            loc_a = gi_area.*((loc_lon_o - loc_lon_i)./(gi_lone - gi_lonw));
            go_z(i,j) = go_z(i,j) + sum(sum(loc_a.*gi_z.*icell_ocell))/go_area(i,j);
            go_fa(i,j) = go_fa(i,j) + sum(sum(loc_a.*icell_ocell))/go_area(i,j);
            go_n(i,j) = go_n(i,j) + loc_n;
            if (opt_debug),
                go_a(i,j) = go_a(i,j) + sum(sum(loc_a.*icell_ocell));
                gi_fa = gi_fa + loc_a.*icell_ocell()./gi_area;
            end
        end
        % case #3b: old cells spanning a new cell W boundary (only)
        icell_ocell = gi_mask.*icelln_le_ocelln.*icells_ge_ocells.*(1.0-icellw_ge_ocellw).*icelle_gt_ocellw;
        loc_n = sum(sum(icell_ocell));
        if (loc_n > 0.0),
            loc_lon_i = mean(gi_lone(find(icell_ocell == 1.0)));
            loc_lon_o = go_lonw(i,j);
            loc_a = gi_area.*((loc_lon_i - loc_lon_o)./(gi_lone - gi_lonw));
            go_z(i,j) = go_z(i,j) + sum(sum(loc_a.*gi_z.*icell_ocell))/go_area(i,j);
            go_fa(i,j) = go_fa(i,j) + sum(sum(loc_a.*icell_ocell))/go_area(i,j);
            go_n(i,j) = go_n(i,j) + loc_n;
            if (opt_debug),
                go_a(i,j) = go_a(i,j) + sum(sum(loc_a.*icell_ocell));
                gi_fa = gi_fa + loc_a.*icell_ocell()./gi_area;
            end
        end
        % case #4a: old cells spanning a new cell NE corner
        icell_ocell = gi_mask.*(1.0-icelln_le_ocelln).*icells_lt_ocelln.*(1.0-icelle_le_ocelle).*icellw_lt_ocelle;
        loc_n = sum(sum(icell_ocell));
        if (loc_n > 0.0),
            loc_lat_i = mean(gi_lats(find(icell_ocell == 1.0)));
            loc_lat_o = go_latn(i,j);
            loc_lon_i = mean(gi_lonw(find(icell_ocell == 1.0)));
            loc_lon_o = go_lone(i,j);
            loc_a = 2.0*pi*(par_rEarth^2)*(sin((pi/180.0)*loc_lat_o) - sin((pi/180.0)*loc_lat_i))*(1.0/n_lon_in)*((loc_lon_o - loc_lon_i)./(gi_lone - gi_lonw));
            go_z(i,j) = go_z(i,j) + sum(sum(loc_a.*gi_z.*icell_ocell))/go_area(i,j);
            go_fa(i,j) = go_fa(i,j) + sum(sum(loc_a.*icell_ocell))/go_area(i,j);
            go_n(i,j) = go_n(i,j) + loc_n;
            if (opt_debug),
                go_a(i,j) = go_a(i,j) + sum(sum(loc_a.*icell_ocell));
                gi_fa = gi_fa + loc_a.*icell_ocell()./gi_area;
            end
        end
        % case #4b: old cells spanning a new cell NW corner
        icell_ocell = gi_mask.*(1.0-icelln_le_ocelln).*icells_lt_ocelln.*(1.0-icellw_ge_ocellw).*icelle_gt_ocellw;
        loc_n = sum(sum(icell_ocell));
        if (loc_n > 0.0),
            loc_lat_i = mean(gi_lats(find(icell_ocell == 1.0)));
            loc_lat_o = go_latn(i,j);
            loc_lon_i = mean(gi_lone(find(icell_ocell == 1.0)));
            loc_lon_o = go_lonw(i,j);
            loc_a = 2.0*pi*(par_rEarth^2)*(sin((pi/180.0)*loc_lat_o) - sin((pi/180.0)*loc_lat_i))*(1.0/n_lon_in)*((loc_lon_i - loc_lon_o)./(gi_lone - gi_lonw));
            go_z(i,j) = go_z(i,j) + sum(sum(loc_a.*gi_z.*icell_ocell))/go_area(i,j);
            go_fa(i,j) = go_fa(i,j) + sum(sum(loc_a.*icell_ocell))/go_area(i,j);
            go_n(i,j) = go_n(i,j) + loc_n;
            if (opt_debug),
                go_a(i,j) = go_a(i,j) + sum(sum(loc_a.*icell_ocell));
                gi_fa = gi_fa + loc_a.*icell_ocell()./gi_area;
            end
        end
        % case #4c: old cells spanning a new cell SE corner
        icell_ocell = gi_mask.*(1.0-icells_ge_ocells).*icelln_gt_ocells.*(1.0-icelle_le_ocelle).*icellw_lt_ocelle;
        loc_n = sum(sum(icell_ocell));
        if (loc_n > 0.0),
            loc_lat_i = mean(gi_latn(find(icell_ocell == 1.0)));
            loc_lat_o = go_lats(i,j);
            loc_lon_i = mean(gi_lonw(find(icell_ocell == 1.0)));
            loc_lon_o = go_lone(i,j);
            loc_a = 2.0*pi*(par_rEarth^2)*(sin((pi/180.0)*loc_lat_i) - sin((pi/180.0)*loc_lat_o))*(1.0/n_lon_in)*((loc_lon_o - loc_lon_i)./(gi_lone - gi_lonw));
            go_z(i,j) = go_z(i,j) + sum(sum(loc_a.*gi_z.*icell_ocell))/go_area(i,j);
            go_fa(i,j) = go_fa(i,j) + sum(sum(loc_a.*icell_ocell))/go_area(i,j);
            go_n(i,j) = go_n(i,j) + loc_n;
            if (opt_debug),
                go_a(i,j) = go_a(i,j) + sum(sum(loc_a.*icell_ocell));
                gi_fa = gi_fa + loc_a.*icell_ocell()./gi_area;
            end
        end
        % case #4d: old cells spanning a new cell SW corner
        icell_ocell = gi_mask.*(1.0-icells_ge_ocells).*icelln_gt_ocells.*(1.0-icellw_ge_ocellw).*icelle_gt_ocellw;
        loc_n = sum(sum(icell_ocell));
        if (loc_n > 0.0),
            loc_lat_i = mean(gi_latn(find(icell_ocell == 1.0)));
            loc_lat_o = go_lats(i,j);
            loc_lon_i = mean(gi_lone(find(icell_ocell == 1.0)));
            loc_lon_o = go_lonw(i,j);
            loc_a = 2.0*pi*(par_rEarth^2)*(sin((pi/180.0)*loc_lat_i) - sin((pi/180.0)*loc_lat_o))*(1.0/n_lon_in)*((loc_lon_i - loc_lon_o)./(gi_lone - gi_lonw));
            go_z(i,j) = go_z(i,j) + sum(sum(loc_a.*gi_z.*icell_ocell))/go_area(i,j);
            go_fa(i,j) = go_fa(i,j) + sum(sum(loc_a.*icell_ocell))/go_area(i,j);
            go_n(i,j) = go_n(i,j) + loc_n;
            if (opt_debug),
                go_a(i,j) = go_a(i,j) + sum(sum(loc_a.*icell_ocell));
                gi_fa = gi_fa + loc_a.*icell_ocell()./gi_area;
            end
        end
        % normalize to cell area
        if (go_fa(i,j) > 0.0),
            go_z(i,j) = go_z(i,j)/go_fa(i,j);
        end
        % < END lat LOOP
    end
    % < END lon LOOP
end
% replace zero hits cells with NaNs
go_z(find(go_n(:,:) == 0.0)) = NaN;
%
% *** plot something ... ************************************************ %
%
if (opt_plot),
    figure;
    plot_2dgridded(flipud(go_n(:,:)'),0.9E19,'','go_n','go_n');
    figure;
    plot_2dgridded(flipud(go_fa(:,:)'),0.9E19,'','go_fa','go_fa');
    figure;
    plot_2dgridded(flipud(go_z(:,:)'),0.9E19,'','go_z','go_z');
    if (opt_debug),
        figure;
        plot_2dgridded(flipud(go_a(:,:)'),0.9E19,'','go_a','go_a');
        figure;
        plot_2dgridded(flipud(gi_fa(:,:)'),0.9E19,'','gi_fa','gi_fa');
    end
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** END *************************************************************** %
% *********************************************************************** %
%
% set return value
% NOTE: output is (i,j) rather than (GENIE grid) (j,i)
zo = go_z;
fao = go_fa;
%
% *********************************************************************** %
