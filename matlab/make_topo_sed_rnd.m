function [PTOPOOUT] = make_topo_sed_rnd(PTOPOIN,PDMIN,PDMAX,PNAME)
% make_topo_sed_rnd
%
%   ***********************************************************************
%   *** Create randomized SEDGEM topo *************************************
%   ***********************************************************************
%
%   make_topo_sed_rnd(PDIR,PSTR1,PSTR2,PVAR1,PVAR2,PSTROUT)
%   BLAH
%
%   PDIR [STRING] (e.g. '')
%   --> BLAH
%   PSTRHEAD [STRING] (e.g. '')
%   --> head string (common to all experiment file names)
%   PSTRTAIL [STRING] (e.g. '')
%   --> tail string (common to all experiment file names)
%   PVAR [STRING] (e.g. '')
%   --> variable (file) to extract
%   PSTROUT [STRING] (e.g. '')
%   --> output directory
%
%   ***********************************************************************
%   *** HISTORY ***********************************************************
%   ***********************************************************************
%
%   14/11/22: CREATED [derived from Goldschmidt 2013 era process_hyps.m]
%
%   ***********************************************************************

% *********************************************************************** %
% *** INITIALIZE PARAMETERS & VARIABLES ********************************* %
% *********************************************************************** %
%
% initialize
close all;
% set dummy variables
grid_depth_old = PTOPOIN;
bath_min = PDMIN;
bath_max = PDMAX;
str_filename = PNAME;
% determine grid size
loc_size = size(grid_depth_old);
imax = loc_size(1);
jmax = loc_size(2);
% set date
str_date = [datestr(date,11), datestr(date,5), datestr(date,7)];
%
% *********************************************************************** %

% *********************************************************************** %
% *** LOAD HYPSOGRAPHIC DATA ******************************************** %
% *********************************************************************** %
%
% load data (processed with 10 m intervals bins)
hyps=load('data\elev0010_area_int_norm.dat','-ascii');
% invert depth
hyps(:,1)=-hyps(:,1);
% filter out too shallow/deep data
hyps(find(hyps(:,1)<bath_min),:)=[];
hyps(find(hyps(:,1)>bath_max),:)=[];
%
% *** process data ****************************************************** %
%
% calculate total fractional area remaining (after filtering)
loc_total_frac_seafloor = hyps(end,2)-hyps(1,2);
% re-normalize fractional area
hyps(:,2) = (hyps(:,2)-hyps(1,2))/loc_total_frac_seafloor;
%
% *********************************************************************** %

% *********************************************************************** %
% *** CREATE TOPO ******************************************************* %
% *********************************************************************** %
%
for i = 1:imax,
    for j = 1:jmax,
        if (grid_depth_old(i,j) > bath_min),
            roll = rand(1);
            grid_depth_new(i,j) = double(hyps(find(abs(roll - hyps(:,2)) == min(abs(roll - hyps(:,2)))),1));
        else
            grid_depth_new(i,j) = grid_depth_old(i,j);
        end
    end
end
%
% *** plot topo ********************************************************* %
%
plot_2Dgridded(grid_depth_old,9999,'',[str_filename '.old'],'old grid depth (m)');
%
plot_2Dgridded(grid_depth_new,9999,'',[str_filename '.new'],'new grid depth (m)');
%
% *** save topo ********************************************************* %
%
% save sediment depth file
if (~isempty(str_filename)),
    fprint_jigrid(grid_depth_new,[str_filename '.rnd.' str_date],'  %6.1f');
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** END *************************************************************** %
% *********************************************************************** %
%
% return function value
PTOPOOUT = grid_depth_new;
% END
disp(['END ...'])
%
% *********************************************************************** %
