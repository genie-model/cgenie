function [] = plot_cmap(PNCOL,POPT)
% plot_cmap
%
%   ***********************************************************************
%   *** plot color scales *************************************************
%   ***********************************************************************
%
%   plot_cmap( ... )
%   plots the available color scales
%   (excepting the built-in MATLAB ones, but other than 'jet')
%   and takes 2 arguments:
%
%   PNCOL [INTEGER]
%   --> number of colors to plot
%   POPT [STRING]
%   --> the string for an alternative plotting parameter set
%   --> if an empty (i.e., '') value is passed to this parameter
%       then the default parameter set is used

%   Example
%           plot_cmap(30,'');
%           plots all the available color scales with 30 colors
%
%   ***********************************************************************
%   *** HISTORY ***********************************************************
%   ***********************************************************************
%
%   15/02/11: CREATED
%   15/02/12: completed initial working version
%
%   ***********************************************************************

% *********************************************************************** %
% *** INITIALIZE PARAMETERS & VARIABLES ********************************* %
% *********************************************************************** %
%
% *** INITIALIZE ******************************************************** %
%
% close currently open windows
close all;
%
% *** USER OPTIONS ****************************************************** %
%
% plot format
plot_widthfrac = 1.0;
plot_n_col = PNCOL;
%
% *** DEFINITIONS ******************************************************* %
%
% set date
str_date = [datestr(date,11), datestr(date,5), datestr(date,7)];
% set function name
str_function = 'plot-cmap';
% load plotting options
if isempty(POPT), POPT='plot_fields_settings'; end
eval(POPT);
if ~isempty(plot_format), plot_format_old='n'; end
% plotting paths
addpath(library_path1);
if (~plot_format_old),
    addpath(library_path2);
    addpath(library_path3);
end
% create dummy data
data = [0:1:10];
% define color scale name list
str_cmap = struct('cname', {});
str_cmap = setfield(str_cmap, {1}, 'cname', ['anom']);
str_cmap = setfield(str_cmap, {2}, 'cname', ['wt%']);
str_cmap = setfield(str_cmap, {3}, 'cname', ['_parula']);
str_cmap = setfield(str_cmap, {4}, 'cname', ['RdYlGn']);
str_cmap = setfield(str_cmap, {5}, 'cname', ['RdYlBu']);
str_cmap = setfield(str_cmap, {6}, 'cname', ['RdGy']);
str_cmap = setfield(str_cmap, {7}, 'cname', ['RdBu']);
str_cmap = setfield(str_cmap, {8}, 'cname', ['PuOr']);
str_cmap = setfield(str_cmap, {9}, 'cname', ['PRGn']);
str_cmap = setfield(str_cmap, {10}, 'cname', ['PiYG']);
str_cmap = setfield(str_cmap, {11}, 'cname', ['BrBG']);
str_cmap = setfield(str_cmap, {12}, 'cname', ['LinearL']);
str_cmap = setfield(str_cmap, {13}, 'cname', ['CubicYF']);
str_cmap = setfield(str_cmap, {14}, 'cname', ['jet']);
n_cmap = 14;
%
% *********************************************************************** %

% *********************************************************************** %
% *** PLOT COLOR SCALES! ************************************************ %
% *********************************************************************** %
%
% *** CREATE FIGURE ***************************************************** %
%
scrsz = get(0,'ScreenSize');
figure('Position',[((1.0-plot_dscrsz)/2)*plot_dscrsz*scrsz(3) ((1.0-plot_dscrsz)/2)*plot_dscrsz*scrsz(4) 0.75*plot_dscrsz*scrsz(3) 0.75*plot_dscrsz*scrsz(4)]);
clf;
% define plotting regions
fh(1) = axes('Position',[0 0 1 1],'Visible','off');
fh(2) = axes('Position',[0.05 0.1 plot_widthfrac*0.9 0.5],'Visible','off');
fh(3) = axes('Position',[0.05 0.7 plot_widthfrac*0.9 0.3],'Visible','off');
% date-stamp plot
set(gcf,'CurrentAxes',fh(1));
text(0.95,0.50,[str_function, ' : ', str_date],'FontName','Arial','FontSize',10,'Rotation',90.0,'HorizontalAlignment','center','VerticalAlignment','top');
%
% *** DRAW COLOR BARS *************************************************** %
%
set(gcf,'CurrentAxes',fh(2));
hold on;
%
for n = 1:n_cmap,
    % define colormap
    cmap = make_cmap(str_cmap(n).cname,plot_n_col);
    colormap(cmap);
    % define color bar scaling
    loc_offst = (n-1)/n_cmap;
    loc_scale = 1.0/n_cmap;
    % draw end cap -- bottom
    m = 1;
    h = fill(loc_offst + loc_scale*[0.0 0.25 0.5],[m m-1.0 m],cmap(m,:));
    set(h,'LineWidth',0.5);
    set(h,'EdgeColor','k');
    % draw color bar rectangles
    for m = 2:plot_n_col-1,
        h = fill(loc_offst + loc_scale*[0.0 0.0 0.5 0.5],[m-1.0 m m m-1.0],cmap(m,:));
        set(h,'LineWidth',0.5);
        set(h,'EdgeColor','k');
    end
    % draw end cap -- top
    m = plot_n_col;
    h = fill(loc_offst + loc_scale*[0.0 0.25 0.5],[m-1.0 m m-1.0],cmap(m,:));
    set(h,'LineWidth',0.5);
    set(h,'EdgeColor','k');
    % label color bar
    %     for m = 2:n_col-1,
    %         str = num2str(m);
    %         textsize = 10;
    %         text(0.40,m,str,'FontName','Arial','FontSize',textsize);
    %     end
end
%
% *** LABEL COLOR BARS ************************************************** %
%
set(gcf,'CurrentAxes',fh(3));
hold on;
%
for n = 1:n_cmap,
    loc_offst = (n-1)/n_cmap;
    loc_scale = 1.0/n_cmap;
    loc_str = str_cmap(n).cname;
    loc_str = strrep(loc_str,'_',' ');
    text(loc_offst + loc_scale*0.25,0.0,loc_str,'FontName','Arial','FontSize',15,'Rotation',90.0,'HorizontalAlignment','left','VerticalAlignment','middle');
end
%
% *** PRINT PLOT ******************************************************** %
%
set(gcf,'CurrentAxes',fh(1));
str_filename = ['colorscales'];
str_filename = [str_filename '.' str_date];
if (plot_format_old)
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
% END
disp(['END ...']);
%
% *********************************************************************** %
