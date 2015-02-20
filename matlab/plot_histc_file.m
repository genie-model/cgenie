function [] = plot_histc_file(PDATA,PBINBNDS,POPT)
% plot_histc_file
%
%   ***********************************************************************
%   *** plot color-coded histogram ****************************************
%   ***********************************************************************
%
%   plot_histc_file( ... )
%   blah ...
%
%   PDATA [STRING] (e.g., 'xxx')
%   --> blah ...
%   PBINS [STRING] (e.g., 'xxx')
%   --> blah ...
%   POPT [STRING] (e.g., 'plotting_config_2')
%   --> the string for an alternative plotting parameter set
%   --> if an empty (i.e., '') value is passed to this parameter
%       then the default parameter set is used
%
%   ***********************************************************************
%   *** HISTORY ***********************************************************
%   ***********************************************************************
%
%   14/08/19: CREATED
%   14/08/20: further development and 1st stable version
%   14/12/31: renamed from plot_histc.m
%
%   ***********************************************************************

% *********************************************************************** %
% *** INITIALIZE PARAMETERS & VARIABLES ********************************* %
% *********************************************************************** %
%
% close open windows
% NOTE: don't clear variable space here ...
close all;
% load plotting options
if isempty(POPT), POPT='plot_hist_settings'; end
eval(POPT);
% process dummy parameters
dataid = PDATA;
binbndsid = PBINBNDS;
%
% *** misc (local) parameters ******************************************* %
%
% set date
str_date = [datestr(date,11), datestr(date,5), datestr(date,7)];
% set function name
str_function = 'plot-histc';
% plotting paths
addpath(library_path1);
if (plot_format_old == 'n'),
    addpath(library_path2);
    addpath(library_path3);
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** LOAD & PROCESS DATA *********************************************** %
% *********************************************************************** %
%
% *** LOAD BINS BNDS AND COLORSCALE ************************************* %
%
data_binbnds = load(binbndsid,'-ascii');
nbins = length(data_binbnds)-1;
% create bin centers
for n = 1:nbins,
    data_binctrs(n) = (data_binbnds(n) + data_binbnds(n+1))/2.0;
end
% create data arrays
data_n_binned = zeros(nbins,1);
%
% *** LOAD DATA ********************************************************* %
%
% load overlay datafile
datafile = [dataid];
fid = fopen(datafile);
C = textscan(fid, '%f %f %f %f %f %f %f %f %s', 'CommentStyle', '%');
data_raw = cell2mat(C(1:8));
fclose(fid);
data_size = size(data_raw(:,:));
nmax=data_size(1);
% extract data values column
data = data_raw(:,7);
data_bin = zeros(nmax,1);
%
% *** PROCESS DATA ****************************************************** %
%
% bin data
data_n_binnedex = 0;
for n = 1:nmax,
    loc_bin=[];
    loc_bin = intersect(find(data(n)>=data_binbnds(:)),find(data(n)<data_binbnds(:))-1);
    if isempty(loc_bin),
        data_n_binnedex = data_n_binnedex + 1;
        data_bin_n(n) = -1;
    else
        data_n_binned(loc_bin) = data_n_binned(loc_bin) + 1;
        data_bin_n(n) = loc_bin;
    end
end
% normalize data (if requested)
if data_normalize,
    data_n_binned = 100.0 * data_n_binned/(sum(data_n_binned(:)) + data_n_binnedex);
end
%
% *** DEFINE COLORS ***************************************************** %
%
% define 'anomoly' color map
c1 = 1;
c2 = double(1 + int32((nbins+1)/2.0));
c3 = (nbins+1)+1;
c4 = double((nbins+1)+1 + int32((nbins+1)/2.0));
c5 = (nbins+1)+1 + (nbins+1);
i1 = [255 0 0];
i2 = [255 255 127];
i3 = [255 255 255];
i4 = [127 255 255];
i5 = [0 0 255];
cmap_anom(1,:) = abs(i1)/255.0;
for c = c1+1:c2,
    cmap_anom(c,:) = abs(i1 + ((c-c1)/(c2-c1))*(i2 - i1))/255.0;
end
for c = c2+1:c3,
    cmap_anom(c,:) = abs(i2 + ((c-c2)/(c3-c2))*(i3 - i2))/255.0;
end
for c = c3+1:c4,
    cmap_anom(c,:) = abs(i3 + ((c-c3)/(c4-c3))*(i4 - i3))/255.0;
end
for c = c4+1:c5,
    cmap_anom(c,:) = abs(i4 + ((c-c4)/(c5-c4))*(i5 - i4))/255.0;
end
cmap_anom = flipdim(cmap_anom,2);
%
% *********************************************************************** %

% *********************************************************************** %
% *** PLOT DATA ********************************************************* %
% *********************************************************************** %
%
% *** CREATE FIGURE ***************************************************** %
%
scrsz = get(0,'ScreenSize');
figure('Position',[((1.0-plot_dscrsz)/2)*plot_dscrsz*scrsz(3) ((1.0-plot_dscrsz)/2)*plot_dscrsz*scrsz(4) 0.75*plot_dscrsz*scrsz(3) 0.75*plot_dscrsz*scrsz(4)]);
clf;
% define plotting regions
fh(1) = axes('Position',[0 0 1 1],'Visible','off');
fh(2) = axes('Position',[0.15 0.15 0.65 0.70]);
fh(3) = axes('Position',[0.75 0.15 0.15 0.70],'Visible','off');
% define colormap
switch data_colobar,
    case ('anom')
        cmap = cmap_anom;
    otherwise
        cmap = colormap(jet((2*(nbins+1))+1));
end
if data_colorbar_inv, cmap = flipdim(cmap,2); end,
colormap(cmap);
% date-stamp plot
set(gcf,'CurrentAxes',fh(1));
text(0.95,0.50,[str_function, ' : ', strrep(dataid,'_',' '), ' : ', str_date],'FontName','Arial','FontSize',10,'Rotation',90.0,'HorizontalAlignment','center','VerticalAlignment','top');
%
% *** CREATE DATA HISTOGRAM ********************************************* %
%
set(gcf,'CurrentAxes',fh(2));
hold on;
% set title
if ~isempty(plot_title)
    title(plot_title,'FontSize',18);
else
    title(['Data: ',strrep(dataid,'_',' ')],'FontSize',12);
end
% set axes limits -- x
if (axis_xmin >= axis_xmax),
    axis_xmin = min(data_binbnds(:));
    axis_xmax = max(data_binbnds(:));
end
if (axis_xmin >= axis_xmax), disp(['ERROR: Failed to autoscale x-axis ... ']); return; end
% set axes limits -- y
if (axis_ymin >= axis_ymax),
    axis_ymin = min(data_n_binned(:));
    axis_ymax = max(data_n_binned(:));
end
if (axis_ymin >= axis_ymax), disp(['ERROR: Failed to autoscale y-axis ... ']); return; end
% set color axis and ticks
caxis([axis_xmin-(axis_xmax-axis_xmin)/nbins axis_xmax]);
set(gca,'PlotBoxAspectRatio',[1.0 0.5 1.0]);
axis([axis_xmin axis_xmax axis_ymin axis_ymax]);
set(gca,'TickDir','out');
% label axes
if isempty(plot_dataname),
    set(gca,'XLabel',text('String','Environmental parameter','FontSize',15));
else
    set(gca,'XLabel',text('String',[plot_dataname],'FontSize',15));
end
if data_normalize,
    set(gca,'YLabel',text('String','% occurrences','FontSize',15));
else
    set(gca,'YLabel',text('String','# of occurrences','FontSize',15));
end
% draw filled rectangles
for n = 1:nbins,    
    col = round((1/2)+nbins*(data_binctrs(n)-axis_xmin)/(axis_xmax-axis_xmin));
    if col < 1
        col = 1;
    elseif col > nbins
        col = 2*(nbins+1)+1;
    else
        col = 2*col+1;
    end
    h = patch([data_binbnds(n) data_binbnds(n) data_binbnds(n+1) data_binbnds(n+1)],[0.0 data_n_binned(n) data_n_binned(n) 0.0],cmap(col,:));
    set(h,'EdgeColor','k');
end
%
% *** PRINT PLOT ******************************************************** %
%
set(gcf,'CurrentAxes',fh(1));
str_filename = ['colorhistogram.' dataid];
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
disp(['END ...'])
%
% *********************************************************************** %
