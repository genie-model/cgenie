function [] = plot_histc_2d(PDATA1,PBINBNDS1,PDATA2,PBINBNDS2,POPT,PNAME)
% plot_histc_2d
%
%   ***********************************************************************
%   *** plot color-coded histogram ****************************************
%   ***********************************************************************
%
%   plot_histc_2d( ... )
%   blah ...
%
%   PDATA1 [vector] (e.g., )
%   --> blah ...
%   PBINBNDS1 [vector] (e.g., [0 1 2 3 4 5 6 7 8 9 10])
%   --> blah ...
%   PDATA2 [vector] [OPTIONAL] (e.g., )
%   --> blah ...
%   PBINBNDS2 [vector] [OPTIONAL] (e.g., [0 1000 2000 3000 4000 5000 6000])
%   --> blah ...
%   POPT [STRING] (e.g., 'plotting_config_2')
%   --> the string for an alternative plotting parameter set
%   --> if an empty (i.e., '') value is passed to this parameter
%       then the default parameter set is used
%   PNAME [STRING] (e.g., 'my_plot')
%   --> the string for an alternative filename
%   --> if an empty (i.e., '') value is passed to this parameter
%       then a filename is automatically generated
%
%   ***********************************************************************
%   *** HISTORY ***********************************************************
%   ***********************************************************************
%
%   14/12/31: CREATED [copied from plot_histc_file.m]
%   15/01/07: auto-set new plotting format
%             adjusted auto setting of y-axis scale
%             fixed x-axis truncation
%   15/01/23: changed to use make _cmap.m (replacing make_cmap5.m)
%
%   ***********************************************************************

% *********************************************************************** %
% *** INITIALIZE PARAMETERS & VARIABLES ********************************* %
% *********************************************************************** %
%
% load plotting options
if isempty(POPT), POPT='plot_hist_settings'; end
eval(POPT);
% process dummy parameters
data_1 = PDATA1;
data_2 = PDATA2;
binbnds_1 = PBINBNDS1;
binbnds_2 = PBINBNDS2;
str_filename = PNAME;
% check for no data #2 and create dummy data_2 data to simplify processing
if (isempty(data_2)), binbnds_2 = []; end
if (isempty(binbnds_2)), data_2 = data_1; end
% set date
str_date = [datestr(date,11), datestr(date,5), datestr(date,7)];
% set function name
str_function = 'plot-histc-2d';
% plot format
if ~isempty(plot_format), plot_format_old=false; end
% plotting paths
addpath(library_path1);
if (~plot_format_old),
    addpath(library_path2);
    addpath(library_path3);
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** PROCESS DATA ****************************************************** %
% *********************************************************************** %
%
% *** PROCESS BINS ****************************************************** %
%
% set #bins
nbins_1 = length(binbnds_1)-1;
% create bin centers (why ... ?)
for m = 1:nbins_1,
    binctrs_1(m) = (binbnds_1(m) + binbnds_1(m+1))/2.0;
end
% create bin counts (used in normalizing data)
data_n_binned = zeros(nbins_1,1);
% create x-axis bins if data #2 present (test via bin boundary array #2)
if (~isempty(binbnds_2)),
    nbins_2 = length(binbnds_2)-1;
    for o = 1:nbins_2,
        binctrs_2(o) = (binbnds_2(o) + binbnds_2(o+1))/2.0;
    end
    data_n_binned_2d = zeros(nbins_1,nbins_2);
end
%
% *** INITIALIZE DATA *************************************************** %
%
% filter data for NaNs and determine data length
loc_nmax=length(data_1);
n = 1;
nn = 1;
while (nn <= loc_nmax)
    if (isnan(data_1(n)) || isnan(data_2(n))),
        data_1(n) = [];
        data_2(n) = [];
    else
        n=n+1;
    end
    nn=nn+1;
end
nmax = n-1;
%
data_bin_n_1 = zeros(nmax,1);
data_bin_n_2 = zeros(nmax,1);
data_n_binnedex = 0;
%
% *** BIN DATA ON X-AXIS ************************************************ %
%
% bin data
for n = 1:nmax,
    % determine which  bin (if any) the data falls within
    loc_bin=[];
    loc_bin = intersect(find(data_1(n)>=binbnds_1(:)),find(data_1(n)<binbnds_1(:))-1);
    %
    if isempty(loc_bin),
        % process out-of-range values:
        % if not truncate: include in end bin
        if (~data_truncate),
            if (data_1(n) < binbnds_1(1)), loc_bin = 1; end
            if (data_1(n) > binbnds_1(end)), loc_bin = nbins_1; end
            if isempty(loc_bin),
                disp(['ERROR: cannot find a bin for the data value: (', data_1(n), ').']);
                return;
            end
            data_n_binned(loc_bin) = data_n_binned(loc_bin) + 1;
            data_bin_n_1(n) = loc_bin;
        else
            data_n_binnedex = data_n_binnedex + 1;
            data_bin_n_1(n) = -1;
        end
    else
        data_n_binned(loc_bin) = data_n_binned(loc_bin) + 1;
        data_bin_n_1(n) = loc_bin;
    end
end
% normalize data
if data_normalize,
    data_n_binned = 100.0 * data_n_binned/(sum(data_n_binned(:)));
end
%
% *** BIN DATA ON Z-AXIS ************************************************ %
%
if (~isempty(binbnds_2)),
    for m = 1:nbins_1,
        % determine which  bin (if any) the data falls within
        loc_bin=[];
        loc_bin_n = find(data_bin_n_1(:) == m);
        for n = 1:length(loc_bin_n),
            loc_n = loc_bin_n(n);
            loc_bin = intersect(find(data_2(loc_n)>=binbnds_2(:)),find(data_2(loc_n)<binbnds_2(:))-1);
            if isempty(loc_bin),
                data_bin_n_2(n) = -1;
            else
                data_n_binned_2d(m,loc_bin) = data_n_binned_2d(m,loc_bin) + 1;
                data_bin_n_2(loc_n) = loc_bin;
            end
        end
        data_n_binned(m) = sum(data_n_binned_2d(m,:));
    end
    % normalize data
    if data_normalize,
        data_n_binned_2d = 100.0 * data_n_binned_2d/(sum(sum(data_n_binned_2d(:,:))));
        data_n_binned = 100.0 * data_n_binned/(sum(data_n_binned(:)));
    end
end
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
fh(2) = axes('Position',[0.10 0.10 0.65 0.70]);
fh(3) = axes('Position',[0.80 0.15 0.10 0.60],'Visible','off');
% define colormap
if (isempty(binbnds_2)),
    cmap = make_cmap(colorbar_name,nbins_1);
else
    cmap = make_cmap(colorbar_name,nbins_2);
end
if (colorbar_inv), cmap = flipdim(cmap,1); end,
colormap(cmap);
% date-stamp plot
set(gcf,'CurrentAxes',fh(1));
text(0.95,0.50,[str_function, ' : ', str_date],'FontName','Arial','FontSize',10,'Rotation',90.0,'HorizontalAlignment','center','VerticalAlignment','top');
%
% *** CREATE DATA HISTOGRAM ********************************************* %
%
set(gcf,'CurrentAxes',fh(2));
hold on;
% set title
if ~isempty(plot_title)
    title(plot_title,'FontSize',18);
else
    title(['Data frequency distribution'],'FontSize',12);
end
% set axes limits -- x
if (axis_xmin >= axis_xmax),
    axis_xmin = binbnds_1(1);
    axis_xmax = binbnds_1(end);
end
if (axis_xmin >= axis_xmax), disp(['ERROR: Failed to autoscale x-axis ... ']); return; end
% set axes limits -- y
if (axis_ymin >= axis_ymax),
    axis_ymin = 0.0;
    axis_ymax = max(data_n_binned(:));
end
if (axis_ymin >= axis_ymax), disp(['ERROR: Failed to autoscale y-axis ... ']); return; end
% set color axis and ticks
set(gca,'PlotBoxAspectRatio',[1.0 0.5 1.0]);
axis([axis_xmin axis_xmax axis_ymin axis_ymax]);
set(gca,'TickDir','out');
%set(gca,'FontSize',10);
%
textsize = 4+round(36/nbins_1);
if textsize > 10, textsize = 10; end
if (~data_truncate),
    set(gca,'XTick',[binbnds_1(:)],'XTickLabel','','FontSize',textsize);
    loc_ticklabel = num2str(binbnds_1(:));
    loc_ticklabel(1,:) = ' ';
    loc_ticklabel(end,:) = ' ';
    set(gca,'XTick',[binbnds_1(:)],'XTickLabel',loc_ticklabel,'FontSize',textsize);
else
    set(gca,'XTick',[binbnds_1(:)],'FontSize',textsize);
end
% label axes
if isempty(plot_dataname_1),
    set(gca,'XLabel',text('String','Environmental parameter','FontSize',15));
else
    set(gca,'XLabel',text('String',[plot_dataname_1],'FontSize',15));
end
if data_normalize,
    set(gca,'YLabel',text('String','% occurrences','FontSize',15));
else
    set(gca,'YLabel',text('String','# of occurrences','FontSize',15));
end
% draw filled rectangles
if (isempty(binbnds_2)),
    for m = 1:nbins_1,
        h = patch([binbnds_1(m) binbnds_1(m) binbnds_1(m+1) binbnds_1(m+1)],[0.0 data_n_binned(m) data_n_binned(m) 0.0],cmap(m,:));
        set(h,'EdgeColor','k');
    end
else
    for m = 1:nbins_1,
        o = 1;
        h = patch([binbnds_1(m) binbnds_1(m) binbnds_1(m+1) binbnds_1(m+1)],[0.0 data_n_binned_2d(m,o) data_n_binned_2d(m,o) 0.0],cmap(o,:));
        set(h,'EdgeColor','k');
        for o = 2:nbins_2,
            h = patch([binbnds_1(m) binbnds_1(m) binbnds_1(m+1) binbnds_1(m+1)],[sum(data_n_binned_2d(m,1:o-1)) sum(data_n_binned_2d(m,1:o)) sum(data_n_binned_2d(m,1:o)) sum(data_n_binned_2d(m,1:o-1))],cmap(o,:));
            set(h,'EdgeColor','k');
        end
    end
end
%
% *** CREATE COLOR BAR ********************************************** %
%
set(gcf,'CurrentAxes',fh(3));
hold on;
set(gca,'XTick',[],'YTick',[]);
%
if (isempty(binbnds_2)),
    loc_nbins = nbins_1;
    loc_binbnds = binbnds_1;
else
    loc_nbins = nbins_2;
    loc_binbnds = binbnds_2;
end
axis([0 1 0 loc_nbins]);
% plot color bar different depending on truncation (or not)
if (~data_truncate && isempty(binbnds_2)),
    loc_nmin = 2;
    loc_nmax = loc_nbins-1;
else
    loc_nmin = 1;
    loc_nmax = loc_nbins;
end
% draw color bar rectangles
if (~data_truncate && isempty(binbnds_2)),
    m = 1;
    h = fill([0.0 0.15 0.3],[m m-1.0 m],cmap(m,:));
    set(h,'LineWidth',0.5);
    set(h,'EdgeColor','k');
end
for m = loc_nmin:loc_nmax,
    h = fill([0.0 0.0 0.3 0.3],[m-1.0 m m m-1.0],cmap(m,:));
    set(h,'LineWidth',0.5);
    set(h,'EdgeColor','k');
end
if (~data_truncate && isempty(binbnds_2)),
    m = loc_nbins;
    h = fill([0.0 0.15 0.3],[m-1.0 m m-1.0],cmap(m,:));
    set(h,'LineWidth',0.5);
    set(h,'EdgeColor','k');
end
% label color bar
for m = loc_nmin:loc_nmax+1,
    str = num2str(loc_binbnds(m));
    textsize = 2+round(80/loc_nbins);
    if textsize > 10, textsize = 10; end
    text(0.40,m-1,str,'FontName','Arial','FontSize',textsize);
end
%
if isempty(plot_dataname_1),
    loc_str = 'Environmental parameter';
else
    if (isempty(binbnds_2)),
        loc_str = plot_dataname_1;
    else
        loc_str = plot_dataname_2;
    end
end
set(gcf,'CurrentAxes',fh(1));
text(0.90,0.50,loc_str,'FontName','Arial','FontSize',15,'Rotation',90.0,'HorizontalAlignment','center','VerticalAlignment','top');
%
% *** END PLOTTING ********************************************* %
%
hold off;
%
% *** PRINT PLOT ******************************************************** %
%
set(gcf,'CurrentAxes',fh(1));
if isempty(str_filename), str_filename = ['colorhistogram']; end
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
%%%disp(['END ...'])
%
% *********************************************************************** %
