function [] = plot_crossplotc(PVECX,PVECY,PVECZ,PSTRX,PSTRY,PSTRZ,POPT,PNAME)
% plot_scatterc
%
%   ***********************************************************************
%   *** plot color-coded cross (scatter) plot *****************************
%   ***********************************************************************
%
%   plot_crossplotc( ... )
%   plots a scatter (x vs. y) plot of 2 data sets provided in vector form,
%   with the option of color-coding the points according to a third
%   data set (also a vector)
%
%   PVECX [VECTOR] (e.g., [1 2 3 4])
%   --> vector of the x axis data
%   PVECY [VECTOR] (e.g., [2 4 6 8])
%   --> vector of the y axis data
%   PVECZ [VECTOR] [OPTIONAL] (e.g., [4 3 2 1])
%   --> vector of the z axis data
%   --> pass and empty vector, i.e. [] for no z (color) axis
%   PSTRY [STRING] [OPTIONAL] (e.g., 'phosphate')
%   --> x-axis label
%   --> leave blank, i.e., '', for no x axis label
%       (although option, passing a string value is advisable)
%   PSTRY [STRING] [OPTIONAL] (e.g., 'oxygen')
%   --> y-axis label
%   --> leave blank, i.e., '', for no y axis label
%       (although option, passing a string value is advisable)
%   PSTRZ [STRING] [OPTIONAL] (e.g., 'depth (m)')
%   --> z-axis (color bar) label
%   --> leave blank, i.e., '', for no z (color) axis label
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
%   14/12/20: CREATED [from scatter code in plot_fields_biogem_3d_i.m]
%   14/12/30: carried with development
%   15/01/13: adjusted color scale call
%             added range error-checking
%
%   ***********************************************************************

% *********************************************************************** %
% *** INITIALIZE PARAMETERS & VARIABLES ********************************* %
% *********************************************************************** %
%
% process dummy parameters
data_x = PVECX;
data_y = PVECY;
data_z = PVECZ;
str_x = PSTRX;
str_y = PSTRY;
str_z = PSTRZ;
str_filename = PNAME;
% load plotting options
if isempty(POPT), POPT='plot_fields_settings'; end
eval(POPT);
%
% *** misc (local) parameters ******************************************* %
%
% set date
str_date = [datestr(date,11), datestr(date,5), datestr(date,7)];
% set function name
str_function = 'plot-scatterc';
%
% *********************************************************************** %

% *********************************************************************** %
% *** PROCESS DATA ****************************************************** %
% *********************************************************************** %
%
% make a copy of the data
loc_x = data_x;
loc_y = data_y;
loc_z = data_z;
% filter data
loc_nan = isnan(loc_x);
loc_x(loc_nan)=[];
loc_y(loc_nan)=[];
if ~isempty(data_z), loc_z(loc_nan)=[]; end
loc_nan = isnan(loc_y);
loc_x(loc_nan)=[];
loc_y(loc_nan)=[];
if ~isempty(data_z), loc_z(loc_nan)=[]; end
loc_nan = isnan(loc_z);
loc_x(loc_nan)=[];
loc_y(loc_nan)=[];
if ~isempty(data_z), loc_z(loc_nan)=[]; end
% determine data limts
loc_xmin = min(loc_x);
loc_xmax = max(loc_x);
loc_ymin = min(loc_y);
loc_ymax = max(loc_y);
loc_zmin = min(loc_z);
loc_zmax = min(loc_z);
% check data range
if ((loc_xmin == loc_xmax) || (loc_ymin == loc_ymax))
    disp(['ERROR: Cannot scale x or y axes (no range in data values). Cross-plot cannot be plotted.']);
    return;
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** PLOT DATA ********************************************************* %
% *********************************************************************** %
%
% *** CREATE AND CONFIGURE FIGURE *************************************** %
%
% initialize figure
figure;
fh(1) = axes('Position',[0 0 1 1],'Visible','off');
if ~isempty(data_z),
    fh(2) = axes('Position',[0.10 0.10 0.80 0.80]);
else
    fh(2) = axes('Position',[0.10 0.10 0.70 0.80]);
end
% define colormap
if ~isempty(data_z),
    cmap = make_cmap(colorbar_name,255);
    if (colorbar_inv == 'y'), cmap = flipdim(cmap,1); end,
    colormap(cmap);
    caxis([min(data_z) max(data_z)]);
end
% date-stamp plot
set(gcf,'CurrentAxes',fh(1));
text(0.95,0.50,[str_function, ' / ', 'on: ', str_date],'FontName','Arial','FontSize',8,'Rotation',90.0,'HorizontalAlignment','center','VerticalAlignment','top');
%
% *** PLOT DATA ********************************************************* %
%
set(gcf,'CurrentAxes',fh(2));
hold on;
if ~isempty(data_z),
    scatter(data_x,data_y,data_size,data_z,'filled');
    xlabel([strrep(str_x,'_','-')]);
    ylabel([strrep(str_y,'_','-')]);
else
    scatter(data_x,data_y,data_size);
    xlabel([strrep(str_x,'_','-')]);
    ylabel([strrep(str_y,'_','-')]);
end
axis([loc_xmin loc_xmax loc_ymin loc_ymax]);
if ~isempty(data_z),
    hb = colorbar;
    set(get(hb,'ylabel'),'string',str_z);
end
if ~isempty(plot_title)
    title(plot_title,'FontSize',18);
else
    title(['Data ID: ',strrep(str_y,'_','-'),' vs. ', strrep(str_x,'_','-')],'FontSize',12);
end
%
% *** ADD REGRESSION **************************************************** %
%
if (data_stats=='y'),
    % add regression line & calc R2
    [p,S] = polyfit(loc_x,loc_y,data_fit_n);
    loc_ymean = mean(loc_y);
    loc_yf = polyval(p,loc_x);
    loc_SStot = sum((loc_y - loc_ymean).^2);
    loc_SSres = sum ((loc_y - loc_yf).^2);
    loc_R2 = 1 - loc_SSres/loc_SStot;
    loc_n = length(loc_z);
    if (data_fit_n > 1),
        loc_x = [loc_xmin:(loc_xmax-loc_xmin)/10:loc_xmax];
        loc_y = p(3)+p(2)*loc_x+p(1)*loc_x.^2;
        plot(loc_x,loc_y,'Color','k','LineWidth',1.0);
        loc_str1 = ['y = ' num2str(p(3)) ' + ' num2str(p(2)) '*x + ' num2str(p(1)) '*x^2'];
    else
        line([loc_xmin loc_xmax], [p(2)+p(1)*loc_xmin p(2)+p(1)*loc_xmax],'Color','k','LineWidth',1.0);
        loc_str1 = ['y = ' num2str(p(2)) ' + ' num2str(p(1)) '*x'];
    end
    loc_str2 = ['R2 = ' num2str(loc_R2) ' : n = ' num2str(loc_n)];
    % add 1:1 line
    line([min([loc_xmin loc_ymin]) max([loc_xmax loc_ymax])], [min([loc_xmin loc_ymin]) max([loc_xmax loc_ymax])],'Color','k','LineWidth',1.0,'LineStyle','--');
    % find current x,y limits
    loc_xlim = xlim;
    loc_ylim = ylim;
    loc_xmin = loc_xlim(1);
    loc_xmax = loc_xlim(2);
    loc_ymin = loc_ylim(1);
    loc_ymax = loc_ylim(2);
    % add text labels
    if ( ((p(2) > 0.0) && (data_fit_n > 1)) || ((p(1) > 0.0) && (data_fit_n == 1)) ),
        % positive slope
        text(loc_xmin+0.05*(loc_xmax-loc_xmin),loc_ymin+0.95*(loc_ymax-loc_ymin),loc_str1,'FontName','Arial','FontSize',11,'HorizontalAlignment','left','VerticalAlignment','middle');
        text(loc_xmin+0.05*(loc_xmax-loc_xmin),loc_ymin+0.85*(loc_ymax-loc_ymin),loc_str2,'FontName','Arial','FontSize',10,'HorizontalAlignment','left','VerticalAlignment','middle');
    else
        % assume negative slope
        text(loc_xmin+0.95*(loc_xmax-loc_xmin),loc_ymin+0.95*(loc_ymax-loc_ymin),loc_str1,'FontName','Arial','FontSize',11,'HorizontalAlignment','right','VerticalAlignment','middle');
        text(loc_xmin+0.95*(loc_xmax-loc_xmin),loc_ymin+0.85*(loc_ymax-loc_ymin),loc_str2,'FontName','Arial','FontSize',10,'HorizontalAlignment','right','VerticalAlignment','middle');
    end
end
%
% *** END FIGURE ******************************************************** %
%
hold off;
%
% *** PRINT PLOT ******************************************************** %
%
if isempty(str_filename), str_filename = ['crossplot']; end
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
%disp(['END ...'])
%
% *********************************************************************** %
