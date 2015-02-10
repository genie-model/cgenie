function [] = plot_timeseries_biogem(PEXP1,PEXP2,PTMIN,PTMAX,PDATA1,PDATA1N,PDATA2,PDATA2N,PDATA3,PDATA3N,POPT,PNAME)
% plot_timeseries_biogem
%
%   ***********************************************************************
%
%   ***********************************************************************
%   *** PLOT TIME-SERIES OUTPUT *******************************************
%   ***********************************************************************
%
%   plot_timeseries_biogem()
%
%   plot_timeseries_biogem()
%   takes shit loads of arguments (10 actually);
%
%   PEXP1 [STRING] (e.g. 'preindustrial_spinup')
%   --> the (first) experiment name
%   PEXP2 [STRING] [OPTIONAL] (e.g. 'enhanced_export')
%   --> the name of a 2nd, optional experiment
%   --> leave PEXP2 blank, i.e., '', for no second experiment
%   PTMIN [REAL] (e.g. 0.0)
%   --> minimum plotted time
%   PTMAX [REAL] (e.g. 1000.0)
%   --> maximum plotted time
%   PDATA1 [STRING] (e.g. 'misc_surpH')
%   --> time-series variable name for additional data to plot
%       (omit the 'biogem_series_' and '.res' parts of the filename)
%   --> leave blank, i.e., '', for no additional data panel
%   PDATA1N [INTEGER] (e.g. 3)
%   --> the column of the time-series file to plot
%   PDATA2 [STRING] (e.g. 'ocn_DIC_13C')
%   --> time-series variable name for additional data to plot
%       (omit the 'biogem_series_' and '.res' parts of the filename)
%   --> leave blank, i.e., '', for no additional data panel
%   PDATA2N [INTEGER] (e.g. 3)
%   --> the column of the time-series file to plot
%   PDATA3 [STRING] (e.g. 'ocn_temp')
%   --> time-series variable name for additional data to plot
%       (omit the 'biogem_series_' and '.res' parts of the filename)
%   --> leave blank, i.e., '', for no additional data panel
%   PDATA3N [INTEGER] (e.g. 3)
%   --> the column of the time-series file to plot
%   POPT [STRING] (e.g., 'plot_timeseries_settings_2')
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
%   14/05/10: CREATED [modified from PETM-in-a-day version]
%   14/06/17: changed to 3 optional panels
%   14/06/18: added data scaling
%   14/06/20: added log10 timescale option
%             removed commented out tmp code
%             added optional inversion analysis
%             added alt plotting formats
%             added help text
%   14/08/11: developed inversion analysis
%   14/10/28: added fudge to make xtick work for all panel combinations
%             + some general reorganisation
%   14/10/29: fixed axis scaling bug
%   14/11/09: added units to optional panel y-axes
%   14/11/09: auto plot format
%             added sedcorenv plotting capability
%             added alt filename
%             added alt time scale units
%   14/11/10: [minor]
%             change to plot title
%             general debugging on inversion analysis option
%   14/11/17: fixed data-scaling bug
%   14/11/19: added overlay data plotting
%   14/11/20: test for relevant directopries
%   15/01/10: edited help text
%             + added option for 2nd experiment for 
%               anomoly plotting in the optional data panels
%             fixed x-axis label bug (on 1st optional panel)
%
%   ***********************************************************************

% *********************************************************************** %
% *** INITIALIZE PARAMETERS & VARIABLES ********************************* %
% *********************************************************************** %
%
% close plot windows
close all;
% load plotting options
if isempty(POPT), POPT='plot_timeseries_settings'; end
eval(POPT);
% set dummy variables
expid1 = PEXP1;
expid2 = PEXP2;
data1_name = PDATA1;
data1_n = PDATA1N;
if (~isempty(data1_name) && ischar(data1_n)),
    disp(['ERROR: column number of data #1 must be entered as a number, not string.']);
    return;
end
data2_name = PDATA2;
data2_n = PDATA2N;
if (~isempty(data2_name) && ischar(data2_n)),
    disp(['ERROR: column number of data #2 must be entered as a number, not string.']);
    return;
end
data3_name = PDATA3;
data3_n = PDATA3N;
if (~isempty(data3_name) && ischar(data3_n)),
    disp(['ERROR: column number of data #3 must be entered as a number, not string.']);
    return;
end
axis_tmin = PTMIN;
axis_tmax = PTMAX;
if (ischar(axis_tmin) || ischar(axis_tmax)),
    disp(['ERROR: min/max time must be entered as a number, not string.']);
    return;
end
altfilename = PNAME;
% define grey color
color_g = [0.75 0.75 0.75];
% set date
str_date = [datestr(date,11), datestr(date,5), datestr(date,7)];
% set function name
str_function = 'plot-timeseries-biogem';
% set filename
str_filename = [expid1];
% plot format
if ~isempty(plot_format), plot_format_old='n'; end
% plotting paths
addpath(library_path1);
if (plot_format_old == 'n'),
    addpath(library_path2);
    addpath(library_path3);
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** LOAD DATA ********************************************************* %
% *********************************************************************** %
%
% test for relevant directories
data_dir = [data_path '/' expid1];
if (exist(data_dir, 'dir') == 0)
    disp(['ERROR: Experiment cannot be found.']);
    if (exist(data_path, 'dir') == 0)
        disp(['INFO: Path: ' data_path ' cannot be found.']);
    else
        disp(['INFO: Path: ' data_path ' exists.']);
        disp(['INFO: Experiment name: ' expid1 ' cannot be found.']);
    end
    return;
end
% test for an isotope-enabled experiment
data_file = [data_path '/' expid1 '/biogem/biogem_series_atm_pCO2_13C.res'];
if (exist(data_file, 'file') ~= 2)
    disp(['ERROR: The experiment needs to be 13C-enabled in order to use this plotting function.']);
    return;
end
% load basic data
data_pCO2     = load([data_path '/' expid1 '/biogem/biogem_series_atm_pCO2.res'],'ascii');
data_pCO2_13C = load([data_path '/' expid1 '/biogem/biogem_series_atm_pCO2_13C.res'],'ascii');
data_Tatm     = load([data_path '/' expid1 '/biogem/biogem_series_atm_temp.res'],'ascii');
data_seaice   = load([data_path '/' expid1 '/biogem/biogem_series_misc_seaice.res'],'ascii');
% set time units
switch plot_tunits
    case 'kyr'
        data_pCO2(:,1)     = data_pCO2(:,1)*1.0E-3;
        data_pCO2_13C(:,1) = data_pCO2_13C(:,1)*1.0E-3;
        data_Tatm(:,1)     = data_Tatm(:,1)*1.0E-3;
        data_seaice(:,1)   = data_seaice(:,1)*1.0E-3;
    case 'Myr'
        data_pCO2(:,1)     = data_pCO2(:,1)*1.0E-6;
        data_pCO2_13C(:,1) = data_pCO2_13C(:,1)*1.0E-6;
        data_Tatm(:,1)     = data_Tatm(:,1)*1.0E-6;
        data_seaice(:,1)   = data_seaice(:,1)*1.0E-6;
end
% load optional data file #1
if ~isempty(data1_name)
    data1_file = [data_path '/' expid1 '/biogem/biogem_series_' data1_name '.res'];
    data1_fileALT = [data_path '/' expid1 '/sedgem/sedcoreenv_' data1_name '.res'];
    if (exist(data1_file, 'file') == 2)
        data1 = load(data1_file,'ascii');
        [m,n] = size(data1);
        if (data1_n > n || data1_n < 2),
            disp(['ERROR: You must chose a column number between 2 and ' n ' in BIOGEM time-series file: ', data1_file]);
            return;
        end
        % if 2nd experiment => load file and create anomoly
        if ~isempty(expid2)
            loc_data1_file = [data_path '/' expid2 '/biogem/biogem_series_' data1_name '.res'];
            if (exist(loc_data1_file, 'file') == 2)
                loc_data1 = load(loc_data1_file,'ascii');
                if (length(loc_data1(:,1)) ~= length(data1(:,1))),
                    loc_data = interp1(loc_data1(:,1),loc_data1(:,2:end),data1(:,1),'cubic');
                    loc_data1 = [data1(:,1), loc_data];
                end
                data1(:,2:end) = loc_data1(:,2:end) - data1(:,2:end);
            else
                disp(['ERROR: BIOGEM time-series file ', loc_data1_file, ' does not exist.']);
                return;
            end
        end
    elseif (exist(data1_fileALT, 'file') == 2)
        data1 = load(data1_fileALT,'ascii');
        [m,n] = size(data1);
        % NOTE: convert kyr to yr
        data1(:,1) = 1000.0*data1(:,1);
    else
        disp(['ERROR: BIOGEM time-series file ', data1_file, ' does not exist.']);
        disp(['(or SEDGEM sedcorenv file ', data1_fileALT, ' does not exist)']);
        return;
    end
    data1(:,n) = axis_data1_scale*data1(:,data1_n);
    axis_data1_min = axis_data1_min;
    axis_data1_max = axis_data1_max;
    if (opt_invanalysis), disp(['WARNING: selection of opt_invanalysis=true option over-rides use of chosen time-series file: ', data1_file]); end
    switch plot_tunits
        case 'kyr'
            data1(:,1) = data1(:,1)*1.0E-3;
        case 'Myr'
            data1(:,1) = data1(:,1)*1.0E-6;
    end
    if ~isempty(overlaydata1_file),
        if (exist(overlaydata1_file, 'file') == 2)
            overlaydata1 = load(overlaydata1_file,'ascii');
        else
            disp(['ERROR: overlay data file ', overlaydata1_file, ' does not exist.']);
            return;
        end
    end
else
    data1 = [];
    overlaydata1_file = [];
end
% load optional data file #2
if ~isempty(data2_name)
    data2_file = [data_path '/' expid1 '/biogem/biogem_series_' data2_name '.res'];
    data2_fileALT = [data_path '/' expid1 '/sedgem/sedcoreenv_' data2_name '.res'];
    if (exist(data2_file, 'file') == 2)
        data2 = load(data2_file,'ascii');
        [m,n] = size(data2);
        if (data2_n > n || data2_n < 2),
            disp(['ERROR: You must chose a column number between 2 and ' n ' in BIOGEM time-series file: ', data2_file]);
            return;
        end
        % if 2nd experiment => load file and create anomoly
        if ~isempty(expid2)
            loc_data2_file = [data_path '/' expid2 '/biogem/biogem_series_' data2_name '.res'];
            if (exist(loc_data2_file, 'file') == 2)
                loc_data2 = load(loc_data2_file,'ascii');
                if (length(loc_data2(:,1)) ~= length(data2(:,1))),
                    loc_data = interp1(loc_data2(:,1),loc_data2(:,2:end),data2(:,1),'cubic');
                    loc_data2 = [data2(:,1), loc_data];
                end
                data2(:,2:end) = loc_data2(:,2:end) - data2(:,2:end);
            else
                disp(['ERROR: BIOGEM time-series file ', loc_data2_file, ' does not exist.']);
                return;
            end
        end
    elseif (exist(data2_fileALT, 'file') == 2)
        data2 = load(data2_fileALT,'ascii');
        [m,n] = size(data2);
        % NOTE: convert kyr to yr
        data2(:,1) = 1000.0*data2(:,1);
    else
        disp(['ERROR: BIOGEM time-series file ', data2_file, ' does not exist.']);
        disp(['(or SEDGEM sedcorenv file ', data2_fileALT, ' does not exist)']);
        return;
    end
    data2(:,data2_n) = axis_data2_scale*data2(:,data2_n);
    axis_data2_min = axis_data2_min;
    axis_data2_max = axis_data2_max;
    if (opt_invanalysis), disp(['WARNING: selection of opt_invanalysis=true option over-rides use of chosen time-series file: ', data2_file]); end
    switch plot_tunits
        case 'kyr'
            data2(:,1) = data2(:,1)*1.0E-3;
        case 'Myr'
            data2(:,1) = data2(:,1)*1.0E-6;
    end
    if ~isempty(overlaydata2_file),
        if (exist(overlaydata2_file, 'file') == 2)
            overlaydata2 = load(overlaydata2_file,'ascii');
        else
            disp(['ERROR: overlay data file ', overlaydata2_file, ' does not exist.']);
            return;
        end
    end
else
    data2 = [];
    overlaydata2_file = [];
end
% load optional data file #3
if ~isempty(data3_name)
    data3_file = [data_path '/' expid1 '/biogem/biogem_series_' data3_name '.res'];
    data3_fileALT = [data_path '/' expid1 '/sedgem/sedcoreenv_' data3_name '.res'];
    if (exist(data3_file, 'file') == 2)
        data3 = load(data3_file,'ascii');
        [m,n] = size(data3);
        if (data3_n > n || data3_n < 2),
            disp(['ERROR: You must chose a column number between 2 and ' n ' in BIOGEM time-series file: ', data3_file]);
            return;
        end
        % if 2nd experiment => load file and create anomoly
        if ~isempty(expid2)
            loc_data3_file = [data_path '/' expid2 '/biogem/biogem_series_' data3_name '.res'];
            if (exist(loc_data3_file, 'file') == 2)
                loc_data3 = load(loc_data3_file,'ascii');
                if (length(loc_data3(:,1)) ~= length(data3(:,1))),
                    loc_data = interp1(loc_data3(:,1),loc_data3(:,2:end),data3(:,1),'cubic');
                    loc_data3 = [data3(:,1), loc_data];
                end
                data3(:,2:end) = loc_data3(:,2:end) - data3(:,2:end);
            else
                disp(['ERROR: BIOGEM time-series file ', loc_data3_file, ' does not exist.']);
                return;
            end
        end
    elseif (exist(data3_fileALT, 'file') == 2)
        data3 = load(data3_fileALT,'ascii');
        [m,n] = size(data3);
        % NOTE: convert kyr to yr
        data3(:,1) = 1000.0*data3(:,1);
    else
        disp(['ERROR: BIOGEM time-series file ', data3_file, ' does not exist.']);
        disp(['(or SEDGEM sedcorenv file ', data3_fileALT, ' does not exist)']);
        return;
    end
    data3(:,data3_n) = axis_data3_scale*data3(:,data3_n);
    axis_data3_min = axis_data3_min;
    axis_data3_max = axis_data3_max;
    if (opt_invanalysis), disp(['WARNING: selection of opt_invanalysis=true option over-rides use of chosen time-series file: ', data3_file]); end
    switch plot_tunits
        case 'kyr'
            data3(:,1) = data3(:,1)*1.0E-3;
        case 'Myr'
            data3(:,1) = data3(:,1)*1.0E-6;
    end
    if ~isempty(overlaydata3_file),
        if (exist(overlaydata3_file, 'file') == 2)
            overlaydata3 = load(overlaydata3_file,'ascii');
        else
            disp(['ERROR: overlay data file ', overlaydata3_file, ' does not exist.']);
            return;
        end
    end
else
    data3 = [];
    overlaydata3_file = [];
end
%
% *** DIAGNOSED EMISSIONS FORCING DATA ********************************** &
%
if opt_invanalysis,
    % determine for type of forcing output and load
    file_FpCO2 = [data_path '/' expid1 '/biogem/biogem_series_diag_misc_inversion_forcing_FpCO2.res'];
    file_FpCO2_13C = [data_path '/' expid1 '/biogem/biogem_series_diag_misc_inversion_forcing_FpCO2_13C.res'];
    file_FDIC = [data_path '/' expid1 '/biogem/biogem_series_diag_misc_inversion_forcing_FDIC.res'];
    file_FDIC_13C = [data_path '/' expid1 '/biogem/biogem_series_diag_misc_inversion_forcing_FDIC_13C.res'];
    %
    string_forcing = 'xxx';
    %test for and load atm forcing
    if (exist(file_FpCO2, 'file') == 2)
        data_FpCO2 = load(file_FpCO2,'ascii');
        if (abs(sum(data_FpCO2(:,2))) > 1.0E15*plot_FCthreshold/12.0)
            string_forcing = 'atm';
            data_FCO2 = data_FpCO2;
            data_FpCO2_13C = load(file_FpCO2_13C,'ascii');
            data_FCO2_13C = data_FpCO2_13C(:,3);
        end
    end
    % test for and load ocn forcing
    if exist(file_FDIC, 'file') == 2
        data_FDIC = load(file_FDIC,'ascii');
        if (abs(sum(data_FDIC(:,2))) > 1.0E15*plot_FCthreshold/12.0)
            string_forcing = 'ocn';
            data_FCO2 = data_FDIC;
            data_FDIC_13C = load(file_FDIC_13C,'ascii');
            data_FCO2_13C = data_FDIC_13C(:,4);
        end
    end
    if (~exist(file_FpCO2, 'file') == 2) && (~exist(file_FDIC, 'file') == 2),
        disp(['ERROR: Diagnosed emissions forcing data does not exist.']);
        return;
    end
    if (exist('data_FCO2','var') ~= 1),
        disp(['ERROR: Non-zero cumulative diagnosed emissions do not exist.']);
        return;
    end
end
%
% *** PROCESS DATA ****************************************************** %
%
% set length justin case ...
n_data = length(data_pCO2(:,1));
% set time-axis
% NOTE: correct to nearest integer
if (axis_tmin == axis_tmax),
    axis_tmin = int32(data_pCO2(1,1)-0.001);
    axis_tmax = int32(data_pCO2(end,1)+0.001);
end
if opt_log10,
    axis_tmin = log10(max(1.0,axis_tmin));
    axis_tmax = log10(axis_tmax);
end
%
% *** process CO2 inversion data ***
if opt_invanalysis,
    n = 1;
    data_FCO2_t(1) = data_FCO2(n,1);
    data_FCO2_sum(1) = data_FCO2(n,2);
    for n = 2:n_data,
        data_FCO2_t(n) = data_FCO2(n,1);
        data_FCO2_dt(n) = data_FCO2(n,1) - data_FCO2(n-1,1);
        data_FCO2_sum(n) = data_FCO2_sum(n-1) + data_FCO2(n,2);
    end
    % guess first interval
    data_FCO2_dt(1) = data_FCO2_dt(2);
    % scale rate
    data_FCO2_dF = data_FCO2(:,2)./data_FCO2_dt(:);
    % adjust units
    data_FCO2_sum(:) = 12.0*data_FCO2_sum(:)/1.0E15;
    data_FCO2_dF(:) = 12.0*data_FCO2_dF(:)/1.0E15;
    % filter d13C to remove data at trivial emissions rates
    % NOTE: a low threshold is set, although in practice, the smallest
    %       non-zero emissions rate might be e.g. 1/48 the max
    %       (currently set to 0.0 -- could be NaN)
    data_FCO2_13C(find(abs(data_FCO2_dF) < 1.0E-6*(max(abs(data_FCO2_dF))))) = 0.0;
    %
    % *** interpolate d13C ***
    % create interpolated timeline and bin boundaries in time
    interp_FCO2_t = [min(data_FCO2_t):plot_interp_Dt:max(data_FCO2_t)]';
    bins_FCO2_t = [min(data_FCO2_t):plot_bins_Dt:max(data_FCO2_t)]';
    % interpolate emissions and d13C onto interpolated timeline
    interp_FCO2_dF = interp1(data_FCO2_t,data_FCO2_dF,interp_FCO2_t,'nearest');
    interp_FCO2_13C = interp1(data_FCO2_t,data_FCO2_13C,interp_FCO2_t,'nearest');
    % re-bin emissions and 13C
    % NOTE: normalize d13C to emissions
    [binn_FCO2_dF bindata_FCO2_dF binctrs_FCO2] = make_hist([interp_FCO2_t interp_FCO2_dF],bins_FCO2_t,1);
    [binn_FCO2_13C bindata_FCO2_13C binctrs_FCO2] = make_hist([interp_FCO2_t interp_FCO2_dF.*interp_FCO2_13C],bins_FCO2_t,1);
    bindata_FCO2_13C = bindata_FCO2_13C./bindata_FCO2_dF;
    %
    bindata_FCO2_13C(find(abs(bindata_FCO2_dF(:)) < plot_FCthreshold)) = NaN;
    bindata_FCO2_dF(find(abs(bindata_FCO2_dF(:)) < plot_FCthreshold)) = NaN;
    %
    % adjust time time-scale but keep e.g. dt in units of yr-1
    switch plot_tunits
        case 'kyr'
            data_FCO2_t(:) = data_FCO2_t(:)*1.0E-3;
            bins_FCO2_t(:) = bins_FCO2_t(:)*1.0E-3;
            binctrs_FCO2(:) = binctrs_FCO2(:)*1.0E-3;
        case 'Myr'
            data_FCO2_t(:) = data_FCO2_t(:)*1.0E-6;
            bins_FCO2_t(:) = bins_FCO2_t(:)*1.0E-6;
            binctrs_FCO2(:) = binctrs_FCO2(:)*1.0E-6;
    end
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** PLOT DATA ********************************************************* %
% *********************************************************************** %
%
% *** DEFINE FIGURE ***************************************************** %
%
% create figure
scrsz = get(0,'ScreenSize');
figure('Position',[((1.0-plot_dscrsz)/2)*plot_dscrsz*scrsz(3) ((1.0-plot_dscrsz)/2)*plot_dscrsz*scrsz(4) 0.50*plot_dscrsz*scrsz(3) 0.90*plot_dscrsz*scrsz(4)]);
clf;
% define plotting regions
fh(1) = axes('Position',[0 0 1 1],'Visible','off');
fh(2) = axes('Position',[0.00 0.00 1.00 0.05],'Visible','off');
if (~isempty(data1)),
    if (~isempty(data2)),
        if (~isempty(data3)),
            fh(3) = axes('Position',[0.10 0.10 0.70 0.14],'XTickLabel',[]);
        end
        fh(4) = axes('Position',[0.10 0.26 0.70 0.14],'XTickLabel',[]);
    end
    fh(5) = axes('Position',[0.10 0.42 0.70 0.14],'XTickLabel',[]);
elseif opt_invanalysis,
    fh(3) = axes('Position',[0.10 0.10 0.70 0.14],'XTickLabel',[]);
    fh(4) = axes('Position',[0.10 0.26 0.70 0.14],'XTickLabel',[]);
    fh(5) = axes('Position',[0.10 0.42 0.70 0.14],'XTickLabel',[]);
end
fh(6) = axes('Position',[0.10 0.58 0.70 0.14],'XTickLabel',[]);
fh(7) = axes('Position',[0.10 0.74 0.70 0.14],'XTickLabel',[]);
fh(8) = axes('Position',[0.90 0.00 0.10 1.00],'Visible','off');
fh(9) = axes('Position',[0.00 0.90 0.70 0.10],'Visible','off');
% date-stamp plot
set(gcf,'CurrentAxes',fh(1));
text(0.95,0.50,[str_function, ' : ', strrep(expid1,'_',' '), ' : ', str_date],'FontName','Arial','FontSize',10,'Rotation',90.0,'HorizontalAlignment','center','VerticalAlignment','top');
%
set(gcf,'CurrentAxes',fh(9));
if ~isempty(plot_title)
    str_title = plot_title;
    str_title_sze = 15;
elseif ~isempty(altfilename)
    str_title = strrep(altfilename,'_',' ');
    str_title_sze = 15;
else
    str_title = ['Time-series: ',strrep(expid1,'_',' ')];
    str_title_sze = 12;
end
text(0.15,0.25,str_title,'FontName','Arial','FontSize',str_title_sze);
%
switch plot_tunits
    case 'kyr'
        str_tunits = 'kyr';
    case 'Myr'
        str_tunits = 'Myr';
    otherwise
        str_tunits = 'yr';
end
%
% *** PLOT PANEL #1 ***************************************************** %
%
set(gcf,'CurrentAxes',fh(7));
hold on;
% create plot on first y-axis
% set axes
if (axis_pCO2min == axis_pCO2max),
    axis_pCO2min = 1.0E6*min(data_pCO2(:,3));
    axis_pCO2max = 1.0E6*max(data_pCO2(:,3));
end
if (axis_pCO2min == axis_pCO2max), disp(['ERROR: Failed to autoscale pCO2 ... ']); return; end
%
if opt_log10,
    hl1 = line(log10(data_pCO2(:,1)),1.0E6*data_pCO2(:,3),'Color','r','LineWidth',1.0);
else
    hl1 = line(data_pCO2(:,1),1.0E6*data_pCO2(:,3),'Color','r','LineWidth',1.0);
end
ax1 = gca;
set(ax1,'XColor','k','TickDir','out','XTickLabel','');
set(ax1,'XLim',[axis_tmin axis_tmax]);
set(ax1,'YColor','r','TickDir','out');
set(ax1,'YLim',[axis_pCO2min, axis_pCO2max]);
set(ax1,'YLabel',text('String',['pCO_{2} (\muatm)'],'FontSize',10));
% create 2nd y-axis
% set axes
if (axis_d13Cmin == axis_d13Cmax),
    axis_d13Cmin = min(data_pCO2_13C(:,3));
    axis_d13Cmax = max(data_pCO2_13C(:,3));
end
if (axis_d13Cmin == axis_d13Cmax), disp(['ERROR: Failed to autoscale \delta^{13}C of pCO_{2} ... ']); return; end
%
ax2 = axes('Position',get(ax1,'Position'),'Color','none','YAxisLocation','right','YColor','b');
if opt_log10,
    hl2 = line(log10(data_pCO2_13C(:,1)),data_pCO2_13C(:,3),'Color','b','LineWidth',1.0,'Parent',ax2);
else
    hl2 = line(data_pCO2_13C(:,1),data_pCO2_13C(:,3),'Color','b','LineWidth',1.0,'Parent',ax2);
end
set(ax2,'XAxisLocation','bottom','XColor','k','XTick',[],'XTickLabel','');
set(ax2,'XLim',[axis_tmin axis_tmax]);
set(ax2,'YLim',[axis_d13Cmin, axis_d13Cmax]);
set(ax2,'YLabel',text('String',['\delta^{13}C (' char(8240) ')'],'FontSize',10));
%
% *** PLOT PANEL #2 ***************************************************** %
%
set(gcf,'CurrentAxes',fh(6));
hold on;
% create plot on first y-axis
% set axes
if (axis_Tatmmin == axis_Tatmmax),
    axis_Tatmmin = min(data_Tatm(:,2));
    axis_Tatmmax = max(data_Tatm(:,2));
end
if (axis_Tatmmin == axis_Tatmmax), disp(['ERROR: Failed to autoscale T(atm) ... ']); return; end
%
if opt_log10,
    hl1 = line(log10(data_Tatm(:,1)),data_Tatm(:,2),'Color','r','LineWidth',1.0);
else
    hl1 = line(data_Tatm(:,1),data_Tatm(:,2),'Color','r','LineWidth',1.0);
end
ax1 = gca;
set(ax1,'XColor','k','TickDir','out','XTickLabel','');
set(ax1,'XLim',[axis_tmin axis_tmax]);
set(ax1,'YColor','r','TickDir','out');
set(ax1,'YLim',[axis_Tatmmin axis_Tatmmax]);
set(ax1,'YLabel',text('String',['T_{atm} (' char(176) 'C)'],'FontSize',10));
% create 2nd y-axis
if (axis_icemin == axis_icemax),
    axis_icemin = min(data_seaice(:,3));
    axis_icemax = max(data_seaice(:,3));
end
if (axis_icemin == axis_icemax), disp(['ERROR: Failed to autoscale seaice cover ... ']); return; end
% set 2nd axes
ax2 = axes('Position',get(ax1,'Position'),'Color','none','YAxisLocation','right','YColor','b');
if opt_log10,
    hl2 = line(log10(data_seaice(:,1)),data_seaice(:,3),'Color','b','LineWidth',1.0,'Parent',ax2);
else
    hl2 = line(data_seaice(:,1),data_seaice(:,3),'Color','b','LineWidth',1.0,'Parent',ax2);
end
set(ax2,'YLim',[axis_icemin axis_icemax]);
set(ax2,'YLabel',text('String',['Seaice (%)'],'FontSize',10));
% create x-axis (depending on number of panels)
if (isempty(data1) && ~opt_invanalysis)
    set(ax2,'XAxisLocation','bottom','XColor','k','TickDir','out','XTickLabelMode','auto');
    set(ax2,'XLim',[axis_tmin axis_tmax]);
    if opt_log10,
        set(ax2,'XLabel',text('String',['log_{10}(time) (' str_tunits ')'],'FontSize',15));
    else
        set(ax2,'XLabel',text('String',['time (' str_tunits ')'],'FontSize',15));
    end
else
    set(ax2,'XAxisLocation','bottom','XColor','k','TickDir','out','XTick',[],'XTickLabel','');
    set(ax2,'XLim',[axis_tmin axis_tmax]);
end
%
% *** PLOT PANEL #3 (OPTIONAL DATA #1) ********************************** %
%
if (~isempty(data1))
    set(gcf,'CurrentAxes',fh(5));
    hold on;
    % set axes limits
    if (axis_data1_min >= axis_data1_max),
        axis_data1_min = min(data1(:,data1_n));
        axis_data1_max = max(data1(:,data1_n));
    end
    if (axis_data1_min >= axis_data1_max), disp(['ERROR: Failed to autoscale data1 ... ']); return; end
    % plot data
    if opt_log10,
        hl1 = line(log10(data1(:,1)),data1(:,data1_n),'Color','k','LineWidth',1.0);
        if (opt_plotpoints), hp1 = scatter(log10(data1(:,1)),data1(:,data1_n),'o','Filled','Sizedata',plot_datasize,'MarkerFaceColor','y','MarkerEdgeColor','k'); end
        if (~isempty(overlaydata1_file)), hp1 = scatter(log10(overlaydata1(:,1)),overlaydata1(:,2),'o','Filled','Sizedata',plot_datasize,'MarkerFaceColor','y','MarkerEdgeColor','k'); end
    else
        hl1 = line(data1(:,1),data1(:,data1_n),'Color','k','LineWidth',1.0);
        if (opt_plotpoints), hp1 = scatter(data1(:,1),data1(:,data1_n),'o','Filled','Sizedata',plot_datasize,'MarkerFaceColor','y','MarkerEdgeColor','k'); end
        if (~isempty(overlaydata1_file)), hp1 = scatter(overlaydata1(:,1),overlaydata1(:,2),'o','Filled','Sizedata',plot_datasize,'MarkerFaceColor','y','MarkerEdgeColor','k'); end
    end
elseif(opt_invanalysis)
    set(gcf,'CurrentAxes',fh(5));
    hold on;
    % set axes limits
    if (axis_data1_min >= axis_data1_max),
        axis_data1_min = min(data_FCO2_dF(:));
        axis_data1_max = max(data_FCO2_dF(:));
    end
    if (axis_data1_min >= axis_data1_max), disp(['ERROR: Failed to autoscale data1 ... ']); return; end
    % create plot on first y-axis
    if (opt_rebinned),
        hl1 = bar(binctrs_FCO2(:),bindata_FCO2_dF(:),1.0,'hist');
    else
        hl1 = bar(data_FCO2_t(:),data_FCO2_dF(:),1.0,'hist');
    end
    delete(findobj('marker','*'))
end
% AXES
if (~isempty(data1) || opt_invanalysis),
    ax1 = gca;
    % create and label y-axis
    set(ax1,'YColor','k','TickDir','out');
    set(ax1,'YLim',[axis_data1_min axis_data1_max]);
    if (~isempty(data1)),
        if (isempty(plot_data1_title)),
            plot_data1_title = data1_name;
            plot_data1_title(find(plot_data1_title(:)=='_')) = '.';
            set(ax1,'YLabel',text('String',[plot_data1_title ' (' plot_data1_units ')'],'FontSize',10));
        else
            set(ax1,'YLabel',text('String',[plot_data1_title ' (' plot_data1_units ')'],'FontSize',12));
        end
    else
        set(ax1,'YLabel',text('String','Emissions (PgC yr^{-1})','FontSize',10));
    end
    % create and label x-axis depending on number of panels
    if (isempty(data2) && ~opt_invanalysis),
        set(ax1,'XColor','k','TickDir','out','XTickLabelMode','auto');
        set(ax1,'XLim',[axis_tmin axis_tmax]);
        if opt_log10,
            set(ax1,'XLabel',text('String',['log_{10}(time) (' str_tunits ')'],'FontSize',15));
        else
            set(ax1,'XLabel',text('String',['time (' str_tunits ')'],'FontSize',15));
        end
    else
        set(ax1,'XColor','k','TickDir','out','XTickLabel','');
        set(ax1,'XLim',[axis_tmin axis_tmax]);
    end
end
%
% *** PLOT PANEL #4 (OPTIONAL DATA #2) ********************************** %
%
if (~isempty(data2))
    set(gcf,'CurrentAxes',fh(4));
    hold on;
    % set axes limits
    if (axis_data2_min >= axis_data2_max),
        axis_data2_min = min(data2(:,data2_n));
        axis_data2_max = max(data2(:,data2_n));
    end
    if (axis_data2_min >= axis_data2_max), disp(['ERROR: Failed to autoscale data2 ... ']); return; end
    % plot data
    if opt_log10,
        hl1 = line(log10(data2(:,1)),data2(:,data2_n),'Color','k','LineWidth',1.0);
        if (opt_plotpoints), hp1 = scatter(log10(data2(:,1)),data2(:,data2_n),'o','Filled','Sizedata',plot_datasize,'MarkerFaceColor','y','MarkerEdgeColor','k'); end
        if (~isempty(overlaydata2_file)), hp1 = scatter(log10(overlaydata2(:,1)),overlaydata2(:,2),'o','Filled','Sizedata',plot_datasize,'MarkerFaceColor','y','MarkerEdgeColor','k'); end
    else
        hl1 = line(data2(:,1),data2(:,data2_n),'Color','k','LineWidth',1.0);
        if (opt_plotpoints), hp1 = scatter(data2(:,1),data2(:,data2_n),'o','Filled','Sizedata',plot_datasize,'MarkerFaceColor','y','MarkerEdgeColor','k'); end
        if (~isempty(overlaydata2_file)), hp1 = scatter(overlaydata2(:,1),overlaydata2(:,2),'o','Filled','Sizedata',plot_datasize,'MarkerFaceColor','y','MarkerEdgeColor','k'); end
    end
elseif(opt_invanalysis)
    set(gcf,'CurrentAxes',fh(4));
    hold on;
    % set axes limits
    if (axis_data2_min >= axis_data2_max),
        axis_data2_min = min(data_FCO2_sum(:));
        axis_data2_max = max(data_FCO2_sum(:));
    end
    if (axis_data2_min >= axis_data2_max), disp(['ERROR: Failed to autoscale data2 ... ']); return; end
    % create plot on first y-axis
    hl1 = bar(data_FCO2_t(:),data_FCO2_sum(:),1.0,'hist');
    %hl1 = line(data_FCO2_t(:),data_FCO2_sum(:),'Color','k','LineWidth',1.5);
    delete(findobj('marker','*'))
end
% AXES
if (~isempty(data2) || opt_invanalysis),
    ax1 = gca;
    set(ax1,'XLim',[axis_tmin axis_tmax]);
    set(ax1,'XColor','k','XTick',[]);
    set(ax1,'YLim',[axis_data2_min axis_data2_max]);
    set(ax1,'YColor','k','YTick',[]);
    % create 2nd y-axis
    ax2 = axes('Position',get(ax1,'Position'),'Color','none','YAxisLocation','right','YColor','k');
    % create and label y-axis
    set(ax2,'YColor','k','TickDir','out','YTickLabelMode','auto');
    set(ax2,'YLim',[axis_data2_min axis_data2_max]);
    if (~isempty(data2)),
        if (isempty(plot_data2_title)),
            plot_data2_title = data2_name;
            plot_data2_title(find(plot_data2_title(:)=='_')) = '.';
            set(ax2,'YLabel',text('String',[plot_data2_title ' (' plot_data2_units ')'],'FontSize',10));
        else
            set(ax2,'YLabel',text('String',[plot_data2_title ' (' plot_data2_units ')'],'FontSize',12));
        end
    else
        set(ax2,'YLabel',text('String','\Sigma(emissions) (PgC)','FontSize',10));
    end
    % create and label x-axis depending on number of panels
    if (isempty(data3) && ~opt_invanalysis),
        set(ax2,'XColor','k','TickDir','out','XTickLabelMode','auto');
        set(ax2,'XLim',[axis_tmin axis_tmax]);
        if opt_log10,
            set(ax2,'XLabel',text('String',['log_{10}(time) (' str_tunits ')'],'FontSize',15));
        else
            set(ax2,'XLabel',text('String',['time (' str_tunits ')'],'FontSize',15));
        end
    else
        set(ax2,'XColor','k','TickDir','out','XTickLabel','');
        set(ax2,'XLim',[axis_tmin axis_tmax]);
    end
end
%
% *** PLOT PANEL #5 (OPTIONAL DATA #3) ********************************** %
%
if (~isempty(data3))
    set(gcf,'CurrentAxes',fh(3));
    hold on;
    % set axes limits
    if (axis_data3_min >= axis_data3_max),
        axis_data3_min = min(data3(:,data3_n));
        axis_data3_max = max(data3(:,data3_n));
    end
    if (axis_data3_min >= axis_data3_max), disp(['ERROR: Failed to autoscale data3 ... ']); return; end
    % plot data
    if opt_log10,
        hl1 = line(log10(data3(:,1)),data3(:,data3_n),'Color','k','LineWidth',1.0);
        if (opt_plotpoints), hp1 = scatter(log10(data3(:,1)),data3(:,data3_n),'o','Filled','Sizedata',plot_datasize,'MarkerFaceColor','y','MarkerEdgeColor','k'); end
        if (~isempty(overlaydata3_file)), hp1 = scatter(log10(overlaydata3(:,1)),overlaydata3(:,2),'o','Filled','Sizedata',plot_datasize,'MarkerFaceColor','y','MarkerEdgeColor','k'); end
    else
        hl1 = line(data3(:,1),data3(:,data3_n),'Color','k','LineWidth',1.0);
        if (opt_plotpoints), hp1 = scatter(data3(:,1),data3(:,data3_n),'o','Filled','Sizedata',plot_datasize,'MarkerFaceColor','y','MarkerEdgeColor','k'); end
        if (~isempty(overlaydata3_file)), hp1 = scatter(overlaydata3(:,1),overlaydata3(:,2),'o','Filled','Sizedata',plot_datasize,'MarkerFaceColor','y','MarkerEdgeColor','k'); end
    end
elseif(opt_invanalysis)
    set(gcf,'CurrentAxes',fh(3));
    hold on;
    % set axes limits
    if (axis_data3_min >= axis_data3_max),
        axis_data3_min = max(-100,min(data_FCO2_13C(:)));
        axis_data3_max = min(100,max(data_FCO2_13C(:)));
        loc_lim = max(abs(axis_data3_min),abs(axis_data3_max));
        axis_data3_min = -loc_lim;
        axis_data3_max = loc_lim;
    end
    if (axis_data3_min >= axis_data3_max), disp(['ERROR: Failed to autoscale data3 ... ']); return; end
    % create plot on first y-axis
    if (opt_rebinned),
        hl1 = bar(binctrs_FCO2(:),bindata_FCO2_13C(:),1.0,'hist');
    else
        hl1 = bar(data_FCO2_t(:),data_FCO2_13C(:),1.0,'hist');
    end
    delete(findobj('marker','*'))
end
% AXES
if (~isempty(data3) || opt_invanalysis),
    ax1 = gca;
    % create and label y-axis
    set(ax1,'YColor','k','TickDir','out');
    set(ax1,'YLim',[axis_data3_min axis_data3_max]);
    if (~isempty(data3)),
        if (isempty(plot_data3_title)),
            plot_data3_title = data3_name;
            plot_data3_title(find(plot_data3_title(:)=='_')) = '.';
            set(ax1,'YLabel',text('String',[plot_data3_title ' (' plot_data3_units ')'],'FontSize',10));
        else
            set(ax1,'YLabel',text('String',[plot_data3_title ' (' plot_data3_units ')'],'FontSize',12));
        end
    else
        set(ax1,'YLabel',text('String',['Emissions \delta^{13}C (' char(8240) ')'],'FontSize',10));
    end
    % create and label x-axis
    set(ax1,'XColor','k','TickDir','out','XTickLabelMode','auto');
    set(ax1,'XLim',[axis_tmin axis_tmax]);
    if opt_log10,
        set(ax1,'XLabel',text('String',['log_{10}(time) (' str_tunits ')'],'FontSize',15));
    else
        set(ax1,'XLabel',text('String',['time (' str_tunits ')'],'FontSize',15));
    end
end
%
% *** PRINT PLOT ******************************************************** %
%
set(gcf,'CurrentAxes',fh(1));
str_filename = ['timeseries.' str_filename];
if (~isempty(altfilename)), str_filename = altfilename; end
if opt_invanalysis, str_filename = [str_filename '.' 'INV']; end
if opt_rebinned, str_filename = [str_filename '.' 'INV_RB']; end
if opt_log10, str_filename = [str_filename '.' 'LOG10']; end
if (~isempty(expid2)), str_filename = [str_filename '.' 'ANOM']; end
str_filename = [str_filename '.' str_date];
if (plot_format_old == 'y')
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
%%%close all;
%
% *********************************************************************** %
