% plot_hist_settings
%
%   ***********************************************************************
%   *** DEFAULT PARAMETER SETTINGS FOR plot_hist DATA PLOTTING ************
%   ***********************************************************************
%
%   Edit this file directly for additional user settings:
%   NOTE: CONFIGURATION PATHS *MUST* BE CORRECT
%   NOTE: QUESTIONS HAVE CAN TAKE 'n' OR 'y' AS VALID SETTINGS
%
%   CONFIGURATION PATHS
%      data_path = 'cgenie_output'
%      library_path1 = 'C:\cgenie.muffin\genie-matlab'
%   DATA PROCESSING
%      data_normalize = false;normalize data (as a % of all occurrences)
%      data_truncate = true;  truncate data set at bin limits
%   COLOR AND SCALING
%      data_colobar='';       colorbar color scale name
%      data_colorbar_inv=true;Invert colorbar colors?
%      axis_xmin = 0.0;       min plotted x value
%      axis_xmax = 0.0;       max plotted x value
%      axis_ymin = 0.0;       min plotted y value
%      axis_ymax = 0.0;       max plotted y value
%   PLOT FORMAT AND DIMENSIONS
%      plot_format_old=true;  Choose 'old' style plotting?
%                             ('new' plotting (false) requires additional
%                             Windows library resources)
%      plot_format = '';      Format of 'new' syle plot (if above is 'y')
%                             options: 'jpg', 'png', 'eps'
%                             'pngT' adds transparency to png
%      plot_dataname_1 = '';  Optional replacement x-axis (data 1) label
%                             (empty '' for default label)
%      plot_dataname_2 = '';  Optional replacement z-axis (data 2) label
%                             (empty '' for default label)
%      plot_title = '';       Optional replacement title
%                             (empty '' for default title)
%      plot_dscrsz = 0.60;    Fraction of screen size of plotting window
%   OPTIONAL ADDITIONAL PATHS [SEE USER MANUAL]
%      plot_path2 = 'C:\cgenie.muffin\genie-matlab\xpdfbin-win-3.03\bin32'
%      plot_path3 = 'C:\cgenie.muffin\genie-matlab\export_fig'
%
%   ***********************************************************************

% *********************************************************************** %
% *** USER SETTINGS ***************************************************** %
% *********************************************************************** %
%
% PARAMATER             % DEFAULT BRIEF DESCRIPTION [SEE ABOVE]
data_path='cgenie_output'; % [ 'cgenie_output']
library_path1 = 'C:\cgenie.muffin\genie-matlab';
data_normalize = false; % [true] NORMALIZE DATA
data_truncate = true;   % [true] TRUNCATE DATA
colorbar_name='parula'; % ['parula'] COLORBAR COLOR SCALE NAME
colorbar_inv=true;      % [true] INVERT COLORBAR
colobar_data='';        % ['']
axis_xmin = 0.0;        % [ 0.0] x PLOTTING MINIMUM
axis_xmax = 0.0;        % [ 0.0] x PLOTTING MAXIMUM
axis_ymin = 0.0;        % [ 0.0] y PLOTTING MINIMUM
axis_ymax = 0.0;        % [ 0.0] y PLOTTING MAXIMUM
plot_format_old = true; % [true]  'OLD' STYLE PLOTTING
plot_format = '';       % [  '']  FORMAT OF (NEW STYLE) PLOT
plot_dataname_1 = '';     % [  '']  DATA LABEL 1
plot_dataname_2 = '';     % [  '']  DATA LABEL 2
plot_title = '';        % [  '']  OPTIONAL REPLACEMENT TITLE
plot_dscrsz = 0.60;     % [0.60]  FRACTIONAL FIGURE WINDOW SIZE
library_path2 = 'C:\cgenie.muffin\genie-matlab\xpdfbin-win-3.03\bin32';
library_path3 = 'C:\cgenie.muffin\genie-matlab\export_fig';
%
% *********************************************************************** %
