% plot_sedcore_settings
%
%   ***********************************************************************
%   *** DEFAULT PARAMETER SETTINGS FOR sedcore DATA PLOTTING **************
%   ***********************************************************************
%
%   Edit this file directly for additional user settings:
%   NOTE: CONFIGURATION PATHS *MUST* BE CORRECT
%
%   ** configure paths and experiment
%      data_path='cgenie_output'
%      library_path1 = 'C:\cgenie.muffin\genie-matlab'
%   ** main (most commonly used) parameters
%      par_expduration = 50.0; Experiment run duration in kyr
%                              -> used to define age of stratigraphic 
%                                 marker layer
%      plot_nratio = 1;        Ratio of points to points plotted
%                              (for sub-sampling of data)
%      opt_relreflevel = true; Plot relative to a reference level?
%                              If false => stratigraphic height is plotted 
%                              relative to the surface
%   ** axis controls
%      axis_Cmin = 0.0;        Bulk concentration min plotted value (wt%)
%      axis_Cmax = 0.0;        Bulk concentration max plotted value (wt%)
%                              => set both equal for autoscaling
%      axis_d13Cmin = 0.0;     d13C min plotted value (o/oo)
%      axis_d13Cmax = 0.0;     d13C max plotted value (o/oo)
%                              => set both equal for autoscaling
%      axis_SRmin = 0.0;       Sed rate min plotted value (cm kyr-1)
%      axis_SRmax = 0.0;       Sed rate max plotted value (cm kyr-1)
%                              => set both equal for autoscaling
%      axis_agemodelmin = 0.0; Age model min plotted value (kyr or cm)
%      axis_agemodelmax = 0.0; Age model max plotted value (kyr or cm)
%                              => set both equal for autoscaling
%      axis_dy = 0.0;          Tick spacing on y-axis (kyr or cm)
%                              => set to zero for automatic spacing
%   ** additional data plotting controls 
%      axis_data1min = 0.0;    min plotted value of 1st optional data set
%      axis_data1max = 0.0;    max plotted value of 1st optional data set
%      axis_data2min = 0.0;    min plotted value of 2nd optional data set
%      axis_data2max = 0.0;    max plotted value of 1st optional data set
%                              => set both equal for autoscaling
%      plot_data1_title = '';  title for 1st optional data set
%      plot_data1_units = '';  units for 1st optional data set
%      plot_data2_title = '';  title for 2nd optional data set
%      plot_data2_units = '';  units for 2nd optional data set
%      plot_data1_ts_n = 3;    data column of time-sereis #1 to be plotted
%      plot_data2_ts_n = 3;    data column of time-sereis #2 to be plotted
%   ** plotting refinements and other options
%      axis_Cthresh = 0.1;     CaCO3 wt% threshold for masking out
%                              carbonate related data
%      opt_ashagemodel=false;  Use (internally-generated) ash age model?
%                              If also not det age => CaCO3 age model
%      opt_detagemodel=false;  Use (internally-generated) det age model?
%                              If also not ash age => CaCO3 age model
%      par_ref_coln = 90;      Number of numerically tagged levels
%      par_ashmaxn=1           Number of ash maximum to be used [1-3]
%      par_SR_mean_default=1.0;Default sedimentation rate (cm kyr-1)
%      plot_axislabel_prec=3;  Precision used for axis labelling
%      plot_datasize = 20.0;   Data point size
%      plot_rSR = 0.85;        Scaling for bulk wt% color fill bar height
%                              (values <1.0 expand, >1.0 contract height)
%      plot_dscrsz = 0.75;     Fraction of screen size of plotting window
%   ** alternative plotting
%      plot_format_old = 'y'; Choose 'old' style plotting?
%                             ('new' plotting ('y') requires additional
%                             Windows library resources)
%      plot_format = '';      Format of 'new' syle plot (if above is 'y')
%                             options: 'jpg', 'png', 'eps'
%                             'pngT' adds transparency to png
%      opt_fatplot=false;     plot fatter panels (if no time-series)
%      plot_path2 = 'C:\cgenie.muffin\genie-matlab\xpdfbin-win-3.03\bin32'
%      plot_path3 = 'C:\cgenie.muffin\genie-matlab\export_fig'
%
%   ***********************************************************************

% *********************************************************************** %
% *** USER SETTINGS ***************************************************** %
% *********************************************************************** %
%
% PARAMATER                % DEFAULT BRIEF DESCRIPTION [SEE ABOVE]
%                          *** configure paths and experiment ***
data_path='cgenie_output'; % RELATIVE PATH TO MODEL RESULTS
library_path1 = 'C:\cgenie.muffin\genie-matlab'; % PLOTTING RESOURCES PATH
%                          *** main (most commonly used) parameters ***
par_expduration = 50.0;    % [ 50.0] EXPERIMENT RUN DURATION (KYR)
plot_nratio = 1;           % [    1] DATA RESAMPLING RATIO FOR PLOTTIUNG
opt_relreflevel = true;    % [ true] PLOT RELATIVE TO REFERENCE LEVEL
%                          *** axis controls ***
axis_Cmin = 0.0;           % [  0.0] BULK CONCENTRATION PLOTTING MIN [%]
axis_Cmax = 0.0;           % [  0.0] BULK CONCENTRATION PLOTTING MAX [%]
axis_d13Cmin = 0.0;        % [  0.0] CaCO3 (d13C) PLOTTING MIN [o/oo]
axis_d13Cmax = 0.0;        % [  0.0] CaCO3 (d13C) PLOTTING MAX [o/oo]
axis_SRmin = 0.0;          % [  0.0] SED RATE PLOTTING MIN (cm kyr-1)
axis_SRmax = 0.0;          % [  0.0] SED RATE PLOTTING MAX (cm kyr-1)
axis_agemodelmin = 0.0;    % [  0.0] AGE MODEL MIN (kyr or cm)
axis_agemodelmax = 0.0;    % [  0.0] AGE MODEL MAX (kyr or cm)
axis_dy = 0.0;             % [  0.0] INCREMENT OF y AXIS (cm/kyr)
%                          *** additional data plotting controls ***
axis_data1min = 0.0;       % [   ''] DATA #1 PLOTTING MINIMUM
axis_data1max = 0.0;       % [   ''] DATA #1 PLOTTING MAXIMUM
axis_data2min = 0.0;       % [   ''] DATA #2 PLOTTING MINIMUM
axis_data2max = 0.0;       % [   ''] DATA #2 PLOTTING MAXIMUM
plot_data1_title = '';     % [   ''] DATA #1 NAME
plot_data1_units = '';     % [   ''] DATA #1 UNITS
plot_data2_title = '';     % [   ''] DATA #2 NAME
plot_data2_units = '';     % [   ''] DATA #2 UNITS
plot_data1_ts_n = 3;       % [    3] DATA #1 TIMESERIES DATA COLUMN
plot_data2_ts_n = 3;       % [    3] DATA #2 TIMESERIES DATA COLUMN
%                          *** plotting refinements and other options ***
axis_Cthresh = 0.1;        % [  0.1] CaCO3 TRACER PLOTTING THRESHOLD (wt%)
opt_ashagemodel=false;     % [false] ASH AGE MODEL?
opt_detagemodel=false;     % [false] DET AGE MODEL?
par_ref_coln = 90;         % [   90] number of numerically-tagged levels
par_ashmaxn=1;             % [    1] Number of ash maximum to be used
par_ashoffsetn=0;          % [    1] Number of layers offset from ash peak
par_SR_mean_default = 1.0; % [  1.0] DEFAULT SEDIMENTATION RATE
plot_axislabel_prec = 3;   % [    3] X-AXIS LABEL PRECISION
plot_datasize = 20.0;      % [ 20.0] DATA POINT PLOTTING SIZE
plot_rSR = 0.85;           % [ 0.85] COLOR FILL ADJUSTMENT SCALING
plot_dscrsz = 0.75;        % [ 0.75] FRACTIONAL FIGURE WINDOW SIZE
%                          *** alternative plotting ***
plot_format_old = 'y';     % [  'y']  'OLD' STYLE PLOTTING
plot_format = '';          % [  '']  FORMAT OF (NEW STYLE) PLOT
opt_fatplot=false;         % [false] FATTER PLOTS?
library_path2 = 'C:\cgenie.muffin\genie-matlab\xpdfbin-win-3.03\bin32';
library_path3 = 'C:\cgenie.muffin\genie-matlab\export_fig';
%
% *********************************************************************** %
