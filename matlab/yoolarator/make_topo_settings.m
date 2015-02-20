% make_topo_settings
%
%   ***********************************************************************
%   *** DEFAULT PARAMETER SETTINGS FOR make_topo TOPO GENERATOR ***********
%   ***********************************************************************
%
%   Edit this file directly for additional user settings:
%
%
%   ***********************************************************************

% *********************************************************************** %
% *** USER SETTINGS ***************************************************** %
% *********************************************************************** %
%
% PARAMATER                % DEFAULT BRIEF DESCRIPTION [SEE ABOVE]
%                          *** configure paths and experiment ***
par_gcm='hadcm3l';         % [ 'hadcm3l'] GCM model name
gcm_path='DATA';           % [    'DATA'] path to HadCM3L netCDf files
par_um_topo='maas201o';    % ['maas201o'] UM topo name
library_path='C:\cgenie.muffin\genie-matlab\yoolarator'; % RESOURCES PATH
%                          *** primary functionality ***
opt_windonly=false;        % [     false] ONLY RE_GRID WINDS?
opt_bathonly=false;        % [     false] ONLY RE_GRID BATHYMETRY?
opt_randomrunoff=true;     % [      true] 
opt_filtertopo=true;       % [      true] 
%                          *** regridding parameters ***
par_sm = 0.0;              % [       0.0] BATHYMETRY SMOOTHING PARAMETER
%
% *********************************************************************** %
