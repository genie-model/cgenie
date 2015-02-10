function [] = make_merge_biogem(PPATH,PEXP1,PTIME1,PEXP2,PTIME2,PEXP3,PTIME3,PEXP4,PTIME4,PEXPOUT)
% make_merge_biogem
%
%   ***********************************************************************
%   *** Merge experiments *************************************************
%   ***********************************************************************
%
%   make_merge_biogem(PPATH,PEXP1,PTIME1,PEXP2,PTIME2,PEXP3,PTIME3,PEXP4,PTIME4,PEXPOUT)
%   merges time-series data (and copies other files) as a single experiment
%
%   PPATH [STRING] (e.g. 'cgenie_output')
%   --> results directory path
%   PEXP1 [STRING] (e.g. 'spinup1')
%   --> the 1st experiment name
%   PTIME1 [INETGER] (e.g. 10000)
%   --> duration of 1st experiment (years)
%   PEXP2 [STRING] (e.g. 'spinup2')
%   --> optional 2nd experiment name
%   PTIME2 [INETGER] (e.g. 0)
%   --> duration of 1st experiment (years)
%   PEXP3 [STRING] (e.g. 'spinup3')
%   --> optional 3rd experiment name
%   PTIME3 [INETGER] (e.g. 0)
%   --> duration of 1st experiment (years)
%   PEXP4 [STRING] (e.g. 'spinup4')
%   --> optional 4th experiment name
%   PTIME4 [INETGER] (e.g. 0)
%   --> duration of 1st experiment (years)
%   PEXPOUT [STRING] (e.g. 'spinup_complete')
%   --> the resulting (merged) experiment name
%
%   ***********************************************************************
%   *** HISTORY ***********************************************************
%   ***********************************************************************
%
%   14/01/19: CREATED
%   14/01/26: completed initial release
%   14/11/03: altered dummy variables names
%             + variables for run-length and adjusting summed time-scale
%             also replaced NaNs with a recognisable non number
%   14/11/03: *** split off of make_merge_runs ***
%   14/11/09: [minor]
%             fixed age offsets
%   14/11/16: added path passed parameter
%   15/01/10: recorded number of columns in each file to make output files
%             commensurate (rather than all having max # columns)
%             + adjusted so that only the .res file names present in the 
%             primary (1st) experiment are created as output (.res) files
%
%   ***********************************************************************

% *********************************************************************** %
% *** INITIALIZE PARAMETERS & VARIABLES ********************************* %
% *********************************************************************** %
%
% set path to experiments
data_path = PPATH;
% stuff
str_expin = struct('name', {}, 'path', {});
str_expout = struct('name', {}, 'path', {});
data_primaryexp = struct('resname', {}, 'n_col', {});
%
cell_dir_null = {'.'; '..'; '.svn'; 'netcdf_record_numbers'; '_restart.nc'};
n_dir_null = length(cell_dir_null);
% populate experiment in structure and check for existence of experiments
n_exp = 1;
str_expin = setfield(str_expin, {1}, 'name', PEXP1);
str_expin = setfield(str_expin, {1}, 'path', [data_path '/' PEXP1]);
if (exist(str_expin(1).path) ~= 7),
    disp(['ERROR: 1st experiment ', str_expin(1).path, ' does not exist.']);
    return;
end
age_offset(1) = 0.0;
if ~isempty(PEXP2),
    str_expin = setfield(str_expin, {2}, 'name', PEXP2);
    str_expin = setfield(str_expin, {2}, 'path', [data_path '/' PEXP2]);
    n_exp = 2;
    if (exist(str_expin(2).path) ~= 7),
        disp(['ERROR: 2nd experiment ', str_expin(2).path, ' does not exist.']);
        return;
    end
end
age_offset(2) = age_offset(1)+PTIME1;
if ~isempty(PEXP3),
    str_expin = setfield(str_expin, {3}, 'name', PEXP3);
    str_expin = setfield(str_expin, {3}, 'path', [data_path '/' PEXP3]);
    n_exp = 3;
    if (exist(str_expin(3).path) ~= 7),
        disp(['ERROR: 3rd experiment ', str_expin(3).path, ' does not exist.']);
        return;
    end
end
age_offset(3) = age_offset(2)+PTIME2;
if ~isempty(PEXP4),
    str_expin = setfield(str_expin, {4}, 'name', PEXP4);
    str_expin = setfield(str_expin, {4}, 'path', [data_path '/' PEXP4]);
    n_exp = 4;
    if (exist(str_expin(4).path) ~= 7),
        disp(['ERROR: 4th experiment ', str_expin(4).path, ' does not exist.']);
        return;
    end
end
age_offset(4) = age_offset(3)+PTIME3;
% more stuff
str_expout = setfield(str_expout, {1}, 'name', PEXPOUT);
str_expout = setfield(str_expout, {1}, 'path', [data_path '/' PEXPOUT]);
% create output directory
mkdir([data_path '/' PEXPOUT]);
mkdir([data_path '/' PEXPOUT '/biogem']);
% set date
str_date = [datestr(date,11), datestr(date,5), datestr(date,7)];
%
% *********************************************************************** %

% *********************************************************************** %
% *** MERGE FILES -- BIOGEM ********************************************* %
% *********************************************************************** %
%
% *** biogem: read in file lists and data; copy netCDF ****************** %
%
n_size_new = [0 0];
% 
for m = 1:n_exp,
    %
    n_size_old = n_size_new;
    % get directory listing
    dir_data = dir([str_expin(m).path(:)' '/biogem']);
    % count number of items
    loc_dir_length = length(dir_data);
    % remove null directory names
    for o = 1:n_dir_null,
        i = 1;
        n = 1;
        while (n <= loc_dir_length)
            if (strcmp(dir_data(i).name,char(cell_dir_null(o)))),
                dir_data(i) = [];
            else
                i = i + 1;
            end
            n = n + 1;
        end
        % re-count number of items
        loc_dir_length = length(dir_data);
    end
    % COPY 'netCDF' FILES
    % NOTE: later to involve merging ...
    i = 1;
    n = 1;
    while (n <= loc_dir_length)
        if (strcmp(dir_data(i).name(end-2:end),'.nc')),
            copyfile([str_expin(m).path(:)' '/biogem/' dir_data(i).name],[str_expout.path(:)' '/biogem/' 'exp' num2str(m) '.' dir_data(i).name])
            dir_data(i) = [];
        else
            i = i + 1;
        end
        n = n + 1;
    end
    % re-count number of items
    loc_dir_length = length(dir_data);
    % COPY 'biogem_year' FILES
    % NOTE: don't copy these for now ... without renaming the year string,
    %       much confusion is possible ...
    i = 1;
    n = 1;
    while (n <= loc_dir_length)
        if (strcmp(dir_data(i).name(1:min(11,end)),'biogem_year')),
            %%%copyfile([str_expin(m).path(:)' '/biogem/' dir_data(i).name],[str_expout.path(:)' '/biogem/' dir_data(i).name])
            dir_data(i) = [];
        else
            i = i + 1;
        end
        n = n + 1;
    end
    % re-count number of items
    loc_dir_length = length(dir_data);
    % COPY 'biogem_series' FILES
    % determine maximum data array size required
    loc_n_size = [0 0];
    for n = 1:loc_dir_length,
        tmp_data = load([str_expin(m).path '/biogem/' dir_data(n).name],'ascii');
        tmp_size = size(tmp_data);
        loc_n_size(1) = tmp_size(1);
        if (tmp_size(2) > loc_n_size(2)), loc_n_size(2) = tmp_size(2); end
        % store list of filenames in the primary (1st) experiment
        if (m == 1),
            data_primaryexp = setfield(data_primaryexp, {n}, 'resname', dir_data(n).name);
            data_primaryexp = setfield(data_primaryexp, {n}, 'n_col', tmp_size(2));
            n_dir_length = loc_dir_length;
        end
    end
    %
    n_size_new(1) = n_size_old(1) + loc_n_size(1);
    %
    for n = 1:n_dir_length,
        if (exist([str_expin(m).path '/biogem/' data_primaryexp(n).resname],'file') ~= 0),
            tmp_data = load([str_expin(m).path '/biogem/' data_primaryexp(n).resname],'ascii');
            tmp_size = size(tmp_data);
            tmp_data(:,1) = tmp_data(:,1) + age_offset(m);
            data(n_size_old(1)+1:n_size_new(1),1:tmp_size(2),n) = tmp_data;
            if (tmp_size(2) < loc_n_size(2)), data(n_size_old(1)+1:n_size_new(1),tmp_size(2)+1:loc_n_size(2),n) = NaN; end
        end
    end
end
%
% *** biogem: interpolate time-series data ****************************** %
% 
% %%%
%
% *** biogem: save time-series data ************************************* %
% 
for n = 1:n_dir_length,
    tmp_data = data(:,1:data_primaryexp(n).n_col,n);
    tmp_data(find(isnan(tmp_data))) = -1.999999E19;
    save([str_expout.path(:)' '/biogem/' data_primaryexp(n).resname],'tmp_data','-ascii');
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
