function [data_n_binned data_binned data_binctrs] = make_hist(PDATA,PBINBNDS,PNORM)
% make_hist
%
%   ***********************************************************************
%   *** make histogram ****************************************************
%   ***********************************************************************
%
%   make_hist( ... )
%   blah ...
%
%   PDATA [2D ARRAY]
%   --> blah ...
%       data format is 2 columns:
%       the first is the value to be binned
%       the second is the weighting (if any)
%   PBINS [VECTOR]
%   --> blah ...
%   PBINS [LOGICAL]
%   --> blah ...
%
%   ***********************************************************************
%   *** HISTORY ***********************************************************
%   ***********************************************************************
%
%   14/11/10: COPIED FROM PLOT_HISTC
%   14/11/10: further development and 1st stable version
%
%   ***********************************************************************

% *********************************************************************** %
% *** INITIALIZE PARAMETERS & VARIABLES ********************************* %
% *********************************************************************** %
%
% process dummy parameters
data_ip = PDATA;
data_binbnds = PBINBNDS;
data_normalize = PNORM;
%
data_size = size(data_ip);
nmax = data_size(1);
%
% *********************************************************************** %

% *********************************************************************** %
% *** PROCESS DATA ****************************************************** %
% *********************************************************************** %
%
% *** CREATE BIN BOUNDARIES ********************************************* %
%
nbins = length(data_binbnds)-1;
% create bin centers
for n = 1:nbins,
    data_binctrs(n) = (data_binbnds(n) + data_binbnds(n+1))/2.0;
end
% create data arrays
data_n_binned = zeros(nbins,1);
data_binned   = zeros(nbins,1);
%
% *** PROCESS DATA ****************************************************** %
%
% bin data
data_n_binnedex = 0;
for n = 1:nmax,
    loc_bin=[];
    loc_bin = intersect(find(data_ip(n,1)>=data_binbnds(:)),find(data_ip(n,1)<data_binbnds(:))-1);
    if isempty(loc_bin),
        data_n_binnedex = data_n_binnedex + 1;
        data_bin_n(n) = -1;
    else
        data_n_binned(loc_bin) = data_n_binned(loc_bin) + 1;
        if (data_size(2) > 1), data_binned(loc_bin) = data_binned(loc_bin) + data_ip(n,2); end
        data_bin_n(n) = loc_bin;
    end
end
% normalize data (if requested)
if data_normalize,
    data_binned   = data_binned./data_n_binned;
    data_n_binned = 100.0 * data_n_binned/(sum(data_n_binned(:)) + data_n_binnedex);
end
%
% *********************************************************************** %

% *********************************************************************** %
% *** END *************************************************************** %
% *********************************************************************** %
%
%
% *********************************************************************** %
