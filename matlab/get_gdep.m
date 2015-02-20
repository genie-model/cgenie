function [a] = get_gdep(kmax, ez0, maxdep)
% GET_GDEP  Calculates vertical array for GOLDSTEIN
%
%	>> gdep = get_gdep(kmax, ez0, maxdep);
%
%	Where,
%
%		gdep	= depth (in km)
%		kmax	= number of model levels
%		ez0	= ez0 parameter (usually 0.1)
%		maxdep	= maximum model depth (in km)
%
%       Andrew Yool (axy@soc.soton.ac.uk), 15th May 2003.

nargs = nargin;
if nargs == 2
	maxdep = 5;
end

z1 = ez0*((1.0 + 1/ez0)^(1.0/kmax) - 1.0);
tv4 = ez0*((z1/ez0+1)^0.5-1);
tv2 = 0;
tv1 = 0;
zro(kmax) = -tv4;
zw(kmax+1) = tv2;
for k=1:1:kmax
	if ez0 > 0
		tv3 = ez0*((z1/ez0+1)^k-1);
		dz(kmax-k+1) = tv3 - tv2;
		tv2 = tv3;
		tv5 = ez0*((z1/ez0+1)^(k+0.5)-1);
		if k < kmax
			dza(kmax-k) = tv5 - tv4;
		end
		tv4 = tv5;
		tv1 = tv1 + dz(kmax-k+1);
	else
		dz(k) = 1d0/kmax;
		dza(k) = 1d0/kmax;
	end
end

for k=kmax:-1:1
	if k > 1
		zro(k-1) = zro(k) - dza(k-1);
	end
	zw(k) = zw(k+1) - dz(k);
end

a = zw * maxdep;