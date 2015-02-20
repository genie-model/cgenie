function fldop = rg021(fldip,ovlx,ovly,csip,mask_op, fland)
% RG021  fldop = rg021(fldip,ovlx,ovly,csip,mask_op)
%
%    August 2004 : feeding this in as an input now
%    fland = 0.1;
%
%    Used by the likes of REGRID_2D.
%
%    Ian Totterdell (ijt@soc.soton.ac.uk)
%    Edited by Andrew Yool (axy@soc.soton.ac.uk), 11th June 2003.

sz_fldip = size(fldip);
n2ip = sz_fldip(2);
%
fldZ = isnan(fldip);
fldY = isfinite(fldip);
%
fld0 = fldip;
fld0(fldZ) = 0;
%
fld1 = transpose(ovly)*(fld0.*repmat(csip,[1 n2ip]))*ovlx;
%
fld2 = transpose(ovly)*(fldY.*repmat(csip,[1 n2ip]))*ovlx;
%
% The following is a new line I've inserted to fix the code
fld3 = transpose(ovly)*(1*repmat(csip,[1 n2ip]))*ovlx;
%
% The immediate following is the old code I've replaced
%nzfld2 = ( fld2 > fland );
nzfld2 = ( fld2./fld3 > fland );
%
sz_fld1 = size(fld1);
n1op = sz_fld1(1);
n2op = sz_fld1(2);
%
% n1op
% n2op
%
fldop(1:n1op,1:n2op) = NaN;
fldop(nzfld2) = fld1(nzfld2)./fld2(nzfld2);
fldop = fldop + mask_op;
%
clear fland sz_fldip n2ip fldZ fldY fld0 fld1 fld2 nzfld2 sz_fld1 n1op n2op
%
