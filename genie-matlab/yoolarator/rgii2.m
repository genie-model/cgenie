function [ovl,lovl] = rgii2(ia0,oa0,ncyc)
% RGII2   [ovl,lovl] = rgii2(ia0,oa0,ncyc)
%
%    Used by the likes of REGRID_2D.
%
%    Ian Totterdell (ijt@soc.soton.ac.uk)
%    Edited by Andrew Yool (axy@soc.soton.ac.uk), 11th June 2003.

lcyclic = ( nargin > 2 );
if( lcyclic & (ncyc<3) )
  msge = sprintf('invalid cyclicity');
  error(msge);
end
%
if( lcyclic )
%
  ios = max(find( oa0 <= ia0(1) ));
  iof = max(find( oa0 <= ia0(1)+360 ));
  ois = max(find( ia0 <= oa0(1) ));
%
  if( isempty(ios) )
    nshft = iof;
    opc1 = [ oa0(iof:ncyc)-360 oa0(1:iof) ];
  elseif( isempty(ois) )
    nshft = ios;
    opc1 = [ oa0(ios:ncyc) oa0(1:ios)+360 ];
  else
    nshft = 0;
    opc1 = oa0;
  end
%
  noa = max(size(opc1));
  opc2 = [ opc1(2:noa) opc1(noa+1-ncyc)+360 ];
%
  ipc1 = ia0;
  nia = max(size(ipc1));
  ipc2 = [ ipc1(2:nia) ipc1(1)+360 ];
%
else
%
  no = max(size(oa0));
  noa = no - 1;
  opc1 = oa0(1:noa);
  opc2 = oa0(2:no);
%
  ni = max(size(ia0));
  nia = ni - 1;
  ipc1 = ia0(1:nia);
  ipc2 = ia0(2:ni);
%
end
%
ipcl = permute(repmat(ipc1,noa,1),[2 1]);
ipcr = permute(repmat(ipc2,noa,1),[2 1]);
opcl = repmat(opc1,nia,1);
opcr = repmat(opc2,nia,1);
%
% whos opcr ipcr opcl ipcl
ovrlp = max(0,( min(opcr,ipcr) - max(opcl,ipcl) ));
%
if( lcyclic )
%
  if( nshft == 0 )
    ovl = ovrlp;
  else
    ovl = [ ovrlp(:,ncyc+2-nshft:ncyc) ovrlp(:,ncyc+1)+ovrlp(:,1) ovrlp(:,2:ncyc+1-nshft) ];
  end
%
else
  ovl = ovrlp;
end
%
lovl = ( ovl > 0 );
%
