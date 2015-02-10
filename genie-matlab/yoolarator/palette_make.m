function a = palette_make(pal, fine)
% This macro makes a new colour palette ('pal2') from a smaller one by
% interpolating between the colours specified in the small, sample palette
% ('pal').
%
% Andrew Yool (axy@soc.soton.ac.uk), 9th May 2000.

% fine=10;
clear pal2;
[pp qq]=size (pal);

pal2(1,:)=pal(1,:);
for i=1:1:(pp-1)
	pos=((i-1)*fine)+1;
	stepr=(pal(i,1) - pal(i+1,1))/fine;
	stepg=(pal(i,2) - pal(i+1,2))/fine;
	stepb=(pal(i,3) - pal(i+1,3))/fine;
	for j=1:1:fine
		pal2(pos+j,1)=pal(i,1) - (stepr*j);
		pal2(pos+j,2)=pal(i,2) - (stepg*j);
		pal2(pos+j,3)=pal(i,3) - (stepb*j);
	end
end

a = pal2;
