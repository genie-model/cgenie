function [a] = rain(fine)
% RAIN  Creates a rainbow palette of user-specified size
%
%	This command builds on BASE_RAIN (formerly RAIN) to 
%	allow the user to specify the actual number of 
%	colours in the rainbow palette.
%
%	>> rain(12);
%
%	This will produce a palette with exactly 12 colours in
%	it.  The colours used will be drawn from the standard
%	rainbow palette.
%
%       Andrew Yool (axy@soc.soton.ac.uk), 12th February 2003.

basefine = 200;

pal=[
    0.7500         0    0.7500;
         0         0    1.0000;
    0.5000    1.0000    1.0000;
    0.0000    0.8000    0.0000;
    1.0000    1.0000    0.3300;
    1.0000    0.5000         0;
    0.5000         0         0];

clear pal2;
basefine=basefine + 1;

if basefine<0
pal2=pal;
else
pal2(1,:)=pal(1,:);
for i=1:1:6
	pos=((i-1)*basefine)+1;
	stepr=(pal(i,1) - pal(i+1,1))/basefine;
	stepg=(pal(i,2) - pal(i+1,2))/basefine;
	stepb=(pal(i,3) - pal(i+1,3))/basefine;
	for j=1:1:basefine
		pal2(pos+j,1)=pal(i,1) - (stepr*j);
		pal2(pos+j,2)=pal(i,2) - (stepg*j);
		pal2(pos+j,3)=pal(i,3) - (stepb*j);
	end
end
end

bigpal = max(size(pal2));

if fine < 2
	pal3 = pal2(1,:);
elseif fine == 2
	pal3(1,:) = pal2(1,:);
	pal3(fine,:) = pal2(end,:);
elseif fine > 200
	error (' Please be serious - do you really want such a large palette?');
else
	pal3(1,:) = pal2(1,:);
	pal3(fine,:) = pal2(end,:);
	
	t1 = bigpal - fine;
	t2 = (t1 / (fine - 1));
	pos = 1;
	for i = 2:1:(fine - 1)
		pos = pos + t2 + 1;
		pos2 = round(pos);
		pal3(i,:) = pal2(pos2,:);
	end
end

colormap(pal3);
a = pal3;
