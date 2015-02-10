function [a] = oceanpal(fine)
% OCEANPAL  Creates an ocean depth palette of user-specified size
%
%	This command builds on RAIN, but uses an "ocean depth"
%	palette.
%
%	>> oceanpal(12);
%
%	This will produce a palette with exactly 12 colours in
%	it.  The colours used will be drawn from the standard
%	ocean depth palette.
%
%	Andrew Yool (axy@soc.soton.ac.uk), 16th May 2003.

basefine = 200;

% AY (16/09/04) : made a little bit more colourful

% The old palette ...
% pal = [
% 	0.6000         0    0.7000;
%     0.4500         0    0.6500;
%     0.3000         0    0.6000;
%     0.1000         0    0.6000;
%     0.2286    0.1714    0.6429;
%     0.2929    0.2571    0.6643;
%     0.3571    0.3429    0.6857;
%     0.4214    0.4286    0.7071;
%     0.4857    0.5143    0.7286;
%     0.5500    0.6000    0.7500;
%     0.6000    0.6500    0.8000;
%     0.6667    0.7667    0.8667;
%     0.7333    0.8833    0.9333;];

% The new palette ...
pal = [
	0.6000         0    0.7500;
    0.4500         0    0.7000;
    0.3000         0    0.6500;
    0.1000    0.1000    0.6000;
    0.2000    0.2000    0.5500;
    0.3000    0.3000    0.6000;
    0.3500    0.3500    0.6500;
    0.4250    0.4250    0.7000;
    0.5000    0.5000    0.7250;
    0.5500    0.5500    0.7500;
    0.6000    0.6500    0.8000;
    0.6500    0.7500    0.8500;
    0.7500    0.9000    0.9500;];

clear pal2;
basefine=basefine + 1;

if basefine<0
pal2=pal;
else
pal2(1,:)=pal(1,:);
for i=1:1:12
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
