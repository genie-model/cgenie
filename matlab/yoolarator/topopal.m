function [a] = topopal
% TOPOPAL  Creates topological colour map
%
%    Based on an altitude range from -6500 m to 6500 m, this colour
%    map attempts to replicate the kind of map used commonly on
%    atlases.  Definition is increased between -1000 m and 1000 m.
%
%    Andrew Yool (axy@soc.soton.ac.uk), 16th May 2003.

% extreme deep (-7000 to -6000, 5)
newcol(1,:)=[0.6 0 0.7];
newcol(2,:)=[0.3 0 0.6];
% deep (-6000 to -1000, 25)
newcol(3,:)=[0.1 0 0.6];
newcol(4,:)=[0.55 0.6 0.75];
% surface (-1000 to 0, 5)
newcol(5,:)=[0.6 0.65 0.8];
newcol(6,:)=[0.8 1 1];
% coastal (0 to 1000, 5)
newcol(7,:)=[0 0.67 0];
newcol(8,:)=[0.8 0.8 0.3];
% mountainous (1000 to 6000, 25)
newcol(9,:)=[1 1 0.5];
newcol(10,:)=[0.35 0.15 0.15];
% extreme mountainous (6000 to 7000, 5)
newcol(11,:)=[0.8 0.4 0.8];
newcol(12,:)=[0.9 0.75 0.9];

clear temp topopal pos
temp=palette_make(newcol(1:2,:),2);
for i=1:1:3, topopal(i*2,:)=temp(i,:); topopal((i*2)-1,:)=temp(i,:); end
pos=i*2;
temp=palette_make(newcol(3:4,:),7);
for i=1:1:8, topopal((i*2)+pos,:)=temp(i,:); topopal((i*2)+pos-1,:)=temp(i,:); end
pos=(i*2)+pos;
temp=palette_make(newcol(5:6,:),3);
for i=1:1:4, topopal(i+pos,:)=temp(i,:); end
pos=i+pos;
temp=palette_make(newcol(7:8,:),3);
for i=1:1:4, topopal(i+pos,:)=temp(i,:); end
pos=i+pos;
temp=palette_make(newcol(9:10,:),7);
for i=1:1:8, topopal((i*2)+pos,:)=temp(i,:); topopal((i*2)+pos-1,:)=temp(i,:); end
pos=(i*2)+pos;
temp=palette_make(newcol(11:12,:),2);
for i=1:1:3, topopal((i*2)+pos,:)=temp(i,:); topopal((i*2)+pos-1,:)=temp(i,:); end
pos=(i*2)+pos;

colormap(topopal);

a = topopal;
