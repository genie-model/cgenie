% PAPER  Sets paper size, etc. to suit A4 paper
%
%    Andrew Yool (axy@soc.soton.ac.uk), 29th July 2003.

h=gcf;
set (h, 'PaperType', 'a4letter');
temp=get(h,'PaperSize');
if temp(1)>temp(2)
%set (h,'PaperPosition',[0.25 0.25 11.1929 7.76772]);
set (h,'PaperPosition',[0.0 0.0 11.6929 8.26772]);
else
%set (h,'PaperPosition',[0.25 0.25 7.76772 11.1929]);
set (h,'PaperPosition',[0.0 0.0 8.26772 11.6929]);
end
clear h
