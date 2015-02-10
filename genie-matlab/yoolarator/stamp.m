% STAMP   Date-stamps plots with a time and date
%
%    This command adds a small piece of text to the top
%    right of the current figure to indicate the time and
%    date of the figure's generation.
%
%    Note : if this command is to be used with SUPTITLE,
%    don't use it and use SUPTITLE2 instead.
%
%    Andrew Yool (axy@soc.soton.ac.uk), 28th February 2001.

clear h

h=gca;

% get limits of axes on current graph
temptime=xlim;
xlimit=temptime(2);
xrange=temptime(2) - temptime(1);
temptime=ylim;
ylimit=temptime(1);
yrange=temptime(2) - temptime(1);

% get position of current graph on page
temptime=get(h,'Position');
pagexlimit=temptime(1) + temptime(3);
pagexrange=temptime(3);
pageylimit=temptime(2);
pageyrange=temptime(4);

% calculate position of text so that it appears in the top right
% corner of the page
temptime=(0.90 - pagexlimit)/pagexrange;
temptime2=(temptime*xrange) + xlimit;
textxpos=temptime2;
temptime=(0.98 - pageylimit)/pageyrange;
temptime2=(temptime*yrange) + ylimit;
textypos=temptime2;

clear clock;
clear nowtime;
nowtime=fix(clock);
if nowtime(5)>9
datestamp=sprintf('%d:%d, %d/%d/%d',nowtime(4),nowtime(5),nowtime(3),nowtime(2),nowtime(1));
else
datestamp=sprintf('%d:0%d, %d/%d/%d',nowtime(4),nowtime(5),nowtime(3),nowtime(2),nowtime(1));
end

h2=text (textxpos,textypos,[datestamp]);
set (h2, 'FontSize', 8);
set (h2, 'HorizontalAlignment','center');
