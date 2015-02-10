% Taylor diagram for summarizing model performance
%
% Taylor, 2001 - JGR, 106(D7)
%
% Program adapted from the IDL routine from K.E. Taylor
% (simplified version including less options)
%
% ---- Call :
%    plot_taylor(tsig,rsig,tcorr,out,name_experiment,title)
%
% ---- Input:
%    Needed:
%       tsig     : Standard deviation for the experiments (n values)
%       rsig     : Standard deviation for the reference (1 value)
%       tcorr    : Correlation between the reference and the experiments
%                 (n values)
%
%    Optional:
%       out             : 1 = standard display in a window (default)
%                         2 = encapsulated postcript output named 'output.eps'
%       name_experiment : Name of the experiment used in the legend
%                 ([n x length of the longer name] array of characters) 
%       maintit         : Title (string)
% ---- Output:
%       postcript or figure depending on the 'out' option ...
%       
%
% ----------------------------------------------------------------------
% G. Charria (02/2008)
% A. Yool    (16/06/2008; extensive "prettying up")
% ----------------------------------------------------------------------

function [plt, leg] = plot_taylor(tsig,rsig,tcorr,out,name_experiment,maintit,set_sigmax)

% close all
% clf

% ---------------------------
% ---- Parameters definition
% ---------------------------
% sigmax : radial limit of plot
if (max(tsig./rsig) < 1)
  sigmax = 1;
else
  sigmax = max((tsig./rsig))+0.1;
end

% AXY: allow user to control sigmax with a final argument
flag_bounds = tsig * 0;
if (nargin == 7)
  sigmax = set_sigmax;
  
  % this also sets all data values outside this maximum to equal the
  % maximum, and tags them up for special treatment (symbols turn
  % black) at the plotting stage
  t1 = tsig / rsig;
  t2 = find(t1 > sigmax);
  tsig(t2) = rsig * sigmax;
  flag_bounds(t2) = 1;
end

% AXY: just in case something other than standard deviation is used
%      and has values less than zero, this section sets the values
%      to NaNs
t1 = tsig / rsig;
t2 = find(t1 < 0);
tsig(t2) = NaN;

% Test on input for the choice of output.
if (nargin < 4)
  out = 1;
end

% Y-axis title
ytitle = 'Standard deviation (normalised)';
% X-axis title
xtitle = 'Standard deviation (normalised)';
% Fontsize for the plot
axcsiz = 12;
% Tick location
% AXY: parameter "val" should be adjusted ad hoc to get a satisfying
%      distribution of labels - I've tried, unsuccessfully, to come
%      up with a good automated version, but haven't managed so far
% AXY: the new code below tries out a series of potential tick increments
%      and selects as the first/best one that which produces 8 ticks or more;
%      far from ideal, but the results look OK
tryval = [0.1 0.2 0.25 0.5 1 2]; maxtry = max(size(tryval));
for i = 1:1:maxtry
  val = tryval(i);
  yticloc=0.:val:sigmax;
  [q1, q2] = max(size(yticloc));
  numtic(i) = q1;
end
q1 = find(numtic >= 8); q2 = max(q1);
if isempty(q2), q2 = maxtry; end
val = tryval(q2); 
val = 1;
yticloc=0.:val:sigmax;
ylabels = yticloc;
xticloc = yticloc;
xlabels = ylabels;
dummy=0.;

% AXY: sort out legend labels
str1 = name_experiment;
[s1, s2] = size(str1);
str2(2:(s1+1),:) = str1;
if s2 < 9, str2(s1+1,9) = ' '; end
str2(1,1:9) = 'Reference';
name_experiment = str2;

% -------------------------------------
% Minor Ticks
rminticval = 0.05:0.1:0.95;
% Subminor Ticks
rsubticval = 0.91:0.01:0.99;
% Major Ticks
rlabticval = [0.0, 0.1, 0.2, 0.3, 0.4,...
    0.5, 0.6, 0.7, ...
    0.8, 0.9, 0.95, 0.99, 1.0];
% Major Tick Labels
rticlabels = ['0.0 '; '0.1 '; '0.2 ';...
    '0.3 '; '0.4 '; '0.5 ';...
    '0.6 '; '0.7 '; '0.8 ';...
    '0.9 '; '0.95'; '0.99';...
    '1.0 '];
% 'Correlation' label
corrt=['C';'o';'r';'r';'e';'l';'a';'t';'i';'o';'n'];
% Length of minor ticks
minortic= 0.015;
% Length of subminor ticks
subtic = 0.010;

% Definition of markers:
val = length(tsig);
% AXY: use Ocean Data View colourmap for plotting
% pal2 = odvpal(val+2); pal = pal2(2:1:end-1,:);
pal2 = hsv(val+2); pal = pal2(2:1:end-1,:);
kind=['+';'o';'x';'s';'d';'^';'v';'p';'h';'*'];
colorm=['b';'r';'g';'c';'m';'y';'k'];
n=1;
marker(1:(size(colorm,1)*size(kind,1)),1:2)=' ';
for ic=1:size(colorm,1)
  for ik=1:size(kind,1)
    marker(n,:)=[kind(ik,:) colorm(ic,:)];
    n=n+1;
  end
end
if (length(tsig) > 70)
  disp('You must introduce new markers to plot more than 70 cases.')
  disp('The ''marker'' character array need to be extended inside the code.')
  return
end

% --------------------------------------
% Test to draw main rays or not
arccntl=1;
if (arccntl > 0)
  rticlen=1.; % Draw the complete ray
else
  rticlen=0.02; % Draw just a Tick
end

% if (out==2)
%     figure;
%     set(gcf,'Visible','off')
% end

% -----------------------------
% ---- Plot main axes (X and Y)
% -----------------------------
% plot(dummy,dummy)
box('off')
pbaspect([1 1 1])
% set(gcf,'Color','w')
set(gca,'XLim',[0 sigmax],...
  'YLim',[0., sigmax],...
  'YMinorTick','on',...
  'XMinorTick','on',...
  'TickLen',[0.01 0.01],...
  'XTick',xticloc,...
  'XTickLabel',xlabels,...
  'YTick',yticloc,...
  'YTickLabel',ylabels,...
  'FontSize',axcsiz)
xlabel(xtitle)
ylabel(ytitle)

hold on;

% ------------------------------------
% ---- Draw Results
% ------------------------------------
% Loop over the number of experiments
% AXY: this has been moved here so that a legend can be plotted properly
%  -- Plot reference data point
h = plot (1, 0, 'o', 'MarkerSize', 6, 'LineWidth', 2, ...
  'MarkerEdgeColor', [1 0 0], 'MarkerFaceColor', [0 0 0]);
%  -- Plot data
for i1=1:length(tsig)
  xx = (tsig(i1)/rsig(1))*tcorr(i1);
  yy = (tsig(i1)/rsig(1))*sqrt(1.-tcorr(i1)*tcorr(i1));
  h=plot(xx,yy,marker(i1,1),'MarkerSize',6,'Linewidth',2);
  if flag_bounds(i1) == 0
    set(h, 'Color', pal(i1,:));
  else
    set(h, 'Color', [0 0 0]);
  end
end

% -----------------------------
% ---- Plot radial lines
% -----------------------------
% Plot the extreme line
xa = sigmax*(0:0.001:1);
ya = sigmax*sqrt(1.-(0:0.001:1).*(0:0.001:1));
plot(xa,ya,'k')

% Plot the '1' line
xa = 1.*(0:0.001:1);
ya = 1.*sqrt(1.-(0:0.001:1).*(0:0.001:1));
plot(xa,ya,':k','LineWidth',1.5)

% Plot few other lines ...
for il=0.2:0.2:1
  xa = il*(0:0.001:1);
  ya = il*sqrt(1.-(0:0.001:1).*(0:0.001:1));
  plot(xa,ya,':k')
end
for il=1:(sigmax-1)/3:sigmax
  xa = il*(0:0.001:1);
  ya = il*sqrt(1.-(0:0.001:1).*(0:0.001:1));
  plot(xa,ya,':k')
end

% ------------------------------------
% ---- Draw tick marks (on the circle)
% ------------------------------------
% *** Main Ticks *** + Correlation labels + 'Correlation' title
for i=1:length(rlabticval) 
  
  r = rlabticval(i);
  ticx(1) = sigmax*r;
  ticy(1) = sigmax*sqrt(1.0 - r*r);
  ticx(2) = (sigmax*(1.-rticlen))*r;
  ticy(2) = (sigmax*(1.-rticlen))*sqrt(1.0 - r*r);
  
  plot(ticx,ticy,'k')
  
  % Tick Labels
  val = 1.02; % default = 0.015
  text((sigmax*val)*r,(sigmax*val)*sqrt(1.0 - r*r),rticlabels(i,:),'Fontsize',axcsiz)
  
end

% *** 'Correlation' title ***
val = 1.1; % default = 0.1
for i=1:size(corrt,1)
  r=0.575+(i-1)*((0.825-0.575)/11);
  text((sigmax*val)*r,(sigmax*val)*sqrt(1.0 - r*r),corrt(i,:),...
    'Fontsize',axcsiz,...
    'Rotation',-((pi/2)-acos(r))*180/pi)    
end

% *** Minor Ticks ***
for i=1:length(rminticval)
  
  r = rminticval(i);
  ticx(1) = sigmax*r;
  ticy(1) = sigmax*sqrt(1.0 - r*r);
  ticx(2) = (sigmax*(1.-minortic))*r;
  ticy(2) = (sigmax*(1.-minortic))*sqrt(1.0 - r*r);
  plot(ticx,ticy,'k')
end

% *** Sub minor Ticks ***
for i=1:length(rsubticval)
  r = rsubticval(i);
  ticx(1) = sigmax*r;
  ticy(1) = sigmax*sqrt(1.0 - r*r);
  ticx(2) = (sigmax*(1.-subtic))*r;
  ticy(2) = (sigmax*(1.-subtic))*sqrt(1.0 - r*r);
  plot(ticx,ticy,'k')
end

% ------------------------------------
% ---- Draw Results
% ------------------------------------
% Loop over the number of experiments
% AXY: the data is plotted twice so that it appears on top of all of
%      the plot's "scaffolding"; otherwise the plot symbols could be
%      obscured by the various lines that are plotted after the first
%      set of data plot instructions
%  -- Plot reference
h = plot (1, 0, 'o', 'MarkerSize', 6, 'LineWidth', 2, ...
  'MarkerEdgeColor', [1 0 0], 'MarkerFaceColor', [0 0 0]);
%  -- Plot data
for i1=1:length(tsig)
  xx = (tsig(i1)/rsig(1))*tcorr(i1);
  yy = (tsig(i1)/rsig(1))*sqrt(1.-tcorr(i1)*tcorr(i1));
  h=plot(xx,yy,marker(i1,1),'MarkerSize',6,'Linewidth',2);
  if flag_bounds(i1) == 0
    set(h, 'Color', pal(i1,:));
  else
    set(h, 'Color', [0 0 0]);
  end
end
%  -- Draw legend
% val = 1.05;
% axis ([0 (sigmax*val) 0 (sigmax*val)]);
% for i1=1:length(tsig)
%   xx = sigmax * val;
%   yy = sigmax - ((i1-1)*0.05*sigmax);
%   if ((nargin > 4) & (~isempty(name_experiment)))
%     h=plot(xx,yy,marker(i1,1),...
%       'MarkerSize',8,'Linewidth',2.5);
%     set(h, 'Color', pal(i1,:));
%     text(xx+0.06,yy,name_experiment(i1,:),'Fontsize',axcsiz)       
%   end
% end
% AXY: because of the way that data is now plotted, it's possible to
%      use a normal Matlab legend to show the data
h1 = legend(name_experiment, 0);
set(h1, 'FontSize', 6);
pos = get(h1, 'Position'); 
% sort out horizontal position of legend box
pos(1) = 0.78;
% sort out vertical position of legend box
pos(2) = 0.99 - pos(4);
set(h1, 'Position', pos);
% h2 = get(h1, 'Children');
% set(h2(1))
% lt = length(tsig);
% for i = 1:1:lt
%   j = ((i-1)*2)+1;
%   % set(h2(j+1), 'LineStyle', '.');
%   set(h2(j), 'Marker', marker(lt+1-i,1));
%   set(h2(j), 'Color', pal(lt+1-i,:));
%   set(h2(j), 'MarkerSize', 8, 'Linewidth', 2.5);
% end

% AXY: shunt graph slightly sideways to left
pos = get(gca, 'Position'); if pos(1) < 0.2, pos(1) = 0.05; set(gca, 'Position', pos); end

plt = gca;
leg = h1;

% ------------------------------------
% ---- Title
% ------------------------------------
if (nargin > 5)
  title(maintit,'Fontsize',axcsiz)
end

% ------------------------------------
% ---- Print eps file
% ------------------------------------
if (out==2)
  set(gcf, 'PaperType', 'A4');
  set(gcf, 'PaperOrientation', 'portrait');
  set(gcf, 'PaperUnits', 'inches');
  set(gcf, 'PaperPosition', [0.8353    3.3765    6.5882    4.9412]);
end
