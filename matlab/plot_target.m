% ------------------------------PLOT_TARGET-------------------------------
% ------------------------------------------------------------------------
% Description: Plots the target diagram of Jolliff et al.(2009) for 
% model skill assessment. 
%
% plot_target has been designed to be accomodate most uses of the target
% diagram including a parameter sweep coloured by parameter values and the
% plotting of individual experiments with a legend (see EXAMPLE CALLS). 
% The function is designed such that statistics can be calculated elsewhere 
% and plotted in bulk, avoiding problems with processing large ensembles in
% matlab. 
%
% Reference: Joliff, J.K., Kindle, J.C., Shulman, I., Penta, B., Friedrichs
% ,M.A.M., Helber, R., Arnone, R.A., (2009) Summary diagrams for coupled
% hydrodynamic-ecosystem model skill assessment. Journal of Marine Systems,
% 79, 64-82 (see figs 7 & 14).
%
% Author: J D Wilson (wilsonjd@cardiff.ac.uk)
% ------------------------------------------------------------------------
% INPUT VARIABLES:
% -rmsd:        std dev normalised unbiased root mean square difference
%               (mx1 variable)
% -bias:        std dev normalised bias
%               (mx1 variable)
%
% OPTIONAL VARIABLES ([] for blank):
% -colour:      colour points according to value(s) 
%               (mx1 variable OR LineSpec colour string i.e. 'r')
% -R_marker:    correlation coefficient that will be plotted as marker
%               (single value OR multiple i.e. [0.4 0.5])
% -ou_marker:   observation uncertainity that will be plotted as marker
%               (single value OR multiple i.e. [0.4 0.5])
% -axes_handle: handle of axes, if present will plot on corresponding axes
%               otherwise if blank will plot in a new figure window
%
% OUTPUT VARIABLES:
% -target_handle:   handle of the target plot axes for plotting on same
%                   axes
%
% ------------------------------------------------------------------------
% EXAMPLE CALLS:
% -single parameter sweep coloured by parameter values with min R of 0.8: 
% plot_target(rmsd, bias, colour, 0.8, [], [])
%
% -single parameter sweep with multiple R markers:
% plot_target(rmsd, bias, [], [0.2 0.4 0.8], [], [])
%
% -multiple experiments with ou values:
% figure_handle = plot_target(rmsd_1, bias_1, 'r', [], 0.03, [])
% plot_target(rmsd_2, bias_2, 'b', [], 0.05, figure_handle)
% plot_target(rmsd_3, bias_3, 'g', [], 0.02, figure_handle)
%
% POTENTIAL PROBLEMS/ISSUES:
% - resizing  / zooming in & out within matlab window is not possible
% - observational uncertainty not implemented fully
%
% ------------------------------HISTORY-----------------------------------
% Created: 10/06/2012
% 15/06/2012: marker subfunctions, ability to plot multiple markers
% 19/08/2012: added axes_handle input to fix open fig window problems
% 28/08/2012: removed origin axes legend entries so legend can be used
% ------------------------------------------------------------------------
% ------------------------------------------------------------------------


function [ target_handle ] = plot_target ( rmsd , bias , colour , R_marker, ou_marker, axes_handle)


% ------------------------------PLOT DATA---------------------------------
% determine whether this is plotting a new figure or adding to an existing
% plot and set up an axes handle (==target_handle) so function knows 
% where to plot subsequent calls if needed.
if isempty(axes_handle)==1
    figs=max(findall(0,'Type','Figure'));  % find highest figure handle 
        
    if isempty(figs)==1                    % if none, figs is empty!
        figs=0;                            % so set figs = 0
    end
    
    figure(figs+1);                        % otherwise new figure window
    target_handle=gca;                     % get new axes_handle
    replot_flag=0;                         % flag for new plot 
else
    target_handle=axes_handle;             % but if given, plot on this!
    replot_flag=1;                         % flag as replotting 
end
    
% plot data as scatter plot (with standard matlab axes)
% (n.b. scatter allows flexibility with colour over plot function)
% >> no colour!
if isempty(colour)==1
    scatter(target_handle,rmsd,bias,50,'k','filled');
    hold on
end

% >> colour!
if isempty(colour)==0
    colormap('Jet')
    scatter(target_handle,rmsd,bias,50,colour,'filled');
    hold on
end

% ------------------------DRAW NEW ORIGIN AXES----------------------------
if replot_flag==0   % only draw if being plotted for first time!

% determine min/max of axis
if (max(abs(rmsd))>1 | max(abs(bias))>1) == 1
    maxi=2.0; mini=-2.0;
elseif (max(abs(rmsd))>2 | max(abs(bias))>2) == 1
    disp('rmsd or bias numbers are large...problem?')
    maxi=2.0; mini=-2.0;
elseif (max(abs(rmsd))<1 | max(abs(bias))<1) == 1
    maxi=1.0; mini=-1.0;
end

% draw new axis lines through origin...
haxisLine=plot([mini-0.2 maxi+0.2],[0 0],'k');
vaxisLine=plot([0 0],[maxi+0.2 mini-0.2],'k');

% ...but don't allow them to be included in the legend
set(get(get(haxisLine,'Annotation'),'LegendInformation'),'IconDisplayStyle','off');
set(get(get(vaxisLine,'Annotation'),'LegendInformation'),'IconDisplayStyle','off');

% set major ticks...
XL_major=[mini:0.5:maxi];
YL_major=[mini:0.5:maxi];
set(target_handle,'XTick',XL_major);
set(target_handle,'YTick',YL_major);

% and minor ticks...
XL_minor=[mini-0.2:0.1:maxi+0.2];
YL_minor=[mini-0.2:0.1:maxi+0.2];

% get tick labels for major
XLab=get(target_handle,'XtickLabel');
YLab=get(target_handle,'YtickLabel');

% derive tick offsets to align labels with ticks although a bit haphazard!
Xoff=diff(get(target_handle,'XLim'))./70;
Yoff=diff(get(target_handle,'YLim'))./70;

Xoff_minor=diff(get(target_handle,'XLim'))./110;
Yoff_minor=diff(get(target_handle,'YLim'))./90;

% Plot new major ticks on new axes (and remove from legend) ...
for i=1:length(XL_major)
    xmajtickLine=plot(target_handle,[XL_major(i) XL_major(i)],[0 Yoff],'-k');
    set(get(get(xmajtickLine,'Annotation'),'LegendInformation'),'IconDisplayStyle','off');
end;
for i=1:length(YL_major)
   ymajtickLine=plot(target_handle,[Xoff, 0],[YL_major(i) YL_major(i)],'-k');
   set(get(get(ymajtickLine,'Annotation'),'LegendInformation'),'IconDisplayStyle','off');
end;
% and minor ticks
for i=1:length(XL_minor)
    xmintickLine=plot(target_handle,[XL_minor(i) XL_minor(i)],[0 Yoff_minor],'-k');
    set(get(get(xmintickLine,'Annotation'),'LegendInformation'),'IconDisplayStyle','off');
end;
for i=1:length(YL_minor)
   ymintickLine=plot(target_handle,[Xoff_minor, 0],[YL_minor(i) YL_minor(i)],'-k');
   set(get(get(ymintickLine,'Annotation'),'LegendInformation'),'IconDisplayStyle','off');
end;

% add new tick labels
text(XL_major-0.05,zeros(size(XL_major))-1.*Yoff,XLab);
text(zeros(size(YL_major))-3.*Xoff,YL_major,YLab);

% add new axes labels
text([maxi+0.1],[0]+0.1,'RMSD*''(\sigma_d)')
text([0]+0.05,[maxi+0.1],'Bias*')

% remove the original axes
box off;
axis off;
set(gcf,'color','w');

% end of drawing new axes through the origin!
end 

% ---------------------------PLOT MARKERS---------------------------------
range1=[-1:0.01:1]'; % range over which to calc circular line points

% where RMSD = 1 (points within this marker are *almost* always 
% positively correlated  see pg. 72):
if replot_flag==0
    circle1=real(0+sqrt(1-((range1-0)).^2));
    circle2=real(0-sqrt(1-((range1-0)).^2));

    markerLine1=plot(target_handle,range1,circle1,'k');
    markerLine2=plot(target_handle,range1,circle2,'k');
    set(get(get(markerLine1,'Annotation'),'LegendInformation'),'IconDisplayStyle','off');
end

% ----------------------------PLOT R MARKERS------------------------------

% user defined marker(s) for correlation coefficient (R):
% subfunction calculates equivalent RMSD*'(sigma_diff) value (see pg. 72, eq. 14)
if isempty(R_marker)==0
    plot_marker(R_marker , target_handle)
end

% ----------------PLOT OBSERVATION UNCERTAINITY MARKER--------------------
% plot marker for observational uncertainty, 
% (currently not fully implemented see pg. 73 for details on this)
if isempty(ou_marker)==0
    plot_ou(ou_marker , target_handle)
end
    
% ---------------------------COLOUR BAR-----------------------------------
% plot a colourbar if data is given in colour (i.e. not a LineSpec)
if isempty(colour)==0 & ischar(colour)==0
    C=colorbar ('h');
    set(C,'location','South',...
        'XTick',[max(colour) min(colour)],...
        'XTickLabel',{'Max', 'Min'})
end

% --------------------------FINISHING TOUCHES-----------------------------
% set dimensions straight 
set(gca,'DataAspectRatio',[1 1 1])
end

% ------------------------------------------------------------------------
% -----------------------------SUB FUNCTIONS------------------------------
% ------------------------------------------------------------------------
 
 function [] = plot_marker ( R_marker , target_handle )
 R=sqrt(1.0+R_marker.^2-2*R_marker.^2)';
 for n=1:size(R)
    range2{:,n}=[-(R(n)):0.01:R(n)+0.01]'; % range over which to calc circular line points
 end
 
 for n=1:size(R)
    circle1{:,n}=real(0+sqrt(R(n)^2-((range2{1,n}-0).^2)));
    circle2{:,n}=real(0-sqrt(R(n)^2-((range2{1,n}-0).^2)));
 end
 
 for n=1:size(R)
    RLine1=plot(target_handle,range2{1,n},circle1{1,n},'k--');
    RLine2=plot(target_handle,range2{1,n},circle2{1,n},'k--');
    set(get(get(RLine1,'Annotation'),'LegendInformation'),'IconDisplayStyle','off');
 end
end
  
 function [] = plot_ou ( ou_marker , target_handle )
 ou=ou_marker';
 for n=1:size(ou)
    range3{:,n}=[-(ou(n)):0.01:ou(n)+0.01]' % range over which to calc circular line points
 end
 
for n=1:size(ou)
    circle1{:,n}=real(0+sqrt(ou(n)^2-((range3{1,n}-0).^2)));
    circle2{:,n}=real(0-sqrt(ou(n)^2-((range3{1,n}-0).^2)));
end
 
for n=1:size(ou)
    ouLine1=plot(target_handle,range3{1,n},circle1{1,n},'k--');
    ouLine2=plot(target_handle,range3{1,n},circle2{1,n},'k--');
    set(get(get(ouLine1,'Annotation'),'LegendInformation'),'IconDisplayStyle','off');
end
 end
 
 %------------------------------------------------------------------------
 %----------------------------------END-----------------------------------
