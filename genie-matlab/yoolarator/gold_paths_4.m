function output = gold_paths_4(fname, grdsz, outname);
% GOLD_PATHS_4  Generates PSILES and PATHS files from a K1 file
%
%   Usage : 
%
%   >> gold_paths_4(fname, [i j k], outname);
%
%   Where :
% 
%   fname   = *.k1 filename (e.g. 'worber.k1')
%   i, j, k = dimensions of grid (e.g. [36 36 8])
%   outname = output file name
%
%   i = longitude; j = latitude; k = depth
%
%   Note : this routine interacts extensively with
%   the operator.  As well as requiring typed input,
%   the routine also requires the operator to use
%   figure windows to edit grids and paths.  It is
%   hoped that the user prompting is sufficient, but
%   contact the author at the address below should
%   any problems persist.
%
%   Note also that the automatic subroutines used to
%   determine paths and runoff are very crude, and
%   that care should be taken when using them.  In
%   the case of runoff, hand editing of the field is
%   almost inevitable (but see RUNOFF for a shortcut).
% 
%   See also GEN_TOPO_5, RUNOFF, RUNOFF2, GOLD_PATHS.
%
%   Andrew Yool (axy@soc.soton.ac.uk), January 2005.

fprintf('\n - Welcome to the automatic path generator\n');

% Get rid of all existing windows
close all

% ==================================================
% Set up
% ==================================================

% Set up grid size
imax = grdsz(1); jmax = grdsz(2); kmax = grdsz(3);

% Load up a topography file, remove its edges and NaN pad it
% a = load ('workar.k1');
a = load (fname);
[sy, sx] = size(a);
if sy < jmax+2 | sy > jmax + 2, error (' - Grid incompatibility in latitude direction'); end
if sx < imax+2 | sx > imax + 2, error (' - Grid incompatibility in longitude direction'); end
b = a(2:jmax+1, 2:imax+1);
c = flipud(b); c(jmax+1,:) = NaN; c(:,imax+1) = NaN;

% Output filename
% outname = 'rubbish';

% ==================================================
% Topography edit
% ==================================================

% Set up label for colorbar
for i = 1:1:kmax
    if i < 10
        txt = sprintf('k=%d ',i);
    else
        txt = sprintf('k=%d',i);
    end
    cbartxt(i,1:4) = txt;
end
cbartxt(kmax+1,1:4) = 'land';

% Do the topography alteration
fprintf('\n - Topography alteration procedure :\n');
fprintf('   + left button to deepen grid cell\n');
fprintf('   + right button to shallow grid cell\n');
fprintf('   + middle button to finish\n\n');
figure(1); clf
cbar = oceanpal(kmax); cbar(kmax+1,:) = [0 1 0]; colormap(cbar);
t1 = c; t1(t1 > kmax) = kmax + 1;
[sy,sx] = size(c); sy = sy - 1; sx = sx - 1;
flag = 0;
while flag == 0
    figure(1); clf
    pcolor (t1); % axis image;
    caxis ([0.5 (kmax+1.5)]); h = colorbar ('horiz');
    set(h,'XTick',1:9,'XTickLabel',cbartxt);
    title ('Ocean land/sea mask and bathymetry');
    [x,y,button] = ginput(1);
    ix = floor(x); iy = floor(y);
    if ix < 1 | ix > sx | iy < 1 | iy > sy
        fprintf(' - Out of grid range\n');
    elseif button == 2
        flag = 1;
        fprintf('\n - Topography alteration complete\n');
    else
        if t1(iy, ix) > 1 & button == 1
            fprintf('   Deepening cell at (%d, %d)\n', ix, iy);
            t1(iy, ix) = t1(iy, ix) - 1; 
        elseif t1(iy, ix) < kmax+1 & button == 3
            fprintf('   Shallowing cell at (%d, %d)\n', ix, iy);
            t1(iy, ix) = t1(iy, ix) + 1; 
        else
            fprintf('   Cannot deepen/shallow cell at (%d, %d)\n',ix,iy);
        end
    end
    clf    
end
d = t1;

% Set new land to 95
runoff = (d > kmax);
tc = c; tc(tc <= kmax) = 95;
d(runoff) = tc(runoff);

% Copy new topography
% Question here about whether to include/exclude shelves as land
t2 = d;
% t2(d >= kmax) = 1; t2(d < kmax) = 0;
t2(d > kmax) = 1; t2(d <= kmax) = 0;

% ==================================================
% Show the result
% ==================================================

figure(1); clf
cbar = oceanpal(kmax); cbar(kmax+1,:) = [0 1 0]; colormap(cbar);
pcolor (t1); axis image;
caxis ([0.5 (kmax+1.5)]); h = colorbar ('horiz');
set(h,'XTick',1:9,'XTickLabel',cbartxt);
title ('Ocean land/sea mask and bathymetry');
paper; orient landscape;
% set(gcf,'PaperPosition',[1.0 3.5 6.2 4.7]);
fname = sprintf('%s_pic_01',outname);
print ('-depsc', '-painters', fname);

% ==================================================
% Runoff alteration
% ==================================================

% Unspecified cells are those that are newly land after
% topography alteration by the user (or were unspecified,
% value 95, in the original topography).  In this section 
% the user can choose to have these cells assigned by the
% computer, left unspecified for hand-editing later, or
% even raze all of the runoff paths and let the computer
% guess the lot (useful where no runoff mask exists in
% the first place)

fprintf('\n - Would you like to fill in unspecified runoff paths?\n');
fprintf('   0. unspecified runoff paths stay unspecified\n');
fprintf('   1. computer guesses unspecified runoff paths\n');
fprintf('   2. computer guesses *ALL* runoff paths\n');
flag = 0;
while flag == 0
    method = input(' - ');
    if method == 0, runoff = 0; unspec = 0; flag = 1;
        fprintf('\n - Remember to hand-edit runoff paths later\n');
    elseif method == 1,  runoff = 1; unspec = 1; flag = 1;
        fprintf('\n - Unspecified runoff paths guessed (hand editing may be necessary)\n');
    elseif method == 2,  runoff = 1; unspec = 0; flag = 1;
        fprintf('\n - All runoff paths guessed (hand editing will be necessary)\n');
    else    
        fprintf(' - Inappropriate answer - try again\n');
    end
end

q1 = d(1:jmax,1:imax);
if runoff == 1
    q1(q1 > kmax) = 95;
    
    for i = 1:islands
        if i == npole
            % North polar island
            % All runoff flows south
            q1(t4 == npole) = 92;
        elseif i == spole
            % South polar island
            % All runoff flows north
            q1(t4 == spole) = 94;
        else
            % Non-polar islands
            % Runoff flows west on the western side of an island
            % and east on the eastern side of an island
            q2 = fisl(:,:,i);
            [fi, fj] = find(q2 == 1);
            mwest = min(fj);
            meast = max(fj);
            islwidth = meast - mwest + 1;
            if islwidth == 1
                % Island is only one cell wide
                q2(1:2:end,mwest) = q2(1:2:end,mwest) * 91;
                q2(2:2:end,mwest) = q2(2:2:end,mwest) * 93;
            elseif mod(islwidth,2) == 0
                % Island is an even number of cells wide
                hfway = mwest + (islwidth / 2) - 1;
                q2(:,1:hfway) = q2(:,1:hfway) * 93;
                q2(:,hfway+1:end) = q2(:,hfway+1:end) * 91;
            else                
                % Island is an odd number of cells wide
                hfway = mwest + (ceil(islwidth / 2)) - 1;
                q2(:,1:(hfway-1)) = q2(:,1:(hfway-1)) * 93;
                q2(:,(hfway+1):end) = q2(:,(hfway+1):end) * 91;
                q2(1:2:end,hfway) = q2(1:2:end,hfway) * 91;
                q2(2:2:end,hfway) = q2(2:2:end,hfway) * 93;
            end
            q1(q2 > 90) = q2(q2 > 90);
            % Note also that this routine will make a mess of
            % things if your island wraps east-west 
            if mwest == 1 & meast == jmax
                fprintf('\n - Island %d wraps east-west - runoff problems likely!\n');
            end
        end
    end
end

if unspec == 1
    % Only alter cells with value 95 in them (i.e. unspecified)
    q3 = d(1:jmax,1:imax);
    q3(q3 == 95) = q1(q3 == 95);
else
    q3 = q1;
end
d = q3; d(jmax+1,:) = NaN; d(:,imax+1) = NaN;

% ==================================================
% Save the result
% ==================================================

e(2:jmax+1,2:imax+1) = d(1:jmax,1:imax);
% The next lines can cause trouble for maps with a polar ocean ...
% e(1,:) = e(2,:);
% e(jmax+2,:) = e(jmax+1,:);
% ... so replacing them with ...
e(1,:) = 94;      % north flowing runoff at the south pole
% e(imax+2,:) = 92; % south flowing runoff at the north pole
e(jmax+2,:) = 18; % south flowing runoff at the north pole
e(:,1) = e(:,imax+1);
e(:,imax+2) = e(:,2);
f = flipud(e);

fname = sprintf('%s.k1',outname);
fid = fopen(fname,'w');
for j = 1:1:jmax+2
    for i = 1:1:imax+2
        if f(j,i) < 10
            fprintf(fid,'  %d',f(j,i));
        else
            fprintf(fid,' %d',f(j,i));
        end
    end
    fprintf(fid,'\n');
end
% fprintf(fid,'\n');
fclose(fid);

fprintf(' - New K1 file created (%s)\n', fname);

% ==================================================
% Show the land-sea psiles mask
% ==================================================

% Create psiles version of land-sea grid
q1 = flipud(f); q1(q1 < kmax+1) = 0; q1(q1 > kmax) = 1;
q1(jmax+2,:) = 1;
for j = jmax+1:-1:1
    for i = 1:imax
        % Extra offset needed for i position 'cos k1 runs 0:imax+1 in code
        q2(j,i) = max([q1(j,i+1) q1(j,i+2) q1(j+1,i+1) q1(j+1,i+2)]);
    end
end
t2 = q2; t2(jmax+2,:) = NaN; t2(:,imax + 1) = NaN;

% Cutting this for now - unnecessary
figure(1); clf
cmap = [1 1 1; 0 0 0]; colormap(cmap);
pcolor (t2); axis image;
caxis ([-0.5 1.5]); h = colorbar ('horiz');
set(h,'XTick',0:1,'XTickLabel',char('Ocean','Land'));
title ('Ocean land/sea mask');
% paper; orient landscape;
% fname = sprintf('%s_pic_02',outname);
% print ('-dpsc', '-painters', fname);

%%%blah = input (' - Press return to continue ');

% ==================================================
% How many islands?
% ==================================================

% How many islands does the user want to specify?
fprintf('\n - How many islands?\n');
islands = input(' - ');

% ==================================================
% Get user to identify islands
% ==================================================

fprintf('\n - Point to a location within each island in turn\n');
fprintf('\n   Note : the first selected island is *special*\n');
isl = 0;
[sy, sx] = size(t2);
while isl < islands
    [x,y] = ginput(1);
    ix = floor(x); iy = floor(y);
    if ix < 1 | ix > sx | iy < 1 | iy > sy
        fprintf(' - Out of grid range\n');
    else
        if t2(iy, ix) == 0
            fprintf(' - Ocean cell - try again\n');
        elseif t2(iy, ix) == 1
            isl = isl + 1;
            fprintf(' - Island %d identified at (%d, %d)\n', isl, ix, iy);
            islandx(isl) = ix;
            islandy(isl) = iy;
        end
    end
end  

% ==================================================
% Determine bounds of islands and edges
% ==================================================

fprintf('\n - Determining island bounds and edges\n');
t3 = t2(1:jmax+1,1:imax);

for i = 1:islands    
    global map
    map = t3;
    
    global final_map
    final_map = map * 0;
    
    global edge_map
    edge_map = map * 0;
    
    global iter
    iter = 1;
    
    posx = islandx(i);
    posy = islandy(i);
    
    fillmap(posx, posy);
    
    fmap(:,:,i) = map;
    fisl(:,:,i) = final_map;
    fedg(:,:,i) = edge_map;
    
    fprintf('\n - Finished island %d\n',i);
end

% ==================================================
% Set any "unclaimed" land as island 1
% ==================================================

fprintf('\n - Checking for unclaimed land\n');

% Build up a composite "map" based on individual islands
totisl = fmap(:,:,1) .* 0;
for i = 1:1:islands
    totisl = totisl + fisl(:,:,i);
end
totisl(totisl > 0) = 1;

% How does this differ from the actual map?
difisl = fmap(:,:,1) - totisl;
maxdif = max(max(difisl));
if maxdif == 0
    fprintf(' - All land claimed\n');
else
    fprintf(' - Some land unclaimed - setting it to island 1\n');
    q1 = (difisl > 0);
    q2 = fisl(:,:,1);
    q2(q1) = 1;
    fisl(:,:,1) = q2;
end
    
% ==================================================
% Save the result - psiles file
% ==================================================

fprintf('\n - Preparing PSILES file\n',i);

t4 = t3 * 0;
for i = 1:islands
    t1 = fisl(:,:,i);
    t2 = (t1 == 1);
    t4(t2) = i;
end

% Greenland fix
% In normal topography maps, Greenland, while an island,
% is treated as if it were part of the giant island (the
% Americas + Africa + Europe + Asia).  The code above
% doesn't permit this, so a fix is added here such that
% *any* land not identified as a distinct island is
% set up as island 1 (the special island).  As an aside,
% this is also useful if you don't connect North America
% to Asia on the standard topography.
% q1 = fmap(:,:,1);
% q1(t4 > 0) = 0;
% % figure(5); pcolor (q1);
% t4(q1 == 1) = 1;

% t5(2:jmax+1,2:imax+1) = t4;
% % Next lines modified so that east-west axis is mirrored
% % and polar values are land (this bit is tricky and may
% % need hand-editting if there are complex polar islands)
% spole = max(max(t5(2:3,:))); npole = max(max(t5(jmax:jmax+1,:)));
% t5(1,:) = spole; t5(jmax+2,:) = npole;
% t5(:,1) = t5(:,imax+1); t5(:,imax+2) = t5(:,2);
% 
% fprintf(' - North pole = island %d, South pole = island %d\n', ...
%     npole, spole);
% t6 = t5; t6(jmax+3,:) = NaN; t6(:,imax+3) = NaN;
% % figure(6); pcolor (t6);

% clear val
% for j = 1:1:jmax+1
%     for i=2:1:imax+1
%         v1 = [t5(j,i) t5(j+1,i) t5(j,i+1) t5(j+1,i+1)];
%         v2 = max(v1);
%         if v2 > 0
%             val(j,i-1) = v2;
%         else
%             val(j,i-1) = 0;
%         end
%     end
% end

val = t4;

fname = sprintf('%s.psiles',outname);
fid = fopen(fname,'w');
for j = jmax+1:-1:1
    for i = 1:1:imax
        if val(j,i) < 10
            fprintf(fid,'  %d',val(j,i));
        else
            fprintf(fid,' %d',val(j,i));
        end
    end
    fprintf(fid,'\n');
end
% fprintf(fid,'\n');
fclose(fid);

fprintf(' - PSILES file created (%s)\n', fname);

% ==================================================
% Reverse-engineer a K1 islands map
% ==================================================

% Get PSILES grid
s1 = t4;
s1(:,imax+1) = s1(:,1);
% Get K1 grid
s2(2:jmax+1,1:imax) = q3(1:jmax,1:imax);
s2(s2 <=kmax) = 0; s2(s2 > kmax) = 1;
s2(1,:) = 1; s2(jmax+2,:) = 1;

% Reverse PSI points to tracer points
psi(1:jmax,1:imax,1) = s1(1:jmax,1:imax);
psi(1:jmax,1:imax,2) = s1(2:jmax+1,1:imax);
psi(1:jmax,1:imax,3) = s1(1:jmax,2:imax+1);
psi(1:jmax,1:imax,4) = s1(2:jmax+1,2:imax+1);
psi2 = max(psi, [], 3);
psi3(2:jmax+1,1:imax) = psi2(1:jmax,1:imax);
psi3(1,1:imax) = s1(1,1:imax);
psi3(jmax+2,1:imax) = s1(jmax+1,1:imax);

% Put this information onto K1 grid
s3 = (s2 == 1);
s4 = s2*0;
s4(s3) = psi3(s3);
s5 = s4; s5(end+1,:) = NaN; s5(:,end+1) = NaN;

% ==================================================
% Show the result
% ==================================================

fprintf ('\n - Displaying new island map\n');
% t4 = t3 * 0;
% for i = 1:islands
%     tmp = fisl(:,:,i);
%     t4(tmp == 1) = i;
% end
% tmp = sum(fisl, 3);
% t4(tmp > 1) = islands + 1;
% t4(jmax+2,:) = NaN; t4(:,imax+1) = NaN;

% Set up label for colorbar
clear cbartxt
cbartxt(1,1:9) = '  Ocean  ';
for i = 1:1:islands
    if i < 10
        txt = sprintf('Island=%d ',i);
    else
        txt = sprintf('Island=%d',i);
    end
    cbartxt(i+1,1:9) = txt;
end
cbartxt(islands+2,1:9) = 'Try again';

figure(1); clf
cmap = rain(islands+2); colormap(cmap);
pcolor (s5); axis image;
caxis ([-0.5 (islands+1.5)]); h = colorbar ('horiz');
set(h,'XTick',0:(islands+1),'XTickLabel',cbartxt);
title ('Islands identified from topography');
paper; orient landscape; 
% set(gcf,'PaperPosition',[1.0 3.5 6.2 4.7]);
fname = sprintf('%s_pic_02',outname);
print ('-depsc', '-painters', fname);

%%%blah = input (' - Press return to continue ');

% ==================================================
% Determine bounds of islands and edges
% ==================================================

fprintf('\n - Checking island integrity and edges\n');
r1 = s4;

% Increase recursion limit to 1000
set(0,'RecursionLimit',1000);

for i = 1:islands    
    % Set up this island
    r1 = s4 * 0; r1(s4 == i) = 1;
    r2 = s4 * 0; r2(s4 > 0) = 1;
    
    % Count island cells
    scells(i) = sum(r1(r1==1));
    
    % Choose a start cell
    [tj, ti] = find(r1 == 1);
    posx = ti(1); posy = tj(1);
    
    global map
    map = r2;
    
    global final_map
    final_map = map * 0;
    
    global edge_map
    edge_map = map * 0;
    
    global iter
    iter = 1;
    
    fillmap(posx, posy);
    
    qmap(:,:,i) = map;
    qisl(:,:,i) = final_map;
    qedg(:,:,i) = edge_map;
    
    % Count island cells (for island integrity)
    ecells(i) = sum(final_map(final_map==1));

    % Is there are less cells at the end than at the start,
    % that's because this island is fragmented.  For the
    % purposes of further plots, etc. fix this
    if scells(i) > ecells(i)
        qisl(:,:,i) = r1;
    end
    
    fprintf('\n - Finished island %d\n',i);
end

fragisl = 0;
for i = 1:islands    
    if scells(i) > ecells(i)
        fprintf(' - Island %d is fragmented\n', i);
        % Only an issue if this isn't island 1
        if i > 1,
            fragisl = 1;
        end
    else
        fprintf(' - Island %d is contiguous\n', i);
    end
end

% Advise user on which edge routine to choose
if fragisl == 1
    fprintf('\n - Option 1 below is non-viable\n');
else
    fprintf('\n - Both options below are viable\n');
end

% ==================================================
% Choose method of path generation
% ==================================================

fprintf('\n - How would you like to generate your paths?\n');
fprintf('   1. computer automatically finds edges (you edit them)\n');
fprintf('   2. you create straight line edges\n');
flag = 0;
while flag == 0
    method = input(' - ');
    if method == 1, guess = 0; drawpath = 0; flag = 1;
        fprintf('\n - Computer automation chosen\n');
    elseif method == 2, guess = 0; drawpath = 1; flag = 1;
        fprintf('\n - Straight line edges chosen\n');
    else
        fprintf(' - Inappropriate answer - try again\n');
    end
end

% ==================================================
% Alternatively build paths via straight lines
% ==================================================
if drawpath == 1
    
    fprintf('\n - Build up paths as straight lines\n');
    fprintf('   + left button select grid cell\n');
    fprintf('   + right button to exit island\n\n');
    clear nedg
    
    cmap = [0.7 0.7 1; 0 1 0; 1 0 0; 1 1 0; 1 0 1]; colormap(cmap);
    for i = 2:1:islands
        q1 = qmap(:,:,1); q1(end+1,:) = NaN; q1(:,end+1) = NaN;
        q2 = qisl(:,:,i); q2(end+1,:) = NaN; q2(:,end+1) = NaN;
        q4 = q2 * 0;
        q3 = q1 + q2 + (q4*3);
        [sy,sx] = size(qmap(:,:,1));
        flag = 0;
        iter = 0; lastx = NaN; lasty = NaN;
        while flag == 0
            figure(1); clf
            pcolor (q3); axis image;
            caxis ([-0.5 4.5]); h = colorbar ('horiz'); 
            set(h,'XTick',0:1:4,'XTickLabel', ...
                [' Ocean ';' Land  ';'Island ';' Edge  ';'Current']);
            titstr = sprintf('Island %d edge', i); title (titstr);
            if iter == 0
                fprintf(' - Point to start cell\n');
            elseif iter == 1
                fprintf(' - Point to first cell to link to\n');
            else
                fprintf(' - Point to next cell to link to\n');
            end
            [x,y,button] = ginput(1);
            ix = floor(x); iy = floor(y);
            if button == 1
                if ix < 1 | ix > sx | iy < 1 | iy > sy
                    fprintf('   Out of grid range\n');
                else
                    % First check if cell is acceptable
                    if q1(iy, ix) > 0 & button == 1
                        % Cell is land, dump it
                        fprintf('   This cell is a land cell, try again\n');
                    else
                        % Is this the first cell?
                        if iter == 0
                            % Cell is OK, but is first cell
                            fprintf('   First cell acceptable - please continue\n');
                            lastx = ix; lasty = iy;
                            iter = iter + 1;
                        else
                            % Second or further cell - is it in line?
                            if ix ~= lastx & iy ~= lasty
                                fprintf('   This cell is not in line, try again\n');
                            elseif ix == lastx & iy == lasty
                                fprintf('   This cell was chosen last time, try again\n');
                            elseif (ix == sx & lastx == 1) | (ix == 1 & lastx == sx)
                                % Crossed the east-west boundary
                                % ... but did the user just want a line running east-west?
                                tx = sort([ix lastx]);
                                ty = sort([iy lasty]);
                                test = q1(ty(1):ty(2),tx(1):tx(2)); maxtest = max(test);
                                if maxtest > 0
                                    fprintf('   Crossed the east-west boundary, acceptable - please continue\n');
                                    q4(iy,1) = 1; q4(iy,sx) = 1;
                                    lastx = ix; lasty = iy;
                                else
                                    q5 = q4; q5(lasty,lastx) = 0;
                                    test2 = q5(ty(1):ty(2),tx(1):tx(2)); maxtest2 = max(test2);
                                    if maxtest2 > 0
                                        fprintf('   This line crosses the path, assuming island complete\n');
                                        flag = 1;
                                    end
                                    fprintf('   This line is acceptable - please continue\n');
                                    q4(ty(1):ty(2),tx(1):tx(2)) = 1;
                                    lastx = ix; lasty = iy;
                                end
                                iter = iter + 1;
                            else
                                % Cell in line - is there land in the way?
                                tx = sort([ix lastx]);
                                ty = sort([iy lasty]);
                                test = q1(ty(1):ty(2),tx(1):tx(2)); maxtest = max(test);
                                if maxtest > 0
                                    fprintf('   This line crosses land, try again\n');
                                else
                                    q5 = q4; q5(lasty,lastx) = 0;
                                    test2 = q5(ty(1):ty(2),tx(1):tx(2)); maxtest2 = max(test2);
                                    if maxtest2 > 0
                                        fprintf('   This line crosses the path, assuming island complete\n');
                                        flag = 1;
                                    end
                                    fprintf('   This line is acceptable - please continue\n');
                                    q4(ty(1):ty(2),tx(1):tx(2)) = 1;
                                    lastx = ix; lasty = iy;
                                    iter = iter + 1;
                                end
                            end
                        end
                    end
                end
            else
                fprintf(' - Exiting this island\n');
                flag = 1;
            end
            q3 = q1 + q2 + (q4*3);
            if iter > 0, q3(lasty, lastx) = 4; end
            clf    
        end
        nedg(1:sy,1:sx,i) = q4(1:sy,1:sx);
    end 
    % Set new edge data for island 1 to be the automatic edge
    nedg(:,:,1) = qedg(:,:,1);
    % Set edge data to the field generated here
    old_fedg = qedg;
    qedg = nedg;
end

% ==================================================
% Process edge data
% ==================================================

% When processing edge data to get paths, the first thing
% to do is get rid of dangling edge points with only one
% neighbor in its four cell neighborhood

fprintf ('\n - Processing island edges to remove lonely cells\n');

for i = 2:islands
    t1 = qedg(:,:,i);
    flag = 0;
    while flag == 0
        sumt1 = sum(sum(t1));
        t2 = lonely_cell(t1);
        sumt2 = sum(sum(t2));
        if sumt1 == sumt2
            fprintf('\n - Island %d has no lonely cells\n',i);
            flag = 1;    
        else
            fprintf('\n - Island %d has lonely cells (= %d)\n',i,sumt1-sumt2);
        end
        t1 = t2;
    end
    qedg2(:,:,i) = t2;
end

% Set new edge data for island 1 to be the automatic edge
qedg2(:,:,1) = qedg(:,:,1);

% ==================================================
% Check if paths are suitable
% ==================================================

fprintf ('\n - Checking if island paths are suitable\n');

for i = 2:islands
    fprintf(' - Island %d ...\n',i);
    t1 = qedg2(:,:,i);
    [answer] = check_nbors(t1);
    if answer == 1
        fprintf('\n - Island %d path is suitable\n',i);
        goodisl(i) = 1;
    else
        fprintf('\n - Island %d path needs editing\n',i);
        goodisl(i) = 0;
    end
end

% ==================================================
% Get user to manually edit edge data for paths
% ==================================================

% Manually edit edge data to remove parts of the path you
% don't want

fprintf('\n - Remove edge cells to ensure clean path\n');
fprintf('   + left button to flip grid cell state\n');
fprintf('   + right button to exit this island\n\n');

cmap = [1 1 1; 0 0 0]; colormap(cmap);
for i = 2:1:islands
    t1 = qedg2(:,:,i); t1(jmax+3,:) = NaN; t1(:,imax+1) = NaN;
    [sy,sx] = size(t1);
    flag = 0;
    while flag == 0
        figure(1); clf
        pcolor (t1); axis image;
        titstr = sprintf('Island %d edge', i); title (titstr);
        [x,y,button] = ginput(1);
        ix = floor(x); iy = floor(y);
        if ix < 1 | ix > sx | iy < 1 | iy > sy
            fprintf('   Out of grid range\n');
        else
            if t1(iy, ix) == 1 & button == 1
                fprintf('   Killing cell at (%d, %d)\n', ix, iy);
                t1(iy, ix) = 0; 
            elseif t1(iy, ix) == 0 & button == 1 & qmap(iy,ix,i) == 0
                fprintf('   Creating cell at (%d, %d)\n', ix, iy);
                t1(iy, ix) = 1; 
            else
                fprintf('   Cannot kill/create cell at (%d, %d)\n',ix,iy);
            end
        end
        if button > 1, flag = 1; end
        clf    
    end
    qedg3(1:jmax+2,1:imax,i) = t1(1:jmax+2,1:imax);
end
% Set new edge data for island 1 to be the automatic edge
qedg3(:,:,1) = qedg2(:,:,1);

% ==================================================
% Check if paths are suitable
% ==================================================

fprintf ('\n - Checking if island paths are suitable\n');

for i = 2:islands
    fprintf('   Island %d ...\n',i);
    t1 = qedg3(:,:,i);
    [answer] = check_nbors(t1);
    if answer == 1
        fprintf('\n - Island %d path is suitable\n',i);
        goodisl(i) = 1;
    else
        fprintf('\n - Island %d path needs editing\n',i);
        goodisl(i) = 0;
    end
end

% Plot up the paths and islands
isledg = qisl(:,:,1) * 0;
for i = 1:islands
    tmp1 = qisl(:,:,i) * (i*2);
    tmp2 = qedg3(:,:,i) * ((i*2)-1);
    tmp3 = tmp1 + tmp2;
    q1 = find(tmp3 > 0);
    isledg(q1) = tmp3(q1);
end
isledg(isledg == 0) = NaN;
isledg(jmax+3,:) = NaN; isledg(:,imax+1) = NaN;

clear cbartxt
for i = 1:1:islands
    if i < 10
        txt  = sprintf(' Edge %d  ',i);
        txt2 = sprintf('Island %d ',i);    
    else
        txt  = sprintf(' Edge %d ',i);
        txt2 = sprintf('Island %d',i);    
    end
    pos = ((i-1)*2) + 1;
    cbartxt(pos,1:9) = txt;
    cbartxt(pos+1,1:9) = txt2;
end

figure(1); clf
cmap = rain(islands*2); colormap(cmap);
pcolor (isledg); axis image;
caxis ([0.5 ((islands*2)+0.5)]); h = colorbar ('horiz');
set(h,'XTick',1:(islands*2),'XTickLabel',cbartxt);
title ('Islands and island edges (ignore edge 1)');
paper; orient landscape; 
% set(gcf,'PaperPosition',[1.0 3.5 6.2 4.7]);
fname = sprintf('%s_pic_03',outname);
print ('-depsc', '-painters', fname);

%%%blah = input (' - Press return to continue ');

% ==================================================
% If suitable, calculate paths
% ==================================================

fprintf ('\n - Calculating island paths\n');

clear path_d path_x path_y path_data
bot = 1;
for i = 2:islands
    fprintf(' - Island %d ...\n',i);
    [pathx, pathy, pathd, pathmap] = check_path(qedg3(:,:,i));
    szpath = max(size(pathd)); pathlen(i) = szpath;
    path_d(i,1:szpath) = pathd(1, 1:szpath);
    path_x(i,1:szpath) = pathx(1, 1:szpath);
    path_y(i,1:szpath) = pathy(1, 1:szpath);
    top = bot + szpath - 1;
    path_data(1, bot:top) = pathd(1, 1:szpath);
    path_data(2, bot:top) = pathx(1, 1:szpath);
    path_data(3, bot:top) = pathy(1, 1:szpath);
    path_data(1:3,top+1) = -999;
    bot = top + 2;
    
    figure(i+1); clf
    rain(szpath);
    tmp = pathmap; tmp(jmax+2,:) = NaN; tmp(:,imax+1) = NaN;
    tmp(tmp == 0) = NaN;
    pcolor (tmp);
    titstr = sprintf('- Island %d (%d cells)',i,szpath);
    title(titstr);
    colorbar ('horiz');
end
path_data = permute(path_data, [2 1]);

% ==================================================
% Plot this stuff up
% ==================================================

plotmap0 = sum(qisl,3);
plotmap = plotmap0(2:jmax+1,1:imax);
plotmap(jmax+1,:) = NaN; plotmap(:,imax+1) = NaN;
figure(1); clf
cmap = [1 1 1; 0 0 0]; colormap(cmap);
pcolor (plotmap); shading flat; axis image;
hold on;

for i = 2:islands
    clear pointx pointy pointu pointv
    % Get this island's data
    top = bot + pathlen(i) - 1;
    pathd = path_d(i,1:pathlen(i));
    pathx = path_x(i,1:pathlen(i));
    pathy = path_y(i,1:pathlen(i));
    
    % Work out arrow positions, etc.
    for j = 1:pathlen(i)
        if pathd(j) == 1
            pointx(j) = pathy(j)+ 1 ; pointy(j) = pathx(j) + 0.5;
            pointu(j) = 1; pointv(j) = 0;
        elseif pathd(j) == -1
            pointx(j) = pathy(j) + 1; pointy(j) = pathx(j) + 0.5;
            pointu(j) = -1; pointv(j) = 0;
        elseif pathd(j) == 2
            pointx(j) = pathy(j) + 0.5; pointy(j) = pathx(j) + 1;
            pointu(j) = 0; pointv(j) = 1;
        elseif pathd(j) == -2
            pointx(j) = pathy(j) + 0.5; pointy(j) = pathx(j) + 1;
            pointu(j) = 0; pointv(j) = -1;
        end
        if pointx(j) == (imax+1)
            pointx(j) = 1;
        end
    end
    % Need to subtract 1 from pointy 'cos of polar framing (see below)
    quiver(pointx, pointy-1, pointu, pointv, 0.25, 'k-');
end
title ('Paths around islands');
paper; orient landscape; 
% set(gcf,'PaperPosition',[1.0 3.5 6.2 4.7]);
fname = sprintf('%s_pic_04',outname);
print ('-deps', '-painters', fname);

% ==================================================
% Write this stuff out
% ==================================================

fprintf('\n - Writing out path information (except island 1)\n');

fname = sprintf('%s.paths',outname);
fid = fopen(fname,'w');
for i = 2:islands
    fprintf(fid,'%d ',pathlen(i));
end
fprintf(fid,'\n');
fprintf(fid,'\n');
for i = 2:islands
    for j = 1:pathlen(i)
        % Subtract 1 from path_x (really path_j) because polar
        % framing of grid (i.e. putting land points at the north
        % and south poles for earlier operations) adds 1 to 
        % the position of path in the j dimension
        fprintf(fid,'%d %d %d\n',path_d(i,j),path_y(i,j),path_x(i,j)-1);
    end
    fprintf(fid,'\n');
end
fclose(fid);

output = 1;

fprintf(' - Done and dusted\n\n');

% keyboard
