function [pathx, pathy, pathd, pathmap] = check_path(edge_map)
%    Checks path to ensure that each cell has only 2 neighbours.
%
%    Andrew Yool (axy@soc.soton.ac.uk), 17th August 2004.

% Get the size of the input grid
[sx, sy] = size(edge_map);
% Find where the edge cells are
[px, py] = find(edge_map);
% Find out how many edge cells there are
[t1, t2] = size(px);
cells = t1;

% Work out most NW cell
[miny, indy] = min(py);
findy = (py == miny);
px2 = px; px2(findy == 0) = 0;
[maxx, indx] = max(px2);

nwcell = [px(indx) py(indy)];

% Initialise path arrays
firstx = nwcell(1); firsty = nwcell(2);
lastx = firstx; lasty = firsty;
i = 1;
endflag = 0;

% Initialise path variables
increx = [1 0 -1 0];
increy = [0 1 0 -1];
pathmap = edge_map * 0;
lastcell = 1;

% Loop through cells finding four cell neighbours
while endflag == 0
    tx = lastx; ty = lasty;
    pathmap(lastx, lasty) = i;
    nbors = [0 0 0 0];
    % Standard four cell neighbourhood
    if ty < sy, if edge_map(tx,ty+1) == 1, nbors(2) = 1; end; end
    if tx < sx, if edge_map(tx+1,ty) == 1, nbors(1) = 1; end; end
    if ty > 1,  if edge_map(tx,ty-1) == 1, nbors(4) = 1; end; end
    if tx > 1,  if edge_map(tx-1,ty) == 1, nbors(3) = 1; end; end
    % East-west boundary cells
    if ty == 1, if edge_map(tx,sy) == 1, nbors(4) = 1; end; end
    if ty == sy, if edge_map(tx,1) == 1, nbors(2) = 1; end; end
    % Blank neighbours if this is the first cell
    if i == 1
        nbors(1) = 0; nbors(4) = 0;
    end
    % Blank last neighbour
    nbors(lastcell) = 0;
    % Check that only one neighbour remains
    if sum(nbors) > 1
        nbors(2) = 0;
    end
    % Act on neighbourhood information
    % Need to note the interface rather than the cell!
    if nbors(1) == 1
        % North
        pathx(i) = lastx; pathy(i) = lasty;
        pathd(i) = +2; lastcell = 3;
        lastx = lastx + 1; lasty = lasty;
    elseif nbors(2) == 1
        % East
        pathx(i) = lastx; pathy(i) = lasty;
        pathd(i) = +1; lastcell = 4;
        lastx = lastx; lasty = lasty + 1;
    elseif nbors(3) == 1
        % South
        pathx(i) = lastx - 1; pathy(i) = lasty;
        pathd(i) = -2; lastcell = 1;
        lastx = lastx - 1; lasty = lasty;
    elseif nbors(4) == 1
        % West
        pathx(i) = lastx; pathy(i) = lasty - 1;
        pathd(i) = -1; lastcell = 2;
        lastx = lastx; lasty = lasty - 1;
    end
    % Check if this means the cell has cross the east-west boundary
    if lasty == sy + 1
        lasty = 1;
    elseif lasty == 0
        lasty = sy;
    end
    if pathy(i) == sy + 1
        pathy(i) = 1;
    elseif pathy(i) == 0
        pathy(i) = sy;
    end
    % Check if we're back at the start cell
    if lastx == firstx & lasty == firsty
        fprintf('Initial cell reached - routine stopping\n');
        endflag = 1;
    end
    i = i + 1;
end
