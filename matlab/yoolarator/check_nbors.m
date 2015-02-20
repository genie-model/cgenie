function [flaw] = check_nbors(edge_map)
%    Examines four cell neighbourhood for other cells.
%
%    Andrew Yool (axy@soc.soton.ac.uk), 15th July 2004.

% Get the size of the input grid
[sx, sy] = size(edge_map);
% Find where the edge cells are
[px, py] = find(edge_map);
% Find out how many edge cells there are
[t1, t2] = size(px);
cells = t1;

% Set up output array
new_edge = edge_map;

% Loop through cells counting four cell neighbours
duffcells = 0;
for i = 1:1:cells
    tx = px(i); ty = py(i);
    nbors = 0;
    % Standard four cell neighbourhood
    if ty < sy, if edge_map(tx,ty+1) == 1, nbors = nbors + 1; end; end
    if tx < sx, if edge_map(tx+1,ty) == 1, nbors = nbors + 1; end; end
    if ty > 1,  if edge_map(tx,ty-1) == 1, nbors = nbors + 1; end; end
    if tx > 1,  if edge_map(tx-1,ty) == 1, nbors = nbors + 1; end; end
    % East-west boundary cells
    if ty == 1, if edge_map(tx,ty) == 1, nbors = nbors + 1; end; end
    if ty == sy, if edge_map(tx,1) == 1, nbors = nbors + 1; end; end
    % If cell has fewer or more than two neighbours ...
    if nbors <2
        fprintf('Cell (%d, %d) has too few (%d) neighbours\n',ty,tx,nbors);
        duffcells = duffcells + 1;
    elseif nbors >2
        fprintf('Cell (%d, %d) has too many (%d) neighbours\n',ty,tx,nbors);
        duffcells = duffcells + 1;
    else
        fprintf('Cell (%d, %d) has two neighbours\n',ty,tx);
    end
end

% Report back to user
fprintf('\nThis island edge has %d cells with too few/many neighbours\n',duffcells);
if duffcells > 0
    fprintf('\nIsland edge array needs editing\n');
    flaw = 0;
else
    fprintf('\nExcellent!  Island edge array acceptable\n');
    flaw = 1;
end
