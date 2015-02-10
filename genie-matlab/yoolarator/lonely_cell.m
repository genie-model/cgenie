function new_edge = lonely_cell(edge_map)
%    Tracks down cells with one only neighbour in four cell 'hood.
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
    % If only one neighbour, cell dies
    if nbors == 1, new_edge(tx,ty) = 0; end
end
