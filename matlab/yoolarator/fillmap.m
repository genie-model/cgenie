function fillmap(testx, testy)
%    Test fill function
%
%    Andrew Yool (axy@soc.soton.ac.uk), 12th July 2004.

global map
global final_map
global edge_map
global iter

% Use eight cell neighbourhood?
eightcell = 1;
% Wrap east-west boundary?
eastwest = 1;

% How big is the map?
[ymax, xmax] = size(map);

%fprintf('%d, %d\n', testx, testy);
fprintf('.');
if mod(iter, 70) == 0, fprintf('\n'); end
iter = iter + 1;

if map(testy, testx) == 1 & final_map(testy, testx) == 0
	% This is a contiguous cell - index it
	final_map(testy, testx) = 1;
	
	% Check its perimeters for cells with a different value
	if testx > 1, if map(testy,(testx-1)) ~= 1, edge_map(testy,(testx-1)) = 1; end; end;
	if testx > 1 & testy > 1, if map((testy-1),(testx-1)) ~= 1, edge_map((testy-1),(testx-1)) = 1; end; end;
	if testx > 1 & testy < ymax, if map((testy+1),(testx-1)) ~= 1, edge_map((testy+1),(testx-1)) = 1; end; end;
	if testx < xmax, if map(testy,(testx+1)) ~= 1, edge_map(testy,(testx+1)) = 1; end; end;
	if testx < xmax & testy > 1, if map((testy-1),(testx+1)) ~= 1, edge_map((testy-1),(testx+1)) = 1; end; end;
	if testx < xmax & testy < ymax, if map((testy+1),(testx+1)) ~= 1, edge_map((testy+1),(testx+1)) = 1; end; end;
	if testy > 1, if map((testy-1),testx) ~= 1, edge_map((testy-1),testx) = 1; end; end;
	if testy < ymax, if map((testy+1),testx) ~= 1, edge_map((testy+1),testx) = 1; end; end;
	% And again for east-west boundary if applicable
	if eastwest == 1 & testx == 1
		if map(testy,xmax) ~= 1, edge_map(testy,xmax) = 1; end;
		if testy > 1, if map((testy-1),xmax) ~= 1, edge_map((testy-1),xmax) = 1; end; end;
		if testy < ymax, if map((testy+1),xmax) ~= 1, edge_map((testy+1),xmax) = 1; end; end;
	end
	if eastwest == 1 & testx == xmax
		if map(testy,1) ~= 1, edge_map(testy,1) = 1; end;
		if testy > 1, if map((testy-1),1) ~= 1, edge_map((testy-1),1) = 1; end; end;
		if testy < ymax, if map((testy+1),1) ~= 1, edge_map((testy+1),1) = 1; end; end;
	end				
	
	% Now recursively search neighbouring cells
	
	% West
	t1 = testx - 1; t2 = testy;
	if t1 > 0
		fillmap(t1, t2);
	end
	% East
	t1 = testx + 1; t2 = testy;
	if t1 < (xmax+1)
		fillmap(t1, t2);
	end
	% South
	t1 = testx; t2 = testy - 1;
	if t2 > 0
		fillmap(t1, t2);
	end
	% North
	t1 = testx; t2 = testy + 1;
	if t2 < (ymax+1)
		fillmap(t1, t2);
	end

	% Are we examining the eight cell neighbourhood?
	if eightcell == 1
		% North-west
		t1 = testx - 1; t2 = testy + 1;
		if t1 > 0 & t2 < (ymax+1)
			fillmap(t1, t2);
		end
		% South-west
		t1 = testx - 1; t2 = testy - 1;
		if t1 > 0 & t2 > 0
			fillmap(t1, t2);
		end
		% North-east
		t1 = testx + 1; t2 = testy + 1;
		if t1 < (xmax+1) & t2 < (ymax+1)
			fillmap(t1, t2);
		end
		% South-east
		t1 = testx + 1; t2 = testy - 1;
		if t1 < (xmax+1) & t2 > 0
			fillmap(t1, t2);
		end
	end
	
	% Does the map wrap east-west?
	if eastwest == 1
		% Jump west
		if testx == 1
			t1 = xmax; t2 = testy;
			fillmap(t1, t2);
		end
		% Jump east
		if testx == xmax
			t1 = 1; t2 = testy;
			fillmap(t1, t2);
		end
	end
	
else
	
	return
	
end
