function [] = runoff_rnd(k1name)
% RUNOFF_RND cgoldstein runoff assignment
%   RUNOFF_RND(K1) grow 'pseudo-dendritic' water-sheds
%   starting from random locations on the coast.
%   The GOLDSTEIN topography file K1 is used as the starting point;
%   all ocea bathymetry information is preserved, but the runoff
%   direction are replaced
%
%   RUNOFF_RND(K1) produces drainage basins probably as good
%   a guess as any if you know nothing a priori about the paleo-topography.
%   By growing 'rivers' out from the coast, and by only adding runoff
%   runoff directions that point either to the ocean or to a pre-existing
%   adjacent river cell, everything must a priori connect to the ocean
%   (i.e., there no inland lakes should form, or other drainage anomalies),
%   although it is quite possible to have pretty stupid-tortuous river
%   paths develop.
%   However, you can define water-shed boundaries (e.g., mountain chains)
%   by marking these in to the input (draft) K1 file using a designation
%   of '99'. RUNOFF_RND(K1) will then ensure that no river crosses
%   such barriers. At the end, the barrier of '99' cells is
%   randomly assigned runoff directions into any of the surrounding basins.
%
%   RUNOFF_RND(K1) can handle; pole-to-pole, and equator-to-equator
%   continents, as well as n islands. The only thing you have to be aware
%   of is when there is a continent that crosses the map boundary (i=0).
%   In this event, the edge of the World (edge of map) becomes a de facto
%   (linear-meridional) watershed.

% load data and set array dimensions
k1_old = load(k1name,'-ascii');
k1_size = size(k1_old);
j_max = k1_size(1,1);
i_max = k1_size(1,2);
k1_new = zeros(j_max,i_max);
k1_set = zeros(j_max,i_max);
k1_mask = ones(j_max,i_max);
k1_col = NaN(j_max,i_max);
k1_u = zeros(j_max,i_max);
k1_v = zeros(j_max,i_max);

% assign tempoary North and South pole pseudo-data
k1_new(1,:) = 99;
k1_new(:,1) = 99;
k1_set(1,:) = 1;
k1_set(:,1) = 1;
k1_new(j_max,:) = 99;
k1_new(:,i_max) = 99;
k1_set(j_max,:) = 1;
k1_set(:,i_max) = 1;

% COPY WET GRID CELLS
% set grid search pattern
k1_search_order = rand(j_max,i_max);
k1_search_order(1,:) = -1;
k1_search_order(j_max,:) = -1;
k1_search_order(:,1) = -1;
k1_search_order(:,i_max) = -1;
% grid search
[C i] = max(max(k1_search_order));
[C j] = max(max(k1_search_order'));
while C >= 0.0
    %disp([num2str(i) ',' num2str(j) ' / ' num2str(C)]);
    if k1_old(j,i) < 90
        % mark wet grid point as set and searched
        k1_set(j,i) = 1;
        k1_search_order(j,i) = -1;
        % copy topography information from input topo
        k1_new(j,i) = k1_old(j,i);
    elseif k1_old(j,i) == 99
        % mark dry grid point as searched AND (finally) set
        k1_set(j,i) = 1;
        k1_search_order(j,i) = -1;
        % assign topo label
        k1_new(j,i) = k1_old(j,i);
    else
        % mark dry grid point as searched but not (finally) set
        k1_search_order(j,i) = -1;
        % assign topo label
        k1_new(j,i) = 99;
    end
    [C i] = max(max(k1_search_order));
    [C j] = max(max(k1_search_order'));
end

% ASSIGN NON WATER-SHED RUN-OFF
% set grid search pattern
k1_search_order = rand(j_max,i_max);
k1_search_order(1,:) = -1;
k1_search_order(j_max,:) = -1;
k1_search_order(:,1) = -1;
k1_search_order(:,i_max) = -1;
% grid search
[C i] = max(max(k1_search_order));
[C j] = max(max(k1_search_order'));
n_col = 1;
while C >= 0.0
    %disp([num2str(i) ',' num2str(j) ' / ' num2str(C) ' / ' num2str(k1_set(j,i)) ' / ' num2str(k1_new(j,i))]);
    if k1_set(j,i) == 0
        % un-assigned (dry) cell
        % find the coast or the nearest water-shed boundary
        while k1_set(j,i) == 0
            % first look around - check the cell values in all 4 directions
            k1_try(1) = k1_new(j-1,i);
            k1_try(2) = k1_new(j,i+1);
            k1_try(3) = k1_new(j+1,i);
            k1_try(4) = k1_new(j,i-1);
            % what is there?
            if min([k1_try]) < 90
                % BEST: found some coast! (yipee!)
                while k1_set(j,i) == 0
                    [j_probe,i_probe,dir_probe,j_dir,i_dir] = ijprobe(j,i);
                    if k1_new(j_probe,i_probe) < 90
                        % assign runoff direction towards coast
                        k1_new(j,i) = dir_probe;
                        k1_set(j,i) = 1;
                        k1_search_order(j,i) = -1;
                        k1_col(j,i) = n_col;
                        n_col = n_col + 1;
                        k1_u(j,i) = i_dir;
                        k1_v(j,i) = j_dir;
                    end
                end
            elseif min([k1_try]) < 95
                % 2nd BEST: found a water-shed! (pretty good)
                while k1_set(j,i) == 0
                    [j_probe,i_probe,dir_probe,j_dir,i_dir] = ijprobe(j,i);
                    if k1_new(j_probe,i_probe) < 95
                        % assign runoff direction in the direction of
                        % the water-shed
                        k1_new(j,i) = dir_probe;
                        k1_set(j,i) = 1;
                        k1_search_order(j,i) = -1;
                        k1_col(j,i) = k1_col(j_probe,i_probe);
                        k1_u(j,i) = i_dir;
                        k1_v(j,i) = j_dir;
                    end
                end
            else
                % P*SS POOR: found absolutely zip :(
                % => try moving to an adjacent cell in a random direction
                % allow 100 (v. generous) tries to find somewhere to move to
                % (in a perfect alternative universe it would only take a
                % max of 4 tries to try all 4 possible directions in
                % turn at random, but the world is not perfect and there is
                % still a ,inute (but finite) probability that everything
                % hangs here ...)
                count = 0;
                while (count < 64)
                    [j_probe,i_probe,dir_probe,j_dir,i_dir] = ijprobe(j,i);
                    count = count + 1;
                    if k1_set(j_probe,i_probe) == 0
                        i = i_probe;
                        j = j_probe;
                        break
                    end
                end
            end
        end
    else
        k1_search_order(j,i) = -1;
    end
    [C i] = max(max(k1_search_order));
    [C j] = max(max(k1_search_order'));
end

% ASSIGN ANY WATER-SHED RUN-OFF
% set grid search pattern
k1_search_order = rand(j_max,i_max);
k1_search_order(1,:) = -1;
k1_search_order(j_max,:) = -1;
k1_search_order(:,1) = -1;
k1_search_order(:,i_max) = -1;
% grid search
[C i] = max(max(k1_search_order));
[C j] = max(max(k1_search_order'));
while C >= 0.0
    if k1_new(j,i) == 99
        % un-assign the cell because it is a designated water-shed
        % that has not yet been assigned to a runoff basin
        k1_set(j,i) = 0;
        while k1_set(j,i) == 0
            % find the coast or the nearest water-shed boundary!
            % (1) look around - check all 4 directions
            k1_try(1) = k1_new(j-1,i);
            k1_try(2) = k1_new(j,i+1);
            k1_try(3) = k1_new(j+1,i);
            k1_try(4) = k1_new(j,i-1);
            % assume a water-shed ...
            while k1_set(j,i) == 0
                [j_probe,i_probe,dir_probe,j_dir,i_dir] = ijprobe(j,i);
                if k1_new(j_probe,i_probe) < 95
                    % assign runoff direction in the direction of
                    % the water-shed
                    k1_new(j,i) = dir_probe;
                    k1_set(j,i) = 1;
                    k1_search_order(j,i) = -1;
                    k1_col(j,i) = k1_col(j_probe,i_probe);
                    k1_u(j,i) = i_dir;
                    k1_v(j,i) = j_dir;
                end
            end
        end
    else
        k1_search_order(j,i) = -1;
    end
    [C i] = max(max(k1_search_order));
    [C j] = max(max(k1_search_order'));
end

% some checks
if sum(sum(k1_set)) < i_max*j_max
    disp('WARNING: not all grid points set');
    disp('Check specified drainage basin boundaries (such a 4 cells meeting at a point)');
end

% assign East and West grid pseudo-data
k1_new(1,:) = 18;
k1_new(j_max,:) = 94;
k1_new(:,1) = k1_new(:,2);
k1_new(:,i_max) = k1_new(:,i_max-1);

% save data - k1 file
fid = fopen([k1name '.NEW'],'w');
for i = 1:i_max
    for j = 1:j_max
        if k1_new(i,j) < 10
            fprintf(fid,'  %d',k1_new(i,j));
        else
            fprintf(fid,' %d',k1_new(i,j));
        end
    end
    fprintf(fid,'\n');
end
fclose(fid);
% save data - drain basin colors
fid = fopen([k1name '.COL'],'w');
for i = 1:i_max
    for j = 1:j_max
        if k1_col(i,j) < 10
            fprintf(fid,'   %d',k1_col(i,j));
        elseif k1_col(i,j) < 100
            fprintf(fid,'  %d',k1_col(i,j));
        else
            fprintf(fid,' %d',k1_col(i,j));
        end
    end
    fprintf(fid,'\n');
end
fclose(fid);
% plot somethink pretty
figure;
hold on;
colormap('colorcube');
pcolor(flipdim(k1_col(1:j_max-1,2:i_max),1));
[X Y] = meshgrid(1.5:1:j_max-1.5,1.5:1:i_max-1.5); 
quiver(X,Y,flipdim(k1_u(2:j_max-1,2:i_max-1),1),-flipdim(k1_v(2:j_max-1,2:i_max-1),1),0.5,'-k','LineWidth',1.0);
axis([1 i_max 1 j_max]);
filename = [k1name '.NEW' '.ps'];
print('-dpsc2', filename);


