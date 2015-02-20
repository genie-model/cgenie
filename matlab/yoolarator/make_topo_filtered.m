function [] = make_filtered_topo(k1name)
% MAKE_FILTERED_TOPO
%   MAKE_FILTERED_TOPO(K1) filter the ocean floor topography
%   to remove single cell 'holes'.
%   The GOLDSTEIN topography file K1 is used as the starting point.

% INITIALIZE AND LOAD TOPO
%
close all;
% set parameters
par_k_max = 15;
par_n_neigh = 3;
par_opt1 = true;
par_opt2 = true;
% load data and set array dimensions
k1_old = load([k1name '.k1'],'-ascii');
k1_size = size(k1_old);
j_max = k1_size(1,1);
i_max = k1_size(1,2);
% plot somethink pretty
k1_plot = k1_old;
k1_plot(find(k1_plot > 90)) = NaN;
figure;
hold on;
colormap('jet');
pcolor(flipdim(k1_plot(1:j_max-1,2:i_max),1));
[X Y] = meshgrid(1.5:1:j_max-1.5,1.5:1:i_max-1.5); 
axis([1 i_max 1 j_max]);
filename = [k1name '.UNFILTERED' '.ps'];
print('-dpsc2', filename);

% LOOP
%
C = true;
loop_count = 0;
k1_new = k1_old;
while C,
    C = false;
    loop_count = loop_count + 1;
    for i = 2:i_max-1,
        for j = 2:j_max-1,
            % check for ocean cell
            if k1_new(j,i) < 90
                % first look around - check the cell values in all 4 directions
                k1_neigh(1) = k1_new(j-1,i);
                k1_neigh(2) = k1_new(j,i+1);
                k1_neigh(3) = k1_new(j+1,i);
                k1_neigh(4) = k1_new(j,i-1);
                % find land neighbours
                k1_neigh_l = k1_neigh(find(k1_neigh > 90));
                k1_neigh_o = k1_neigh(find(k1_neigh < 90));
                % find shallower and deeper neighbours
                k1_neigh_s = k1_neigh_o(find(k1_neigh_o > k1_new(j,i)));
                k1_neigh_d = k1_neigh_o(find(k1_neigh_o < k1_new(j,i)));
                % STEP #0 -- check for k too shallow
                if k1_new(j,i) > par_k_max,
                    k1_new(j,i) = k1_new(j,i)-1;
                    C = true;
                end
                % STEP #1 -- fill in holes
                % find shallower neighbours + land
                if ( (length(k1_neigh_s) >= par_n_neigh) || ( (length(k1_neigh_l) >= 2) && (length(k1_neigh_s) >= 1) && par_opt1)  || ( (length(k1_neigh_l) == 1) && (length(k1_neigh_s) >= 2)  && par_opt2) ),
                    disp(['MAKE SHALLOWER :: ' 'Loop count = ' num2str(loop_count) ' @ (' num2str(i) ',' num2str(j) ',' num2str(k1_new(j,i)) ')' '; s = ' num2str(length(k1_neigh_s)) '; l = ' num2str(length(k1_neigh_l))])
                    if (k1_new(j,i) < par_k_max),
                        k1_new(j,i) = k1_new(j,i) + 1;
                        C = true;
                    end
                end
                % STEP #2 -- smooth down peaks
                % find deeper neighbours + land
                if ( (length(k1_neigh_d) > par_n_neigh)  || ( (length(k1_neigh_l) >= 2) && (length(k1_neigh_d) > 1)  && par_opt1) || ( (length(k1_neigh_l) == 1) && (length(k1_neigh_d) >= par_n_neigh)  && par_opt2) ),
                    disp(['MAKE DEEPER :: ' 'Loop count = ' num2str(loop_count) ' @ (' num2str(i) ',' num2str(j) ',' num2str(k1_new(j,i)) ')' '; d = ' num2str(length(k1_neigh_d)) '; l = ' num2str(length(k1_neigh_l))])
                    if (k1_new(j,i) > 1), 
                        k1_new(j,i) = k1_new(j,i) - 1; 
                        C = true;
                    end
                end
            end
        end
    end
    % assign East and West grid pseudo-data
    k1_new(:,1) = k1_new(:,i_max-1);
    k1_new(:,i_max) = k1_new(:,2);
end

% SAVE
% save data - k1 file
fid = fopen([k1name '.FILTERED.k1'],'w');
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
% plot somethink pretty
k1_plot = k1_new;
k1_plot(find(k1_plot > 90)) = NaN;
figure;
hold on;
colormap('jet');
pcolor(flipdim(k1_plot(1:j_max-1,2:i_max),1));
[X Y] = meshgrid(1.5:1:j_max-1.5,1.5:1:i_max-1.5); 
axis([1 i_max 1 j_max]);
filename = [k1name '.FILTERED' '.ps'];
print('-dpsc2', filename);
