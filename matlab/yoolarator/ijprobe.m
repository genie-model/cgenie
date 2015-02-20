function [j_out,i_out,direction_out,j_dir,i_dir] = ijprobe(j_in,i_in)
% IJPROBE (N, W, E, W,) runoff search and allocation
i_out = i_in;
j_out = j_in;
i_dir = 0;
j_dir = 0;
direction = rand(1,2) - 0.5;
if direction(1,1) > 0.0
    if direction(1,2) > 0.0
        % E
        i_out = i_in + 1;
        direction_out = 91;
        i_dir = 1;
    else
        % W
        i_out = i_in - 1;
        direction_out = 93;
        i_dir = -1;
    end
else
    if direction(1,2) > 0.0
        % S
        j_out = j_in + 1;
        direction_out = 92;
        j_dir = 1;
    else
        % N
        j_out = j_in - 1;
        direction_out = 94;
        j_dir = -1;
    end
end