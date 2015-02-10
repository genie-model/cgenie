function [] = fprint_2D_d(PDATA,PNAME)
% fprint_2D_d(PDATA,PNAME)

% initialize local variables
[jmax imax] = size(PDATA);
data_save = flipud(PDATA);
filename = PNAME;

%
fid = fopen(filename,'w');
for j = 1:jmax
    for i = 1:imax
            fprintf(fid,'  %d',data_save(j,i));
    end
    fprintf(fid,'\n');
end
fclose(fid);
