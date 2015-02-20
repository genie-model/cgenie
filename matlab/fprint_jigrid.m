function [] = fprint_jigrid(PDATA,PNAME,PFORMAT)
% fprint_jigrid(PDATA,PNAME,PFORMAT)

% initialize local variables
[jmax imax] = size(PDATA);
data_save = flipud(PDATA);
data_save = PDATA;
filename = PNAME;
format = PFORMAT;
%
fid = fopen(filename,'w');
for j = jmax:-1:1
    for i = 1:imax
            fprintf(fid,format,data_save(j,i));
    end
    fprintf(fid,'\n');
end
fclose(fid);
