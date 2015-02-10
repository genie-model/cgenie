function [] = fprint_1D2_d(PDATA,PNAME)
% fprint_1D2_d(Pkmax,PDATA,PNAME)

% initialize local variables
data_save = PDATA;
data_size = size(data_save);
kmax = data_size(1);
filename = PNAME;

%
fid = fopen(filename,'w');
for k = 1:kmax
    fprintf(fid,'  %d',data_save(k,1));
    fprintf(fid,'  %d',data_save(k,2));
    fprintf(fid,'\n');
end
fclose(fid);
