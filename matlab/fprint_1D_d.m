function [] = fprint_1D_d(PDATA,PNAME)
% fprint_1D_d(Pkmax,PDATA,PNAME)

% initialize local variables
data_save = PDATA;
kmax = length(data_save);
filename = PNAME;

%
fid = fopen(filename,'w');
for k = 1:kmax
    fprintf(fid,'  %d\n',data_save(k));
end
fclose(fid);
