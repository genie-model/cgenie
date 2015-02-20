function [] = fprint_1Dn_d(PDATA,PNAME)
% fprint_1D2_d(Pkmax,PDATA,PNAME)

% initialize local variables
data_save = PDATA;
data_size = size(data_save);
mmax = data_size(1);
nmax = data_size(2);
filename = PNAME;

%
fid = fopen(filename,'w');
for m = 1:mmax
    for n = 1:nmax
            fprintf(fid,'  %d',data_save(m,n));
    end
    fprintf(fid,'\n');
end
fclose(fid);
