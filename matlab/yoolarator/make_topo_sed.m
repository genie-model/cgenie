function [] = make_topo_sed(KMAX,KMAX_FILTER,K1IN,K1OUT)

% load k1 file
seds=load(K1IN);
% calculate ocean layer depth distribution
D=-get_gdep(KMAX,0.1,5000.0);
% extend for calculating mask
D(KMAX+2) = 1.0;
% replace shallow ocean areas with depth 'KMAX+1' (which will correspond to 0 m)
seds(find(seds(:,:)>(KMAX_FILTER-1)))=KMAX+1;
% replace land areas with depth 'KMAX+1' (which will correspond to 0 m)
seds(find(seds(:,:)>(KMAX+1)))=KMAX+1;
% replace ocean level with depth (m)
seds_D=D(seds);
% save sediment depth file
save([K1OUT '.dat.'],'seds_D','-ascii');
% replace ocean level with mask
seds(find(seds(:,:)<KMAX_FILTER))=KMAX+2;
% replace ocean level with depth (m)
seds_D=D(seds);
% save sediment mask file
save([K1OUT '.mask.'],'seds_D','-ascii');
