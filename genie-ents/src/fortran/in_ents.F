cmsw
cmsw Reads in restarts
cmsw
         subroutine in_ents(fname)

#include "genie_ents.cmn"
      include 'var_ents.cmn'

      integer i,j,unit

        character*(*) fname

cccccccccccccccc FOR NETCDF
        real, allocatable :: mphoto(:,:),mrespveg(:,:),mleaf(:,:),
     : mrespsoil(:,:),mCveg(:,:),mCsoil(:,:),mfv(:,:),mtqld1(:,:),
     : mtqld2(:,:),msnow(:,:)
c
        real mpco2ld

ccc	data coming in separate variables

        allocate(mphoto(imax,jmax))
        allocate(mrespveg(imax,jmax))
        allocate(mleaf(imax,jmax))
        allocate(mrespsoil(imax,jmax))
        allocate(mCveg(imax,jmax))
        allocate(mCsoil(imax,jmax))
        allocate(mfv(imax,jmax))
        allocate(mtqld1(imax,jmax))
        allocate(mtqld2(imax,jmax))
        allocate(msnow(imax,jmax))

        photo=mphoto
        respveg=mrespveg
        leaf=mleaf
        respsoil=mrespsoil
        Cveg=mCveg
        Csoil=mCsoil
        fv=mfv
        tqld(1,:,:)=mtqld1
        tqld(2,:,:)=mtqld2
        snow=msnow
        pco2ld=mpco2ld
ccccccccccccccccccccccccccccccccccccccccccccccc

cmsw Initialise water bucket capacity

      do i=1,imax
        do j=1,jmax
          if(ents_k1(i,j).gt.ents_kmax)then
             bcap(i,j)=min(k8,k9+(k10*Csoil(i,j)))
cmsw initial roughness length
             z0(i,j)=max(0.001,kz0*Cveg(i,j))
          endif
        enddo
      enddo

      end
