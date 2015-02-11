*
* island.f subroutine to calculate path integral around island.
* Started 7/5/2
* var ds version 10/5/5
*
      subroutine island(ubloc,erisl1,isl,indj)
#include "ocean.cmn"

      integer i, k, lpi, ipi, jpi, isl, indj

      real ubloc(2,0:maxi+1,0:maxj),erisl1,cor,tv1,tv2
      erisl1 = 0.0
      do i=1,npi(isl)
         lpi = lpisl(i,isl)
         ipi = ipisl(i,isl)
         jpi = jpisl(i,isl)
         if(abs(lpi).eq.1)then
            cor = - s(jpi)*0.25*(ubloc(2,ipi,jpi) + ubloc(2,ipi+1,jpi)
     &    + ubloc(2,ipi,jpi-1) + ubloc(2,ipi+1,jpi-1))
         else
            cor = sv(jpi)*0.25*(ubloc(1,ipi-1,jpi) + ubloc(1,ipi,jpi)
     &    + ubloc(1,ipi-1,jpi+1) + ubloc(1,ipi,jpi+1))
         endif
         erisl1 = erisl1 + sign(1,lpi)*(drag(abs(lpi),ipi,jpi)
     &      *ubloc(abs(lpi),ipi,jpi) + cor
     &      - indj*tau(abs(lpi),ipi,jpi)*rh(abs(lpi),ipi,jpi))
     &      *(c(jpi)*dphi*(2.0 - abs(lpi))
     &      + rcv(jpi)*dsv(jpi)*(abs(lpi) - 1.0))
c
c calc tricky bits and add to source term for path integral round
c islands, all sums have at least one element
c
         if(indj.eq.1)then
            if(abs(lpi).eq.1)then
               tv1 = 0.0
               do k=ku(1,ipi,jpi),mk(ipi+1,jpi)
                  tv1 = tv1 + bp(ipi+1,jpi,k)*dz(k)
               enddo
               do k=ku(1,ipi,jpi),mk(ipi,jpi)
                  tv1 = tv1 - bp(ipi,jpi,k)*dz(k)
               enddo
               erisl1 = erisl1 + (sbp(ipi+1,jpi) - sbp(ipi,jpi) + tv1)
     &            *sign(1,lpi)*rh(1,ipi,jpi)
            else
               tv2 = 0.0
               do k=ku(2,ipi,jpi),mk(ipi,jpi+1)
                  tv2 = tv2 + bp(ipi,jpi+1,k)*dz(k)
               enddo
               do k=ku(2,ipi,jpi),mk(ipi,jpi)
                  tv2 = tv2 - bp(ipi,jpi,k)*dz(k)
               enddo
               erisl1 = erisl1 + (sbp(ipi,jpi+1) - sbp(ipi,jpi) + tv2)
     &            *sign(1,lpi)*rh(2,ipi,jpi)
            endif
         endif
      enddo
      end
