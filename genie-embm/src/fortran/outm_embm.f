c subroutine outm.f writes out data for goldstein last change  6/6/95
c expanded to write out atmos and sea ice data (Bob 10/5/02)
c
c AY (02/12/03) : contracted to excise GOLDSTEIN data
c
c RM (16/05/05) : edited for variable sin(lat) resolution (from NRE, 6/12/04)
c
      subroutine outm_embm(unit)

#include "embm.cmn"

      integer i, j, l, unit
      real    tmp_val(2)
      real    area

c       jdannan testing a better write out
        write(unit,fmt='(e24.15)')tq

c     AY (08/03/04) : write out averages for restart checks
      if (debug_loop) write(*,220) 'Avg T','Avg Q'
c
c     Clear temporary variables
      do l=1,2
         tmp_val(l) = 0
      enddo
c
c     Sum layer state variables and flow field
      area = 0.
      do j=1,jmax
         do i=1,imax
            area = area + ds(j)
            do l=1,2
               tmp_val(l) = tmp_val(l) + tq(l,i,j)*ds(j)
            enddo
         enddo
      enddo
c
c     Print average values out
      if (debug_loop) write(*,210) tmp_val(1)/area,
     +     (tmp_val(2)/area)*1000.

 210  format(2f13.9)
 220  format(2a13)

      end
