c
c diagend.f end-of-run diagnostics for c-goldstein created 2/9/2 nre
c
c tsdata and tqdata contain observational estimates for ts and tq
c err is the mismatch between model and data weighted by errw
c diagnostic deep temperatures for exact comparison with Jia (2003) 16/6/3
c
c AY (05/12/03) : altered for genie-goldstein
c		  references to GOLDSTEIN and sea-ice excised
c                 contains many diagnostics - these need to go somewhere!

      subroutine diagend_embm

      use genie_util, ONLY: check_unit, check_iostat

#include "embm.cmn"

      real err
      real err3, err4, err_embm
      real tqdata(2,maxi,maxj)
      real errwtq(2)
      real tqav(2), tqvar(2)

      integer i, j, l, ios

      data tqav, tqvar/4*0.0/

c axes
      real lon(maxi),lat(maxj)

c If 'tsinterp' is '.true.': i) discontinue writing out of model-data
c field, ii) replace error score with the score calculated using the
c 'err_gold(...)' function further below
      if (.not.tqinterp) then
c read interpolated Levitus and NCEP data

      call check_unit(32,__LINE__,__FILE__)
      open(32,file=indir_name(1:lenin)//tdatafile(1:lentdata),
     &     iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      read(32,*,iostat=ios)((tqdata(1,i,j),i=1,imax),j=1,jmax)
      call check_iostat(ios,__LINE__,__FILE__)
      close(32,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

      call check_unit(33,__LINE__,__FILE__)
      open(33,file=indir_name(1:lenin)//qdatafile(1:lenqdata),
     &     iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      read(33,*,iostat=ios)((tqdata(2,i,j),i=1,imax),j=1,jmax)
      call check_iostat(ios,__LINE__,__FILE__)
      close(33,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

      do j=1,jmax
         do i=1,imax
            tqdata(2,i,j) = tqdata(2,i,j)*1e-3
         enddo
      enddo

c calculate weights based on variance of data NB not real spatial but
c computational spatial

      do j=1,jmax
         do i=1,imax
            do l=1,2
               tqav(l) = tqav(l) + tqdata(l,i,j)
               tqvar(l) = tqvar(l) + tqdata(l,i,j)*tqdata(l,i,j)
            enddo
         enddo
      enddo

      do l=1,2
         tqav(l) = tqav(l)/(imax*jmax)
         tqvar(l) = tqvar(l)/(imax*jmax) - tqav(l)*tqav(l)
      enddo

c specify weights

      errwtq(1) = 1.0/tqvar(1)
      errwtq(2) = 1.0/tqvar(2)

c calculate error compared to observations (!)

      call check_unit(25,__LINE__,__FILE__)
      open(25,file=outdir_name(1:lenout)//'tmp.err',iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

      err = 0.
      do j=1,jmax
         do i=1,imax
            do l=1,2
               err = err + errwtq(l)*(tq(l,i,j) - tqdata(l,i,j))**2
            enddo
            if (debug_init)
     & write(25,10)(tq(l,i,j) - tqdata(l,i,j),l=1,2)
         enddo
      enddo
   10 format(e15.5)
      close(25,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

      err = sqrt(err/(imax*jmax*2))
      if (debug_init) print*,
     & 'EMBM : weighted r.m.s. model-data error ',err
      else
         if (debug_init) print*,
     & "Writing out of model-data error fields (file"//
     $        " 'tmp.err') is inactive when observational dataset"//
     $        " is interpolated at runtime (i.e., 'tqinterp' is"//
     &        " '.true.')."
      endif

c AP (03/08/06) : Call external error function
c                 Should return identical result
      do i=1,imax
         lon(i)=180.0*(phi0+(i-0.5)*dphi)/pi
      enddo
      do j=1,jmax
         lat(j)=180.0*asin(s(j))/pi
      enddo
      err3 = err_embm(tq(1,1:imax,1:jmax), 1, imax, jmax, indir_name,
     $     lenin, tdatafile, lentdata, tdata_scaling, tdata_offset,
     $     tqinterp, tdata_varname, tdata_missing, lon, lat)
      if (qdata_rhum) then
         err4 = err_embm(rq_pa(1:imax,1:jmax), 2, imax, jmax, indir_name,
     $        lenin, qdatafile, lenqdata, qdata_scaling, qdata_offset,
     $        tqinterp, qdata_varname, qdata_missing, lon, lat)
      else
         err4 = err_embm(tq(2,1:imax,1:jmax), 2, imax, jmax, indir_name,
     $        lenin, qdatafile, lenqdata, qdata_scaling, qdata_offset,
     $        tqinterp, qdata_varname, qdata_missing, lon, lat)
      endif
      if (debug_init) print*,
     & 'err_embm composite = ',sqrt( ((err3**2*imax*jmax) +
     &                                 (err4**2*imax*jmax))
     &                               / ( 2*imax*jmax ) )

      call diagfna

      end
*
* diagfna.f quick modification of tstepa.f to allow calculation and
* plotting of northwards atm. heat flux 22/3/3
* subroutine tstepa.f for program goldstein, introduced 8/2/02
* transports tair, qair meridionally and vertically
* updates tair, qair in lower atmosphere
*
* flux version fully explicit one step second order variable depth
*
      subroutine diagfna

      use genie_util, ONLY: check_unit, check_iostat

#include "embm.cmn"

      real fn(2), diffextra, fntot
      integer i, j, l, ios

      call check_unit(43,__LINE__,__FILE__)
      open(43,file=outdir_name(1:lenout)//lout//'.fofya',iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
c     *NB where is the close for this?*

* 2nd order explicit step

      do 100 j=1,jmax
         fntot = 0.

         do 110 i=1,imax
            l=1
            if(j.ne.jmax)then

               fn(l) = cv(j)*betam(l)*uatm(2,i,j)*(tq1(l,i,j+1)
     1                       + tq1(l,i,j))*0.5
               diffextra = 0.
               fn(l) = fn(l) - cv(j)*cv(j)*(diffa(l,2,j)
     1                     + diffextra)
     1                     *(tq1(l,i,j+1) - tq1(l,i,j))*rds(j)
            else
               fn(l) = 0.
            endif

c nre dimless height of atm set to 1, implies factor of h in fluxsc

c++++++++++++++++++++++++++++++++++
c diagnostic for n. heat flux
               fntot = fntot + fn(1)
c++++++++++++++++++++++++++++++++++
  110    continue
         if(j.lt.jmax)write(43,'(e15.5)')
     &              dphi*fntot*usc*rhoair*cpa*hatmbl(1)*rsc

  100 continue

      end
