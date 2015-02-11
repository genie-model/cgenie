c SG File created from diagosc.F for ENTS diagnostics
c SG Comments below are from original diagosc.F file
c
c diagosc.f extra diagnostic routine for c-goldstein v2 with seasonal cycle
c calculate average over nyear timesteps. Extra diagnostics to be included
c WHERE INDICATED
c file created 18/6/3 Neil Edwards
c
      subroutine entsdiagosc(nyear,istep,iout,
#ifdef hfoutput
     &     istep0,
#endif
     :     albs_lnd,                         !< surface albedo
     :     land_snow_lnd                     !< land snow cover
     :     )
#include "genie_ents.cmn"
#include "var_ents.cmn"

#ifdef hfoutput
      character datestring*10
#endif

ccccc FOR NETCDF
        include 'netcdf.inc'
ccccc

      real rnyear

      integer istep,i,j,iout
#ifdef hfoutput
      integer istep0
#endif
      integer nyear

c surface albedo
      real,dimension(maxi,maxj),intent(inout)::albs_lnd
c land snow cover
      real,dimension(maxi,maxj),intent(inout)::land_snow_lnd

ccccccccccccccccccccccccc FOR ENTS NETCDF
        character fname*200, label*200
        real, allocatable :: var_data(:,:,:)
        integer myyear,mymonth,myday
        logical fexist

        interface

         character(len=10) function ConvertFunc(innumber,flag) result(outname)
         integer::innumber, flag
         end function ConvertFunc

         subroutine netcdf_ents(a,b,c,d)
          character*200 a,c
          real b(:,:,:)
          integer d
         end subroutine netcdf_ents

        end interface
cccccccccccccccccccccccc

      if (dosc) then
      rnyear = 1.0/nyear

      do j=1,jmax
         do i=1,imax
            tqldavg(1,i,j) = tqldavg(1,i,j) + (tqld(1,i,j)
     1                       *rnyear)
            tqldavg(2,i,j) = tqldavg(2,i,j) + (tqld(2,i,j)
     1                       *rnyear)
            snowavg(i,j) = snowavg(i,j)
     &           + (real(land_snow_lnd(i,j))*rnyear)
            albsavg(i,j) = albsavg(i,j) + (albs_lnd(i,j)*rnyear)
            bcapavg(i,j) = bcapavg(i,j) + (bcap(i,j)*rnyear)
            z0avg(i,j)   = z0avg(i,j)   + (z0(i,j)*rnyear)
         enddo
      enddo


      if((iout.eq.1).and.(mod(istep,ents_ianav).eq.0
     &   .and.(istep.ge.ents_ianav)))then
         print*,'writing averaged data at istep ',istep

#ifdef hfoutput
         write(datestring,'(i10.10)') istep+istep0
         open(15,file=trim(outdir_name)//trim(ents_out_name)
     :         //'.hfltavg_'//datestring)
#else
         open(15,file= trim(outdir_name)//trim(ents_out_name)
     1         //'.ltavg')
#endif

ccccccccccccccccc  FOR NETCDF

        myyear=int(istep/ents_nyear)
        mymonth=int(12*mod(istep,ents_nyear)/ents_nyear)
        myday=int(360*istep/ents_nyear-mymonth*30-(myyear-1)*360)

        fname=trim(outdir_name)//trim(ents_out_name)//'_yearav_'//
     : trim(ConvertFunc(myyear,10))//'.nc'

        inquire(file=fname,exist=fexist)
        if (fexist.eqv..true.) then
                open(8,file=fname,status='old')
                close(8,status='delete')
        end if
cccccccccccccccccccccccccccccccccc
        allocate(var_data(1,jmax,imax))
         do j=1,jmax
            do i=1,imax
cccccccccccccc OLD LINE
               write(15,10)tqldavg(1,i,j)
            var_data(1,j,i)=tqldavg(1,i,j)
            enddo
         enddo
cccccccccccccc  OLD LINE
         close(15)
        label='ltavg'

        call netcdf_ents(fname,var_data,label,myday)
        deallocate(var_data)
ccccccccccccccccccccccccccccccccccccccc

#ifdef hfoutput
         write(datestring,'(i10.10)') istep+istep0
         open(15,file=trim(outdir_name)//trim(ents_out_name)
     1         //'.hflqavg_'//datestring)
#else
         open(15,file=trim(outdir_name)//trim(ents_out_name)
     1         //'.lqavg')
#endif

ccccccccccccccccccccccccccccccccccccccc ncdf-replacement
        allocate(var_data(1,jmax,imax))
         do j=1,jmax
            do i=1,imax
cccccccccccccc OLD LINE
               write(15,10)tqldavg(2,i,j)
            var_data(1,j,i)=tqldavg(2,i,j)
            enddo
         enddo
cccccccccccccc  OLD LINE
        close(15)
        label='lqavg'

        call netcdf_ents(fname,var_data,label,myday)
        deallocate(var_data)
cccccccccccccccccccccccccccccccccccccc

#ifdef hfoutput
         write(datestring,'(i10.10)') istep+istep0
         open(15,file= trim(outdir_name)//trim(ents_out_name)
     1        //'.hfsnowavg_'//datestring)
#else
         open(15,file= trim(outdir_name)//trim(ents_out_name)
     1        //'.snowavg')
#endif
ccccccccccccccccccccccccccccccccccccccc ncdf-replacement
        allocate(var_data(1,jmax,imax))
         do j=1,jmax
            do i=1,imax
cccccccccccccc OLD LINE
               write(15,10)snowavg(i,j)
            var_data(1,j,i)=snowavg(i,j)
            enddo
         enddo
cccccccccccccc  OLD LINE
         close(15)
        label='snowavg'

        call netcdf_ents(fname,var_data,label,myday)
        deallocate(var_data)
ccccccccccccccccccccccccccccccccccccccc

#ifdef hfoutput
         write(datestring,'(i10.10)') istep+istep0
         open(15,file= trim(outdir_name)//trim(ents_out_name)
     1       //'.hfz0avg_'//datestring)
#else
         open(15,file= trim(outdir_name)//trim(ents_out_name)
     1       //'.z0avg')
#endif
ccccccccccccccccccccccccccccccccccccccc ncdf-replacement
        allocate(var_data(1,jmax,imax))
         do j=1,jmax
            do i=1,imax
cccccccccccccc OLD LINE
               write(15,10)z0avg(i,j)
            var_data(1,j,i)=z0avg(i,j)
            enddo
         enddo
cccccccccccccc  OLD LINE
         close(15)
        label='z0avg'

        call netcdf_ents(fname,var_data,label,myday)
        deallocate(var_data)
ccccccccccccccccccccccccccccccccccccccc

#ifdef hfoutput
         write(datestring,'(i10.10)') istep+istep0
         open(15,file=trim(outdir_name)//trim(ents_out_name)
     1         //'.hfalbsavg_'//datestring)
#else
         open(15,file= trim(outdir_name)//trim(ents_out_name)
     1         //'.albsavg')
#endif
ccccccccccccccccccccccccccccccccccccccc ncdf-replacement
        allocate(var_data(1,jmax,imax))
         do j=1,jmax
            do i=1,imax
cccccccccccccc OLD LINE
               write(15,10)albsavg(i,j)
            var_data(1,j,i)=albsavg(i,j)
            enddo
         enddo
cccccccccccccc  OLD LINE
         close(15)
        label='albsavg'

        call netcdf_ents(fname,var_data,label,myday)
        deallocate(var_data)
ccccccccccccccccccccccccccccccccccccccc

#ifdef hfoutput
         write(datestring,'(i10.10)') istep+istep0
         open(15,file= trim(outdir_name)//trim(ents_out_name)
     1         //'.hfbcapavg_'//datestring)
#else
         open(15,file= trim(outdir_name)//trim(ents_out_name)
     1         //'.bcapavg')
#endif

ccccccccccccccccccccccccccccccccccccccc ncdf-replacement
        allocate(var_data(1,jmax,imax))
         do j=1,jmax
            do i=1,imax
cccccccccccccc OLD LINE
               write(15,10)bcapavg(i,j)
            var_data(1,j,i)=bcapavg(i,j)
            enddo
         enddo
cccccccccccccc  OLD LINE
         close(15)
        label='bcapavg'

        call netcdf_ents(fname,var_data,label,myday)
        deallocate(var_data)
ccccccccccccccccccccccccccccccccccccccc

         open(45,file= trim(outdir_name)//trim(ents_out_name)
     1         //'.'//'gmairt')
         write(45,'(2e18.7)')real(istep/nyear)-0.5,
     1                       gmairttot/(real(imax*jmax))

c 	CHECK THIS VARIABLE!!!!!!!!!!!!!

c
c perform diagnostics on averaged data, either by rewriting other diag
c routines to accept data as argument, or by simply copying code,
c otherwise diagnose by integrating one (short) step from .avg file.
c
c diagnostic code to be inserted here
c

            print*,'resetting averaged data arrays at step',istep
            do j=1,jmax
               do i=1,imax
                  tqldavg(1,i,j) = 0.
                  tqldavg(2,i,j) = 0.
                  snowavg(i,j) = 0.
                  albsavg(i,j) = 0.
                  bcapavg(i,j) = 0.
                  z0avg(i,j)   = 0.
               enddo
            enddo
      endif
      endif
  10  format(e14.7)
      end
