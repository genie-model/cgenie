MODULE embm_data

  USE embm_lib
  USE embm_netcdf
  IMPLICIT NONE

CONTAINS

  subroutine inm_embm(unit)
    implicit none

    integer i, j, l, unit
    real    tmp_val(2)
    real    area

    ! extra read statement for embm atmos
    read (unit,*)(((tq(l,i,j),l=1,2),i=1,imax),j=1,jmax)

    ! write out averages for restart checks
    if (debug_init) write(*,220) 'Avg T','Avg Q'

    do l=1,2
       tmp_val(l) = 0
    enddo

    !     Sum layer state variables and flow field
    area = 0.
    do j=1,jmax
       do i=1,imax
          area = area + ds(j)
          do l=1,2
             tmp_val(l) = tmp_val(l) + tq(l,i,j)*ds(j)
          enddo
       enddo
    enddo

    !     Print average values out
    if (debug_init) write(*,210) tmp_val(1)/area, (tmp_val(2)/area)*1000.

210 format(2f13.9)
220 format(2a13)

  end subroutine inm_embm


  ! This module reads netcdf restarts for EMBM
  subroutine inm_netcdf_embm
    implicit none

    real temp_read(maxi,maxj)
    real shum_read(maxi,maxj)
    integer iday
    integer ifail
    integer ncid
    logical lexist
    character*200 fnamein
    real timestep
    integer i,j,l
    real    tmp_val(2)
    real    area

    timestep=24.0*60.0*60.0*yearlen/real(nyear*ndta)

    fnamein=trim(filenetin)

    ifail=0
    inquire(file=trim(fnamein), exist=lexist)
    if (.not.lexist) then
       print*,' Missing file ',trim(fnamein)
       ifail=1
    endif
    if (ifail.ne.0) then
       print*,' Correct error and try again '
       stop 1
    endif

    print*,' embm: Opening restart file for read: ', &
         & trim(filenetin)

    call open_file_nc(trim(fnamein),ncid)
    call get1di_data_nc(ncid,'ioffset',1,ioffset_rest,ifail)
    call get1di_data_nc(ncid,'iyear',1,iyear_rest,ifail)
    call get1di_data_nc(ncid,'imonth',1,imonth_rest,ifail)
    call get1di_data_nc(ncid,'iday',1,iday,ifail)
    call get2d_data_nc(ncid,'air_temp',maxi,maxj,temp_read,ifail)
    call get2d_data_nc(ncid,'humidity',maxi,maxj,shum_read,ifail)
    call close_file_nc(trim(filenetin),ncid)

    day_rest=iday
    ioffset_rest=mod(ioffset_rest,nint(yearlen))

    day_rest=day_rest+timestep/(24*60*60.)
    !     This bit so that we don't get too far out in our count....
    !     Anchor to a day if we start drifting.
    !     Means timestep can never be less than 1/1000 of a day!!!!
    if (abs(iday-day_rest).le.1e-3) then
       day_rest=iday
    endif
    if (day_rest.ge.31) then
       day_rest=day_rest-30
       imonth_rest=imonth_rest+1
       if (imonth_rest.eq.13) then
          imonth_rest=1
          iyear_rest=iyear_rest+1
       endif
    endif
    iday=nint(day_rest)
    print*,'day in embm restart is now',day_rest
    print*,'iday in embm restart is now',iday

    tq(1,1:maxi,1:maxj)=temp_read(:,:)
    tq(2,1:maxi,1:maxj)=shum_read(:,:)

    ! AY (06/10/04) : copied from inm_embm.f

    !     AY (08/03/04) : write out averages for restart checks
    write(*,220) 'Avg T','Avg Q'
    !
    !     Clear temporary variables
    do l=1,2
       tmp_val(l) = 0
    enddo
    !
    !     Sum layer state variables and flow field
    area = 0.
    do j=1,jmax
       do i=1,imax
          area = area + ds(j)
          do l=1,2
             tmp_val(l) = tmp_val(l) + tq(l,i,j)*ds(j)
          enddo
       enddo
    enddo

    ! Print average values out
    write(*,210) tmp_val(1)/area, (tmp_val(2)/area)*1000.

210 format(2f13.9)
220 format(2a13)

    return
  end subroutine inm_netcdf_embm


  ! subroutine outm.f writes out data for goldstein last change  6/6/95
  ! expanded to write out atmos and sea ice data (Bob 10/5/02)
  !
  ! AY (02/12/03) : contracted to excise GOLDSTEIN data
  !
  ! RM (16/05/05) : edited for variable sin(lat) resolution (from NRE, 6/12/04)
  !
  subroutine outm_embm(unit)
    implicit none
    integer i, j, l, unit
    real    tmp_val(2)
    real    area

    !       jdannan testing a better write out
    write(unit,fmt='(e24.15)')tq

    !     AY (08/03/04) : write out averages for restart checks
    if (debug_loop) write(*,220) 'Avg T','Avg Q'
    !
    !     Clear temporary variables
    do l=1,2
       tmp_val(l) = 0
    enddo
    !
    !     Sum layer state variables and flow field
    area = 0.
    do j=1,jmax
       do i=1,imax
          area = area + ds(j)
          do l=1,2
             tmp_val(l) = tmp_val(l) + tq(l,i,j)*ds(j)
          enddo
       enddo
    enddo
    !
    !     Print average values out
    if (debug_loop) write(*,210) tmp_val(1)/area, (tmp_val(2)/area)*1000.

210 format(2f13.9)
220 format(2a13)

  end subroutine outm_embm


  ! This module writes netcdf restarts for embm
  subroutine outm_netcdf_embm(istep)
    implicit none
    include 'netcdf.inc'

    integer istep

    real temp_write(maxi,maxj)
    real shum_write(maxi,maxj)

    real lons1(maxi)
    real lats1(maxj)

    !      integer landmask(maxi,maxj)

    integer i,j

    integer ntempid
    integer nshumid

    integer nlon1id,nlongit1id,nlat1id,nlatit1id
    integer nrecsid,ioffsetid

    integer dim1pass(2)

    character fname*200

    !     For date and restarts...
    integer iday
    integer iyearid
    integer imonthid
    integer idayid
    character yearstring*10
    character monthstring*2
    character daystring*2
    character datestring*7

    !     For netcdf...
    integer status
    integer ncid

    real timestep

    ! AY (06/10/04) : extra variables for printing out average model properties
    integer l
    real    tmp_val(2)
    real    area

    !     output file format is yyyy_mm_dd
    !     30 day months are assumed
    if (mod(yearlen,30.0).ne.0) then
       print*, 'ERROR: Goldstein NetCDF restarts (outm_netdf):'
       print*, '   mod(yearlen,30.0) must be zero'
       stop
    end if

    timestep=yearlen/real(nyear*ndta)

    iday=nint(day_rest)

    if(mod(istep,(iwstp*ndta)).eq.0)then

       !     WRITE A RESTART.....

       ! AY (04/10/04) : uses grid variables calculated in initialise_seaice.F
       do i=1,maxi
          lons1(i)=nclon1(i)
       end do

       do j=1,maxj
          lats1(j)=nclat1(j)
       enddo

       temp_write(:,:)=real(tq(1,1:maxi,1:maxj))
       shum_write(:,:)=real(tq(2,1:maxi,1:maxj))

       write(datestring,'(i7.7)') istep
       write(yearstring,'(i10)') iyear_rest
       write(monthstring,'(i2.2)') imonth_rest
       write(daystring,'(i2.2)') iday

       !-------------------------------------------------------
       !     create a netcdf file
       !-------------------------------------------------------
       fname=trim(dirnetout)//'/embm_restart_'// &
            & trim(adjustl(yearstring))//'_'//monthstring//'_'// &
            & daystring//'.nc'
       print*,' Opening netcdf restart file for write: ',trim(fname)
       status=nf_create(trim(fname), nf_clobber, ncid)
       call check_err(status)
       status=nf_def_dim(ncid, 'nrecs',1,nrecsid)
       call check_err(status)
       status=nf_def_dim(ncid, 'longitude',maxi,nlon1id)
       call check_err(status)
       status=nf_def_dim(ncid, 'latitude',maxj,nlat1id)
       call check_err(status)

       status=nf_def_var(ncid,'longitude',nf_real,1,nlon1id,nlongit1id)
       call check_err(status)
       status=nf_def_var(ncid,'latitude',nf_real,1,nlat1id,nlatit1id)
       call check_err(status)
       dim1pass(1)=nlon1id
       dim1pass(2)=nlat1id
       status=nf_def_var(ncid,'ioffset',nf_int,1,nrecsid,ioffsetid)
       call check_err(status)
       status=nf_def_var(ncid,'iyear',nf_int,1,nrecsid,iyearid)
       call check_err(status)
       status=nf_def_var(ncid,'imonth',nf_int,1,nrecsid,imonthid)
       call check_err(status)
       status=nf_def_var(ncid,'iday',nf_int,1,nrecsid,idayid)
       call check_err(status)

       status=nf_def_var(ncid,'air_temp',nf_double,2,dim1pass,ntempid)
       call check_err(status)
       status=nf_def_var(ncid,'humidity',nf_double,2,dim1pass,nshumid)
       call check_err(status)
       status=nf_enddef(ncid)
       call check_err(status)

       status=nf_put_var_int(ncid,iyearid,iyear_rest)
       call check_err(status)
       status=nf_put_var_int(ncid,imonthid,imonth_rest)
       call check_err(status)
       status=nf_put_var_int(ncid,idayid,iday)
       call check_err(status)
       status=nf_put_var_int(ncid,ioffsetid,ioffset_rest)
       call check_err(status)

       status=nf_put_var_double(ncid,nlongit1id,lons1)
       call check_err(status)
       status=nf_put_var_double(ncid,nlatit1id,lats1)
       call check_err(status)
       status=nf_put_var_double(ncid,ntempid,temp_write)
       call check_err(status)
       status=nf_put_var_double(ncid,nshumid,shum_write)
       call check_err(status)

       status=nf_close(ncid)
       call check_err(status)

       ! AY (06/10/04) : copied from outm_embm.f

       !     AY (08/03/04) : write out averages for restart checks
       write(*,220) 'Avg T','Avg Q'
       !
       !     Clear temporary variables
       do l=1,2
          tmp_val(l) = 0
       enddo
       !
       !     Sum layer state variables and flow field
       area = 0.
       do j=1,jmax
          do i=1,imax
             area = area + ds(j)
             do l=1,2
                tmp_val(l) = tmp_val(l) + tq(l,i,j)*ds(j)
             enddo
          enddo
       enddo
       !
       !     Print average values out
       write(*,210) tmp_val(1)/area, (tmp_val(2)/area)*1000.

210    format(2f13.9)
220    format(2a13)

    endif

    day_rest=day_rest+timestep
    !     This bit so that we don't get too far out in our count....
    !     Anchor to a day if we start drifting.
    !     Means timestep can never be less than 1/1000 of a day!!!!
    if (abs(iday-day_rest).le.1e-3) then
       print*,'CORRECTING TIME-LAG! in outm_netcdf in genie-goldstein', &
            & iday,day_rest
       day_rest=iday
    endif
    if (day_rest.ge.31) then
       day_rest=day_rest-30
       imonth_rest=imonth_rest+1
       if (imonth_rest.eq.13) then
          imonth_rest=1
          iyear_rest=iyear_rest+1
       endif
    endif
    iday=nint(day_rest)

    return
  end subroutine outm_netcdf_embm



  ! subroutine outm.f writes out data for goldstein last change  6/6/95
  ! expanded to write out atmos and sea ice data (Bob 10/5/02)
  !
  ! AY (05/02/04) : modified to output surface fluxes
  !                 note the organisation of output file (one field after
  !                 the other, no clever interlacing)
  !
  ! AY (12/07/05) : removed surplus input argument to function

  subroutine outm_surf(unit,co2_in,albedo_ocn,usurf_ocn,latent_ocn, &
       & sensible_ocn,netsolar_ocn,netlong_ocn,evap_ocn,pptn_ocn, &
       & runoff_ocn,latent_atm,sensible_atm,netsolar_atm,netlong_atm, &
       & evap_atm,pptn_atm,dhght_sic,darea_sic)
    implicit none
    integer unit
    real co2_in(imax,jmax), usurf_ocn(imax,jmax)
    real, dimension(imax,jmax) :: albedo_ocn,latent_ocn,sensible_ocn, &
         & netsolar_ocn,netlong_ocn,evap_ocn,pptn_ocn,runoff_ocn, &
         & latent_atm, sensible_atm,netsolar_atm,netlong_atm, &
         & evap_atm,pptn_atm,dhght_sic,darea_sic

    write(unit,10 )latent_ocn
    write(unit,10 )sensible_ocn
    write(unit,10 )netsolar_ocn
    write(unit,10 )netlong_ocn
    write(unit,10 )evap_ocn
    write(unit,10 )pptn_ocn
    write(unit,10 )runoff_ocn
    write(unit,10 )albedo_ocn
    write(unit,10 )usurf_ocn
    write(unit,10 )latent_atm
    write(unit,10 )sensible_atm
    write(unit,10 )netsolar_atm
    write(unit,10 )netlong_atm
    write(unit,10 )evap_atm
    write(unit,10 )pptn_atm
    write(unit,10 )co2_in
    write(unit,10 )dhght_sic
    write(unit,10 )darea_sic

10  format(e21.13)
  end subroutine outm_surf

END MODULE embm_data
