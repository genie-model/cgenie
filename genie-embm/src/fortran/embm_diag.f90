MODULE embm_diag

  USE embm_lib
  USE embm_netcdf
  IMPLICIT NONE

CONTAINS

  ! Frequent atmospheric diagnostics.
  subroutine diag3(sum1,sum2,isum1,isum2)
    implicit none
    real    sum1(4), sum2(4)
    real    area1, area2
    integer isum1(4), isum2(4)

    integer i,j

    sum1 = 0.0
    sum2 = 0.0
    isum1 = 0
    isum2 = 0

    ! Initial conditions for minimum values
    sum1(4) = 999.9
    sum2(4) = 999.9

    ! Calculate averages, minima, maxima, etc.
    area1 = 0.0
    area2 = 0.0
    do j=1,jmax
       do i=1,imax
          ! Northern/Southern hemisphere averages
          if(j.gt.(jmax/2))then
             sum1(1) = sum1(1) + tq(1,i,j)*ds(j)
             sum2(1) = sum2(1) + tq(2,i,j)*ds(j)
             area1 = area1 + ds(j)
          else
             sum1(2) = sum1(2) + tq(1,i,j)*ds(j)
             sum2(2) = sum2(2) + tq(2,i,j)*ds(j)
             area2 = area2 + ds(j)
          endif
          ! Maximum temperature
          if(tq(1,i,j).gt.sum1(3)) then
             sum1(3) = tq(1,i,j)
             isum1(1) = i
             isum1(2) = j
          endif
          ! Minimum temperature
          if(tq(1,i,j).lt.sum1(4)) then
             sum1(4) = tq(1,i,j)
             isum1(3) = i
             isum1(4) = j
          endif
          ! Maximum specific humidity
          if(tq(2,i,j).gt.sum2(3)) then
             sum2(3) = tq(2,i,j)
             isum2(1) = i
             isum2(2) = j
          endif
          ! Minimum specific humidity
          if(tq(2,i,j).lt.sum2(4)) then
             sum2(4) = tq(2,i,j)
             isum2(3) = i
             isum2(4) = j
          endif
          !
       enddo
    enddo

    sum1(1) = sum1(1) / area1
    sum2(1) = sum2(1) / area1
    sum1(2) = sum1(2) / area2
    sum2(2) = sum2(2) / area2

    ! Convert humidity to g / kg
    sum2 = sum2 * 1000.0
  end subroutine diag3


  ! Main diagnostics.
  subroutine diaga
    implicit none
    real amin,amax,sum1
    real area
    real pme(maxi,maxj)

    integer i,j,iamin,iamax,jamin,jamax

    print *

    call aminmax(imax,jmax,tq(1,1,1),amin,amax,iamin,iamax,jamin,jamax,2,1)
    print*,'min atm T ',amin,' at ',iamin,jamin
    print*,'max atm T ',amax,' at ',iamax,jamax
    call aminmax(imax,jmax,tq(1,1,1),amin,amax,iamin,iamax,jamin,jamax,2,2)
    print*,'min atm q ',1e3*amin,' at ',iamin,jamin
    print*,'max atm q ',1e3*amax,' at ',iamax,jamax
    call aminmax(imax,jmax,pptn(1,1),amin,amax,iamin,iamax,jamin,jamax,1,1)
    print*,'min pptn  ',amin,' at ',iamin,jamin
    print*,'max pptn  ',amax,' at ',iamax,jamax
    call aminmax(imax,jmax,evap(1,1),amin,amax,iamin,iamax,jamin,jamax,1,1)
    print*,'min evap  ',amin,' at ',iamin,jamin
    print*,'max evap  ',amax,' at ',iamax,jamax
    pme = pptn - evap
    call aminmax(imax,jmax,pme(1,1),amin,amax,iamin,iamax,jamin,jamax,1,1)
    print*,'min P-E   ',amin,' at ',iamin,jamin
    print*,'max P-E   ',amax,' at ',iamax,jamax

    sum1 = 0
    area = 0.0
    do j=1,jmax
       do i=1,imax
          sum1 = sum1 + tq(1,i,j) * ds(j)
          area = area + ds(j)
       enddo
    enddo
    sum1 = sum1 / area
    write (6,*) 'average SAT', sum1

  end subroutine diaga


  subroutine aminmax(imax,jmax,a,amin,amax,iamin,iamax,jamin,jamax,lmax,l)
    implicit none
    integer i,j,imax,jmax,iamin,iamax,jamin,jamax,lmax,l
    real amin,amax,a(lmax,imax,jmax)

    amin = a(l,1,1)
    amax = a(l,1,1)
    iamin = 1
    iamax = 1
    jamin = 1
    jamax = 1
    do j=1,jmax
       do i=1,imax
          if(a(l,i,j).lt.amin)then
             amin = a(l,i,j)
             iamin = i
             jamin = j
          endif
          if(a(l,i,j).gt.amax)then
             amax = a(l,i,j)
             iamax = i
             jamax = j
          endif
       enddo
    enddo
  end subroutine aminmax


  ! End-of-run diagnostics
  subroutine diagend_embm
    use genie_util, ONLY: check_unit, check_iostat
    implicit none

    real err
    real err3, err4
    real tqdata(2,maxi,maxj)
    real errwtq(2)
    real tqav(2), tqvar(2)

    integer i, j, l, ios

    data tqav, tqvar/4*0.0/

    real lon(maxi),lat(maxj)

    ! If 'tsinterp' is '.true.': i) discontinue writing out of model-data
    ! field, ii) replace error score with the score calculated using the
    ! 'err_gold(...)' function further below
    if (.not.tqinterp) then
       ! read interpolated Levitus and NCEP data
       call check_unit(32,__LINE__,__FILE__)
       open(32,file=indir_name(1:lenin)//tdatafile(1:lentdata),iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       read(32,*,iostat=ios)((tqdata(1,i,j),i=1,imax),j=1,jmax)
       call check_iostat(ios,__LINE__,__FILE__)
       close(32,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       call check_unit(33,__LINE__,__FILE__)
       open(33,file=indir_name(1:lenin)//qdatafile(1:lenqdata),iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       read(33,*,iostat=ios)((tqdata(2,i,j),i=1,imax),j=1,jmax)
       call check_iostat(ios,__LINE__,__FILE__)
       close(33,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       tqdata(2,:,:) = tqdata(2,:,:) * 1.0E-3

       ! calculate weights based on variance of data NB not real spatial but
       ! computational spatial
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

       ! specify weights
       errwtq(1) = 1.0/tqvar(1)
       errwtq(2) = 1.0/tqvar(2)

       ! calculate error compared to observations (!)
       call check_unit(25,__LINE__,__FILE__)
       open(25,file=outdir_name(1:lenout)//'tmp.err',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       err = 0.
       do j=1,jmax
          do i=1,imax
             do l=1,2
                err = err + errwtq(l)*(tq(l,i,j) - tqdata(l,i,j))**2
             enddo
             if (debug_init) write(25,10)(tq(l,i,j) - tqdata(l,i,j),l=1,2)
          enddo
       enddo
10     format(e15.5)
       close(25,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)

       err = sqrt(err/(imax*jmax*2))
       if (debug_init) &
            &print*, 'EMBM : weighted r.m.s. model-data error ',err
    else
       if (debug_init) print*, &
            & "Writing out of model-data error fields (file"// &
            & " 'tmp.err') is inactive when observational dataset"// &
            & " is interpolated at runtime (i.e., 'tqinterp' is"// &
            & " '.true.')."
    endif

    do i=1,imax
       lon(i)=180.0*(phi0+(i-0.5)*dphi)/pi
    enddo
    do j=1,jmax
       lat(j)=180.0*asin(s(j))/pi
    enddo
    err3 = err_embm(tq(1,1:imax,1:jmax), 1, imax, jmax, indir_name, &
         & lenin, tdatafile, lentdata, tdata_scaling, tdata_offset, &
         & tqinterp, tdata_varname, tdata_missing, lon, lat)
    if (qdata_rhum) then
       err4 = err_embm(rq_pa(1:imax,1:jmax), 2, imax, jmax, indir_name, &
            &lenin, qdatafile, lenqdata, qdata_scaling, qdata_offset, &
            & tqinterp, qdata_varname, qdata_missing, lon, lat)
    else
       err4 = err_embm(tq(2,1:imax,1:jmax), 2, imax, jmax, indir_name, &
            & lenin, qdatafile, lenqdata, qdata_scaling, qdata_offset, &
            & tqinterp, qdata_varname, qdata_missing, lon, lat)
    endif
    if (debug_init) print*, &
         & 'err_embm composite = ',sqrt( ((err3**2*imax*jmax) + &
         & (err4**2*imax*jmax)) / ( 2*imax*jmax ) )

    call diagfna
  end subroutine diagend_embm


  ! diagfna.f quick modification of tstepa.f to allow calculation and
  ! plotting of northwards atm. heat flux 22/3/3
  ! subroutine tstepa.f for program goldstein, introduced 8/2/02
  ! transports tair, qair meridionally and vertically
  ! updates tair, qair in lower atmosphere
  !
  ! flux version fully explicit one step second order variable depth
  !
  subroutine diagfna

    use genie_util, ONLY: check_unit, check_iostat

    real fn(2), diffextra, fntot
    integer i, j, l, ios

    call check_unit(43,__LINE__,__FILE__)
    open(43,file=outdir_name(1:lenout)//lout//'.fofya',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    !     *NB where is the close for this?*

    ! 2nd order explicit step

    do j=1,jmax
       fntot = 0.
       do i=1,imax
          l=1
          if(j.ne.jmax)then

             fn(l) = cv(j)*betam(l)*uatm(2,i,j)*(tq1(l,i,j+1)+ tq1(l,i,j))*0.5
             diffextra = 0.
             fn(l) = fn(l) - cv(j)*cv(j)*(diffa(l,2,j) + diffextra) * &
                  & (tq1(l,i,j+1) - tq1(l,i,j))*rds(j)
          else
             fn(l) = 0.
          endif

          ! nre dimless height of atm set to 1, implies factor of h in fluxsc

          !++++++++++++++++++++++++++++++++++
          ! diagnostic for n. heat flux
          fntot = fntot + fn(1)
          !++++++++++++++++++++++++++++++++++
       end do
       if(j.lt.jmax)write(43,'(e15.5)') &
            & dphi*fntot*usc*rhoair*cpa*hatmbl(1)*rsc
    end do

  end subroutine diagfna


  ! Extra diagnostic routine for seasonal cycle
  subroutine diagosc_embm(istep,iout,ext,fx0flux,fwflux, wateratm)
    use genie_util, ONLY: check_unit, check_iostat
    implicit none

    integer istep, iout, ios
    real    fx0flux(4,maxi,maxj), fwflux(2,maxi,maxj)
    character ext*3

    real rnyear
    real err3, err4
    real watereb, wateratm, vsc
    integer i,j,l
    real work((maxi+1)*(maxj+1))
    real lon(maxi),lat(maxj)

    rnyear=1.0/nyear

    do j=1,jmax
       do i=1,imax
          do l=1,2
             tqavg(l,i,j) = tqavg(l,i,j) + tq(l,i,j)*rnyear
          enddo
          ! variable 'qdryavg' has been superseded by 'q_pa_avg' which is the
          ! annual mean of 'q_pa' computed in 'genie-embm/src/fortran/embmg.F'
          ! update annual-average fields for precipitation-adjusted humidity
          ! (i.e., humidity after precipitation)
          q_pa_avg(i,j) = q_pa_avg(i,j) + q_pa(i,j)*rnyear
          rq_pa_avg(i,j) = rq_pa_avg(i,j) + rq_pa(i,j)*rnyear
          do l=1,4
             fx0avg(l,i,j) = fx0avg(l,i,j) + fx0flux(l,i,j)*rnyear
          enddo
          do l=1,2
             fwavg(l,i,j) = fwavg(l,i,j) + fwflux(l,i,j)*rnyear
          enddo
       enddo
    enddo

    if(iout.eq.1)then
       print*,'EMBM : writing averaged data at istep ', istep/real(ndta)
       ! write averaged data (a near-copy of outm.f) not a restart
       ! as such, therefore can write less accurate, more economical output
       call check_unit(2,__LINE__,__FILE__)
       open(2,file=outdir_name(1:lenout)//lout//'.osc.'//ext, iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       write(2,10,iostat=ios)tqavg
       call check_iostat(ios,__LINE__,__FILE__)
       write(2,10,iostat=ios)(((fx0avg(l,i,j),i=1,imax), j=1,jmax),l=1,4)
       call check_iostat(ios,__LINE__,__FILE__)
       write(2,10,iostat=ios)(((fwavg(l,i,j),i=1,imax),j=1,jmax),l=1,2)
       call check_iostat(ios,__LINE__,__FILE__)
       close(2,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)

       print*,'Writing EMBM mean annual netCDF file at time',istep
       call ini_netcdf_embm(istep,2)
       call write_netcdf_embm(imax, jmax, k1, tqavg, q_pa_avg,rq_pa_avg, &
            & fx0avg,fwavg, work, maxi, maxj, 2)
       call end_netcdf_embm(2)
       print*

       ! increment average counter
       iav = iav + 1

       ! perform diagnostics on averaged data, either by rewriting other diag
       ! routines to accept data as argument, or by simply copying code,
       ! otherwise diagnose by integrating one (short) step from .avg file.
       do i=1,imax
          lon(i)=180.0*(phi0+(i-0.5)*dphi)/pi
       enddo
       do j=1,jmax
          lat(j)=180.0*asin(s(j))/pi
       enddo
       err3 = err_embm(tqavg(1,1:imax,1:jmax), 1, imax, jmax, indir_name, &
            & lenin, tdatafile, lentdata, tdata_scaling, tdata_offset, &
            & tqinterp, tdata_varname, tdata_missing, lon, lat)
       if (qdata_rhum) then
          err4 = err_embm(rq_pa_avg, 2, imax, jmax, indir_name, &
               & lenin, qdatafile, lenqdata, qdata_scaling, qdata_offset, &
               & tqinterp, qdata_varname, qdata_missing, lon, lat)
       else
          err4 = err_embm(q_pa_avg, 2, imax, jmax, indir_name, &
               & lenin, qdatafile, lenqdata, qdata_scaling, qdata_offset, &
               & tqinterp, qdata_varname, qdata_missing, lon, lat)
       endif
       print*,'err_embm annual average composite = ', &
            & sqrt( ((err3**2*imax*jmax) + (err4**2*imax*jmax)) / &
            & ( 2*imax*jmax ) )

       ! Calculate the atmosphere water content, wateratm
       watereb=0.
       do j=1,jmax
          do i=1,imax
             watereb = watereb+tqavg(2,i,j)*ds(j)
          enddo
       enddo
       vsc = dphi*rsc*rsc*1.0e-12
       wateratm= (watereb*rhoao*hatmbl(2))*vsc

       print*,'EMBM : resetting averaged data arrays at step', &
            & istep/real(ndta)
       do j=1,jmax
          do i=1,imax
             do l=1,2
                tqavg(l,i,j) = 0.
             enddo
             ! reset annual-average fields for precipitation-adjusted
             ! humidity (i.e., humidity after precipitation)
             q_pa_avg(i,j) = 0.
             rq_pa_avg(i,j) = 0.
             do l=1,2
                fx0avg(l,i,j)  = 0.
                fwavg(l,i,j)   = 0.
             enddo
             do l=3,4
                fx0avg(l,i,j)  = 0.
             enddo
          enddo
       enddo
       print*
    endif

10  format(e14.7)

  end subroutine diagosc_embm


  ! Return an RMS error value for the specified EMBM model field
  ! compared with the contents of the supplied data file.
  function err_embm(modeldata, tracerid, imax, jmax, indir_name, &
       & lenin, obsdatafile, lenobsdata, datascaling, dataoffset, &
       & interpolate, varname, missing, lon, lat)
    implicit none

    ! Error value
    real err_embm, err

    ! Grid size
    integer imax, jmax

    ! Model data field
    real modeldata(imax,jmax)

    ! Data field type
    integer tracerid

    ! scaling used to convert observational data to make them comparable to
    ! model data
    real datascaling, dataoffset

    ! Interpolate: if '.false.', read in observational data of model-grid
    ! resolution from file; if '.true.', read in observational data from
    ! NetCDF file and interpolate
    logical interpolate

    ! EMBM input/output directories
    integer lenin
    character indir_name*100

    ! EMBM T/S data files
    integer lenobsdata
    character obsdatafile*128

    ! observation-based dataset
    REAL                                 :: missing
    CHARACTER(25)                        :: varname

    ! EMBM grid
    REAL,DIMENSION(imax)                 :: lon
    REAL,DIMENSION(jmax)                 :: lat

    ! Weighting factor (reciprocal of obs. data variance)
    real errw

    ! Indices
    integer i,j

    ! Observational data, average and variance
    real obsdata(imax,jmax)
    real obsdata_av, obsdata_var

    call read_embm_target_field(tracerid,imax,jmax,indir_name,lenin, &
         & obsdatafile,lenobsdata,datascaling,dataoffset,interpolate, &
         & varname,missing,lon,lat,obsdata)

    ! calculate weights based on variance of data NB not real spatial but
    ! computational spatial
    obsdata_av  = 0.
    obsdata_var = 0.
    do j=1,jmax
       do i=1,imax
          obsdata_av = obsdata_av + obsdata(i,j)
          obsdata_var= obsdata_var + obsdata(i,j)*obsdata(i,j)
       enddo
    enddo
    obsdata_av  = obsdata_av/(imax*jmax)
    obsdata_var = obsdata_var/(imax*jmax) - obsdata_av*obsdata_av
    errw        = 1.0/obsdata_var

    ! calculate the rms error
    err = 0.
    do j=1,jmax
       do i=1,imax
          err = err + errw * (modeldata(i,j) - obsdata(i,j))**2
       enddo
    enddo
    err_embm = sqrt(err/(imax*jmax))

  end function err_embm


  ! reading in of data-based target fields for comparison with the model's
  ! internal fields
  subroutine read_embm_target_field(tracerid,imax,jmax,indir_name, &
       & lenin,obsdatafile,lenobsdata,datascaling,dataoffset, &
       & interpolate,varname,missing,lon,lat,obsdata)
    use genie_util, ONLY: check_unit, check_iostat, die
    use local_netcdf
    implicit none

    ! Grid size
    integer imax, jmax

    ! Data field type
    integer tracerid

    ! scaling used to convert observational data to make them comparable to
    ! model data
    real datascaling, dataoffset

    ! Interpolate: if '.false.', read in observational data of model-grid
    ! resolution from file; if '.true.', read in observational data from
    ! NetCDF file and interpolate
    logical interpolate

    ! EMBM input/output directories
    integer lenin
    character indir_name*100

    ! EMBM T/S data files
    integer lenobsdata
    character obsdatafile*128

    ! observation-based dataset
    TYPE(real2dVar),DIMENSION(1)         :: tq_obs
    TYPE(real1dVar),DIMENSION(2)         :: tq_obs_axis
    INTEGER                              :: nx_obs,ny_obs
    REAL,POINTER,DIMENSION(:,:)          :: sinlat_obs
    INTEGER                              :: ncid_in, ncstatus
    INTEGER                              :: i_obs,j_obs
    INTEGER                              :: i_obs_min,j_obs_min
    INTEGER                              :: i0,i1,jtmp
    INTEGER                              :: ii,jj,iii,nwidth
    REAL                                 :: missing
    CHARACTER(25)                        :: varname
    REAL                                 :: obstmp

    ! EMBM grid
    REAL,DIMENSION(imax)                 :: lon
    REAL,DIMENSION(jmax)                 :: lat
    REAL,DIMENSION(2,jmax)               :: sinlat

    ! interpolation
    INTEGER                              :: n_int,n_ext
    REAL                                 :: distmean,distmax
    REAL                                 :: rlon,rlat
    REAL                                 :: testmask
    REAL                                 :: dist,distmin,distminocean
    REAL                                 :: cosdlon

    ! miscelaneous variables
    INTEGER                              :: status
    REAL                                 :: pi

    ! Indices
    integer i,j

    ! Observational data, average and variance
    real obsdata(imax,jmax)

    ! For file checks
    integer ios

    i_obs_min = 0
    j_obs_min = 0

    pi=4.0*atan(1.0)

    if (.not.interpolate) then
       ! Open the data file
       call check_unit(32,__LINE__,__FILE__)
       open(32,file=indir_name(1:lenin)//obsdatafile(1:lenobsdata),iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       read(32,*,iostat=ios)((obsdata(i,j),i=1,imax),j=1,jmax)
       call check_iostat(ios,__LINE__,__FILE__)
       close(32,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)

       ! Apply a scaling for the humidity data
       if (tracerid.eq.2) then
          do j=1,jmax
             do i=1,imax
                obsdata(i,j) = obsdata(i,j) * 1.e-3
             enddo
          enddo
       endif
    else
       ! Read in NetCDF dataset and interpolate onto model grid
       call openNetCDFRead(indir_name(1:lenin)// &
            & obsdatafile(1:lenobsdata),ncid_in)
       tq_obs(1)%name = varname
       call lookupVars(ncid_in,tq_obs)
       ! size of observational dataset
       nx_obs=tq_obs(1)%dimLens(1)
       ny_obs=tq_obs(1)%dimLens(2)
       allocate(tq_obs(1)%data(0:nx_obs,1:ny_obs),stat=status)
       if (status /= 0) call die("Could not allocate memory")
       ncstatus = nf90_get_var(ncid_in,tq_obs(1)%id, &
            & tq_obs(1)%data(1:nx_obs,1:ny_obs))
       if (ncstatus /= NF90_NOERR) call handle_nc_err(ncstatus)
       tq_obs_axis(1)%name = tq_obs(1)%dimnames(1)
       tq_obs_axis(2)%name = tq_obs(1)%dimnames(2)
       ! Note, the zeroth longitude index represents the same values
       ! as for the last value (the actual coordinate is offset by 360
       ! degrees) to facilitate dealing with periodicity of the
       ! longitude
       call lookupVars(ncid_in,tq_obs_axis)
       allocate(tq_obs_axis(1)%data(0:nx_obs),stat=status)
       if (status /= 0) call die("Could not allocate memory")
       allocate(tq_obs_axis(2)%data(1:ny_obs),stat=status)
       if (status /= 0) call die("Could not allocate memory")
       ncstatus = nf90_get_var(ncid_in,tq_obs_axis(1)%id, &
            & tq_obs_axis(1)%data(1:nx_obs))
       if (ncstatus /= NF90_NOERR) call handle_nc_err(ncstatus)
       tq_obs_axis(1)%data(0)=tq_obs_axis(1)%data(nx_obs)-360.
       do j=1,ny_obs
          do i=1,nx_obs
             if (abs((tq_obs(1)%data(i,j)-missing)/missing).lt.1e-5) then
                tq_obs(1)%data(i,j)=9.99999e19
             endif
          enddo
          tq_obs(1)%data(0,j)=tq_obs(1)%data(nx_obs,j)
       enddo
       ncstatus = nf90_get_var(ncid_in,tq_obs_axis(2)%id, &
            & tq_obs_axis(2)%data)
       if (ncstatus /= NF90_NOERR) call handle_nc_err(ncstatus)
       ! prepare auxiliary arrays:
       ! first index    function
       ! 1            sin()
       ! 2            cos()
       allocate(sinlat_obs(2,ny_obs),stat=status)
       if (status /= 0) call die("Could not allocate memory")
       ! for grid of observation-based dataset
       do j=1,ny_obs
          sinlat_obs(1,j) = sin(tq_obs_axis(2)%data(j)*pi/180.)
          sinlat_obs(2,j) = cos(tq_obs_axis(2)%data(j)*pi/180.)
       enddo
       ! for GOLDSTEIN grid
       do j=1,jmax
          sinlat(1,j) = sin(lat(j)*pi/180.)
          sinlat(2,j) = cos(lat(j)*pi/180.)
       enddo
       ! flip latitudinal axis if required; test monotonicity of axis
       ! of observation-based dataset; convert GENIE's longitudinal
       ! axis to same range as that of the observational dataset
       if (tq_obs_axis(2)%data(ny_obs).lt.tq_obs_axis(2)%data(1)) then
          do j=1,int(ny_obs/2+0.5)
             obstmp=tq_obs_axis(2)%data(j)
             tq_obs_axis(2)%data(j)=tq_obs_axis(2)%data(ny_obs+1-j)
             tq_obs_axis(2)%data(ny_obs+1-j)=obstmp
             do i=0,nx_obs
                obstmp=tq_obs(1)%data(i,j)
                tq_obs(1)%data(i,j)=tq_obs(1)%data(i,ny_obs+1-j)
                tq_obs(1)%data(i,ny_obs+1-j)=obstmp
             enddo
          enddo
       endif
       do i=2,nx_obs
          if (tq_obs_axis(1)%data(i).le.tq_obs_axis(1)%data(i-1)) then
             call die("Non-incremental longitudinal axis")
          endif
       enddo
       do j=2,ny_obs
          if (tq_obs_axis(2)%data(j).le.tq_obs_axis(2)%data(j-1)) then
             call die("Non-incremental latitudinal axis")
          endif
       enddo
       do i=1,imax
          do while (lon(i).le.tq_obs_axis(1)%data(0))
             lon(i)=lon(i)+360.
          enddo
          do while (lon(i).gt.tq_obs_axis(1)%data(nx_obs))
             lon(i)=lon(i)-360.
          enddo
       enddo
       ! bi-linear interpolation, parts of this code is based on the
       ! interpolation routine 'genie-cgoldstein/laz2siz.f' (modified
       ! from tri-linear to bi-linear interpolations), the
       ! "extrapolation" part has been replaced by a horizontal search
       ! for the nearest valid point on the sphere.
       n_int=0
       distmean=0.
       distmax=0.
       n_ext=0
       do j=1,jmax
          do i=1,imax
             ! find location of model grid point on observation-based
             ! grid.
             i_obs = 0
             do while ((tq_obs_axis(1)%data(i_obs).lt.lon(i)).and. &
                  & (i_obs.le.nx_obs))
                i_obs = i_obs+1
             enddo
             ! This could possibly be done more general without the
             ! restriction that any model point has to be inside the
             ! extremes of the latitude coordinates of the
             ! observation-based grid
             j_obs = 1
             do while ((tq_obs_axis(2)%data(j_obs).lt.lat(j)).and. &
                  & (j_obs.le.ny_obs))
                j_obs = j_obs+1
             enddo
             if ((i_obs.eq.0).or. (i_obs.gt.nx_obs).or. &
                 & (j_obs.eq.1).or. (j_obs.gt.ny_obs)) then
                call die("Coordinates or depth outside of the"// &
                     & " boundaries set by observational dataset")
             endif
             rlon = (lon(i)-tq_obs_axis(1)%data(i_obs-1))/ &
                  & (tq_obs_axis(1)%data(i_obs)- &
                  & tq_obs_axis(1)%data(i_obs-1))
             rlat = (lat(j)-tq_obs_axis(2)%data(j_obs-1))/ &
                  & (tq_obs_axis(2)%data(j_obs)- &
                  & tq_obs_axis(2)%data(j_obs-1))
             testmask = max(tq_obs(1)%data(i_obs,j_obs), &
                  & tq_obs(1)%data(i_obs-1,j_obs), &
                  & tq_obs(1)%data(i_obs,j_obs-1), &
                  & tq_obs(1)%data(i_obs-1,j_obs-1))
             ! interpolate if no land at corners of cube encompassing
             ! the model grid location
             if (testmask.lt.1.e10) then
                obsdata(i,j) = (1.0-rlon)*((1.0-rlat)* &
                     & tq_obs(1)%data(i_obs-1,j_obs-1)+ rlat &
                     & *tq_obs(1)%data(i_obs-1,j_obs))+ rlon &
                     & *((1.0-rlat)* tq_obs(1)%data(i_obs,j_obs-1 &
                     & )+ rlat*tq_obs(1)%data(i_obs,j_obs))
                n_int=n_int+1
             else
                ! find horizonatlly nearest (true distance on sphere)
                ! point

                ! to compute arc distance dist between two points
                ! ((lon1,lat1) and (lon2,lat2)) on a sphere use:
                !
                !   dist=arccos(sin(lat1)*sin(lat2)+
                !               cos(lat1)*cos(lat2)*cos(lat2-lat1))
                !
                ! Note, this formula is affected by rounding errors
                ! for small angles, so resolution of close points is
                ! limited, especially if 4-byte
                ! arithmetic/trigonometry is used
                !
                ! start with rectangle defined by (i_obs-1,j_obs-1),
                ! (i_obs,j_obs), find within newly added points both
                ! the nearest valid point AND the nearest point
                distmin = pi
                distminocean = pi
                do ii=1,2
                   do jj=1,2
                      cosdlon=cos(pi*(lon(i)- &
                           & tq_obs_axis(1)%data(i_obs+1-ii))/180.)
                      jtmp=j_obs+1-jj
                      dist = acos(sinlat(1,j)*sinlat_obs(1,jtmp)+ &
                           & sinlat(2,j)*sinlat_obs(2,jtmp)*cosdlon)
                      distmin=min(distmin,dist)
                      testmask=max(tq_obs(1)%data(i_obs+1-ii,jtmp), &
                           & tq_obs(1)%data(i_obs+1-ii,jtmp))
                      if ((testmask.lt.1e10).and. &
                           & (distminocean.gt.dist)) then
                         distminocean=dist
                         i_obs_min=i_obs+1-ii
                         j_obs_min=jtmp
                      endif
                   enddo
                enddo
                nwidth=1
                ! repeat until nearest of the newly added points is
                ! farther away than nearest valid point,
                do while ((distmin.lt.distminocean).and. &
                     & (nwidth.lt.int(nx_obs/2)+1).and. &
                     & (nwidth.lt.ny_obs))
                   distmin = pi
                   ! add grid-point circumference around rectangle,
                   ! take into account periodicity in longitudinal
                   ! direction and also look across northern and
                   ! southern poles.  find nearest valid point AND
                   ! nearest point within newly added points
                   nwidth=nwidth+1
                   ! reflect i range if rectangle spreads across
                   ! northern or southern border
                   if ((j_obs-nwidth).lt.1) then
                      i0=i_obs-nwidth-int(nx_obs/2)
                      i1=i_obs-1+nwidth-int(nx_obs/2)
                      jtmp=abs(j_obs-nwidth-1)
                   else
                      i0=i_obs-nwidth
                      i1=i_obs-1+nwidth
                      jtmp=j_obs-nwidth
                   endif
                   do ii=i0,i1
                      iii=modulo(ii-1,nx_obs)+1
                      cosdlon=cos(pi*(lon(i)-tq_obs_axis(1)%data(iii))/180.)
                      dist = acos(sinlat(1,j)*sinlat_obs(1,jtmp)+ &
                           & sinlat(2,j)*sinlat_obs(2,jtmp)*cosdlon)
                      distmin=min(distmin,dist)
                      testmask=max(tq_obs(1)%data(iii,j_obs-nwidth), &
                           & tq_obs(1)%data(iii,j_obs-nwidth))
                      if ((testmask.lt.1e10).and.(distminocean.gt.dist)) then
                         distminocean=dist
                         i_obs_min=iii
                         j_obs_min=jtmp
                      endif
                   enddo
                   ! reflect i range if rectangle spreads across
                   ! northern or southern border
                   if ((j_obs-1+nwidth).gt.ny_obs) then
                      i0=i_obs-nwidth-int(nx_obs/2)
                      i1=i_obs-1+nwidth-int(nx_obs/2)
                      jtmp=2*ny_obs-(j_obs+nwidth-2)
                   else
                      i0=i_obs-nwidth
                      i1=i_obs-1+nwidth
                      jtmp=j_obs-1+nwidth
                   endif
                   do ii=i0,i1
                      iii=modulo(ii-1,nx_obs)+1
                      cosdlon=cos(pi*(lon(i)-tq_obs_axis(1)%data(iii))/180.)
                      dist = acos(sinlat(1,j)*sinlat_obs(1,jtmp)+ &
                           & sinlat(2,j)*sinlat_obs(2,jtmp)*cosdlon)
                      distmin=min(distmin,dist)
                      testmask=max(tq_obs(1)%data(iii,j_obs-1+nwidth), &
                           & tq_obs(1)%data(iii,j_obs-1+nwidth))
                      if ((testmask.lt.1e10).and.(distminocean.gt.dist)) then
                         distminocean=dist
                         i_obs_min=iii
                         j_obs_min=jtmp
                      endif
                   enddo
                   do jj=j_obs-nwidth+1,j_obs-2+nwidth
                      ! reflect i range if rectangle spreads across
                      ! northern or southern border
                      if ((jj.lt.1).or.(jj.gt.ny_obs)) then
                         iii=modulo(i_obs-nwidth-1+int(nx_obs/2), nx_obs)
                         if (jj.lt.1) then
                            jtmp=abs(jj-1)
                         else
                            jtmp=2*ny_obs-(jj-1)
                         endif
                      else
                         iii=modulo(i_obs-nwidth-1,nx_obs)+1
                         jtmp=jj
                      endif
                      cosdlon=cos(pi*(lon(i)-tq_obs_axis(1)%data(iii))/180.)
                      dist = acos(sinlat(1,j)*sinlat_obs(1,jtmp)+ &
                           & sinlat(2,j)*sinlat_obs(2,jtmp)*cosdlon)
                      distmin=min(distmin,dist)
                      testmask=max(tq_obs(1)%data(iii,jj), &
                           & tq_obs(1)%data(iii,jj))
                      if ((testmask.lt.1e10).and.(distminocean.gt.dist)) then
                         distminocean=dist
                         i_obs_min=iii
                         j_obs_min=jtmp
                      endif
                   enddo
                   do jj=j_obs-nwidth+1,j_obs-2+nwidth
                      ! reflect i range if rectangle spreads across
                      ! northern or southern border
                      if ((jj.lt.1).or.(jj.gt.ny_obs)) then
                         iii=modulo(i_obs-2+nwidth+int(nx_obs/2),nx_obs)
                         if (jj.lt.1) then
                            jtmp=abs(jj-1)
                         else
                            jtmp=2*ny_obs-(jj-1)
                         endif
                      else
                         iii=modulo(i_obs-2+nwidth,nx_obs)+1
                         jtmp=jj
                      endif
                      cosdlon=cos(pi*(lon(i)-tq_obs_axis(1)%data(iii))/180.)
                      dist = acos(sinlat(1,j)*sinlat_obs(1,jtmp)+ &
                           & sinlat(2,j)*sinlat_obs(2,jtmp)*cosdlon)
                      distmin=min(distmin,dist)
                      testmask=max(tq_obs(1)%data(iii,jj), &
                           & tq_obs(1)%data(iii,jj))
                      if ((testmask.lt.1e10).and.(distminocean.gt.dist)) then
                         distminocean=dist
                         i_obs_min=iii
                         j_obs_min=jtmp
                      endif
                   enddo
                enddo
                ! vertically interpolate at point with shortest
                ! distance from target point
                obsdata(i,j) = tq_obs(1)%data(i_obs_min,j_obs_min)
                distmean=distmean+distminocean
                if (distminocean.gt.distmax) then
                   distmax=distminocean
                endif
                n_ext=n_ext+1
             endif
          enddo
       enddo
       if (n_ext.gt.0) then
          distmean=distmean/real(n_ext)
       endif
       print *, 'fraction of interpolated points,'
       print *, 'fraction of extrapolated points,'
       print *, 'mean distance of extrapolated points (degrees),'
       print *, 'maximum distance of extrapolated point (degrees)'
       print *, real(n_int)/real(n_int+n_ext), &
            & real(n_ext)/real(n_int+n_ext),distmean*180./pi,distmax*180./pi

       ! Clean up
       deallocate(sinlat_obs,stat=status)
       if (status /= 0) call die("Could not allocate memory")
       deallocate(tq_obs_axis(1)%data,stat=status)
       if (status /= 0) call die("Could not allocate memory")
       deallocate(tq_obs_axis(2)%data,stat=status)
       if (status /= 0) call die("Could not allocate memory")
       deallocate(tq_obs(1)%data,stat=status)
       if (status /= 0) call die("Could not allocate memory")
       call closeNetCDF(ncid_in)

       ! Apply a scaling for the humidity data
       do j=1,jmax
          do i=1,imax
             obsdata(i,j) = obsdata(i,j)/datascaling-dataoffset
          enddo
       enddo
    endif
  end subroutine read_embm_target_field

END MODULE embm_diag
