MODULE embm_diag

  USE embm_lib
  USE embm_netcdf
  IMPLICIT NONE

CONTAINS

  ! Frequent atmospheric diagnostics.
  SUBROUTINE diag3(sum1, sum2, isum1, isum2)
    IMPLICIT NONE
    REAL, DIMENSION(4), INTENT(OUT) :: sum1, sum2
    INTEGER, DIMENSION(4), INTENT(OUT) :: isum1, isum2

    REAL :: area1, area2
    INTEGER :: i, j

    sum1 = 0.0 ; sum2 = 0.0 ; isum1 = 0 ; isum2 = 0

    ! Initial conditions for minimum values
    sum1(4) = 999.9
    sum2(4) = 999.9

    ! Calculate averages, minima, maxima, etc.
    area1 = 0.0
    area2 = 0.0
    DO j = 1, jmax
       DO i = 1, imax
          ! Northern/Southern hemisphere averages
          IF (j > jmax / 2) THEN
             sum1(1) = sum1(1) + tq(1,i,j) * ds(j)
             sum2(1) = sum2(1) + tq(2,i,j) * ds(j)
             area1 = area1 + ds(j)
          ELSE
             sum1(2) = sum1(2) + tq(1,i,j) * ds(j)
             sum2(2) = sum2(2) + tq(2,i,j) * ds(j)
             area2 = area2 + ds(j)
          END IF
          ! Maximum temperature
          IF (tq(1,i,j) > sum1(3)) THEN
             sum1(3) = tq(1,i,j)
             isum1(1) = i
             isum1(2) = j
          END IF
          ! Minimum temperature
          IF (tq(1,i,j) < sum1(4)) THEN
             sum1(4) = tq(1,i,j)
             isum1(3) = i
             isum1(4) = j
          END IF
          ! Maximum specific humidity
          IF (tq(2,i,j) > sum2(3)) THEN
             sum2(3) = tq(2,i,j)
             isum2(1) = i
             isum2(2) = j
          END IF
          ! Minimum specific humidity
          IF (tq(2,i,j) < sum2(4)) THEN
             sum2(4) = tq(2,i,j)
             isum2(3) = i
             isum2(4) = j
          END IF
       END DO
    END DO

    sum1(1) = sum1(1) / area1
    sum2(1) = sum2(1) / area1
    sum1(2) = sum1(2) / area2
    sum2(2) = sum2(2) / area2

    ! Convert humidity to g / kg
    sum2 = sum2 * 1000.0
  END SUBROUTINE diag3


  ! Main diagnostics.
  SUBROUTINE diaga
    IMPLICIT NONE

    REAL :: amin, amax, sum1, area, pme(maxi,maxj)
    INTEGER :: i, j, iamin, iamax, jamin, jamax

    PRINT *
    CALL aminmax(tq(1,:,:), amin, amax, iamin, iamax, jamin, jamax)
    PRINT *, 'min atm T ', amin, ' at ', iamin, jamin
    PRINT *, 'max atm T ', amax, ' at ', iamax, jamax
    CALL aminmax(tq(2,:,:), amin, amax, iamin, iamax, jamin, jamax)
    PRINT *, 'min atm q ', 1.0E3 * amin, ' at ', iamin, jamin
    PRINT *, 'max atm q ', 1.0E3 * amax, ' at ', iamax, jamax
    CALL aminmax(pptn, amin, amax, iamin, iamax, jamin, jamax)
    PRINT *, 'min pptn  ', amin, ' at ', iamin, jamin
    PRINT *, 'max pptn  ', amax, ' at ', iamax, jamax
    CALL aminmax(evap, amin, amax, iamin, iamax, jamin, jamax)
    PRINT *, 'min evap  ', amin, ' at ', iamin, jamin
    PRINT *, 'max evap  ', amax, ' at ', iamax, jamax
    pme = pptn - evap
    CALL aminmax(pme, amin, amax, iamin, iamax, jamin, jamax)
    PRINT *, 'min P-E   ', amin, ' at ', iamin, jamin
    PRINT *, 'max P-E   ', amax, ' at ', iamax, jamax

    sum1 = 0
    area = 0.0
    DO j = 1, jmax
       DO i = 1, imax
          sum1 = sum1 + tq(1,i,j) * ds(j)
          area = area + ds(j)
       END DO
    END DO
    sum1 = sum1 / area
    WRITE (6,*) 'average SAT', sum1
  END SUBROUTINE diaga


  SUBROUTINE aminmax(a, amin, amax, iamin, iamax, jamin, jamax)
    IMPLICIT NONE
    INTEGER :: iamin, iamax, jamin, jamax
    REAL :: amin, amax, a(:,:)

    INTEGER :: loc(2)

    amin = MINVAL(a) ; loc = MINLOC(a) ; iamin = loc(1) ; jamin = loc(2)
    amax = MAXVAL(a) ; loc = MAXLOC(a) ; iamax = loc(1) ; jamax = loc(2)
  END SUBROUTINE aminmax


  ! End-of-run diagnostics
  SUBROUTINE diagend_embm
    USE genie_util, ONLY: check_unit, check_iostat
    IMPLICIT NONE

    REAL :: err, err3, err4
    REAL :: tqdata(2,maxi,maxj), errwtq(2), tqav(2) = 0, tqvar(2) = 0
    INTEGER :: i, j, l, ios
    REAL :: lon(maxi), lat(maxj)

    ! If 'tsinterp' is '.true.': i) discontinue writing out of model-data
    ! field, ii) replace error score with the score calculated using the
    ! 'err_gold(...)' function further below
    IF (.NOT. tqinterp) THEN
       ! read interpolated Levitus and NCEP data
       CALL check_unit(32, __LINE__, __FILE__)
       OPEN(32,FILE=indir_name(1:lenin)//tdatafile(1:lentdata),IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       READ (32,*,IOSTAT=ios) ((tqdata(1,i,j), i = 1, imax), j = 1, jmax)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CLOSE(32,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CALL check_unit(33, __LINE__, __FILE__)
       OPEN(33,FILE=indir_name(1:lenin)//qdatafile(1:lenqdata),IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       READ (33,*,IOSTAT=ios) ((tqdata(2,i,j), i = 1, imax), j = 1, jmax)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CLOSE(33,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       tqdata(2,:,:) = tqdata(2,:,:) * 1.0E-3

       ! Calculate weights based on variance of data NB not real
       ! spatial but computational spatial
       DO l = 1, 2
          tqav(l) = SUM(tqdata(l,:,:))
          tqvar(l) = SUM(tqdata(l,:,:) * tqdata(l,:,:))
       END DO
       tqav = tqav / (imax * jmax)
       tqvar = tqvar / (imax * jmax) - tqav * tqav

       ! specify weights
       errwtq = 1.0 / tqvar

       ! calculate error compared to observations (!)
       CALL check_unit(25, __LINE__, __FILE__)
       OPEN(25,FILE=outdir_name(1:lenout)//'tmp.err',IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       err = 0.
       DO j = 1, jmax
          DO i = 1, imax
             DO l = 1, 2
                err = err + errwtq(l) * (tq(l,i,j) - tqdata(l,i,j))**2
             END DO
             IF (debug_init) WRITE(25,10) (tq(l,i,j) - tqdata(l,i,j), l = 1, 2)
          END DO
       END DO
10     FORMAT(e15.5)
       CLOSE(25,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)

       err = SQRT(err / (imax * jmax * 2))
       IF (debug_init) PRINT *,  'EMBM : weighted r.m.s. model-data error ', err
    ELSE
       IF (debug_init) PRINT *, &
            & 'Writing out of model-data error fields (file' //  &
            & ' tmp.err) is inactive when observational dataset' //  &
            & ' is interpolated at runtime (i.e., tqinterp is' //  &
            & ' .true.).'
    END IF

    FORALL (i=1:imax) lon(i) = 180.0 * (phi0 + (i - 0.5) * dphi) / pi
    FORALL (j=1:jmax) lat(j) = 180.0 * ASIN(s(j)) / pi
    err3 = err_embm(tq(1,1:imax,1:jmax), 1, imax, jmax, indir_name, &
         & lenin, tdatafile, lentdata, tdata_scaling, tdata_offset, &
         & tqinterp, tdata_varname, tdata_missing, lon, lat)
    IF (qdata_rhum) THEN
       err4 = err_embm(rq_pa(1:imax,1:jmax), 2, imax, jmax, indir_name, &
            &lenin, qdatafile, lenqdata, qdata_scaling, qdata_offset, &
            & tqinterp, qdata_varname, qdata_missing, lon, lat)
    ELSE
       err4 = err_embm(tq(2,1:imax,1:jmax), 2, imax, jmax, indir_name, &
            & lenin, qdatafile, lenqdata, qdata_scaling, qdata_offset, &
            & tqinterp, qdata_varname, qdata_missing, lon, lat)
    END IF
    IF (debug_init) PRINT *, &
         & 'err_embm composite = ', SQRT(((err3**2 * imax * jmax) + &
         & (err4**2 * imax * jmax)) / (2 * imax * jmax))

    CALL diagfna
  END SUBROUTINE diagend_embm


  ! diagfna.f quick modification of tstepa.f to allow calculation and
  ! plotting of northwards atm. heat flux 22/3/3
  ! subroutine tstepa.f for program goldstein, introduced 8/2/02
  ! transports tair, qair meridionally and vertically
  ! updates tair, qair in lower atmosphere
  !
  ! flux version fully explicit one step second order variable depth
  !
  SUBROUTINE diagfna
    USE genie_util, ONLY: check_unit, check_iostat
    IMPLICIT NONE

    REAL :: fn(2), diffextra, fntot
    INTEGER :: i, j, l, ios

    CALL check_unit(43, __LINE__, __FILE__)
    OPEN(43,FILE=outdir_name(1:lenout)//lout//'.fofya',IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)

    ! 2nd order explicit step
    DO j = 1, jmax
       fntot = 0.0
       DO i = 1, imax
          l=1
          IF (j /= jmax) THEN
             fn(l) = cv(j) * betam(l) * uatm(2,i,j) * &
                  & (tq1(l,i,j+1) + tq1(l,i,j)) * 0.5
             diffextra = 0.0
             fn(l) = fn(l) - cv(j) * cv(j) * (diffa(l,2,j) + diffextra) * &
                  & (tq1(l,i,j+1) - tq1(l,i,j)) * rds(j)
          ELSE
             fn(l) = 0.0
          END IF
          fntot = fntot + fn(1)
       END DO
       IF (j < jmax) WRITE (43,'(e15.5)') &
            & dphi * fntot * usc * rhoair * cpa * hatmbl(1) * rsc
    END DO
  END SUBROUTINE diagfna


  ! Extra diagnostic routine for seasonal cycle
  SUBROUTINE diagosc_embm(istep, iout, ext, fx0flux, fwflux, wateratm)
    USE genie_util, ONLY: check_unit, check_iostat
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: istep, iout
    REAL, INTENT(IN) :: fx0flux(4,maxi,maxj), fwflux(2,maxi,maxj)
    CHARACTER(LEN=3), INTENT(IN) :: ext
    REAL, INTENT(OUT) :: wateratm

    REAL :: rnyear, err3, err4, watereb, vsc
    INTEGER :: i, j, l, ios
    REAL :: work((maxi+1) * (maxj+1)), lon(maxi), lat(maxj)

    rnyear = 1.0 / nyear
    DO j = 1, jmax
       DO i = 1, imax
          DO l = 1, 2
             tqavg(l,i,j) = tqavg(l,i,j) + tq(l,i,j) * rnyear
          END DO
          ! update annual-average fields for precipitation-adjusted humidity
          ! (i.e., humidity after precipitation)
          q_pa_avg(i,j) = q_pa_avg(i,j) + q_pa(i,j) * rnyear
          rq_pa_avg(i,j) = rq_pa_avg(i,j) + rq_pa(i,j) * rnyear
          DO l = 1, 4
             fx0avg(l,i,j) = fx0avg(l,i,j) + fx0flux(l,i,j) * rnyear
          END DO
          DO l = 1, 2
             fwavg(l,i,j) = fwavg(l,i,j) + fwflux(l,i,j) * rnyear
          END DO
       END DO
    END DO

    IF (iout == 1) THEN
       PRINT *, 'EMBM : writing averaged data at istep ', istep / REAL(ndta)
       CALL check_unit(2, __LINE__, __FILE__)
       OPEN(2,FILE=outdir_name(1:lenout)//lout//'.osc.'//ext,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       WRITE (2,10,IOSTAT=ios) tqavg
       CALL check_iostat(ios, __LINE__, __FILE__)
       WRITE (2,10,IOSTAT=ios) &
            & (((fx0avg(l,i,j), i = 1, imax), j = 1, jmax), l = 1, 4)
       CALL check_iostat(ios, __LINE__, __FILE__)
       WRITE (2,10,IOSTAT=ios) &
            & (((fwavg(l,i,j), i = 1, imax), j = 1, jmax), l = 1, 2)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CLOSE(2,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)

       PRINT *, 'Writing EMBM mean annual netCDF file at time', istep
       CALL ini_netcdf_embm(istep, 2)
       CALL write_netcdf_embm(k1, tqavg, q_pa_avg, rq_pa_avg, &
            & fx0avg, fwavg, work, maxi, maxj, 2)
       CALL end_netcdf_embm(2)
       PRINT *

       ! Increment average counter
       iav = iav + 1

       ! Perform diagnostics on averaged data, either by rewriting other diag
       ! routines to accept data as argument, or by simply copying code,
       ! otherwise diagnose by integrating one (short) step from .avg file.
       FORALL (i=1:imax) lon(i) = 180.0 * (phi0 + (i - 0.5) * dphi) / pi
       FORALL (j=1:jmax) lat(j) = 180.0 * ASIN(s(j)) / pi
       err3 = err_embm(tqavg(1,1:imax,1:jmax), 1, imax, jmax, indir_name, &
            & lenin, tdatafile, lentdata, tdata_scaling, tdata_offset, &
            & tqinterp, tdata_varname, tdata_missing, lon, lat)
       IF (qdata_rhum) THEN
          err4 = err_embm(rq_pa_avg, 2, imax, jmax, indir_name, &
               & lenin, qdatafile, lenqdata, qdata_scaling, qdata_offset, &
               & tqinterp, qdata_varname, qdata_missing, lon, lat)
       ELSE
          err4 = err_embm(q_pa_avg, 2, imax, jmax, indir_name, &
               & lenin, qdatafile, lenqdata, qdata_scaling, qdata_offset, &
               & tqinterp, qdata_varname, qdata_missing, lon, lat)
       END IF
       PRINT *, 'err_embm annual average composite = ', &
            & SQRT(((err3**2 * imax * jmax) + (err4**2 * imax * jmax)) / &
            & (2 * imax * jmax))

       ! Calculate the atmosphere water content, wateratm
       watereb = 0.0
       DO j = 1, jmax
          DO i = 1, imax
             watereb = watereb + tqavg(2,i,j) * ds(j)
          END DO
       END DO
       vsc = dphi * rsc * rsc * 1.0E-12
       wateratm = (watereb * rhoao * hatmbl(2)) * vsc

       PRINT *, 'EMBM : resetting averaged data arrays at step', &
            & istep / REAL(ndta)
       tqavg = 0.0 ; q_pa_avg = 0.0 ; rq_pa_avg = 0.0
       fx0avg  = 0.0 ; fwavg   = 0.0
       PRINT *
    END IF

10  FORMAT(e14.7)

  END SUBROUTINE diagosc_embm


  ! Return an RMS error value for the specified EMBM model field
  ! compared with the contents of the supplied data file.
  FUNCTION err_embm(modeldata, tracerid, imax, jmax, indir_name, &
       & lenin, obsdatafile, lenobsdata, datascaling, dataoffset, &
       & interpolate, varname, missing, lon, lat)
    IMPLICIT NONE
    REAL :: err_embm
    REAL :: modeldata(imax,jmax)        ! Model data field
    INTEGER :: tracerid                 ! Data field type
    INTEGER :: imax, jmax               ! Grid size
    CHARACTER(LEN=100) :: indir_name    ! EMBM input/output directories
    INTEGER :: lenin
    CHARACTER(LEN=128) :: obsdatafile   ! EMBM T/S data files
    INTEGER :: lenobsdata
    ! Scaling to convert obs. data for comparison with model
    REAL :: datascaling, dataoffset
    ! If '.false.', read obs. data of model-grid resolution from file;
    ! if '.true.', read obs. data from NetCDF file and interpolate.
    LOGICAL :: interpolate
    CHARACTER(LEN=25) :: varname        ! Observation-based dataset
    REAL :: missing
    REAL :: lon(imax), lat(jmax)        ! EMBM grid

    REAL :: errw

    ! Observational data, average and variance
    REAL :: obsdata(imax,jmax), obsdata_av, obsdata_var

    CALL read_embm_target_field(tracerid, imax, jmax, indir_name, lenin, &
         & obsdatafile, lenobsdata, datascaling, dataoffset, interpolate, &
         & varname, missing, lon, lat, obsdata)

    ! Calculate weights based on variance of data NB not real spatial but
    ! computational spatial
    obsdata_av = SUM(obsdata)
    obsdata_var = SUM(obsdata**2)
    obsdata_av = obsdata_av / (imax * jmax)
    obsdata_var = obsdata_var / (imax * jmax) - obsdata_av * obsdata_av
    errw = 1.0 / obsdata_var

    ! Calculate the RMS error
    err_embm = SQRT(SUM(errw * (modeldata - obsdata)**2) / (imax * jmax))
  END FUNCTION err_embm


  ! Reading in of data-based target fields for comparison with the model's
  ! internal fields.
  SUBROUTINE read_embm_target_field(tracerid, imax, jmax, indir_name, &
       & lenin, obsdatafile, lenobsdata, datascaling, dataoffset, &
       & interpolate, varname, missing, lon, lat, obsdata)
    USE genie_util, ONLY: check_unit, check_iostat, die
    USE local_netcdf
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: tracerid                ! Data field type
    INTEGER, INTENT(IN) :: imax, jmax              ! Grid size
    CHARACTER(LEN=100), INTENT(IN) :: indir_name   ! EMBM input/output dir.
    INTEGER, INTENT(IN) :: lenin
    CHARACTER(LEN=128), INTENT(IN) :: obsdatafile  ! EMBM T/S data files
    INTEGER, INTENT(IN) :: lenobsdata
    REAL, INTENT(IN) :: datascaling, dataoffset
    LOGICAL, INTENT(IN) :: interpolate
    CHARACTER(LEN=25), INTENT(IN) :: varname
    REAL, INTENT(IN) :: missing
    REAL, INTENT(INOUT) :: lon(imax), lat(jmax)    ! EMBM grid
    REAL, INTENT(OUT) :: obsdata(imax,jmax)

    ! Observation-based dataset
    TYPE(real2dVar), DIMENSION(1) :: tq_obs
    TYPE(real1dVar), DIMENSION(2) :: tq_obs_axis
    INTEGER :: nx_obs, ny_obs
    REAL, POINTER, DIMENSION(:,:) :: sinlat_obs
    INTEGER :: ncid_in, ncstatus, i_obs, j_obs, i_obs_min, j_obs_min
    INTEGER :: i0, i1, jtmp, ii, jj, iii, nwidth
    REAL :: obstmp
    REAL, DIMENSION(2,jmax) :: sinlat

    ! Interpolation
    INTEGER :: n_int, n_ext
    REAL :: distmean, distmax, rlon, rlat, testmask
    REAL :: dist, distmin, distminocean, cosdlon

    INTEGER :: status, i, j, ios

    i_obs_min = 0
    j_obs_min = 0

    IF (.NOT. interpolate) THEN
       ! Open the data file
       CALL check_unit(32, __LINE__, __FILE__)
       OPEN(32,FILE=indir_name(1:lenin)//obsdatafile(1:lenobsdata),IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       READ (32,*,IOSTAT=ios) ((obsdata(i,j), i = 1, imax), j = 1, jmax)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CLOSE(32,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)

       ! Apply a scaling for the humidity data
       IF (tracerid == 2) THEN
          obsdata = obsdata * 1.0E-3
       END IF
    ELSE
       ! Read in NetCDF dataset and interpolate onto model grid
       CALL openNetCDFRead(indir_name(1:lenin) // &
            & obsdatafile(1:lenobsdata), ncid_in)
       tq_obs(1)%name = varname
       CALL lookupVars(ncid_in, tq_obs)
       ! Size of observational dataset
       nx_obs = tq_obs(1)%dimLens(1)
       ny_obs = tq_obs(1)%dimLens(2)
       ALLOCATE(tq_obs(1)%data(0:nx_obs,1:ny_obs), STAT=status)
       IF (status /= 0) CALL die('Could not allocate memory')
       ncstatus = NF90_GET_VAR(ncid_in, tq_obs(1)%id, &
            & tq_obs(1)%data(1:nx_obs,1:ny_obs))
       IF (ncstatus /= NF90_NOERR) CALL handle_nc_err(ncstatus)
       tq_obs_axis(1)%name = tq_obs(1)%dimnames(1)
       tq_obs_axis(2)%name = tq_obs(1)%dimnames(2)
       ! Note, the zeroth longitude index represents the same values
       ! as for the last value (the actual coordinate is offset by 360
       ! degrees) to facilitate dealing with periodicity of the
       ! longitude
       CALL lookupVars(ncid_in,tq_obs_axis)
       ALLOCATE(tq_obs_axis(1)%data(0:nx_obs), STAT=status)
       IF (status /= 0) CALL die('Could not allocate memory')
       ALLOCATE(tq_obs_axis(2)%data(1:ny_obs), STAT=status)
       IF (status /= 0) CALL die('Could not allocate memory')
       ncstatus = NF90_GET_VAR(ncid_in, tq_obs_axis(1)%id, &
            & tq_obs_axis(1)%data(1:nx_obs))
       IF (ncstatus /= NF90_NOERR) CALL handle_nc_err(ncstatus)
       tq_obs_axis(1)%data(0) = tq_obs_axis(1)%data(nx_obs) - 360.0
       DO j = 1, ny_obs
          DO i = 1, nx_obs
             IF (ABS((tq_obs(1)%data(i,j) - missing) / missing) < 1.0E-5) THEN
                tq_obs(1)%data(i,j) = 9.99999E19
             END IF
          END DO
          tq_obs(1)%data(0,j) = tq_obs(1)%data(nx_obs,j)
       END DO
       ncstatus = NF90_GET_VAR(ncid_in, tq_obs_axis(2)%id, &
            & tq_obs_axis(2)%data)
       IF (ncstatus /= NF90_NOERR) CALL handle_nc_err(ncstatus)
       ! prepare auxiliary arrays:
       ! first index    function
       ! 1            sin()
       ! 2            cos()
       ALLOCATE(sinlat_obs(2,ny_obs), STAT=status)
       IF (status /= 0) CALL die('Could not allocate memory')
       ! For grid of observation-based dataset
       DO j = 1, ny_obs
          sinlat_obs(1,j) = SIN(tq_obs_axis(2)%data(j) * pi / 180.0)
          sinlat_obs(2,j) = COS(tq_obs_axis(2)%data(j) * pi / 180.0)
       END DO
       ! for GOLDSTEIN grid
       DO j = 1, jmax
          sinlat(1,j) = SIN(lat(j) * pi / 180.0)
          sinlat(2,j) = COS(lat(j) * pi / 180.0)
       END DO
       ! Flip latitudinal axis if required; test monotonicity of axis
       ! of observation-based dataset; convert GENIE's longitudinal
       ! axis to same range as that of the observational dataset
       IF (tq_obs_axis(2)%data(ny_obs) < tq_obs_axis(2)%data(1)) THEN
          DO j = 1, INT(ny_obs / 2 + 0.5)
             obstmp = tq_obs_axis(2)%data(j)
             tq_obs_axis(2)%data(j) = tq_obs_axis(2)%data(ny_obs+1-j)
             tq_obs_axis(2)%data(ny_obs+1-j) = obstmp
             DO i = 0, nx_obs
                obstmp = tq_obs(1)%data(i,j)
                tq_obs(1)%data(i,j) = tq_obs(1)%data(i,ny_obs+1-j)
                tq_obs(1)%data(i,ny_obs+1-j) = obstmp
             END DO
          END DO
       END IF
       DO i = 2, nx_obs
          IF (tq_obs_axis(1)%data(i) <= tq_obs_axis(1)%data(i-1)) &
               & CALL die('Non-incremental longitudinal axis')
       END DO
       DO j = 2, ny_obs
          IF (tq_obs_axis(2)%data(j) <= tq_obs_axis(2)%data(j-1)) &
               & CALL die('Non-incremental latitudinal axis')
       END DO
       DO i = 1, imax
          DO WHILE (lon(i) <= tq_obs_axis(1)%data(0))
             lon(i) = lon(i) + 360.0
          END DO
          DO WHILE (lon(i) > tq_obs_axis(1)%data(nx_obs))
             lon(i) = lon(i) - 360.0
          END DO
       END DO
       ! Bi-linear interpolation, parts of this code is based on the
       ! interpolation routine 'genie-cgoldstein/laz2siz.f' (modified
       ! from tri-linear to bi-linear interpolations), the
       ! "extrapolation" part has been replaced by a horizontal search
       ! for the nearest valid point on the sphere.
       n_int = 0
       distmean = 0.0
       distmax = 0.0
       n_ext = 0
       DO j = 1, jmax
          DO i = 1, imax
             ! find location of model grid point on observation-based
             ! grid.
             i_obs = 0
             DO WHILE (tq_obs_axis(1)%data(i_obs) < lon(i) .AND. &
                  & i_obs <= nx_obs)
                i_obs = i_obs + 1
             END DO
             ! This could possibly be done more general without the
             ! restriction that any model point has to be inside the
             ! extremes of the latitude coordinates of the
             ! observation-based grid
             j_obs = 1
             DO WHILE (tq_obs_axis(2)%data(j_obs) < lat(j) .AND. &
                  & j_obs <= ny_obs)
                j_obs = j_obs + 1
             END DO
             IF (i_obs == 0 .OR. i_obs > nx_obs .OR. &
                  & j_obs == 1 .OR. j_obs > ny_obs) THEN
                CALL die('Coordinates or depth outside of the' // &
                     & ' boundaries set by observational dataset')
             END IF
             rlon = (lon(i) - tq_obs_axis(1)%data(i_obs-1)) / &
                  & (tq_obs_axis(1)%data(i_obs) - tq_obs_axis(1)%data(i_obs-1))
             rlat = (lat(j)-tq_obs_axis(2)%data(j_obs-1)) / &
                  & (tq_obs_axis(2)%data(j_obs) - tq_obs_axis(2)%data(j_obs-1))
             testmask = MAX(tq_obs(1)%data(i_obs,j_obs), &
                  & tq_obs(1)%data(i_obs-1,j_obs), &
                  & tq_obs(1)%data(i_obs,j_obs-1), &
                  & tq_obs(1)%data(i_obs-1,j_obs-1))
             ! Interpolate if no land at corners of cube encompassing
             ! the model grid location
             IF (testmask < 1.0E10) THEN
                obsdata(i,j) = &
                     & (1.0 - rlon) * &
                     & ((1.0 - rlat) * tq_obs(1)%data(i_obs-1,j_obs-1) + &
                     &         rlat  * tq_obs(1)%data(i_obs-1,j_obs)) + &
                     & rlon * &
                     & ((1.0 - rlat) * tq_obs(1)%data(i_obs,j_obs-1) + &
                     &         rlat  * tq_obs(1)%data(i_obs,j_obs))
                n_int = n_int + 1
             ELSE
                ! Find horizonatlly nearest (true distance on sphere)
                ! point

                ! To compute arc distance dist between two points
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
                ! Start with rectangle defined by (i_obs-1,j_obs-1),
                ! (i_obs,j_obs), find within newly added points both
                ! the nearest valid point AND the nearest point
                distmin = pi
                distminocean = pi
                DO ii = 1, 2
                   DO jj = 1, 2
                      cosdlon = COS(pi * (lon(i) - &
                           & tq_obs_axis(1)%data(i_obs+1-ii)) / 180.0)
                      jtmp = j_obs + 1 - jj
                      dist = ACOS(sinlat(1,j) * sinlat_obs(1,jtmp) + &
                           & sinlat(2,j) * sinlat_obs(2,jtmp) * cosdlon)
                      distmin = MIN(distmin, dist)
                      testmask = MAX(tq_obs(1)%data(i_obs+1-ii,jtmp), &
                           & tq_obs(1)%data(i_obs+1-ii,jtmp))
                      IF (testmask < 1.0E10 .AND. distminocean > dist) THEN
                         distminocean = dist
                         i_obs_min = i_obs + 1 - ii
                         j_obs_min = jtmp
                      END IF
                   END DO
                END DO
                nwidth = 1
                ! Repeat until nearest of the newly added points is
                ! farther away than nearest valid point,
                DO WHILE (distmin < distminocean .AND. &
                     & nwidth < INT(nx_obs / 2) + 1 .AND. &
                     & nwidth < ny_obs)
                   distmin = pi
                   ! Add grid-point circumference around rectangle,
                   ! take into account periodicity in longitudinal
                   ! direction and also look across northern and
                   ! southern poles.  find nearest valid point AND
                   ! nearest point within newly added points
                   nwidth = nwidth + 1
                   ! Reflect i range if rectangle spreads across
                   ! northern or southern border
                   IF (j_obs - nwidth < 1) THEN
                      i0 = i_obs - nwidth - INT(nx_obs / 2)
                      i1 = i_obs - 1 + nwidth - INT(nx_obs / 2)
                      jtmp = ABS(j_obs - nwidth - 1)
                   ELSE
                      i0 = i_obs - nwidth
                      i1 = i_obs - 1 + nwidth
                      jtmp = j_obs - nwidth
                   END IF
                   DO ii = i0, i1
                      iii = MODULO(ii - 1, nx_obs) + 1
                      cosdlon = COS(pi * (lon(i) - &
                           & tq_obs_axis(1)%data(iii)) / 180.0)
                      dist = ACOS(sinlat(1,j) * sinlat_obs(1,jtmp) + &
                           & sinlat(2,j) * sinlat_obs(2,jtmp) * cosdlon)
                      distmin = MIN(distmin, dist)
                      testmask = MAX(tq_obs(1)%data(iii,j_obs-nwidth), &
                           & tq_obs(1)%data(iii,j_obs-nwidth))
                      IF (testmask < 1.0E10 .AND. distminocean > dist) THEN
                         distminocean = dist
                         i_obs_min = iii
                         j_obs_min = jtmp
                      END IF
                   END DO
                   ! Reflect i range if rectangle spreads across
                   ! northern or southern border
                   IF (j_obs-1+nwidth > ny_obs) THEN
                      i0 = i_obs - nwidth - INT(nx_obs / 2)
                      i1 = i_obs - 1 + nwidth - INT(nx_obs / 2)
                      jtmp = 2 * ny_obs - (j_obs + nwidth - 2)
                   ELSE
                      i0 = i_obs - nwidth
                      i1 = i_obs - 1 + nwidth
                      jtmp = j_obs - 1 + nwidth
                   END IF
                   DO ii = i0, i1
                      iii = MODULO(ii - 1, nx_obs) + 1
                      cosdlon = COS(pi * (lon(i) - &
                           & tq_obs_axis(1)%data(iii)) / 180.0)
                      dist = ACOS(sinlat(1,j) * sinlat_obs(1,jtmp) + &
                           & sinlat(2,j) * sinlat_obs(2,jtmp) * cosdlon)
                      distmin = MIN(distmin, dist)
                      testmask = MAX(tq_obs(1)%data(iii,j_obs-1+nwidth), &
                           & tq_obs(1)%data(iii,j_obs-1+nwidth))
                      IF (testmask < 1.0E10 .AND. distminocean > dist) THEN
                         distminocean = dist
                         i_obs_min = iii
                         j_obs_min = jtmp
                      END IF
                   END DO
                   DO jj = j_obs-nwidth+1, j_obs-2+nwidth
                      ! Reflect i range if rectangle spreads across
                      ! northern or southern border
                      IF (jj < 1 .OR. jj > ny_obs) THEN
                         iii = MODULO(i_obs-nwidth-1+INT(nx_obs/2), nx_obs)
                         IF (jj < 1) THEN
                            jtmp = ABS(jj-1)
                         ELSE
                            jtmp = 2 * ny_obs - (jj-1)
                         END IF
                      ELSE
                         iii = MODULO(i_obs-nwidth-1, nx_obs) + 1
                         jtmp = jj
                      END IF
                      cosdlon = COS(pi * (lon(i) - &
                           & tq_obs_axis(1)%data(iii)) / 180.0)
                      dist = ACOS(sinlat(1,j) * sinlat_obs(1,jtmp) + &
                           & sinlat(2,j) * sinlat_obs(2,jtmp) * cosdlon)
                      distmin = MIN(distmin, dist)
                      testmask = MAX(tq_obs(1)%data(iii,jj), &
                           & tq_obs(1)%data(iii,jj))
                      IF (testmask < 1.0E10 .AND. distminocean > dist) THEN
                         distminocean = dist
                         i_obs_min = iii
                         j_obs_min = jtmp
                      END IF
                   END DO
                   DO jj = j_obs-nwidth+1, j_obs-2+nwidth
                      ! Reflect i range if rectangle spreads across
                      ! northern or southern border
                      IF (jj < 1 .OR. jj > ny_obs) THEN
                         iii = MODULO(i_obs-2+nwidth+INT(nx_obs/2), nx_obs)
                         IF (jj < 1) THEN
                            jtmp = ABS(jj-1)
                         ELSE
                            jtmp = 2 * ny_obs - (jj-1)
                         END IF
                      ELSE
                         iii = MODULO(i_obs-2+nwidth, nx_obs) + 1
                         jtmp = jj
                      END IF
                      cosdlon = COS(pi * (lon(i) - &
                           & tq_obs_axis(1)%data(iii)) / 180.0)
                      dist = ACOS(sinlat(1,j) * sinlat_obs(1,jtmp) + &
                           & sinlat(2,j) * sinlat_obs(2,jtmp) * cosdlon)
                      distmin = MIN(distmin, dist)
                      testmask = MAX(tq_obs(1)%data(iii,jj), &
                           & tq_obs(1)%data(iii,jj))
                      IF (testmask < 1.0E10 .AND. distminocean > dist) THEN
                         distminocean = dist
                         i_obs_min = iii
                         j_obs_min = jtmp
                      END IF
                   END DO
                END DO
                ! Vertically interpolate at point with shortest
                ! distance from target point
                obsdata(i,j) = tq_obs(1)%data(i_obs_min,j_obs_min)
                distmean = distmean + distminocean
                IF (distminocean > distmax) THEN
                   distmax = distminocean
                END IF
                n_ext = n_ext + 1
             END IF
          END DO
       END DO
       IF (n_ext > 0) THEN
          distmean = distmean / REAL(n_ext)
       END IF
       print *, 'fraction of interpolated points,'
       print *, 'fraction of extrapolated points,'
       print *, 'mean distance of extrapolated points (degrees),'
       print *, 'maximum distance of extrapolated point (degrees)'
       print *, REAL(n_int) / REAL(n_int+n_ext), &
            & REAL(n_ext) / REAL(n_int+n_ext), &
            & distmean * 180.0 / pi, distmax * 180.0 / pi

       ! Clean up
       DEALLOCATE(sinlat_obs, STAT=status)
       IF (status /= 0) CALL die('Could not allocate memory')
       DEALLOCATE(tq_obs_axis(1)%data, STAT=status)
       IF (status /= 0) CALL die('Could not allocate memory')
       DEALLOCATE(tq_obs_axis(2)%data, STAT=status)
       IF (status /= 0) CALL die('Could not allocate memory')
       DEALLOCATE(tq_obs(1)%data, STAT=status)
       IF (status /= 0) CALL die('Could not allocate memory')
       CALL closeNetCDF(ncid_in)

       ! Apply a scaling for the humidity data
       obsdata = obsdata / datascaling - dataoffset
    END IF
  END SUBROUTINE read_embm_target_field

END MODULE embm_diag
