MODULE goldstein_data

  USE goldstein_lib
  USE goldstein_netcdf
  IMPLICIT NONE

CONTAINS

  ! Read NetCDF restarts for goldstein
  SUBROUTINE inm_netcdf(lrestart_genie)
    IMPLICIT NONE
    LOGICAL, INTENT(IN) :: lrestart_genie

    REAL, DIMENSION(maxi,maxj,maxk) :: &
         & temp_read, salinity_read, uvel_read, vvel_read
    REAL, DIMENSION(maxi,maxj) :: evap_read, late_read, sens_read

    ! For date and restarts...
    INTEGER :: iday

    ! For netcdf...
    INTEGER :: ifail, ncid
    LOGICAL :: lexist
    CHARACTER(LEN=200) :: fnamein

    REAL :: timestep

    ! extra variables for printing out average model properties
    INTEGER :: i, j, k, l, icell
    REAL :: tmp_val(4)

    timestep = 24.0 * 60.0 * 60.0 * yearlen / REAL(nyear)
    fnamein = TRIM(filenetin)
    ifail = 0
    INQUIRE(FILE=TRIM(fnamein),EXIST=lexist)
    IF (.NOT. lexist) THEN
       PRINT *, ' Missing file ', TRIM(fnamein)
       ifail = 1
    END IF
    IF (ifail /= 0) THEN
       PRINT *, ' Correct error and try again '
       STOP 1
    END IF

    PRINT *, 'GOLDSTEIN: Opening restart file for read: ', TRIM(filenetin)

    call open_file_nc(TRIM(fnamein), ncid)
    call get1di_data_nc(ncid, 'ioffset', 1, ioffset_rest, ifail)
    call get1di_data_nc(ncid, 'iyear', 1, iyear_rest, ifail)
    call get1di_data_nc(ncid, 'imonth', 1, imonth_rest, ifail)
    call get1di_data_nc(ncid, 'iday', 1, iday, ifail)
    call get3d_data_nc(ncid, 'temp', maxi, maxj, maxk, temp_read, ifail)
    call get3d_data_nc(ncid, 'salinity', maxi, maxj, maxk, salinity_read, ifail)
    call get3d_data_nc(ncid, 'uvel', maxi, maxj, maxk, uvel_read, ifail)
    call get3d_data_nc(ncid, 'vvel', maxi, maxj, maxk, vvel_read, ifail)
    call get2d_data_nc(ncid, 'evap', maxi, maxj, evap_read, ifail)
    call get2d_data_nc(ncid, 'late', maxi, maxj, late_read, ifail)
    call get2d_data_nc(ncid, 'sens', maxi, maxj, sens_read, ifail)
    call close_file_nc(TRIM(filenetin), ncid)

    day_rest = iday
    ioffset_rest = MOD(ioffset_rest, NINT(yearlen))

    day_rest = day_rest + timestep / (24 * 60 * 60.0)
    ! This bit so that we don't get too far out in our count....
    ! Anchor to a day if we start drifting.
    ! Means timestep can never be less than 1/1000 of a day!!!!
    IF (ABS(iday-day_rest) <= 1.0E-3) day_rest = iday
    IF (day_rest >= 31) THEN
       day_rest = day_rest - 30
       imonth_rest = imonth_rest + 1
       IF (imonth_rest == 13) THEN
          imonth_rest = 1
          iyear_rest = iyear_rest + 1
       END IF
    END IF
    iday = NINT(day_rest)
    IF (debug_init) PRINT *, 'day in goldstein restart is now', day_rest
    IF (debug_init) PRINT *, 'iday in goldstein restart is now', iday

    ts(1,1:maxi,1:maxj,1:maxk) = temp_read(:,:,:)
    ts(2,1:maxi,1:maxj,1:maxk) = salinity_read(:,:,:)
    u1(1,1:maxi,1:maxj,1:maxk) = uvel_read(:,:,:)
    u1(2,1:maxi,1:maxj,1:maxk) = vvel_read(:,:,:)
    u(:,:,:,:) = u1(:,:,:,:)

    ! The i and j below were :,: but this fails at
    ! compilation time in the 64x32 case for a
    ! reason I can't work out.  DJL 27/10/2005
    IF (lrestart_genie) THEN
       DO j = 1, maxj
          DO i = 1, maxi
             evap_save2(i,j) = evap_read(i,j)
             late_save2(i,j) = late_read(i,j)
             sens_save2(i,j) = sens_read(i,j)
          END DO
       END DO
    ELSE
       DO j = 1, maxj
          DO i = 1, maxi
             evap_save2(i,j) = 0.0
             late_save2(i,j) = 0.0
             sens_save2(i,j) = 0.0
          END DO
       END DO
    END IF

    ! Write out layer averages for restart checks
    IF (debug_init) &
         & WRITE (*,120) 'Layer', 'Avg T', 'Avg S', 'Avg U', 'Avg V', 'Cells'
    DO k = 1, maxk
       ! Clear temporary variables
       tmp_val = 0
       icell = 0

       ! Sum layer state variables and flow field
       DO j = 1, maxj
          DO i = 1, maxi
             IF (k >= k1(i,j)) icell = icell + 1
             DO l = 1, 2
                IF (k >= k1(i,j)) THEN
                   tmp_val(l) = tmp_val(l) + ts(l,i,j,k)
                END IF
                tmp_val(l+2) = tmp_val(l+2) + u1(l,i,j,k)

                ! Set up u array from read-in u1 array
                u(l,i,j,k) = u1(l,i,j,k)
             END DO
          END DO
       END DO

       ! Print average values out
       IF (debug_init) &
            & WRITE (*,110) k, tmp_val(1) / icell, &
            & (tmp_val(2) / icell) + saln0, tmp_val(3) / (maxi * maxj), &
            & tmp_val(4) / (maxi * maxj), icell
    END DO

110 FORMAT(i5,2f13.9,2e17.9,i4)
120 FORMAT(a5,2a13,2a17,a6)
  END SUBROUTINE inm_netcdf


  ! Write NetCDF restarts for goldstein
  SUBROUTINE outm_netcdf(istep)
    USE netcdf
    IMPLICIT NONE
    INTEGER, INTENT(in) :: istep

    REAL, dimension(maxi,maxj,maxk) :: &
         & temp_write, salinity_write, uvel_write, vvel_write
    REAL, DIMENSION(maxi,maxj) :: evap_write, late_write, sens_write

    REAL :: lons1(maxi), lats1(maxj), depths1(maxk)
    INTEGER :: landmask(maxi,maxj,maxk)
    INTEGER :: i, j, k

    INTEGER ::ntempid, nsalinityid, nuvelid, nvvelid, nevapid, nlateid, nsensid
    INTEGER :: nlon1id, nlongit1id, ndep1id, nlat1id, nlatit1id, ndepth1id
    INTEGER :: nrecsid(1), ioffsetid, ncid
    INTEGER :: dim1pass(3), dimpass(2)
    CHARACTER(LEN=200) :: fname

    ! For date and restarts...
    INTEGER :: iday, iyearid, imonthid, idayid
    CHARACTER(LEN=10) :: yearstring
    CHARACTER(LEN=2) :: monthstring, daystring
    CHARACTER(LEN=7) :: datestring
    REAL :: timestep

    ! Extra variables for printing out average model properties
    INTEGER :: l, icell
    REAL :: tmp_val(4)

    ! Output file format is yyyy_mm_dd

    timestep = yearlen / REAL(nyear)
    iday = NINT(day_rest)

    IF (MOD(istep, iwstp) == 0) THEN
       ! WRITE A RESTART.....

       ! This bit modified from initialise_ocean.F
       lons1 = nclon1
       lats1 = nclat1
       DO k = 1, maxk
          depths1(k) = ncdepth(maxk-k+1)
       END DO

       landmask(:,:,:) = 0
       DO i = 1, maxi
          DO j = 1, maxj
             DO k = 1, maxk
                IF (k >= k1(i,j)) landmask(i,j,:) = 1
             END DO
          END DO
       END DO

       temp_write(:,:,:) = REAL(ts(1,1:maxi,1:maxj,1:maxk) * landmask(:,:,:))
       salinity_write(:,:,:) = &
            & REAL(ts(2,1:maxi,1:maxj,1:maxk) * landmask(:,:,:))
       uvel_write(:,:,:) = REAL(u(1,1:maxi,1:maxj,1:maxk))
       vvel_write(:,:,:) = REAL(u(2,1:maxi,1:maxj,1:maxk))
       evap_write(:,:) = evap_save2(:,:)
       late_write(:,:) = late_save2(:,:)
       sens_write(:,:) = sens_save2(:,:)

       WRITE (datestring,'(i7.7)') istep
       WRITE (yearstring,'(i10)') iyear_rest
       WRITE (monthstring,'(i2.2)') imonth_rest
       WRITE (daystring,'(i2.2)') iday

       !-------------------------------------------------------
       ! create a netcdf file
       !-------------------------------------------------------
       fname = TRIM(dirnetout) // '/goldstein_restart_' // &
            & TRIM(adjustl(yearstring)) // '_' // monthstring // &
            & '_'//daystring//'.nc'
       PRINT *, ' Opening netcdf restart file for write: ', TRIM(fname)
       CALL check_err(NF90_CREATE(TRIM(fname), NF90_CLOBBER, ncid))
       CALL check_err(NF90_DEF_DIM(ncid, 'nrecs', 1, nrecsid(1)))
       CALL check_err(NF90_DEF_DIM(ncid, 'longitude', maxi, nlon1id))
       CALL check_err(NF90_DEF_DIM(ncid, 'latitude', maxj, nlat1id))
       CALL check_err(NF90_DEF_DIM(ncid, 'depth', maxk, ndep1id))

       CALL check_err(NF90_DEF_VAR(ncid, 'longitude', &
            & NF90_REAL, (/ nlon1id /), nlongit1id))
       CALL check_err(NF90_DEF_VAR(ncid, 'latitude', &
            & NF90_REAL, (/ nlat1id /), nlatit1id))
       CALL check_err(NF90_DEF_VAR(ncid, 'depth', NF90_REAL, &
            & (/ ndep1id /), ndepth1id))
       dim1pass(1) = nlon1id
       dim1pass(2) = nlat1id
       dim1pass(3) = ndep1id
       dimpass(1) = nlon1id
       dimpass(2) = nlat1id
       CALL check_err(NF90_DEF_VAR(ncid, 'ioffset', &
            & NF90_INT, nrecsid, ioffsetid))
       CALL check_err(NF90_DEF_VAR(ncid, 'iyear', NF90_INT, nrecsid, iyearid))
       CALL check_err(NF90_DEF_VAR(ncid, 'imonth', NF90_INT, nrecsid, imonthid))
       CALL check_err(NF90_DEF_VAR(ncid, 'iday', NF90_INT, nrecsid, idayid))
       CALL check_err(NF90_DEF_VAR(ncid, 'temp', NF90_DOUBLE, &
            & dim1pass, ntempid))
       CALL check_err(NF90_DEF_VAR(ncid, 'salinity', NF90_DOUBLE, &
            & dim1pass, nsalinityid))
       CALL check_err(NF90_DEF_VAR(ncid, 'uvel', NF90_DOUBLE, &
            & dim1pass, nuvelid))
       CALL check_err(NF90_DEF_VAR(ncid, 'vvel', NF90_DOUBLE, &
            & dim1pass, nvvelid))
       CALL check_err(NF90_DEF_VAR(ncid, 'evap', NF90_DOUBLE, &
            & dimpass(1:2), nevapid))
       CALL check_err(NF90_DEF_VAR(ncid, 'late', NF90_DOUBLE, &
            & dimpass(1:2), nlateid))
       CALL check_err(NF90_DEF_VAR(ncid, 'sens', NF90_DOUBLE, &
            & dimpass(1:2), nsensid))

       CALL check_err(NF90_PUT_ATT(ncid, nlongit1id, 'units', 'degrees_east'))
       CALL check_err(NF90_PUT_ATT(ncid, nlongit1id, 'long_name', 'longitude'))
       CALL check_err(NF90_PUT_ATT(ncid, nlatit1id, 'units', 'degrees_north'))
       CALL check_err(NF90_PUT_ATT(ncid, nlatit1id, 'long_name', 'latitude'))

       CALL check_err(NF90_ENDDEF(ncid))

       CALL check_err(NF90_PUT_VAR(ncid, iyearid, iyear_rest))
       CALL check_err(NF90_PUT_VAR(ncid, imonthid, imonth_rest))
       CALL check_err(NF90_PUT_VAR(ncid, idayid, iday))
       CALL check_err(NF90_PUT_VAR(ncid, ioffsetid, ioffset_rest))

       CALL check_err(NF90_PUT_VAR(ncid, nlongit1id, lons1))
       CALL check_err(NF90_PUT_VAR(ncid, nlatit1id, lats1))
       CALL check_err(NF90_PUT_VAR(ncid, ndepth1id, depths1))
       CALL check_err(NF90_PUT_VAR(ncid, ntempid, temp_write))
       CALL check_err(NF90_PUT_VAR(ncid, nsalinityid, salinity_write))
       CALL check_err(NF90_PUT_VAR(ncid, nuvelid, uvel_write))
       CALL check_err(NF90_PUT_VAR(ncid, nvvelid, vvel_write))
       CALL check_err(NF90_PUT_VAR(ncid, nevapid, evap_write))
       CALL check_err(NF90_PUT_VAR(ncid, nlateid, late_write))
       CALL check_err(NF90_PUT_VAR(ncid, nsensid, sens_write))

       CALL check_err(NF90_CLOSE(ncid))

       ! Write out layer averages for restart checks
       WRITE (*,120) 'Layer', 'Avg T', 'Avg S', 'Avg U', 'Avg V', 'Cells'
       DO k = 1, maxk
          ! Clear temporary variables
          tmp_val = 0
          icell = 0

          ! Sum layer state variables and flow field
          DO j = 1, maxj
             DO i = 1, maxi
                IF (k >= k1(i,j)) icell = icell + 1
                DO l = 1, 2
                   IF (k >= k1(i,j)) THEN
                      tmp_val(l) = tmp_val(l) + ts(l,i,j,k)
                   END IF
                   tmp_val(l+2) = tmp_val(l+2) + u(l,i,j,k)
                END DO
             END DO
          END DO

          ! Print average values out
          WRITE (*,110) k, tmp_val(1) / icell, &
               & (tmp_val(2) / icell) + saln0, &
               & tmp_val(3) / (maxi * maxj), tmp_val(4) / (maxi * maxj), icell
       END DO

110    FORMAT(i5,2f13.9,2e17.9,i4)
120    FORMAT(a5,2a13,2a17,a6)
    END IF

    day_rest = day_rest + timestep
    ! This bit so that we don't get too far out in our count....
    ! Anchor to a day if we start drifting.
    ! Means timestep can never be less than 1/1000 of a day!!!!
    IF (ABS(iday - day_rest) <= 1.0E-3) THEN
       PRINT *, 'CORRECTING TIME-LAG! in outm_netcdf in genie-goldstein', &
            & iday, day_rest
       day_rest = iday
    END IF
    IF (day_rest >= 31) THEN
       day_rest = day_rest - 30
       imonth_rest = imonth_rest + 1
       IF (imonth_rest == 13) THEN
          imonth_rest = 1
          iyear_rest = iyear_rest + 1
       END IF
    END IF
    iday = NINT(day_rest)

  END SUBROUTINE outm_netcdf

END MODULE goldstein_data
