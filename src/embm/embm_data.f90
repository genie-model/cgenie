MODULE embm_data

  USE embm_lib
  USE embm_netcdf
  IMPLICIT NONE

CONTAINS

  ! This module reads netcdf restarts for EMBM
  SUBROUTINE inm_netcdf_embm
    IMPLICIT NONE

    REAL, DIMENSION(maxi,maxj) :: temp_read, shum_read
    INTEGER :: iday, ifail, ncid
    LOGICAL :: lexist
    CHARACTER(LEN=200) :: fnamein
    REAL :: timestep
    INTEGER :: i, j
    REAL :: tmp_val(2), area

    timestep = 24.0 * 60.0 * 60.0 * yearlen / REAL(nyear * ndta)
    fnamein = TRIM(filenetin)
    ifail = 0
    INQUIRE(FILE=TRIM(fnamein), EXIST=lexist)
    IF (.NOT. lexist) THEN
       PRINT *, ' Missing file ', TRIM(fnamein)
       ifail = 1
    END IF
    IF (ifail /= 0) THEN
       PRINT *, ' Correct error and try again '
       STOP 1
    END IF

    PRINT *, ' embm: Opening restart file for read: ', &
         & TRIM(filenetin)

    CALL open_file_nc(TRIM(fnamein), ncid)
    CALL get1di_data_nc(ncid, 'ioffset', 1, ioffset_rest, ifail)
    CALL get1di_data_nc(ncid, 'iyear', 1, iyear_rest, ifail)
    CALL get1di_data_nc(ncid, 'imonth', 1, imonth_rest, ifail)
    CALL get1di_data_nc(ncid, 'iday', 1, iday, ifail)
    CALL get2d_data_nc(ncid, 'air_temp', maxi, maxj, temp_read, ifail)
    CALL get2d_data_nc(ncid, 'humidity', maxi, maxj, shum_read, ifail)
    CALL close_file_nc(TRIM(filenetin), ncid)

    day_rest = iday
    ioffset_rest = MOD(ioffset_rest, NINT(yearlen))
    day_rest = day_rest + timestep / (24 * 60 * 60.0)
    ! This bit so that we don't get too far out in our count....
    ! Anchor to a day if we start drifting.
    ! Means timestep can never be less than 1/1000 of a day!!!!
    IF (ABS(iday - day_rest) <= 1.0E-3) day_rest = iday
    IF (day_rest >= 31) THEN
       day_rest = day_rest - 30
       imonth_rest = imonth_rest + 1
       IF (imonth_rest == 13) THEN
          imonth_rest = 1
          iyear_rest = iyear_rest + 1
       END IF
    END IF
    iday = NINT(day_rest)
    PRINT *, 'day in embm restart is now', day_rest
    PRINT *, 'iday in embm restart is now', iday

    tq(1,1:maxi,1:maxj) = temp_read(:,:)
    tq(2,1:maxi,1:maxj) = shum_read(:,:)

    WRITE (*,220) 'Avg T', 'Avg Q'
    tmp_val = 0
    area = 0.0
    DO j = 1, maxj
       DO i = 1, maxi
          area = area + ds(j)
          tmp_val = tmp_val + tq(:,i,j) * ds(j)
       END DO
    END DO
    WRITE (*,210) tmp_val(1) / area, (tmp_val(2) / area) * 1000.0
210 FORMAT(2f13.9)
220 FORMAT(2a13)

  END SUBROUTINE inm_netcdf_embm


  ! This module writes netcdf restarts for embm
  SUBROUTINE outm_netcdf_embm(istep)
    USE netcdf
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: istep

    REAL, DIMENSION(maxi,maxj) :: temp_write, shum_write
    REAL :: lons1(maxi), lats1(maxj)
    INTEGER :: i, j, ntempid, nshumid
    INTEGER :: nlon1id, nlongit1id, nlat1id, nlatit1id, nrecsid(1), ioffid
    INTEGER :: dim1pass(2)
    CHARACTER(LEN=200) :: fname

    ! For date and restarts...
    INTEGER :: iday, iyearid, imonthid, idayid
    CHARACTER(LEN=10) :: yearstring
    CHARACTER(LEN=2) :: monthstring, daystring
    CHARACTER(LEN=7) :: datestring

    ! For netcdf...
    INTEGER :: ncid
    REAL :: timestep

    REAL :: tmp_val(2), area

    ! output file format is yyyy_mm_dd
    timestep = yearlen / REAL(nyear * ndta)
    iday = NINT(day_rest)
    IF (mod(istep, iwstp * ndta) == 0)THEN
       ! WRITE A RESTART.....
       lons1 = nclon1
       lats1 = nclat1
       temp_write(:,:) = REAL(tq(1,1:maxi,1:maxj))
       shum_write(:,:) = REAL(tq(2,1:maxi,1:maxj))

       WRITE (datestring,'(i7.7)') istep
       WRITE (yearstring,'(i10)') iyear_rest
       WRITE (monthstring,'(i2.2)') imonth_rest
       WRITE (daystring,'(i2.2)') iday

       fname = TRIM(dirnetout) // '/embm_restart_' // &
            & TRIM(ADJUSTL(yearstring)) // '_' // &
            & monthstring // '_' // daystring // '.nc'
       PRINT *, ' Opening netcdf restart file for write: ', TRIM(fname)
       CALL check_err(NF90_CREATE(TRIM(fname), NF90_CLOBBER, ncid))
       CALL check_err(NF90_DEF_DIM(ncid, 'nrecs', 1, nrecsid(1)))
       CALL check_err(NF90_DEF_DIM(ncid, 'longitude', maxi, nlon1id))
       CALL check_err(NF90_DEF_DIM(ncid, 'latitude', maxj, nlat1id))

       CALL check_err(NF90_DEF_VAR(ncid, 'longitude', &
            & NF90_REAL, (/ nlon1id /), nlongit1id))
       CALL check_err(NF90_DEF_VAR(ncid, 'latitude', &
            & NF90_REAL, (/ nlat1id /), nlatit1id))
       dim1pass = (/ nlon1id, nlat1id /)
       CALL check_err(NF90_DEF_VAR(ncid, 'ioffset', NF90_INT, nrecsid, ioffid))
       CALL check_err(NF90_DEF_VAR(ncid, 'iyear', NF90_INT, nrecsid, iyearid))
       CALL check_err(NF90_DEF_VAR(ncid, 'imonth', NF90_INT, nrecsid, imonthid))
       CALL check_err(NF90_DEF_VAR(ncid, 'iday', NF90_INT, nrecsid, idayid))

       CALL check_err(NF90_DEF_VAR(ncid, 'air_temp', &
            & NF90_DOUBLE, dim1pass, ntempid))
       CALL check_err(NF90_DEF_VAR(ncid, 'humidity', &
            & NF90_DOUBLE, dim1pass, nshumid))
       CALL check_err(NF90_ENDDEF(ncid))

       CALL check_err(NF90_PUT_VAR(ncid, iyearid, iyear_rest))
       CALL check_err(NF90_PUT_VAR(ncid, imonthid, imonth_rest))
       CALL check_err(NF90_PUT_VAR(ncid, idayid, iday))
       CALL check_err(NF90_PUT_VAR(ncid, ioffid, ioffset_rest))

       CALL check_err(NF90_PUT_VAR(ncid, nlongit1id, lons1))
       CALL check_err(NF90_PUT_VAR(ncid, nlatit1id, lats1))
       CALL check_err(NF90_PUT_VAR(ncid, ntempid, temp_write))
       CALL check_err(NF90_PUT_VAR(ncid, nshumid, shum_write))

       CALL check_err(NF90_CLOSE(ncid))

       WRITE (*,220) 'Avg T','Avg Q'
       tmp_val = 0.0
       area = 0.0
       DO j = 1, maxj
          DO i = 1, maxi
             area = area + ds(j)
             tmp_val = tmp_val + tq(:,i,j) * ds(j)
          END DO
       END DO
       WRITE (*,210) tmp_val(1) / area, (tmp_val(2) / area) * 1000.0

210    FORMAT(2f13.9)
220    FORMAT(2a13)
    END IF

    day_rest = day_rest + timestep
    ! This bit so that we don't get too far out in our count....
    ! Anchor to a day if we start drifting.
    ! Means timestep can never be less than 1/1000 of a day!!!!
    IF (ABS(iday - day_rest) <= 1e-3) THEN
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
  END SUBROUTINE outm_netcdf_embm


  subroutine outm_surf(unit, co2_in, albedo_ocn, usurf_ocn, latent_ocn, &
       & sensible_ocn, netsolar_ocn, netlong_ocn, evap_ocn, pptn_ocn, &
       & runoff_ocn, latent_atm, sensible_atm, netsolar_atm, netlong_atm, &
       & evap_atm, pptn_atm,dhght_sic, darea_sic)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: unit
    REAL, DIMENSION(maxi,maxj), INTENT(IN) :: &
         & co2_in, usurf_ocn, albedo_ocn, latent_ocn, sensible_ocn, &
         & netsolar_ocn, netlong_ocn, evap_ocn, pptn_ocn, runoff_ocn, &
         & latent_atm, sensible_atm, netsolar_atm, netlong_atm, &
         & evap_atm, pptn_atm, dhght_sic, darea_sic

    WRITE (unit,10) latent_ocn
    WRITE (unit,10) sensible_ocn
    WRITE (unit,10) netsolar_ocn
    WRITE (unit,10) netlong_ocn
    WRITE (unit,10) evap_ocn
    WRITE (unit,10) pptn_ocn
    WRITE (unit,10) runoff_ocn
    WRITE (unit,10) albedo_ocn
    WRITE (unit,10) usurf_ocn
    WRITE (unit,10) latent_atm
    WRITE (unit,10) sensible_atm
    WRITE (unit,10) netsolar_atm
    WRITE (unit,10) netlong_atm
    WRITE (unit,10) evap_atm
    WRITE (unit,10) pptn_atm
    WRITE (unit,10) co2_in
    WRITE (unit,10) dhght_sic
    WRITE (unit,10) darea_sic

10  FORMAT(e21.13)
  END SUBROUTINE outm_surf

END MODULE embm_data
