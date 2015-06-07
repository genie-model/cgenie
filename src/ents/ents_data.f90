MODULE ents_data

  USE ents_lib
  USE ents_netcdf
  IMPLICIT NONE

CONTAINS

  ! Reads restarts from NetCDF
  SUBROUTINE in_ents_netcdf(fname, land_snow_lnd)
    USE netcdf
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: fname
    REAL, DIMENSION(:,:), INTENT(INOUT) :: land_snow_lnd

    INTEGER :: i, j
    CHARACTER(LEN=20), DIMENSION(11) :: labels = &
         & (/ 'photosynthesis', 'veg_resp      ', 'leaf          ', &
         &    'soil_resp     ', 'vegC          ', 'soilC         ', &
         &    'fv            ', 'land_temp     ', 'land_water    ', &
         &    'snow          ', 'pco2ld        ' /)
    INTEGER :: kk, var_id, ncid
    CHARACTER(LEN=200) :: var_name
    REAL :: tempdata1, tempdata(maxi,maxj)

    DO kk = 1, 11
       var_name = labels(kk)
       CALL check_err(NF90_OPEN(fname, NF90_NOWRITE, ncid))
       CALL check_err(NF90_INQ_VARID(ncid, var_name, var_id))
       IF (kk /= 11) THEN
          CALL check_err(NF90_GET_VAR(ncid, var_id, tempdata))
       ELSE
          CALL check_err(NF90_GET_VAR(ncid, var_id, tempdata1))
       END IF
       CALL check_err(NF90_CLOSE(ncid))
       DO j = 1, maxj
          DO i = 1, maxi
             SELECT CASE (kk)
             CASE (1)
                photo(j,i) = tempdata(i,j)
             CASE (2)
                respveg(j,i) = tempdata(i,j)
             CASE (3)
                leaf(j,i) = tempdata(i,j)
             CASE (4)
                respsoil(j,i) = tempdata(i,j)
             CASE (5)
                Cveg(j,i) = tempdata(i,j)
             CASE (6)
                Csoil(j,i) = tempdata(i,j)
             CASE (7)
                fv(j,i) = tempdata(i,j)
             CASE (8)
                tqld(1,j,i) = tempdata(i,j)
             CASE (9)
                tqld(2,j,i) = tempdata(i,j)
             CASE (10)
                land_snow_lnd(j,i) = tempdata(i,j)
             CASE (11)
                pco2ld = tempdata1
             END SELECT
          END DO
       END DO
    END DO

    ! Initialise water bucket capacity
    WHERE (ents_k1 > ents_kmax)
       bcap = MIN(k8, k9 + (k10 * Csoil))
       ! initial roughness length
       z0 = MAX(0.001, kz0 * Cveg)
    END WHERE
  END SUBROUTINE in_ents_netcdf

  SUBROUTINE out_ents_netcdf(istep, land_snow_lnd)
    USE netcdf
    USE genie_global, ONLY: writing_gui_restarts
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: istep
    REAL, DIMENSION(:,:), INTENT(IN) :: land_snow_lnd

    REAL, DIMENSION(:,:,:), ALLOCATABLE :: var_data
    CHARACTER(LEN=8), DIMENSION(10) :: labels = &
         & (/ 'photo   ', 'respveg ', 'leaf    ', 'respsoil', 'Cveg    ', &
         &    'Csoil   ', 'fv      ', 'tqld1   ', 'tqld2   ', 'snow    ' /)
    INTEGER :: myyear, mymonth, myday, inistep
    CHARACTER(LEN=200) :: fname, label
    LOGICAL :: fexist
    INTEGER :: i, j, kk, var_id, vardim_id, ncid

    inistep = INT(nyear * iniday / yearlen) + istep
    myyear = INT(inistep / nyear)
    mymonth = INT(12 * MOD(inistep, nyear) / nyear)
    myday = INT(yearlen * inistep / nyear - &
         & mymonth * (yearlen / 12) - myyear * yearlen)

    IF (writing_gui_restarts) THEN
       fname = 'gui_restart_ents.nc'
    ELSE IF (MOD(iwstp, nyear) == 0) THEN
       fname = TRIM(outdir_name) // TRIM(out_name) // &
            & '_restart_' // TRIM(ConvertFunc(myyear - 1, 10)) // &
            & '_12_30.nc'
    ELSE
       fname = TRIM(outdir_name) // TRIM(out_name) // &
            & '_restart_' // &
            & TRIM(ConvertFunc(myyear, 10)) // '_' // &
            & TRIM(ConvertFunc(mymonth, 2)) // '_' // &
            & TRIM(ConvertFunc(myday, 2)) // '.nc'
    END IF

    INQUIRE(FILE=fname,EXIST=fexist)
    IF (fexist) THEN
       OPEN(8,FILE=fname,STATUS='old')
       CLOSE(8,STATUS='delete')
    END IF

    DO kk = 1, 10
       ALLOCATE(var_data(1,maxj,maxi))
       label = labels(kk)
       DO j = 1, maxj
          DO i = 1, maxi
             SELECT CASE (kk)
             CASE (1)
                var_data(1,j,i) = photo(i,j)
             CASE (2)
                var_data(1,j,i) = respveg(i,j)
             CASE (3)
                var_data(1,j,i) = leaf(i,j)
             CASE (4)
                var_data(1,j,i) = respsoil(i,j)
             CASE (5)
                var_data(1,j,i) = Cveg(i,j)
             CASE (6)
                var_data(1,j,i) = Csoil(i,j)
             CASE (7)
                var_data(1,j,i) = fv(i,j)
             CASE (8)
                var_data(1,j,i) = tqld(1,i,j)
             CASE (9)
                var_data(1,j,i) = tqld(2,i,j)
             CASE (10)
                var_data(1,j,i) = land_snow_lnd(i,j)
             END SELECT
          END DO
       END DO
       CALL netcdf_ents(fname, var_data, label, myday)
       DEALLOCATE(var_data)
    END DO

    ! Adding final restart value (single)
    CALL check_err(NF90_OPEN(fname, NF90_WRITE, ncid))
    CALL check_err(NF90_REDEF(ncid))
    CALL check_err(NF90_DEF_DIM(ncid, 'pco2ld', 1, vardim_id))
    CALL check_err(NF90_DEF_VAR(ncid, 'pco2ld', &
         & NF90_FLOAT, (/ vardim_id /), var_id))
    CALL check_err(NF90_PUT_ATT(ncid, var_id, 'long_name', 'pco2ld'))
    CALL check_err(NF90_ENDDEF(ncid))
    CALL check_err(NF90_PUT_VAR(ncid, var_id, pco2ld))
    CALL check_err(NF90_CLOSE(ncid))
  END SUBROUTINE out_ents_netcdf

  ! Write output for restarts
  SUBROUTINE out_ents(unit, land_snow_lnd)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: unit
    REAL, DIMENSION(:,:), INTENT(IN) :: land_snow_lnd

    INTEGER :: i, j

    WRITE (unit,*) ((photo(i,j), i = 1, maxi), j = 1, maxj)
    WRITE (unit,*) ((respveg(i,j), i = 1, maxi), j = 1, maxj)
    WRITE (unit,*) ((leaf(i,j), i = 1, maxi), j = 1, maxj)
    WRITE (unit,*) ((respsoil(i,j), i = 1, maxi), j = 1, maxj)

    WRITE (unit,*) ((Cveg(i,j), i = 1, maxi), j = 1, maxj)
    WRITE (unit,*) ((Csoil(i,j), i = 1, maxi), j = 1, maxj)
    WRITE (unit,*) ((fv(i,j), i = 1, maxi), j = 1, maxj)

    WRITE (unit,*) ((tqld(1,i,j), i = 1, maxi), j = 1, maxj)
    WRITE (unit,*) ((tqld(2,i,j), i = 1, maxi), j = 1, maxj)

    WRITE (unit,*) ((land_snow_lnd(i,j), i = 1, maxi), j = 1, maxj)

    WRITE (unit,*) pco2ld
  END SUBROUTINE out_ents

END MODULE ents_data
