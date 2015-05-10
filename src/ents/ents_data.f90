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
