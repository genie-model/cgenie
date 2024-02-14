
PROGRAM nf90test
  USE netcdf
  IMPLICIT NONE
  INTEGER :: ncido, dimido, varido
  INTEGER :: ncidi, dimidi, varidi
  REAL, DIMENSION(10) :: valso = (/ 1, 4, 9, 16, 25, 36, 49, 64, 81, 100 /)
  INTEGER :: lenchk
  REAL, DIMENSION(10) :: valsi
  CALL chk(NF90_CREATE('tmp.nc', NF90_CLOBBER, ncido))
  CALL chk(NF90_DEF_DIM(ncido, 'x', 10, dimido))
  CALL chk(NF90_DEF_VAR(ncido, 'v', NF90_DOUBLE, (/ dimido /), varido))
  CALL chk(NF90_ENDDEF(ncido))
  CALL chk(NF90_PUT_VAR(ncido, varido, valso))
  CALL chk(NF90_CLOSE(ncido))
  CALL chk(NF90_OPEN('tmp.nc', NF90_NOWRITE, ncidi))
  CALL chk(NF90_INQ_DIMID(ncidi, 'x', dimidi))
  CALL chk(NF90_INQUIRE_DIMENSION(ncidi, dimidi, LEN=lenchk))
  IF (lenchk /= 10) THEN
     PRINT *, 'Dimension length mismatch'
     STOP 1
  END IF
  CALL chk(NF90_INQ_VARID(ncidi, 'v', varidi))
  CALL chk(NF90_GET_VAR(ncidi, varidi, valsi))
  IF (ANY(valsi /= valso)) THEN
     PRINT *, 'Value mismatch'
     STOP 1
  ELSE
    PRINT *, 'OK'
  END IF
CONTAINS
  SUBROUTINE chk(status)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: status
    IF (status /= NF90_NOERR) THEN
       PRINT *, 'NetCDF error: ', NF90_STRERROR(status)
       STOP 1
    END IF
  END SUBROUTINE chk
END PROGRAM nf90test

