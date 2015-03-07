SUBROUTINE check_err(iret)
  USE netcdf
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: iret

  IF (iret /= 0) THEN
     PRINT *, ' error in netcdf ', iret
     PRINT *, NF90_STRERROR(iret)
     STOP 1
  END IF
END SUBROUTINE check_err

FUNCTION loc_dim(text, dimname, nall)
  IMPLICIT NONE
  INTEGER :: loc_dim, nall
  CHARACTER(LEN=200) :: dimname(nall)
  CHARACTER(LEN=*) :: text
  INTEGER :: ilen, lnsig, ifail, i, ilen1

  ilen = lnsig(text)
  ifail = -1
  DO i = 1, nall
     ilen1 = lnsig(dimname(i))
     IF (text(1:ilen) == dimname(i)(1:ilen1)) THEN
        ifail = i
        EXIT
     END IF
  END DO

  IF (ifail <= 0) THEN
     PRINT *, ' Failure to locate dimension name '
     PRINT *, text(1:ilen)
     DO i = 1, nall
        ilen1 = lnsig(dimname(i))
        IF (ilen1 > 0) PRINT *, nall, i, dimname(i)(1:ilen1)
     END DO
     STOP
  END IF
  loc_dim = ifail
END FUNCTION loc_dim
