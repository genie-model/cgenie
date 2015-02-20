SUBROUTINE ininc(fname, nmaxdims, ndim, nvar, natts, nattsvar, &
     & vdims, vadims, ndims, dimname, varname, &
     & attdimname, attvarname, ncid, iddim, idvar)
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: fname
  INTEGER, INTENT(IN) :: nmaxdims, ndim, nvar
  INTEGER, DIMENSION(1:ndim), INTENT(IN) :: ndims, natts
  INTEGER, DIMENSION(1:nvar), INTENT(IN) :: vdims, nattsvar
  INTEGER, DIMENSION(1:nmaxdims, 1:nvar), INTENT(IN) :: vadims
  CHARACTER(LEN=200), DIMENSION(1:ndim), INTENT(IN) :: dimname
  CHARACTER(LEN=200), DIMENSION(1:nvar), INTENT(IN) :: varname
  CHARACTER(LEN=200), DIMENSION(2, 1:nmaxdims, 1:ndim), INTENT(IN) :: attdimname
  CHARACTER(LEN=200), DIMENSION(2, 1:nmaxdims, 1:nvar), INTENT(IN) :: attvarname
  INTEGER, INTENT(OUT) :: ncid
  INTEGER, DIMENSION(1:ndim), INTENT(OUT) :: iddim
  INTEGER, DIMENSION(1:nvar), INTENT(OUT) :: idvar

  INCLUDE 'netcdf.inc'
  INTEGER :: iflen, lnsig, ii, itlen, iii, itlen1, itlen2

  INTEGER :: iret, idebug
  INTEGER :: tempdims(200), dimdim(200)
  CHARACTER(LEN=200) :: tname, tname1, tname2
  REAL :: realval(1)
  INTEGER :: realkind

  realkind = KIND(realval)
  idebug = 0
  iflen = lnsig(fname)

  ! Enter define mode
  IF (idebug == 1) THEN
     PRINT *, ' opening file = ', fname(1:iflen)
  END IF
  iret = NF_CREATE(fname(1:iflen), NF_CLOBBER, ncid)
  CALL check_err(iret)

  ! define dimensions
  IF (idebug == 1) PRINT *, ' defining dimensions ', ndim
  DO ii = 1, ndim
     tname = dimname(ii)
     itlen = lnsig(tname)
     iret = NF_DEF_DIM(ncid, tname(1:itlen), ndims(ii), dimdim(ii))
  END DO

  ! define coordinates etc
  IF (idebug == 1) PRINT *, ' defining coordinates ', ndim
  DO ii = 1, ndim
     tname = dimname(ii)
     itlen = lnsig(tname)
     tempdims(1) = dimdim(ii)
     iret = NF_DEF_VAR(ncid, tname(1:itlen), NF_REAL, 1, tempdims, iddim(ii))
     CALL check_err(iret)
  END DO

  ! and the real data
  IF (idebug == 1) PRINT *, ' defining variables ', nvar
  DO ii = 1, nvar
     tname = varname(ii)
     itlen = lnsig(tname)
     IF (idebug == 1) &
          & PRINT *, '       variables ', ii, vdims(ii), tname(1:itlen)
     DO iii = 1, vdims(ii)
        tempdims(iii) = dimdim(vadims(iii,ii))
        IF (idebug == 1) PRINT *, '       variables ', ii, iii, tempdims(iii)
     END DO
     iret = NF_DEF_VAR(ncid,tname(1:itlen), NF_REAL, vdims(ii), &
          & tempdims,idvar(ii))
     CALL check_err(iret)
  END DO

  ! assign attributes for coordinates
  IF (idebug == 1) PRINT *, ' Defining attibutes of coordinates ', ndim
  DO ii = 1, ndim
     IF (idebug == 1) PRINT *, ' Number of attributes = ', ii, natts(ii)
     DO iii = 1, natts(ii)
        tname1 = attdimname(1,iii,ii)
        tname2 = attdimname(2,iii,ii)
        itlen1 = lnsig(tname1)
        itlen2 = lnsig(tname2)
        IF (idebug == 1) THEN
           PRINT *, ii, iii, tname1(1:itlen1)
           PRINT *, ii, iii, tname2(1:itlen2)
        END IF
        iret = NF_PUT_ATT_TEXT(ncid, iddim(ii), &
             & tname1(1:itlen1), itlen2, tname2(1:itlen2))
        CALL check_err(iret)
     END DO
     realval(1) = -99999.0
     IF (realkind == 4) THEN
        iret = NF_PUT_ATT_REAL(ncid,iddim(ii), 'missing_value', &
             & NF_REAL, 1, realval)
     ELSE IF (realkind == 8) THEN
        iret = NF_PUT_ATT_DOUBLE(ncid,iddim(ii), 'missing_value', &
             & NF_REAL, 1, realval)
     ELSE
        PRINT *, 'precision problem in writenc6'
        STOP
     END IF
     CALL check_err(iret)
  END DO

  ! assign attributes for variables
  IF (idebug == 1) PRINT *, ' Defining attibutes of variables ', nvar
  DO ii = 1, nvar
     IF (idebug == 1) PRINT *, ' Number of attributes = ', ii, nattsvar(ii)
     DO iii = 1, nattsvar(ii)
        tname1 = attvarname(1,iii,ii)
        tname2 = attvarname(2,iii,ii)
        itlen1 = lnsig(tname1)
        itlen2 = lnsig(tname2)
        IF (idebug == 1) THEN
           PRINT *, ii, iii, tname1(1:itlen1)
           PRINT *, ii, iii, tname2(1:itlen2)
        END IF
        iret = NF_PUT_ATT_TEXT(ncid,idvar(ii), &
             & tname1(1:itlen1),itlen2, tname2(1:itlen2))
        CALL check_err(iret)
     END DO
     realval(1) = -99999.0
     IF (realkind == 4) THEN
        iret = NF_PUT_ATT_REAL(ncid,idvar(ii), 'missing_value', &
             & NF_REAL, 1, realval)
     ELSE IF (realkind == 8) THEN
        iret = NF_PUT_ATT_DOUBLE(ncid,idvar(ii), 'missing_value', &
             & NF_REAL, 1, realval)
     ELSE
        PRINT *, 'precision problem in writenc6'
        STOP
     END IF
     CALL check_err(iret)
  END DO

  ! global attribute
  IF (idebug == 1) PRINT *, ' Writing global attribute '
  tname = 'Produced using writenc6 program by PJV'
  itlen = lnsig(tname)
  iret = NF_PUT_ATT_TEXT(ncid, NF_GLOBAL,'title', itlen,tname(1:itlen))
  CALL check_err(iret)

  ! leave define mode
  iret = NF_ENDDEF(ncid)
  CALL check_err(iret)

  IF (idebug == 1) PRINT *, ' Finished ininc '
END SUBROUTINE ininc


SUBROUTINE writedim(ncid, iddim, data)
  IMPLICIT NONE
  INCLUDE 'netcdf.inc'
  INTEGER, INTENT(IN) :: ncid, iddim
  REAL, INTENT(IN) :: data(*)

  INTEGER :: iret, realkind

  realkind = KIND(data(1))

  ! write coordinates
  IF (realkind == 4) THEN
     iret = NF_PUT_VAR_REAL(ncid, iddim, data)
  ELSE IF (realkind == 8) THEN
     iret = NF_PUT_VAR_DOUBLE(ncid, iddim, data)
  ELSE
     PRINT *, 'precision problem in writedim'
     STOP
  END IF
  CALL check_err(iret)
END SUBROUTINE writedim


SUBROUTINE writevar(ncid, idvar, data)
  IMPLICIT NONE
  INCLUDE 'netcdf.inc'
  INTEGER, INTENT(IN) :: ncid, idvar
  REAL, INTENT(IN) :: data(*)

  INTEGER :: iret, realkind

  ! write data
  realkind = KIND(data(1))
  IF (realkind == 4) THEN
     iret = NF_PUT_VAR_REAL(ncid, idvar, data)
  ELSE IF (realkind == 8) THEN
     iret = NF_PUT_VAR_DOUBLE(ncid, idvar, data)
  ELSE
     PRINT *, 'precision problem in writevar'
     STOP
  END IF
  CALL check_err(iret)
END SUBROUTINE writevar


SUBROUTINE writevar2(ncid, idvar, data, ix1, ix2, iy1, iy2, iz1, iz2, it1, it2)
  IMPLICIT NONE
  INCLUDE 'netcdf.inc'
  INTEGER, INTENT(IN) :: ncid, idvar
  INTEGER, INTENT(IN) :: ix1, ix2, iy1, iy2, iz1, iz2, it1, it2
  REAL, INTENT(IN) :: data(*)

  INTEGER, DIMENSION(4) :: start, count
  INTEGER :: iret, realkind

  ! write data
  realkind = KIND(data(1))
  start(1) = ix1
  start(2) = iy1
  start(3) = iz1
  IF (it1 > 0) start(4) = it1
  start(4) = it1
  count(1) = ix2 - ix1 + 1
  count(2) = iy2 - iy1 + 1
  count(3) = iz2 - iz1 + 1
  IF (it1 > 0) count(4) = it2 - it1 + 1

  IF (realkind == 4) THEN
     iret = NF_PUT_VARA_REAL(ncid, idvar, start,count, data)
  ELSE IF (realkind == 8) THEN
     iret = NF_PUT_VARA_DOUBLE(ncid, idvar, start, count, data)
  ELSE
     PRINT *, 'precision problem in writevar2'
  END IF

  CALL check_err(iret)
END SUBROUTINE writevar2


! close file
SUBROUTINE closenc(ncid)
  IMPLICIT NONE
  INCLUDE 'netcdf.inc'
  INTEGER, INTENT(IN) :: ncid

  INTEGER :: iret

  iret = NF_CLOSE(ncid)
  CALL check_err(iret)
END SUBROUTINE closenc
