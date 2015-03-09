MODULE writenc6

  INTERFACE writevar
     MODULE PROCEDURE writevar_1d
     MODULE PROCEDURE writevar_2d
     MODULE PROCEDURE writevar_3d
  END INTERFACE writevar

CONTAINS

  SUBROUTINE ininc(fname, nmaxdims, ndim, nvar, natts, nattsvar, &
       & vdims, vadims, ndims, dimname, varname, &
       & attdimname, attvarname, ncid, iddim, idvar)
    USE netcdf
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: fname
    INTEGER, INTENT(IN) :: nmaxdims, ndim, nvar
    INTEGER, DIMENSION(1:ndim), INTENT(IN) :: ndims, natts
    INTEGER, DIMENSION(1:nvar), INTENT(IN) :: vdims, nattsvar
    INTEGER, DIMENSION(1:nmaxdims, 1:nvar), INTENT(IN) :: vadims
    CHARACTER(LEN=200), DIMENSION(1:ndim), INTENT(IN) :: dimname
    CHARACTER(LEN=200), DIMENSION(1:nvar), INTENT(IN) :: varname
    CHARACTER(LEN=200), DIMENSION(2, 1:nmaxdims, 1:ndim), INTENT(IN) :: &
         & attdimname
    CHARACTER(LEN=200), DIMENSION(2, 1:nmaxdims, 1:nvar), INTENT(IN) :: &
         & attvarname
    INTEGER, INTENT(OUT) :: ncid
    INTEGER, DIMENSION(1:ndim), INTENT(OUT) :: iddim
    INTEGER, DIMENSION(1:nvar), INTENT(OUT) :: idvar

    INTEGER :: iflen, lnsig, ii, iii

    INTEGER :: iret, idebug
    INTEGER :: tempdims(200), dimdim(200)
    REAL :: realval(1)

    idebug = 0
    iflen = lnsig(fname)

    ! Enter define mode
    IF (idebug == 1) THEN
       PRINT *, ' opening file = ', fname(1:iflen)
    END IF
    CALL check_err(NF90_CREATE(fname(1:iflen), NF90_CLOBBER, ncid))

    ! define dimensions
    IF (idebug == 1) PRINT *, ' defining dimensions ', ndim
    DO ii = 1, ndim
       CALL check_err(NF90_DEF_DIM(ncid, TRIM(dimname(ii)), &
            & ndims(ii), dimdim(ii)))
    END DO

    ! define coordinates etc
    IF (idebug == 1) PRINT *, ' defining coordinates ', ndim
    DO ii = 1, ndim
       CALL check_err(NF90_DEF_VAR(ncid, TRIM(dimname(ii)), NF90_REAL, &
            & (/ dimdim(ii) /), iddim(ii)))
    END DO

    ! and the real data
    IF (idebug == 1) PRINT *, ' defining variables ', nvar
    DO ii = 1, nvar
       IF (idebug == 1) &
            & PRINT *, '       variables ', ii, vdims(ii), TRIM(varname(ii))
       DO iii = 1, vdims(ii)
          tempdims(iii) = dimdim(vadims(iii,ii))
          IF (idebug == 1) PRINT *, '       variables ', ii, iii, tempdims(iii)
       END DO
       CALL check_err(NF90_DEF_VAR(ncid, TRIM(varname(ii)), NF90_REAL, &
            & tempdims(1:vdims(ii)), idvar(ii)))
    END DO

    ! assign attributes for coordinates
    IF (idebug == 1) PRINT *, ' Defining attibutes of coordinates ', ndim
    DO ii = 1, ndim
       IF (idebug == 1) PRINT *, ' Number of attributes = ', ii, natts(ii)
       DO iii = 1, natts(ii)
          IF (idebug == 1) THEN
             PRINT *, ii, iii, TRIM(attdimname(1,iii,ii))
             PRINT *, ii, iii, TRIM(attdimname(2,iii,ii))
          END IF
          CALL check_err(NF90_PUT_ATT(ncid, iddim(ii), &
               & TRIM(attdimname(1,iii,ii)), TRIM(attdimname(2,iii,ii))))
          CALL check_err(iret)
       END DO
       realval(1) = -99999.0
       CALL check_err(NF90_PUT_ATT(ncid, iddim(ii), 'missing_value', realval))
    END DO

    ! assign attributes for variables
    IF (idebug == 1) PRINT *, ' Defining attibutes of variables ', nvar
    DO ii = 1, nvar
       IF (idebug == 1) PRINT *, ' Number of attributes = ', ii, nattsvar(ii)
       DO iii = 1, nattsvar(ii)
          IF (idebug == 1) THEN
             PRINT *, ii, iii, TRIM(attvarname(1,iii,ii))
             PRINT *, ii, iii, TRIM(attvarname(2,iii,ii))
          END IF
          CALL check_err(NF90_PUT_ATT(ncid, idvar(ii), &
               & TRIM(attvarname(1,iii,ii)), TRIM(attvarname(2,iii,ii))))
       END DO
       realval(1) = -99999.0
       CALL check_err(NF90_PUT_ATT(ncid, idvar(ii), 'missing_value', realval))
    END DO

    ! global attribute
    IF (idebug == 1) PRINT *, ' Writing global attribute '
    CALL check_err(NF90_PUT_ATT(ncid, NF90_GLOBAL, 'title', &
         & 'Produced using writenc6 program by PJV'))

    ! leave define mode
    CALL check_err(NF90_ENDDEF(ncid))

    IF (idebug == 1) PRINT *, ' Finished ininc '
  END SUBROUTINE ininc


  SUBROUTINE writedim(ncid, iddim, data)
    USE netcdf
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ncid, iddim
    REAL, DIMENSION(:), INTENT(IN) :: data

    ! write coordinates
    CALL check_err(NF90_PUT_VAR(ncid, iddim, data))
  END SUBROUTINE writedim


  SUBROUTINE writevar_1d(ncid, idvar, data)
    USE netcdf
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ncid, idvar
    REAL, DIMENSION(:), INTENT(IN) :: data

    ! write data
    CALL check_err(NF90_PUT_VAR(ncid, idvar, data))
  END SUBROUTINE writevar_1d

  SUBROUTINE writevar_2d(ncid, idvar, data)
    USE netcdf
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ncid, idvar
    REAL, DIMENSION(:,:), INTENT(IN) :: data

    ! write data
    CALL check_err(NF90_PUT_VAR(ncid, idvar, data))
  END SUBROUTINE writevar_2d

  SUBROUTINE writevar_3d(ncid, idvar, data)
    USE netcdf
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ncid, idvar
    REAL, DIMENSION(:,:,:), INTENT(IN) :: data

    ! write data
    CALL check_err(NF90_PUT_VAR(ncid, idvar, data))
  END SUBROUTINE writevar_3d


  ! close file
  SUBROUTINE closenc(ncid)
    USE netcdf
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ncid

    CALL check_err(NF90_CLOSE(ncid))
  END SUBROUTINE closenc

END MODULE writenc6
