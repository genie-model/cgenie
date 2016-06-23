SUBROUTINE getscalari_data_nc(ncid, varname, scalarout,ifail)
  IMPLICIT NONE
  INTEGER, INTENT(IN)          :: ncid      ! netCDF dataset ID
  CHARACTER(LEN=*), INTENT(IN) :: varname   ! name of variable to collect
  INTEGER, INTENT(OUT)    :: scalarout ! the output scalar
  INTEGER, INTENT(OUT)         :: ifail

  INTEGER tmp_array(1)
  call get1di_data_nc(ncid, varname, 1, tmp_array, ifail)
  scalarout = tmp_array(1)
END SUBROUTINE

! Integer version of above code. Any changes in above should also be
! reflected here
SUBROUTINE get1di_data_nc(ncid, varname, dim1, arrayout,ifail)
  USE genie_global, ONLY: write_status
  USE NETCDF
  IMPLICIT NONE
  INTEGER, INTENT(IN)          :: ncid      ! netCDF dataset ID
  CHARACTER(LEN=*), INTENT(IN) :: varname   ! name of variable to collect
  INTEGER, INTENT(IN)          :: dim1      ! Size of array in 1st dimension
  INTEGER, INTENT(OUT)    :: arrayout(dim1) ! the output array
  INTEGER, INTENT(OUT)         :: ifail

  INTEGER :: status, varid, ndims, dim1nc, dimid(1)

  ! Find the variable ID
  ifail = 0
  status = NF90_INQ_VARID(ncid, varname, varid)
  IF (status /= NF90_NOERR) THEN
     WRITE (6, *) 'ERROR: could not find ', varname
     ifail = 1
     RETURN
  END IF

  ! get information on variable
  status = NF90_INQUIRE_VARIABLE(ncid, varid, ndims=ndims)
  IF (status /= NF90_NOERR) THEN
     WRITE (6, *) 'ERROR: could not find dims. of variable'
     CALL write_status('ERRORED')
  END IF
  IF (ndims /= 1) THEN
     WRITE (6, *) 'ERROR: variable has ', ndims, &
          & ' dimensions and we expect 1'
  END IF
  status = NF90_INQUIRE_VARIABLE(ncid, varid, dimids=dimid)
  IF (status /= NF90_NOERR) THEN
     WRITE (6, *) 'ERROR: could not find dimension ID'
     CALL write_status('ERRORED')
  END IF

  ! check that dimensions match
  status = NF90_INQUIRE_DIMENSION(ncid, dimid(1), len=dim1nc)
  IF (status /= NF90_NOERR) THEN
     WRITE (6, *) 'ERROR: Could not get 1st dimension from ' &
          & //'netcdf file'
     CALL write_status('ERRORED')
  END IF
  IF (dim1nc /= dim1) THEN
     WRITE (6, *) 'ERROR: 1st dimension of variable in model ' &
          & //'and netcdf file do not match'
     WRITE (6, *) 'model and netcdf dims are ', dim1, ' and ' &
          & , dim1nc
     CALL write_status('ERRORED')
  END IF

  ! get variable
  status = NF90_GET_VAR(ncid, varid, arrayout)
  IF (status /= NF90_NOERR) THEN
     WRITE (6, *) 'ERROR: getting variable'
     CALL write_status('ERRORED')
  END IF
END SUBROUTINE get1di_data_nc
