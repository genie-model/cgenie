SUBROUTINE get2d_data_nc(ncid, varname, dim1, dim2, arrayout,ifail)
  USE genie_global, ONLY: write_status
  USE netcdf
  IMPLICIT NONE
  INTEGER, INTENT(IN)          :: ncid      ! netCDF dataset ID
  CHARACTER(LEN=*), INTENT(IN) :: varname   ! name of variable to collect
  INTEGER, INTENT(IN)          :: dim1, dim2
  REAL, INTENT(OUT) :: arrayout(dim1,dim2) ! the output array
  INTEGER, INTENT(OUT)         :: ifail

  INTEGER :: status, varid, ndims, dimids(2), dim1nc, dim2nc


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
  IF (ndims /= 2) then
     WRITE (6, *) 'ERROR: variable has ', ndims, &
          & ' dimensions and we expect 2'
  END IF
  status = NF90_INQUIRE_VARIABLE(ncid, varid, dimids=dimids)
  IF (status /= NF90_NOERR) THEN
     WRITE (6, *) 'ERROR: could not find dimension IDs'
     CALL write_status('ERRORED')
  END IF

  ! check that dimensions match
  status = NF90_INQUIRE_DIMENSION(ncid, dimids(1), len=dim1nc)
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
  status = NF90_INQUIRE_DIMENSION(ncid, dimids(2), len=dim2nc)
  IF (status /= NF90_NOERR) THEN
     WRITE (6, *) 'ERROR: Could not get 2nd dimension from ' &
          & //'netcdf file'
     stop
  END IF
  IF (dim2nc /= dim2) THEN
     WRITE (6, *) 'ERROR: 2nd dimension of variable in model ' &
          & //'and netcdf file do not match'
     WRITE (6, *) 'model and netcdf dims are ', dim2, ' and ' &
          & , dim2nc
     CALL write_status('ERRORED')
  END IF

  ! get variable
  status = NF90_GET_VAR(ncid, varid, arrayout)
  IF (status /= NF90_NOERR) THEN
     WRITE (6, *) 'ERROR: getting variable'
     CALL write_status('ERRORED')
  END IF
END SUBROUTINE get2d_data_nc
