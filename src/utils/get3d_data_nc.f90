SUBROUTINE get3d_data_nc(ncid, varname, dim1, dim2, dim3, arrayout, ifail)
  USE netcdf
  IMPLICIT NONE
  INTEGER, INTENT(IN)          :: ncid      ! netCDF dataset ID
  CHARACTER(LEN=*), INTENT(IN) :: varname   ! name of variable to collect
  INTEGER, INTENT(IN)          :: dim1, dim2, dim3
  REAL, INTENT(OUT) :: arrayout(dim1,dim2,dim3) ! the output array
  INTEGER, INTENT(OUT)         :: ifail

  INTEGER :: status, varid, ndims, dimids(3), dim1nc, dim2nc, dim3nc

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
     STOP
  END IF
  IF (ndims /= 3) THEN
     WRITE (6, *) 'ERROR: variable has ', ndims, &
          & ' dimensions and we expect 3'
  END IF
  status = NF90_INQUIRE_VARIABLE(ncid, varid, dimids=dimids)
  IF (status /= NF90_NOERR) THEN
     WRITE (6, *) 'ERROR: could not find dimension IDs'
     STOP
  END IF

  ! check that dimensions match
  status = NF90_INQUIRE_DIMENSION(ncid, dimids(1), len=dim1nc)
  IF (status /= NF90_NOERR) THEN
     WRITE (6, *) 'ERROR: Could not get 1st dimension from ' &
          & //'netcdf file'
     STOP
  END IF
  IF (dim1nc /= dim1) THEN
     WRITE (6, *) 'ERROR: 1st dimension of variable in model ' &
          & //'and netcdf file do not match'
     WRITE (6, *) 'model and netcdf dims are ', dim1, ' and ' &
          & , dim1nc
     stop
  END IF
  status = NF90_INQUIRE_DIMENSION(ncid, dimids(2), len=dim2nc)
  IF (status /= NF90_NOERR) THEN
     WRITE (6, *) 'ERROR: Could not get 2nd dimension from ' &
          & //'netcdf file'
     STOP
  END IF
  IF (dim2nc /= dim2) THEN
     WRITE (6, *) 'ERROR: 2nd dimension of variable in model ' &
          & //'and netcdf file do not match'
     WRITE (6, *) 'model and netcdf dims are ', dim2, ' and ' &
          & , dim2nc
     STOP
  END IF
  status = NF90_INQUIRE_DIMENSION(ncid, dimids(3), len=dim3nc)
  IF (status /= NF90_NOERR) THEN
     WRITE (6, *) 'ERROR: Could not get 3rd dimension from ' &
          & //'netcdf file'
     STOP
  END IF
  if (dim3nc /= dim3) THEN
     WRITE (6, *) 'ERROR: 3rd dimension of variable in model ' &
          & //'and netcdf file do not match'
     WRITE (6, *) 'model and netcdf dims are ', dim3, ' and ' &
          & , dim3nc
     STOP
  END IF

  ! get variable
  status = NF90_GET_VAR(ncid, varid, arrayout)
  IF (status /= NF90_NOERR) THEN
     WRITE (6, *) 'ERROR: getting variable'
     STOP
  END IF
END SUBROUTINE get3d_data_nc
