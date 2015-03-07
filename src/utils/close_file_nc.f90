SUBROUTINE close_file_nc(filein, ncid)
  USE netcdf
  IMPLICIT NONE

  CHARACTER(LEN=*), INTENT(IN) :: filein    ! Name of file to open
  INTEGER, INTENT(IN)          :: ncid      ! netCDF dataset ID

  INTEGER :: status

  status = NF90_CLOSE(ncid)
  IF (status /= NF90_NOERR) then
     WRITE (6, *) 'ERROR: closing ', filein
     STOP
  END IF
END SUBROUTINE close_file_nc
