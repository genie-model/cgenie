SUBROUTINE open_file_nc(filein, ncid)
  USE netcdf
  IMPLICIT NONE

  CHARACTER(LEN=*), INTENT(IN) :: filein    ! Name of file to open
  INTEGER, INTENT(OUT)         :: ncid      ! netCDF dataset ID

  INTEGER :: status
  status = NF90_OPEN(filein, NF90_NOWRITE, ncid)
  IF (status /= NF90_NOERR) then
     WRITE (6, *) 'ERROR: opening ', filein
     STOP
  END IF
END SUBROUTINE open_file_nc
