!----------------------------------------------------------------------
!>
!> Module: local_netcdf
!>
!> INTERFACE module talking to the NetCDF library
!>
!----------------------------------------------------------------------
MODULE local_netcdf

  USE netcdf
  USE genie_global, ONLY: write_status

  ! lookup information about variables by using its name
  INTERFACE lookupVars
     MODULE PROCEDURE lookupReal1dVars
     MODULE PROCEDURE lookupReal2dVars
     MODULE PROCEDURE lookupReal3dVars
  END INTERFACE

  ! Declaration of user-derived data types.
  ! Attributes for variables
  TYPE basicAttInfo
     CHARACTER(LEN=NF90_MAX_NAME) :: long_name
     CHARACTER(LEN=NF90_MAX_NAME) :: standard_name
     CHARACTER(LEN=NF90_MAX_NAME) :: units
     REAL                         :: missing_value=-9.99E9
  END TYPE basicAttInfo

  TYPE real1dVar
     CHARACTER(LEN=NF90_MAX_NAME)         :: name   ! variable name
     INTEGER                              :: xtype=NF90_FLOAT
     INTEGER                              :: ndims=1 ! num associated dims
     INTEGER,DIMENSION(1)                 :: dimIDs ! IDs of assoc dims
     INTEGER,DIMENSION(1)                 :: dimLens ! lengths of assoc dims
     CHARACTER(LEN=NF90_MAX_NAME),DIMENSION(1) :: dimnames ! names of assoc dims
     INTEGER                              :: nAtts  ! num associated atts
     INTEGER                              :: id     ! id assigned by lib call
     TYPE(basicAttInfo)                   :: basicAtts
     REAL, POINTER, DIMENSION(:)          :: data
  END TYPE REAL1dVar

  type REAL2dVar
     CHARACTER(LEN=NF90_MAX_NAME)         :: name   ! variable name
     INTEGER                              :: xtype=NF90_FLOAT
     INTEGER                              :: ndims=2 ! num associated dims
     INTEGER, DIMENSION(2)                :: dimIDs ! IDs of assoc dims
     INTEGER, DIMENSION(2)                :: dimLens ! lengths of assoc dims
     CHARACTER(LEN=NF90_MAX_NAME),DIMENSION(2) :: dimnames ! names of assoc dims
     INTEGER                              :: nAtts  ! num associated atts
     INTEGER                              :: id     ! id assigned by lib call
     TYPE(basicAttInfo)                   :: basicAtts
     REAL, POINTER, DIMENSION(:,:)        :: data
  END TYPE REAL2dVar

  type REAL3dVar
     CHARACTER(LEN=NF90_MAX_NAME)         :: name   ! variable name
     INTEGER                              :: xtype=NF90_FLOAT
     INTEGER                              :: ndims=3 ! num associated dims
     INTEGER, DIMENSION(3)                :: dimIDs ! IDs of assoc dims
     INTEGER, DIMENSION(3)                :: dimLens ! lengths of assoc dims
     CHARACTER(LEN=NF90_MAX_NAME),DIMENSION(3) :: dimnames ! names of assoc dims
     INTEGER                              :: nAtts  ! num associated atts
     INTEGER                              :: id     ! id assigned by lib call
     TYPE(basicAttInfo)                   :: basicAtts
     REAL, POINTER, DIMENSION(:,:,:)      :: data
  END TYPE REAL3dVar

contains

  !----------------------------------------------------------------------
  !
  ! Subroutine: lookupREAL1dVars
  !
  ! Lookup information about variables (1D,  'flat', REAL-valued)
  ! sets vars(n)%id to -99 if variable not found
  !
  !----------------------------------------------------------------------

  SUBROUTINE lookupREAL1dVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                          :: ncid
    TYPE(REAL1dVar), DIMENSION(:), INTENT(INOUT) :: vars

    INTEGER :: status, ii, jj, xtype

    ! write the values to the dataset
    DO ii = 1, SIZE(vars)
       status = NF90_INQ_VARID(ncid, vars(ii)%name, vars(ii)%id)
       IF (status == NF90_ENOTVAR) THEN
          vars(ii)%id = -99
       ELSE
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          status = NF90_INQUIRE_VARIABLE(ncid, vars(ii)%id, vars(ii)%name, &
               & xtype, vars(ii)%ndims, vars(ii)%dimIDs)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          IF (xtype /= NF90_FLOAT .AND. xtype /= NF90_DOUBLE) &
               & CALL handle_err('Wrong variable type!')
          DO jj = 1, vars(ii)%ndims
             status = NF90_INQUIRE_DIMENSION(ncid, vars(ii)%dimIDs(jj), &
                  & vars(ii)%dimnames(jj), vars(ii)%dimLens(jj))
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          END DO
       END IF
    END DO
  END SUBROUTINE lookupREAL1dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: lookupREAL2dVars
  !
  ! Lookup information about variables (2D, 'flat', REAL-valued)
  ! sets vars(n)%id to -99 if variable not found
  !
  !----------------------------------------------------------------------

  SUBROUTINE lookupREAL2dVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                          :: ncid
    TYPE(REAL2dVar), DIMENSION(:), INTENT(INOUT) :: vars

    INTEGER :: status, ii, jj, xtype

    ! write the values to the dataset
    DO ii = 1, SIZE(vars)
       status = NF90_INQ_VARID(ncid, vars(ii)%name, vars(ii)%id)
       IF (status == NF90_ENOTVAR) THEN
          vars(ii)%id = -99
       ELSE
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          status = NF90_INQUIRE_VARIABLE(ncid, vars(ii)%id, vars(ii)%name, &
               & xtype, vars(ii)%ndims, vars(ii)%dimIDs)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          IF (xtype /= NF90_FLOAT .AND. xtype /= NF90_DOUBLE) &
               & CALL handle_err('Wrong variable type!')
          DO jj = 1, vars(ii)%ndims
             status = NF90_INQUIRE_DIMENSION(ncid, vars(ii)%dimIDs(jj), &
                  & vars(ii)%dimnames(jj), vars(ii)%dimLens(jj))
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          END DO
       END IF
    END DO
  END SUBROUTINE lookupREAL2dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: lookupREAL3dVars
  !
  ! Lookup information about variables (3D, 'flat', REAL-valued)
  ! sets vars(n)%id to -99 if variable not found
  !
  !----------------------------------------------------------------------

  SUBROUTINE lookupREAL3dVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                          :: ncid
    TYPE(REAL3dVar), DIMENSION(:), INTENT(INOUT) :: vars

    INTEGER :: status, ii, jj, xtype

    ! write the values to the dataset
    DO ii = 1, SIZE(vars)
       status = NF90_INQ_VARID(ncid, vars(ii)%name, vars(ii)%id)
       IF (status == NF90_ENOTVAR) THEN
          vars(ii)%id = -99
       ELSE
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          status = NF90_INQUIRE_VARIABLE(ncid, vars(ii)%id, vars(ii)%name, &
               & xtype, vars(ii)%ndims, vars(ii)%dimIDs)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          IF (xtype /= NF90_FLOAT .AND. xtype /= NF90_DOUBLE) &
               & CALL handle_err('Wrong variable type!')
          DO jj = 1, vars(ii)%ndims
             status = NF90_INQUIRE_DIMENSION(ncid, vars(ii)%dimIDs(jj), &
                  & vars(ii)%dimnames(jj), vars(ii)%dimLens(jj))
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          END DO
       END IF
    END DO
  END SUBROUTINE lookupREAL3dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: openNetCDFRead
  !
  ! Opens an existing dataset for reading & returns dataset id
  !
  !----------------------------------------------------------------------

  SUBROUTINE openNetCDFRead(filename, ncid)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: filename
    INTEGER, INTENT(OUT)         :: ncid

    INTEGER :: status

    ! create the dataset
    ! NF90_NOWRITE: open dataset for reading only
    ! NF90_WRITE (write only)
    ! NF90_SHARE (read and write) also available
    status = NF90_OPEN(TRIM(filename), NF90_NOWRITE, ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)
  END SUBROUTINE openNetCDFRead

  !----------------------------------------------------------------------
  !
  ! Subroutine: closeNetCDF
  !
  ! Close an open dataset with given id
  !
  !----------------------------------------------------------------------

  SUBROUTINE closeNetCDF(ncid)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ncid

    INTEGER :: status

    status = NF90_CLOSE(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)
  END SUBROUTINE closeNetCDF

  !----------------------------------------------------------------------
  !
  ! Subroutine: handle_err
  !
  ! simple error handler
  !
  !----------------------------------------------------------------------

  SUBROUTINE handle_err(message)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: message

    WRITE (*,*) TRIM(message)
    CALL write_status('ERRORED')
  END SUBROUTINE handle_err

  !----------------------------------------------------------------------
  !
  ! Subroutine: handle_nc_err
  !
  ! simple error handler
  !
  !----------------------------------------------------------------------

  SUBROUTINE handle_nc_err(status)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: status

    IF (status /= nf90_noerr) THEN
       WRITE (*,*) TRIM(NF90_STRERROR(status))
       CALL write_status('ERRORED')
    END IF
  END SUBROUTINE handle_nc_err

END MODULE local_netcdf
