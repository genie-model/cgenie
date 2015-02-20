!----------------------------------------------------------------------
!>
!> Module: local_netcdf
!>
!> INTERFACE module talking to the NetCDF library
!>
!----------------------------------------------------------------------
MODULE local_netcdf

  USE netcdf

  INTERFACE defineDims
     MODULE PROCEDURE defineNocoordDims
     MODULE PROCEDURE defineRealDims
     MODULE PROCEDURE defineIntegerDims
     MODULE PROCEDURE defineRealRecordDims
  END INTERFACE

  INTERFACE defineVars
     MODULE PROCEDURE defineReal1dVars
     MODULE PROCEDURE defineReal2dVars
     MODULE PROCEDURE defineReal3dVars
     MODULE PROCEDURE defineInteger1dVars
     MODULE PROCEDURE defineInteger2dVars
     MODULE PROCEDURE defineInteger3dVars
     MODULE PROCEDURE defineReal1dRecordVars
     MODULE PROCEDURE defineReal2dRecordVars
     MODULE PROCEDURE defineReal3dRecordVars
     MODULE PROCEDURE defineInteger1dRecordVars
     MODULE PROCEDURE defineInteger2dRecordVars
     MODULE PROCEDURE defineInteger3dRecordVars
  END INTERFACE

  INTERFACE dimVars
     MODULE PROCEDURE dimReal1dVars
     MODULE PROCEDURE dimReal2dVars
     MODULE PROCEDURE dimReal3dVars
     MODULE PROCEDURE dimInteger1dVars
     MODULE PROCEDURE dimInteger2dVars
     MODULE PROCEDURE dimInteger3dVars
     MODULE PROCEDURE dimReal1dRecordVars
     MODULE PROCEDURE dimReal2dRecordVars
     MODULE PROCEDURE dimReal3dRecordVars
     MODULE PROCEDURE dimInteger1dRecordVars
     MODULE PROCEDURE dimInteger2dRecordVars
     MODULE PROCEDURE dimInteger3dRecordVars
  END INTERFACE

  INTERFACE writeVars
     MODULE PROCEDURE writeReal1dVars
     MODULE PROCEDURE writeReal2dVars
     MODULE PROCEDURE writeReal3dVars
     MODULE PROCEDURE writeInteger1dVars
     MODULE PROCEDURE writeInteger2dVars
     MODULE PROCEDURE writeInteger3dVars
  END INTERFACE

  ! lookup information about variables by using its name
  INTERFACE lookupVars
     MODULE PROCEDURE lookupReal1dVars
     MODULE PROCEDURE lookupReal2dVars
     MODULE PROCEDURE lookupReal3dVars
     MODULE PROCEDURE lookupInteger1dVars
     MODULE PROCEDURE lookupInteger2dVars
     MODULE PROCEDURE lookupInteger3dVars
     MODULE PROCEDURE lookupReal1dRecordVars
     MODULE PROCEDURE lookupReal2dRecordVars
     MODULE PROCEDURE lookupReal3dRecordVars
     MODULE PROCEDURE lookupInteger1dRecordVars
     MODULE PROCEDURE lookupInteger2dRecordVars
     MODULE PROCEDURE lookupInteger3dRecordVars
  END INTERFACE

  ! append variables as new entries at the end of record dimensions
  ! (if there is one)
  INTERFACE appendVars
     MODULE PROCEDURE appendReal1dVars
     MODULE PROCEDURE appendReal2dVars
     MODULE PROCEDURE appendReal3dVars
     MODULE PROCEDURE appendInteger1dVars
     MODULE PROCEDURE appendInteger2dVars
     MODULE PROCEDURE appendInteger3dVars
  END INTERFACE

  ! Declaration of user-derived data types.
  ! Attributes for variables
  TYPE basicAttInfo
     CHARACTER(LEN=NF90_MAX_NAME) :: long_name
     CHARACTER(LEN=NF90_MAX_NAME) :: standard_name
     CHARACTER(LEN=NF90_MAX_NAME) :: units
     REAL                         :: missing_value=-9.99E9
  END TYPE basicAttInfo

  TYPE dimInfo
     CHARACTER(LEN=NF90_MAX_NAME) :: name ! dimension name
     INTEGER                      :: len  ! length of dimension
     INTEGER                      :: id   ! id assigned by lib call
  END TYPE dimInfo

  TYPE realDimInfo
     CHARACTER(LEN=NF90_MAX_NAME) :: name
     INTEGER :: len
     INTEGER :: id
     ! Define coordinates?
     LOGICAL :: coordsDefine=.TRUE.
     ! Define boundaries? only rectangular grids supported
     LOGICAL :: boundsDefine=.FALSE.
     ! Attributes for the coordinates
     TYPE(basicAttInfo) :: basicAtts
     ! Coordinates
     REAL, POINTER, DIMENSION(:) :: coords
     ! Lower and upper boundaries of the coordinates
     REAL, POINTER, DIMENSION(:) :: boundsLower
     REAL, POINTER, DIMENSION(:) :: boundsUpper
  END TYPE realDimInfo

  TYPE integerDimInfo
     CHARACTER(LEN=NF90_MAX_NAME) :: name
     INTEGER                      :: len
     INTEGER                      :: id
     ! Define coordinates?
     LOGICAL :: coordsDefine=.TRUE.
     ! Define boundaries? only rectangular grids supported
     LOGICAL :: boundsDefine=.FALSE.
     ! Attributes for the coordinates
     TYPE(basicAttInfo) :: basicAtts
     ! Coordinates
     INTEGER, POINTER, DIMENSION(:) :: coords
     ! Lower and upper boundaries of the coordinates
     INTEGER, POINTER, DIMENSION(:) :: boundsLower
     INTEGER, POINTER, DIMENSION(:) :: boundsUpper
  END TYPE integerDimInfo

  TYPE recordDimInfo
     CHARACTER(LEN=NF90_MAX_NAME) :: name ! dimension name
     INTEGER :: len=NF90_UNLIMITED       ! unlimited length => record dimensions
     INTEGER :: id                       ! id assigned by lib call
  END TYPE recordDimInfo

  TYPE realRecordDimInfo
     CHARACTER(LEN=NF90_MAX_NAME) :: name ! dimension name
     INTEGER :: len=NF90_UNLIMITED       ! unlimited length => record dimensions
     INTEGER :: id                       ! id assigned by lib call
     ! Define coordinates?
     LOGICAL :: coordsDefine=.TRUE.
     ! Define boundaries? only rectangular grids supported
     LOGICAL :: boundsDefine=.FALSE.
     ! Attributes for the coordinates
     TYPE(basicAttInfo) :: basicAtts
  END TYPE realRecordDimInfo

  TYPE integerRecordDimInfo
     CHARACTER(LEN=NF90_MAX_NAME) :: name ! dimension name
     INTEGER :: len=NF90_UNLIMITED       ! unlimited length => record dimensions
     INTEGER :: id                       ! id assigned by lib call
     ! Define coordinates?
     LOGICAL :: coordsDefine=.TRUE.
     ! Define boundaries? only rectangular grids supported
     LOGICAL :: boundsDefine=.FALSE.
     ! Attributes for the coordinates
     TYPE(basicAttInfo) :: basicAtts
  END TYPE integerRecordDimInfo

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

  type INTEGER1dVar
     CHARACTER(LEN=NF90_MAX_NAME)         :: name   ! variable name
     INTEGER                              :: xtype=NF90_INT
     INTEGER                              :: ndims=1 ! num associated dims
     INTEGER, DIMENSION(1)                :: dimIDs ! IDs of assoc dims
     INTEGER, DIMENSION(1)                :: dimLens ! lengths of assoc dims
     CHARACTER(LEN=NF90_MAX_NAME),DIMENSION(1) :: dimnames ! names of assoc dims
     INTEGER                              :: nAtts  ! num associated atts
     INTEGER                              :: id     ! id assigned by lib call
     TYPE(basicAttInfo)                   :: basicAtts
     INTEGER, POINTER, DIMENSION(:)       :: data
  END TYPE INTEGER1dVar

  type INTEGER2dVar
     CHARACTER(LEN=NF90_MAX_NAME)         :: name   ! variable name
     INTEGER                              :: xtype=NF90_INT
     INTEGER                              :: ndims=2 ! num associated dims
     INTEGER, DIMENSION(2)                :: dimIDs ! IDs of assoc dims
     INTEGER, DIMENSION(2)                :: dimLens ! lengths of assoc dims
     CHARACTER(LEN=NF90_MAX_NAME),DIMENSION(2) :: dimnames ! names of assoc dims
     INTEGER                              :: nAtts  ! num associated atts
     INTEGER                              :: id     ! id assigned by lib call
     TYPE(basicAttInfo)                   :: basicAtts
     INTEGER, POINTER, DIMENSION(:,:)     :: data
  END TYPE INTEGER2dVar

  type INTEGER3dVar
     CHARACTER(LEN=NF90_MAX_NAME)         :: name   ! variable name
     INTEGER                              :: xtype=NF90_INT
     INTEGER                              :: ndims=3 ! num associated dims
     INTEGER, DIMENSION(3)                :: dimIDs ! IDs of assoc dims
     INTEGER, DIMENSION(3)                :: dimLens ! lengths of assoc dims
     CHARACTER(LEN=NF90_MAX_NAME),DIMENSION(3) :: dimnames ! names of assoc dims
     INTEGER                              :: nAtts  ! num associated atts
     INTEGER                              :: id     ! id assigned by lib call
     TYPE(basicAttInfo)                   :: basicAtts
     INTEGER, POINTER, DIMENSION(:,:,:)   :: data
  END TYPE INTEGER3dVar

  type REAL1dRecordVar
     CHARACTER(LEN=NF90_MAX_NAME)         :: name   ! variable name
     INTEGER                              :: xtype=NF90_FLOAT
     INTEGER                              :: ndims=2 ! num associated dims
     INTEGER, DIMENSION(2)                :: dimIDs ! IDs of assoc dims
     INTEGER, DIMENSION(2)                :: dimLens ! lengths of assoc dims
     CHARACTER(LEN=NF90_MAX_NAME),DIMENSION(2) :: dimnames ! names of assoc dims
     ! Index to record dimension for arrays dimIDs, dimLens
     INTEGER                              :: recordDimIndex=0
     INTEGER                              :: nAtts  ! num associated atts
     INTEGER                              :: id     ! id assigned by lib call
     TYPE(basicAttInfo)                   :: basicAtts
     REAL, POINTER, DIMENSION(:)          :: data
  END TYPE REAL1dRecordVar

  type REAL2dRecordVar
     CHARACTER(LEN=NF90_MAX_NAME)         :: name   ! variable name
     INTEGER                              :: xtype=NF90_FLOAT
     INTEGER                              :: ndims=3 ! num associated dims
     INTEGER, DIMENSION(3)                :: dimIDs ! IDs of assoc dims
     INTEGER, DIMENSION(3)                :: dimLens ! lengths of assoc dims
     CHARACTER(LEN=NF90_MAX_NAME),DIMENSION(3) :: dimnames ! names of assoc dims
     ! Index to record dimension for arrays dimIDs, dimLens
     INTEGER                              :: recordDimIndex=0
     INTEGER                              :: nAtts  ! num associated atts
     INTEGER                              :: id     ! id assigned by lib call
     TYPE(basicAttInfo)                   :: basicAtts
     REAL, POINTER, DIMENSION(:,:)        :: data
  END TYPE REAL2dRecordVar

  type REAL3dRecordVar
     CHARACTER(LEN=NF90_MAX_NAME)         :: name   ! variable name
     INTEGER                              :: xtype=NF90_FLOAT
     INTEGER                              :: ndims=4 ! num associated dims
     INTEGER, DIMENSION(4)                :: dimIDs ! IDs of assoc dims
     INTEGER, DIMENSION(4)                :: dimLens ! lengths of assoc dims
     CHARACTER(LEN=NF90_MAX_NAME),DIMENSION(4) :: dimnames ! names of assoc dims
     ! Index to record dimension for arrays dimIDs, dimLens
     INTEGER                              :: recordDimIndex=0
     INTEGER                              :: nAtts  ! num associated atts
     INTEGER                              :: id     ! id assigned by lib call
     TYPE(basicAttInfo)                   :: basicAtts
     REAL, POINTER, DIMENSION(:,:,:)      :: data
  END TYPE REAL3dRecordVar

  type INTEGER1dRecordVar
     CHARACTER(LEN=NF90_MAX_NAME)         :: name   ! variable name
     INTEGER                              :: xtype=NF90_INT
     INTEGER                              :: ndims=2 ! num associated dims
     INTEGER, DIMENSION(2)                :: dimIDs ! IDs of assoc dims
     INTEGER, DIMENSION(2)                :: dimLens ! lengths of assoc dims
     CHARACTER(LEN=NF90_MAX_NAME),DIMENSION(2) :: dimnames ! names of assoc dims
     ! Index to record dimension for arrays dimIDs, dimLens
     INTEGER                              :: recordDimIndex=0
     INTEGER                              :: nAtts  ! num associated atts
     INTEGER                              :: id     ! id assigned by lib call
     TYPE(basicAttInfo)                   :: basicAtts
     REAL, POINTER, DIMENSION(:)          :: data
  END TYPE INTEGER1dRecordVar

  type INTEGER2dRecordVar
     CHARACTER(LEN=NF90_MAX_NAME)         :: name   ! variable name
     INTEGER                              :: xtype=NF90_INT
     INTEGER                              :: ndims=3 ! num associated dims
     INTEGER, DIMENSION(3)                :: dimIDs ! IDs of assoc dims
     INTEGER, DIMENSION(3)                :: dimLens ! lengths of assoc dims
     CHARACTER(LEN=NF90_MAX_NAME),DIMENSION(3) :: dimnames ! names of assoc dims
     ! Index to record dimension for arrays dimIDs, dimLens
     INTEGER                              :: recordDimIndex=0
     INTEGER                              :: nAtts  ! num associated atts
     INTEGER                              :: id     ! id assigned by lib call
     TYPE(basicAttInfo)                   :: basicAtts
     REAL, POINTER, DIMENSION(:,:)        :: data
  END TYPE INTEGER2dRecordVar

  type INTEGER3dRecordVar
     CHARACTER(LEN=NF90_MAX_NAME)         :: name   ! variable name
     INTEGER                              :: xtype=NF90_INT
     INTEGER                              :: ndims=4 ! num associated dims
     INTEGER, DIMENSION(4)                :: dimIDs ! IDs of assoc dims
     INTEGER, DIMENSION(4)                :: dimLens ! lengths of assoc dims
     CHARACTER(LEN=NF90_MAX_NAME),DIMENSION(4) :: dimnames ! names of assoc dims
     ! Index to record dimension for arrays dimIDs, dimLens
     INTEGER                              :: recordDimIndex=0
     INTEGER                              :: nAtts  ! num associated atts
     INTEGER                              :: id     ! id assigned by lib call
     TYPE(basicAttInfo)                   :: basicAtts
     REAL, POINTER, DIMENSION(:,:,:)      :: data
  END TYPE INTEGER3dRecordVar

contains

  !----------------------------------------------------------------------
  !
  ! Subroutine: createNetCDF
  ! Opens a new dataset & returns dataset id
  !
  !----------------------------------------------------------------------

  SUBROUTINE createNetCDF(filename, ncid, clobber)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: filename
    INTEGER, INTENT(OUT)         :: ncid
    LOGICAL, OPTIONAL            :: clobber

    INTEGER :: status

    ! create the dataset
    ! NF90_NOCLOBBER: do not overwrite an existing dataset with same filename
    ! NF90_CLOBBER also available
    IF (PRESENT(clobber) .AND. (clobber)) THEN
       status = NF90_CREATE(TRIM(filename), NF90_CLOBBER, ncid)
    ELSE
       status = NF90_CREATE(TRIM(filename), NF90_NOCLOBBER, ncid)
    END IF
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)
    ! write global attributes
    status = NF90_PUT_ATT(ncid, NF90_GLOBAL, "title", &
         & "Grid ENabled Integrated Earth system modelling " // &
         & "(GENIE) model output")
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)
    status = NF90_PUT_ATT(ncid,NF90_GLOBAL,"institution", "__INSTITUTION__")
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)
    status = NF90_PUT_ATT(ncid, NF90_GLOBAL, "source", &
         & "Grid ENabled Integrated Earth system modelling " // &
         & "(GENIE) framework version: __VERSION__")
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)
    status = NF90_PUT_ATT(ncid, NF90_GLOBAL, "history", &
         & "__TIMESTAMP__: Creation of dataset")
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)
    status = NF90_PUT_ATT(ncid, NF90_GLOBAL, "references", &
         & "http://www.genie.ac.uk")
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)
    status = NF90_PUT_ATT(ncid,NF90_GLOBAL,"comment", "")
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)
    status = NF90_PUT_ATT(ncid,NF90_GLOBAL,"Conventions", "CF-1.4")
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)
  END SUBROUTINE createNetCDF

  !-----------------------------------------------------------------------
  !>
  !> Subroutine: defineNocoordDims
  !>
  !> Defines dimensions without coordinates in an opened dataset
  !>
  !-----------------------------------------------------------------------  !

  SUBROUTINE defineNocoordDims(ncid, dims)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                        :: ncid
    TYPE(DimInfo), INTENT(INOUT), DIMENSION(:) :: dims

    INTEGER :: status, ii

    status = NF90_REDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    DO ii = 1,size(dims)
       ! defined dimensions
       ! dims(ii)%id is given a value by the nf90_def_dim function
       status = NF90_DEF_DIM(ncid, dims(ii)%name, dims(ii)%len, dims(ii)%id)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
    END DO

    status = NF90_ENDDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)
  END SUBROUTINE defineNocoordDims

  !-----------------------------------------------------------------------
  !>
  !> Subroutine: defineREALDims
  !>
  !> Defines dimensions with REAL-valued coordinates in an opened dataset
  !>
  !-----------------------------------------------------------------------

  SUBROUTINE defineREALDims(ncid, dims)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                            :: ncid
    TYPE(REALDimInfo), INTENT(INOUT), DIMENSION(:) :: dims

    INTEGER :: status, varid, dimidBoundaries, varidBoundaries, ii

    status = NF90_REDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    DO ii = 1, SIZE(dims)
       ! defined dimensions
       ! dims(ii)%id is given a value by the NF90_DEF_DIM function
       status = NF90_DEF_DIM(ncid, dims(ii)%name, dims(ii)%len, dims(ii)%id)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       IF (dims(ii)%coordsDefine) THEN
          ! define and write coordinates as 1-dimensional variables
          status = NF90_DEF_VAR(ncid, dims(ii)%name, &
               & NF90_FLOAT, dims(ii)%id, varid)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          ! Add appropriate attributes to the coordinate entry
          status = NF90_PUT_ATT(ncid, varid, "units", &
               & dims(ii)%basicAtts%units)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          status = NF90_PUT_ATT(ncid, varid, "long_name", &
               & dims(ii)%basicAtts%long_name)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          status = NF90_PUT_ATT(ncid, varid, "standard_name", &
               dims(ii)%basicAtts%standard_name)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)

          ! define and write coordinate boundaries if requested
          IF (dims(ii)%boundsDefine) THEN
             ! unless already there, create dimension referring to the
             ! upper and lower boundaries, respectively
             ! no associated coordinates needed
             status = NF90_INQ_DIMID(ncid, "nv", dimidBoundaries)
             ! if non-existing, try to create the dimension
             IF (status == NF90_EBADDIM) THEN
                status = NF90_DEF_DIM(ncid, "nv", 2, dimidBoundaries)
                IF (status /= NF90_NOERR) CALL handle_nc_err(status)
             END IF
             ! write boundary coordinates to dataset
             ! no attributes required, CF-compliance implies
             ! attributes from the coordinate variable to be used for
             ! the boundaries as well
             status = NF90_DEF_VAR(ncid, TRIM(dims(ii)%name)//"_bnds", &
                  & NF90_FLOAT, (/ dimidBoundaries, dims(ii)%id /), &
                  & varidBoundaries)
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
             ! add attribute to coordinate variable to reference to
             ! boundaries variable
             status = NF90_PUT_ATT(ncid,varid,"bounds", &
                  & TRIM(dims(ii)%name)//"_bnds")
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          END IF
       END IF
    END DO

    status = NF90_ENDDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    DO ii = 1, SIZE(dims)
       IF (dims(ii)%coordsDefine) THEN
          ! write the coordinates to the dataset
          status = NF90_PUT_VAR(ncid, varid, dims(ii)%coords)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)

          ! define and write coordinate boundaries if requested
          IF (dims(ii)%boundsDefine) THEN
             status = NF90_PUT_VAR(ncid, varidBoundaries, &
                  & dims(ii)%boundsLower, &
                  & (/ 1, 1 /), (/ 1, dims(ii)%len, 1 /))
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
             status = NF90_PUT_VAR(ncid, varidBoundaries, &
                  & dims(ii)%boundsUpper, (/ 2, 1 /), (/ 1, dims(ii)%len /))
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          END IF
       END IF
    END DO
  END SUBROUTINE defineREALDims

  !-----------------------------------------------------------------------
  !>
  !> Subroutine: defineINTEGERDims
  !>
  !> Defines dimensions with INTEGER-valued coordinates in an opened
  !> dataset
  !>
  !-----------------------------------------------------------------------

  SUBROUTINE defineINTEGERDims(ncid, dims)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                               :: ncid
    TYPE(INTEGERDimInfo), INTENT(INOUT), DIMENSION(:) :: dims

    INTEGER :: status, varid, dimidBoundaries, varidBoundaries, ii

    status = NF90_REDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    DO ii = 1, SIZE(dims)
       ! defined dimensions
       ! dims(ii)%id is given a value by the NF90_DEF_DIM function
       status = NF90_DEF_DIM(ncid, dims(ii)%name, dims(ii)%len, dims(ii)%id)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)

       IF (dims(ii)%coordsDefine) THEN
          ! define and write coordinates as 1-dimensional variables
          status = NF90_DEF_VAR(ncid, dims(ii)%name, &
               & NF90_FLOAT, dims(ii)%id,varid)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          ! Add appropriate attributes to the coordinate entry
          status = NF90_PUT_ATT(ncid, varid, "units", &
               & dims(ii)%basicAtts%units)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          status = NF90_PUT_ATT(ncid, varid, "long_name", &
               & dims(ii)%basicAtts%long_name)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          status = NF90_PUT_ATT(ncid, varid, "standard_name", &
               & dims(ii)%basicAtts%standard_name)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)

          ! define and write coordinate boundaries if requested
          IF (dims(ii)%boundsDefine) THEN
             ! unless already there, create dimension referring to the
             ! upper and lower boundaries, respectively
             ! no associated coordinates needed
             status = NF90_INQ_DIMID(ncid, "nv", dimidBoundaries)
             ! if non-existing, try to create the dimension
             IF (status == NF90_EBADDIM) THEN
                status = NF90_DEF_DIM(ncid, "nv", 2, dimidBoundaries)
                IF (status /= NF90_NOERR) CALL handle_nc_err(status)
             END IF
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
             ! write boundary coordinates to dataset
             ! no attributes required, CF-compliance implies
             ! attributes from the coordinate variable to be used for
             ! the boundaries as well
             status = NF90_DEF_VAR(ncid, TRIM(dims(ii)%name)//"_bnds", &
                  & NF90_FLOAT, (/ dimidBoundaries,  dims(ii)%id /), &
                  & varidBoundaries)
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
             ! add attribute to coordinate variable to reference to
             ! boundaries variable
             status = NF90_PUT_ATT(ncid, varid, "bounds", &
                  & TRIM(dims(ii)%name)//"_bnds")
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          END IF
       END IF
    END DO

    status = NF90_ENDDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    DO ii = 1, SIZE(dims)
       IF (dims(ii)%coordsDefine) THEN
          ! write the coordinates to the dataset
          status = NF90_PUT_VAR(ncid, varid, dims(ii)%coords)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)

          ! define and write coordinate boundaries if requested
          IF (dims(ii)%boundsDefine) THEN
             status = NF90_PUT_VAR(ncid, varidBoundaries, &
                  & dims(ii)%boundsLower, (/ 1, 1 /), (/ 1, dims(ii)%len, 1 /))
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
             status = NF90_PUT_VAR(ncid, varidBoundaries, &
                  & dims(ii)%boundsUpper, (/ 2, 1 /), (/ 1, dims(ii)%len /))
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          END IF
       END IF
    END DO
  END SUBROUTINE defineINTEGERDims

  !-----------------------------------------------------------------------
  !>
  !> Subroutine: defineREALRecordDims
  !>
  !> Defines dimensions with INTEGER-valued coordinates in an opened
  !> dataset
  !>
  !-----------------------------------------------------------------------

  SUBROUTINE defineREALRecordDims(ncid,dims)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                                  :: ncid
    TYPE(REALRecordDimInfo), INTENT(INOUT), DIMENSION(:) :: dims

    INTEGER :: status, varid, dimidBoundaries, varidBoundaries, ii

    status = NF90_REDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    DO ii = 1, SIZE(dims)
       ! defined dimensions
       ! dims(ii)%id is given a value by the NF90_DEF_DIM function
       status = NF90_DEF_DIM(ncid, dims(ii)%name, dims(ii)%len, dims(ii)%id)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)

       IF (dims(ii)%coordsDefine) THEN
          ! define and write coordinates as 1-dimensional variables
          status = NF90_DEF_VAR(ncid, dims(ii)%name, &
               & NF90_FLOAT,  dims(ii)%id, varid)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          ! Add appropriate attributes to the coordinate entry
          status = NF90_PUT_ATT(ncid, varid, "units", &
               & dims(ii)%basicAtts%units)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          status = NF90_PUT_ATT(ncid, varid, "long_name", &
               & dims(ii)%basicAtts%long_name)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          status = NF90_PUT_ATT(ncid, varid, "standard_name", &
               & dims(ii)%basicAtts%standard_name)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)

          ! define and write coordinate boundaries if requested
          IF (dims(ii)%boundsDefine) THEN
             ! unless already there, create dimension referring to the
             ! upper and lower boundaries, respectively
             ! no associated coordinates needed
             status = NF90_INQ_DIMID(ncid, "nv", dimidBoundaries)
             ! if non-existing, try to create the dimension
             IF (status == NF90_EBADDIM) THEN
                status = NF90_DEF_DIM(ncid, "nv", 2, dimidBoundaries)
                IF (status /= NF90_NOERR) CALL handle_nc_err(status)
             END IF
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
             ! write boundary coordinates to dataset
             ! no attributes required, CF-compliance implies
             ! attributes from the coordinate variable to be used for
             ! the boundaries as well
             status = NF90_DEF_VAR(ncid, TRIM(dims(ii)%name)//"_bnds", &
                  & NF90_FLOAT, (/ dimidBoundaries, dims(ii)%id /), &
                  & varidBoundaries)
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
             ! add attribute to coordinate variable to reference to
             ! boundaries variable
             status = NF90_PUT_ATT(ncid, varid, "bounds", &
                  & TRIM(dims(ii)%name)//"_bnds")
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          END IF
       END IF
    END DO

    status = NF90_ENDDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)
  END SUBROUTINE defineREALRecordDims

  !-----------------------------------------------------------------------
  !>
  !> Subroutine: defineREAL1dVars
  !>
  !> Defines a set of 'flat' variables (1D, REAL valued) in a open dataset
  !>
  !-----------------------------------------------------------------------

  SUBROUTINE defineREAL1dVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                          :: ncid
    TYPE(REAL1dVar), INTENT(INOUT), DIMENSION(:) :: vars

    INTEGER :: ii, status

    status = NF90_REDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    DO ii = 1, SIZE(vars)
       ! NF90_DEF_VAR sets value for vars(ii)%id
       status = NF90_DEF_VAR(ncid, vars(ii)%name, &
            & vars(ii)%xtype, vars(ii)%dimIDs, vars(ii)%id)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       CALL addVarAttributes(ncid, vars(ii)%id, vars(ii)%basicAtts)
    END DO

    status = NF90_ENDDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    CALL lookupVars(ncid, vars)
  END SUBROUTINE defineREAL1dVars


  !----------------------------------------------------------------------
  !
  ! Subroutine: defineREAL2dVars
  !
  ! Defines a set of 'flat' variables (2D, REAL valued) in a open dataset
  !
  !----------------------------------------------------------------------

  SUBROUTINE defineREAL2dVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                          :: ncid
    TYPE(REAL2dVar), INTENT(INOUT), DIMENSION(:) :: vars

    INTEGER :: ii, status

    status = NF90_REDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    DO ii = 1, SIZE(vars)
       ! NF90_DEF_VAR sets value for vars(ii)%id
       status = NF90_DEF_VAR(ncid, vars(ii)%name, &
            & vars(ii)%xtype, vars(ii)%dimIDs, vars(ii)%id)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       CALL addVarAttributes(ncid, vars(ii)%id, vars(ii)%basicAtts)
    END DO

    status = NF90_ENDDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    CALL lookupVars(ncid, vars)
  END SUBROUTINE defineREAL2dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: defineREAL3dVars
  !
  ! Defines a set of 'flat' variables (3D, REAL valued) in a open dataset
  !
  !----------------------------------------------------------------------

  SUBROUTINE defineREAL3dVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                          :: ncid
    TYPE(REAL3dVar), INTENT(INOUT), DIMENSION(:) :: vars

    INTEGER :: ii, status

    status = NF90_REDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    DO ii=1, SIZE(vars)
       ! NF90_DEF_VAR sets value for vars(ii)%id
       status = NF90_DEF_VAR(ncid, vars(ii)%name, &
            & vars(ii)%xtype, vars(ii)%dimIDs, vars(ii)%id)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       CALL addVarAttributes(ncid, vars(ii)%id, vars(ii)%basicAtts)
    END DO

    status = NF90_ENDDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    CALL lookupVars(ncid, vars)
  END SUBROUTINE defineREAL3dVars

  !-----------------------------------------------------------------------
  !>
  !> Subroutine: defineINTEGER1dVars
  !>
  !> Defines a set of 'flat' variables (1D, INTEGER valued) in a open dataset
  !>
  !-----------------------------------------------------------------------

  SUBROUTINE defineINTEGER1dVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                             :: ncid
    TYPE(INTEGER1dVar), INTENT(INOUT), DIMENSION(:) :: vars

    INTEGER :: ii, status

    status = NF90_REDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    DO ii = 1, SIZE(vars)
       ! NF90_DEF_VAR sets value for vars(ii)%id
       status = NF90_DEF_VAR(ncid, vars(ii)%name, &
            & vars(ii)%xtype, vars(ii)%dimIDs, vars(ii)%id)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       CALL addVarAttributes(ncid, vars(ii)%id, vars(ii)%basicAtts)
    END DO

    status = NF90_ENDDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    CALL lookupVars(ncid, vars)
  END SUBROUTINE defineINTEGER1dVars


  !----------------------------------------------------------------------
  !
  ! Subroutine: defineINTEGER2dVars
  !
  ! Defines a set of 'flat' variables (2D, INTEGER valued) in a open dataset
  !
  !----------------------------------------------------------------------

  SUBROUTINE defineINTEGER2dVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                             :: ncid
    TYPE(INTEGER2dVar), INTENT(INOUT), DIMENSION(:) :: vars

    INTEGER :: ii, status

    status = NF90_REDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    DO ii = 1, SIZE(vars)
       ! NF90_DEF_VAR sets value for vars(ii)%id
       status = NF90_DEF_VAR(ncid, vars(ii)%name, &
            & vars(ii)%xtype, vars(ii)%dimIDs, vars(ii)%id)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       CALL addVarAttributes(ncid, vars(ii)%id, vars(ii)%basicAtts)
    END DO

    status = NF90_ENDDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    CALL lookupVars(ncid, vars)
  END SUBROUTINE defineINTEGER2dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: defineINTEGER3dVars
  !
  ! Defines a set of 'flat' variables (3D, INTEGER valued) in a open dataset
  !
  !----------------------------------------------------------------------

  SUBROUTINE defineINTEGER3dVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                             :: ncid
    TYPE(INTEGER3dVar), INTENT(INOUT), DIMENSION(:) :: vars

    INTEGER :: ii, status

    status = NF90_REDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    DO ii = 1, SIZE(vars)
       ! NF90_DEF_VAR sets value for vars(ii)%id
       status = NF90_DEF_VAR(ncid, vars(ii)%name, &
            & vars(ii)%xtype, vars(ii)%dimIDs, vars(ii)%id)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       CALL addVarAttributes(ncid, vars(ii)%id, vars(ii)%basicAtts)
    END DO

    status = NF90_ENDDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    CALL lookupVars(ncid, vars)
  END SUBROUTINE defineINTEGER3dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: defineREAL1dRecordVars
  !
  ! Defines a set of 'flat' variables (1D, REAL valued) in a open dataset
  !
  !----------------------------------------------------------------------

  SUBROUTINE defineREAL1dRecordVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                                :: ncid
    TYPE(REAL1dRecordVar), INTENT(INOUT), DIMENSION(:) :: vars

    INTEGER :: ii, status

    status = NF90_REDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    DO ii = 1, SIZE(vars)
       ! NF90_DEF_VAR sets value for vars(ii)%id
       status = NF90_DEF_VAR(ncid, vars(ii)%name, &
            & vars(ii)%xtype,  vars(ii)%dimIDs, vars(ii)%id)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       CALL addVarAttributes(ncid, vars(ii)%id, vars(ii)%basicAtts)
    END DO

    status = NF90_ENDDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    CALL lookupVars(ncid, vars)
  END SUBROUTINE defineREAL1dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: defineREAL2dRecordVars
  !
  ! Defines a set of 'flat' variables (2D, REAL valued) in a open dataset
  !
  !----------------------------------------------------------------------

  SUBROUTINE defineREAL2dRecordVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                                :: ncid
    TYPE(REAL2dRecordVar), INTENT(INOUT), DIMENSION(:) :: vars

    INTEGER :: ii, status

    status = NF90_REDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    DO ii = 1, SIZE(vars)
       ! NF90_DEF_VAR sets value for vars(ii)%id
       status = NF90_DEF_VAR(ncid, vars(ii)%name, &
            & vars(ii)%xtype, vars(ii)%dimIDs, vars(ii)%id)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       CALL addVarAttributes(ncid, vars(ii)%id, vars(ii)%basicAtts)
    END DO

    status = NF90_ENDDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    CALL lookupVars(ncid, vars)
  END SUBROUTINE defineREAL2dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: defineREAL3dRecordVars
  !
  ! Defines a set of 'flat' variables (3D, REAL valued) in a open dataset
  !
  !----------------------------------------------------------------------

  SUBROUTINE defineREAL3dRecordVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                                :: ncid
    TYPE(REAL3dRecordVar), INTENT(INOUT), DIMENSION(:) :: vars

    INTEGER :: ii, status

    status = NF90_REDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    DO ii = 1, SIZE(vars)
       ! NF90_DEF_VAR sets value for vars(ii)%id
       status = NF90_DEF_VAR(ncid, vars(ii)%name, &
            & vars(ii)%xtype, vars(ii)%dimIDs, vars(ii)%id)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       CALL addVarAttributes(ncid, vars(ii)%id, vars(ii)%basicAtts)
    END DO

    status = NF90_ENDDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    CALL lookupVars(ncid, vars)
  END SUBROUTINE defineREAL3dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: defineINTEGER1dRecordVars
  !
  ! Defines a set of 'flat' variables (1D, REAL valued) in a open dataset
  !
  !----------------------------------------------------------------------

  SUBROUTINE defineINTEGER1dRecordVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                                   :: ncid
    TYPE(INTEGER1dRecordVar), INTENT(INOUT), DIMENSION(:) :: vars

    INTEGER :: ii, status

    status = NF90_REDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    DO ii = 1, SIZE(vars)
       ! NF90_DEF_VAR sets value for vars(ii)%id
       status = NF90_DEF_VAR(ncid, vars(ii)%name, &
            & vars(ii)%xtype, vars(ii)%dimIDs, vars(ii)%id)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       CALL addVarAttributes(ncid, vars(ii)%id, vars(ii)%basicAtts)
    END DO

    status = NF90_ENDDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    CALL lookupVars(ncid, vars)
  END SUBROUTINE defineINTEGER1dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: defineINTEGER2dRecordVars
  !
  ! Defines a set of 'flat' variables (2D, REAL valued) in a open dataset
  !
  !----------------------------------------------------------------------

  SUBROUTINE defineINTEGER2dRecordVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                                   :: ncid
    TYPE(INTEGER2dRecordVar), INTENT(INOUT), DIMENSION(:) :: vars

    INTEGER :: ii, status

    status = NF90_REDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    DO ii = 1, SIZE(vars)
       ! NF90_DEF_VAR sets value for vars(ii)%id
       status = NF90_DEF_VAR(ncid, vars(ii)%name, &
            & vars(ii)%xtype, vars(ii)%dimIDs, vars(ii)%id)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       CALL addVarAttributes(ncid, vars(ii)%id, vars(ii)%basicAtts)
    END DO

    status = NF90_ENDDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    CALL lookupVars(ncid, vars)
  END SUBROUTINE defineINTEGER2dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: defineINTEGER3dRecordVars
  !
  ! Defines a set of 'flat' variables (3D, REAL valued) in a open dataset
  !
  !----------------------------------------------------------------------

  SUBROUTINE defineINTEGER3dRecordVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                                   :: ncid
    TYPE(INTEGER3dRecordVar), INTENT(INOUT), DIMENSION(:) :: vars

    INTEGER :: ii, status

    status = NF90_REDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    DO ii = 1, SIZE(vars)
       ! NF90_DEF_VAR sets value for vars(ii)%id
       status = NF90_DEF_VAR(ncid, vars(ii)%name, &
            & vars(ii)%xtype, vars(ii)%dimIDs, vars(ii)%id)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       CALL addVarAttributes(ncid, vars(ii)%id, vars(ii)%basicAtts)
    END DO

    status = NF90_ENDDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)

    CALL lookupVars(ncid, vars)
  END SUBROUTINE defineINTEGER3dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: addVarAttributes
  !
  ! Internal subroutine, common functionality for all define*Vars
  ! functions
  !
  !----------------------------------------------------------------------

  SUBROUTINE addVarAttributes(ncid, varid, basicAtts)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ncid, varid
    TYPE(basicAttInfo), INTENT(IN) :: basicAtts

    INTEGER :: status

    ! Write any appropriate attributes to the dataset
    ! e.g. units here
    status = NF90_PUT_ATT(ncid, varid, "units", basicAtts%units)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)
    status = NF90_PUT_ATT(ncid, varid, "long_name", basicAtts%long_name)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)
    status = NF90_PUT_ATT(ncid, varid, "standard_name", basicAtts%standard_name)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)
    status = NF90_PUT_ATT(ncid, varid, "missing_value", basicAtts%missing_value)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)
  END SUBROUTINE addVarAttributes

  !----------------------------------------------------------------------
  !
  ! Subroutine: dimREAL1dVars
  !
  !
  !
  !----------------------------------------------------------------------

  SUBROUTINE dimREAL1dVars(ncid, vars, dimnames)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                          :: ncid
    TYPE(REAL1dVar), INTENT(INOUT), DIMENSION(:) :: vars
    CHARACTER(LEN=*), INTENT(IN), DIMENSION(1)   :: dimnames

    INTEGER :: ii, dimid, status

    DO ii = 1, SIZE(vars)
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(1)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(1) = dimid
    END DO
  END SUBROUTINE dimREAL1dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: dimREAL2dVars
  !
  !
  !
  !----------------------------------------------------------------------

  SUBROUTINE dimREAL2dVars(ncid, vars, dimnames)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                          :: ncid
    TYPE(REAL2dVar), INTENT(INOUT), DIMENSION(:) :: vars
    CHARACTER(LEN=*), INTENT(IN), DIMENSION(2)   :: dimnames

    INTEGER :: ii, dimid, status

    DO ii = 1, SIZE(vars)
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(1)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(1) = dimid
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(2)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(2) = dimid
    END DO
  END SUBROUTINE dimREAL2dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: dimREAL3dVars
  !
  !
  !
  !----------------------------------------------------------------------

  SUBROUTINE dimREAL3dVars(ncid, vars, dimnames)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                          :: ncid
    TYPE(REAL3dVar), INTENT(INOUT), DIMENSION(:) :: vars
    CHARACTER(LEN=*), INTENT(IN), DIMENSION(3)   :: dimnames

    INTEGER :: ii, dimid, status

    DO ii = 1, SIZE(vars)
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(1)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(1) = dimid
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(2)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(2) = dimid
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(3)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(3) = dimid
    END DO
  END SUBROUTINE dimREAL3dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: dimINTEGER1dVars
  !
  !
  !
  !----------------------------------------------------------------------

  SUBROUTINE dimINTEGER1dVars(ncid, vars, dimnames)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                             :: ncid
    TYPE(INTEGER1dVar), INTENT(INOUT), DIMENSION(:) :: vars
    CHARACTER(LEN=*), INTENT(IN), DIMENSION(1)      :: dimnames

    INTEGER :: ii, dimid, status

    DO ii = 1, SIZE(vars)
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(1)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(1) = dimid
    END DO
  END SUBROUTINE dimINTEGER1dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: dimINTEGER2dVars
  !
  !
  !
  !----------------------------------------------------------------------

  SUBROUTINE dimINTEGER2dVars(ncid, vars, dimnames)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                             :: ncid
    TYPE(INTEGER2dVar), INTENT(INOUT), DIMENSION(:) :: vars
    CHARACTER(LEN=*), INTENT(IN), DIMENSION(2)      :: dimnames

    INTEGER :: ii, dimid, status

    DO ii = 1, SIZE(vars)
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(1)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(1) = dimid
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(2)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(2) = dimid
    END DO
  END SUBROUTINE dimINTEGER2dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: dimINTEGER3dVars
  !
  !
  !
  !----------------------------------------------------------------------

  SUBROUTINE dimINTEGER3dVars(ncid, vars, dimnames)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                             :: ncid
    TYPE(INTEGER3dVar), INTENT(INOUT), DIMENSION(:) :: vars
    CHARACTER(LEN=*), INTENT(IN), DIMENSION(3)      :: dimnames

    INTEGER :: ii, dimid, status

    DO ii = 1, SIZE(vars)
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(1)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(1) = dimid
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(2)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(2) = dimid
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(3)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(3) = dimid
    END DO
  END SUBROUTINE dimINTEGER3dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: dimREAL1dRecordVars
  !
  !
  !
  !----------------------------------------------------------------------

  SUBROUTINE dimREAL1dRecordVars(ncid, vars, dimnames)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                                :: ncid
    TYPE(REAL1dRecordVar), INTENT(INOUT), DIMENSION(:) :: vars
    CHARACTER(LEN=*), INTENT(IN), DIMENSION(2)         :: dimnames

    INTEGER :: ii, dimid, status

    DO ii = 1, SIZE(vars)
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(1)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(1) = dimid
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(2)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(2) = dimid
    END DO
  END SUBROUTINE dimREAL1dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: dimREAL2dRecordVars
  !
  !
  !
  !----------------------------------------------------------------------

  SUBROUTINE dimREAL2dRecordVars(ncid, vars, dimnames)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                                :: ncid
    TYPE(REAL2dRecordVar), INTENT(INOUT), DIMENSION(:) :: vars
    CHARACTER(LEN=*), INTENT(IN), DIMENSION(3)         :: dimnames

    INTEGER :: ii, dimid, status

    DO ii = 1, SIZE(vars)
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(1)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(1) = dimid
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(2)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(2) = dimid
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(3)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(3) = dimid
    END DO
  END SUBROUTINE dimREAL2dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: dimREAL3dRecordVars
  !
  !
  !
  !----------------------------------------------------------------------

  SUBROUTINE dimREAL3dRecordVars(ncid, vars, dimnames)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                                :: ncid
    TYPE(REAL3dRecordVar), INTENT(INOUT), DIMENSION(:) :: vars
    CHARACTER(LEN=*), INTENT(IN), DIMENSION(4)         :: dimnames

    INTEGER :: ii, dimid, status

    DO ii = 1, SIZE(vars)
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(1)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(1) = dimid
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(2)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(2) = dimid
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(3)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(3) = dimid
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(4)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(4) = dimid
    END DO
  END SUBROUTINE dimREAL3dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: dimINTEGER1dRecordVars
  !
  !
  !
  !----------------------------------------------------------------------

  SUBROUTINE dimINTEGER1dRecordVars(ncid, vars, dimnames)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                                   :: ncid
    TYPE(INTEGER1dRecordVar), INTENT(INOUT), DIMENSION(:) :: vars
    CHARACTER(LEN=*), INTENT(IN), DIMENSION(2)            :: dimnames

    INTEGER :: ii, dimid, status

    DO ii = 1, SIZE(vars)
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(1)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(1) = dimid
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(2)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(2) = dimid
    END DO
  END SUBROUTINE dimINTEGER1dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: dimINTEGER2dRecordVars
  !
  !
  !
  !----------------------------------------------------------------------

  SUBROUTINE dimINTEGER2dRecordVars(ncid, vars, dimnames)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                                   :: ncid
    TYPE(INTEGER2dRecordVar), INTENT(INOUT), DIMENSION(:) :: vars
    CHARACTER(LEN=*), INTENT(IN), DIMENSION(3)            :: dimnames

    INTEGER :: ii, dimid, status

    DO ii = 1, SIZE(vars)
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(1)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(1) = dimid
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(2)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(2) = dimid
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(3)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(3) = dimid
    END DO
  END SUBROUTINE dimINTEGER2dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: dimINTEGER3dRecordVars
  !
  !
  !
  !----------------------------------------------------------------------

  SUBROUTINE dimINTEGER3dRecordVars(ncid, vars, dimnames)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                                   :: ncid
    TYPE(INTEGER3dRecordVar), INTENT(INOUT), DIMENSION(:) :: vars
    CHARACTER(LEN=*), INTENT(IN), DIMENSION(4)            :: dimnames

    INTEGER :: ii, dimid, status

    DO ii = 1, SIZE(vars)
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(1)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(1) = dimid
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(2)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(2) = dimid
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(3)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(3) = dimid
       status = NF90_INQ_DIMID(ncid, TRIM(dimnames(4)), dimid)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       vars(ii)%dimIDs(4) = dimid
    END DO
  END SUBROUTINE dimINTEGER3dRecordVars

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
  ! Subroutine: lookupINTEGER1dVars
  !
  ! Lookup information about variables (1D, 'flat', INTEGER-valued)
  ! sets vars(n)%id to -99 if variable not found
  !
  !----------------------------------------------------------------------

  SUBROUTINE lookupINTEGER1dVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                             :: ncid
    TYPE(INTEGER1dVar), DIMENSION(:), INTENT(INOUT) :: vars

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
          IF (xtype /= NF90_INT .AND. xtype /= NF90_SHORT) &
               & CALL handle_err('Wrong variable type!')
          DO jj = 1, vars(ii)%ndims
             status = NF90_INQUIRE_DIMENSION(ncid, vars(ii)%dimIDs(jj), &
                  & vars(ii)%dimnames(jj), vars(ii)%dimLens(jj))
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          END DO
       END IF
    END DO
  END SUBROUTINE lookupINTEGER1dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: lookupINTEGER2dVars
  !
  ! Lookup information about variables (2D, 'flat', INTEGER-valued)
  ! sets vars(n)%id to -99 if variable not found
  !
  !----------------------------------------------------------------------

  SUBROUTINE lookupINTEGER2dVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                             :: ncid
    TYPE(INTEGER2dVar), DIMENSION(:), INTENT(INOUT) :: vars

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
          IF (xtype /= NF90_INT .AND. xtype /= NF90_SHORT) &
               & CALL handle_err('Wrong variable type!')
          DO jj = 1, vars(ii)%ndims
             status = NF90_INQUIRE_DIMENSION(ncid, vars(ii)%dimIDs(jj), &
                  & vars(ii)%dimnames(jj), vars(ii)%dimLens(jj))
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          END DO
       END IF
    END DO
  END SUBROUTINE lookupINTEGER2dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: lookupINTEGER3dVars
  !
  ! Lookup information about variables (3D, 'flat', INTEGER-valued)
  ! sets vars(n)%id to -99 if variable not found
  !
  !----------------------------------------------------------------------

  SUBROUTINE lookupINTEGER3dVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                             :: ncid
    TYPE(INTEGER3dVar), DIMENSION(:), INTENT(INOUT) :: vars

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
          IF (xtype /= NF90_INT .AND. xtype /= NF90_SHORT) &
               & CALL handle_err('Wrong variable type!')
          DO jj = 1, vars(ii)%ndims
             status = NF90_INQUIRE_DIMENSION(ncid, vars(ii)%dimIDs(jj), &
                  & vars(ii)%dimnames(jj), vars(ii)%dimLens(jj))
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          END DO
       END IF
    END DO
  END SUBROUTINE lookupINTEGER3dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: lookupREAL1dRecordVars
  !
  ! Lookup information about variables (2D, 'flat', REAL-valued)
  ! sets vars(n)%id to -99 if variable not found
  !
  !----------------------------------------------------------------------

  SUBROUTINE lookupREAL1dRecordVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                                :: ncid
    TYPE(REAL1dRecordVar), DIMENSION(:), INTENT(INOUT) :: vars

    INTEGER :: status, ii, jj, recdimid, xtype

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
          status = NF90_INQUIRE(ncid, unlimitedDimId=recdimid)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          IF (recdimid == -1) CALL handle_err('No record dimension found!')
          vars(ii)%recordDimIndex = 0
          DO jj = 1, vars(ii)%ndims
             status = NF90_INQUIRE_DIMENSION(ncid, vars(ii)%dimIDs(jj), &
                  & vars(ii)%dimnames(jj), vars(ii)%dimLens(jj))
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
             IF (vars(ii)%dimIDs(jj) == recdimid) THEN
                vars(ii)%recordDimIndex = jj
             END IF
          END DO
       END IF
    END DO
  END SUBROUTINE lookupREAL1dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: lookupREAL2dRecordVars
  !
  ! Lookup information about variables (2D, 'flat', REAL-valued)
  ! sets vars(n)%id to -99 if variable not found
  !
  !----------------------------------------------------------------------

  SUBROUTINE lookupREAL2dRecordVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                                :: ncid
    TYPE(REAL2dRecordVar), DIMENSION(:), INTENT(INOUT) :: vars

    INTEGER :: status, ii, jj, recdimid, xtype

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
          status = NF90_INQUIRE(ncid, unlimitedDimId=recdimid)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          IF (recdimid == -1) CALL handle_err('No record dimension found!')
          vars(ii)%recordDimIndex = 0
          DO jj = 1, vars(ii)%ndims
             status = NF90_INQUIRE_DIMENSION(ncid, vars(ii)%dimIDs(jj), &
                  & vars(ii)%dimnames(jj), vars(ii)%dimLens(jj))
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
             IF (vars(ii)%dimIDs(jj) == recdimid) THEN
                vars(ii)%recordDimIndex = jj
             END IF
          END DO
       END IF
    END DO
  END SUBROUTINE lookupREAL2dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: lookupREAL3dRecordVars
  !
  ! Lookup information about variables (2D, 'flat', REAL-valued)
  ! sets vars(n)%id to -99 if variable not found
  !
  !----------------------------------------------------------------------

  SUBROUTINE lookupREAL3dRecordVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                                :: ncid
    TYPE(REAL3dRecordVar), DIMENSION(:), INTENT(INOUT) :: vars

    INTEGER :: status, ii, jj, recdimid, xtype

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
          status = NF90_INQUIRE(ncid, unlimitedDimId=recdimid)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          IF (recdimid == -1) CALL handle_err('No record dimension found!')
          vars(ii)%recordDimIndex = 0
          DO jj = 1, vars(ii)%ndims
             status = NF90_INQUIRE_DIMENSION(ncid, vars(ii)%dimIDs(jj), &
                  & vars(ii)%dimnames(jj), vars(ii)%dimLens(jj))
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
             IF (vars(ii)%dimIDs(jj) == recdimid) THEN
                vars(ii)%recordDimIndex = jj
             END IF
          END DO
       END IF
    END DO
  END SUBROUTINE lookupREAL3dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: lookupINTEGER1dRecordVars
  !
  ! Lookup information about variables (2D, 'flat', REAL-valued)
  ! sets vars(n)%id to -99 if variable not found
  !
  !----------------------------------------------------------------------

  SUBROUTINE lookupINTEGER1dRecordVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                                  :: ncid
    TYPE(INTEGER1dRecordVar), DIMENSION(:), INTENT(INOUT) :: vars

    INTEGER :: status, ii, jj, recdimid, xtype

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
          IF (xtype /= NF90_INT .AND. xtype /= NF90_SHORT) &
               & CALL handle_err('Wrong variable type!')
          status = NF90_INQUIRE(ncid, unlimitedDimId=recdimid)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          IF (recdimid == -1) CALL handle_err('No record dimension found!')
          vars(ii)%recordDimIndex = 0
          DO jj = 1, vars(ii)%ndims
             status = NF90_INQUIRE_DIMENSION(ncid, vars(ii)%dimIDs(jj), &
                  & vars(ii)%dimnames(jj), vars(ii)%dimLens(jj))
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
             IF (vars(ii)%dimIDs(jj) == recdimid) THEN
                vars(ii)%recordDimIndex = jj
             END IF
          END DO
       END IF
    END DO
  END SUBROUTINE lookupINTEGER1dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: lookupINTEGER2dRecordVars
  !
  ! Lookup information about variables (2D, 'flat', REAL-valued)
  ! sets vars(n)%id to -99 if variable not found
  !
  !----------------------------------------------------------------------

  SUBROUTINE lookupINTEGER2dRecordVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                                   :: ncid
    TYPE(INTEGER2dRecordVar), DIMENSION(:), INTENT(INOUT) :: vars

    INTEGER :: status, ii, jj, recdimid, xtype

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
          IF (xtype /= NF90_INT .AND. xtype /= NF90_SHORT) &
               & CALL handle_err('Wrong variable type!')
          status = NF90_INQUIRE(ncid, unlimitedDimId=recdimid)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          IF (recdimid == -1) CALL handle_err('No record dimension found!')
          vars(ii)%recordDimIndex = 0
          DO jj = 1, vars(ii)%ndims
             status = NF90_INQUIRE_DIMENSION(ncid, vars(ii)%dimIDs(jj), &
                  & vars(ii)%dimnames(jj), vars(ii)%dimLens(jj))
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
             IF (vars(ii)%dimIDs(jj) == recdimid) THEN
                vars(ii)%recordDimIndex = jj
             END IF
          END DO
       END IF
    END DO
  END SUBROUTINE lookupINTEGER2dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: lookupINTEGER3dRecordVars
  !
  ! Lookup information about variables (2D, 'flat', REAL-valued)
  ! sets vars(n)%id to -99 if variable not found
  !
  !----------------------------------------------------------------------

  SUBROUTINE lookupINTEGER3dRecordVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                                   :: ncid
    TYPE(INTEGER3dRecordVar), DIMENSION(:), INTENT(INOUT) :: vars

    INTEGER :: status, ii, jj, recdimid, xtype

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
          IF (xtype /= NF90_INT .AND. xtype /= NF90_SHORT) &
               & CALL handle_err('Wrong variable type!')
          status = NF90_INQUIRE(ncid, unlimitedDimId=recdimid)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          IF (recdimid == -1) CALL handle_err('No record dimension found!')
          vars(ii)%recordDimIndex = 0
          DO jj = 1, vars(ii)%ndims
             status = NF90_INQUIRE_DIMENSION(ncid, vars(ii)%dimIDs(jj), &
                  & vars(ii)%dimnames(jj), vars(ii)%dimLens(jj))
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
             IF (vars(ii)%dimIDs(jj) == recdimid) then
                vars(ii)%recordDimIndex = jj
             END IF
          END DO
       END IF
    END DO
  END SUBROUTINE lookupINTEGER3dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: writeREAL1dVars
  !
  ! Write set of variables (1D, 'flat', REAL-valued) to a setup dataset
  !
  !----------------------------------------------------------------------

  SUBROUTINE writeREAL1dVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                       :: ncid
    TYPE(REAL1dVar), DIMENSION(:), INTENT(IN) :: vars

    INTEGER :: status, ii

    ! write the values to the dataset
    DO ii = 1, SIZE(vars)
       status = NF90_PUT_VAR(ncid, vars(ii)%id, vars(ii)%data)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
    END DO
  END SUBROUTINE writeREAL1dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: writeREAL2dVars
  !
  ! Write set of variables (2D, 'flat', REAL-valued) to a setup dataset
  !
  !----------------------------------------------------------------------

  SUBROUTINE writeREAL2dVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                       :: ncid
    TYPE(REAL2dVar), DIMENSION(:), INTENT(IN) :: vars

    INTEGER :: status, ii

    ! write the values to the dataset
    DO ii = 1, SIZE(vars)
       status = NF90_PUT_VAR(ncid, vars(ii)%id, vars(ii)%data)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
    END DO
  END SUBROUTINE writeREAL2dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: writeREAL3dVars
  !
  ! Write set of variables (3D, 'flat', REAL-valued) to a setup dataset
  !
  !----------------------------------------------------------------------

  SUBROUTINE writeREAL3dVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                       :: ncid
    TYPE(REAL3dVar), DIMENSION(:), INTENT(IN) :: vars

    INTEGER :: status, ii

    ! write the values to the dataset
    DO ii = 1, SIZE(vars)
       status = NF90_PUT_VAR(ncid, vars(ii)%id, vars(ii)%data)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
    END DO
  END SUBROUTINE writeREAL3dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: writeINTEGER1dVars
  !
  ! Write set of variables (1D, 'flat', INTEGER-valued) to a setup dataset
  !
  !----------------------------------------------------------------------

  SUBROUTINE writeINTEGER1dVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                          :: ncid
    TYPE(INTEGER1dVar), DIMENSION(:), INTENT(IN) :: vars

    INTEGER :: status, ii

    ! write the values to the dataset
    DO ii = 1, SIZE(vars)
       status = NF90_PUT_VAR(ncid, vars(ii)%id, vars(ii)%data)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
    END DO
  END SUBROUTINE writeINTEGER1dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: writeINTEGER2dVars
  !
  ! Write set of variables (2D, 'flat', INTEGER-valued) to a setup dataset
  !
  !----------------------------------------------------------------------

  SUBROUTINE writeINTEGER2dVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                          :: ncid
    TYPE(INTEGER2dVar), DIMENSION(:), INTENT(IN) :: vars

    INTEGER :: status, ii

    ! write the values to the dataset
    DO ii = 1, SIZE(vars)
       status = NF90_PUT_VAR(ncid, vars(ii)%id, vars(ii)%data)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
    END DO
  END SUBROUTINE writeINTEGER2dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: writeINTEGER3dVars
  !
  ! Write set of variables (3D, 'flat', INTEGER-valued) to a setup dataset
  !
  !----------------------------------------------------------------------

  SUBROUTINE writeINTEGER3dVars(ncid, vars)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                             :: ncid
    TYPE(INTEGER3dVar), DIMENSION(:), INTENT(IN)    :: vars

    INTEGER :: status, ii

    ! write the values to the dataset
    DO ii = 1, SIZE(vars)
       status = NF90_PUT_VAR(ncid, vars(ii)%id, vars(ii)%data)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
    END DO
  END SUBROUTINE writeINTEGER3dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: appendREAL1dVars
  !
  ! Append set of variables (1D, 'flat', REAL-valued)
  ! at the end (+offset) of the record dimension
  !----------------------------------------------------------------------

  SUBROUTINE appendREAL1dVars(ncid, vars, recordCoord, &
       & recordCoordBounds, offset)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                             :: ncid
    TYPE(REAL1dRecordVar), DIMENSION(:), INTENT(IN) :: vars
    REAL, INTENT(IN), OPTIONAL                      :: recordCoord
    REAL, INTENT(IN), DIMENSION(2), OPTIONAL        :: recordCoordBounds
    INTEGER, INTENT(IN), OPTIONAL                   :: offset

    INTEGER :: status, ii, recdimlen, recvarid
    INTEGER, DIMENSION(2) :: startoffset

    ! write the values to the dataset
    DO ii = 1, SIZE(vars)
       recdimlen = vars(ii)%dimLens(vars(ii)%recordDimIndex) + 1
       IF (PRESENT(offset)) recdimlen = recdimlen - 1 + offset
       SELECT CASE (vars(ii)%recordDimIndex)
       CASE (0)
          CALL handle_err("Variable not defined along a record dimension!")
       CASE (1)
          startoffset = (/recdimlen, 1/)
       CASE (2)
          startoffset = (/1, recdimlen/)
       END SELECT
       status = NF90_PUT_VAR(ncid, vars(ii)%id, vars(ii)%data, startoffset)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       IF (PRESENT(recordCoord)) then
          status = NF90_INQ_VARID(ncid, &
               & vars(ii)%dimnames(vars(ii)%recordDimIndex), recvarid)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          status = NF90_PUT_VAR(ncid, recvarid, recordCoord, (/ recdimlen /))
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          IF (PRESENT(recordCoordBounds)) then
             status = NF90_INQ_VARID(ncid, &
                  & TRIM(vars(ii)%dimnames(vars(ii)%recordDimIndex))//"_bnds", &
                  & recvarid)
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
             status = NF90_PUT_VAR(ncid, recvarid, recordCoordBounds, &
                  & (/ 1, recdimlen /))
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          END IF
       END IF
    END DO
  END SUBROUTINE appendREAL1dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: appendREAL2dVars
  !
  ! Append set of variables (2D, 'flat', REAL-valued)
  ! at the end (+offset) of the record dimension
  !----------------------------------------------------------------------

  SUBROUTINE appendREAL2dVars(ncid, vars, recordCoord, &
       & recordCoordBounds, offset)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                             :: ncid
    TYPE(REAL2dRecordVar), DIMENSION(:), INTENT(IN) :: vars
    REAL, INTENT(IN), optional                      :: recordCoord
    REAL, INTENT(IN), DIMENSION(2), optional        :: recordCoordBounds
    INTEGER, INTENT(IN), optional                   :: offset

    INTEGER :: status, ii, recdimlen, recvarid
    INTEGER, DIMENSION(3) :: startoffset

    ! write the values to the dataset
    DO ii = 1, SIZE(vars)
       recdimlen = vars(ii)%dimLens(vars(ii)%recordDimIndex)+1
       IF (PRESENT(offset)) recdimlen=recdimlen-1+offset
       SELECT CASE (vars(ii)%recordDimIndex)
       CASE (0)
          CALL handle_err("Variable not defined along a record dimension!")
       CASE (1)
          startoffset = (/recdimlen, 1, 1/)
       CASE (2)
          startoffset = (/1, recdimlen, 1/)
       CASE (3)
          startoffset = (/1, 1, recdimlen/)
       END SELECT
       status = NF90_PUT_VAR(ncid, vars(ii)%id, vars(ii)%data, startoffset)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       IF (PRESENT(recordCoord)) then
          status = NF90_INQ_VARID(ncid, &
               & vars(ii)%dimnames(vars(ii)%recordDimIndex), recvarid)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          status = NF90_PUT_VAR(ncid, recvarid, recordCoord, (/ recdimlen /))
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          IF (PRESENT(recordCoordBounds)) then
             status = NF90_INQ_VARID(ncid, &
                  & TRIM(vars(ii)%dimnames(vars(ii)%recordDimIndex))//"_bnds", &
                  & recvarid)
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
             status = NF90_PUT_VAR(ncid, recvarid, &
                  & recordCoordBounds, (/ 1, recdimlen /))
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          END IF
       END IF
    END DO
  END SUBROUTINE appendREAL2dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: appendREAL3dVars
  !
  ! Append set of variables (3D, 'flat', REAL-valued)
  ! at the end (+offset) of the record dimension
  !----------------------------------------------------------------------

  SUBROUTINE appendREAL3dVars(ncid, vars, recordCoord, &
       & recordCoordBounds, offset)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                             :: ncid
    TYPE(REAL3dRecordVar), DIMENSION(:), INTENT(IN) :: vars
    REAL, INTENT(IN), optional                      :: recordCoord
    REAL, INTENT(IN), DIMENSION(2), optional        :: recordCoordBounds
    INTEGER, INTENT(IN), optional                   :: offset

    INTEGER :: status, ii, recdimlen, recvarid
    INTEGER, DIMENSION(4) :: startoffset

    ! write the values to the dataset
    DO ii = 1, SIZE(vars)
       recdimlen = vars(ii)%dimLens(vars(ii)%recordDimIndex)+1
       IF (PRESENT(offset)) recdimlen=recdimlen-1+offset
       SELECT CASE (vars(ii)%recordDimIndex)
       CASE (0)
          CALL handle_err("Variable not defined along a record dimension!")
       CASE (1)
          startoffset = (/recdimlen, 1, 1, 1/)
       CASE (2)
          startoffset = (/1, recdimlen, 1, 1/)
       CASE (3)
          startoffset = (/1, 1, recdimlen, 1/)
       CASE (4)
          startoffset = (/1, 1, 1, recdimlen/)
       END SELECT
       status = NF90_PUT_VAR(ncid, vars(ii)%id, vars(ii)%data, startoffset)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       IF (PRESENT(recordCoord)) then
          status = NF90_INQ_VARID(ncid, &
               & vars(ii)%dimnames(vars(ii)%recordDimIndex), recvarid)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          status = NF90_PUT_VAR(ncid, recvarid, recordCoord, (/ recdimlen /))
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          IF (PRESENT(recordCoordBounds)) then
             status = NF90_INQ_VARID(ncid, &
                  & TRIM(vars(ii)%dimnames(vars(ii)%recordDimIndex))//"_bnds", &
                  & recvarid)
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
             status = NF90_PUT_VAR(ncid, recvarid, &
                  & recordCoordBounds, (/ 1, recdimlen /))
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          END IF
       END IF
    END DO
  END SUBROUTINE appendREAL3dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: appendINTEGER1dVars
  !
  ! Append set of variables (1D, 'flat', REAL-valued)
  ! at the end (+offset) of the record dimension
  !----------------------------------------------------------------------

  SUBROUTINE appendINTEGER1dVars(ncid, vars, recordCoord, &
       & recordCoordBounds, offset)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                                :: ncid
    TYPE(INTEGER1dRecordVar), DIMENSION(:), INTENT(IN) :: vars
    REAL, INTENT(IN), optional                         :: recordCoord
    REAL, INTENT(IN), DIMENSION(2), optional           :: recordCoordBounds
    INTEGER, INTENT(IN), optional                      :: offset

    INTEGER :: status, ii, recdimlen, recvarid
    INTEGER, DIMENSION(2) :: startoffset

    ! write the values to the dataset
    DO ii = 1, SIZE(vars)
       recdimlen = vars(ii)%dimLens(vars(ii)%recordDimIndex)+1
       IF (PRESENT(offset)) recdimlen=recdimlen-1+offset
       SELECT CASE (vars(ii)%recordDimIndex)
       CASE (0)
          CALL handle_err("Variable not defined along a record dimension!")
       CASE (1)
          startoffset = (/recdimlen, 1/)
       CASE (2)
          startoffset = (/1, recdimlen/)
       END SELECT
       status = NF90_PUT_VAR(ncid, vars(ii)%id, vars(ii)%data, startoffset)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       IF (PRESENT(recordCoord)) then
          status = NF90_INQ_VARID(ncid, &
               & vars(ii)%dimnames(vars(ii)%recordDimIndex), recvarid)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          status = NF90_PUT_VAR(ncid, recvarid, recordCoord, (/ recdimlen /))
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          IF (PRESENT(recordCoordBounds)) then
             status = NF90_INQ_VARID(ncid, &
                  & TRIM(vars(ii)%dimnames(vars(ii)%recordDimIndex))//"_bnds", &
                  & recvarid)
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
             status = NF90_PUT_VAR(ncid, recvarid, &
                  & recordCoordBounds, (/ 1, recdimlen /))
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          END IF
       END IF
    END DO
  END SUBROUTINE appendINTEGER1dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: appendINTEGER2dVars
  !
  ! Append set of variables (2D, 'flat', REAL-valued)
  ! at the end (+offset) of the record dimension
  !----------------------------------------------------------------------

  SUBROUTINE appendINTEGER2dVars(ncid, vars, recordCoord, &
       & recordCoordBounds, offset)
    IMPLICIT NONE
    INTEGER, INTENT(IN   )                             :: ncid
    TYPE(INTEGER2dRecordVar), DIMENSION(:), INTENT(IN) :: vars
    REAL, INTENT(IN), optional                         :: recordCoord
    REAL, INTENT(IN), DIMENSION(2), optional           :: recordCoordBounds
    INTEGER, INTENT(IN), optional                      :: offset

    INTEGER :: status, ii, recdimlen, recvarid
    INTEGER, DIMENSION(3) :: startoffset

    ! write the values to the dataset
    DO ii = 1, SIZE(vars)
       recdimlen = vars(ii)%dimLens(vars(ii)%recordDimIndex)+1
       IF (PRESENT(offset)) recdimlen=recdimlen-1+offset
       SELECT CASE (vars(ii)%recordDimIndex)
       CASE (0)
          CALL handle_err("Variable not defined along a record dimension!")
       CASE (1)
          startoffset = (/recdimlen, 1, 1/)
       CASE (2)
          startoffset = (/1, recdimlen, 1/)
       CASE (3)
          startoffset = (/1, 1, recdimlen/)
       END SELECT
       status = NF90_PUT_VAR(ncid, vars(ii)%id, vars(ii)%data, startoffset)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       IF (PRESENT(recordCoord)) then
          status = NF90_INQ_VARID(ncid, &
               & vars(ii)%dimnames(vars(ii)%recordDimIndex), recvarid)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          status = NF90_PUT_VAR(ncid, recvarid, recordCoord, (/ recdimlen /))
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          IF (PRESENT(recordCoordBounds)) then
             status = NF90_INQ_VARID(ncid, &
                  & TRIM(vars(ii)%dimnames(vars(ii)%recordDimIndex))//"_bnds", &
                  & recvarid)
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
             status = NF90_PUT_VAR(ncid, recvarid, &
                  & recordCoordBounds, (/ 1, recdimlen /))
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          END IF
       END IF
    END DO
  END SUBROUTINE appendINTEGER2dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: appendINTEGER3dVars
  !
  ! Append set of variables (3D, 'flat', REAL-valued)
  ! at the end (+offset) of the record dimension
  !----------------------------------------------------------------------

  SUBROUTINE appendINTEGER3dVars(ncid, vars, recordCoord, &
       & recordCoordBounds, offset)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                                :: ncid
    TYPE(INTEGER3dRecordVar), DIMENSION(:), INTENT(IN) :: vars
    REAL, INTENT(IN), optional                         :: recordCoord
    REAL, INTENT(IN), DIMENSION(2), optional           :: recordCoordBounds
    INTEGER, INTENT(IN), optional                      :: offset

    INTEGER :: status, ii, recdimlen, recvarid
    INTEGER, DIMENSION(4) :: startoffset

    ! write the values to the dataset
    DO ii = 1, SIZE(vars)
       recdimlen = vars(ii)%dimLens(vars(ii)%recordDimIndex)+1
       IF (PRESENT(offset)) recdimlen=recdimlen-1+offset
       SELECT CASE (vars(ii)%recordDimIndex)
       case (0)
          CALL handle_err("Variable not defined along a record dimension!")
       case (1)
          startoffset = (/recdimlen, 1, 1, 1/)
       case (2)
          startoffset = (/1, recdimlen, 1, 1/)
       case (3)
          startoffset = (/1, 1, recdimlen, 1/)
       case (4)
          startoffset = (/1, 1, 1, recdimlen/)
       END SELECT
       status = NF90_PUT_VAR(ncid, vars(ii)%id, vars(ii)%data, startoffset)
       IF (status /= NF90_NOERR) CALL handle_nc_err(status)
       IF (PRESENT(recordCoord)) then
          status = NF90_INQ_VARID(ncid, &
               & vars(ii)%dimnames(vars(ii)%recordDimIndex), recvarid)
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          status = NF90_PUT_VAR(ncid, recvarid, recordCoord, (/ recdimlen /))
          IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          IF (PRESENT(recordCoordBounds)) then
             status = NF90_INQ_VARID(ncid, &
                  & TRIM(vars(ii)%dimnames(vars(ii)%recordDimIndex))//"_bnds", &
                  & recvarid)
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
             status = NF90_PUT_VAR(ncid, recvarid, &
                  & recordCoordBounds, (/ 1, recdimlen /))
             IF (status /= NF90_NOERR) CALL handle_nc_err(status)
          END IF
       END IF
    END DO
  END SUBROUTINE appendINTEGER3dVars

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
  ! Subroutine: openNetCDFWrite
  !
  ! Opens an existing dataset for writing & returns dataset id
  !
  !----------------------------------------------------------------------

  SUBROUTINE openNetCDFWrite(filename, ncid)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: filename
    INTEGER, INTENT(OUT)         :: ncid

    INTEGER :: status

    ! create the dataset
    ! NF90_NOWRITE: open dataset for reading only
    ! NF90_WRITE (write only)
    ! NF90_SHARE (read and write) also available
    status = NF90_OPEN(TRIM(filename), NF90_WRITE, ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)
  END SUBROUTINE openNetCDFWrite

  !----------------------------------------------------------------------
  !
  ! Subroutine: reDef
  !
  ! Reenter 'definition mode'
  !
  !----------------------------------------------------------------------

  SUBROUTINE reDef(ncid)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ncid

    INTEGER :: status

    status = NF90_REDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)
  END SUBROUTINE reDef

  !----------------------------------------------------------------------
  !
  ! Subroutine: endDef
  !
  ! End of 'definition mode', enter 'data mode' for an open dataset
  !
  !----------------------------------------------------------------------

  SUBROUTINE endDef(ncid)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ncid

    INTEGER :: status

    status = NF90_ENDDEF(ncid)
    IF (status /= NF90_NOERR) CALL handle_nc_err(status)
  END SUBROUTINE endDef

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
    STOP "Stopped"
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
       STOP "Stopped"
    END IF
  END SUBROUTINE handle_nc_err

END MODULE local_netcdf
