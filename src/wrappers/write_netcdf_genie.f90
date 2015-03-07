MODULE write_netcdf

CONTAINS

  ! Subroutine to enable instantaneous easy output of
  ! grads/ferret-friendly single-level netcdf data
  !
  !     this version for the genie grid (ocean or atmosphere).
  !     (see also write_netcdf_igcm.f and write_netcdf_std.f in genie-igcm3)
  !
  !     [filename    = output file name]
  !     [data1       = data to be output]
  !     [invarnames  = name to be given to the netcdf variable]
  !     [alon1,alat1 = values of longitudes and latitudes]
  !     [nx,ny       = number of longitudes and latitudes]
  SUBROUTINE write_netcdf_genie(filename, data1_in, invarnames, &
       & alon1_in, alat1_in)
    USE genie_control
    USE writenc6
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN)               :: filename
    REAL, INTENT(IN), DIMENSION(:,:,:)         :: data1_in
    CHARACTER(LEN=*), INTENT(IN), DIMENSION(:) :: invarnames
    REAL, INTENT(IN), DIMENSION(:)             :: alon1_in
    REAL, INTENT(IN), DIMENSION(:)             :: alat1_in

    INTEGER :: nx, ny, ndim, nvar
    INTEGER, DIMENSION(nall) :: natts, nattsvar,vdims, ndims
    INTEGER, DIMENSION(nmaxdims,nall) :: vadims
    CHARACTER(LEN=BUFSIZ), DIMENSION(nall,nfiles)     :: dimname, varname
    CHARACTER(LEN=BUFSIZ), DIMENSION(2,nmaxdims,nall) :: attdimname, attvarname
    INTEGER, DIMENSION(nall,nfiles)                   :: iddim, idvar

    ! GENIE GRID:
    REAL, ALLOCATABLE, DIMENSION(:) :: alon1,alat1

    ! DATA:
    REAL, ALLOCATABLE, DIMENSION(:,:,:) :: data1

    ! NETCDF + AWI STUFF:
    INTEGER :: ncid, loc_dim

    INTEGER :: i, j, k
    INTEGER, DIMENSION(3) :: dataShape
    INTEGER, DIMENSION(1) :: varNamesShape, lonShape, latShape

    ! determine extent of longititude and latitude arrays
    lonShape = SHAPE(alon1_in)
    latShape = SHAPE(alat1_in)
    nx = lonShape(1)
    ny = latShape(1)

    ! determine extent of data and varaible name arrays
    dataShape = SHAPE(data1_in)
    varNamesShape = SHAPE(invarnames)

    ! test for consistency
    ! NB **ASSUME** THAT DATA ARRAY IS (LON:LAT:NUMVARS)

    IF (dataShape(1) /= nx) THEN
       PRINT *, "Error:", __LINE__, __FILE__, "Longitude inconsistency"
       STOP
    ELSE IF (dataShape(2) /= ny) THEN
       PRINT *, "Error:" ,__LINE__, __FILE__, "Latitude inconsistency"
       STOP
    ELSE IF (dataShape(3) /= varNamesShape(1)) THEN
       PRINT *, "Error:", __LINE__, __FILE__, "Num vars inconsistency"
       STOP
    END IF

    nvar = varNamesShape(1)

    ! allocate internal storage
    ALLOCATE(alon1(nx))
    ALLOCATE(alat1(ny))
    ALLOCATE(data1(nx,ny,nvar))

    ! copy values--reasons of floating pt resolution
    DO k = 1, nvar
       DO j = 1, ny
          DO i = 1, nx
             data1(i,j,k) = data1_in(i,j,k)
          END DO
       END DO
    END DO

    DO i = 1, nx
       alon1(i) = alon1_in(i)
    END DO

    DO j = 1, ny
       alat1(j) = alat1_in(j)
    END DO

    ! Set up dimension information
    ! NB **ASSUME** DATA ON SAME LON-LAT GRID ONLY
    ! COPIED FROM INITIALISE_ATMOS.F
    ndim = 2
    dimname(1,1) = 'longitude'
    ndims(1) = nx
    natts(1) = 2
    attdimname(1,1,1) = 'long_name'
    attdimname(2,1,1) = 'longitude'
    attdimname(1,2,1) = 'units'
    attdimname(2,2,1) = 'degrees east'

    dimname(2,1) = 'latitude'
    ndims(2) = ny
    natts(2) = 2
    attdimname(1,1,2) = 'long_name'
    attdimname(2,1,2) = 'latitude'
    attdimname(1,2,2) = 'units'
    attdimname(2,2,2) = 'degrees north'

    ! Setup variable information
    DO k = 1, nvar
       varname(k,1) = TRIM(invarnames(k))
       vdims(k) = 2
       vadims(1,k) = loc_dim('longitude', dimname, nall)
       vadims(2,k) = loc_dim('latitude', dimname, nall)
       nattsvar(k) = 2
       attvarname(1,1,k) = 'long_name'
       attvarname(2,1,k) = 'data from the model'
       attvarname(1,2,k) = 'units'
       attvarname(2,2,k) = 'no units'
    END DO

    CALL ininc(TRIM(filename), nmaxdims, ndim, nvar, natts, nattsvar, &
         & vdims, vadims, ndims, dimname(1,1), varname(1,1), &
         & attdimname, attvarname, ncid,iddim(1,1),idvar(1,1))

    CALL writedim(ncid, iddim(1,1), alon1)
    CALL writedim(ncid, iddim(2,1), alat1)

    DO k = 1, nvar
       CALL writevar(ncid, idvar(loc_dim(invarnames(k),varname(1,1),nall),1), &
            & data1(:,:,k))
    END DO

    CALL closenc(ncid)

    DEALLOCATE(alon1)
    DEALLOCATE(alat1)
    DEALLOCATE(data1)

  END SUBROUTINE write_netcdf_genie

END MODULE write_netcdf
