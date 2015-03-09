!----------------------------------------------------------------------
!>
!> Module: local_output
!>
!> Output routines: The routines are generally stateless (access via
!> filename of the output file. These routines use the NetCDF routines
!> provided by the module "local_netcdf".
!>
!----------------------------------------------------------------------
MODULE local_output

  USE genie_util, ONLY: message, die
  USE local_netcdf

  PRIVATE

  PUBLIC :: resetOutput
  PUBLIC :: defineDimension
  PUBLIC :: defineRecordDimension
  PUBLIC :: writeReal2dVariable
  PUBLIC :: writeInteger2dVariable
  PUBLIC :: writeReal2dRecordVariable
  PUBLIC :: writeReal3dVariable
  PUBLIC :: writeReal3dRecordVariable

CONTAINS

  !----------------------------------------------------------------------
  !>
  !> Internal SUBROUTINE: openOutput
  !>
  !> opens output file, returns handle to output file
  !>
  !----------------------------------------------------------------------
  SUBROUTINE openOutput(filename, ID, readonly, reset)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: filename
    INTEGER, INTENT(OUT) :: ID
    LOGICAL, OPTIONAL, INTENT(IN) :: readonly, reset

    LOGICAL :: exists

    IF (PRESENT(readonly) .AND. readonly) THEN
       CALL message("Opening existing output file in read-only mode!", 3)
       CALL openNetCDFRead(filename, ID)
    ELSE
       INQUIRE(FILE=filename,EXIST=exists)
       IF (.NOT. exists) THEN
          CALL message("Creating new output file: "//filename, 3)
          CALL createNetCDF(filename, ID)
          CALL endDef(ID)
       ELSE
          IF (PRESENT(reset) .AND. reset) THEN
             CALL message("Resetting existing output file: "//filename, 3)
             CALL createNetCDF(filename, ID, .TRUE.)
          ELSE
             CALL message("Opening existing output file: "//filename, 3)
             CALL openNetCDFWrite(filename, ID)
          END IF
       END IF
    END IF
  END SUBROUTINE openOutput

  !----------------------------------------------------------------------
  !>
  !> Internal SUBROUTINE: closeInOutput
  !>
  !> closes input/output file
  !>
  !----------------------------------------------------------------------
  SUBROUTINE closeInOutput(ID)
    INTEGER, INTENT(IN) :: ID

    CALL closeNetCDF(ID)
  END SUBROUTINE closeInOutput

  !----------------------------------------------------------------------
  !>
  !> SUBROUTINE: resetOutput
  !>
  !> resets (or creates) output file
  !>
  !----------------------------------------------------------------------
  SUBROUTINE resetOutput(filename)
    CHARACTER(LEN=*), INTENT(IN) :: filename

    INTEGER :: ID

    CALL openOutput(filename, ID, .FALSE., .TRUE.)
    CALL closeInOutput(ID)
  END SUBROUTINE resetOutput

  !----------------------------------------------------------------------
  !>
  !> SUBROUTINE: defineDimension
  !>
  !> defines Dimension
  !>
  !----------------------------------------------------------------------

  SUBROUTINE defineDimension(filename, dimName, dimValues, dimBoundariesLower, &
       & dimBoundariesUpper, dimLongname, dimStandardname, dimUnits)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: filename, dimName
    REAL, DIMENSION(:), INTENT(IN) :: &
         & dimValues, dimBoundariesLower, dimBoundariesUpper
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: &
         & dimLongname, dimStandardname, dimUnits

    INTEGER :: ID, status, dimlen
    TYPE(realDimInfo), POINTER, DIMENSION(:) :: dimNetCDF

    dimLen = SIZE(dimValues)

    CALL openOutput(filename, ID)

    ALLOCATE(dimNetCDF(1), STAT=status)
    IF (status /= 0) CALL die("Could not allocate storage")
    ALLOCATE(dimNetCDF(1)%coords(1:dimlen), STAT=status)
    IF (status /= 0) CALL die("Could not ALLOCATE storage")
    ALLOCATE(dimNetCDF(1)%boundsLower(dimlen), STAT=status)
    IF (status /= 0) CALL die("Could not ALLOCATE storage")
    ALLOCATE(dimNetCDF(1)%boundsUpper(dimlen), STAT=status)
    IF (status /= 0) CALL die("Could not ALLOCATE storage")

    dimNetCDF(1)%boundsDefine = .TRUE.
    dimNetCDF(1)%name = dimName
    dimNetCDF(1)%len = dimLen
    dimNetCDF(1)%basicAtts%long_name = dimLongname
    dimNetCDF(1)%basicAtts%standard_name = dimStandardname
    dimNetCDF(1)%basicAtts%units = dimUnits
    dimNetCDF(1)%coords(:) = dimValues(:)
    dimNetCDF(1)%boundsLower(:) = dimBoundariesLower(:)
    dimNetCDF(1)%boundsUpper(:) = dimBoundariesUpper(:)

    PRINT *, filename
    CALL defineDims(ID, dimNetCDF)
    PRINT *, filename

    CALL closeInOutput(ID)

    DEALLOCATE(dimNetCDF(1)%coords)
    DEALLOCATE(dimNetCDF(1)%boundsLower)
    DEALLOCATE(dimNetCDF(1)%boundsUpper)
    DEALLOCATE(dimNetCDF)
  END SUBROUTINE defineDimension

  !----------------------------------------------------------------------
  !>
  !> SUBROUTINE: defineRecordDimension
  !>
  !> defines record Dimension
  !>
  !----------------------------------------------------------------------

  SUBROUTINE defineRecordDimension(filename, dimName, dimLongname, &
       & dimStandardname, dimUnits)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: filename, dimName
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: &
         & dimLongname, dimStandardname, dimUnits

    INTEGER :: ID, status
    TYPE(realRecordDimInfo), POINTER, DIMENSION(:) :: dimNetCDF

    CALL openOutput(filename, ID)

    ALLOCATE(dimNetCDF(1), STAT=status)
    IF (status /= 0) CALL die("Could not ALLOCATE storage")

    dimNetCDF(1)%coordsDefine = .TRUE.
    dimNetCDF(1)%boundsDefine = .TRUE.
    dimNetCDF(1)%name = dimName
    dimNetCDF(1)%basicAtts%long_name = dimLongname
    dimNetCDF(1)%basicAtts%standard_name = dimStandardname
    dimNetCDF(1)%basicAtts%units = dimUnits

    CALL defineDims(ID, dimNetCDF)

    CALL closeInOutput(ID)

    DEALLOCATE(dimNetCDF)
  END SUBROUTINE defineRecordDimension

  !----------------------------------------------------------------------
  !>
  !> SUBROUTINE: writeReal2dVariable
  !>
  !> defines Variable, if non-existing
  !> writes Variable
  !>
  !----------------------------------------------------------------------
  SUBROUTINE writeReal2dVariable(filename, varName, varDimName1, varDimName2, &
       & varValues, varLongname, varStandardname, varUnits, varMissingValue)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: filename, varName
    CHARACTER(LEN=*), INTENT(IN) :: varDimName1, varDimName2
    REAL, DIMENSION(:,:), INTENT(IN) :: varValues
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: &
         & varLongname, varStandardname, varUnits
    REAL, INTENT(IN), OPTIONAL :: varMissingValue

    CHARACTER(LEN=NF90_MAX_NAME), DIMENSION(2) :: varDimNames
    TYPE(real2dVar), POINTER, DIMENSION(:) :: varNetCDF
    INTEGER :: ID, status
    INTEGER, DIMENSION(2) :: arraySize

    CALL openOutput(filename, ID)

    ALLOCATE(varNetCDF(1), STAT=status)
    IF (status /= 0) CALL die("Could not ALLOCATE storage")
    arraySize = SHAPE(varValues)
    ALLOCATE(varNetCDF(1)%data(arraySize(1),arraySize(2)), STAT=status)
    IF (status /= 0) CALL die("Could not ALLOCATE storage")
    varNetCDF(1)%name = varName
    varNetCDF(1)%basicAtts%long_name = varLongname
    varNetCDF(1)%basicAtts%standard_name = varStandardname
    varNetCDF(1)%basicAtts%units = varUnits
    varNetCDF(1)%basicAtts%missing_value = varMissingValue
    varNetCDF(1)%data(:,:) = varValues(:,:)

    CALL lookupVars(ID, varNetCDF)

    IF (varNetCDF(1)%id < 0) THEN
       varDimNames(1) = varDimName1
       varDimNames(2) = varDimName2
       CALL dimVars(ID, varNetCDF, varDimNames)

       CALL defineVars(ID,varNetCDF)
    END IF

    CALL writeVars(ID,varNetCDF)

    CALL closeInOutput(ID)

    DEALLOCATE(varNetCDF(1)%data)
    DEALLOCATE(varNetCDF)
  END SUBROUTINE writeReal2dVariable

  !----------------------------------------------------------------------
  !>
  !> SUBROUTINE: writeInteger2dVariable
  !>
  !> defines Variable, if non-existing
  !> writes Variable
  !>
  !----------------------------------------------------------------------
  SUBROUTINE writeInteger2dVariable(filename, varName, &
       & varDimName1, varDimName2, varValues, varLongname, &
       & varStandardname, varUnits, varMissingValue)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: filename, varName
    CHARACTER(LEN=*), INTENT(IN) :: varDimName1, varDimName2
    INTEGER, DIMENSION(:,:), INTENT(IN) :: varValues
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: &
         & varLongname, varStandardname, varUnits
    INTEGER, INTENT(IN), OPTIONAL :: varMissingValue

    TYPE(integer2dVar), POINTER, DIMENSION(:) :: varNetCDF
    INTEGER :: ID, status
    INTEGER, DIMENSION(2) :: arraySize

    CALL openOutput(filename, ID)

    ALLOCATE(varNetCDF(1), STAT=status)
    IF (status /= 0) CALL die("Could not ALLOCATE storage")
    arraySize = SHAPE(varValues)
    ALLOCATE(varNetCDF(1)%data(arraySize(1),arraySize(2)), STAT=status)
    IF (status /= 0) CALL die("Could not ALLOCATE storage")
    varNetCDF(1)%name = varName
    varNetCDF(1)%basicAtts%long_name = varLongname
    varNetCDF(1)%basicAtts%standard_name = varStandardname
    varNetCDF(1)%basicAtts%units = varUnits
    varNetCDF(1)%basicAtts%missing_value = varMissingValue
    varNetCDF(1)%data(:,:) = varValues(:,:)

    CALL lookupINTEGER2dVars(ID, varNetCDF)

    IF (varNetCDF(1)%id < 0) THEN
       CALL dimINTEGER2dVars(ID,varNetCDF,(/varDimName1,varDimName2/))
       CALL defineINTEGER2dVars(ID,varNetCDF)
    END IF

    CALL writeINTEGER2dVars(ID,varNetCDF)

    CALL closeInOutput(ID)

    DEALLOCATE(varNetCDF(1)%data)
    DEALLOCATE(varNetCDF)
  END SUBROUTINE writeINTEGER2dVariable

  !----------------------------------------------------------------------
  !>
  !> SUBROUTINE: writeReal2dRecordVariable
  !>
  !> defines Variable, if non-existing
  !> writes Variable
  !>
  !----------------------------------------------------------------------
  SUBROUTINE writeReal2dRecordVariable(filename, varName, &
       & varDimName1, varDimName2, varDimName3, varValues, &
       & recordCoord, recordCoordBounds, &
       & varLongname, varStandardname, varUnits, varMissingValue, offset)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: filename, varName
    CHARACTER(LEN=*), INTENT(IN) :: varDimName1,varDimName2, varDimName3
    REAL, DIMENSION(:,:), INTENT(IN) :: varValues
    REAL, INTENT(IN) :: recordCoord
    REAL, INTENT(IN), DIMENSION(2) :: recordCoordBounds
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: &
         & varLongname, varStandardname, varUnits
    REAL, INTENT(IN), OPTIONAL :: varMissingValue
    INTEGER, INTENT(IN), OPTIONAL :: offset

    CHARACTER(LEN=NF90_MAX_NAME), DIMENSION(3) :: varDimNames
    TYPE(real2dRecordVar), POINTER, DIMENSION(:) :: varNetCDF
    INTEGER :: ID, status
    INTEGER, DIMENSION(2) :: arraySize

    CALL openOutput(filename, ID)

    ALLOCATE(varNetCDF(1), STAT=status)
    IF (status /= 0) CALL die("Could not ALLOCATE storage")
    arraySize = SHAPE(varValues)
    ALLOCATE(varNetCDF(1)%data(arraySize(1),arraySize(2)), STAT=status)
    IF (status /= 0) CALL die("Could not ALLOCATE storage")
    varNetCDF(1)%name = varName
    varNetCDF(1)%basicAtts%long_name = varLongname
    varNetCDF(1)%basicAtts%standard_name = varStandardname
    varNetCDF(1)%basicAtts%units = varUnits
    varNetCDF(1)%basicAtts%missing_value = varMissingValue
    varNetCDF(1)%data(:,:) = varValues(:,:)

    CALL lookupVars(ID,varNetCDF)
    IF (varNetCDF(1)%id < 0) THEN
       varDimNames(1)=varDimName1
       varDimNames(2)=varDimName2
       varDimNames(3)=varDimName3
       CALL dimVars(ID, varNetCDF, varDimNames)

       CALL defineVars(ID, varNetCDF)
    END IF

    IF (PRESENT(offset)) THEN
       CALL appendVars(ID, varNetCDF, recordCoord, recordCoordBounds, &
            & offset=offset)
    ELSE
       CALL appendVars(ID, varNetCDF, recordCoord, recordCoordBounds)
    END IF
    CALL closeInOutput(ID)

    DEALLOCATE(varNetCDF(1)%data)
    DEALLOCATE(varNetCDF)
  END SUBROUTINE writeReal2dRecordVariable

  !----------------------------------------------------------------------
  !>
  !> SUBROUTINE: writeReal3dVariable
  !>
  !> defines Variable, if non-existing
  !> writes Variable
  !>
  !----------------------------------------------------------------------
  SUBROUTINE writeReal3dVariable(filename, varName, &
       & varDimName1, varDimName2, varDimName3, varValues, &
       & varLongname, varStandardname, varUnits, varMissingValue)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: filename, varName
    CHARACTER(LEN=*), INTENT(IN) :: varDimName1, varDimName2, varDimName3
    REAL, DIMENSION(:,:,:), INTENT(IN) :: varValues
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: &
         & varLongname, varStandardname, varUnits
    REAL, INTENT(IN), OPTIONAL :: varMissingValue

    CHARACTER(LEN=NF90_MAX_NAME), DIMENSION(3) :: varDimNames
    TYPE(real3dVar), POINTER, DIMENSION(:) :: varNetCDF
    INTEGER :: ID, status
    INTEGER, DIMENSION(3) :: arraySize

    CALL openOutput(filename, ID)

    ALLOCATE(varNetCDF(1), STAT=status)
    IF (status /= 0) CALL die("Could not ALLOCATE storage")
    arraySize = SHAPE(varValues)
    ALLOCATE(varNetCDF(1)%data(arraySize(1),arraySize(2),arraySize(3)), &
         & STAT=status)
    IF (status /= 0) CALL die("Could not ALLOCATE storage")
    varNetCDF(1)%name = varName
    varNetCDF(1)%basicAtts%long_name = varLongname
    varNetCDF(1)%basicAtts%standard_name = varStandardname
    varNetCDF(1)%basicAtts%units = varUnits
    varNetCDF(1)%basicAtts%missing_value = varMissingValue
    varNetCDF(1)%data(:,:,:) = varValues(:,:,:)

    CALL lookupVars(ID, varNetCDF)

    IF (varNetCDF(1)%id < 0) THEN
       varDimNames(1) = varDimName1
       varDimNames(2) = varDimName2
       varDimNames(3) = varDimName3
       CALL dimVars(ID, varNetCDF, varDimNames)

       CALL defineVars(ID, varNetCDF)
    END IF

    CALL writeVars(ID, varNetCDF)

    CALL closeInOutput(ID)

    DEALLOCATE(varNetCDF(1)%data)
    DEALLOCATE(varNetCDF)
  END SUBROUTINE writeReal3dVariable

  !----------------------------------------------------------------------
  !>
  !> SUBROUTINE: writeReal3dRecordVariable
  !>
  !> defines Variable, if non-existing
  !> writes Variable
  !>
  !----------------------------------------------------------------------
  SUBROUTINE writeReal3dRecordVariable(filename, varName, &
       & varDimName1, varDimName2, varDimName3, varDimName4, varValues, &
       & recordCoord, recordCoordBounds, &
       & varLongname, varStandardname, varUnits, varMissingValue, offset)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: filename, varName
    CHARACTER(LEN=*), INTENT(IN) :: &
         & varDimName1, varDimName2, varDimName3, varDimName4
    REAL, DIMENSION(:,:,:), INTENT(IN) :: varValues
    REAL, INTENT(IN) :: recordCoord
    REAL, INTENT(IN), DIMENSION(2) :: recordCoordBounds
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: &
         & varLongname, varStandardname, varUnits
    REAL, INTENT(IN), OPTIONAL :: varMissingValue
    INTEGER, INTENT(IN), OPTIONAL :: offset

    CHARACTER(LEN=NF90_MAX_NAME), DIMENSION(4) :: varDimNames
    TYPE(real3dRecordVar), POINTER, DIMENSION(:) :: varNetCDF
    INTEGER :: ID, status
    INTEGER, DIMENSION(3) :: arraySize

    CALL openOutput(filename, ID)

    ALLOCATE(varNetCDF(1), STAT=status)
    IF (status /= 0) CALL die("Could not ALLOCATE storage")
    arraySize = SHAPE(varValues)
    ALLOCATE(varNetCDF(1)%data(arraySize(1),arraySize(2),arraySize(3)), &
         & STAT=status)
    IF (status /= 0) CALL die("Could not ALLOCATE storage")
    varNetCDF(1)%name = varName
    varNetCDF(1)%basicAtts%long_name = varLongname
    varNetCDF(1)%basicAtts%standard_name = varStandardname
    varNetCDF(1)%basicAtts%units = varUnits
    varNetCDF(1)%basicAtts%missing_value = varMissingValue
    varNetCDF(1)%data(:,:,:) = varValues(:,:,:)

    CALL lookupVars(ID, varNetCDF)
    IF (varNetCDF(1)%id < 0) THEN
       varDimNames(1) = varDimName1
       varDimNames(2) = varDimName2
       varDimNames(3) = varDimName3
       varDimNames(4) = varDimName4
       CALL dimVars(ID, varNetCDF, varDimNames)

       CALL defineVars(ID, varNetCDF)
    END IF

    IF (PRESENT(offset)) THEN
       CALL appendVars(ID, varNetCDF, recordCoord, recordCoordBounds, &
            & offset=offset)
    ELSE
       CALL appendVars(ID, varNetCDF, recordCoord, recordCoordBounds)
    END IF
    CALL closeInOutput(ID)

    DEALLOCATE(varNetCDF(1)%data)
    DEALLOCATE(varNetCDF)
  END SUBROUTINE writeReal3dRecordVariable

END MODULE local_output
