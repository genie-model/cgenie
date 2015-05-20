!! Module for GENIE-wide utility routines, such as file checks,
!! NetCDF I/O routines? etc.

MODULE genie_util

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: check_unit, check_iostat, message, die

CONTAINS

  ! print a message and abort the program
  ! optional references to the line number and file of the error
  SUBROUTINE die(msg, line, file)
    USE genie_global, ONLY: write_status
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN)           :: msg
    INTEGER,          INTENT(IN), OPTIONAL :: line
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: file

    WRITE (6,*) "ERROR: ", TRIM(msg)
    IF (PRESENT(line)) THEN
       WRITE (6,*) 'at line: ', line
    END IF
    IF (PRESENT(file)) THEN
       WRITE (6,*) 'in file: ', TRIM(file)
    END IF
    WRITE (6,*) 'stopping'
    CALL flush(6)
    CALL write_status('ERRORED')
  END SUBROUTINE die

  ! Test whether a file unit is already in use
  SUBROUTINE check_unit(unitNum, line, file)
    USE genie_control, ONLY : BUFSIZ
    IMPLICIT NONE
    INTEGER,          INTENT(IN)           :: unitNum
    INTEGER,          INTENT(IN), OPTIONAL :: line
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: file

    LOGICAL :: LUNITINUSE
    CHARACTER(LEN=BUFSIZ) :: errStr

    INQUIRE(UNIT=unitNum,OPENED=lUnitInUse)
    IF (lUnitInUse) THEN
       WRITE (errStr,*) "unit", unitNum, "already in use"
       CALL die(errStr, line, file)
    END IF
  END SUBROUTINE check_unit

  ! Test the IOSTAT value for an I/O action
  SUBROUTINE check_iostat(ios, line, file)
    USE genie_control, ONLY : BUFSIZ
    IMPLICIT NONE
    INTEGER,          INTENT(IN)           :: ios
    INTEGER,          INTENT(IN), OPTIONAL :: line
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: file

    IF (ios/=0) THEN
       CALL die("I/O ERROR", line, file)
    END IF
  END SUBROUTINE check_iostat

  ! write message to screen if verbosity is sufficient
  SUBROUTINE message(msg,level,filename)
    USE genie_global, ONLY: verbosity, BUFSIZ
    IMPLICIT NONE
    ! The message
    CHARACTER(LEN=*), INTENT(IN) :: msg
    ! print if level <= global verbosity
    INTEGER, INTENT(IN) :: level
    ! Optional log filename
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: filename

    INTEGER, PARAMETER :: unitNum=8
    INTEGER :: ios
    CHARACTER(LEN=BUFSIZ) :: errStr

    IF (level <= verbosity) THEN
       IF (PRESENT(filename)) THEN
          CALL check_unit(unitNum,__LINE__,__FILE__)
          OPEN(UNIT=unitNum,FILE=filename,STATUS='UNKNOWN',IOSTAT=ios)
          IF (ios /= 0) THEN
             WRITE (errStr,*) 'could not open log file writing:', TRIM(filename)
             CALL die(errStr, __LINE__, __FILE__)
          END IF
          WRITE (unitNum,*) msg
          CLOSE(unitNum,IOSTAT=ios)
          IF (ios /= 0) THEN
             WRITE (errStr,*) 'could not close log file:', TRIM(filename)
             CALL die(errStr, __LINE__, __FILE__)
          END IF
       END IF
       WRITE (6,*) msg
    END IF
  END SUBROUTINE message

END MODULE genie_util
