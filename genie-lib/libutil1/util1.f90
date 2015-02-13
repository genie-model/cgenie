!
! This function returns the singificant length of a character string
!
FUNCTION lnsig(title)
  IMPLICIT NONE
  CHARACTER(LEN=*) :: title
  INTEGER :: lnsig, ilen, i

  ilen = LEN(title)
  DO i = ilen, 1, -1
     IF (title(i:i) /= ' ') EXIT
  END DO
  lnsig = i
  IF (i > 0) THEN
     IF (ICHAR(title(i:i)) == 0) lnsig = i - 1
  END IF
END FUNCTION lnsig


!
! This function returns the length of the non-blank character
! string TEXT. It also removes any leading blanks
!
! First find out the length of the non-blank string
!
FUNCTION lnsig1(text)
  IMPLICIT NONE
  INTEGER :: lnsig1, ilen, ii, iend, ibeg, iii
  CHARACTER(LEN=*) :: text

  ilen = LEN(text)
  DO ii = ilen, 1, -1
     IF (text(ii:ii) /= ' ') EXIT
  END DO
  IF (ii > 0) THEN
     IF (ICHAR(text(ii:ii)) == 0) ii = ii - 1
  END IF

  iend = ii
  IF (iend /= 0) THEN
     DO ii = 1, iend
        IF (text(ii:ii) /= ' ') EXIT
     END DO
     ibeg = ii
     text(1:iend-ibeg+1) = text(ibeg:iend)
     IF (iend-ibeg+1 /= ilen) THEN
        DO iii = iend-ibeg+2, ilen
           text(iii:iii) = ' '
        END DO
     END IF
     lnsig1 = iend-ibeg+1
  ELSE
     lnsig1 = 1
  END IF
END FUNCTION LNSIG1
