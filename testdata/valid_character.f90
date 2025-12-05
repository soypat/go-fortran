! Test CHARACTER assumed-length and variable-length syntax
! This captures the fix for CHARACTER*(*) parsing.

! Test 1: CHARACTER assumed-length syntax: CHARACTER CH*(*)
      SUBROUTINE TEST_CHAR_ASSUMED(CH)
      IMPLICIT NONE
      CHARACTER CH*(*)
      CHARACTER CHH*12
      CHH = CH
      END SUBROUTINE

! Test 2: Multiple CHARACTER declarations with different lengths
      SUBROUTINE TEST_CHAR_MULTI(STR1, STR2)
      IMPLICIT NONE
      CHARACTER STR1*(*), STR2*(*)
      CHARACTER BUFFER*256
      BUFFER = STR1 // STR2
      END SUBROUTINE

! Test 3: CHARACTER array with assumed length
      SUBROUTINE TEST_CHAR_ARRAY(NAMES, N)
      IMPLICIT NONE
      INTEGER N
      CHARACTER NAMES(N)*(*)
      INTEGER I
      DO I=1,N
        PRINT *, NAMES(I)
      END DO
      END SUBROUTINE

! Test 4: Mixed CHARACTER declarations
      SUBROUTINE TEST_CHAR_MIXED(A, B, D)
      IMPLICIT NONE
      CHARACTER A*(*), B*20, D*(*)
      CHARACTER C*10
      C = A
      END SUBROUTINE
