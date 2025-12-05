! Test corpus for implicit typing, especially DO loop variables.
! This captures the fix for implicit variable creation in DO loops.

! Test 1: Default implicit typing (I-N are INTEGER, A-H,O-Z are REAL)
      PROGRAM TEST_DEFAULT_IMPLICIT
      DO I=1,10
        X = I * 2.0
        DO J=1,5
          Y = X + J
        END DO
      END DO
      K = I + J
      A = X + Y
      END PROGRAM

! Test 2: Custom IMPLICIT statement with DO loop
      SUBROUTINE TEST_CUSTOM_IMPLICIT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DO I=1,200
        X = DBLE(I) * 3.14159D0
        DO J=1,100
          Y = X / DBLE(J)
        END DO
      END DO
      END SUBROUTINE

! Test 3: Labeled DO loop (F77 style)
      SUBROUTINE TEST_LABELED_DO
      IMPLICIT REAL*8 (A-H,O-Z)
      DO 10 I=1,50
        X = I
        DO 20 J=1,25
          Y = X * J
   20   CONTINUE
   10 CONTINUE
      END SUBROUTINE

! Test 4: Nested DO loops with step
      SUBROUTINE TEST_NESTED_STEP
      DO I=1,100,2
        DO J=10,1,-1
          DO K=1,I,J
            N = I + J + K
          END DO
        END DO
      END DO
      END SUBROUTINE

! Test 5: DO WHILE with implicit variable
      SUBROUTINE TEST_DO_WHILE
      I = 0
      DO WHILE (I .LT. 10)
        X = I * 2.5
        I = I + 1
      END DO
      END SUBROUTINE

! Test 6: Multiple IMPLICIT statements
      SUBROUTINE TEST_MULTI_IMPLICIT
      IMPLICIT INTEGER (A-C)
      IMPLICIT REAL*8 (D-H,O-Z)
      DO I=1,10
        A = I
        D = DBLE(A)
      END DO
      END SUBROUTINE

! Test 7: Complex expression in DO bounds using implicit vars
      SUBROUTINE TEST_COMPLEX_BOUNDS(NMAX)
      IMPLICIT INTEGER (N)
      DO I=1,NMAX
        DO J=I,NMAX-I+1
          X = I + J
        END DO
      END DO
      END SUBROUTINE
