! golden.f90 - Progressive feature test file
! This file builds complexity incrementally with print statements
! at each level to verify execution progress during transpiler development.

      PROGRAM GOLDEN
      IMPLICIT NONE

! ==============================================================================
! LEVEL 1: Basic print statement
! ==============================================================================
      PRINT *, 'LEVEL 1: Hello, World!'

! ==============================================================================
! LEVEL 2: Variable declarations and simple assignments
! ==============================================================================
      INTEGER :: i, j, k
      REAL :: x, y, z
      LOGICAL :: flag
      CHARACTER(LEN=20) :: message

      i = 42
      x = 3.14159
      flag = .TRUE.
      message = 'Variables assigned'

      PRINT *, 'LEVEL 2: i =', i, ', x =', x
      PRINT *, 'LEVEL 2: flag =', flag
      PRINT *, 'LEVEL 2:', message

! ==============================================================================
! LEVEL 3: Arithmetic expressions and type promotion
! ==============================================================================
      j = i + 10
      k = i * 2
      y = x * 2.0
      z = REAL(i) + x

      PRINT *, 'LEVEL 3: j =', j, ', k =', k
      PRINT *, 'LEVEL 3: y =', y, ', z =', z

! ==============================================================================
! LEVEL 4: Conditional execution (IF statements)
! ==============================================================================
      IF (i .GT. 40) THEN
          PRINT *, 'LEVEL 4: i is greater than 40'
      END IF

      IF (flag) THEN
          PRINT *, 'LEVEL 4: flag is true'
      ELSE
          PRINT *, 'LEVEL 4: flag is false'
      END IF

      IF (x .LT. 3.0) THEN
          PRINT *, 'LEVEL 4: x < 3.0'
      ELSE IF (x .LT. 4.0) THEN
          PRINT *, 'LEVEL 4: 3.0 <= x < 4.0'
      ELSE
          PRINT *, 'LEVEL 4: x >= 4.0'
      END IF

! ==============================================================================
! LEVEL 5: Array declarations and memory access
! ==============================================================================
      INTEGER, DIMENSION(5) :: arr1
      REAL, DIMENSION(3, 3) :: matrix
      INTEGER :: idx

      ! Initialize array elements
      arr1(1) = 10
      arr1(2) = 20
      arr1(3) = 30
      arr1(4) = 40
      arr1(5) = 50

      PRINT *, 'LEVEL 5: arr1(1) =', arr1(1)
      PRINT *, 'LEVEL 5: arr1(3) =', arr1(3)
      PRINT *, 'LEVEL 5: arr1(5) =', arr1(5)

      ! Initialize matrix
      matrix(1, 1) = 1.0
      matrix(1, 2) = 0.0
      matrix(1, 3) = 0.0
      matrix(2, 1) = 0.0
      matrix(2, 2) = 1.0
      matrix(2, 3) = 0.0
      matrix(3, 1) = 0.0
      matrix(3, 2) = 0.0
      matrix(3, 3) = 1.0

      PRINT *, 'LEVEL 5: matrix(1,1) =', matrix(1, 1)
      PRINT *, 'LEVEL 5: matrix(2,2) =', matrix(2, 2)

! ==============================================================================
! LEVEL 6: DO loops and iteration
! ==============================================================================
      INTEGER :: sum_val
      sum_val = 0

      DO i = 1, 5
          sum_val = sum_val + arr1(i)
      END DO

      PRINT *, 'LEVEL 6: sum of arr1 =', sum_val

      ! Nested loop
      sum_val = 0
      DO i = 1, 3
          DO j = 1, 3
              sum_val = sum_val + 1
          END DO
      END DO

      PRINT *, 'LEVEL 6: nested loop count =', sum_val

! ==============================================================================
! LEVEL 7: Subroutine calls
! ==============================================================================
      INTEGER :: result

      CALL SIMPLE_SUB()
      CALL ADD_VALUES(10, 20, result)
      PRINT *, 'LEVEL 7: ADD_VALUES(10, 20) =', result

      CALL MODIFY_ARRAY(arr1, 5)
      PRINT *, 'LEVEL 7: arr1 after modify:', arr1(1), arr1(2), arr1(3)

! ==============================================================================
! LEVEL 8: Function calls
! ==============================================================================
      INTEGER :: fact_result
      REAL :: sqrt_result

      fact_result = FACTORIAL(5)
      PRINT *, 'LEVEL 8: FACTORIAL(5) =', fact_result

      sqrt_result = SQUARE_ROOT(16.0)
      PRINT *, 'LEVEL 8: SQUARE_ROOT(16.0) =', sqrt_result

! ==============================================================================
! LEVEL 9: Complex control flow
! ==============================================================================
      INTEGER :: n, fib_result

      n = 7
      fib_result = FIBONACCI(n)
      PRINT *, 'LEVEL 9: FIBONACCI(7) =', fib_result

      ! DO WHILE loop
      i = 1
      sum_val = 0
      DO WHILE (i .LE. 10)
          sum_val = sum_val + i
          i = i + 1
      END DO
      PRINT *, 'LEVEL 9: sum 1 to 10 =', sum_val

! ==============================================================================
! LEVEL 10: Mixed expressions and operations
! ==============================================================================
      REAL :: expr_result

      expr_result = (x + y) * z - REAL(k) / 2.0
      PRINT *, 'LEVEL 10: complex expr =', expr_result

      ! Logical operations
      LOGICAL :: cond1, cond2, cond3
      cond1 = (i .GT. 5) .AND. (j .LT. 100)
      cond2 = (x .GE. 3.0) .OR. (y .LE. 1.0)
      cond3 = .NOT. flag

      PRINT *, 'LEVEL 10: cond1 =', cond1, ', cond2 =', cond2
      PRINT *, 'LEVEL 10: cond3 =', cond3

! ==============================================================================
! LEVEL 11: Character operations
! ==============================================================================
      CHARACTER(LEN=10) :: str1, str2
      CHARACTER(LEN=20) :: str3

      str1 = 'Hello'
      str2 = 'World'
      str3 = str1 // ' ' // str2

      PRINT *, 'LEVEL 11: concatenation:', str3

! ==============================================================================
! LEVEL 12: Intrinsic functions
! ==============================================================================
      REAL :: angle, sin_val, cos_val, abs_val
      INTEGER :: max_val, min_val

      angle = 0.5
      sin_val = SIN(angle)
      cos_val = COS(angle)
      abs_val = ABS(-5.5)
      max_val = MAX(i, j, k)
      min_val = MIN(10, 20, 5)

      PRINT *, 'LEVEL 12: SIN(0.5) =', sin_val
      PRINT *, 'LEVEL 12: COS(0.5) =', cos_val
      PRINT *, 'LEVEL 12: ABS(-5.5) =', abs_val
      PRINT *, 'LEVEL 12: MAX =', max_val, ', MIN =', min_val

! ==============================================================================
! COMPLETION
! ==============================================================================
      PRINT *, '============================================='
      PRINT *, 'GOLDEN TEST COMPLETE: All levels executed!'
      PRINT *, '============================================='

      END PROGRAM GOLDEN

! ==============================================================================
! Subroutines and Functions
! ==============================================================================

      SUBROUTINE SIMPLE_SUB()
          PRINT *, 'LEVEL 7: Inside SIMPLE_SUB'
      END SUBROUTINE SIMPLE_SUB

      SUBROUTINE ADD_VALUES(a, b, result)
          INTEGER, INTENT(IN) :: a, b
          INTEGER, INTENT(OUT) :: result

          result = a + b
          PRINT *, 'LEVEL 7: Inside ADD_VALUES'
      END SUBROUTINE ADD_VALUES

      SUBROUTINE MODIFY_ARRAY(arr, n)
          INTEGER, INTENT(IN) :: n
          INTEGER, DIMENSION(n), INTENT(INOUT) :: arr
          INTEGER :: i

          DO i = 1, n
              arr(i) = arr(i) * 2
          END DO
          PRINT *, 'LEVEL 7: Inside MODIFY_ARRAY'
      END SUBROUTINE MODIFY_ARRAY

      INTEGER FUNCTION FACTORIAL(n)
          INTEGER, INTENT(IN) :: n
          INTEGER :: i, result

          result = 1
          DO i = 1, n
              result = result * i
          END DO

          FACTORIAL = result
      END FUNCTION FACTORIAL

      REAL FUNCTION SQUARE_ROOT(x)
          REAL, INTENT(IN) :: x

          SQUARE_ROOT = SQRT(x)
      END FUNCTION SQUARE_ROOT

      INTEGER FUNCTION FIBONACCI(n)
          INTEGER, INTENT(IN) :: n
          INTEGER :: a, b, temp, i

          IF (n .LE. 1) THEN
              FIBONACCI = n
              RETURN
          END IF

          a = 0
          b = 1

          DO i = 2, n
              temp = a + b
              a = b
              b = temp
          END DO

          FIBONACCI = b
      END FUNCTION FIBONACCI
