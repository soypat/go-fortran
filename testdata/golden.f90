! golden.f90 - Progressive feature test file
! Each level is a separate subroutine for easier incremental testing

      PROGRAM GOLDEN
      IMPLICIT NONE

      CALL LEVEL01()
      CALL LEVEL02()
      CALL LEVEL03()
      CALL LEVEL04()
      CALL LEVEL05()
      CALL LEVEL06()
      CALL LEVEL07()
      CALL LEVEL08()
      CALL LEVEL09()
      CALL LEVEL10()
      CALL LEVEL11()
      CALL LEVEL12()

      PRINT *, '============================================='
      PRINT *, 'GOLDEN TEST COMPLETE: All levels executed!'
      PRINT *, '============================================='

      CONTAINS

! ==============================================================================
! LEVEL 1: Basic print statement
! ==============================================================================
      SUBROUTINE LEVEL01()
          PRINT *, 'LEVEL 1: Hello, World!'
      END SUBROUTINE LEVEL01

! ==============================================================================
! LEVEL 2: Variable declarations and simple assignments
! ==============================================================================
      SUBROUTINE LEVEL02()
          INTEGER :: i
          REAL :: x
          LOGICAL :: flag
          CHARACTER(LEN=20) :: message

          i = 42
          x = 3.14159
          flag = .TRUE.
          message = 'Variables assigned'

          PRINT *, 'LEVEL 2: i =', i, ', x =', x
          PRINT *, 'LEVEL 2: flag =', flag
          PRINT *, 'LEVEL 2:', message
      END SUBROUTINE LEVEL02

! ==============================================================================
! LEVEL 3: Arithmetic expressions and type promotion
! ==============================================================================
      SUBROUTINE LEVEL03()
          INTEGER :: i, j, k
          REAL :: x, y, z

          i = 42
          x = 3.14159

          j = i + 10
          k = i * 2
          y = x * 2.0
          z = REAL(i) + x

          PRINT *, 'LEVEL 3: j =', j, ', k =', k
          PRINT *, 'LEVEL 3: y =', y, ', z =', z
      END SUBROUTINE LEVEL03

! ==============================================================================
! LEVEL 4: Conditional execution (IF statements)
! ==============================================================================
      SUBROUTINE LEVEL04()
          INTEGER :: i
          REAL :: x
          LOGICAL :: flag

          i = 42
          x = 3.14159
          flag = .TRUE.

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
      END SUBROUTINE LEVEL04

! ==============================================================================
! LEVEL 5: Array declarations and memory access
! ==============================================================================
      SUBROUTINE LEVEL05()
          INTEGER, DIMENSION(5) :: arr1
          REAL, DIMENSION(3, 3) :: matrix

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
      END SUBROUTINE LEVEL05

! ==============================================================================
! LEVEL 6: DO loops and iteration
! ==============================================================================
      SUBROUTINE LEVEL06()
          INTEGER :: i, j
          INTEGER, DIMENSION(5) :: arr1
          INTEGER :: sum_val

          ! Initialize array
          arr1(1) = 10
          arr1(2) = 20
          arr1(3) = 30
          arr1(4) = 40
          arr1(5) = 50

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
      END SUBROUTINE LEVEL06

! ==============================================================================
! LEVEL 7: Subroutine calls
! ==============================================================================
      SUBROUTINE LEVEL07()
          INTEGER, DIMENSION(5) :: arr1
          INTEGER :: result

          arr1(1) = 10
          arr1(2) = 20
          arr1(3) = 30
          arr1(4) = 40
          arr1(5) = 50

          CALL SIMPLE_SUB()
          CALL ADD_VALUES(10, 20, result)
          PRINT *, 'LEVEL 7: ADD_VALUES(10, 20) =', result

          CALL MODIFY_ARRAY(arr1, 5)
          PRINT *, 'LEVEL 7: arr1 after modify:', arr1(1), arr1(2), arr1(3)
      END SUBROUTINE LEVEL07

! ==============================================================================
! LEVEL 8: Function calls
! ==============================================================================
      SUBROUTINE LEVEL08()
          INTEGER :: fact_result
          REAL :: sqrt_result

          fact_result = FACTORIAL(5)
          PRINT *, 'LEVEL 8: FACTORIAL(5) =', fact_result

          sqrt_result = SQUARE_ROOT(16.0)
          PRINT *, 'LEVEL 8: SQUARE_ROOT(16.0) =', sqrt_result
      END SUBROUTINE LEVEL08

! ==============================================================================
! LEVEL 9: Complex control flow
! ==============================================================================
      SUBROUTINE LEVEL09()
          INTEGER :: i, n, fib_result, sum_val

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
      END SUBROUTINE LEVEL09

! ==============================================================================
! LEVEL 10: Mixed expressions and operations
! ==============================================================================
      SUBROUTINE LEVEL10()
          INTEGER :: i, j, k
          REAL :: x, y, z, expr_result
          LOGICAL :: flag, cond1, cond2, cond3

          i = 11
          j = 52
          k = 84
          x = 3.14159
          y = 6.28318
          z = 45.14159
          flag = .TRUE.

          expr_result = (x + y) * z - REAL(k) / 2.0
          PRINT *, 'LEVEL 10: complex expr =', expr_result

          ! Logical operations
          cond1 = (i .GT. 5) .AND. (j .LT. 100)
          cond2 = (x .GE. 3.0) .OR. (y .LE. 1.0)
          cond3 = .NOT. flag

          PRINT *, 'LEVEL 10: cond1 =', cond1, ', cond2 =', cond2
          PRINT *, 'LEVEL 10: cond3 =', cond3
      END SUBROUTINE LEVEL10

! ==============================================================================
! LEVEL 11: Character operations
! ==============================================================================
      SUBROUTINE LEVEL11()
          CHARACTER(LEN=10) :: str1, str2
          CHARACTER(LEN=20) :: str3

          str1 = 'Hello'
          str2 = 'World'
          str3 = str1 // ' ' // str2

          PRINT *, 'LEVEL 11: concatenation:', str3
      END SUBROUTINE LEVEL11

! ==============================================================================
! LEVEL 12: Intrinsic functions
! ==============================================================================
      SUBROUTINE LEVEL12()
          REAL :: angle, sin_val, cos_val, abs_val
          INTEGER :: i, j, k, max_val, min_val

          i = 11
          j = 52
          k = 84

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
      END SUBROUTINE LEVEL12

! ==============================================================================
! Helper Subroutines and Functions
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

      END PROGRAM GOLDEN
