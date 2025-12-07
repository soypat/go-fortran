! Test specification part features in Fortran 90
! Simplified to features supported by transpiler

PROGRAM specification_test
  IMPLICIT NONE

  ! Type declarations with attributes
  INTEGER :: i, j, k
  REAL :: x, y, z
  DOUBLE PRECISION :: dp_val
  LOGICAL :: flag
  CHARACTER(LEN=20) :: name
  CHARACTER*10 :: old_style_string

  ! Array declarations
  INTEGER, DIMENSION(10) :: vec
  REAL, DIMENSION(5, 5) :: matrix
  REAL :: array(100, 200)

  ! Declarations with attributes
  INTEGER, PARAMETER :: MAX_SIZE = 1000
  REAL, PARAMETER :: PI = 3.14159265359
  INTEGER, SAVE :: counter

  ! COMMON blocks (variables declared first)
  REAL :: a, b, c
  REAL :: x1, y1, z1
  COMMON /block1/ a, b, c
  COMMON /block2/ x1, y1, z1

  ! EQUIVALENCE
  EQUIVALENCE (i, j)

  ! DATA statements
  DATA k /3/
  DATA x /1.0/, y /2.0/

  ! EXTERNAL and INTRINSIC
  INTRINSIC sin, cos, sqrt

END PROGRAM specification_test

SUBROUTINE test_specifications(n, arr)
  IMPLICIT NONE

  ! Intent attributes
  INTEGER, INTENT(IN) :: n
  REAL, INTENT(INOUT), DIMENSION(10) :: arr

  ! Local variables
  INTEGER :: i
  REAL :: temp

END SUBROUTINE test_specifications

MODULE specification_module
  IMPLICIT NONE

  ! Module variables
  INTEGER :: public_var

  ! Module parameters
  INTEGER, PARAMETER :: MODULE_CONST = 42

  CONTAINS

  FUNCTION public_func(x) RESULT(res)
    REAL, INTENT(IN) :: x
    REAL :: res

    res = x * 2.0
  END FUNCTION public_func

END MODULE specification_module

FUNCTION typed_function(a, b) RESULT(sum_val)
  ! Function with explicit result variable
  IMPLICIT NONE
  REAL, INTENT(IN) :: a, b
  REAL :: sum_val

  sum_val = a + b
END FUNCTION typed_function

SUBROUTINE array_decl()
  INTEGER, DIMENSION(3) :: a
  a = (/ 1, 2, 3 /)
END SUBROUTINE
