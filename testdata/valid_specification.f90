! Test specification part features in Fortran 90
! These are collected as tokens in Phase 1

PROGRAM specification_test
  ! USE statements
  USE iso_fortran_env, ONLY: real64, int32
  USE my_module

  ! IMPLICIT statements
  IMPLICIT NONE

  ! Type declarations with attributes
  INTEGER :: i, j, k
  REAL :: x, y, z
  DOUBLE PRECISION :: dp_val
  COMPLEX :: c
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
  REAL, TARGET :: target_var
  INTEGER, POINTER :: ptr

  ! Allocatable arrays
  REAL, ALLOCATABLE, DIMENSION(:) :: dynamic_vec
  REAL, ALLOCATABLE :: dynamic_matrix(:,:)

  ! Intent attributes (for subroutine parameters)
  ! INTEGER, INTENT(IN) :: input_param
  ! REAL, INTENT(OUT) :: output_param
  ! INTEGER, INTENT(INOUT) :: inout_param

  ! COMMON blocks
  COMMON /block1/ a, b, c
  COMMON /block2/ x1, y1, z1

  ! EQUIVALENCE
  EQUIVALENCE (i, j)

  ! DATA statements
  DATA i, j, k /1, 2, 3/
  DATA x /1.0/, y /2.0/

  ! Type definitions
  TYPE :: point
    REAL :: x
    REAL :: y
  END TYPE point

  TYPE :: person
    CHARACTER(LEN=50) :: name
    INTEGER :: age
    REAL :: height
  END TYPE person

  ! Derived type variables
  TYPE(point) :: p1, p2
  TYPE(person) :: john

  ! INTERFACE blocks
  INTERFACE
    SUBROUTINE external_sub(x)
      REAL :: x
    END SUBROUTINE external_sub
  END INTERFACE

  INTERFACE operator(+)
    MODULE PROCEDURE add_points
  END INTERFACE

  ! EXTERNAL and INTRINSIC
  EXTERNAL external_func
  INTRINSIC sin, cos, sqrt

  ! NAMELIST
  NAMELIST /input_data/ i, j, x, y
  NAMELIST /output_data/ matrix, vec

END PROGRAM specification_test

SUBROUTINE test_specifications(n, arr)
  IMPLICIT NONE

  ! Intent attributes
  INTEGER, INTENT(IN) :: n
  REAL, INTENT(INOUT), DIMENSION(:) :: arr

  ! Local variables
  INTEGER :: i
  REAL :: temp
  REAL, ALLOCATABLE :: work(:)

  ! Optional and keyword arguments
  ! INTEGER, OPTIONAL :: opt_param

  ! Assumed shape arrays
  REAL :: local_array(n)

END SUBROUTINE test_specifications

MODULE specification_module
  IMPLICIT NONE

  ! Module-level accessibility
  PRIVATE
  PUBLIC :: public_var, public_func

  ! Module variables
  INTEGER :: public_var
  INTEGER, PRIVATE :: private_var

  ! Module parameters
  INTEGER, PARAMETER :: MODULE_CONST = 42

  ! Module type definitions
  TYPE, PUBLIC :: module_type
    INTEGER :: value
    REAL :: data
  END TYPE module_type

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

SUBROUTINE array_section()
	  REAL, DIMENSION(10, 10) :: a
	  a(:, 1:5) = 0.0
	END SUBROUTINE