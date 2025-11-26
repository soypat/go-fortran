! Test invalid specification syntax
! Phase 1: Only structural errors are detected (program unit structure)
! Phase 2: Specification statement errors will be detected (variable declarations, etc.)

! Invalid: PROGRAM with no name
! Note: The specification statements below won't be parsed, just the structural error
PROGRAM ! ERROR "expected program name"

! Invalid: MODULE with no name
MODULE ! ERROR "expected module name"

! The following errors will only be detected in Phase 2
! (when specification statement parsing is implemented)
! For now, they parse successfully as the statements are collected as tokens

PROGRAM implicit_none_placement
  ! Invalid: IMPLICIT NONE must come before other declarations
  INTEGER :: x
  IMPLICIT NONE  ! ERROR "IMPLICIT NONE must appear before type declarations"

  ! Invalid: Duplicate IMPLICIT NONE
  IMPLICIT NONE  ! ERROR "duplicate IMPLICIT statement"

  ! Invalid: PARAMETER without value
  INTEGER, PARAMETER :: NO_VALUE  ! FUTURE ERROR: PARAMETER requires initialization

  ! Invalid: INTENT outside subroutine/function
  INTEGER, INTENT(IN) :: bad_intent  ! FUTURE ERROR: INTENT only valid for dummy arguments

  ! Invalid: Mismatched array dimensions
  REAL, DIMENSION(10, 20) :: matrix
  ! matrix = 0.0  ! Would need dimensions to match

  ! Invalid: Multiple type specifications
  INTEGER REAL :: confused  ! FUTURE ERROR: "expected ::"

  ! Invalid: ALLOCATABLE with explicit shape
  REAL, ALLOCATABLE :: bad_alloc(10)  ! FUTURE ERROR: ALLOCATABLE must be deferred shape

  ! Invalid: Missing required components
  TYPE incomplete_type
    ! FUTURE ERROR: empty type definition
  END TYPE

  ! Invalid: TYPE inside executable region would be Phase 2 error

END PROGRAM implicit_none_placement

SUBROUTINE intent_errors(x, y, z)
  ! Invalid: INTENT on non-dummy argument
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: x
  INTEGER :: y
  INTEGER, INTENT(OUT) :: local_var  ! FUTURE ERROR: INTENT on local variable
  REAL :: z

  ! Invalid: Undeclared variable in INTENT
  ! INTEGER, INTENT(IN) :: undeclared  ! Would be caught in Phase 2

END SUBROUTINE intent_errors

MODULE bad_accessibility
  IMPLICIT NONE

  ! Invalid: PUBLIC/PRIVATE on undeclared entity
  PUBLIC :: nonexistent_var  ! FUTURE ERROR: entity not declared

  ! Invalid: Conflicting accessibility
  PRIVATE :: conflict_var
  PUBLIC :: conflict_var  ! FUTURE ERROR: conflicting accessibility

  INTEGER :: conflict_var

END MODULE bad_accessibility

FUNCTION bad_result() RESULT(res)
  IMPLICIT NONE
  ! Invalid: RESULT variable not declared
  ! REAL :: res  ! Missing this would be Phase 2 error

  ! Invalid: RESULT name same as function name
  ! REAL :: bad_result  ! FUTURE ERROR: RESULT should be different from function name

END FUNCTION bad_result

! Note: Complex CONTAINS errors (like duplicate CONTAINS or declarations after CONTAINS)
! will be properly validated in Phase 2 when the specification part is fully parsed.
! For Phase 1, these structural issues are collected as tokens and not validated.
