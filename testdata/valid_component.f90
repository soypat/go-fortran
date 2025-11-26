SUBROUTINE test
  TYPE :: person
    CHARACTER(LEN=50) :: name
    INTEGER :: age
    REAL :: height
    REAL (kind=8) :: money
  END TYPE person
    TYPE(person) :: p
    p%age = 30
    END SUBROUTINE