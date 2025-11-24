! Test file for KIND parameter support
PROGRAM test_kind
    ! INTEGER with KIND selector (F90 syntax)
    INTEGER(KIND=8) :: bigint
    INTEGER(8) :: shorthand

    ! REAL with F77 kind syntax
    REAL*8 :: dbl1
    REAL*4 :: sgl1

    ! REAL with F90 KIND syntax
    REAL(KIND=8) :: dbl2
    REAL(4) :: sgl2

    ! CHARACTER with length as expression
    CHARACTER(LEN=80) :: str1
    CHARACTER(80) :: str2
    CHARACTER*10 :: str3

    ! Test assignment
    bigint = 123456789
    dbl1 = 3.14159d0
END PROGRAM test_kind

! Function with KIND in result type
REAL*8 FUNCTION compute_double(x)
    REAL*8 :: x
    compute_double = x * 2.0d0
END FUNCTION

! Subroutine with KIND parameters
SUBROUTINE process(n, arr)
    INTEGER(KIND=4), INTENT(IN) :: n
    REAL(KIND=8), DIMENSION(n), INTENT(INOUT) :: arr
    INTEGER :: i

    DO i = 1, n
        arr(i) = arr(i) * 2.0d0
    END DO
END SUBROUTINE
