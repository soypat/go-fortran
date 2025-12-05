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
      CALL LEVEL13()
      CALL LEVEL14()
      CALL LEVEL15()
      CALL LEVEL16()
      CALL LEVEL17()
      ! CALL LEVEL18() ! TODO: ALLOCATE/DEALLOCATE not yet implemented
      CALL LEVEL19()
      CALL LEVEL20()
      CALL LEVEL21()
      CALL LEVEL23()
      CALL LEVEL24()
      CALL LEVEL25()
      CALL LEVEL26()
      CALL LEVEL27()
      CALL LEVEL28()
      ! CALL LEVEL29() ! TODO: Cray pointers not yet implemented
      STOP 0
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
! LEVEL 13: Loop control (CYCLE, EXIT, CONTINUE)
! ==============================================================================
      SUBROUTINE LEVEL13()
          INTEGER :: i, sum_val, count
          INTEGER, DIMENSION(10) :: arr

          ! Initialize array with some negative values
          arr(1) = 5
          arr(2) = -3
          arr(3) = 7
          arr(4) = -1
          arr(5) = 9
          arr(6) = 2
          arr(7) = -4
          arr(8) = 6
          arr(9) = 8
          arr(10) = 1

          ! Test CYCLE - skip negative values
          sum_val = 0
          DO i = 1, 10
              IF (arr(i) .LT. 0) CYCLE
              sum_val = sum_val + arr(i)
          END DO
          PRINT *, 'LEVEL 13: sum of positive =', sum_val

          ! Test EXIT - break when value > 7
          count = 0
          DO i = 1, 10
              IF (arr(i) .GT. 7) EXIT
              count = count + 1
          END DO
          PRINT *, 'LEVEL 13: count before >7 =', count

          ! Test CONTINUE - no-op statement
          DO i = 1, 3
              CONTINUE
              count = i
          END DO
          PRINT *, 'LEVEL 13: last count =', count
      END SUBROUTINE LEVEL13

! ==============================================================================
! LEVEL 14: Simple GOTO and Labels
! ==============================================================================
      SUBROUTINE LEVEL14()
          INTEGER :: x, y

          ! Test unconditional GOTO - jump over assignment
          GOTO 100
          x = 999  ! This should be skipped
100       CONTINUE
          x = 10

          ! Test conditional GOTO - simple control flow
          y = 5
          IF (y .EQ. 5) GOTO 200
          y = 999  ! This should be skipped
200       CONTINUE

          PRINT *, 'LEVEL 14: x =', x, ', y =', y
      END SUBROUTINE LEVEL14

! LEVEL 15: SELECT CASE Statements
      SUBROUTINE LEVEL15()
          INTEGER :: choice, result

          ! Test simple SELECT CASE with single values
          choice = 2
          SELECT CASE (choice)
          CASE (1)
              result = 10
          CASE (2)
              result = 20
          CASE (3)
              result = 30
          CASE DEFAULT
              result = 0
          END SELECT
          PRINT *, 'LEVEL 15: choice =', choice, ', result =', result

          ! Test SELECT CASE with multiple values in one CASE
          choice = 5
          SELECT CASE (choice)
          CASE (1, 2, 3)
              result = 100
          CASE (4, 5, 6)
              result = 200
          CASE DEFAULT
              result = 999
          END SELECT
          PRINT *, 'LEVEL 15: choice =', choice, ', result =', result

          ! Test CASE DEFAULT
          choice = 99
          SELECT CASE (choice)
          CASE (1)
              result = 10
          CASE (2)
              result = 20
          CASE DEFAULT
              result = 777
          END SELECT
          PRINT *, 'LEVEL 15: choice =', choice, ', result =', result
      END SUBROUTINE LEVEL15

! LEVEL 16: String Intrinsics and Substrings
      SUBROUTINE LEVEL16()
          CHARACTER(LEN=20) :: str1, str2, str3
          INTEGER :: len_val, len_trim_val, index_val

          ! Test LEN intrinsic
          str1 = 'Hello'
          len_val = LEN(str1)
          PRINT *, 'LEVEL 16: LEN =', len_val

          ! Test LEN_TRIM intrinsic
          len_trim_val = LEN_TRIM(str1)
          PRINT *, 'LEVEL 16: LEN_TRIM =', len_trim_val

          ! Test TRIM intrinsic
          str2 = TRIM(str1)
          PRINT *, 'LEVEL 16: TRIM =', str2

          ! Test INDEX intrinsic
          str1 = 'Hello World'
          index_val = INDEX(str1, 'World')
          PRINT *, 'LEVEL 16: INDEX =', index_val

          ! Test ADJUSTL intrinsic
          str1 = '   Left'
          str2 = ADJUSTL(str1)
          PRINT *, 'LEVEL 16: ADJUSTL =', str2

          ! Test ADJUSTR intrinsic
          str1 = 'Right   '
          str3 = ADJUSTR(str1)
          PRINT *, 'LEVEL 16: ADJUSTR =', str3

          str1 = 'abcdef'
          str3 = str1(2:4)
          str1(2:3) = 'z'
          PRINT *, 'LEVEL 16: str3 =', str3
      END SUBROUTINE LEVEL16

! LEVEL 17: Array Intrinsics
      SUBROUTINE LEVEL17()
          INTEGER, DIMENSION(3, 4) :: matrix
          INTEGER, DIMENSION(5) :: vector
          INTEGER :: size_total, size_dim1, size_dim2
          INTEGER :: lb, ub

          ! Test SIZE intrinsic (total elements)
          size_total = SIZE(matrix)
          PRINT *, 'LEVEL 17: SIZE(matrix) =', size_total

          ! Test SIZE intrinsic with dimension
          size_dim1 = SIZE(matrix, 1)
          PRINT *, 'LEVEL 17: SIZE(matrix,1) =', size_dim1

          size_dim2 = SIZE(matrix, 2)
          PRINT *, 'LEVEL 17: SIZE(matrix,2) =', size_dim2

          ! Test SIZE on 1D array
          size_total = SIZE(vector)
          PRINT *, 'LEVEL 17: SIZE(vector) =', size_total

          ! Test LBOUND intrinsic
          lb = LBOUND(matrix, 1)
          PRINT *, 'LEVEL 17: LBOUND(matrix,1) =', lb

          lb = LBOUND(matrix, 2)
          PRINT *, 'LEVEL 17: LBOUND(matrix,2) =', lb

          ! Test UBOUND intrinsic
          ub = UBOUND(matrix, 1)
          PRINT *, 'LEVEL 17: UBOUND(matrix,1) =', ub

          ub = UBOUND(matrix, 2)
          PRINT *, 'LEVEL 17: UBOUND(matrix,2) =', ub
      END SUBROUTINE LEVEL17

! LEVEL 18: ALLOCATE and DEALLOCATE
      SUBROUTINE LEVEL18()
          INTEGER, ALLOCATABLE, DIMENSION(:) :: vec
          INTEGER, ALLOCATABLE, DIMENSION(:,:) :: mat

          ! Allocate 1D array
          ALLOCATE(vec(5))
          vec(1) = 10
          vec(2) = 20
          vec(3) = 30
          PRINT *, 'LEVEL 18: vec(1) =', vec(1)
          PRINT *, 'LEVEL 18: vec(3) =', vec(3)
          PRINT *, 'LEVEL 18: SIZE(vec) =', SIZE(vec)

          ! Allocate 2D array
          ALLOCATE(mat(2, 3))
          mat(1, 1) = 100
          mat(2, 3) = 200
          PRINT *, 'LEVEL 18: mat(1,1) =', mat(1, 1)
          PRINT *, 'LEVEL 18: mat(2,3) =', mat(2, 3)
          PRINT *, 'LEVEL 18: SIZE(mat) =', SIZE(mat)

          ! Deallocate
          DEALLOCATE(vec)
          DEALLOCATE(mat)
          PRINT *, 'LEVEL 18: Arrays deallocated'
      END SUBROUTINE LEVEL18

      ! LEVEL 19: COMMON Blocks
      SUBROUTINE LEVEL19()
          CALL SET_COMMON_VALUES()
          CALL PRINT_COMMON_VALUES()
      END SUBROUTINE LEVEL19

      ! Helper subroutines for LEVEL19
      SUBROUTINE SET_COMMON_VALUES()
          INTEGER :: x, y
          REAL :: z
          COMMON /SHARED/ x, y, z

          x = 42
          y = 99
          z = 3.14159
      END SUBROUTINE SET_COMMON_VALUES

      SUBROUTINE PRINT_COMMON_VALUES()
          INTEGER :: x, y
          REAL :: Z
          COMMON /SHARED/ x, y, Z

          PRINT *, 'LEVEL 19: x =', x
          PRINT *, 'LEVEL 19: y =', y
          PRINT *, 'LEVEL 19: z =', Z
      END SUBROUTINE PRINT_COMMON_VALUES

      ! LEVEL 20: DATA Statements
      SUBROUTINE LEVEL20()
          INTEGER :: a, b, c
          REAL :: x, y
          DATA a, b, c / 10, 20, 30 /
          DATA x, y / 3.14, 2.71 /

          PRINT *, 'LEVEL 20: a =', a
          PRINT *, 'LEVEL 20: b =', b
          PRINT *, 'LEVEL 20: c =', c
          PRINT *, 'LEVEL 20: x =', x
          PRINT *, 'LEVEL 20: y =', y
      END SUBROUTINE LEVEL20

      ! LEVEL 21: Advanced GOTO (Arithmetic IF and Computed GOTO)
      SUBROUTINE LEVEL21()
          INTEGER :: x, choice

          ! Test Arithmetic IF: IF (expr) neg, zero, pos
          x = -5
          IF (x) 10, 20, 30
10        PRINT *, 'LEVEL 21: x is negative'
          GOTO 40
20        PRINT *, 'LEVEL 21: x is zero'
          GOTO 40
30        PRINT *, 'LEVEL 21: x is positive'

          ! Test Computed GOTO: GO TO (labels) index
40        choice = 2
          GO TO (100, 200, 300), choice
100       PRINT *, 'LEVEL 21: Choice was 1'
          GOTO 400
200       PRINT *, 'LEVEL 21: Choice was 2'
          GOTO 400
300       PRINT *, 'LEVEL 21: Choice was 3'

400       CONTINUE
      END SUBROUTINE LEVEL21

      ! LEVEL 22: STOP Statement
      SUBROUTINE LEVEL22()

      END SUBROUTINE LEVEL22

      ! LEVEL 23: PARAMETER Constants
      SUBROUTINE LEVEL23()
          INTEGER, PARAMETER :: MAX_SIZE = 100
          REAL, PARAMETER :: PI = 3.14159
          REAL, PARAMETER :: TAU = 2.0 * PI

          PRINT *, 'LEVEL 23: MAX_SIZE =', MAX_SIZE
          PRINT *, 'LEVEL 23: PI =', PI
          PRINT *, 'LEVEL 23: TAU =', TAU
      END SUBROUTINE LEVEL23

! ==============================================================================
! LEVEL 24: Array Constructors
! ==============================================================================
      SUBROUTINE LEVEL24()
          INTEGER, DIMENSION(3) :: vec1
          INTEGER, DIMENSION(5) :: vec2

          ! Array constructor with explicit values
          vec1 = (/ 10, 20, 30 /)

          PRINT *, 'LEVEL 24: vec1(1) =', vec1(1)
          PRINT *, 'LEVEL 24: vec1(2) =', vec1(2)
          PRINT *, 'LEVEL 24: vec1(3) =', vec1(3)

          ! Array constructor with multiple values
          vec2 = (/ 100, 200, 300, 400, 500 /)

          PRINT *, 'LEVEL 24: vec2(1) =', vec2(1)
          PRINT *, 'LEVEL 24: vec2(5) =', vec2(5)
      END SUBROUTINE LEVEL24

! ==============================================================================
! LEVEL 25: KIND Parameters
! ==============================================================================
      SUBROUTINE LEVEL25()
          ! Test various KIND parameters
          INTEGER(KIND=1) :: i1
          INTEGER(KIND=2) :: i2
          INTEGER(KIND=4) :: i4
          INTEGER(KIND=8) :: i8
          REAL(KIND=4) :: r4
          REAL(KIND=8) :: r8

          ! Assign values
          i1 = 127           ! Max value for int8
          i2 = 32767         ! Max value for int16
          i4 = 2147483647    ! Max value for int32
          i8 = 9223372036854775807_8  ! Max value for int64
          r4 = 3.14159
          r8 = 3.141592653589793D0

          PRINT *, 'LEVEL 25: i1 =', i1
          PRINT *, 'LEVEL 25: i2 =', i2
          PRINT *, 'LEVEL 25: i4 =', i4
          PRINT *, 'LEVEL 25: i8 =', i8
          PRINT *, 'LEVEL 25: r4 =', r4
          PRINT *, 'LEVEL 25: r8 =', r8
      END SUBROUTINE LEVEL25

! ==============================================================================
! LEVEL 26: BOZ Literals and Double Precision
! ==============================================================================
      SUBROUTINE LEVEL26()
          INTEGER :: hex_val, oct_val, bin_val
          REAL(KIND=8) :: d1, d2, d4

          ! Test BOZ literals (Binary/Octal/heXadecimal)
          hex_val = INT(Z'FF')           ! 255 in hexadecimal
          oct_val = INT(O'377')          ! 255 in octal
          bin_val = INT(B'11111111')     ! 255 in binary

          PRINT *, 'LEVEL 26: hex_val =', hex_val
          PRINT *, 'LEVEL 26: oct_val =', oct_val
          PRINT *, 'LEVEL 26: bin_val =', bin_val

          ! Test double precision literals (D exponent)
          d1 = 1.0D0                ! 1.0
          d2 = 1.23D+02             ! 123.0
          d4 = 2.718281828D0        ! e constant

          PRINT *, 'LEVEL 26: d1 =', d1
          PRINT *, 'LEVEL 26: d2 =', d2
          PRINT *, 'LEVEL 26: d4 =', d4
      END SUBROUTINE LEVEL26

      SUBROUTINE LEVEL27()
          ! Test patterns that cause transpilation errors in g2efile.f90

          ! Issue 1: Inline comment in PARAMETER (line 10621)
          INTEGER, PARAMETER :: ncomp = 5  ! number of params

          ! Issue 2: D0 in division expression in PARAMETER (line 32308)
          REAL(KIND=8), PARAMETER :: factor = 1.0D0 / 86400.0e0

          ! Issue 3: D0 in function call in PARAMETER (lines 32314, 33736)
          REAL(KIND=8), PARAMETER :: root3 = SQRT(3.0D0)
          REAL(KIND=8), PARAMETER :: PI = 4.D0 * ATAN(1.D0)

          REAL(KIND=8) :: result

          result = factor * root3 * DBLE(ncomp)
          PRINT *, 'LEVEL 27: ncomp =', ncomp
          PRINT *, 'LEVEL 27: factor =', factor
          PRINT *, 'LEVEL 27: root3 =', root3
          PRINT *, 'LEVEL 27: PI =', PI
          PRINT *, 'LEVEL 27: result =', result
      END SUBROUTINE LEVEL27

      SUBROUTINE LEVEL28()
          ! Test COMMON block arrays with initialization
          REAL :: YQR(256), SUMXRQ(512), YMNRT(3)
          REAL :: MATRIX(10,20)
          INTEGER :: COUNTS(100)
          COMMON/HOLDRT/YQR,SUMXRQ,YMNRT,MATRIX
          COMMON/STATS/COUNTS

          PRINT *, 'LEVEL 28: COMMON block arrays initialized'
          YQR(1) = 1.5
          SUMXRQ(512) = 99.9
          YMNRT(2) = 3.14
          MATRIX(5,10) = 42.5
          COUNTS(50) = 42
          PRINT *, 'LEVEL 28: YQR(1) =', YQR(1)
          PRINT *, 'LEVEL 28: SUMXRQ(512) =', SUMXRQ(512)
          PRINT *, 'LEVEL 28: YMNRT(2) =', YMNRT(2)
          PRINT *, 'LEVEL 28: MATRIX(5,10) =', MATRIX(5,10)
          PRINT *, 'LEVEL 28: COUNTS(50) =', COUNTS(50)
      END SUBROUTINE LEVEL28


    SUBROUTINE LEVEL29()
        ! Test advanced features: DIMENSION, MALLOC, DATA with hex, labeled DO
      !   DOUBLE PRECISION ,ALLOCATABLE,  DIMENSION(:) :: AA
      !   INTEGER   ,ALLOCATABLE,  DIMENSION(:) :: II
      !   LOGICAL   ,ALLOCATABLE,  DIMENSION(:) :: LL
        IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L),INTEGER (I)
        POINTER (NPAA,AA(1)), (NPII,II(1)), (NPLL,LL(1)) ! cray style pointer, implicit initialization.
        INTEGER :: N, M, MAXMUM, MAXDM1, MAXDEF
        INTEGER :: MAT(2,2)
        CHARACTER A, B
        CHARACTER, DIMENSION(4) :: C
        EQUIVALENCE (A,B, MAT(1,2))
        EQUIVALENCE (C, MAT(1,1))
        ! Initialize with hex values (Cray-style hex literals)
        DOUBLEPRECISION          :: DEFALT
        INTEGER,DIMENSION(2)     :: I_DEFALT
        DATA I_DEFALT(1) /Z'7777777'/
        DATA I_DEFALT(2) /Z'7777777'/
        EQUIVALENCE ( DEFALT, I_DEFALT )
        PRINT *, 'LEVEL 29: Advanced features test'
        MAT(1,2) = 64;
        ! Test MALLOC intrinsic
        MAXDM1 = 100
        NPAA = MALLOC(MAXDM1 * 8)
        IF( NPAA .EQ. 0 ) THEN
           STOP 69
        ENDIF
        NPII = NPAA
        NPLL = NPII
        M = 1
        ! Initialize array using labeled DO loop
         MAXDEF=MIN(200000,MAXDM1)
         DO  900 M=1,MAXDEF,32768
         MAXMUM=MIN(M+32767,MAXDEF)
         DO  800 N=M,MAXMUM
        !  AA(N)=DEFALT
800      END DO
900      END DO
        PRINT *, 'CHAR A,B:', A, B
        PRINT *, 'CHAR C', C
        PRINT *, 'LEVEL 29: AA(2) ', AA(2), 'MAT(1,2)', MAT(1,2)
        PRINT *, 'LEVEL 29: Initialized', MAXMUM - M + 1, 'elements'
    END SUBROUTINE LEVEL29

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
