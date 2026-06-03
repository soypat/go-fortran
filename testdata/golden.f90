! golden.f90 - Progressive feature test file
! Each level is a separate subroutine for easier incremental testing

      PROGRAM GOLDEN
      IMPLICIT NONE
      INTEGER :: lv62y

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
      CALL LEVEL18()
      CALL LEVEL19()
      CALL LEVEL20()
      CALL LEVEL21()
      CALL LEVEL23()
      CALL LEVEL24()
      CALL LEVEL25()
      CALL LEVEL26()
      CALL LEVEL27()
      CALL LEVEL28()
      CALL LEVEL29()
      CALL LEVEL30()
      CALL LEVEL31()
      CALL LEVEL32()
      CALL LEVEL33()
      CALL LEVEL34()
      CALL LEVEL35()
      CALL LEVEL36()
      CALL LEVEL37()
      CALL LEVEL38()
      CALL LEVEL39()
      CALL LEVEL40()
      CALL LEVEL41()
      CALL LEVEL42()
      CALL LEVEL43()
      CALL LEVEL44()
      CALL LEVEL45()
      CALL LEVEL46()
      CALL LEVEL47()
      CALL LEVEL48()
      CALL LEVEL49()
      CALL LEVEL50()
      CALL LEVEL51()
      CALL LEVEL52()
      CALL LEVEL53()
      CALL LEVEL54()
      CALL LEVEL55()
      CALL LEVEL56()
      CALL LEVEL57()
      CALL LEVEL58()
      CALL LEVEL59()
      CALL LEVEL60()
      CALL LEVEL61()
      CALL LEVEL62(42, lv62y)
      CALL LEVEL63()
      CALL LEVEL64()
      CALL LEVEL65()
      CALL LEVEL66()
      CALL LEVEL67()
      CALL LEVEL68()
      CALL LEVEL69()
      CALL LEVEL70()
      CALL LEVEL71()
      STOP 0
      CALL EXIT(0)
      CALL EXIT
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
          CHARACTER :: a
          a = 'a' ! letter a
          i = 42
          x = 3.14159
          flag = .TRUE.
          message = 'Variables assigned'
          PRINT *, 'LEVEL 2: i =', i, ', x =', x
          PRINT *, 'LEVEL 2: flag =', flag
          PRINT *, 'LEVEL 2:', message
          PRINT *, 'LEVEL 2:', a, a ! Check Spacing between single characters.
          PRINT *, a,a,a,i,a ! Check spacing, varied
          PRINT *, i,a,i,a
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
          REAL :: x, test
          LOGICAL :: flag

          i = 42
          x = 3.14159
          test = 0.
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
          REAL :: x=1,y=2

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
          CALL MULDST(x,y)
          PRINT *, 'LEVEL 7: x=x*y', x, y
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
          LOGICAL :: flag, cond1, cond2, cond3, cond4

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
          ! .NOT. lower precedence than relational: .NOT.x.LT.y = .NOT.(x.LT.y)
          cond4 = .NOT.x.LT.y

          PRINT *, 'LEVEL 10: cond1 =', cond1, ', cond2 =', cond2
          PRINT *, 'LEVEL 10: cond3 =', cond3, ', cond4=', cond4
      END SUBROUTINE LEVEL10

! ==============================================================================
! LEVEL 11: Character operations
! ==============================================================================
      SUBROUTINE LEVEL11()
          CHARACTER(LEN=10) :: str1, str2
          CHARACTER(LEN=20) :: str3
          CHARACTER(3) :: sufx(2)
          INTEGER :: idx

          str1 = 'Hello'
          str2 = 'World'
          str3 = str1 // ' ' // str2

          sufx(1) = 'Go!'
          sufx(2) = 'Hi!'
          idx = 1
          str3 = str1(1:5) // sufx(idx)

          PRINT *, 'LEVEL 11: concatenation:', str3
          PRINT *, 'LEVEL 11: substr+array:', str3
      END SUBROUTINE LEVEL11

! ==============================================================================
! LEVEL 12: Intrinsic functions
! ==============================================================================
      SUBROUTINE LEVEL12()
          REAL :: angle, sin_val, cos_val, abs_val, log_val, v1(3),v2(3), dot_val,zabs,zreal,zimag
          DOUBLEPRECISION :: zzabs, zzreal, zzimag
          COMPLEX :: z, z2
          DOUBLECOMPLEX :: zz
          INTEGER :: i, j, k, max_val, min_val
          zz = DCMPLX(3.0D0, 1.0D0)
          do i = 1, 3
            v1(i) = i
            v2(i) = i
          end do
          i = 11
          j = 52
          k = 84
          z = COMPLEX(1, 2)

          angle = 0.5
          sin_val = SIN(angle)
          cos_val = COS(angle)
          abs_val = ABS(-5.5)
          max_val = MAX(i, j, k)
          min_val = MIN(10, 20, 5)
          log_val = 2.*LOG(angle)
          dot_val = DOT_PRODUCT(v1,v2)
          zabs = CABS(z)
          zreal = REAL(z)
          zimag = AIMAG(z)
          zzabs = CDABS(zz)
          zzreal = DREAL(zz)
          zzimag = DIMAG(zz)

        !   PRINT *, 'LEVEL 12: LOG(0.5) = ', log_val
          PRINT *, 'LEVEL 12: SIN(0.5) =', sin_val
          PRINT *, 'LEVEL 12: COS(0.5) =', cos_val
          PRINT *, 'LEVEL 12: ABS(-5.5) =', abs_val
          PRINT *, 'LEVEL 12: MAX =', max_val, ', MIN =', min_val
          PRINT *, 'LEVEL 12: dot product:', dot_val
          PRINT *, 'LEVEL 12: z=1+2i, ZABS,ZREAL,ZIMAG ', zabs, zreal, zimag
          PRINT *, 'LEVEL 12: zz=3+1i, ZZABS,ZZREAL,ZZIMAG',zzabs,zzreal,zzimag
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
          ! Test goto ENDDO
          DO 30 i = 1, 2
            PRINT *, 'LEVEL 13: do,goto,end do', i
            goto 30
            PRINT *, 'LEVEL 13: not printed', i
30        END DO
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
          INTEGER :: a, b, c, i
          REAL :: x, y, vec1(3)
          CHARACTER(3) :: REPEAT(2)
          CHARACTER(2) :: MTHS(3)
          DATA a, b, c / 10, 20, 30 /
          DATA x, y / 3.14, 2.71 /
          DATA REPEAT/2*"REP"/
          DATA vec1 /2*1.2, 2.0/
          DATA (MTHS(i),i=1,3)/'JA','FE','MA'/

          PRINT *, 'LEVEL 20: a =', a
          PRINT *, 'LEVEL 20: b =', b
          PRINT *, 'LEVEL 20: c =', c
          PRINT *, 'LEVEL 20: x =', x
          PRINT *, 'LEVEL 20: y =', y
          PRINT *, 'LEVEL 20', REPEAT(1), REPEAT(2)
          PRINT *, 'LEVEL 20:',vec1(1), vec1(2), vec1(3)
          PRINT *, 'LEVEL 20: implied-do', MTHS(1), MTHS(2), MTHS(3)
      END SUBROUTINE LEVEL20

      ! LEVEL 21: Advanced GOTO (Arithmetic IF and Computed GOTO)
      SUBROUTINE LEVEL21()
          INTEGER :: x, choice
          REAL :: test
          test = 0.
          ! Test Arithmetic IF: IF (expr) neg, zero, pos
          x = -5
          IF (x) 10, 20, 30
10        PRINT *, 'LEVEL 21: x is negative'
          GOTO 90
20        PRINT *, 'LEVEL 21: x is zero'
          GOTO 90
30        PRINT *, 'LEVEL 21: x is positive'
          
          IF (test-.1e-13) 40,50,60
40        PRINT *, 'LEVEL 21: test negative'
          GOTO 90
50        PRINT *, 'LEVEL 21: test zero'
          GOTO 90
60        PRINT *, 'LEVEL 21: test positive'

          ! Test Computed GOTO: GO TO (labels) index
90        choice = 2
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
          INTEGER, DIMENSION(3) :: vec1, vec3
          INTEGER, DIMENSION(5) :: vec2

          ! Array constructor with explicit values
          vec1 = (/ 10, 20, 30 /)

          PRINT *, 'LEVEL 24: vec1(1) =', vec1(1)
          PRINT *, 'LEVEL 24: vec1(2) =', vec1(2)
          PRINT *, 'LEVEL 24: vec1(3) =', vec1(3)

          ! Array constructor with multiple values
          vec2 = (/ 100, 200, 300, 400, 500 /)
          vec3 = (/ 20, 30, 40/)
          PRINT *, 'LEVEL 24: vec2(1) =', vec2(1)
          PRINT *, 'LEVEL 24: vec2(5) =', vec2(5)
          if (ALL(vec1 == vec3)) then
            PRINT *, 'LEVEL 24: vec eq'
          else 
            PRINT *, 'LEVEL 24: vec neq'
          end if
          if (ALL(vec1 == (/10, 20, 30/))) then
            PRINT *, 'LEVEL 24: vecinline eq'
          else
            PRINT *, 'LEVEL 24: vecinline neq'
          endif
      END SUBROUTINE LEVEL24

! ==============================================================================
! LEVEL 25: KIND Parameters
! ==============================================================================
      SUBROUTINE LEVEL25()
          ! Test various KIND parameters
          INTEGER, PARAMETER :: int32 = 4
          ! Test DOUBLEPRECISION and REAL PARAMETER constants in output list
          ! (exercises InferType + Eval of float PARAMETER init)
          REAL(KIND=8), PARAMETER :: dp_neg = 2.71828182845904D0
          REAL(KIND=4), PARAMETER :: sp_neg = 2.71828
          INTEGER(int32) :: n_lapack
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
          N_LAPACK = 42

          PRINT *, 'LEVEL 25: i1 =', i1
          PRINT *, 'LEVEL 25: i2 =', i2
          PRINT *, 'LEVEL 25: i4 =', i4
          PRINT *, 'LEVEL 25: i8 =', i8
          PRINT *, 'LEVEL 25: r4 =', r4
          PRINT *, 'LEVEL 25: r8 =', r8
          PRINT *, 'LEVEL 25: n_lapack =', n_lapack, ' int32=', int32
          PRINT *, 'LEVEL 25: dp_neg =', dp_neg
          PRINT *, 'LEVEL 25: sp_neg =', sp_neg

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
          IMPLICIT REAL (A-Z)
          REAL :: YQR(256), SUMXRQ(512), YMNRT(3)
          REAL :: MATRIX(10,20)
          INTEGER :: COUNTS(100)
          COMMON/HOLDRT/YQR,SUMXRQ,YMNRT,MATRIX
          COMMON/STATS/COUNTS,ALPHC(2,2)

          PRINT *, 'LEVEL 28: COMMON block arrays initialized'
          YQR(1) = 1.5
          SUMXRQ(512) = 99.9
          YMNRT(2) = 3.14
          MATRIX(5,10) = 42.5
          COUNTS(50) = 42
          ALPHC(1,1)=1.
          ALPHC(1,2)=2.
          ALPHC(2,1)=3.
          ALPHC(2,2)=4.
          PRINT *, 'LEVEL 28: YQR(1) =', YQR(1)
          PRINT *, 'LEVEL 28: SUMXRQ(512) =', SUMXRQ(512)
          PRINT *, 'LEVEL 28: YMNRT(2) =', YMNRT(2)
          PRINT *, 'LEVEL 28: MATRIX(5,10) =', MATRIX(5,10)
          PRINT *, 'LEVEL 28: COUNTS(50) =', COUNTS(50)
          PRINT *, 'LEVEL 28: IMPLICIT ALPHC=', ALPHC(1,1), ALPHC(1,2), ALPHC(2,1), ALPHC(2,2)
      END SUBROUTINE LEVEL28

    SUBROUTINE LEVEL29()
        INTEGER(1), PARAMETER :: firstLetter = 97 ! 97 is ascii for 'a'
        INTEGER :: letters
        INTEGER(1) :: MAT(2,2)
        INTEGER :: MAT4(1,2)
        CHARACTER(4) A
        EQUIVALENCE (A, MAT(1,1), MAT4(1,2))
        DATA letters /Z'61626364'/
        ! SECOND EQUIVALENCE
        REAL :: F
        INTEGER :: N
        EQUIVALENCE(F, N)
        MAT(1,1) = firstLetter
        MAT(1,2) = firstLetter+1
        MAT(2,1) = firstLetter+2
        MAT(2,2) = firstLetter+3
        PRINT *, 'LEVEL 29: byte mat ', A
        MAT4(1,2) = letters
        PRINT *, 'LEVEL 29: uint32 mat ', A
        F = 1
        PRINT *, 'LEVEL 29: linked float=1,int', F, N
        N = 1109917696 ! is 42 in floating point land.
        PRINT *, 'LEVEL 29: linked float,int=1109917696', F, N
    END SUBROUTINE LEVEL29
    SUBROUTINE LEVEL30() ! EQUIVALENCE playaround.
        CHARACTER :: A, B
        CHARACTER, DIMENSION(4) :: C
        INTEGER :: MAT(2,2)
        DOUBLEPRECISION          :: DEFALT
        INTEGER,DIMENSION(2)     :: I_DEFALT
        DATA I_DEFALT(1) /Z'7777777'/
        DATA I_DEFALT(2) /Z'7777777'/
        EQUIVALENCE ( DEFALT, I_DEFALT )
        EQUIVALENCE (C(1), MAT(1,1))
        EQUIVALENCE (A, B, MAT(1,2))
        MAT(1,1) = 64 ! Affect C.
        MAT(1,2) = 97 ! Affect A and B.
        PRINT *, 'LEVEL 30: CHAR A,B:', A, B
        PRINT *, 'LEVEL 30: CHAR C:', C(1)
        PRINT *, 'LEVEL 30: DEFALT', DEFALT, I_DEFALT(1), I_DEFALT(2)
    END SUBROUTINE LEVEL30
    SUBROUTINE LEVEL31()
        IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L),INTEGER (I,K)
        PARAMETER (K=16)
        POINTER (NPAA, AA(1)), (NPII, II(1))
        INTEGER :: M, INITS=0
        NPAA = MALLOC(K*8)
        if (NPAA.EQ.0) THEN
            STOP 69
        ENDIF
        NPII = NPAA
        DO 900 M=1,K,2
            INITS = INITS + 1
            AA(M) = INITS
900     END DO
        PRINT *, 'LEVEL 31: INITS', INITS 
        PRINT *, 'LEVEL 31: AA(1),AA(2),AA(3),AA(4)',AA(1),AA(2),AA(3),AA(4)
    END SUBROUTINE LEVEL31
    SUBROUTINE LEVEL32()
        CHARACTER(LEN=10) :: A(2,2)
        CHARACTER(3), DIMENSION(2) :: B=(/'ABC', 'CBA'/)
        A(1,1) = 'ABC'
        A(1,2) = 'DEFGH'
        A(2,1) = 'GHI'
        A(2,2) = 'JKL'
        PRINT *, 'LEVEL 32:', A(1,1),A(1,2),A(2,1),A(2,2)
        PRINT *, 'LEVEL 32:', B(1), B(2)
        PRINT *, 'LEVEL 32: substr', A(1,2)(1:3)
        B(1:2) = (/'XYZ', 'PQR'/)
        PRINT *, 'LEVEL 32: range assign', B(1), B(2)
    END SUBROUTINE LEVEL32
    SUBROUTINE LEVEL33() ! ARRAY RANGES.
        INTEGER, PARAMETER :: NM=3
        INTEGER :: i,j,range
        DOUBLE PRECISION :: XSN(NM,NM), COF_COM_TOR(NM,NM)
        do i = 1,NM
            do j = 1,NM
                XSN(i,j) = 0.0
                COF_COM_TOR(i,j) = 1.0
            end do
        end do
        XSN(1,1) = 1.0
        COF_COM_TOR(1,1) = 10.0
        COF_COM_TOR(2,2) = 20.0
        COF_COM_TOR(3,3) = 20.0
        range = 2
        XSN(1:range,range:3) = XSN(1:range,range:3) + COF_COM_TOR(1:range,range:3)
        do i = 1,NM
            PRINT *, 'LEVEL 33:', XSN(i,1), XSN(i,2), XSN(i,3)
        end do
    END SUBROUTINE LEVEL33
    SUBROUTINE LEVEL34() ! Statement Function declarations
        INTEGER :: IDX, M
        IDX(M) = (M*(M/2))*2
        REAL VOLUME, RADIUS
        VOLUME(RADIUS) = 4.189*RADIUS**3
        PRINT *, 'LEVEL 34:', IDX(2), IDX(3), VOLUME(23.0)
    END SUBROUTINE LEVEL34

    SUBROUTINE LEVEL35() ! WRITE statement variants
        INTEGER :: m, n,i
        REAL :: x, wh(6)=(/1.,2.,3.,0.5,1.,1.5/)
        m = 3
        n = 5
        x = 2.5
        ! List-directed with string
        WRITE(*,*) "LEVEL 35: Hello from WRITE"
        ! List-directed with multiple values
        WRITE(*,*) m, n, x
        ! List-directed with mixed string and values
        WRITE(*,*) "LEVEL 35: Values:", m, n
        ! Formatted with label (no outputs)
        WRITE(*,220)
        ! Formatted with label (with outputs)
        WRITE(*,230) m, n, x
220     FORMAT('LEVEL 35: Formatted output line')
230     FORMAT('LEVEL35: m=',I3,' n=',I3,' x=',F5.2)
        WRITE(*,240) (wh(i),i=1,n)
        ! Slash is a conctrol character, is basically equivalent to inserting a '\n' byte in the format.
240     FORMAT('LEVEL35: Newline:'/'LEVEL35: WHI/WR =', 6ES12.4)
    END SUBROUTINE LEVEL35

    SUBROUTINE LEVEL36() ! COMMON and EQUIVALENCE mixing
        IMPLICIT REAL (A-H,O-Z)
        REAL :: d1k,d2k,d3k
        COMMON /BLK/d1k,d2k,d3k
        DIMENSION delta(3)
        EQUIVALENCE (d1k, delta)
        delta(1) = 1.0
        delta(2) = 2.0
        delta(3) = 3.0
        PRINT *, 'LEVEL 36: Equiv d=', d1k, d2k, d3k
        CALL BLKINVDECL()
        CALL BLKDECL()
    END SUBROUTINE LEVEL36

    SUBROUTINE LEVEL37() ! File IO: OPEN, WRITE, CLOSE, READ
        INTEGER :: iounit, x, y,  rstat1=-1, rstat2=-1, wstat1=-1, wstat2=-1
        CHARACTER(LEN=20) :: msg
        iounit = 10
        x = 42
        y = 99
        msg = 'Hello File IO'

        ! Create and write to file
        OPEN(UNIT=iounit, FILE='test_io.txt', STATUS='REPLACE', ACTION='WRITE')
        WRITE(iounit, '(A)', IOSTAT=wstat1) msg
        WRITE(iounit, '(I5,I5)', IOSTAT=wstat2) x, y
        CLOSE(UNIT=iounit)

        ! Reopen and read
        x = 0
        y = 0
        OPEN(UNIT=iounit, FILE='test_io.txt', STATUS='OLD', ACTION='READ')
        READ(iounit, '(A)', IOSTAT=rstat1) msg
        READ(iounit, '(I5,I5)',IOSTAT=rstat2) x, y
        CLOSE(UNIT=iounit)

        ! Print results
        PRINT *, 'LEVEL 37: READ BACK', msg, x, y
        PRINT *, 'LEVEL 37: IOSTAT', rstat1, rstat2, wstat1, wstat2
    END SUBROUTINE LEVEL37

! ==============================================================================
! LEVEL 38: Namelist READ/WRITE loopback test
! ==============================================================================
    SUBROUTINE LEVEL38()
        INTEGER :: iounit = 99
        INTEGER :: errCode
        INTEGER :: x, y, z
        REAL :: a, b
        NAMELIST /TESTDATA/ x, y, z, a, b

        ! Set values to write
        x = 10
        y = 20
        z = 30
        a = 1.5
        b = 2.5

        ! Write namelist to file
        OPEN(UNIT=iounit, FILE='test_namelist.txt', STATUS='REPLACE', ACTION='WRITE')
        WRITE(iounit, TESTDATA)
        CLOSE(UNIT=iounit)

        ! Reset values
        x = 0
        y = 0
        z = 0
        a = 0.0
        b = 0.0

        ! Read it back using namelist
        OPEN(UNIT=iounit, FILE='test_namelist.txt', STATUS='OLD', ACTION='READ')
        READ(iounit, TESTDATA, IOSTAT=errCode)
        CLOSE(UNIT=iounit)

        PRINT *, 'LEVEL 38: NAMELIST x,y,z=', x, y, z
        PRINT *, 'LEVEL 38: NAMELIST a,b=', a, b
    END SUBROUTINE LEVEL38

! ==============================================================================
! LEVEL39: READ with END= branch label (EOF handling)
! ==============================================================================
    SUBROUTINE LEVEL39()
        INTEGER :: N
        CHARACTER(LEN=20) :: CARD
        N = 0
        OPEN(39, FILE='test_io.txt', STATUS='OLD', ACTION='READ')
10      READ(39, '(A)', END=20) CARD
        N = N + 1
        GO TO 10
20      CLOSE(39)
        PRINT *, 'LEVEL 39:', N
    END SUBROUTINE LEVEL39

! ==============================================================================
! LEVEL40: Environment intrinsics.
! ==============================================================================
    SUBROUTINE LEVEL40()
        REAL :: t
        CALL SYSTEM("echo 'LEVEL 40: echo from shell'")
        CALL CPU_TIME(t)
    END SUBROUTINE LEVEL40

    SUBROUTINE LEVEL41() ! Variable format WRITE
        CHARACTER(LEN=30) :: fmt_str
        INTEGER :: x
        x = 41
        fmt_str = '(A,I2,A)'
        WRITE(*, fmt_str) 'LEVEL ', x, ': SUCCESS'
    END SUBROUTINE LEVEL41

    SUBROUTINE LEVEL42() ! CHARACTER array as format specifier
        CHARACTER*1 FMT(5)
        INTEGER :: x
        x = 42
        FMT(1) = '('
        FMT(2) = 'I'
        FMT(3) = '2'
        FMT(4) = ')'
        FMT(5) = ' '
        WRITE(*, FMT) x
        PRINT *, 'LEVEL 42: ok'
    END SUBROUTINE LEVEL42

    SUBROUTINE LEVEL43() ! OPEN/READ/INQUIRE with implicitly declared IOSTAT/EXIST/input var
        IMPLICIT INTEGER (I-N), LOGICAL (P)
        OPEN(UNIT=99, FILE='no_such_file_43.txt', STATUS='OLD', IOSTAT=IOERR)
        IF (IOERR .NE. 0) THEN
            PRINT *, 'LEVEL 43: open failed as expected'
        ELSE
            READ(99, *, IOSTAT=IOERR) NVAL
            CLOSE(99)
        END IF
        INQUIRE(FILE='no_such_file_43.txt', EXIST=PEXIST)
        IF (.NOT.PEXIST) THEN
            PRINT *, 'LEVEL 43: file absent as expected'
        END IF
    END SUBROUTINE LEVEL43

    SUBROUTINE LEVEL44() ! REAL(x, KIND=KIND(y)) type conversion with KIND argument
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        INTEGER NVAL
        NVAL = 7
        HRES = REAL(NVAL, KIND=KIND(HRES))
        PRINT *, 'LEVEL 44:', HRES
    END SUBROUTINE LEVEL44

! LEVEL45: Alternate returns
    SUBROUTINE LEVEL45()
        INTEGER :: x
        x = 2
        CALL ALTRSUB(x, *10, *20)
        PRINT *, 'LEVEL 45: normal'
        GOTO 30
10      PRINT *, 'LEVEL 45: alt 1'
        GOTO 30
20      PRINT *, 'LEVEL 45: alt 2'
30      CONTINUE
    END SUBROUTINE LEVEL45

    SUBROUTINE ALTRSUB(n, *, *)
        INTEGER, INTENT(IN) :: n
        IF (n .EQ. 1) RETURN 1
        IF (n .EQ. 2) RETURN 2
        RETURN
    END SUBROUTINE ALTRSUB

! LEVEL46: READ with implied DO loop (array round-trip)
    SUBROUTINE LEVEL46()
        INTEGER :: iounit, i
        REAL :: vals(5), readback(5)
        DO i = 1, 5
            vals(i) = REAL(i) * 1.5
        END DO
        iounit = 46
        OPEN(UNIT=iounit, FILE='test_idl_read.txt', STATUS='REPLACE', ACTION='WRITE')
        WRITE(iounit, *) (vals(i), i=1, 5)
        CLOSE(UNIT=iounit)
        OPEN(UNIT=iounit, FILE='test_idl_read.txt', STATUS='OLD', ACTION='READ')
        READ(iounit, *) (readback(i), i=1, 5)
        CLOSE(UNIT=iounit)
        PRINT *, 'LEVEL 46:', readback(1), readback(2), readback(3), readback(4), readback(5)
    END SUBROUTINE LEVEL46

! LEVEL47: READ with implied DO and END= (EOF branch)
    SUBROUTINE LEVEL47()
        INTEGER :: iounit, i, eof_hit
        REAL :: arr(3)
        eof_hit = 0
        iounit = 47
        OPEN(UNIT=iounit, FILE='test_end_idl.txt', STATUS='REPLACE', ACTION='WRITE')
        WRITE(iounit, *) 10.0, 20.0, 30.0
        CLOSE(UNIT=iounit)
        OPEN(UNIT=iounit, FILE='test_end_idl.txt', STATUS='OLD', ACTION='READ')
        READ(iounit, *, END=10) (arr(i), i=1, 3)
        GOTO 20
10      eof_hit = 1
20      CLOSE(UNIT=iounit)
        PRINT *, 'LEVEL 47:', arr(1), arr(2), arr(3), eof_hit
    END SUBROUTINE LEVEL47

! LEVEL48: PRINT with implied DO loop
    SUBROUTINE LEVEL48()
        INTEGER :: i
        REAL :: arr(4)
        DO i = 1, 4
            arr(i) = REAL(i) * 2.0
        END DO
        PRINT *, 'LEVEL 48:', (arr(i), i=1, 4)
    END SUBROUTINE LEVEL48

! LEVEL50: Derived-type variable declared with DIMENSION attribute; component assignment
    SUBROUTINE LEVEL50()
        IMPLICIT NONE
        TYPE :: point_t
            INTEGER :: x
            INTEGER :: y
        END TYPE point_t
        type( point_t ), dimension( 2 ) :: pts
        pts(1)%x = 10
        pts(1)%y = 20
        pts(2)%x = 30
        pts(2)%y = 40
        PRINT *, 'LEVEL 50:', pts(1)%x, pts(1)%y, pts(2)%x, pts(2)%y
    END SUBROUTINE LEVEL50

! LEVEL49: Power operator (**) inside a boolean comparison (IF and assignment)
    SUBROUTINE LEVEL49()
        IMPLICIT DOUBLE PRECISION (A-H, O-Z)
        DOUBLE PRECISION :: A, B
        LOGICAL :: C
        A = 2.D0
        B = 3.D0
        C = A**2 .GT. B**2
        IF (A**2 - B**2 .GT. 0.D0) THEN
            PRINT *, 'A**2 > B**2'
        ELSE
            PRINT *, 'A**2 <= B**2'
        END IF
    END SUBROUTINE LEVEL49

! LEVEL51: MOVE_ALLOC intrinsic subroutine
    SUBROUTINE LEVEL51()
        INTEGER, ALLOCATABLE :: arr(:), tmp(:)
        ALLOCATE(arr(3))
        arr(1) = 10
        arr(2) = 20
        arr(3) = 30
        ALLOCATE(tmp(5))
        tmp(1) = arr(1)
        tmp(2) = arr(2)
        tmp(3) = arr(3)
        DEALLOCATE(arr)
        CALL MOVE_ALLOC(tmp, arr)
        PRINT *, 'LEVEL 51:', arr(1), arr(2), arr(3)
    END SUBROUTINE LEVEL51

! LEVEL52: READ into COMMON block scalar variable
      SUBROUTINE LEVEL52()
          INTEGER :: n
          COMMON /L52COM/ n
          n = 0
          OPEN(UNIT=52, FILE='test_common_read.txt', STATUS='REPLACE', ACTION='WRITE')
          WRITE(52, '(I5)') 42
          CLOSE(52)
          OPEN(UNIT=52, FILE='test_common_read.txt', STATUS='OLD', ACTION='READ')
          READ(52, '(I5)') n
          CLOSE(52)
          PRINT *, 'LEVEL 52:', n
      END SUBROUTINE LEVEL52

! LEVEL53: Subroutine parameter names must match between signature and body
      SUBROUTINE LEVEL53()
          INTEGER :: N
          N = 7
          CALL L53HELPER(N)
      END SUBROUTINE LEVEL53

      SUBROUTINE L53HELPER(X)
          INTEGER :: X
          PRINT *, 'LEVEL 53:', X
      END SUBROUTINE L53HELPER

! LEVEL54: Multiple unnamed COMMON statements must map to contiguous memory (no overlap)
      SUBROUTINE LEVEL54()
          INTEGER :: a
          REAL :: x
          INTEGER :: b
          COMMON a, x
          COMMON b
          a = 1
          x = 2.5
          b = 3
          PRINT *, 'LEVEL 54:', a, x, b
      END SUBROUTINE LEVEL54

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
      SUBROUTINE MULDST(x,y)
        REAL, INTENT(OUT) :: x
        REAL, INTENT(IN) :: y
        x = y*x
      END SUBROUTINE
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

      SUBROUTINE BLKINVDECL()
        IMPLICIT REAL (A-H,O-Z)
        COMMON /BLK/d3k,d2k,d1k
        PRINT *, 'BLKINVDECL: Equiv d1k,d2k,d3k=', d1k, d2k, d3k
      END SUBROUTINE BLKINVDECL

      SUBROUTINE BLKDECL()
        IMPLICIT REAL (A-H,O-Z)
        COMMON /BLK/d1k,d2k,d3k
        DIMENSION delta(3)
        EQUIVALENCE (d1k, delta)
        PRINT *, 'BLKDECL: Equiv delta(1..3)=', delta(1), delta(2), delta(3)
        PRINT *, 'BLKDECL: Equiv d1k,d2k,d3k=', d1k, d2k, d3k
      END SUBROUTINE BLKDECL


! LEVEL55: scalar subroutine output args without INTENT declaration (pass-by-ref)
      SUBROUTINE LEVEL55()
        REAL A, B
        A = 0.0
        B = 0.0
        CALL SCALAROUT(1.0, A, B)
        PRINT *, 'LEVEL55:', A, B
      END SUBROUTINE LEVEL55

      SUBROUTINE SCALAROUT(X, Y, Z)
        REAL X, Y, Z
        Y = X + 1.0
        Z = X + 2.0
      END SUBROUTINE SCALAROUT

! LEVEL56: TYPE component with DIMENSION attribute; subscripted component element assignment and read
      SUBROUTINE LEVEL56()
        IMPLICIT NONE
        TYPE :: vec3_t
          REAL, DIMENSION(3) :: v
        END TYPE vec3_t
        TYPE(vec3_t) :: obj
        obj%v(1) = 1.0
        obj%v(2) = 2.0
        obj%v(3) = 3.0
        PRINT *, 'LEVEL56:', obj%v(1) + obj%v(2) + obj%v(3)
      END SUBROUTINE LEVEL56

! LEVEL57: MATMUL intrinsic - matrix-vector and matrix-matrix products
      SUBROUTINE LEVEL57()
        REAL, DIMENSION(3,3) :: mat
        REAL, DIMENSION(3)   :: vec, res1
        REAL, DIMENSION(3,3) :: res2
        INTEGER :: i, j
        DO i = 1, 3
          DO j = 1, 3
            mat(i,j) = REAL(i + j)
          END DO
          vec(i) = REAL(i)
        END DO
        res1 = MATMUL(mat, vec)
        res2 = MATMUL(mat, mat)
        PRINT *, 'LEVEL57:', res1(1), res1(2), res1(3)
      END SUBROUTINE LEVEL57

! LEVEL63: full-range component array assignment: obj%arr(:) = scalar
      SUBROUTINE LEVEL63()
          TYPE :: vec_t
              REAL, DIMENSION(3) :: v
          END TYPE vec_t
          TYPE(vec_t) :: obj
          obj%v(:) = 0.0
          obj%v(1) = 5.0
          PRINT *, 'LEVEL63:', obj%v(1), obj%v(2), obj%v(3)
      END SUBROUTINE LEVEL63

! LEVEL62: PRESENT intrinsic for optional arguments
      SUBROUTINE LEVEL62(x, y)
          INTEGER, INTENT(IN) :: x
          INTEGER, INTENT(OUT), OPTIONAL :: y
          IF (PRESENT(y)) y = x * 2
          PRINT *, 'LEVEL62:', x
      END SUBROUTINE LEVEL62

! LEVEL61: string concat in expression context (WRITE arg)
      SUBROUTINE LEVEL61()
          CHARACTER(LEN=10) :: s
          s = 'world'
          PRINT *, 'hello '//TRIM(s)//'!'
      END SUBROUTINE LEVEL61

! LEVEL60: ALLOCATE on derived-type component array field
      SUBROUTINE LEVEL60()
          TYPE :: mesh_t
              REAL, ALLOCATABLE :: x(:,:)
          END TYPE mesh_t
          TYPE(mesh_t) :: objs(2)
          INTEGER :: i, j
          i = 3
          j = 4
          ALLOCATE(objs(1)%x(i,j))
          objs(1)%x(1,1) = 1.5
          PRINT *, 'LEVEL60:', objs(1)%x(1,1)
          DEALLOCATE(objs(1)%x)
      END SUBROUTINE LEVEL60

! LEVEL59: LOGICAL field of derived type assigned via .NOT. component access
      SUBROUTINE LEVEL59()
          TYPE :: flags_t
              LOGICAL :: active
          END TYPE flags_t
          TYPE(flags_t) :: obj
          obj%active = .TRUE.
          obj%active = .NOT.obj%active
          PRINT *, 'LEVEL59:', obj%active
      END SUBROUTINE LEVEL59

! LEVEL58: string concat assignment to derived-type component field
      SUBROUTINE LEVEL58()
          TYPE :: named_t
              CHARACTER(LEN=20) :: name
          END TYPE named_t
          TYPE(named_t) :: obj
          CHARACTER(LEN=5) :: prefix
          prefix = 'item '
          obj%name = prefix // 'A'
          PRINT *, 'LEVEL58:', obj%name
      END SUBROUTINE LEVEL58

! LEVEL64: array-returning function assignment
      SUBROUTINE LEVEL64()
          REAL, DIMENSION(3) :: a, b
          a(1) = 2.0
          a(2) = 4.0
          a(3) = 6.0
          b = HalfArr(a)
          PRINT *, 'LEVEL64:', b(1), b(2), b(3)
      END SUBROUTINE LEVEL64

! LEVEL65: scalar broadcast to whole array
      SUBROUTINE LEVEL65()
          REAL, DIMENSION(3) :: a
          a = 0.0
          a(2) = 5.0
          PRINT *, 'LEVEL65:', a(1), a(2), a(3)
      END SUBROUTINE LEVEL65

! LEVEL66: whole-array binary arithmetic
      SUBROUTINE LEVEL66()
          REAL, DIMENSION(3) :: a, b, c
          a(1) = 1.0
          a(2) = 2.0
          a(3) = 3.0
          b(1) = 4.0
          b(2) = 5.0
          b(3) = 6.0
          c = a + b
          PRINT *, 'LEVEL66:', c(1), c(2), c(3)
      END SUBROUTINE LEVEL66

! LEVEL67: unary array negation
      SUBROUTINE LEVEL67()
          REAL, DIMENSION(3) :: a, b
          a(1) = -1.0
          a(2) = -2.0
          a(3) = -3.0
          b = -a
          PRINT *, 'LEVEL67:', b(1), b(2), b(3)
      END SUBROUTINE LEVEL67

! LEVEL68: TYPE name case: define as Point, use as POINT
      SUBROUTINE LEVEL68()
          TYPE :: Point
              REAL :: x, y
          END TYPE Point
          TYPE(POINT) :: p
          p%x = 1.0
          p%y = 2.0
          PRINT *, 'LEVEL68:', p%x, p%y
      END SUBROUTINE LEVEL68

! LEVEL69: string concat with derived-type component access
      SUBROUTINE LEVEL69()
          TYPE :: named_t
              CHARACTER(LEN=10) :: first
              CHARACTER(LEN=20) :: full
          END TYPE named_t
          TYPE(named_t) :: obj
          obj%first = 'hello'
          obj%full = 'say: '//obj%first
          PRINT *, 'LEVEL69:', obj%full
      END SUBROUTINE LEVEL69

! LEVEL70: scalar-array arithmetic (scalar*array, array-scalar)
      SUBROUTINE LEVEL70()
          REAL, DIMENSION(3) :: a, b
          REAL :: s
          a(1) = 1.0
          a(2) = 2.0
          a(3) = 3.0
          s = 2.0
          b = s * a
          PRINT *, 'LEVEL70:', b(1), b(2), b(3)
      END SUBROUTINE LEVEL70

! LEVEL71: ABS on array + MAXVAL
      SUBROUTINE LEVEL71()
          REAL, DIMENSION(3) :: a
          REAL :: mx
          a(1) = -1.0
          a(2) = 2.0
          a(3) = -3.0
          mx = MAXVAL(ABS(a))
          PRINT *, 'LEVEL71:', mx
      END SUBROUTINE LEVEL71

      FUNCTION HalfArr(x) RESULT(y)
          REAL, INTENT(IN), DIMENSION(:) :: x
          REAL, DIMENSION(3) :: y
          y(1) = x(1) * 0.5
          y(2) = x(2) * 0.5
          y(3) = x(3) * 0.5
      END FUNCTION HalfArr

      END PROGRAM GOLDEN
