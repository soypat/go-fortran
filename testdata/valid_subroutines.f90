SUBROUTINE hello
	PRINT *, "Hello"
	END SUBROUTINE

SUBROUTINE sub1(a, b, c)
	REAL :: a, b, c
	END SUBROUTINE sub1

SUBROUTINE nosub()
	END SUBROUTINE

PURE SUBROUTINE swap(a, b)
	REAL, INTENT(INOUT) :: a, b
	REAL :: temp
	temp = a
	a = b
	b = temp
	END SUBROUTINE

SUBROUTINE helper_multi()
	END SUBROUTINE

SUBROUTINE sub_bare_end
	END

SUBROUTINE init_data
	INTEGER :: array(10)
	DATA array /1,2,3,4,5,6,7,8,9,10/
	END SUBROUTINE

SUBROUTINE output_fmt
	10 FORMAT(I5, F10.2)
	WRITE(6, 10) 42, 3.14
	END SUBROUTINE

SUBROUTINE use_common
	COMMON /block1/ x, y, z
	REAL :: x, y, z
	END SUBROUTINE

SUBROUTINE many_params(a, b, c, d, e, f, g, h)
	REAL :: a, b, c, d, e, f, g, h
	END SUBROUTINE

SUBROUTINE empty()
	END SUBROUTINE

SUBROUTINE test_keywords(data, type, format)
	INTEGER :: data, type, format
	END SUBROUTINE

SUBROUTINE stateful
	INTEGER, SAVE :: counter
	counter = counter + 1
	END SUBROUTINE