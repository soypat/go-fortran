MODULE constants
	IMPLICIT NONE
	REAL, PARAMETER :: PI = 3.14159
	END MODULE constants

MODULE math
	CONTAINS
	FUNCTION add(a, b)
		INTEGER :: a, b, add
		add = a + b
	END FUNCTION
	END MODULE

MODULE utils
	CONTAINS
	SUBROUTINE print_msg()
		PRINT *, "Hello"
	END SUBROUTINE
	FUNCTION double(x)
		REAL :: x, double
		double = x * 2.0
	END FUNCTION
	END MODULE utils

MODULE types
	TYPE :: point
		REAL :: x, y
	END TYPE
	END MODULE

MODULE interfaces
	INTERFACE
		SUBROUTINE external_sub(x)
			REAL :: x
		END SUBROUTINE
	END INTERFACE
	END MODULE

MODULE outer
	INTEGER :: outer_var
	END MODULE
