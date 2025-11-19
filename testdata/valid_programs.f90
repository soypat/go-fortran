PROGRAM simple
	END PROGRAM simple

PROGRAM test
	INTEGER :: x
	END PROGRAM

PROGRAM main
	REAL :: y
	y = 3.14
	END

PROGRAM main_multi
	END PROGRAM

PROGRAM test_comment
	! This is a comment
	INTEGER :: x  ! inline comment
	END PROGRAM

PROGRAM test_cont
	INTEGER :: very_long_variable_name_that_needs&
	           &continuation
	END PROGRAM

PROGRAM test_bare_end
	END

PROGRAM strict
	IMPLICIT NONE
	INTEGER :: x
	END PROGRAM