SUBROUTINE test(x)
	INTEGER :: x
	IF (x > 0) THEN
		PRINT *, x
	END IF
	END

RECURSIVE SUBROUTINE fact(n, result)
	INTEGER :: n, result
	IF (n <= 1) THEN
		result = 1
	ELSE
		CALL fact(n-1, result)
		result = result * n
	END IF
	END SUBROUTINE

SUBROUTINE test_do
	DO i = 1, 10
		IF (i > 5) THEN
			PRINT *, i
		END IF
	END DO
	END SUBROUTINE

SUBROUTINE inline_if()
	INTEGER :: x
	IF(x.EQ.1) x=x/2
END SUBROUTINE