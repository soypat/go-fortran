RECURSIVE FUNCTION fibonacci(n) RESULT(res)
	INTEGER :: n, res
	IF (n <= 1) THEN
		res = n
	ELSE
		res = fibonacci(n-1) + fibonacci(n-2)
	END IF
	END FUNCTION

FUNCTION gdyn2e()
	REAL :: alat, cldrad, al
	IF (ABS(alat) .ge. 90. - cldrad)al = 1.15*al
	IF (alat+cldrad .ge. 90. - cldrad)al = 1.15*al
END FUNCTION gdyn2e