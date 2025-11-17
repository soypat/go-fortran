RECURSIVE FUNCTION fibonacci(n) RESULT(res)
	INTEGER :: n, res
	IF (n <= 1) THEN
		res = n
	ELSE
		res = fibonacci(n-1) + fibonacci(n-2)
	END IF
	END FUNCTION