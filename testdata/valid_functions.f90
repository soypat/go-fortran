FUNCTION add(x, y)
	INTEGER :: x, y, add
	add = x + y
	END FUNCTION

INTEGER FUNCTION square(n)
	INTEGER :: n
	square = n * n
	END FUNCTION square

REAL FUNCTION average(a, b)
	REAL :: a, b
	average = (a + b) / 2.0
	END

CHARACTER*10 FUNCTION getName()
	getName = "test"
	END FUNCTION

DOUBLE PRECISION FUNCTION compute(x)
	DOUBLE PRECISION :: x
	compute = x * 2.0D0
	END FUNCTION

CHARACTER*4 FUNCTION constituent(n)
	INTEGER :: n
	constituent = "test"
	END FUNCTION

REAL*8 FUNCTION dbl(x)
	REAL*8 :: x
	dbl = x * 2.0
	END FUNCTION

FUNCTION compute_res(x) RESULT(res)
	REAL :: x, res
	res = x * 2.0
	END FUNCTION

ELEMENTAL FUNCTION elemental_square(x)
	REAL :: x, elemental_square
	elemental_square = x * x
	END FUNCTION

FUNCTION func_bare_end(x)
	REAL :: x, func_bare_end
	END
