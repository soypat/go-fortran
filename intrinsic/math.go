package intrinsic

import (
	"math"
	"math/cmplx"
)

type signed interface {
	~int | ~int8 | ~int16 | ~int32 | ~int64
}

type unsigned interface {
	~uint | ~uint8 | ~uint16 | ~uint32 | ~uint64 | ~uintptr
}

type integer interface {
	signed | unsigned
}

type float interface {
	~float32 | ~float64
}

type complexNum interface {
	~complex64 | ~complex128
}

type numeric interface {
	integer | float
}

// ============================================================================
// Fortran Math Intrinsic Functions
// Implements standard Fortran 77/90/95 mathematical intrinsics
// ============================================================================

// ABS returns the absolute value of x
// Fortran: ABS(x) - works with INTEGER, REAL, COMPLEX
// For complex numbers, returns magnitude
func ABS[T signed | float](x T) T {
	if x < 0 {
		return -x
	}
	return x
}

// IABS returns the absolute value of an integer
// Fortran: IABS(i) - works with INTEGER
func IABS[T signed](x T) T {
	if x < 0 {
		return -x
	}
	return x
}

// CABS returns the absolute value (magnitude) of a complex number
// Fortran: CABS(z) - returns REAL
func CABS(z complex64) float32 {
	return float32(cmplx.Abs(complex128(z)))
}

// CDABS returns the absolute value (magnitude) of a double complex number
// Fortran: CDABS(z) - returns DOUBLE PRECISION
func CDABS(z complex128) float64 {
	return cmplx.Abs(z)
}

// CMPLX converts a real or integer to complex with zero imaginary part.
// Fortran: CMPLX(x) - returns COMPLEX
func CMPLX[T float | signed](x T) complex64 {
	r := float64(x)
	return complex64(complex(r, 0))
}

// CMPLX2 creates a complex number from real and imaginary parts.
// Fortran: CMPLX(x, y) - returns COMPLEX
func CMPLX2[T float | signed](x, y T) complex64 {
	r, i := float64(x), float64(y)
	return complex64(complex(r, i))
}

// DCMPLX converts to double complex with zero imaginary part.
// Fortran: DCMPLX(x) - returns DOUBLE COMPLEX
func DCMPLX[T float | signed](x T) complex128 {
	return complex(float64(x), 0)
}

// DCMPLX2 creates a double complex number from real and imaginary parts.
// Fortran: DCMPLX(x, y) - returns DOUBLE COMPLEX
func DCMPLX2[T float | signed](x, y T) complex128 {
	return complex(float64(x), float64(y))
}

// AIMAG returns the imaginary part of a complex number.
// Fortran: AIMAG(z) - returns REAL
func AIMAG(z complex64) float32 {
	return imag(z)
}

// REALPART returns the real part of a complex number.
// Fortran: REAL(z) when z is complex - returns REAL
func REALPART(z complex64) float32 {
	return real(z)
}

// DREALPART returns the real part of a double complex number.
// Fortran: DREAL(z) - returns DOUBLE PRECISION
func DREALPART(z complex128) float64 {
	return real(z)
}

// DIMAG returns the imaginary part of a double complex number.
// Fortran: DIMAG(z) - returns DOUBLE PRECISION
func DIMAG(z complex128) float64 {
	return imag(z)
}

// SQRT returns the square root of x
// Fortran: SQRT(x) - works with REAL, COMPLEX
func SQRT[T float](x T) T {
	return T(math.Sqrt(float64(x)))
}

// CSQRT returns the complex square root of z
// Fortran: CSQRT(z) - returns COMPLEX
func CSQRT(z complex64) complex64 {
	return complex64(cmplx.Sqrt(complex128(z)))
}

// EXP returns e raised to the power x
// Fortran: EXP(x)
func EXP[T float](x T) T {
	return T(math.Exp(float64(x)))
}

// LOG returns the natural logarithm of x
// Fortran: LOG(x) or ALOG(x)
func LOG[T float](x T) T {
	return T(math.Log(float64(x)))
}

// LOG10 returns the base-10 logarithm of x
// Fortran: LOG10(x) or ALOG10(x)
func LOG10[T float](x T) T {
	return T(math.Log10(float64(x)))
}

// SIN returns the sine of x (x in radians)
// Fortran: SIN(x)
func SIN[T float](x T) T {
	return T(math.Sin(float64(x)))
}

// COS returns the cosine of x (x in radians)
// Fortran: COS(x)
func COS[T float](x T) T {
	return T(math.Cos(float64(x)))
}

// TAN returns the tangent of x (x in radians)
// Fortran: TAN(x)
func TAN[T float](x T) T {
	return T(math.Tan(float64(x)))
}

// ASIN returns the arcsine of x in radians
// Fortran: ASIN(x)
func ASIN[T float](x T) T {
	return T(math.Asin(float64(x)))
}

// ACOS returns the arccosine of x in radians
// Fortran: ACOS(x)
func ACOS[T float](x T) T {
	return T(math.Acos(float64(x)))
}

// ATAN returns the arctangent of x in radians
// Fortran: ATAN(x)
func ATAN[T float](x T) T {
	return T(math.Atan(float64(x)))
}

// ATAN2 returns the arctangent of y/x in radians, using signs to determine quadrant
// Fortran: ATAN2(y, x)
func ATAN2[T float](y, x T) T {
	return T(math.Atan2(float64(y), float64(x)))
}

// SINH returns the hyperbolic sine of x
// Fortran: SINH(x)
func SINH[T float](x T) T {
	return T(math.Sinh(float64(x)))
}

// COSH returns the hyperbolic cosine of x
// Fortran: COSH(x)
func COSH[T float](x T) T {
	return T(math.Cosh(float64(x)))
}

// TANH returns the hyperbolic tangent of x
// Fortran: TANH(x)
func TANH[T float](x T) T {
	return T(math.Tanh(float64(x)))
}

// MOD returns the remainder of a divided by p (same sign as a)
// Fortran: MOD(a, p) - works with INTEGER and REAL
// For integers: a - INT(a/p) * p
// For reals: a - AINT(a/p) * p
func MOD[T integer](a, p T) T {
	return a % p
}

// MODREAL returns the floating-point remainder
// Fortran: MOD(a, p) for REAL arguments
func MODREAL[T float](a, p T) T {
	return T(math.Mod(float64(a), float64(p)))
}

// POW returns a to the power of exponent. In fortran represented as a**exponent.
func POW[T float](a, exponent T) T {
	return T(math.Pow(float64(a), float64(exponent)))
}

// POW returns a to the power of exponent. In fortran represented as a**exponent.
func CPOW[T complexNum](a, exponent T) T {
	return T(cmplx.Pow(complex128(a), complex128(exponent)))
}

// SIGN transfers the sign of b to the magnitude of a
// Fortran: SIGN(a, b) returns |a| if b >= 0, -|a| if b < 0
func SIGN[T signed | float](a, b T) T {
	abs_a := a
	if a < 0 {
		abs_a = -a
	}
	if b < 0 {
		return -abs_a
	}
	return abs_a
}

// MAX returns the maximum value from a variadic list
// Fortran: MAX(a1, a2, ..., an)
func MAX[T numeric](first T, rest ...T) T {
	max := first
	for _, v := range rest {
		if v > max {
			max = v
		}
	}
	return max
}

// MIN returns the minimum value from a variadic list
// Fortran: MIN(a1, a2, ..., an)
func MIN[T numeric](first T, rest ...T) T {
	min := first
	for _, v := range rest {
		if v < min {
			min = v
		}
	}
	return min
}

// FLOOR returns the greatest integer less than or equal to x
// Fortran: FLOOR(x)
func FLOOR[T float](x T) T {
	return T(math.Floor(float64(x)))
}

// CEILING returns the least integer greater than or equal to x
// Fortran: CEILING(x)
func CEILING[T float](x T) T {
	return T(math.Ceil(float64(x)))
}

// NINT returns the nearest integer to x
// Fortran: NINT(x) - rounds to nearest integer (0.5 rounds away from zero)
func NINT[T float](x T) int32 {
	return int32(math.Round(float64(x)))
}

// AINT truncates x to a whole number (rounds toward zero)
// Fortran: AINT(x) - returns REAL type
func AINT[T float](x T) T {
	return T(math.Trunc(float64(x)))
}

// ANINT returns the nearest whole number to x
// Fortran: ANINT(x) - like NINT but returns REAL type
func ANINT[T float](x T) T {
	return T(math.Round(float64(x)))
}

// DIM returns the positive difference (a - b if a > b, else 0)
// Fortran: DIM(a, b)
func DIM[T numeric](a, b T) T {
	if a > b {
		return a - b
	}
	return 0
}

// DPROD returns the double-precision product of two REAL arguments
// Fortran: DPROD(x, y) - returns DOUBLE PRECISION
func DPROD(x, y float32) float64 {
	return float64(x) * float64(y)
}

// Utility functions for type-specific operations

// MaxInt32 is a convenience wrapper for MAX with int32
func MaxInt32(vals ...int32) int32 {
	if len(vals) == 0 {
		return 0
	}
	return MAX(vals[0], vals[1:]...)
}

// MinInt32 is a convenience wrapper for MIN with int32
func MinInt32(vals ...int32) int32 {
	if len(vals) == 0 {
		return 0
	}
	return MIN(vals[0], vals[1:]...)
}

// MaxFloat32 is a convenience wrapper for MAX with float32
func MaxFloat32(vals ...float32) float32 {
	if len(vals) == 0 {
		return 0
	}
	return MAX(vals[0], vals[1:]...)
}

// MinFloat32 is a convenience wrapper for MIN with float32
func MinFloat32(vals ...float32) float32 {
	if len(vals) == 0 {
		return 0
	}
	return MIN(vals[0], vals[1:]...)
}
