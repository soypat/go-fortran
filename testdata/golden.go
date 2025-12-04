package main

import "github.com/soypat/go-fortran/intrinsic"

func main() {
	LEVEL01()
	LEVEL02()
	LEVEL03()
	LEVEL04()
	LEVEL05()
	LEVEL06()
	LEVEL07()
	LEVEL08()
	LEVEL09()
	LEVEL10()
	LEVEL11()
	LEVEL12()
	LEVEL13()
	LEVEL14()
	LEVEL15()
	LEVEL16()
	LEVEL17()
	LEVEL19()
	LEVEL20()
	LEVEL21()
	LEVEL23()
	LEVEL24()
	LEVEL25()
	LEVEL26()
	LEVEL27()
	LEVEL28()
}
func LEVEL01() {
}
func LEVEL02() {
	var (
		i int32
		_ = i
	)
	var (
		x float32
		_ = x
	)
	var (
		flag bool
		_    = flag
	)
	var (
		message intrinsic.CharacterArray
		_       = message
	)
	i = 42
	x = 3.14159
	flag = true
	message.SetFromString("Variables assigned")
}
func LEVEL03() {
	var (
		i       int32
		j       int32
		k       int32
		_, _, _ = i, j, k
	)
	var (
		x       float32
		y       float32
		z       float32
		_, _, _ = x, y, z
	)
	i = 42
	x = 3.14159
	j = i + 10
	k = i * 2
	y = x * 2
	z = float32(i) + x
}
func LEVEL04() {
	var (
		i int32
		_ = i
	)
	var (
		x float32
		_ = x
	)
	var (
		flag bool
		_    = flag
	)
	i = 42
	x = 3.14159
	flag = true
}
func LEVEL05() {
	var (
		arr1 *intrinsic.Array[int32]
		_    = arr1
	)
	arr1 = intrinsic.NewArray[int32](nil, 5)
	var (
		matrix *intrinsic.Array[float32]
		_      = matrix
	)
	matrix = intrinsic.NewArray[float32](nil, 3, 3)
	arr1.Set(10, 1)
	arr1.Set(20, 2)
	arr1.Set(30, 3)
	arr1.Set(40, 4)
	arr1.Set(50, 5)
	matrix.Set(1, 1, 1)
	matrix.Set(0, 1, 2)
	matrix.Set(0, 1, 3)
	matrix.Set(0, 2, 1)
	matrix.Set(1, 2, 2)
	matrix.Set(0, 2, 3)
	matrix.Set(0, 3, 1)
	matrix.Set(0, 3, 2)
	matrix.Set(1, 3, 3)
}
func LEVEL06() {
	var (
		i    int32
		j    int32
		_, _ = i, j
	)
	var (
		arr1 *intrinsic.Array[int32]
		_    = arr1
	)
	arr1 = intrinsic.NewArray[int32](nil, 5)
	var (
		sum_val int32
		_       = sum_val
	)
	arr1.Set(10, 1)
	arr1.Set(20, 2)
	arr1.Set(30, 3)
	arr1.Set(40, 4)
	arr1.Set(50, 5)
	sum_val = 0
	sum_val = 0
}
func LEVEL07() {
	var (
		arr1 *intrinsic.Array[int32]
		_    = arr1
	)
	arr1 = intrinsic.NewArray[int32](nil, 5)
	var (
		result int32
		_      = result
	)
	arr1.Set(10, 1)
	arr1.Set(20, 2)
	arr1.Set(30, 3)
	arr1.Set(40, 4)
	arr1.Set(50, 5)
	SIMPLE_SUB()
	ADD_VALUES(10, 20, result)
	MODIFY_ARRAY(arr1, 5)
}
func LEVEL08() {
	var (
		fact_result int32
		_           = fact_result
	)
	var (
		sqrt_result float32
		_           = sqrt_result
	)
	fact_result = FACTORIAL(5)
	sqrt_result = SQUARE_ROOT(16)
}
func LEVEL09() {
	var (
		i          int32
		n          int32
		fib_result int32
		sum_val    int32
		_, _, _, _ = i, n, fib_result, sum_val
	)
	n = 7
	fib_result = FIBONACCI(n)
	i = 1
	sum_val = 0
}
func LEVEL10() {
	var (
		i       int32
		j       int32
		k       int32
		_, _, _ = i, j, k
	)
	var (
		x           float32
		y           float32
		z           float32
		expr_result float32
		_, _, _, _  = x, y, z, expr_result
	)
	var (
		flag       bool
		cond1      bool
		cond2      bool
		cond3      bool
		_, _, _, _ = flag, cond1, cond2, cond3
	)
	i = 11
	j = 52
	k = 84
	x = 3.14159
	y = 6.28318
	z = 45.14159
	flag = true
	expr_result = (x+y)*z - float32(k)/2
	cond1 = (i > 5) && (j < 100)
	cond2 = (x >= 3) || (y <= 1)
	cond3 = !flag
}
func LEVEL11() {
	var (
		str1 intrinsic.CharacterArray
		str2 intrinsic.CharacterArray
		_, _ = str1, str2
	)
	var (
		str3 intrinsic.CharacterArray
		_    = str3
	)
	str1.SetFromString("Hello")
	str2.SetFromString("World")
	str3.SetConcatString(str1.String(), " ", str2.String())
}
func LEVEL12() {
	var (
		angle      float32
		sin_val    float32
		cos_val    float32
		abs_val    float32
		_, _, _, _ = angle, sin_val, cos_val, abs_val
	)
	var (
		i             int32
		j             int32
		k             int32
		max_val       int32
		min_val       int32
		_, _, _, _, _ = i, j, k, max_val, min_val
	)
	i = 11
	j = 52
	k = 84
	angle = 0.5
	sin_val = intrinsic.SIN(angle)
	cos_val = intrinsic.COS(angle)
	abs_val = intrinsic.ABS[float32](-5.5)
	max_val = intrinsic.MAX[int32](i, j, k)
	min_val = intrinsic.MIN[int32](10, 20, 5)
}
func LEVEL13() {
	var (
		i       int32
		sum_val int32
		count   int32
		_, _, _ = i, sum_val, count
	)
	var (
		arr *intrinsic.Array[int32]
		_   = arr
	)
	arr = intrinsic.NewArray[int32](nil, 10)
	arr.Set(5, 1)
	arr.Set(-3, 2)
	arr.Set(7, 3)
	arr.Set(-1, 4)
	arr.Set(9, 5)
	arr.Set(2, 6)
	arr.Set(-4, 7)
	arr.Set(6, 8)
	arr.Set(8, 9)
	arr.Set(1, 10)
	sum_val = 0
	count = 0
}
func LEVEL14() {
	var (
		x    int32
		y    int32
		_, _ = x, y
	)
	x = 999
	goto label100
label100:
	{
	}
	x = 10
	y = 5
	y = 999
	goto label200
label200:
	{
	}
}
func LEVEL15() {
	var (
		choice int32
		result int32
		_, _   = choice, result
	)
	choice = 2
	choice = 5
	choice = 99
}
func LEVEL16() {
	var (
		str1    intrinsic.CharacterArray
		str2    intrinsic.CharacterArray
		str3    intrinsic.CharacterArray
		_, _, _ = str1, str2, str3
	)
	var (
		len_val      int32
		len_trim_val int32
		index_val    int32
		_, _, _      = len_val, len_trim_val, index_val
	)
	str1.SetFromString("Hello")
	len_val = int32(str1.Len())
	len_trim_val = int32(str1.LenTrim())
	str2.SetFromString(str1.Trim().String())
	str1.SetFromString("Hello World")
	index_val = int32(str1.Index("World"))
	str1.SetFromString("   Left")
	str2.SetFromString(str1.AdjustL().String())
	str1.SetFromString("Right   ")
	str3.SetFromString(str1.AdjustR().String())
	str1.SetFromString("abcdef")
	str3.SetFromString(str1.Substring(2, 4))
	str1.SetFromString("z")
}
func LEVEL17() {
	var (
		matrix *intrinsic.Array[int32]
		_      = matrix
	)
	matrix = intrinsic.NewArray[int32](nil, 3, 4)
	var (
		vector *intrinsic.Array[int32]
		_      = vector
	)
	vector = intrinsic.NewArray[int32](nil, 5)
	var (
		size_total int32
		size_dim1  int32
		size_dim2  int32
		_, _, _    = size_total, size_dim1, size_dim2
	)
	var (
		lb   int32
		ub   int32
		_, _ = lb, ub
	)
	size_total = int32(matrix.Size())
	size_dim1 = int32(matrix.SizeDim(1))
	size_dim2 = int32(matrix.SizeDim(2))
	size_total = int32(vector.Size())
	lb = int32(matrix.LowerDim(1))
	lb = int32(matrix.LowerDim(2))
	ub = int32(matrix.UpperDim(1))
	ub = int32(matrix.UpperDim(2))
}
func LEVEL18() {
	var (
		vec *intrinsic.Array[int32]
		_   = vec
	)
	var (
		mat *intrinsic.Array[int32]
		_   = mat
	)
	vec.Set(10, 1)
	vec.Set(20, 2)
	vec.Set(30, 3)
	mat.Set(100, 1, 1)
	mat.Set(200, 2, 3)
}
func LEVEL19() {
	SET_COMMON_VALUES()
	PRINT_COMMON_VALUES()
}
func SET_COMMON_VALUES() {
	SHARED.x = 42
	SHARED.y = 99
	SHARED.z = 3.14159
}
func PRINT_COMMON_VALUES() {
}
func LEVEL20() {
	var (
		a       int32
		b       int32
		c       int32
		_, _, _ = a, b, c
	)
	var (
		x    float32
		y    float32
		_, _ = x, y
	)
}
func LEVEL21() {
	var (
		x      int32
		choice int32
		_, _   = x, choice
	)
	x = -5
	goto label10
label10:
	{
	}
	goto label20
label20:
	{
	}
	goto label30
label30:
	{
	}
	goto label40
label40:
	{
		choice = 2
	}
	goto label100
label100:
	{
	}
	goto label200
label200:
	{
	}
	goto label300
label300:
	{
	}
	goto label400
label400:
	{
	}
}
func LEVEL22() {
}
func LEVEL23() {
	var (
		MAX_SIZE int32
		_        = MAX_SIZE
	)
	var (
		PI float32
		_  = PI
	)
	var (
		TAU float32
		_   = TAU
	)
}
func LEVEL24() {
	var (
		vec1 *intrinsic.Array[int32]
		_    = vec1
	)
	vec1 = intrinsic.NewArray[int32](nil, 3)
	var (
		vec2 *intrinsic.Array[int32]
		_    = vec2
	)
	vec2 = intrinsic.NewArray[int32](nil, 5)
	vec1 = *intrinsic.NewArray[int32]([]int32{10, 20, 30}, 3)
	vec2 = *intrinsic.NewArray[int32]([]int32{100, 200, 300, 400, 500}, 5)
}
func LEVEL25() {
	var (
		i1 int8
		_  = i1
	)
	var (
		i2 int16
		_  = i2
	)
	var (
		i4 int32
		_  = i4
	)
	var (
		i8 int64
		_  = i8
	)
	var (
		r4 float32
		_  = r4
	)
	var (
		r8 float64
		_  = r8
	)
	i1 = 127
	i2 = 32767
	i4 = 2147483647
	i8 = 9223372036854775807
	r4 = 3.14159
	r8 = float64(3.141592653589793)
}
func LEVEL26() {
	var (
		hex_val int32
		oct_val int32
		bin_val int32
		_, _, _ = hex_val, oct_val, bin_val
	)
	var (
		d1      float64
		d2      float64
		d4      float64
		_, _, _ = d1, d2, d4
	)
	hex_val = int32(255)
	oct_val = int32(255)
	bin_val = int32(255)
	d1 = float64(1)
	d2 = float64(123)
	d4 = float64(2.718281828)
}
func LEVEL27() {
	var (
		ncomp int32
		_     = ncomp
	)
	var (
		factor float64
		_      = factor
	)
	var (
		root3 float64
		_     = root3
	)
	var (
		PI float64
		_  = PI
	)
	var (
		result float64
		_      = result
	)
	result = float64(float64(factor*root3) * float64(ncomp))
}
func LEVEL28() {
	HOLDRT.YQR.Set(1.5, 1)
	HOLDRT.SUMXRQ.Set(99.90000000000001, 512)
	HOLDRT.YMNRT.Set(3.14, 2)
	HOLDRT.MATRIX.Set(42.5, 5, 10)
	STATS.COUNTS.Set(42, 50)
}
func SIMPLE_SUB() {
}
func ADD_VALUES(a int32, b int32, result int32) {
	result = a + b
}
func MODIFY_ARRAY(arr intrinsic.Array[int32], n int32) {
	var (
		i int32
		_ = i
	)
}
func FACTORIAL(n int32) (FACTORIAL int32) {
	var (
		i      int32
		result int32
		_, _   = i, result
	)
	result = 1
	FACTORIAL = result
	return
}
func SQUARE_ROOT(x float32) (SQUARE_ROOT float32) {
	SQUARE_ROOT = intrinsic.SQRT(x)
	return
}
func FIBONACCI(n int32) (FIBONACCI int32) {
	var (
		a          int32
		b          int32
		temp       int32
		i          int32
		_, _, _, _ = a, b, temp, i
	)
	a = 0
	b = 1
	FIBONACCI = b
	return
}

var HOLDRT = struct {
	YQR    *intrinsic.Array[float32]
	SUMXRQ *intrinsic.Array[float32]
	YMNRT  *intrinsic.Array[float32]
	MATRIX *intrinsic.Array[float32]
}{YQR: intrinsic.NewArray[float32](nil, 256), SUMXRQ: intrinsic.NewArray[float32](nil, 512), YMNRT: intrinsic.NewArray[float32](nil, 3), MATRIX: intrinsic.NewArray[float32](nil, 10, 20)}
var SHARED struct {
	x int32
	y int32
	z float32
}
var STATS = struct {
	COUNTS *intrinsic.Array[int32]
}{COUNTS: intrinsic.NewArray[int32](nil, 100)}
