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
	LEVEL29()
	intrinsic.Stop(0)
}
func LEVEL01() {
	intrinsic.Print("LEVEL 1: Hello, World!")
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
	message = intrinsic.NewCharacterArray(20)
	i = 42
	x = 3.14159
	flag = true
	message.SetFromString("Variables assigned")
	intrinsic.Print("LEVEL 2: i =", i, ", x =", x)
	intrinsic.Print("LEVEL 2: flag =", flag)
	intrinsic.Print("LEVEL 2:", message)
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
	y = x * 2.0
	z = float32(i) + x
	intrinsic.Print("LEVEL 3: j =", j, ", k =", k)
	intrinsic.Print("LEVEL 3: y =", y, ", z =", z)
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
	if i > 40 {
		intrinsic.Print("LEVEL 4: i is greater than 40")
	}
	if flag {
		intrinsic.Print("LEVEL 4: flag is true")
	} else {
		intrinsic.Print("LEVEL 4: flag is false")
	}
	if x < 3.0 {
		intrinsic.Print("LEVEL 4: x < 3.0")
	} else if x < 4.0 {
		intrinsic.Print("LEVEL 4: 3.0 <= x < 4.0")
	} else {
		intrinsic.Print("LEVEL 4: x >= 4.0")
	}
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
	intrinsic.Print("LEVEL 5: arr1(1) =", arr1.At(1))
	intrinsic.Print("LEVEL 5: arr1(3) =", arr1.At(3))
	intrinsic.Print("LEVEL 5: arr1(5) =", arr1.At(5))
	matrix.Set(1.0, 1, 1)
	matrix.Set(0.0, 1, 2)
	matrix.Set(0.0, 1, 3)
	matrix.Set(0.0, 2, 1)
	matrix.Set(1.0, 2, 2)
	matrix.Set(0.0, 2, 3)
	matrix.Set(0.0, 3, 1)
	matrix.Set(0.0, 3, 2)
	matrix.Set(1.0, 3, 3)
	intrinsic.Print("LEVEL 5: matrix(1,1) =", matrix.At(1, 1))
	intrinsic.Print("LEVEL 5: matrix(2,2) =", matrix.At(2, 2))
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
	for i = 1; i <= 5; i++ {
		sum_val = sum_val + arr1.At(int(i))
	}
	intrinsic.Print("LEVEL 6: sum of arr1 =", sum_val)
	sum_val = 0
	for i = 1; i <= 3; i++ {
		for j = 1; j <= 3; j++ {
			sum_val = sum_val + 1
		}
	}
	intrinsic.Print("LEVEL 6: nested loop count =", sum_val)
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
	ADD_VALUES(10, 20, &result)
	intrinsic.Print("LEVEL 7: ADD_VALUES(10, 20) =", result)
	MODIFY_ARRAY(arr1, 5)
	intrinsic.Print("LEVEL 7: arr1 after modify:", arr1.At(1), arr1.At(2), arr1.At(3))
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
	intrinsic.Print("LEVEL 8: FACTORIAL(5) =", fact_result)
	sqrt_result = SQUARE_ROOT(16.0)
	intrinsic.Print("LEVEL 8: SQUARE_ROOT(16.0) =", sqrt_result)
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
	intrinsic.Print("LEVEL 9: FIBONACCI(7) =", fib_result)
	i = 1
	sum_val = 0
	for i <= 10 {
		sum_val = sum_val + i
		i = i + 1
	}
	intrinsic.Print("LEVEL 9: sum 1 to 10 =", sum_val)
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
	expr_result = (x+y)*z - float32(k)/2.0
	intrinsic.Print("LEVEL 10: complex expr =", expr_result)
	cond1 = (i > 5) && (j < 100)
	cond2 = (x >= 3.0) || (y <= 1.0)
	cond3 = !flag
	intrinsic.Print("LEVEL 10: cond1 =", cond1, ", cond2 =", cond2)
	intrinsic.Print("LEVEL 10: cond3 =", cond3)
}
func LEVEL11() {
	var (
		str1 intrinsic.CharacterArray
		str2 intrinsic.CharacterArray
		_, _ = str1, str2
	)
	str1 = intrinsic.NewCharacterArray(10)
	str2 = intrinsic.NewCharacterArray(10)
	var (
		str3 intrinsic.CharacterArray
		_    = str3
	)
	str3 = intrinsic.NewCharacterArray(20)
	str1.SetFromString("Hello")
	str2.SetFromString("World")
	str3.SetConcatString(str1.String(), " ", str2.String())
	intrinsic.Print("LEVEL 11: concatenation:", str3)
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
	abs_val = float32(intrinsic.ABS[float32](-5.5))
	max_val = intrinsic.MAX[int32](i, j, k)
	min_val = intrinsic.MIN[int32](10, 20, 5)
	intrinsic.Print("LEVEL 12: SIN(0.5) =", sin_val)
	intrinsic.Print("LEVEL 12: COS(0.5) =", cos_val)
	intrinsic.Print("LEVEL 12: ABS(-5.5) =", abs_val)
	intrinsic.Print("LEVEL 12: MAX =", max_val, ", MIN =", min_val)
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
	for i = 1; i <= 10; i++ {
		if arr.At(int(i)) < 0 {
			continue
		}
		sum_val = sum_val + arr.At(int(i))
	}
	intrinsic.Print("LEVEL 13: sum of positive =", sum_val)
	count = 0
	for i = 1; i <= 10; i++ {
		if arr.At(int(i)) > 7 {
			break
		}
		count = count + 1
	}
	intrinsic.Print("LEVEL 13: count before >7 =", count)
	for i = 1; i <= 3; i++ {
		count = i
	}
	intrinsic.Print("LEVEL 13: last count =", count)
}
func LEVEL14() {
	var (
		x    int32
		y    int32
		_, _ = x, y
	)
	goto label100
	x = 999
	goto label100
label100:
	{
	}
	x = 10
	y = 5
	if y == 5 {
		goto label200
	}
	y = 999
	goto label200
label200:
	{
	}
	intrinsic.Print("LEVEL 14: x =", x, ", y =", y)
}
func LEVEL15() {
	var (
		choice int32
		result int32
		_, _   = choice, result
	)
	choice = 2
	switch choice {
	case 1:
		result = 10
	case 2:
		result = 20
	case 3:
		result = 30
	default:
		result = 0
	}
	intrinsic.Print("LEVEL 15: choice =", choice, ", result =", result)
	choice = 5
	switch choice {
	case 1, 2, 3:
		result = 100
	case 4, 5, 6:
		result = 200
	default:
		result = 999
	}
	intrinsic.Print("LEVEL 15: choice =", choice, ", result =", result)
	choice = 99
	switch choice {
	case 1:
		result = 10
	case 2:
		result = 20
	default:
		result = 777
	}
	intrinsic.Print("LEVEL 15: choice =", choice, ", result =", result)
}
func LEVEL16() {
	var (
		str1    intrinsic.CharacterArray
		str2    intrinsic.CharacterArray
		str3    intrinsic.CharacterArray
		_, _, _ = str1, str2, str3
	)
	str1 = intrinsic.NewCharacterArray(20)
	str2 = intrinsic.NewCharacterArray(20)
	str3 = intrinsic.NewCharacterArray(20)
	var (
		len_val      int32
		len_trim_val int32
		index_val    int32
		_, _, _      = len_val, len_trim_val, index_val
	)
	str1.SetFromString("Hello")
	len_val = int32(str1.Len())
	intrinsic.Print("LEVEL 16: LEN =", len_val)
	len_trim_val = int32(str1.LenTrim())
	intrinsic.Print("LEVEL 16: LEN_TRIM =", len_trim_val)
	str2.SetFromString(str1.Trim().String())
	intrinsic.Print("LEVEL 16: TRIM =", str2)
	str1.SetFromString("Hello World")
	index_val = int32(str1.Index("World"))
	intrinsic.Print("LEVEL 16: INDEX =", index_val)
	str1.SetFromString("   Left")
	str2.SetFromString(str1.AdjustL().String())
	intrinsic.Print("LEVEL 16: ADJUSTL =", str2)
	str1.SetFromString("Right   ")
	str3.SetFromString(str1.AdjustR().String())
	intrinsic.Print("LEVEL 16: ADJUSTR =", str3)
	str1.SetFromString("abcdef")
	str3.SetFromString(str1.Substring(2, 4))
	str1.SetFromString("z")
	intrinsic.Print("LEVEL 16: str3 =", str3)
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
	intrinsic.Print("LEVEL 17: SIZE(matrix) =", size_total)
	size_dim1 = int32(matrix.SizeDim(1))
	intrinsic.Print("LEVEL 17: SIZE(matrix,1) =", size_dim1)
	size_dim2 = int32(matrix.SizeDim(2))
	intrinsic.Print("LEVEL 17: SIZE(matrix,2) =", size_dim2)
	size_total = int32(vector.Size())
	intrinsic.Print("LEVEL 17: SIZE(vector) =", size_total)
	lb = int32(matrix.LowerDim(1))
	intrinsic.Print("LEVEL 17: LBOUND(matrix,1) =", lb)
	lb = int32(matrix.LowerDim(2))
	intrinsic.Print("LEVEL 17: LBOUND(matrix,2) =", lb)
	ub = int32(matrix.UpperDim(1))
	intrinsic.Print("LEVEL 17: UBOUND(matrix,1) =", ub)
	ub = int32(matrix.UpperDim(2))
	intrinsic.Print("LEVEL 17: UBOUND(matrix,2) =", ub)
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
	intrinsic.Print("LEVEL 18: vec(1) =", vec.At(1))
	intrinsic.Print("LEVEL 18: vec(3) =", vec.At(3))
	intrinsic.Print("LEVEL 18: SIZE(vec) =", int32(vec.Size()))
	mat.Set(100, 1, 1)
	mat.Set(200, 2, 3)
	intrinsic.Print("LEVEL 18: mat(1,1) =", mat.At(1, 1))
	intrinsic.Print("LEVEL 18: mat(2,3) =", mat.At(2, 3))
	intrinsic.Print("LEVEL 18: SIZE(mat) =", int32(mat.Size()))
	intrinsic.Print("LEVEL 18: Arrays deallocated")
}
func LEVEL19() {
	SET_COMMON_VALUES()
	PRINT_COMMON_VALUES()
}
func SET_COMMON_VALUES() {
	var (
		x    int32
		y    int32
		_, _ = x, y
	)
	var (
		z float32
		_ = z
	)
	shared.x = 42
	shared.y = 99
	shared.z = 3.14159
}
func PRINT_COMMON_VALUES() {
	var (
		x    int32
		y    int32
		_, _ = x, y
	)
	var (
		z float32
		_ = z
	)
	intrinsic.Print("LEVEL 19: x =", shared.x)
	intrinsic.Print("LEVEL 19: y =", shared.y)
	intrinsic.Print("LEVEL 19: z =", shared.z)
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
	a = 10
	b = 20
	c = 30
	x = 3.14
	y = 2.71
	intrinsic.Print("LEVEL 20: a =", a)
	intrinsic.Print("LEVEL 20: b =", b)
	intrinsic.Print("LEVEL 20: c =", c)
	intrinsic.Print("LEVEL 20: x =", x)
	intrinsic.Print("LEVEL 20: y =", y)
}
func LEVEL21() {
	var (
		x      int32
		choice int32
		_, _   = x, choice
	)
	x = -5
	if int(x) < 0 {
		goto label10
	} else if int(x) == 0 {
		goto label20
	} else {
		goto label30
	}
	goto label10
label10:
	{
		intrinsic.Print("LEVEL 21: x is negative")
	}
	goto label40
	goto label20
label20:
	{
		intrinsic.Print("LEVEL 21: x is zero")
	}
	goto label40
	goto label30
label30:
	{
		intrinsic.Print("LEVEL 21: x is positive")
	}
	goto label40
label40:
	{
		choice = 2
	}
	switch int(choice) {
	case 1:
		goto label100
	case 2:
		goto label200
	case 3:
		goto label300
	}
	goto label100
label100:
	{
		intrinsic.Print("LEVEL 21: Choice was 1")
	}
	goto label400
	goto label200
label200:
	{
		intrinsic.Print("LEVEL 21: Choice was 2")
	}
	goto label400
	goto label300
label300:
	{
		intrinsic.Print("LEVEL 21: Choice was 3")
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
		max_size int32 = 100
		_              = max_size
	)
	var (
		pi float32 = 3.14159
		_          = pi
	)
	var (
		tau float32 = 2.0 * pi
		_           = tau
	)
	intrinsic.Print("LEVEL 23: MAX_SIZE =", max_size)
	intrinsic.Print("LEVEL 23: PI =", pi)
	intrinsic.Print("LEVEL 23: TAU =", tau)
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
	vec1 = intrinsic.NewArray[int32]([]int32{10, 20, 30}, 3)
	intrinsic.Print("LEVEL 24: vec1(1) =", vec1.At(1))
	intrinsic.Print("LEVEL 24: vec1(2) =", vec1.At(2))
	intrinsic.Print("LEVEL 24: vec1(3) =", vec1.At(3))
	vec2 = intrinsic.NewArray[int32]([]int32{100, 200, 300, 400, 500}, 5)
	intrinsic.Print("LEVEL 24: vec2(1) =", vec2.At(1))
	intrinsic.Print("LEVEL 24: vec2(5) =", vec2.At(5))
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
	intrinsic.Print("LEVEL 25: i1 =", i1)
	intrinsic.Print("LEVEL 25: i2 =", i2)
	intrinsic.Print("LEVEL 25: i4 =", i4)
	intrinsic.Print("LEVEL 25: i8 =", i8)
	intrinsic.Print("LEVEL 25: r4 =", r4)
	intrinsic.Print("LEVEL 25: r8 =", r8)
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
	intrinsic.Print("LEVEL 26: hex_val =", hex_val)
	intrinsic.Print("LEVEL 26: oct_val =", oct_val)
	intrinsic.Print("LEVEL 26: bin_val =", bin_val)
	d1 = float64(1.0)
	d2 = float64(123.0)
	d4 = float64(2.718281828)
	intrinsic.Print("LEVEL 26: d1 =", d1)
	intrinsic.Print("LEVEL 26: d2 =", d2)
	intrinsic.Print("LEVEL 26: d4 =", d4)
}
func LEVEL27() {
	var (
		ncomp int32 = 5
		_           = ncomp
	)
	var (
		factor float64 = 1.0 / 86400.0
		_              = factor
	)
	var (
		root3 float64 = intrinsic.SQRT(3.0)
		_             = root3
	)
	var (
		pi float64 = 4.0 * intrinsic.ATAN(1.0)
		_          = pi
	)
	var (
		result float64
		_      = result
	)
	result = float64(factor * root3 * float64(ncomp))
	intrinsic.Print("LEVEL 27: ncomp =", ncomp)
	intrinsic.Print("LEVEL 27: factor =", factor)
	intrinsic.Print("LEVEL 27: root3 =", root3)
	intrinsic.Print("LEVEL 27: PI =", pi)
	intrinsic.Print("LEVEL 27: result =", result)
}
func LEVEL28() {
	var (
		yqr     *intrinsic.Array[float32]
		sumxrq  *intrinsic.Array[float32]
		ymnrt   *intrinsic.Array[float32]
		_, _, _ = yqr, sumxrq, ymnrt
	)
	yqr = intrinsic.NewArray[float32](nil, 256)
	sumxrq = intrinsic.NewArray[float32](nil, 512)
	ymnrt = intrinsic.NewArray[float32](nil, 3)
	var (
		matrix *intrinsic.Array[float32]
		_      = matrix
	)
	matrix = intrinsic.NewArray[float32](nil, 10, 20)
	var (
		counts *intrinsic.Array[int32]
		_      = counts
	)
	counts = intrinsic.NewArray[int32](nil, 100)
	intrinsic.Print("LEVEL 28: COMMON block arrays initialized")
	holdrt.yqr.Set(1.5, 1)
	holdrt.sumxrq.Set(99.90000000000001, 512)
	holdrt.ymnrt.Set(3.14, 2)
	holdrt.matrix.Set(42.5, 5, 10)
	stats.counts.Set(42, 50)
	intrinsic.Print("LEVEL 28: YQR(1) =", holdrt.yqr.At(1))
	intrinsic.Print("LEVEL 28: SUMXRQ(512) =", holdrt.sumxrq.At(512))
	intrinsic.Print("LEVEL 28: YMNRT(2) =", holdrt.ymnrt.At(2))
	intrinsic.Print("LEVEL 28: MATRIX(5,10) =", holdrt.matrix.At(5, 10))
	intrinsic.Print("LEVEL 28: COUNTS(50) =", stats.counts.At(50))
}
func LEVEL29() {
	var (
		npaa intrinsic.PointerTo[float64]
		aa   intrinsic.PointerTo[float64]
		_, _ = npaa, aa
	)
	var (
		npii intrinsic.PointerTo[int32]
		ii   intrinsic.PointerTo[int32]
		_, _ = npii, ii
	)
	var (
		npll intrinsic.PointerTo[bool]
		ll   intrinsic.PointerTo[bool]
		_, _ = npll, ll
	)
	var (
		n             int32
		m             int32
		maxmum        int32
		maxdm1        int32
		maxdef        int32
		_, _, _, _, _ = n, m, maxmum, maxdm1, maxdef
	)
	var (
		mat *intrinsic.Array[int32]
		_   = mat
	)
	mat = intrinsic.NewArray[int32](nil, 2, 2)
	var (
		a    intrinsic.CharacterArray
		b    intrinsic.CharacterArray
		_, _ = a, b
	)
	var (
		c *intrinsic.Array[intrinsic.CharacterArray]
		_ = c
	)
	c = intrinsic.NewArray[intrinsic.CharacterArray](nil, 4)
	var (
		i_defalt *intrinsic.Array[int32]
		_        = i_defalt
	)
	i_defalt = intrinsic.NewArray[int32](nil, 2)
	i_defalt.Set(125269879, 1)
	i_defalt.Set(125269879, 2)
	intrinsic.Print("LEVEL 29: Advanced features test")
	mat.Set(64, 1, 2)
	maxdm1 = 100
	npaa = intrinsic.MALLOC[float64](maxdm1 * 8)
	if npaa.DataUnsafe() == nil {
		intrinsic.Stop(69)
	}
	npii = intrinsic.PointerFrom[int32](npaa)
	npll = intrinsic.PointerFrom[bool](npii)
	m = 1
	maxdef = intrinsic.MIN[int32](200000, maxdm1)
	for m = 1; m <= maxdef; m += 32768 {
		maxmum = intrinsic.MIN[int32](m+32767, maxdef)
		for n = m; n <= maxmum; n++ {
		}
	}
	intrinsic.Print("CHAR A,B:", a, b)
	intrinsic.Print("CHAR C", c.At(1))
	intrinsic.Print("LEVEL 29: AA(2) ", aa.At(2), "MAT(1,2)", mat.At(1, 2))
	intrinsic.Print("LEVEL 29: Initialized", maxmum-m+1, "elements")
}
func SIMPLE_SUB() {
	intrinsic.Print("LEVEL 7: Inside SIMPLE_SUB")
}
func ADD_VALUES(a int32, b int32, result *int32) {
	*result = a + b
	intrinsic.Print("LEVEL 7: Inside ADD_VALUES")
}
func MODIFY_ARRAY(arr *intrinsic.Array[int32], n int32) {
	var (
		i int32
		_ = i
	)
	for i = 1; i <= n; i++ {
		arr.Set(arr.At(int(i))*2, int(i))
	}
	intrinsic.Print("LEVEL 7: Inside MODIFY_ARRAY")
}
func FACTORIAL(n int32) (factorial int32) {
	var (
		i      int32
		result int32
		_, _   = i, result
	)
	result = 1
	for i = 1; i <= n; i++ {
		result = result * i
	}
	factorial = result
	return
}
func SQUARE_ROOT(x float32) (square_root float32) {
	square_root = intrinsic.SQRT(x)
	return
}
func FIBONACCI(n int32) (fibonacci int32) {
	var (
		a          int32
		b          int32
		temp       int32
		i          int32
		_, _, _, _ = a, b, temp, i
	)
	if n <= 1 {
		fibonacci = n
	}
	a = 0
	b = 1
	for i = 2; i <= n; i++ {
		temp = a + b
		a = b
		b = temp
	}
	fibonacci = b
	return
}

var holdrt = struct {
	yqr    *intrinsic.Array[float32]
	sumxrq *intrinsic.Array[float32]
	ymnrt  *intrinsic.Array[float32]
	matrix *intrinsic.Array[float32]
}{yqr: intrinsic.NewArray[float32](nil, 256), sumxrq: intrinsic.NewArray[float32](nil, 512), ymnrt: intrinsic.NewArray[float32](nil, 3), matrix: intrinsic.NewArray[float32](nil, 10, 20)}
var shared struct {
	x int32
	y int32
	z float32
}
var stats = struct {
	counts *intrinsic.Array[int32]
}{counts: intrinsic.NewArray[int32](nil, 100)}
