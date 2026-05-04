package main

import (
	"github.com/soypat/go-fortran/intrinsic"
	"github.com/soypat/go-fortran/intrinsic/fortio"
)

func main() {
	GOLDEN()
}
func GOLDEN() {
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
	LEVEL18()
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
	LEVEL30()
	LEVEL31()
	LEVEL32()
	LEVEL33()
	LEVEL34()
	LEVEL35()
	LEVEL36()
	LEVEL37()
	LEVEL38()
	LEVEL39()
	LEVEL40()
	LEVEL41()
	LEVEL42()
	LEVEL43()
	LEVEL44()
	fenv.Stop(0)
}
func LEVEL01() {
	fenv.Print("LEVEL 1: Hello, World!")
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
		message intrinsic.CharacterArray = intrinsic.NewCharacterArray(20)
		_                                = message
	)
	var (
		a intrinsic.CharacterArray = intrinsic.NewCharacterArray(1)
		_                          = a
	)
	a.SetFromString("a")
	i = 42
	x = 3.14159
	flag = true
	message.SetFromString("Variables assigned")
	fenv.Print("LEVEL 2: i =", i, ", x =", x)
	fenv.Print("LEVEL 2: flag =", flag)
	fenv.Print("LEVEL 2:", message)
	fenv.Print("LEVEL 2:", a, a)
	fenv.Print(a, a, a, i, a)
	fenv.Print(i, a, i, a)
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
	fenv.Print("LEVEL 3: j =", j, ", k =", k)
	fenv.Print("LEVEL 3: y =", y, ", z =", z)
}
func LEVEL04() {
	var (
		i int32
		_ = i
	)
	var (
		x    float32
		test float32
		_, _ = x, test
	)
	var (
		flag bool
		_    = flag
	)
	i = 42
	x = 3.14159
	test = 0.0
	flag = true
	if i > 40 {
		fenv.Print("LEVEL 4: i is greater than 40")
	}
	if flag {
		fenv.Print("LEVEL 4: flag is true")
	} else {
		fenv.Print("LEVEL 4: flag is false")
	}
	if x < 3.0 {
		fenv.Print("LEVEL 4: x < 3.0")
	} else if x < 4.0 {
		fenv.Print("LEVEL 4: 3.0 <= x < 4.0")
	} else {
		fenv.Print("LEVEL 4: x >= 4.0")
	}
}
func LEVEL05() {
	var (
		arr1 = intrinsic.NewArray[int32](nil, 5)
		_    = arr1
	)
	var (
		matrix = intrinsic.NewArray[float32](nil, 3, 3)
		_      = matrix
	)
	arr1.Set(10, 1)
	arr1.Set(20, 2)
	arr1.Set(30, 3)
	arr1.Set(40, 4)
	arr1.Set(50, 5)
	fenv.Print("LEVEL 5: arr1(1) =", arr1.At(1))
	fenv.Print("LEVEL 5: arr1(3) =", arr1.At(3))
	fenv.Print("LEVEL 5: arr1(5) =", arr1.At(5))
	matrix.Set(1.0, 1, 1)
	matrix.Set(0.0, 1, 2)
	matrix.Set(0.0, 1, 3)
	matrix.Set(0.0, 2, 1)
	matrix.Set(1.0, 2, 2)
	matrix.Set(0.0, 2, 3)
	matrix.Set(0.0, 3, 1)
	matrix.Set(0.0, 3, 2)
	matrix.Set(1.0, 3, 3)
	fenv.Print("LEVEL 5: matrix(1,1) =", matrix.At(1, 1))
	fenv.Print("LEVEL 5: matrix(2,2) =", matrix.At(2, 2))
}
func LEVEL06() {
	var (
		i    int32
		j    int32
		_, _ = i, j
	)
	var (
		arr1 = intrinsic.NewArray[int32](nil, 5)
		_    = arr1
	)
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
	fenv.Print("LEVEL 6: sum of arr1 =", sum_val)
	sum_val = 0
	for i = 1; i <= 3; i++ {
		for j = 1; j <= 3; j++ {
			sum_val = sum_val + 1
		}
	}
	fenv.Print("LEVEL 6: nested loop count =", sum_val)
}
func LEVEL07() {
	var (
		arr1 = intrinsic.NewArray[int32](nil, 5)
		_    = arr1
	)
	var (
		result int32
		_      = result
	)
	var (
		x    float32 = 1
		y    float32 = 2
		_, _         = x, y
	)
	arr1.Set(10, 1)
	arr1.Set(20, 2)
	arr1.Set(30, 3)
	arr1.Set(40, 4)
	arr1.Set(50, 5)
	SIMPLE_SUB()
	ADD_VALUES(10, 20, &result)
	fenv.Print("LEVEL 7: ADD_VALUES(10, 20) =", result)
	MODIFY_ARRAY(arr1, 5)
	fenv.Print("LEVEL 7: arr1 after modify:", arr1.At(1), arr1.At(2), arr1.At(3))
	MULDST(&x, y)
	fenv.Print("LEVEL 7: x=x*y", x, y)
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
	fenv.Print("LEVEL 8: FACTORIAL(5) =", fact_result)
	sqrt_result = SQUARE_ROOT(16.0)
	fenv.Print("LEVEL 8: SQUARE_ROOT(16.0) =", sqrt_result)
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
	fenv.Print("LEVEL 9: FIBONACCI(7) =", fib_result)
	i = 1
	sum_val = 0
	for i <= 10 {
		sum_val = sum_val + i
		i = i + 1
	}
	fenv.Print("LEVEL 9: sum 1 to 10 =", sum_val)
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
	fenv.Print("LEVEL 10: complex expr =", expr_result)
	cond1 = (i > 5) && (j < 100)
	cond2 = (x >= 3.0) || (y <= 1.0)
	cond3 = !flag
	fenv.Print("LEVEL 10: cond1 =", cond1, ", cond2 =", cond2)
	fenv.Print("LEVEL 10: cond3 =", cond3)
}
func LEVEL11() {
	var (
		str1 intrinsic.CharacterArray = intrinsic.NewCharacterArray(10)
		str2 intrinsic.CharacterArray = intrinsic.NewCharacterArray(10)
		_, _                          = str1, str2
	)
	var (
		str3 intrinsic.CharacterArray = intrinsic.NewCharacterArray(20)
		_                             = str3
	)
	str1.SetFromString("Hello")
	str2.SetFromString("World")
	str3.SetConcatString(str1.String(), " ", str2.String())
	fenv.Print("LEVEL 11: concatenation:", str3)
}
func LEVEL12() {
	var (
		angle                           float32
		sin_val                         float32
		cos_val                         float32
		abs_val                         float32
		log_val                         float32
		v1                              = intrinsic.NewArray[float32](nil, 3)
		v2                              = intrinsic.NewArray[float32](nil, 3)
		dot_val                         float32
		zabs                            float32
		zreal                           float32
		zimag                           float32
		_, _, _, _, _, _, _, _, _, _, _ = angle, sin_val, cos_val, abs_val, log_val, v1, v2, dot_val, zabs, zreal, zimag
	)
	var (
		zzabs   float64
		zzreal  float64
		zzimag  float64
		_, _, _ = zzabs, zzreal, zzimag
	)
	var (
		z    complex64
		z2   complex64
		_, _ = z, z2
	)
	var (
		zz complex128
		_  = zz
	)
	var (
		i             int32
		j             int32
		k             int32
		max_val       int32
		min_val       int32
		_, _, _, _, _ = i, j, k, max_val, min_val
	)
	zz = intrinsic.DCMPLX2[float64](3.0, 1.0)
	for i = 1; i <= 3; i++ {
		v1.Set(float32(i), int(i))
		v2.Set(float32(i), int(i))
	}
	i = 11
	j = 52
	k = 84
	z = intrinsic.CMPLX2[int32](1, 2)
	angle = 0.5
	sin_val = intrinsic.SIN[float32](angle)
	cos_val = intrinsic.COS[float32](angle)
	abs_val = float32(intrinsic.ABS[float32](-5.5))
	max_val = intrinsic.MAX[int32](i, j, k)
	min_val = intrinsic.MIN[int32](10, 20, 5)
	log_val = 2.0 * intrinsic.LOG[float32](angle)
	dot_val = intrinsic.DOT_PRODUCT(v1, v2)
	zabs = intrinsic.CABS(z)
	zreal = intrinsic.REALPART(z)
	zimag = intrinsic.AIMAG(z)
	zzabs = intrinsic.CDABS(zz)
	zzreal = intrinsic.DREALPART(zz)
	zzimag = intrinsic.DIMAG(zz)
	fenv.Print("LEVEL 12: SIN(0.5) =", sin_val)
	fenv.Print("LEVEL 12: COS(0.5) =", cos_val)
	fenv.Print("LEVEL 12: ABS(-5.5) =", abs_val)
	fenv.Print("LEVEL 12: MAX =", max_val, ", MIN =", min_val)
	fenv.Print("LEVEL 12: dot product:", dot_val)
	fenv.Print("LEVEL 12: z=1+2i, ZABS,ZREAL,ZIMAG ", zabs, zreal, zimag)
	fenv.Print("LEVEL 12: zz=3+1i, ZZABS,ZZREAL,ZZIMAG", zzabs, zzreal, zzimag)
}
func LEVEL13() {
	var (
		i       int32
		sum_val int32
		count   int32
		_, _, _ = i, sum_val, count
	)
	var (
		arr = intrinsic.NewArray[int32](nil, 10)
		_   = arr
	)
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
	fenv.Print("LEVEL 13: sum of positive =", sum_val)
	count = 0
	for i = 1; i <= 10; i++ {
		if arr.At(int(i)) > 7 {
			break
		}
		count = count + 1
	}
	fenv.Print("LEVEL 13: count before >7 =", count)
	for i = 1; i <= 3; i++ {
		count = i
	}
	fenv.Print("LEVEL 13: last count =", count)
	for i = 1; i <= 2; i++ {
		fenv.Print("LEVEL 13: do,goto,end do", i)
		goto label30
		fenv.Print("LEVEL 13: not printed", i)
		goto label30
	label30:
	}
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
	fenv.Print("LEVEL 14: x =", x, ", y =", y)
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
	fenv.Print("LEVEL 15: choice =", choice, ", result =", result)
	choice = 5
	switch choice {
	case 1, 2, 3:
		result = 100
	case 4, 5, 6:
		result = 200
	default:
		result = 999
	}
	fenv.Print("LEVEL 15: choice =", choice, ", result =", result)
	choice = 99
	switch choice {
	case 1:
		result = 10
	case 2:
		result = 20
	default:
		result = 777
	}
	fenv.Print("LEVEL 15: choice =", choice, ", result =", result)
}
func LEVEL16() {
	var (
		str1    intrinsic.CharacterArray = intrinsic.NewCharacterArray(20)
		str2    intrinsic.CharacterArray = intrinsic.NewCharacterArray(20)
		str3    intrinsic.CharacterArray = intrinsic.NewCharacterArray(20)
		_, _, _                          = str1, str2, str3
	)
	var (
		len_val      int32
		len_trim_val int32
		index_val    int32
		_, _, _      = len_val, len_trim_val, index_val
	)
	str1.SetFromString("Hello")
	len_val = int32(str1.Len())
	fenv.Print("LEVEL 16: LEN =", len_val)
	len_trim_val = int32(str1.LenTrim())
	fenv.Print("LEVEL 16: LEN_TRIM =", len_trim_val)
	str2.SetFromString(str1.Trim().String())
	fenv.Print("LEVEL 16: TRIM =", str2)
	str1.SetFromString("Hello World")
	index_val = int32(str1.Index("World"))
	fenv.Print("LEVEL 16: INDEX =", index_val)
	str1.SetFromString("   Left")
	str2.SetFromString(str1.AdjustL().String())
	fenv.Print("LEVEL 16: ADJUSTL =", str2)
	str1.SetFromString("Right   ")
	str3.SetFromString(str1.AdjustR().String())
	fenv.Print("LEVEL 16: ADJUSTR =", str3)
	str1.SetFromString("abcdef")
	str3.SetFromString(str1.Substring(2, 4))
	str1.SetSubstring(2, 3, "z")
	fenv.Print("LEVEL 16: str3 =", str3)
}
func LEVEL17() {
	var (
		matrix = intrinsic.NewArray[int32](nil, 3, 4)
		_      = matrix
	)
	var (
		vector = intrinsic.NewArray[int32](nil, 5)
		_      = vector
	)
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
	fenv.Print("LEVEL 17: SIZE(matrix) =", size_total)
	size_dim1 = int32(matrix.SizeDim(1))
	fenv.Print("LEVEL 17: SIZE(matrix,1) =", size_dim1)
	size_dim2 = int32(matrix.SizeDim(2))
	fenv.Print("LEVEL 17: SIZE(matrix,2) =", size_dim2)
	size_total = int32(vector.Size())
	fenv.Print("LEVEL 17: SIZE(vector) =", size_total)
	lb = int32(matrix.LowerDim(1))
	fenv.Print("LEVEL 17: LBOUND(matrix,1) =", lb)
	lb = int32(matrix.LowerDim(2))
	fenv.Print("LEVEL 17: LBOUND(matrix,2) =", lb)
	ub = int32(matrix.UpperDim(1))
	fenv.Print("LEVEL 17: UBOUND(matrix,1) =", ub)
	ub = int32(matrix.UpperDim(2))
	fenv.Print("LEVEL 17: UBOUND(matrix,2) =", ub)
}
func LEVEL18() {
	var (
		vec = new(intrinsic.Array[int32])
		_   = vec
	)
	var (
		mat = new(intrinsic.Array[int32])
		_   = mat
	)
	vec.Allocate(5)
	vec.Set(10, 1)
	vec.Set(20, 2)
	vec.Set(30, 3)
	fenv.Print("LEVEL 18: vec(1) =", vec.At(1))
	fenv.Print("LEVEL 18: vec(3) =", vec.At(3))
	fenv.Print("LEVEL 18: SIZE(vec) =", int32(vec.Size()))
	mat.Allocate(2, 3)
	mat.Set(100, 1, 1)
	mat.Set(200, 2, 3)
	fenv.Print("LEVEL 18: mat(1,1) =", mat.At(1, 1))
	fenv.Print("LEVEL 18: mat(2,3) =", mat.At(2, 3))
	fenv.Print("LEVEL 18: SIZE(mat) =", int32(mat.Size()))
	vec.Deallocate()
	mat.Deallocate()
	fenv.Print("LEVEL 18: Arrays deallocated")
}
func LEVEL19() {
	SET_COMMON_VALUES()
	PRINT_COMMON_VALUES()
}
func SET_COMMON_VALUES() {
	x := intrinsic.UnallocatedPtr[int32](1)
	y := intrinsic.UnallocatedPtr[int32](1)
	z := intrinsic.UnallocatedPtr[float32](1)
	shared.Reset()
	intrinsic.DeclareCommon(&x, &shared)
	intrinsic.DeclareCommon(&y, &shared)
	intrinsic.DeclareCommon(&z, &shared)
	x.Set(42, 1)
	y.Set(99, 1)
	z.Set(3.14159, 1)
}
func PRINT_COMMON_VALUES() {
	x := intrinsic.UnallocatedPtr[int32](1)
	y := intrinsic.UnallocatedPtr[int32](1)
	z := intrinsic.UnallocatedPtr[float32](1)
	shared.Reset()
	intrinsic.DeclareCommon(&x, &shared)
	intrinsic.DeclareCommon(&y, &shared)
	intrinsic.DeclareCommon(&z, &shared)
	fenv.Print("LEVEL 19: x =", x.At(1))
	fenv.Print("LEVEL 19: y =", y.At(1))
	fenv.Print("LEVEL 19: z =", z.At(1))
}
func LEVEL20() {
	var (
		a          int32
		b          int32
		c          int32
		i          int32
		_, _, _, _ = a, b, c, i
	)
	var (
		x       float32
		y       float32
		vec1    = intrinsic.NewArray[float32](nil, 3)
		_, _, _ = x, y, vec1
	)
	var (
		repeat = intrinsic.NewCharacterArrayArray(3, 2)
		_      = repeat
	)
	a = 10
	b = 20
	c = 30
	x = 3.14
	y = 2.71
	repeat.AtPtr(1).SetFromString("REP")
	repeat.AtPtr(2).SetFromString("REP")
	vec1.Set(1.2, 1)
	vec1.Set(1.2, 2)
	vec1.Set(2.0, 3)
	fenv.Print("LEVEL 20: a =", a)
	fenv.Print("LEVEL 20: b =", b)
	fenv.Print("LEVEL 20: c =", c)
	fenv.Print("LEVEL 20: x =", x)
	fenv.Print("LEVEL 20: y =", y)
	fenv.Print("LEVEL 20", repeat.At(1), repeat.At(2))
	fenv.Print("LEVEL 20:", vec1.At(1), vec1.At(2), vec1.At(3))
}
func LEVEL21() {
	var (
		x      int32
		choice int32
		_, _   = x, choice
	)
	var (
		test float32
		_    = test
	)
	test = 0.0
	x = -5
	if jmpSelect := x; jmpSelect < 0 {
		goto label10
	} else if jmpSelect == 0 {
		goto label20
	} else {
		goto label30
	}
	goto label10
label10:
	{
		fenv.Print("LEVEL 21: x is negative")
	}
	goto label90
	goto label20
label20:
	{
		fenv.Print("LEVEL 21: x is zero")
	}
	goto label90
	goto label30
label30:
	{
		fenv.Print("LEVEL 21: x is positive")
	}
	if jmpSelect := test - 1e-14; jmpSelect < 0 {
		goto label40
	} else if jmpSelect == 0 {
		goto label50
	} else {
		goto label60
	}
	goto label40
label40:
	{
		fenv.Print("LEVEL 21: test negative")
	}
	goto label90
	goto label50
label50:
	{
		fenv.Print("LEVEL 21: test zero")
	}
	goto label90
	goto label60
label60:
	{
		fenv.Print("LEVEL 21: test positive")
	}
	goto label90
label90:
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
		fenv.Print("LEVEL 21: Choice was 1")
	}
	goto label400
	goto label200
label200:
	{
		fenv.Print("LEVEL 21: Choice was 2")
	}
	goto label400
	goto label300
label300:
	{
		fenv.Print("LEVEL 21: Choice was 3")
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
	fenv.Print("LEVEL 23: MAX_SIZE =", max_size)
	fenv.Print("LEVEL 23: PI =", pi)
	fenv.Print("LEVEL 23: TAU =", tau)
}
func LEVEL24() {
	var (
		vec1 = intrinsic.NewArray[int32](nil, 3)
		vec3 = intrinsic.NewArray[int32](nil, 3)
		_, _ = vec1, vec3
	)
	var (
		vec2 = intrinsic.NewArray[int32](nil, 5)
		_    = vec2
	)
	vec1 = intrinsic.NewArray[int32]([]int32{10, 20, 30}, 3)
	fenv.Print("LEVEL 24: vec1(1) =", vec1.At(1))
	fenv.Print("LEVEL 24: vec1(2) =", vec1.At(2))
	fenv.Print("LEVEL 24: vec1(3) =", vec1.At(3))
	vec2 = intrinsic.NewArray[int32]([]int32{100, 200, 300, 400, 500}, 5)
	vec3 = intrinsic.NewArray[int32]([]int32{20, 30, 40}, 3)
	fenv.Print("LEVEL 24: vec2(1) =", vec2.At(1))
	fenv.Print("LEVEL 24: vec2(5) =", vec2.At(5))
	if intrinsic.ALL(intrinsic.ArraySetEqual[int32](nil, vec1, vec3)) {
		fenv.Print("LEVEL 24: vec eq")
	} else {
		fenv.Print("LEVEL 24: vec neq")
	}
	if intrinsic.ALL(intrinsic.ArraySetEqual[int32](nil, vec1, intrinsic.NewArray[int32]([]int32{10, 20, 30}, 3))) {
		fenv.Print("LEVEL 24: vecinline eq")
	} else {
		fenv.Print("LEVEL 24: vecinline neq")
	}
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
	fenv.Print("LEVEL 25: i1 =", i1)
	fenv.Print("LEVEL 25: i2 =", i2)
	fenv.Print("LEVEL 25: i4 =", i4)
	fenv.Print("LEVEL 25: i8 =", i8)
	fenv.Print("LEVEL 25: r4 =", r4)
	fenv.Print("LEVEL 25: r8 =", r8)
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
	fenv.Print("LEVEL 26: hex_val =", hex_val)
	fenv.Print("LEVEL 26: oct_val =", oct_val)
	fenv.Print("LEVEL 26: bin_val =", bin_val)
	d1 = float64(1.0)
	d2 = float64(123.0)
	d4 = float64(2.718281828)
	fenv.Print("LEVEL 26: d1 =", d1)
	fenv.Print("LEVEL 26: d2 =", d2)
	fenv.Print("LEVEL 26: d4 =", d4)
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
		root3 float64 = intrinsic.SQRT[float64](3.0)
		_             = root3
	)
	var (
		pi float64 = 4.0 * intrinsic.ATAN[float64](1.0)
		_          = pi
	)
	var (
		result float64
		_      = result
	)
	result = float64(factor * root3 * float64(ncomp))
	fenv.Print("LEVEL 27: ncomp =", ncomp)
	fenv.Print("LEVEL 27: factor =", factor)
	fenv.Print("LEVEL 27: root3 =", root3)
	fenv.Print("LEVEL 27: PI =", pi)
	fenv.Print("LEVEL 27: result =", result)
}
func LEVEL28() {
	// Implicit declarations.
	var alphc = intrinsic.NewArray[float32](nil, 2, 2)
	var _ = alphc
	yqr := intrinsic.UnallocatedArray[float32](256)
	sumxrq := intrinsic.UnallocatedArray[float32](512)
	ymnrt := intrinsic.UnallocatedArray[float32](3)
	matrix := intrinsic.UnallocatedArray[float32](10, 20)
	holdrt.Reset()
	intrinsic.DeclareCommon(yqr, &holdrt)
	intrinsic.DeclareCommon(sumxrq, &holdrt)
	intrinsic.DeclareCommon(ymnrt, &holdrt)
	intrinsic.DeclareCommon(matrix, &holdrt)
	counts := intrinsic.UnallocatedArray[int32](100)
	alphc = intrinsic.UnallocatedArray[float32](2, 2)
	stats.Reset()
	intrinsic.DeclareCommon(counts, &stats)
	intrinsic.DeclareCommon(alphc, &stats)
	fenv.Print("LEVEL 28: COMMON block arrays initialized")
	yqr.Set(1.5, 1)
	sumxrq.Set(99.90000000000001, 512)
	ymnrt.Set(3.14, 2)
	matrix.Set(42.5, 5, 10)
	counts.Set(42, 50)
	alphc.Set(1.0, 1, 1)
	alphc.Set(2.0, 1, 2)
	alphc.Set(3.0, 2, 1)
	alphc.Set(4.0, 2, 2)
	fenv.Print("LEVEL 28: YQR(1) =", yqr.At(1))
	fenv.Print("LEVEL 28: SUMXRQ(512) =", sumxrq.At(512))
	fenv.Print("LEVEL 28: YMNRT(2) =", ymnrt.At(2))
	fenv.Print("LEVEL 28: MATRIX(5,10) =", matrix.At(5, 10))
	fenv.Print("LEVEL 28: COUNTS(50) =", counts.At(50))
	fenv.Print("LEVEL 28: IMPLICIT ALPHC=", alphc.At(1, 1), alphc.At(1, 2), alphc.At(2, 1), alphc.At(2, 2))
}
func LEVEL29() {
	var (
		firstletter int8 = 97
		_                = firstletter
	)
	var (
		letters int32
		_       = letters
	)
	var (
		mat = intrinsic.NewArray[int8](nil, 2, 2)
		_   = mat
	)
	var (
		mat4 = intrinsic.NewArray[int32](nil, 1, 2)
		_    = mat4
	)
	var (
		a intrinsic.CharacterArray = intrinsic.NewCharacterArray(4)
		_                          = a
	)
	intrinsic.Equivalence(&a, intrinsic.PointerOff(mat, mat.AtOffset(1, 1)), intrinsic.PointerOff(mat4, mat4.AtOffset(1, 2)))
	letters = 1633837924
	var (
		f intrinsic.PointerTo[float32]
		_ = f
	)
	var (
		n intrinsic.PointerTo[int32]
		_ = n
	)
	f = intrinsic.MALLOC[float32](4)
	n = intrinsic.PointerFrom[int32](f)
	mat.Set(firstletter, 1, 1)
	mat.Set(firstletter+1, 1, 2)
	mat.Set(firstletter+2, 2, 1)
	mat.Set(firstletter+3, 2, 2)
	fenv.Print("LEVEL 29: byte mat ", a)
	mat4.Set(letters, 1, 2)
	fenv.Print("LEVEL 29: uint32 mat ", a)
	f.Set(float32(float32(1)), 1)
	fenv.Print("LEVEL 29: linked float=1,int", f.At(1), n.At(1))
	n.Set(int32(1109917696), 1)
	fenv.Print("LEVEL 29: linked float,int=1109917696", f.At(1), n.At(1))
}
func LEVEL30() {
	var (
		a    intrinsic.CharacterArray = intrinsic.NewCharacterArray(1)
		b    intrinsic.CharacterArray = intrinsic.NewCharacterArray(1)
		_, _                          = a, b
	)
	var (
		c = intrinsic.NewCharacterArrayArray(1, 4)
		_ = c
	)
	var (
		mat = intrinsic.NewArray[int32](nil, 2, 2)
		_   = mat
	)
	var (
		defalt intrinsic.PointerTo[float64]
		_      = defalt
	)
	var (
		i_defalt = intrinsic.NewArray[int32](nil, 2)
		_        = i_defalt
	)
	i_defalt.Set(125269879, 1)
	i_defalt.Set(125269879, 2)
	intrinsic.Equivalence(&defalt, i_defalt)
	intrinsic.Equivalence(c.AtPtr(1), intrinsic.PointerOff(mat, mat.AtOffset(1, 1)))
	intrinsic.Equivalence(&a, &b, intrinsic.PointerOff(mat, mat.AtOffset(1, 2)))
	mat.Set(64, 1, 1)
	mat.Set(97, 1, 2)
	fenv.Print("LEVEL 30: CHAR A,B:", a, b)
	fenv.Print("LEVEL 30: CHAR C:", c.At(1))
	fenv.Print("LEVEL 30: DEFALT", defalt.At(1), i_defalt.At(1), i_defalt.At(2))
}
func LEVEL31() {
	const k int32 = 16
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
		m     int32
		inits int32 = 0
		_, _        = m, inits
	)
	npaa = intrinsic.MALLOC[float64](k * 8)
	aa = npaa
	if npaa.DataUnsafe() == nil {
		fenv.Stop(69)
	}
	npii = intrinsic.PointerFrom[int32](npaa)
	ii = npii
	for m = 1; m <= k; m += 2 {
		inits = inits + 1
		aa.Set(float64(inits), int(m))
		goto label900
	label900:
	}
	fenv.Print("LEVEL 31: INITS", inits)
	fenv.Print("LEVEL 31: AA(1),AA(2),AA(3),AA(4)", aa.At(1), aa.At(2), aa.At(3), aa.At(4))
}
func LEVEL32() {
	var (
		a = intrinsic.NewCharacterArrayArray(10, 2, 2)
		_ = a
	)
	var (
		b *intrinsic.Array[intrinsic.CharacterArray] = intrinsic.NewCharacterArrayFromStrings(3, []string{"ABC", "CBA"}, 2)
		_                                            = b
	)
	a.AtPtr(1, 1).SetFromString("ABC")
	a.AtPtr(1, 2).SetFromString("DEFGH")
	a.AtPtr(2, 1).SetFromString("GHI")
	a.AtPtr(2, 2).SetFromString("JKL")
	fenv.Print("LEVEL 32:", a.At(1, 1), a.At(1, 2), a.At(2, 1), a.At(2, 2))
	fenv.Print("LEVEL 32:", b.At(1), b.At(2))
	fenv.Print("LEVEL 32: substr", a.At(1, 2).Substring(1, 3))
}
func LEVEL33() {
	var (
		nm int32 = 3
		_        = nm
	)
	var (
		i       int32
		j       int32
		Range   int32
		_, _, _ = i, j, Range
	)
	var (
		xsn         = intrinsic.NewArray[float64](nil, int(nm), int(nm))
		cof_com_tor = intrinsic.NewArray[float64](nil, int(nm), int(nm))
		_, _        = xsn, cof_com_tor
	)
	for i = 1; i <= nm; i++ {
		for j = 1; j <= nm; j++ {
			xsn.Set(float64(0.0), int(i), int(j))
			cof_com_tor.Set(float64(1.0), int(i), int(j))
		}
	}
	xsn.Set(float64(1.0), 1, 1)
	cof_com_tor.Set(float64(10.0), 1, 1)
	cof_com_tor.Set(float64(20.0), 2, 2)
	cof_com_tor.Set(float64(20.0), 3, 3)
	Range = 2
	intrinsic.ArraySetAdd(xsn.View(intrinsic.R(1, int(Range)), intrinsic.R(int(Range), 3)), xsn.View(intrinsic.R(1, int(Range)), intrinsic.R(int(Range), 3)), cof_com_tor.View(intrinsic.R(1, int(Range)), intrinsic.R(int(Range), 3)))
	for i = 1; i <= nm; i++ {
		fenv.Print("LEVEL 33:", xsn.At(int(i), 1), xsn.At(int(i), 2), xsn.At(int(i), 3))
	}
}
func LEVEL34() {
	fenv.Print("LEVEL 34:", (2*(2/2))*2, (3*(3/2))*2, 4.189*intrinsic.POW[float32](float32(23.0), float32(3)))
}
func LEVEL35() {
	var (
		m       int32
		n       int32
		i       int32
		_, _, _ = m, n, i
	)
	var (
		x    float32
		wh   *intrinsic.Array[float32] = intrinsic.NewArray[float32]([]float32{1.0, 2.0, 3.0, 0.5, 1.0, 1.5}, 6)
		_, _                           = x, wh
	)
	m = 3
	n = 5
	x = 2.5
	fenv.WriteWithSpec(fortio.IOSpec{UNIT: 6, FMT: fortio.DefaultFormat()}, "LEVEL 35: Hello from WRITE")
	fenv.WriteWithSpec(fortio.IOSpec{UNIT: 6, FMT: fortio.DefaultFormat()}, m, n, x)
	fenv.WriteWithSpec(fortio.IOSpec{UNIT: 6, FMT: fortio.DefaultFormat()}, "LEVEL 35: Values:", m, n)
	fenv.WriteWithSpec(fortio.IOSpec{UNIT: 6, FMT: fortio.NewFormat(fortio.FormatDescriptor{Type: 'S', Literal: "LEVEL 35: Formatted output line"})})
	fenv.WriteWithSpec(fortio.IOSpec{UNIT: 6, FMT: fortio.NewFormat(fortio.FormatDescriptor{Type: 'S', Literal: "LEVEL35: m="}, fortio.FormatDescriptor{Type: 'I', Width: 3, Repeat: 1}, fortio.FormatDescriptor{Type: 'S', Literal: " n="}, fortio.FormatDescriptor{Type: 'I', Width: 3, Repeat: 1}, fortio.FormatDescriptor{Type: 'S', Literal: " x="}, fortio.FormatDescriptor{Type: 'F', Width: 5, Precision: 2, Repeat: 1})}, m, n, x)
	goto label220
label220:
	{
	}
	goto label230
label230:
	{
	}
	{
		writeArgs := make([]any, 0)
		for i := 1; i <= int(n); i += 1 {
			writeArgs = append(writeArgs, wh.At(int(i)))
		}
		fenv.Write(6, fortio.NewFormat(fortio.FormatDescriptor{Type: 'S', Literal: "LEVEL35: Newline:"}, fortio.FmtNewline, fortio.FormatDescriptor{Type: 'S', Literal: "LEVEL35: WHI/WR ="}, fortio.FormatDescriptor{Type: 'E', Width: 12, Precision: 4, Repeat: 6}), writeArgs...)
	}
	goto label240
label240:
	{
	}
}
func LEVEL36() {
	// Implicit declarations.
	var delta = intrinsic.NewArray[float32](nil, 3)
	var _ = delta
	d1k := intrinsic.UnallocatedPtr[float32](1)
	d2k := intrinsic.UnallocatedPtr[float32](1)
	d3k := intrinsic.UnallocatedPtr[float32](1)
	blk.Reset()
	intrinsic.DeclareCommon(&d1k, &blk)
	intrinsic.DeclareCommon(&d2k, &blk)
	intrinsic.DeclareCommon(&d3k, &blk)
	intrinsic.Equivalence(&d1k, delta)
	delta.Set(1.0, 1)
	delta.Set(2.0, 2)
	delta.Set(3.0, 3)
	fenv.Print("LEVEL 36: Equiv d=", d1k.At(1), d2k.At(1), d3k.At(1))
	BLKINVDECL()
	BLKDECL()
}
func LEVEL37() {
	var (
		iounit              int32
		x                   int32
		y                   int32
		rstat1              int32 = -1
		rstat2              int32 = -1
		wstat1              int32 = -1
		wstat2              int32 = -1
		_, _, _, _, _, _, _       = iounit, x, y, rstat1, rstat2, wstat1, wstat2
	)
	var (
		msg intrinsic.CharacterArray = intrinsic.NewCharacterArray(20)
		_                            = msg
	)
	iounit = 10
	x = 42
	y = 99
	msg.SetFromString("Hello File IO")
	fenv.Open(fortio.OpenSpec{UNIT: iounit, FILE: "test_io.txt", STATUS: fortio.StatusREPLACE, ACTION: fortio.ActionWRITE})
	fenv.WriteWithSpec(fortio.IOSpec{UNIT: iounit, FMT: fortio.NewFormat(fortio.FormatDescriptor{Type: 'A', Repeat: 1}), IOSTAT: &wstat1}, msg)
	fenv.WriteWithSpec(fortio.IOSpec{UNIT: iounit, FMT: fortio.NewFormat(fortio.FormatDescriptor{Type: 'I', Width: 5, Repeat: 1}, fortio.FormatDescriptor{Type: 'I', Width: 5, Repeat: 1}), IOSTAT: &wstat2}, x, y)
	fenv.Close(fortio.CloseSpec{UNIT: iounit})
	x = 0
	y = 0
	fenv.Open(fortio.OpenSpec{UNIT: iounit, FILE: "test_io.txt", STATUS: fortio.StatusOLD, ACTION: fortio.ActionREAD})
	fenv.ReadWithSpec(fortio.IOSpec{UNIT: iounit, FMT: fortio.NewFormat(fortio.FormatDescriptor{Type: 'A', Repeat: 1}), IOSTAT: &rstat1}, &msg)
	fenv.ReadWithSpec(fortio.IOSpec{UNIT: iounit, FMT: fortio.NewFormat(fortio.FormatDescriptor{Type: 'I', Width: 5, Repeat: 1}, fortio.FormatDescriptor{Type: 'I', Width: 5, Repeat: 1}), IOSTAT: &rstat2}, &x, &y)
	fenv.Close(fortio.CloseSpec{UNIT: iounit})
	fenv.Print("LEVEL 37: READ BACK", msg, x, y)
	fenv.Print("LEVEL 37: IOSTAT", rstat1, rstat2, wstat1, wstat2)
}
func LEVEL38() {
	var (
		iounit int32 = 99
		_            = iounit
	)
	var (
		errcode int32
		_       = errcode
	)
	var (
		x       int32
		y       int32
		z       int32
		_, _, _ = x, y, z
	)
	var (
		a    float32
		b    float32
		_, _ = a, b
	)
	x = 10
	y = 20
	z = 30
	a = 1.5
	b = 2.5
	fenv.Open(fortio.OpenSpec{UNIT: iounit, FILE: "test_namelist.txt", STATUS: fortio.StatusREPLACE, ACTION: fortio.ActionWRITE})
	fenv.WriteNamelist(iounit, "TESTDATA", []fortio.NamelistVar{{Name: "x", Ptr: &x}, {Name: "y", Ptr: &y}, {Name: "z", Ptr: &z}, {Name: "a", Ptr: &a}, {Name: "b", Ptr: &b}})
	fenv.Close(fortio.CloseSpec{UNIT: iounit})
	x = 0
	y = 0
	z = 0
	a = 0.0
	b = 0.0
	fenv.Open(fortio.OpenSpec{UNIT: iounit, FILE: "test_namelist.txt", STATUS: fortio.StatusOLD, ACTION: fortio.ActionREAD})
	fenv.ReadNamelist(iounit, "TESTDATA", []fortio.NamelistVar{{Name: "x", Ptr: &x}, {Name: "y", Ptr: &y}, {Name: "z", Ptr: &z}, {Name: "a", Ptr: &a}, {Name: "b", Ptr: &b}})
	fenv.Close(fortio.CloseSpec{UNIT: iounit})
	fenv.Print("LEVEL 38: NAMELIST x,y,z=", x, y, z)
	fenv.Print("LEVEL 38: NAMELIST a,b=", a, b)
}
func LEVEL39() {
	var (
		n int32
		_ = n
	)
	var (
		card intrinsic.CharacterArray = intrinsic.NewCharacterArray(20)
		_                             = card
	)
	n = 0
	fenv.Open(fortio.OpenSpec{UNIT: 39, FILE: "test_io.txt", STATUS: fortio.StatusOLD, ACTION: fortio.ActionREAD})
	goto label10
label10:
	{
		_iostat := fenv.ReadWithSpec(fortio.IOSpec{UNIT: 39, FMT: fortio.NewFormat(fortio.FormatDescriptor{Type: 'A', Repeat: 1})}, &card)
		if _iostat == fortio.IOStatEOF {
			goto label20
		}
	}
	n = n + 1
	goto label10
	goto label20
label20:
	{
		fenv.Close(fortio.CloseSpec{UNIT: 39})
	}
	fenv.Print("LEVEL 39:", n)
}
func LEVEL40() {
	var (
		t float32
		_ = t
	)
	fenv.System("echo 'LEVEL 40: echo from shell'")
	fenv.CpuTime(&t)
}
func LEVEL41() {
	var (
		fmt_str intrinsic.CharacterArray = intrinsic.NewCharacterArray(30)
		_                                = fmt_str
	)
	var (
		x int32
		_ = x
	)
	x = 41
	fmt_str.SetFromString("(A,I2,A)")
	fenv.WriteWithSpec(fortio.IOSpec{UNIT: 6, FMT: fortio.NewFormat(fmt_str.String())}, "LEVEL ", x, ": SUCCESS")
}
func LEVEL42() {
	var (
		fmt = intrinsic.NewCharacterArrayArray(1, 5)
		_   = fmt
	)
	var (
		x int32
		_ = x
	)
	x = 42
	fmt.AtPtr(1).SetFromString("(")
	fmt.AtPtr(2).SetFromString("I")
	fmt.AtPtr(3).SetFromString("2")
	fmt.AtPtr(4).SetFromString(")")
	fmt.AtPtr(5).SetFromString(" ")
	fenv.WriteWithSpec(fortio.IOSpec{UNIT: 6, FMT: fortio.NewFormat(intrinsic.CharacterArrayJoin(fmt))}, x)
	fenv.Print("LEVEL 42: ok")
}
func LEVEL43() {
	// Implicit declarations.
	var (
		ioerr  int32
		nval   int32
		pexist bool
	)
	var _, _, _ = ioerr, nval, pexist
	fenv.Open(fortio.OpenSpec{UNIT: 99, FILE: "no_such_file_43.txt", STATUS: fortio.StatusOLD, IOSTAT: &ioerr})
	if ioerr != 0 {
		fenv.Print("LEVEL 43: open failed as expected")
	} else {
		fenv.ReadWithSpec(fortio.IOSpec{UNIT: 99, FMT: fortio.DefaultFormat(), IOSTAT: &ioerr}, &nval)
		fenv.Close(fortio.CloseSpec{UNIT: 99})
	}
	if !pexist {
		fenv.Print("LEVEL 43: file absent as expected")
	}
}
func LEVEL44() {
	// Implicit declarations.
	var hres float64
	var _ = hres
	var (
		nval int32
		_    = nval
	)
	nval = 7
	hres = float64(float64(nval))
	fenv.Print("LEVEL 44:", hres)
}
func SIMPLE_SUB() {
	fenv.Print("LEVEL 7: Inside SIMPLE_SUB")
}
func ADD_VALUES(a int32, b int32, result *int32) {
	*result = a + b
	fenv.Print("LEVEL 7: Inside ADD_VALUES")
}
func MODIFY_ARRAY(arr *intrinsic.Array[int32], n int32) {
	var (
		i int32
		_ = i
	)
	for i = 1; i <= n; i++ {
		arr.Set(arr.At(int(i))*2, int(i))
	}
	fenv.Print("LEVEL 7: Inside MODIFY_ARRAY")
}
func MULDST(x *float32, y float32) {
	*x = y * *x
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
	square_root = intrinsic.SQRT[float32](x)
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
func BLKINVDECL() {
	// Implicit declarations.
	var (
		d3k intrinsic.PointerTo[float32]
		d2k intrinsic.PointerTo[float32]
		d1k intrinsic.PointerTo[float32]
	)
	var _, _, _ = d3k, d2k, d1k
	d3k = intrinsic.UnallocatedPtr[float32](1)
	d2k = intrinsic.UnallocatedPtr[float32](1)
	d1k = intrinsic.UnallocatedPtr[float32](1)
	blk.Reset()
	intrinsic.DeclareCommon(&d3k, &blk)
	intrinsic.DeclareCommon(&d2k, &blk)
	intrinsic.DeclareCommon(&d1k, &blk)
	fenv.Print("BLKINVDECL: Equiv d1k,d2k,d3k=", d1k.At(1), d2k.At(1), d3k.At(1))
}
func BLKDECL() {
	// Implicit declarations.
	var (
		d1k   intrinsic.PointerTo[float32]
		d2k   intrinsic.PointerTo[float32]
		d3k   intrinsic.PointerTo[float32]
		delta = intrinsic.NewArray[float32](nil, 3)
	)
	var _, _, _, _ = d1k, d2k, d3k, delta
	d1k = intrinsic.UnallocatedPtr[float32](1)
	d2k = intrinsic.UnallocatedPtr[float32](1)
	d3k = intrinsic.UnallocatedPtr[float32](1)
	blk.Reset()
	intrinsic.DeclareCommon(&d1k, &blk)
	intrinsic.DeclareCommon(&d2k, &blk)
	intrinsic.DeclareCommon(&d3k, &blk)
	intrinsic.Equivalence(&d1k, delta)
	fenv.Print("BLKDECL: Equiv delta(1..3)=", delta.At(1), delta.At(2), delta.At(3))
	fenv.Print("BLKDECL: Equiv d1k,d2k,d3k=", d1k.At(1), d2k.At(1), d3k.At(1))
}

var fenv = fortio.NewEnvironment()
var blk = intrinsic.NewCommonBlock("blk", 12)
var holdrt = intrinsic.NewCommonBlock("holdrt", 3884)
var shared = intrinsic.NewCommonBlock("shared", 12)
var stats = intrinsic.NewCommonBlock("stats", 416)
