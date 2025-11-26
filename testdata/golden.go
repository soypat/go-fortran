package main

import (
	"github.com/soypat/go-fortran/intrinsic"
)

var SHARED struct {
	x int32
	y int32
	z float32
}

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
	LEVEL18()
	LEVEL19()
	LEVEL20()
	LEVEL21()
	LEVEL22()
	LEVEL23()
	LEVEL24()
	LEVEL25()

	intrinsic.Exit(0)
}
func LEVEL01() {
	intrinsic.Print("LEVEL 1: Hello, World!")
}

func LEVEL02() {
	var i int32
	var x float32
	var flag bool
	var message intrinsic.CharacterArray = intrinsic.NewCharacterArray(20)
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
		i int32
		j int32
		k int32
	)
	var (
		x float32
		y float32
		z float32
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
	var i int32
	var x float32
	var flag bool
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
	var arr1 = intrinsic.NewArray[int32](5)
	var matrix = intrinsic.NewArray[float32](3, 3)
	arr1.Set(10, int(1))
	arr1.Set(20, int(2))
	arr1.Set(30, int(3))
	arr1.Set(40, int(4))
	arr1.Set(50, int(5))
	intrinsic.Print("LEVEL 5: arr1(1) =", arr1.At(int(1)))
	intrinsic.Print("LEVEL 5: arr1(3) =", arr1.At(int(3)))
	intrinsic.Print("LEVEL 5: arr1(5) =", arr1.At(int(5)))
	matrix.Set(1.0, int(1), int(1))
	matrix.Set(0.0, int(1), int(2))
	matrix.Set(0.0, int(1), int(3))
	matrix.Set(0.0, int(2), int(1))
	matrix.Set(1.0, int(2), int(2))
	matrix.Set(0.0, int(2), int(3))
	matrix.Set(0.0, int(3), int(1))
	matrix.Set(0.0, int(3), int(2))
	matrix.Set(1.0, int(3), int(3))
	intrinsic.Print("LEVEL 5: matrix(1,1) =", matrix.At(int(1), int(1)))
	intrinsic.Print("LEVEL 5: matrix(2,2) =", matrix.At(int(2), int(2)))
}

func LEVEL06() {
	var (
		i int32
		j int32
	)
	var arr1 = intrinsic.NewArray[int32](5)
	var sum_val int32
	arr1.Set(10, int(1))
	arr1.Set(20, int(2))
	arr1.Set(30, int(3))
	arr1.Set(40, int(4))
	arr1.Set(50, int(5))
	sum_val = 0
	for i = 1; i <= 5; i += 1 {
		sum_val = sum_val + arr1.At(int(i))
	}
	intrinsic.Print("LEVEL 6: sum of arr1 =", sum_val)
	sum_val = 0
	for i = 1; i <= 3; i += 1 {
		for j = 1; j <= 3; j += 1 {
			sum_val = sum_val + 1
		}
	}
	intrinsic.Print("LEVEL 6: nested loop count =", sum_val)
}

func LEVEL07() {
	var arr1 = intrinsic.NewArray[int32](5)
	var result int32
	arr1.Set(10, int(1))
	arr1.Set(20, int(2))
	arr1.Set(30, int(3))
	arr1.Set(40, int(4))
	arr1.Set(50, int(5))
	SIMPLE_SUB()
	ADD_VALUES(10, 20, &result)
	intrinsic.Print("LEVEL 7: ADD_VALUES(10, 20) =", result)
	MODIFY_ARRAY(arr1, 5)
	intrinsic.Print("LEVEL 7: arr1 after modify:", arr1.At(int(1)), arr1.At(int(2)), arr1.At(int(3)))
}

func LEVEL08() {
	var fact_result int32
	var sqrt_result float32
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
		i int32
		j int32
		k int32
	)
	var (
		x           float32
		y           float32
		z           float32
		expr_result float32
	)
	var (
		flag  bool
		cond1 bool
		cond2 bool
		cond3 bool
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
	cond1 = i > 5 && j < 100
	cond2 = x >= 3.0 || y <= 1.0
	cond3 = !flag
	intrinsic.Print("LEVEL 10: cond1 =", cond1, ", cond2 =", cond2)
	intrinsic.Print("LEVEL 10: cond3 =", cond3)
}

func LEVEL11() {
	var (
		str1 intrinsic.CharacterArray = intrinsic.NewCharacterArray(10)
		str2 intrinsic.CharacterArray = intrinsic.NewCharacterArray(10)
	)
	var str3 intrinsic.CharacterArray = intrinsic.NewCharacterArray(20)
	str1.SetFromString("Hello")
	str2.SetFromString("World")
	str3.SetFromString(str1.String() + " " + str2.String())
	intrinsic.Print("LEVEL 11: concatenation:", str3)
}

func LEVEL12() {
	var (
		angle   float32
		sin_val float32
		cos_val float32
		abs_val float32
	)
	var (
		i       int32
		j       int32
		k       int32
		max_val int32
		min_val int32
	)
	i = 11
	j = 52
	k = 84
	angle = 0.5
	sin_val = intrinsic.SIN[float32](angle)
	cos_val = intrinsic.COS[float32](angle)
	abs_val = intrinsic.ABS[float32](-5.5)
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
	)
	var arr = intrinsic.NewArray[int32](10)
	arr.Set(5, int(1))
	arr.Set(-3, int(2))
	arr.Set(7, int(3))
	arr.Set(-1, int(4))
	arr.Set(9, int(5))
	arr.Set(2, int(6))
	arr.Set(-4, int(7))
	arr.Set(6, int(8))
	arr.Set(8, int(9))
	arr.Set(1, int(10))
	sum_val = 0
	for i = 1; i <= 10; i += 1 {
		if arr.At(int(i)) < 0 {
			continue
		}
		sum_val = sum_val + arr.At(int(i))
	}
	intrinsic.Print("LEVEL 13: sum of positive =", sum_val)
	count = 0
	for i = 1; i <= 10; i += 1 {
		if arr.At(int(i)) > 7 {
			break
		}
		count = count + 1
	}
	intrinsic.Print("LEVEL 13: count before >7 =", count)
	for i = 1; i <= 3; i += 1 {
		count = i
	}
	intrinsic.Print("LEVEL 13: last count =", count)
}

func LEVEL14() {
	var (
		x int32
		y int32
	)
	goto label100
	x = 999
label100:
	;
	x = 10
	y = 5
	if y == 5 {
		goto label200
	}
	y = 999
label200:
	;
	intrinsic.Print("LEVEL 14: x =", x, ", y =", y)
}

func LEVEL15() {
	var (
		choice int32
		result int32
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
		str1 intrinsic.CharacterArray = intrinsic.NewCharacterArray(20)
		str2 intrinsic.CharacterArray = intrinsic.NewCharacterArray(20)
		str3 intrinsic.CharacterArray = intrinsic.NewCharacterArray(20)
	)
	var (
		len_val      int32
		len_trim_val int32
		index_val    int32
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
	str3.SetFromString(str1.View(int(2), int(4)).String())
	str1.SetRange(int(2), int(3), "z")
	intrinsic.Print("LEVEL 16: str3 =", str3)
}

func LEVEL17() {
	var matrix = intrinsic.NewArray[int32](3, 4)
	var vector = intrinsic.NewArray[int32](5)
	var (
		size_total int32
		size_dim1  int32
		size_dim2  int32
	)
	var (
		lb int32
		ub int32
	)
	size_total = int32(matrix.Size())
	intrinsic.Print("LEVEL 17: SIZE(matrix) =", size_total)
	size_dim1 = int32(matrix.SizeDim(int(1)))
	intrinsic.Print("LEVEL 17: SIZE(matrix,1) =", size_dim1)
	size_dim2 = int32(matrix.SizeDim(int(2)))
	intrinsic.Print("LEVEL 17: SIZE(matrix,2) =", size_dim2)
	size_total = int32(vector.Size())
	intrinsic.Print("LEVEL 17: SIZE(vector) =", size_total)
	lb = int32(matrix.LowerDim(int(1)))
	intrinsic.Print("LEVEL 17: LBOUND(matrix,1) =", lb)
	lb = int32(matrix.LowerDim(int(2)))
	intrinsic.Print("LEVEL 17: LBOUND(matrix,2) =", lb)
	ub = int32(matrix.UpperDim(int(1)))
	intrinsic.Print("LEVEL 17: UBOUND(matrix,1) =", ub)
	ub = int32(matrix.UpperDim(int(2)))
	intrinsic.Print("LEVEL 17: UBOUND(matrix,2) =", ub)
}

func LEVEL18() {
	var vec *intrinsic.Array[int32]
	var mat *intrinsic.Array[int32]
	vec = intrinsic.NewArray[int32](5)
	vec.Set(10, int(1))
	vec.Set(20, int(2))
	vec.Set(30, int(3))
	intrinsic.Print("LEVEL 18: vec(1) =", vec.At(int(1)))
	intrinsic.Print("LEVEL 18: vec(3) =", vec.At(int(3)))
	intrinsic.Print("LEVEL 18: SIZE(vec) =", int32(vec.Size()))
	mat = intrinsic.NewArray[int32](2, 3)
	mat.Set(100, int(1), int(1))
	mat.Set(200, int(2), int(3))
	intrinsic.Print("LEVEL 18: mat(1,1) =", mat.At(int(1), int(1)))
	intrinsic.Print("LEVEL 18: mat(2,3) =", mat.At(int(2), int(3)))
	intrinsic.Print("LEVEL 18: SIZE(mat) =", int32(mat.Size()))
	vec = nil
	mat = nil
	intrinsic.Print("LEVEL 18: Arrays deallocated")
}

func LEVEL19() {
	SET_COMMON_VALUES()
	PRINT_COMMON_VALUES()
}

func LEVEL20() {
	var (
		a int32
		b int32
		c int32
	)
	var (
		x float32
		y float32
	)
	{
		a = 10
		b = 20
		c = 30
	}
	{
		x = 3.14
		y = 2.71
	}
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
	)
	x = -5
	if x < 0 {
		goto label10
	} else if x == 0 {
		goto label20
	} else {
		goto label30
	}
label10:
	intrinsic.Print("LEVEL 21: x is negative")
	goto label40
label20:
	intrinsic.Print("LEVEL 21: x is zero")
	goto label40
label30:
	intrinsic.Print("LEVEL 21: x is positive")
label40:
	choice = 2
	switch choice {
	case 1:
		goto label100
	case 2:
		goto label200
	case 3:
		goto label300
	}
label100:
	intrinsic.Print("LEVEL 21: Choice was 1")
	goto label400
label200:
	intrinsic.Print("LEVEL 21: Choice was 2")
	goto label400
label300:
	intrinsic.Print("LEVEL 21: Choice was 3")
label400:
}

func LEVEL22() {
}

func LEVEL23() {
	const MAX_SIZE int32 = 100
	const PI float32 = 3.14159
	const TAU float32 = 2.0 * PI
	intrinsic.Print("LEVEL 23: MAX_SIZE =", MAX_SIZE)
	intrinsic.Print("LEVEL 23: PI =", PI)
	intrinsic.Print("LEVEL 23: TAU =", TAU)
}

func LEVEL24() {
	var vec1 = intrinsic.NewArray[int32](3)
	var vec2 = intrinsic.NewArray[int32](5)
	vec1 = intrinsic.NewArrayFromValues[int32]([]int32{10, 20, 30})
	intrinsic.Print("LEVEL 24: vec1(1) =", vec1.At(int(1)))
	intrinsic.Print("LEVEL 24: vec1(2) =", vec1.At(int(2)))
	intrinsic.Print("LEVEL 24: vec1(3) =", vec1.At(int(3)))
	vec2 = intrinsic.NewArrayFromValues[int32]([]int32{100, 200, 300, 400, 500})
	intrinsic.Print("LEVEL 24: vec2(1) =", vec2.At(int(1)))
	intrinsic.Print("LEVEL 24: vec2(5) =", vec2.At(int(5)))
}

func LEVEL25() {
	var i1 int8
	var i2 int16
	var i4 int32
	var i8 int64
	var r4 float32
	var r8 float64
	i1 = 127
	i2 = 32767
	i4 = 2147483647
	i8 = 9223372036854775807
	r4 = 3.14159
	r8 = 3.141592653589793
	intrinsic.Print("LEVEL 25: i1 =", i1)
	intrinsic.Print("LEVEL 25: i2 =", i2)
	intrinsic.Print("LEVEL 25: i4 =", i4)
	intrinsic.Print("LEVEL 25: i8 =", i8)
	intrinsic.Print("LEVEL 25: r4 =", r4)
	intrinsic.Print("LEVEL 25: r8 =", r8)
}

func SET_COMMON_VALUES() {
	SHARED.x = 42
	SHARED.y = 99
	SHARED.z = 3.14159
}

func PRINT_COMMON_VALUES() {
	intrinsic.Print("LEVEL 19: x =", SHARED.x)
	intrinsic.Print("LEVEL 19: y =", SHARED.y)
	intrinsic.Print("LEVEL 19: z =", SHARED.z)
}

func SIMPLE_SUB() {
	intrinsic.Print("LEVEL 7: Inside SIMPLE_SUB")
}

func ADD_VALUES(a int32, b int32, result *int32) {
	*result = a + b
	intrinsic.Print("LEVEL 7: Inside ADD_VALUES")
}

func MODIFY_ARRAY(arr *intrinsic.Array[int32], n int32) {
	var i int32
	for i = 1; i <= n; i += 1 {
		arr.Set(arr.At(int(i))*2, int(i))
	}
	intrinsic.Print("LEVEL 7: Inside MODIFY_ARRAY")
}

func FACTORIAL(n int32) int32 {
	var (
		i      int32
		result int32
	)
	result = 1
	for i = 1; i <= n; i += 1 {
		result = result * i
	}
	return result
}

func SQUARE_ROOT(x float32) float32 {
	return intrinsic.SQRT[float32](x)
}

func FIBONACCI(n int32) int32 {
	var (
		a    int32
		b    int32
		temp int32
		i    int32
	)
	if n <= 1 {
		return n
	}
	a = 0
	b = 1
	for i = 2; i <= n; i += 1 {
		temp = a + b
		a = b
		b = temp
	}
	return b
}
