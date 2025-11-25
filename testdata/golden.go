package main

import(
	"github.com/soypat/go-fortran/intrinsic"
)

func main() {
	LEVEL01()
	LEVEL02()
	LEVEL03()
	LEVEL04()
	LEVEL05()
}
func LEVEL01() {
	intrinsic.Print("LEVEL 1: Hello, World!")
}
func LEVEL02() {
	var i int32
	var x float32
	var flag bool
	var message string = "                    "
	i = 42
	x = 3.14159
	flag = true
	message = "Variables assigned  "
	intrinsic.Print("LEVEL 2: i =", i, ", x =", x)
	intrinsic.Print("LEVEL 2: flag =", flag)
	intrinsic.Print("LEVEL 2:", message)
}
func LEVEL03() {
	var (
		i	int32
		j	int32
		k	int32
	)
	var (
		x	float32
		y	float32
		z	float32
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
