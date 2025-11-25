package main

import (
	"github.com/soypat/go-fortran/intrinsic"
)

func main() {
	LEVEL01()
	LEVEL02()
	LEVEL03()
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
