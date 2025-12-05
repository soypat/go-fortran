package intrinsic_test

import (
	"fmt"

	"github.com/soypat/go-fortran/intrinsic"
)

// Example of 1D array with default bounds [1:size]
// Corresponds to Fortran: INTEGER :: arr(5)
func ExampleNewArray_oneDimensional() {
	arr := intrinsic.NewArray[int32](nil, 5)

	// Set elements using Fortran 1-based indexing
	arr.Set(10, 1)
	arr.Set(20, 2)
	arr.Set(30, 3)

	// Access elements
	fmt.Println(arr.At(1))
	fmt.Println(arr.At(2))
	fmt.Println(arr.At(3))

	// Output:
	// 10
	// 20
	// 30
}

// Example of 2D array with column-major layout
// Corresponds to Fortran: REAL :: matrix(3, 4)
func ExampleNewArray_twoDimensional() {
	matrix := intrinsic.NewArray[float32](nil, 3, 4)

	// Create identity-like matrix
	matrix.Set(1.0, 1, 1)
	matrix.Set(1.0, 2, 2)
	matrix.Set(1.0, 3, 3)

	// Access elements
	fmt.Printf("matrix(1,1) = %.1f\n", matrix.At(1, 1))
	fmt.Printf("matrix(2,2) = %.1f\n", matrix.At(2, 2))
	fmt.Printf("matrix(3,3) = %.1f\n", matrix.At(3, 3))

	// Output:
	// matrix(1,1) = 1.0
	// matrix(2,2) = 1.0
	// matrix(3,3) = 1.0
}

// Example of array with custom bounds
// Corresponds to Fortran: DIMENSION A(-5:5)
func ExampleNewArrayWithBounds() {
	// Array with bounds from -5 to 5 (11 elements)
	arr := intrinsic.NewArrayWithBounds[int32](nil,
		[]int{11}, // shape: 11 elements
		[]int{-5}, // lower bound: -5
		[]int{5},  // upper bound: 5
	)

	// Set elements using custom bounds
	arr.Set(100, -5) // First element
	arr.Set(0, 0)    // Middle element
	arr.Set(100, 5)  // Last element

	fmt.Printf("A(-5) = %d\n", arr.At(-5))
	fmt.Printf("A(0) = %d\n", arr.At(0))
	fmt.Printf("A(5) = %d\n", arr.At(5))

	// Output:
	// A(-5) = 100
	// A(0) = 0
	// A(5) = 100
}

// Example of 2D array with custom bounds
// Corresponds to Fortran: DIMENSION matrix(0:2, 10:12)
func ExampleNewArrayWithBounds_twoDimensional() {
	// 2D array with custom bounds: (0:2, 10:12)
	matrix := intrinsic.NewArrayWithBounds[int32](nil,
		[]int{3, 3},  // shape: 3x3
		[]int{0, 10}, // lower bounds: 0, 10
		[]int{2, 12}, // upper bounds: 2, 12
	)

	// Set corner elements
	matrix.Set(1, 0, 10) // Bottom-left
	matrix.Set(2, 2, 12) // Top-right

	fmt.Printf("matrix(0,10) = %d\n", matrix.At(0, 10))
	fmt.Printf("matrix(2,12) = %d\n", matrix.At(2, 12))

	// Output:
	// matrix(0,10) = 1
	// matrix(2,12) = 2
}

// Example demonstrating column-major memory layout
// In Fortran, the first index varies fastest
func ExampleArray_columnMajor() {
	// Create a 2x3 matrix
	matrix := intrinsic.NewArray[int32](nil, 2, 3)

	// Fill with unique values
	matrix.Set(11, 1, 1)
	matrix.Set(21, 2, 1)
	matrix.Set(12, 1, 2)
	matrix.Set(22, 2, 2)
	matrix.Set(13, 1, 3)
	matrix.Set(23, 2, 3)

	// Memory order is column-major: (1,1), (2,1), (1,2), (2,2), (1,3), (2,3)
	// This matches Fortran's array storage sequence

	fmt.Println("Accessing by row:")
	for i := 1; i <= 2; i++ {
		for j := 1; j <= 3; j++ {
			if j > 1 {
				fmt.Print(" ")
			}
			fmt.Printf("%d", matrix.At(i, j))
		}
		fmt.Println()
	}

	// Output:
	// Accessing by row:
	// 11 12 13
	// 21 22 23
}

// Example of intrinsic function equivalents
func ExampleArray_intrinsics() {
	matrix := intrinsic.NewArray[int32](nil, 3, 4)

	// SIZE(array, 1) - size of first dimension
	fmt.Printf("SIZE = %d\n", matrix.Len())

	// SHAPE(array) - shape of all dimensions
	fmt.Printf("SHAPE = %v\n", matrix.Shape())

	// LBOUND(array) - lower bounds
	fmt.Printf("LBOUND = %v\n", matrix.Lower())

	// UBOUND(array) - upper bounds
	fmt.Printf("UBOUND = %v\n", matrix.Upper())

	// Output:
	// SIZE = 3
	// SHAPE = [3 4]
	// LBOUND = [1 1]
	// UBOUND = [3 4]
}
