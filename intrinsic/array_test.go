package intrinsic

import (
	"testing"
)

// Test 1D array with default bounds [1:size]
// Corresponds to Fortran: REAL :: arr(5)
func TestArray1D_DefaultBounds(t *testing.T) {
	arr := NewArray[int32](nil, 5)

	// Verify bounds
	if arr.Len() != 5 {
		t.Errorf("Expected length 5, got %d", arr.Len())
	}

	lower := arr.Lower()
	if len(lower) != 1 || lower[0] != 1 {
		t.Errorf("Expected lower bound [1], got %v", lower)
	}

	upper := arr.Upper()
	if len(upper) != 1 || upper[0] != 5 {
		t.Errorf("Expected upper bound [5], got %v", upper)
	}

	// Test Set and At
	arr.Set(10, 1)
	arr.Set(20, 3)
	arr.Set(50, 5)

	if arr.At(1) != 10 {
		t.Errorf("Expected arr(1) = 10, got %d", arr.At(1))
	}
	if arr.At(3) != 20 {
		t.Errorf("Expected arr(3) = 20, got %d", arr.At(3))
	}
	if arr.At(5) != 50 {
		t.Errorf("Expected arr(5) = 50, got %d", arr.At(5))
	}
}

// Test 2D array with default bounds [1:rows, 1:cols]
// Corresponds to Fortran: REAL :: matrix(3, 4)
// Verify column-major layout as per F77 Table 1
func TestArray2D_DefaultBounds_ColumnMajor(t *testing.T) {
	// Create 3x4 matrix (3 rows, 4 columns)
	matrix := NewArray[float32](nil, 3, 4)

	// Verify shape
	shape := matrix.Shape()
	if len(shape) != 2 || shape[0] != 3 || shape[1] != 4 {
		t.Errorf("Expected shape [3, 4], got %v", shape)
	}

	// Set elements in column-major order
	// Memory layout should be: (1,1), (2,1), (3,1), (1,2), (2,2), (3,2), ...
	matrix.Set(11, 1, 1)
	matrix.Set(21, 2, 1)
	matrix.Set(31, 3, 1)
	matrix.Set(12, 1, 2)
	matrix.Set(22, 2, 2)
	matrix.Set(32, 3, 2)
	matrix.Set(13, 1, 3)
	matrix.Set(23, 2, 3)

	// Verify access
	if matrix.At(1, 1) != 11 {
		t.Errorf("Expected matrix(1,1) = 11, got %f", matrix.At(1, 1))
	}
	if matrix.At(2, 1) != 21 {
		t.Errorf("Expected matrix(2,1) = 21, got %f", matrix.At(2, 1))
	}
	if matrix.At(1, 2) != 12 {
		t.Errorf("Expected matrix(1,2) = 12, got %f", matrix.At(1, 2))
	}
	if matrix.At(2, 3) != 23 {
		t.Errorf("Expected matrix(2,3) = 23, got %f", matrix.At(2, 3))
	}

	// Verify column-major layout by checking underlying data
	// For 3x4 matrix, element (2, 3) should be at offset: (2-1)*1 + (3-1)*3 = 1 + 6 = 7
	expectedOffset := 7
	actualValue := matrix.data[expectedOffset]
	if actualValue != 23 {
		t.Errorf("Column-major layout broken: data[%d] should be 23, got %f", expectedOffset, actualValue)
	}

	// Element (1, 1) should be at offset 0
	if matrix.data[0] != 11 {
		t.Errorf("Column-major layout broken: data[0] should be 11, got %f", matrix.data[0])
	}

	// Element (3, 1) should be at offset 2
	if matrix.data[2] != 31 {
		t.Errorf("Column-major layout broken: data[2] should be 31, got %f", matrix.data[2])
	}
}

// Test 3D array with default bounds [1:dim1, 1:dim2, 1:dim3]
// Corresponds to Fortran: REAL :: cube(2, 3, 4)
func TestArray3D_DefaultBounds(t *testing.T) {
	cube := NewArray[int32](nil, 2, 3, 4)

	// Verify shape
	shape := cube.Shape()
	if len(shape) != 3 || shape[0] != 2 || shape[1] != 3 || shape[2] != 4 {
		t.Errorf("Expected shape [2, 3, 4], got %v", shape)
	}

	// Test corner elements
	cube.Set(111, 1, 1, 1)
	cube.Set(234, 2, 3, 4)
	cube.Set(142, 1, 2, 2)

	if cube.At(1, 1, 1) != 111 {
		t.Errorf("Expected cube(1,1,1) = 111, got %d", cube.At(1, 1, 1))
	}
	if cube.At(2, 3, 4) != 234 {
		t.Errorf("Expected cube(2,3,4) = 234, got %d", cube.At(2, 3, 4))
	}
	if cube.At(1, 2, 2) != 142 {
		t.Errorf("Expected cube(1,2,2) = 142, got %d", cube.At(1, 2, 2))
	}
}

// Test array with custom bounds (including negative)
// Corresponds to Fortran: DIMENSION A(-5:5, 0:9)
// Per F77 Section 5.1.1.2 (line 2120-2121), bounds can be negative, zero, or positive
func TestArrayCustomBounds(t *testing.T) {
	// Example from F77 standard (line 2649): DIMENSION A(-1:8)
	arr1d := NewArrayWithBounds[int32](nil, []int{10}, []int{-1}, []int{8})

	// Test bounds
	lower := arr1d.Lower()
	upper := arr1d.Upper()
	if lower[0] != -1 || upper[0] != 8 {
		t.Errorf("Expected bounds [-1:8], got [%d:%d]", lower[0], upper[0])
	}

	// F77 example (line 2649-2653): A(2) identifies the 4th element
	// subscript_value = 1 + (2 - (-1)) = 4
	// In our 0-based implementation: offset = (2 - (-1)) = 3 (which is the 4th element)
	arr1d.Set(42, 2)
	if arr1d.At(2) != 42 {
		t.Errorf("Expected A(2) = 42, got %d", arr1d.At(2))
	}
	if arr1d.data[3] != 42 { // Should be at position 3 (4th element)
		t.Errorf("Expected data[3] = 42 (4th element), got %d", arr1d.data[3])
	}

	// Test 2D array with custom bounds: DIMENSION(-5:5, 0:9)
	arr2d := NewArrayWithBounds[float32](nil,
		[]int{11, 10}, // shape: (5-(-5)+1=11, 9-0+1=10)
		[]int{-5, 0},  // lower bounds
		[]int{5, 9},   // upper bounds
	)

	arr2d.Set(3.14, 0, 5)
	if arr2d.At(0, 5) != 3.14 {
		t.Errorf("Expected arr(-5:5, 0:9)(0, 5) = 3.14, got %f", arr2d.At(0, 5))
	}

	// Test boundary elements
	arr2d.Set(1.0, -5, 0) // Lower corner
	arr2d.Set(9.0, 5, 9)  // Upper corner
	if arr2d.At(-5, 0) != 1.0 {
		t.Errorf("Expected lower corner = 1.0, got %f", arr2d.At(-5, 0))
	}
	if arr2d.At(5, 9) != 9.0 {
		t.Errorf("Expected upper corner = 9.0, got %f", arr2d.At(5, 9))
	}
}

// Test subscript value formula from F77 Table 1
// Verify offset calculation matches Fortran standard
func TestSubscriptValueFormula(t *testing.T) {
	// Test case from F77 Table 1:
	// For 2D array (j1:k1, j2:k2) with subscript (s1, s2):
	// subscript_value = 1 + (s1 - j1) + (s2 - j2)*d1
	// Where d1 = k1 - j1 + 1

	// Create array with bounds (2:5, 3:7)
	// d1 = 5 - 2 + 1 = 4
	// d2 = 7 - 3 + 1 = 5
	arr := NewArrayWithBounds[int32](nil,
		[]int{4, 5},
		[]int{2, 3},
		[]int{5, 7},
	)

	// Test element (4, 6):
	// subscript_value = 1 + (4 - 2) + (6 - 3)*4 = 1 + 2 + 12 = 15 (Fortran 1-based)
	// offset = 0 + (4 - 2) + (6 - 3)*4 = 2 + 12 = 14 (Go 0-based)
	arr.Set(99, 4, 6)
	expectedOffset := 14
	if arr.data[expectedOffset] != 99 {
		t.Errorf("Subscript formula failed: expected data[%d] = 99, got %d at data[%d]",
			expectedOffset, arr.data[expectedOffset], expectedOffset)
	}
}

// Test stride calculation for column-major layout
func TestStrideCalculation(t *testing.T) {
	// For 3D array (2, 3, 4):
	// stride[0] = 1
	// stride[1] = stride[0] * shape[0] = 1 * 2 = 2
	// stride[2] = stride[1] * shape[1] = 2 * 3 = 6
	arr := NewArray[int32](nil, 2, 3, 4)

	expectedStrides := []int{1, 2, 6}
	for i, expected := range expectedStrides {
		if arr.stride[i] != expected {
			t.Errorf("stride[%d]: expected %d, got %d", i, expected, arr.stride[i])
		}
	}

	// Verify element (2, 3, 4) is at correct position
	// offset = (2-1)*1 + (3-1)*2 + (4-1)*6 = 1 + 4 + 18 = 23
	arr.Set(999, 2, 3, 4)
	if arr.data[23] != 999 {
		t.Errorf("Stride calculation error: data[23] should be 999, got %d", arr.data[23])
	}
}

// Test bounds checking
func TestBoundsChecking(t *testing.T) {
	arr := NewArray[int32](nil, 5) // Bounds [1:5]

	// Test lower bound violation
	defer func() {
		if r := recover(); r == nil {
			t.Error("Expected panic for index 0 (below lower bound 1)")
		}
	}()
	arr.At(0) // Should panic
}

func TestBoundsChecking_UpperBound(t *testing.T) {
	arr := NewArray[int32](nil, 5) // Bounds [1:5]

	// Test upper bound violation
	defer func() {
		if r := recover(); r == nil {
			t.Error("Expected panic for index 6 (above upper bound 5)")
		}
	}()
	arr.At(6) // Should panic
}

func TestBoundsChecking_WrongDimensions(t *testing.T) {
	arr := NewArray[int32](nil, 3, 4)

	// Test wrong number of indices
	defer func() {
		if r := recover(); r == nil {
			t.Error("Expected panic for wrong number of indices")
		}
	}()
	arr.At(1) // Should panic - need 2 indices, provided 1
}

// Test that column-major layout produces correct memory sequence
// For array A(3, 4), memory should be: A(1,1), A(2,1), A(3,1), A(1,2), ...
func TestColumnMajorMemorySequence(t *testing.T) {
	arr := NewArray[int32](nil, 3, 4)

	// Fill array with unique values: element (i, j) gets value i*10 + j
	for i := 1; i <= 3; i++ {
		for j := 1; j <= 4; j++ {
			arr.Set(int32(i*10+j), i, j)
		}
	}

	// Expected memory sequence (column-major):
	// (1,1)=11, (2,1)=21, (3,1)=31, (1,2)=12, (2,2)=22, (3,2)=32,
	// (1,3)=13, (2,3)=23, (3,3)=33, (1,4)=14, (2,4)=24, (3,4)=34
	expectedSequence := []int32{
		11, 21, 31, // Column 1
		12, 22, 32, // Column 2
		13, 23, 33, // Column 3
		14, 24, 34, // Column 4
	}

	for i, expected := range expectedSequence {
		if arr.data[i] != expected {
			t.Errorf("Column-major sequence broken at index %d: expected %d, got %d",
				i, expected, arr.data[i])
		}
	}
}

// Test intrinsic function equivalents
func TestIntrinsicFunctions(t *testing.T) {
	arr := NewArray[int32](nil, 3, 4)

	// SIZE(array, 1) - first dimension size
	if arr.Len() != 3 {
		t.Errorf("SIZE(array, 1): expected 3, got %d", arr.Len())
	}

	// SHAPE(array)
	shape := arr.Shape()
	if len(shape) != 2 || shape[0] != 3 || shape[1] != 4 {
		t.Errorf("SHAPE(array): expected [3, 4], got %v", shape)
	}

	// LBOUND(array)
	lower := arr.Lower()
	if len(lower) != 2 || lower[0] != 1 || lower[1] != 1 {
		t.Errorf("LBOUND(array): expected [1, 1], got %v", lower)
	}

	// UBOUND(array)
	upper := arr.Upper()
	if len(upper) != 2 || upper[0] != 3 || upper[1] != 4 {
		t.Errorf("UBOUND(array): expected [3, 4], got %v", upper)
	}
}

// Benchmark array access vs native Go slices
func BenchmarkArray2D_Access(b *testing.B) {
	arr := NewArray[int32](nil, 100, 100)
	for i := 0; i < b.N; i++ {
		arr.Set(42, 50, 50)
		_ = arr.At(50, 50)
	}
}

func BenchmarkGoSlice2D_Access(b *testing.B) {
	slice := make([][]int32, 100)
	for i := range slice {
		slice[i] = make([]int32, 100)
	}
	for i := 0; i < b.N; i++ {
		slice[49][49] = 42 // Convert to 0-based
		_ = slice[49][49]
	}
}

func TestNewArrayFromValues(t *testing.T) {
	// Test 1D array initialization with values
	arr := NewArray([]int32{10, 20, 30}, 3)
	if arr.Size() != 3 {
		t.Errorf("Expected size 3, got %d", arr.Size())
	}

	// Check values with 1-based indexing
	if arr.At(1) != 10 {
		t.Errorf("Expected arr[1]=10, got %d", arr.At(1))
	}
	if arr.At(2) != 20 {
		t.Errorf("Expected arr[2]=20, got %d", arr.At(2))
	}
	if arr.At(3) != 30 {
		t.Errorf("Expected arr[3]=30, got %d", arr.At(3))
	}

	// Check bounds
	if arr.LowerDim(1) != 1 {
		t.Errorf("Expected lower bound 1, got %d", arr.LowerDim(1))
	}
	if arr.UpperDim(1) != 3 {
		t.Errorf("Expected upper bound 3, got %d", arr.UpperDim(1))
	}
}
