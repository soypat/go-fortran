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

// Test View on 1D array
func TestArray_View1D(t *testing.T) {
	arr := NewArray[int32](nil, 10)
	// Fill with values 1-10
	for i := 1; i <= 10; i++ {
		arr.Set(int32(i*10), i)
	}

	// View of elements 3:7
	view := arr.View(R(3, 7))

	// Check shape
	shape := view.Shape()
	if len(shape) != 1 || shape[0] != 5 {
		t.Errorf("Expected view shape [5], got %v", shape)
	}

	// Check bounds (view uses 1-based indexing)
	if view.LowerDim(1) != 1 || view.UpperDim(1) != 5 {
		t.Errorf("Expected view bounds [1:5], got [%d:%d]", view.LowerDim(1), view.UpperDim(1))
	}

	// Check access: view(1) should equal arr(3) = 30
	if view.At(1) != 30 {
		t.Errorf("Expected view(1) = 30, got %d", view.At(1))
	}
	// view(5) should equal arr(7) = 70
	if view.At(5) != 70 {
		t.Errorf("Expected view(5) = 70, got %d", view.At(5))
	}

	// Test writing through view
	view.Set(999, 3) // Should modify arr(5)
	if arr.At(5) != 999 {
		t.Errorf("Expected arr(5) = 999 after view write, got %d", arr.At(5))
	}
}

// Test View on 2D array
func TestArray_View2D(t *testing.T) {
	// Create 10x5 matrix, fill with (row*100 + col)
	arr := NewArray[int32](nil, 10, 5)
	for i := 1; i <= 10; i++ {
		for j := 1; j <= 5; j++ {
			arr.Set(int32(i*100+j), i, j)
		}
	}

	// View of rows 2:4, cols 1:3
	view := arr.View(R(2, 4), R(1, 3))

	// Check shape: should be [3, 3]
	shape := view.Shape()
	if len(shape) != 2 || shape[0] != 3 || shape[1] != 3 {
		t.Errorf("Expected view shape [3, 3], got %v", shape)
	}

	// view(1, 1) should equal arr(2, 1) = 201
	if view.At(1, 1) != 201 {
		t.Errorf("Expected view(1,1) = 201, got %d", view.At(1, 1))
	}

	// view(3, 3) should equal arr(4, 3) = 403
	if view.At(3, 3) != 403 {
		t.Errorf("Expected view(3,3) = 403, got %d", view.At(3, 3))
	}

	// view(2, 2) should equal arr(3, 2) = 302
	if view.At(2, 2) != 302 {
		t.Errorf("Expected view(2,2) = 302, got %d", view.At(2, 2))
	}
}

// Test nested views
func TestArray_ViewNested(t *testing.T) {
	arr := NewArray[int32](nil, 10, 10)
	for i := 1; i <= 10; i++ {
		for j := 1; j <= 10; j++ {
			arr.Set(int32(i*100+j), i, j)
		}
	}

	// First view: rows 2:8, cols 2:8 (7x7)
	v1 := arr.View(R(2, 8), R(2, 8))
	if v1.Shape()[0] != 7 || v1.Shape()[1] != 7 {
		t.Errorf("Expected v1 shape [7, 7], got %v", v1.Shape())
	}

	// v1(1,1) = arr(2,2) = 202
	if v1.At(1, 1) != 202 {
		t.Errorf("Expected v1(1,1) = 202, got %d", v1.At(1, 1))
	}

	// Second view: rows 2:4, cols 2:4 of v1 (3x3)
	v2 := v1.View(R(2, 4), R(2, 4))
	if v2.Shape()[0] != 3 || v2.Shape()[1] != 3 {
		t.Errorf("Expected v2 shape [3, 3], got %v", v2.Shape())
	}

	// v2(1,1) = v1(2,2) = arr(3,3) = 303
	if v2.At(1, 1) != 303 {
		t.Errorf("Expected v2(1,1) = 303, got %d", v2.At(1, 1))
	}

	// v2(3,3) = v1(4,4) = arr(5,5) = 505
	if v2.At(3, 3) != 505 {
		t.Errorf("Expected v2(3,3) = 505, got %d", v2.At(3, 3))
	}
}

// Test View with stride
func TestArray_ViewWithStride(t *testing.T) {
	arr := NewArray[int32](nil, 10)
	for i := 1; i <= 10; i++ {
		arr.Set(int32(i), i)
	}

	// View with stride 2: elements 1, 3, 5, 7, 9
	view := arr.View(RS(1, 10, 2))

	shape := view.Shape()
	if shape[0] != 5 {
		t.Errorf("Expected view shape [5], got %v", shape)
	}

	// view(1) = arr(1) = 1
	if view.At(1) != 1 {
		t.Errorf("Expected view(1) = 1, got %d", view.At(1))
	}
	// view(2) = arr(3) = 3
	if view.At(2) != 3 {
		t.Errorf("Expected view(2) = 3, got %d", view.At(2))
	}
	// view(5) = arr(9) = 9
	if view.At(5) != 9 {
		t.Errorf("Expected view(5) = 9, got %d", view.At(5))
	}
}

// Test SetFrom (element-wise copy)
func TestArray_SetFrom(t *testing.T) {
	src := NewArray[int32](nil, 5)
	dst := NewArray[int32](nil, 5)
	for i := 1; i <= 5; i++ {
		src.Set(int32(i*10), i)
	}

	dst.SetFrom(src)

	for i := 1; i <= 5; i++ {
		if dst.At(i) != int32(i*10) {
			t.Errorf("Expected dst(%d) = %d, got %d", i, i*10, dst.At(i))
		}
	}
}

// Test SetFrom with views
func TestArray_SetFrom_Views(t *testing.T) {
	src := NewArray[int32](nil, 10)
	dst := NewArray[int32](nil, 10)
	for i := 1; i <= 10; i++ {
		src.Set(int32(i*10), i)
	}

	// Copy src(3:7) to dst(1:5)
	dst.View(R(1, 5)).SetFrom(src.View(R(3, 7)))

	// dst(1) should be src(3) = 30
	if dst.At(1) != 30 {
		t.Errorf("Expected dst(1) = 30, got %d", dst.At(1))
	}
	// dst(5) should be src(7) = 70
	if dst.At(5) != 70 {
		t.Errorf("Expected dst(5) = 70, got %d", dst.At(5))
	}
	// dst(6) should be unchanged (0)
	if dst.At(6) != 0 {
		t.Errorf("Expected dst(6) = 0, got %d", dst.At(6))
	}
}

// Test ArraySetAdd
func TestArraySetAdd(t *testing.T) {
	a := NewArray[float32](nil, 5)
	b := NewArray[float32](nil, 5)
	dst := NewArray[float32](nil, 5)

	for i := 1; i <= 5; i++ {
		a.Set(float32(i), i)
		b.Set(float32(i*10), i)
	}

	ArraySetAdd(dst, a, b)

	// dst(i) should be a(i) + b(i) = i + i*10 = i*11
	for i := 1; i <= 5; i++ {
		expected := float32(i * 11)
		if dst.At(i) != expected {
			t.Errorf("Expected dst(%d) = %f, got %f", i, expected, dst.At(i))
		}
	}
}

// Test the main use case: XSN(1:NM,1:3) = XSN(1:NM,1:3) + COF(1:NM,1:3)
func TestArray_ViewSetAdd(t *testing.T) {
	XSN := NewArray[float32](nil, 100, 10)
	COF := NewArray[float32](nil, 100, 10)

	// Initialize with test data
	for i := 1; i <= 100; i++ {
		for j := 1; j <= 10; j++ {
			XSN.Set(float32(i+j), i, j)
			COF.Set(float32(i*j), i, j)
		}
	}

	NM := 50

	// XSN(1:NM,1:3) = XSN(1:NM,1:3) + COF(1:NM,1:3)
	ArraySetAdd(
		XSN.View(R(1, NM), R(1, 3)),
		XSN.View(R(1, NM), R(1, 3)),
		COF.View(R(1, NM), R(1, 3)),
	)

	// Check a few values
	// Original XSN(1,1) = 1+1 = 2, COF(1,1) = 1*1 = 1, result = 3
	if XSN.At(1, 1) != 3 {
		t.Errorf("Expected XSN(1,1) = 3, got %f", XSN.At(1, 1))
	}

	// XSN(50,3) was 50+3 = 53, COF(50,3) = 50*3 = 150, result = 203
	if XSN.At(50, 3) != 203 {
		t.Errorf("Expected XSN(50,3) = 203, got %f", XSN.At(50, 3))
	}

	// XSN(51,1) should be unchanged (outside the view)
	if XSN.At(51, 1) != 52 { // 51+1 = 52
		t.Errorf("Expected XSN(51,1) = 52 (unchanged), got %f", XSN.At(51, 1))
	}

	// XSN(1,4) should be unchanged (outside the view)
	if XSN.At(1, 4) != 5 { // 1+4 = 5
		t.Errorf("Expected XSN(1,4) = 5 (unchanged), got %f", XSN.At(1, 4))
	}
}

// Test ArraySetSub
func TestArraySetSub(t *testing.T) {
	a := NewArray[int32](nil, 5)
	b := NewArray[int32](nil, 5)
	dst := NewArray[int32](nil, 5)

	for i := 1; i <= 5; i++ {
		a.Set(int32(i*10), i)
		b.Set(int32(i), i)
	}

	ArraySetSub(dst, a, b)

	for i := 1; i <= 5; i++ {
		expected := int32(i*10 - i)
		if dst.At(i) != expected {
			t.Errorf("Expected dst(%d) = %d, got %d", i, expected, dst.At(i))
		}
	}
}

// Test ArraySetMul
func TestArraySetMul(t *testing.T) {
	a := NewArray[int32](nil, 5)
	b := NewArray[int32](nil, 5)
	dst := NewArray[int32](nil, 5)

	for i := 1; i <= 5; i++ {
		a.Set(int32(i), i)
		b.Set(int32(i+1), i)
	}

	ArraySetMul(dst, a, b)

	for i := 1; i <= 5; i++ {
		expected := int32(i * (i + 1))
		if dst.At(i) != expected {
			t.Errorf("Expected dst(%d) = %d, got %d", i, expected, dst.At(i))
		}
	}
}
