package intrinsic

import "unsafe"

// Array represents a multi-dimensional Fortran array with column-major layout.
// It uses a single contiguous memory allocation (slab allocation) for efficiency.
//
// Design rationale based on Fortran standards:
//
// FORTRAN 77 (ANSI X3J3/90.4):
// - Section 5.1.1: Arrays have 1-7 dimensions (line 2075-2076)
// - Section 5.1.1.1: Dimension declarator format is [lower:]upper
// - Section 5.1.1.2: If lower bound omitted, defaults to 1 (line 2123-2124)
// - Section 5.1.1.2: Bounds can be negative, zero, or positive (line 2120-2121)
// - Section 5.2.4-5.2.5: Array element ordering and storage sequence (line 2264-2302)
// - Table 1 (line 2425-2463): Subscript value formula defines column-major layout
//   - 1D: subscript_value = 1 + (s1 - j1)
//   - 2D: subscript_value = 1 + (s1 - j1) + (s2 - j2)*d1
//   - 3D: subscript_value = 1 + (s1 - j1) + (s2 - j2)*d1 + (s3 - j3)*d2*d1
//   - Where di = ki - ji + 1 (size of dimension i)
//
// Fortran 95 (ISO/IEC 1539:1991):
// - Section 6.2.2.3: Array element order (line 7246-7250)
// - Table 6.1 (line 7269-7348): Same subscript order formula as F77
// - Note 1 (line 7350): di = max(ki - ji + 1, 0)
// - Added ALLOCATABLE arrays (line 5188-5193)
// - Added assumed-shape arrays (line 5631-5643)
//
// Column-major layout means the FIRST index varies fastest in memory.
// For a 2D array A(3,4), memory order is: A(1,1), A(2,1), A(3,1), A(1,2), A(2,2), ...
type Array[T any] struct {
	data   []T   // Single contiguous allocation (slab allocation)
	shape  []int // Size of each dimension: shape[i] = upper[i] - lower[i] + 1
	lower  []int // Lower bounds for each dimension (typically 1, but can be negative)
	upper  []int // Upper bounds for each dimension
	stride []int // Column-major strides: stride[0]=1, stride[i]=stride[i-1]*shape[i-1]
}

func NewArray[T any](data []T, dims ...int) *Array[T] {
	if len(dims) > 7 {
		panic("array dimension too large")
	} else if len(dims) == 0 {
		panic("zero dimension array")
	}
	parambuf := make([]int, len(dims)*3)
	shape := parambuf[:len(dims)]
	lower := parambuf[len(dims) : len(dims)*2]
	upper := parambuf[len(dims)*2:]
	for i, d := range dims {
		shape[i], lower[i], upper[i] = d, 1, d
	}
	return NewArrayWithBounds(data, shape, lower, upper)
}

// NewArrayWithBounds creates an array with custom bounds for each dimension.
// Supports arbitrary lower bounds as per F77 Section 5.1.1.2 (line 2120-2121).
//
// Example: DIMENSION(-5:5, 0:9) creates an array with:
//   - shape = [11, 10]  (size of each dimension)
//   - lower = [-5, 0]   (lower bounds)
//   - upper = [5, 9]    (upper bounds)
//
// The shape, lower, and upper slices must have the same length (number of dimensions).
// Column-major strides are computed as:
//   - stride[0] = 1
//   - stride[i] = stride[i-1] * shape[i-1]
func NewArrayWithBounds[T any](data []T, shape, lower, upper []int) *Array[T] {
	if len(shape) != len(lower) || len(shape) != len(upper) {
		panic("array: shape, lower, and upper must have same length")
	}

	// Calculate total size (product of all dimensions)
	totalSize := 1
	for _, dim := range shape {
		if dim < 0 {
			panic("array: dimension size must be non-negative")
		}
		totalSize *= dim
	}
	if data == nil {
		data = make([]T, totalSize)
	} else if len(data) != totalSize {
		panic("array: mismatch data with shape")
	}

	// Calculate column-major strides (F77 Table 1, F95 Table 6.1)
	// stride[0] = 1 (first index varies fastest - column-major)
	// stride[i] = stride[i-1] * shape[i-1]
	stride := make([]int, len(shape))
	if len(shape) > 0 {
		stride[0] = 1
		for i := 1; i < len(shape); i++ {
			stride[i] = stride[i-1] * shape[i-1]
		}
	}

	return &Array[T]{
		data:   data, // Slab allocation: single contiguous memory
		shape:  append([]int(nil), shape...),
		lower:  append([]int(nil), lower...),
		upper:  append([]int(nil), upper...),
		stride: stride,
	}
}

// At returns the element at the given indices (using Fortran indexing with custom bounds)
// Implements the subscript value formula from F77 Table 1 / F95 Table 6.1.
//
// Example for 2D array with bounds (1:3, 1:4):
//
//	arr.At(2, 3) accesses element at row 2, column 3
//	offset = (2 - 1)*1 + (3 - 1)*3 = 1 + 6 = 7
//
// Example for array with custom bounds (-5:5, 0:9):
//
//	arr.At(0, 5) accesses element at indices (0, 5)
//	offset = (0 - (-5))*1 + (5 - 0)*11 = 5 + 55 = 60
func (a *Array[T]) At(indices ...int) T {
	offset := a.offset(indices)
	return a.data[offset]
}

// Set sets the element at the given indices (using Fortran indexing with custom bounds)
// Implements the subscript value formula from F77 Table 1 / F95 Table 6.1.
//
// Example: arr.Set(value, 1, 2) sets the element at row 1, column 2
func (a *Array[T]) Set(value T, indices ...int) {
	offset := a.offset(indices)
	a.data[offset] = value
}

// Len returns the size of the first dimension
// Corresponds to Fortran SIZE(array, 1) intrinsic
func (a *Array[T]) Len() int {
	if len(a.shape) == 0 {
		return 0
	}
	return a.shape[0]
}

// Size returns the total number of elements in the array
// Corresponds to Fortran SIZE(array) intrinsic (no dimension argument)
func (a *Array[T]) Size() int {
	if len(a.shape) == 0 {
		return 0
	}
	total := 1
	for _, dim := range a.shape {
		total *= dim
	}
	return total
}

// SizeDim returns the size of a specific dimension (1-based dimension index)
// Corresponds to Fortran SIZE(array, dim) intrinsic
func (a *Array[T]) SizeDim(dim int) int {
	if dim < 1 || dim > len(a.shape) {
		return 0
	}
	return a.shape[dim-1]
}

// Shape returns a copy of the shape slice (size of each dimension)
// Corresponds to Fortran SHAPE(array) intrinsic
func (a *Array[T]) Shape() []int {
	return append([]int(nil), a.shape...)
}

// Lower returns a copy of the lower bounds slice
// Corresponds to Fortran LBOUND(array) intrinsic (no dimension argument)
func (a *Array[T]) Lower() []int {
	return append([]int(nil), a.lower...)
}

// LowerDim returns the lower bound of a specific dimension (1-based dimension index)
// Corresponds to Fortran LBOUND(array, dim) intrinsic
func (a *Array[T]) LowerDim(dim int) int {
	if dim < 1 || dim > len(a.lower) {
		return 0
	}
	return a.lower[dim-1]
}

// Upper returns a copy of the upper bounds slice
// Corresponds to Fortran UBOUND(array) intrinsic (no dimension argument)
func (a *Array[T]) Upper() []int {
	return append([]int(nil), a.upper...)
}

// UpperDim returns the upper bound of a specific dimension (1-based dimension index)
// Corresponds to Fortran UBOUND(array, dim) intrinsic
func (a *Array[T]) UpperDim(dim int) int {
	if dim < 1 || dim > len(a.upper) {
		return 0
	}
	return a.upper[dim-1]
}

func (a *Array[T]) AtOffset(indices ...int) int {
	return a.offset(indices)
}

// offset calculates the flat index for multi-dimensional access using column-major layout
// Implements the subscript value formula from F77 Table 1 (line 2436-2459) and F95 Table 6.1.
//
// Formula: offset = sum((indices[i] - lower[i]) * stride[i]) for i = 0 to n-1
//
// This corresponds to the Fortran subscript value - 1 (converting from 1-based to 0-based):
//
//	F77: subscript_value = 1 + (s1 - j1) + (s2 - j2)*d1 + (s3 - j3)*d2*d1 + ...
//	Go:  offset          = 0 + (s1 - j1) + (s2 - j2)*d1 + (s3 - j3)*d2*d1 + ...
//
// The column-major layout means the first index varies fastest in memory:
//
//	For array A(3, 4), memory order is: A(1,1), A(2,1), A(3,1), A(1,2), A(2,2), ...
func (a *Array[T]) offset(indices []int) int {
	if len(indices) != len(a.shape) {
		panic("array: wrong number of indices")
	}

	offset := 0
	for i, idx := range indices {
		// Bounds check (F77 Section 5.4.2, line 2341-2365)
		if idx < a.lower[i] || idx > a.upper[i] {
			panic("array: index out of bounds")
		}
		// Apply subscript value formula with column-major strides
		offset += (idx - a.lower[i]) * a.stride[i]
	}
	return offset
}

// Pointer returns the pointer to the underlying flat buffer.
func (a *Array[T]) Pointer() PointerTo[T] {
	return NewPointerFromSlice(a.data)
}

var _ Pointer = (*Array[float32])(nil) // compile time check of interface implementation.

// SizeElement implements [Pointer] interface.
func (a *Array[T]) SizeElement() int {
	var z T
	return int(unsafe.Sizeof(z))
}

// DataUnsafe implements [Pointer] interface.
func (a *Array[T]) DataUnsafe() unsafe.Pointer {
	return unsafe.Pointer(&a.data[0])
}

// DataUnsafe implements [Pointer] interface.
//
// Deprecated: Extremely unsafe.
func (a *Array[T]) SetDataUnsafe(v unsafe.Pointer) {
	a.data = unsafe.Slice((*T)(v), len(a.data))
}

// SizeBuffer implements [Pointer] interface.
func (a *Array[T]) LenBuffer() int {
	return len(a.data)
}
