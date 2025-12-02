package intrinsic

import (
	"fmt"
	"unsafe"
)

func Int2Bool[T integer](v T) bool {
	return v != 0
}

func Bool2Int[T integer](v bool) T {
	if v {
		return 1
	}
	return 0
}

type pointer interface {
	// DataUnsafe returns a pointer to the start of the backing buffer in memory.
	DataUnsafe() unsafe.Pointer
	// LenBuffer returns the length of the backing buffer in memory in elements.
	// This is not in bytes. To obtain size of buffer in bytes do p.LenBuffer()*p.SizeElement().
	LenBuffer() int
	// SizeElement returns the size in bytes of the elements the pointer points to.
	SizeElement() int
}

// MALLOC allocates memory for Fortran MALLOC calls (typically a C library function).
// In transpiled code, actual memory allocation is handled by Go's garbage collector.
//
// Example Fortran usage:
//
//	INTEGER :: ptr
//	DOUBLE PRECISION :: arr(1)
//	POINTER (ptr, arr(1))
//	ptr = MALLOC(1000 * 8)  ! Allocate 1000 doubles
//
// Transpiles to:
//
//	var ptr intrinsic.Pointer[float64]
//	ptr = intrinsic.MALLOC[float64](1000 * 8)
//	// Access via ptr.At(i) with Fortran 1-based indexing
func MALLOC[T any](sizeInBytes int32) Pointer[T] {
	var zero T
	if uintptr(sizeInBytes)%unsafe.Sizeof(zero) != 0 {
		panic("invalid size multiple")
	}
	v := make([]T, uintptr(sizeInBytes)/unsafe.Sizeof(zero))
	return NewPointerFromSlice(v)
}

// NewPointerFromSlice creates a Pointer from an existing Go slice.
// Used internally by MALLOC and for wrapping Go slices to pass to Fortran subroutines.
func NewPointerFromSlice[T any](v []T) Pointer[T] {
	return Pointer[T]{
		v:        unsafe.Pointer(&v[0]),
		alloclen: len(v),
	}
}

// Ptr creates a Pointer from a single element reference.
// Used for passing scalar variables by reference to OUT/INOUT parameters.
func Ptr[T any](v *T) Pointer[T] {
	return Pointer[T]{
		v:        unsafe.Pointer(v),
		alloclen: 1,
	}
}

// Pointer represents a Fortran Cray-style pointer - a typed memory address.
//
// # Fortran POINTER Semantics
//
// In Fortran (particularly Cray Fortran and legacy code), POINTER statements
// declare integer variables that hold memory addresses:
//
//	POINTER (pointer_var, pointee)
//
// The pointer_var is an INTEGER that stores an address (like a C pointer cast to intptr_t).
// The pointee is accessed through that address, similar to C dereferencing.
//
// # Type Safety
//
// Unlike Fortran's untyped integer addresses, Go's Pointer[T] is type-safe:
//   - POINTER (iptr, iarr(1)) → Pointer[int32] (for INTEGER arrays)
//   - POINTER (dptr, darr(1)) → Pointer[float64] (for DOUBLE PRECISION arrays)
//   - POINTER (lptr, larr(1)) → Pointer[bool] (for LOGICAL arrays)
//
// # Usage Patterns
//
// 1. Dynamic allocation:
//
//	var ptr intrinsic.Pointer[float64]
//	ptr = intrinsic.MALLOC[float64](n * 8)
//	x := ptr.At(i)  // 1-based Fortran indexing
//
// 2. Subroutine parameters (arrays):
//
//	// Fortran: SUBROUTINE FOO(arr)
//	// Go: func FOO(arr *intrinsic.Array[float64])
//	// Caller wraps: FOO(&myArray) or FOO(ptr.Array())
//
// 3. Shared memory (EQUIVALENCE-like):
//
//	var iview intrinsic.Pointer[int32]
//	var dview intrinsic.Pointer[float64]
//	// Both point to same memory for type punning
type Pointer[T any] struct {
	v        unsafe.Pointer
	alloclen int // alloclen is length in quantity of allocated elements.
}

// LenBuffer returns the number of elements the pointer can access.
// For MALLOC allocations, this is the allocated size divided by element size.
func (p Pointer[T]) LenBuffer() int {
	return p.alloclen
}

// SizeElement returns size of individual elements the pointer corresponds to in bytes.
func (p Pointer[T]) SizeElement() int {
	var zero T
	return int(unsafe.Sizeof(zero))
}

// Size returns the total size in bytes of the pointed-to memory.
func (p Pointer[T]) Size() int {
	return p.LenBuffer() * p.SizeElement()
}

// Slice returns a Go slice view of the pointed-to memory.
// The slice uses 0-based indexing (Go convention).
// For Fortran 1-based indexing, use At() method instead.
func (p Pointer[T]) Slice() []T {
	return unsafe.Slice(p.Data(), p.alloclen)
}

// At accesses an element using Fortran 1-based indexing.
// This matches Fortran array semantics: arr(1) is the first element.
//
// Example:
//
//	ptr := intrinsic.MALLOC[float64](10 * 8)
//	ptr.At(1)  // First element (Fortran: arr(1))
//	ptr.At(10) // Last element (Fortran: arr(10))
func (p Pointer[T]) At(idx int) T {
	return p.Slice()[idx-1]
}

func (p Pointer[T]) Set(idx int, v T) {
	p.Slice()[idx-1] = v
}

// Data returns the underlying Go pointer (*T) to the first element.
// Useful for passing to Go functions expecting native pointers.
func (p Pointer[T]) Data() *T {
	return (*T)(p.v)
}

// DataAt returns the underlying Go pointer (*T) to the idx'th element.
func (p Pointer[T]) DataAt(idx int) *T {
	return p.View(idx, idx+1).Data()
}

func (p Pointer[T]) DataUnsafe() unsafe.Pointer {
	return p.v
}

// View creates a sub-pointer viewing a range of the original allocation.
// Uses Fortran 1-based indexing: View(1, 10) returns elements 1-10 inclusive.
//
// Example:
//
//	ptr := intrinsic.MALLOC[int32](100 * 4)
//	sub := ptr.View(10, 20)  // Elements 10-20 of original allocation
//	sub.At(1)                // First element of view (element 10 of original)
func (p Pointer[T]) View(startOff, endOff int) Pointer[T] {
	return NewPointerFromSlice(p.Slice()[startOff-1 : endOff])
}

// Array converts the Pointer to an Array for multi-dimensional operations.
// Creates a 1D array with Fortran indexing (lower bound 1, upper bound Len()).
//
// Example:
//
//	ptr := intrinsic.MALLOC[float64](100 * 8)
//	arr := ptr.Array()
//	arr.At(50)  // Access 50th element as Array
func (p Pointer[T]) Array() *Array[T] {
	n := p.LenBuffer()
	return &Array[T]{
		data:   p.Slice(),
		shape:  []int{n},
		lower:  []int{1},
		upper:  []int{n},
		stride: []int{1},
	}
}

// UnsafePointerData converts a Pointer to an integer address, matching Fortran's
// treatment of Cray-style POINTER variables as INTEGER addresses.
//
// This enables Fortran patterns like:
//
//	INTEGER :: ptr1, ptr2
//	DOUBLE PRECISION :: arr1(1), arr2(1)
//	POINTER (ptr1, arr1(1)), (ptr2, arr2(1))
//	ptr1 = MALLOC(100 * 8)
//	ptr2 = ptr1  ! Share the same memory
//
// Transpiles to:
//
//	var ptr1, ptr2 intrinsic.Pointer[float64]
//	ptr1 = intrinsic.MALLOC[float64](100 * 8)
//	ptr2 = ptr1  // Go pointers share directly, no conversion needed
//
// The integer conversion is mainly needed for interop with legacy code
// that stores addresses in INTEGER variables or performs pointer arithmetic.
func UnsafePointerData[I integer, T any](p Pointer[T]) I {
	conv := I(uintptr(p.v))
	if uintptr(conv) != uintptr(p.v) {
		panic("pointer address not representable by target integer size")
	}
	return conv
}

// Equivalence creates a type-punned view of a Pointer, reinterpreting the same
// memory as a different type. This implements Fortran's EQUIVALENCE statement semantics.
//
// # Fortran EQUIVALENCE
//
// The EQUIVALENCE statement forces multiple variables to share the same memory location:
//
//	DOUBLE PRECISION :: dval
//	INTEGER, DIMENSION(2) :: ival
//	EQUIVALENCE (dval, ival)
//
// This allows viewing the same 8 bytes as either:
//   - One float64 (dval)
//   - Two int32 elements (ival)
//
// # Type Punning Rules
//
// When reinterpreting memory from type S to type D:
//
//  1. Total bytes remains constant: len(S) * sizeof(S) == len(D) * sizeof(D) * k
//  2. If sizeof(S) > sizeof(D): destination has MORE elements (expansion)
//     Example: float64[1] → int32[2] (8 bytes → 2×4 bytes)
//  3. If sizeof(S) < sizeof(D): destination has FEWER elements (contraction)
//     Example: int32[3] → float64[1] (3×4=12 bytes → 1×8 bytes, 4 bytes unused)
//  4. If sizeof(S) == sizeof(D): destination has SAME element count (reinterpret)
//     Example: int32[10] → float32[10] (both 4 bytes per element)
//
// # Alignment Requirements
//
// The function panics if sizes are not compatible:
//   - When expanding: sizeof(S) must be divisible by sizeof(D)
//   - When contracting: sizeof(D) must be divisible by sizeof(S)
//
// # Safety Warnings
//
// This function performs UNSAFE type punning:
//   - No guarantee that bit patterns are valid for the destination type
//   - Can violate Go's type safety and memory model
//   - Should only be used for Fortran interop where EQUIVALENCE is required
//   - Prefer explicit conversion functions when possible
//
// # Example
//
//	// Fortran: EQUIVALENCE (dval, ival)
//	var dval float64 = math.Pi
//	srcPtr := intrinsic.NewPointerElemental(&dval)
//
//	// Reinterpret as two int32 elements
//	intPtr := intrinsic.Equivalence[float64, int32](srcPtr)
//
//	// Now intPtr.At(1) and intPtr.At(2) access the low/high 32 bits of Pi
//	low := intPtr.At(1)   // Low 32 bits of float64 representation
//	high := intPtr.At(2)  // High 32 bits of float64 representation
func Equivalence[D any, S any](src pointer) (dst Pointer[D]) {
	szS := src.SizeElement()
	szD := dst.SizeElement()
	nS := src.LenBuffer()
	ptr := src.DataUnsafe()
	if szS == szD {
		return Pointer[D]{v: ptr, alloclen: nS}
	}

	// Calculate total bytes and new element count
	totalBytes := nS * szS
	// Check alignment compatibility
	if totalBytes%szD != 0 {
		panic(fmt.Sprintf("equivalence: incompatible sizes: %d bytes (from %d×%d) not divisible by destination element size %d",
			totalBytes, nS, szS, szD))
	}

	newSize := totalBytes / szD
	return Pointer[D]{
		v:        ptr,
		alloclen: newSize,
	}
}
