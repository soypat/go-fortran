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

type Pointer interface {
	// DataUnsafe returns a pointer to the start of the backing buffer in memory.
	DataUnsafe() unsafe.Pointer
	// LenBuffer returns the length of the backing buffer in memory in elements.
	// This is not in bytes. To obtain size of buffer in bytes do p.LenBuffer()*p.SizeElement().
	LenBuffer() int
	// SizeElement returns the size in bytes of the elements the pointer points to.
	SizeElement() int
}

type PointerSetter interface {
	Pointer
	// SetDataUnsafe is a super unsafe method that should be used extremely cautiously.
	//
	// Deprecated: Do not use this.
	SetDataUnsafe(ptr unsafe.Pointer)
}

// Equivalence implements Fortran's EQUIVALENCE statement by making multiple
// pointers share the same underlying memory allocation.
//
// # Fortran EQUIVALENCE
//
// The EQUIVALENCE statement forces multiple variables to share the same memory location:
//
//	DOUBLE PRECISION :: dval(100)
//	INTEGER :: ival(200)
//	EQUIVALENCE (dval, ival)
//
// After equivalence, dval and ival refer to the same memory region.
//
// # How It Works
//
// Equivalence finds the largest allocation among the provided pointers and sets
// all pointers to share that base address. Each pointer retains its own type
// information for element access.
//
// # Usage
//
// All arguments must be pointers to PointerSetter types (use & operator):
//
//	var floatPtr PointerTo[float64]
//	var intPtr PointerTo[int32]
//	floatPtr = MALLOC[float64](100 * 8)  // Allocate 100 float64s
//
//	Equivalence(&floatPtr, &intPtr)
//	// Now both point to same memory; intPtr sees 200 int32 elements
//
// # Notes
//
// For type-punning (viewing memory as a different type), prefer [PointerFrom]
// which returns a new typed view without modifying the original pointer.
func Equivalence(toEquiv ...PointerSetter) {
	largest := toEquiv[0]
	maxAlloc := sizeUnderlyingAlloc(largest)
	for _, buf := range toEquiv {
		alloc := sizeUnderlyingAlloc(buf)
		if alloc == 0 {
			if c, ok := buf.(*CharacterArray); ok && c.data == nil {
				c.Allocate(1)
				alloc = 1
			}
		}
		if alloc > maxAlloc {
			largest = buf
			maxAlloc = alloc
		}
	}

	baseAddr := largest.DataUnsafe()
	for _, equiv := range toEquiv {
		equiv.SetDataUnsafe(baseAddr)
	}
}

// PointerOff wraps a PointerSetter with an element offset for EQUIVALENCE.
// Used when equivalencing at a specific array element: EQUIVALENCE (A, B(5))
//
// The elemNum parameter is a 1-based element number (Fortran indexing).
// Use Array.AtOffset(indices...) + 1 to convert multi-dimensional indices.
//
// Example:
//
//	// EQUIVALENCE (A, MAT(2,3))
//	intrinsic.Equivalence(&a, intrinsic.PointerOff(mat, mat.AtOffset(2, 3) + 1))
func PointerOff(ptr PointerSetter, elemNum int) PointerSetter {
	q := ptrOff{ptr: ptr, elemOffset: elemNum - 1} // Convert 1-based to 0-based internally
	if q.LenBuffer() < 0 {
		panic("elemNum exceeds size")
	}
	return &q
}

func sizeUnderlyingAlloc(ps PointerSetter) int {
	if q, ok := ps.(*ptrOff); ok {
		return q.SizeUnderlyingAlloc()
	}
	return ps.LenBuffer() * ps.SizeElement()
}

type ptrOff struct {
	ptr        PointerSetter
	elemOffset int
}

var _ PointerSetter = (*ptrOff)(nil)

func (q *ptrOff) SetDataUnsafe(v unsafe.Pointer) {
	q.ptr.SetDataUnsafe(unsafe.Add(v, -q.Offset()))
}
func (q *ptrOff) DataUnsafe() unsafe.Pointer {
	return unsafe.Add(q.ptr.DataUnsafe(), q.Offset())
}
func (q *ptrOff) SizeElement() int {
	return q.ptr.SizeElement()
}
func (q *ptrOff) LenBuffer() int {
	return q.ptr.LenBuffer() - q.elemOffset
}
func (q *ptrOff) Offset() int {
	return q.ptr.SizeElement() * q.elemOffset
}
func (q *ptrOff) SizeUnderlyingAlloc() int {
	if qq, ok := q.ptr.(*ptrOff); ok {
		return qq.SizeUnderlyingAlloc()
	}
	return q.ptr.LenBuffer() * q.ptr.SizeElement()
}

// Ptr creates a Pointer from a single element reference.
// Used for passing scalar variables by reference to OUT/INOUT parameters.
func Ptr[T any](v *T) PointerTo[T] {
	return PointerTo[T]{
		v:        unsafe.Pointer(v),
		alloclen: 1,
	}
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
func MALLOC[T any](sizeInBytes int32) PointerTo[T] {
	var zero T
	if uintptr(sizeInBytes)%unsafe.Sizeof(zero) != 0 {
		panic("invalid size multiple")
	}
	v := make([]T, uintptr(sizeInBytes)/unsafe.Sizeof(zero))
	return NewPointerFromSlice(v)
}

// NewPointerFromSlice creates a Pointer from an existing Go slice.
// Used internally by MALLOC and for wrapping Go slices to pass to Fortran subroutines.
func NewPointerFromSlice[T any](v []T) PointerTo[T] {
	return PointerTo[T]{
		v:        unsafe.Pointer(&v[0]),
		alloclen: len(v),
	}
}

// PointerTo represents a Fortran Cray-style pointer - a typed memory address.
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
// Unlike Fortran's untyped integer addresses, Go's PointerTo[T] is type-safe:
//   - POINTER (iptr, iarr(1)) → PointerTo[int32] (for INTEGER arrays)
//   - POINTER (dptr, darr(1)) → PointerTo[float64] (for DOUBLE PRECISION arrays)
//   - POINTER (lptr, larr(1)) → PointerTo[bool] (for LOGICAL arrays)
//
// # Usage Patterns
//
// 1. Dynamic allocation:
//
//	var ptr intrinsic.PointerTo[float64]
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
//	var iview intrinsic.PointerTo[int32]
//	var dview intrinsic.PointerTo[float64]
//	// Both point to same memory for type punning
type PointerTo[T any] struct {
	v        unsafe.Pointer
	alloclen int // alloclen is length in quantity of allocated elements.
}

var _ Pointer = PointerTo[byte]{} // compile time check of interface implementation.
var _ PointerSetter = (*PointerTo[byte])(nil)

// LenBuffer returns the number of elements the pointer can access.
// For MALLOC allocations, this is the allocated size divided by element size.
func (p PointerTo[T]) LenBuffer() int {
	return p.alloclen
}

// SizeElement returns size of individual elements the pointer corresponds to in bytes.
func (p PointerTo[T]) SizeElement() int {
	var zero T
	return int(unsafe.Sizeof(zero))
}

// Size returns the total size in bytes of the pointed-to memory.
func (p PointerTo[T]) Size() int {
	return p.LenBuffer() * p.SizeElement()
}

// Slice returns a Go slice view of the pointed-to memory.
// The slice uses 0-based indexing (Go convention).
// For Fortran 1-based indexing, use At() method instead.
func (p PointerTo[T]) Slice() []T {
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
func (p PointerTo[T]) At(idx int) T {
	return p.Slice()[idx-1]
}

func (p PointerTo[T]) Set(idx int, v T) {
	p.Slice()[idx-1] = v
}

// Data returns the underlying Go pointer (*T) to the first element.
// Useful for passing to Go functions expecting native pointers.
func (p PointerTo[T]) Data() *T {
	return (*T)(p.v)
}

// DataAt returns the underlying Go pointer (*T) to the idx'th element.
func (p PointerTo[T]) DataAt(idx int) *T {
	return p.View(idx, idx+1).Data()
}

func (p PointerTo[T]) DataUnsafe() unsafe.Pointer {
	return p.v
}

// SetDataUnsafe implements [Pointer].
//
// Deprecated: Extremely unsafe. Do not use.
func (p *PointerTo[T]) SetDataUnsafe(v unsafe.Pointer) {
	p.v = v
}

// View creates a sub-pointer viewing a range of the original allocation.
// Uses Fortran 1-based indexing: View(1, 10) returns elements 1-10 inclusive.
//
// Example:
//
//	ptr := intrinsic.MALLOC[int32](100 * 4)
//	sub := ptr.View(10, 20)  // Elements 10-20 of original allocation
//	sub.At(1)                // First element of view (element 10 of original)
func (p PointerTo[T]) View(startOff, endOff int) PointerTo[T] {
	return NewPointerFromSlice(p.Slice()[startOff-1 : endOff])
}

// Array converts the Pointer to an Array for multi-dimensional operations.
// Creates a 1D array with Fortran indexing (lower bound 1, upper bound Len()).
//
// Example:
//
//	ptr := intrinsic.MALLOC[float64](100 * 8)
//	arr := ptr.Array(4,2,100)
//	arr.At(3,1,89)  // Access element at offset 3,1,89.
func (p PointerTo[T]) Array(shape ...int) *Array[T] {
	return NewArray(p.Slice(), shape...)
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
func UnsafePointerData[I integer, T any](p PointerTo[T]) I {
	conv := I(uintptr(p.v))
	if uintptr(conv) != uintptr(p.v) {
		panic("pointer address not representable by target integer size")
	}
	return conv
}

// PointerFrom creates a type-punned view of an existing [Pointer], reinterpreting
// the same memory as a different element type. This is useful for Fortran
// EQUIVALENCE semantics where the same memory is accessed with different types.
//
// # Type Punning Rules
//
// When reinterpreting memory from source type S to destination type D:
//
//  1. Total bytes remains constant: len(S) * sizeof(S) == len(D) * sizeof(D)
//  2. If sizeof(S) > sizeof(D): destination has MORE elements (expansion)
//     Example: float64[1] → int32[2] (8 bytes → 2×4 bytes)
//  3. If sizeof(S) < sizeof(D): destination has FEWER elements (contraction)
//     Example: int32[2] → float64[1] (2×4=8 bytes → 1×8 bytes)
//  4. If sizeof(S) == sizeof(D): destination has SAME element count
//     Example: int32[10] → float32[10] (both 4 bytes per element)
//
// # Alignment Requirements
//
// The function panics if the total byte size is not evenly divisible by the
// destination element size.
//
// # Example
//
//	// View float64 memory as int32 elements
//	floatPtr := MALLOC[float64](100 * 8)  // 100 float64 elements
//	intPtr := PointerFrom[int32](floatPtr)  // 200 int32 elements (same memory)
//
//	// Access the same 8 bytes as two different types
//	floatPtr.Set(1, 3.14)
//	lowBits := intPtr.At(1)   // Low 32 bits of 3.14
//	highBits := intPtr.At(2)  // High 32 bits of 3.14
//
// # Safety Warnings
//
// This function performs UNSAFE type punning:
//   - No guarantee that bit patterns are valid for the destination type
//   - Can violate Go's type safety and memory model
//   - Should only be used for Fortran interop where EQUIVALENCE is required
func PointerFrom[D any](src Pointer) (dst PointerTo[D]) {
	szS := src.SizeElement()
	szD := dst.SizeElement()
	nS := src.LenBuffer()
	ptr := src.DataUnsafe()
	if szS == szD {
		return PointerTo[D]{v: ptr, alloclen: nS}
	}

	// Calculate total bytes and new element count
	totalBytes := nS * szS
	// Check alignment compatibility
	if totalBytes%szD != 0 {
		panic(fmt.Sprintf("equivalence: incompatible sizes: %d bytes (from %d×%d) not divisible by destination element size %d",
			totalBytes, nS, szS, szD))
	}

	newSize := totalBytes / szD
	return PointerTo[D]{
		v:        ptr,
		alloclen: newSize,
	}
}
