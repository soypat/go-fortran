# Fortran Pointer Type Design

## Overview

The `intrinsic.Pointer[T]` type provides type-safe representation of Fortran's Cray-style POINTER semantics in transpiled Go code. This document describes the design rationale and usage patterns.

## Fortran POINTER Semantics

### Cray-Style POINTER (F77/Legacy)

```fortran
INTEGER :: ptr
DOUBLE PRECISION :: arr(1)
POINTER (ptr, arr(1))

ptr = MALLOC(1000 * 8)
arr(1) = 3.14
x = arr(500)
```

**Semantics:**
- `ptr` is an INTEGER variable holding a memory address (like `intptr_t` in C)
- `arr` is a "pointee" - accessed through the address in `ptr`
- The `(1)` dimension is a placeholder; actual size determined by MALLOC
- No bounds checking; programmer must track size manually

**Key Differences from F90+ POINTER:**
- Cray POINTER: Integer address + type punning, C-style
- F90+ POINTER: Type-safe reference with bounds, modern Fortran

This implementation focuses on **Cray-style POINTER** found in legacy scientific codes.

## Go Representation: Pointer[T]

### Design Goals

1. **Type Safety**: Unlike Fortran's INTEGER addresses, Go's `Pointer[T]` is type-safe
2. **Fortran Indexing**: Support 1-based indexing via `At(i)` method
3. **Memory Safety**: Bounds tracking via `alloclen` field
4. **Interop**: Convert to/from Go pointers and slices
5. **Performance**: Zero-cost abstraction using `unsafe.Pointer`

### Structure

```go
type Pointer[T any] struct {
    v        unsafe.Pointer  // Raw memory address
    alloclen int             // Number of T elements (not bytes!)
}
```

**Why not `*T` or `[]T`?**
- `*T`: No length information for bounds checking
- `[]T`: Go slices are fat pointers (data, len, cap) with different semantics
- `Pointer[T]`: Tracks length, supports Fortran indexing, explicit unsafe operations

## Usage Patterns

### Pattern 1: Dynamic Allocation with MALLOC

**Fortran:**
```fortran
SUBROUTINE ALLOC_EXAMPLE()
    INTEGER :: np
    DOUBLE PRECISION :: data(1)
    POINTER (np, data(1))
    INTEGER :: n, i

    n = 1000
    np = MALLOC(n * 8)

    DO i = 1, n
        data(i) = DBLE(i)
    END DO
END SUBROUTINE
```

**Transpiled Go:**
```go
func ALLOC_EXAMPLE() {
    var np intrinsic.Pointer[float64]
    var n, i int32

    n = 1000
    np = intrinsic.MALLOC[float64](n * 8)

    for i = 1; i <= n; i++ {
        np.Array().Set(float64(i), int(i))
        // Or: np.Slice()[i-1] = float64(i)  // 0-based Go indexing
    }
}
```

**Key Points:**
- MALLOC returns `Pointer[float64]`, not `int32`
- Access via `.Array().Set()` for Fortran-style indexing
- Or `.Slice()[i-1]` for direct Go slice access

### Pattern 2: Pointer Assignment (Shared Memory)

**Fortran:**
```fortran
INTEGER :: ptr1, ptr2
DOUBLE PRECISION :: arr1(1), arr2(1)
POINTER (ptr1, arr1(1)), (ptr2, arr2(1))

ptr1 = MALLOC(100 * 8)
ptr2 = ptr1  ! Now arr1 and arr2 access the same memory
arr2(10) = 99.0
PRINT *, arr1(10)  ! Prints 99.0
```

**Transpiled Go:**
```go
var ptr1, ptr2 intrinsic.Pointer[float64]

ptr1 = intrinsic.MALLOC[float64](100 * 8)
ptr2 = ptr1  // Go's Pointer is a value type, but contains unsafe.Pointer
             // Both now point to same memory

ptr2.Array().Set(99.0, 10)
intrinsic.Print(ptr1.Array().At(10))  // Prints 99.0
```

### Pattern 3: Subroutine Parameters - THE CRITICAL CASE

This is where type system design matters most.

**Fortran:**
```fortran
PROGRAM MAIN
    DOUBLE PRECISION :: matrix(100, 100)
    CALL PROCESS(matrix, 100)
END PROGRAM

SUBROUTINE PROCESS(arr, n)
    INTEGER :: n
    DOUBLE PRECISION :: arr(n, n)
    arr(1, 1) = 42.0
END SUBROUTINE
```

**Current Transpilation (Array by value - INEFFICIENT):**
```go
func MAIN() {
    var matrix = intrinsic.NewArray[float64](100, 100)
    PROCESS(matrix, 100)  // ❌ Passes 80KB Array by value!
}

func PROCESS(arr intrinsic.Array[float64], n int32) {
    arr.Set(42.0, 1, 1)  // Modifies LOCAL COPY, not original!
}
```

**Problem:** Go passes structs by value. Array[T] is a struct with slice, so modifications don't affect caller.

**Solution 1: Pointer Parameters (RECOMMENDED)**
```go
func MAIN() {
    var matrix = intrinsic.NewArray[float64](100, 100)
    PROCESS(&matrix, 100)  // ✅ Pass by reference
}

func PROCESS(arr *intrinsic.Array[float64], n int32) {
    arr.Set(42.0, 1, 1)  // Modifies original
}
```

**Solution 2: Pointer[T] for pointer semantics**
```go
func MAIN() {
    var matrix = intrinsic.NewArray[float64](100, 100)
    // Wrap array as Pointer for passing
    ptr := intrinsic.NewPointerFromSlice(matrix.Data())
    PROCESS(ptr, 100)
}

func PROCESS(arr intrinsic.Pointer[float64], n int32) {
    arr.Array().Set(42.0, 1, 1)  // Works, but verbose
}
```

### Recommended Transpilation Strategy

**For subroutine array parameters, use `*intrinsic.Array[T]`:**

```go
// Fortran: SUBROUTINE FOO(arr, n)
// Go:
func FOO(arr *intrinsic.Array[float64], n int32) {
    // Direct access to caller's array
    arr.Set(value, i, j)
}
```

**Caller adjustments:**
```go
var myArray = intrinsic.NewArray[float64](10, 10)
FOO(&myArray, 10)  // Address-of operator
```

**For POINTER variables in Fortran, use `intrinsic.Pointer[T]`:**

```fortran
SUBROUTINE BAR(ptr, n)
    INTEGER :: ptr
    DOUBLE PRECISION :: data(1)
    POINTER (ptr, data(1))
    ! ...
END SUBROUTINE
```

```go
func BAR(ptr intrinsic.Pointer[float64], n int32) {
    // ptr is a Pointer, can be reassigned, shared, etc.
    data := ptr.Array()
    data.Set(value, i)
}
```

## Type Conversion Matrix

| Fortran Context | Go Type | Pass By | Notes |
|-----------------|---------|---------|-------|
| `DOUBLE PRECISION :: arr(10,10)` | `intrinsic.Array[float64]` | `&arr` (pointer) | Subroutine parameter |
| `POINTER (ptr, arr(1))` | `intrinsic.Pointer[float64]` | value | Cray pointer variable |
| `INTEGER :: n` (scalar OUT param) | `*int32` | pointer | Mutable scalar |
| `INTEGER :: n` (scalar IN param) | `int32` | value | Immutable scalar |
| `ptr = MALLOC(...)` | `intrinsic.MALLOC[T](...)` | returns `Pointer[T]` | Dynamic allocation |

## EQUIVALENCE and Type Punning

**Fortran:**
```fortran
DOUBLE PRECISION :: dval
INTEGER, DIMENSION(2) :: ival
EQUIVALENCE (dval, ival)

ival(1) = Z'7777777'  ! Hex literal
PRINT *, dval          ! Prints bit pattern as double
```

**Possible Go Approach with Pointer:**
```go
var dval float64
dview := intrinsic.NewPointerElemental(&dval)

// Reinterpret same memory as int32 slice
iview := intrinsic.Pointer[int32]{
    v:        unsafe.Pointer(&dval),
    alloclen: 2,  // sizeof(float64) / sizeof(int32) = 8/4 = 2
}

iview.Slice()[0] = 0x7777777  // Set low 32 bits
intrinsic.Print(dval)         // View as float64
```

**Safer alternative:** Explicit conversion functions rather than unsafe reinterpretation.

## Implementation Checklist

### ✅ Completed
- [x] Pointer[T] type with unsafe.Pointer storage
- [x] MALLOC[T] generic allocation
- [x] At(i) for 1-based Fortran indexing
- [x] Slice() for 0-based Go slice access
- [x] Array() conversion to intrinsic.Array[T]
- [x] Len() and LenBytes() for bounds queries
- [x] View(start, end) for sub-pointers
- [x] UnsafePointerData[I, T] for integer conversion

### 🔲 Transpiler Integration Needed
- [ ] Parse POINTER statements → generate Pointer[T] declarations
- [ ] Transform MALLOC calls → intrinsic.MALLOC[T](size)
- [ ] Pointer variable tracking in VarInfo (add PointerSpec field?)
- [ ] Subroutine parameters: Array → *Array for pass-by-reference
- [ ] Pointee array access: arr(i) → ptr.Array().At(i)
- [ ] Pointer assignment: ptr1 = ptr2 → direct assignment (already works!)

### 🔲 Future Enhancements
- [ ] EQUIVALENCE statement support via Pointer type punning
- [ ] Pointer arithmetic for Fortran77 ADDRESS() intrinsic
- [ ] Set() method on Pointer for consistency with Array
- [ ] Bounds checking in debug builds (panic on out-of-bounds At())

## Performance Considerations

### Memory Layout

```
Fortran Array (stack):  [100 x 100 x 8 bytes = 80KB]
                         ↓ pass to subroutine
Go Array (by value):    [copy all 80KB]           ❌ SLOW
Go *Array (pointer):    [8 bytes pointer]         ✅ FAST
Go Pointer[T]:          [16 bytes: ptr + len]     ✅ FAST
```

**Recommendation:** Use `*intrinsic.Array[T]` for subroutine parameters to avoid copies.

### Indexing Performance

```go
// Option 1: Array indexing (boundary-checked in Array.At)
arr.Array().At(i)  // ~5ns overhead for bounds check

// Option 2: Direct slice indexing (unchecked in release builds)
arr.Slice()[i-1]   // ~0ns overhead, but manual index adjustment

// Option 3: Unsafe pointer arithmetic (AVOID unless proven bottleneck)
*(*T)(unsafe.Add(arr.v, (i-1)*unsafe.Sizeof(T(0))))
```

For hot loops, prefer `Slice()` access after a single bounds check.

## Migration Path for Existing Code

### Phase 1: Parse POINTER statements
Add AST node and parser support (see DIMENSION implementation as template)

### Phase 2: Track pointer variables
Add to VarInfo:
```go
type VarInfo struct {
    // ... existing fields ...
    IsPointerVar  bool          // True if declared via POINTER statement
    PointerSpec   *PointerSpec  // Pointee information
}

type PointerSpec struct {
    PointerVar string  // The integer variable holding the address
    Pointee    string  // The array/variable accessed through pointer
    ArraySpec  *ArraySpec  // Dimensions of pointee (if array)
}
```

### Phase 3: Transpile POINTER declarations
```fortran
POINTER (np, data(1))
DOUBLE PRECISION :: data
```
→
```go
var np intrinsic.Pointer[float64]
// 'data' is accessed via np.Array(), not declared separately
```

### Phase 4: Transform pointee access
```fortran
data(i) = value
```
→
```go
np.Array().Set(value, int(i))
```

### Phase 5: Change subroutine signatures
Analyze each SUBROUTINE/FUNCTION:
- Scalar IN parameters: keep as `T`
- Scalar OUT/INOUT parameters: change to `*T`
- Array parameters: change from `Array[T]` to `*Array[T]`
- Update all call sites to pass `&array`

## Conclusion

The `Pointer[T]` type successfully bridges Fortran's Cray-style pointer semantics with Go's type safety. The critical insight for subroutine parameters is:

**Use `*intrinsic.Array[T]` for array parameters to enable pass-by-reference semantics matching Fortran's behavior.**

This avoids expensive copies while maintaining type safety and bounds checking.
