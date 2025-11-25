# Fortran Array Implementation Design

## Overview

The `intrinsic.Array[T]` type provides a standards-compliant implementation of Fortran arrays with proper column-major layout and custom bounds support.

## Standards Compliance

### FORTRAN 77 (ANSI X3J3/90.4)

**Section 5.1.1** (line 2075-2076): Arrays have 1-7 dimensions
```fortran
DIMENSION arr(10)              ! 1D
DIMENSION matrix(3, 4)         ! 2D
DIMENSION cube(2, 3, 4, 5, 6, 7, 8)  ! 7D (maximum)
```

**Section 5.1.1.1**: Dimension declarator format is `[lower:]upper`
```fortran
DIMENSION arr(10)       ! Bounds: 1:10 (lower defaults to 1)
DIMENSION arr(-5:5)     ! Bounds: -5:5 (explicit lower bound)
DIMENSION arr(0:9)      ! Bounds: 0:9 (zero-based)
```

**Section 5.1.1.2** (line 2120-2121): Bounds can be negative, zero, or positive

**Table 1** (line 2425-2463): Subscript value formula defines column-major layout
- 1D: `subscript_value = 1 + (s1 - j1)`
- 2D: `subscript_value = 1 + (s1 - j1) + (s2 - j2)*d1`
- 3D: `subscript_value = 1 + (s1 - j1) + (s2 - j2)*d1 + (s3 - j3)*d2*d1`
- Where `di = ki - ji + 1` (size of dimension i)

**Example from standard** (line 2649-2653):
```fortran
DIMENSION A(-1:8)
A(2) = 42
! A(2) identifies the 4th element
! subscript_value = 1 + (2 - (-1)) = 4
```

### Fortran 95 (ISO/IEC 1539:1991)

**Section 6.2.2.3** (line 7246-7250): Array element order preserved from F77

**Table 6.1** (line 7269-7348): Same subscript order formula as F77

**Note 1** (line 7350): `di = max(ki - ji + 1, 0)`

**Additional features**:
- ALLOCATABLE arrays (line 5188-5193)
- Assumed-shape arrays (line 5631-5643)
- Array sections with stride (line 7358-7383)

## Column-Major Layout

Fortran uses **column-major** layout, meaning the **first index varies fastest** in memory.

For a 2D array `A(3, 4)`, the memory order is:
```
A(1,1), A(2,1), A(3,1), A(1,2), A(2,2), A(3,2), A(1,3), A(2,3), A(3,3), A(1,4), A(2,4), A(3,4)
```

This contrasts with C/Go's row-major layout where the **last index** varies fastest.

## Implementation Details

### Data Structure

```go
type Array[T any] struct {
    data   []T   // Single contiguous allocation (slab allocation)
    shape  []int // Size of each dimension
    lower  []int // Lower bounds (can be negative)
    upper  []int // Upper bounds
    stride []int // Column-major strides
}
```

### Stride Calculation

For column-major layout:
```
stride[0] = 1                      // First dimension varies fastest
stride[i] = stride[i-1] * shape[i-1]
```

Example for 3D array (2, 3, 4):
```
stride[0] = 1
stride[1] = 1 * 2 = 2
stride[2] = 2 * 3 = 6
```

### Offset Calculation

Implements the Fortran subscript value formula (converted to 0-based):
```
offset = sum((indices[i] - lower[i]) * stride[i]) for i = 0 to n-1
```

Example for 2D array with bounds (2:5, 3:7) accessing element (4, 6):
```
offset = (4 - 2) * 1 + (6 - 3) * 4 = 2 + 12 = 14
```

### Slab Allocation

The entire array is allocated as a single contiguous block:
```go
data: make([]T, totalSize)  // totalSize = product of all dimensions
```

Benefits:
- Single allocation instead of N allocations for multi-dimensional arrays
- Better cache locality
- Matches Fortran's memory model
- More efficient for large arrays

## API

### Constructors

```go
// 1D array with default bounds [1:size]
arr := intrinsic.NewArray1D[int32](10)

// 2D array with default bounds [1:rows, 1:cols]
matrix := intrinsic.NewArray2D[float32](3, 4)

// 3D array with default bounds [1:d1, 1:d2, 1:d3]
cube := intrinsic.NewArray3D[int32](2, 3, 4)

// Array with custom bounds
arr := intrinsic.NewArrayWithBounds[int32](
    []int{11, 10},  // shape
    []int{-5, 0},   // lower bounds
    []int{5, 9},    // upper bounds
)
```

### Access Methods

```go
// Get element (using Fortran indexing)
value := arr.At(i, j, k)

// Set element (using Fortran indexing)
arr.Set(value, i, j, k)

// Intrinsic equivalents
len := arr.Len()          // SIZE(array, 1)
shape := arr.Shape()      // SHAPE(array)
lower := arr.Lower()      // LBOUND(array)
upper := arr.Upper()      // UBOUND(array)
```

## Transpiler Integration

### Current Implementation (Nested Slices)

```go
// Fortran:
// INTEGER, DIMENSION(3, 4) :: matrix
// matrix(2, 3) = 42

// Generated Go (current):
var matrix [][]int32 = make([][]int32, 3)
for i := range matrix {
    matrix[i] = make([]int32, 4)  // N allocations!
}
matrix[2-1][3-1] = 42  // Manual index adjustment
```

### Proposed Implementation (Opaque Array)

```go
// Fortran:
// INTEGER, DIMENSION(3, 4) :: matrix
// matrix(2, 3) = 42

// Generated Go (proposed):
var matrix *intrinsic.Array[int32] = intrinsic.NewArray2D[int32](3, 4)
matrix.Set(42, 2, 3)  // Fortran indexing preserved!
```

### Benefits

1. **Single allocation**: More efficient memory usage
2. **Cleaner generated code**: No manual index arithmetic
3. **Fortran semantics**: Preserves 1-based indexing and custom bounds
4. **Column-major layout**: Matches Fortran memory model
5. **Type safety**: Bounds checking at runtime

### Required Transpiler Changes

1. **Array declaration**: Generate `*intrinsic.Array[T]` instead of `[][]T`
```go
// transformArrayDeclaration()
typeExpr := &ast.StarExpr{
    X: &ast.IndexExpr{
        X: &ast.SelectorExpr{
            X:   ast.NewIdent("intrinsic"),
            Sel: ast.NewIdent("Array"),
        },
        Index: elemType,  // int32, float32, etc.
    },
}
```

2. **Array initialization**: Generate `NewArray2D()` call
```go
// transformArrayDeclaration()
init := &ast.CallExpr{
    Fun: &ast.SelectorExpr{
        X:   ast.NewIdent("intrinsic"),
        Sel: ast.NewIdent("NewArray2D"),
    },
    Args: []ast.Expr{rows, cols},
}
```

3. **Array assignment**: Generate `.Set()` call
```go
// transformAssignment() for array LHS
lhs := &ast.CallExpr{
    Fun: &ast.SelectorExpr{
        X:   arrayIdent,
        Sel: ast.NewIdent("Set"),
    },
    Args: append([]ast.Expr{rhs}, indices...),
}
```

4. **Array reference**: Generate `.At()` call
```go
// transformArrayRef()
expr := &ast.CallExpr{
    Fun: &ast.SelectorExpr{
        X:   arrayIdent,
        Sel: ast.NewIdent("At"),
    },
    Args: indices,
}
```

5. **Remove array initialization loops**: No longer needed since Array handles initialization

## Testing

All tests pass (11/11) verifying:
- ✅ 1D, 2D, 3D arrays with default bounds
- ✅ Custom bounds (including negative)
- ✅ Column-major memory layout
- ✅ Subscript value formula (F77 Table 1)
- ✅ Stride calculation
- ✅ Bounds checking
- ✅ Intrinsic function equivalents (SIZE, SHAPE, LBOUND, UBOUND)

## Performance

Benchmarks show comparable performance to native Go slices, with benefits:
- Single allocation vs N allocations
- Better cache locality
- Reduced memory overhead

## Future Enhancements

1. **Array sections**: Support Fortran 95 array slicing `arr(1:10:2)`
2. **Array operations**: Element-wise operations `arr1 = arr2 + arr3`
3. **Assumed-shape arrays**: For subroutine parameters
4. **Allocatable arrays**: Dynamic allocation/deallocation
5. **Zero-copy interop**: Interface with C/Fortran libraries

## References

- FORTRAN 77 Standard (ANSI X3J3/90.4): docs/f77standard.txt
- Fortran 95 Standard (ISO/IEC 1539:1991): docs/f95standard.txt
- Implementation: intrinsic/array.go
- Tests: intrinsic/array_test.go
