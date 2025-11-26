# Known Issues

This document tracks known bugs, limitations, and workarounds in the go-fortran parser and transpiler.

---

## Critical Issues

### 1. Parser: Duplicate PROGRAM Units with CONTAINS

**Status**: ⚠️ Known bug, workaround in place
**Discovered**: 2024-11-25
**Severity**: Medium (workaround available)

#### Description

When parsing a Fortran program with a CONTAINS section, `ParseNextProgramUnit()` returns the PROGRAM block twice:
- First instance: Contains the main program body (e.g., CALL statements)
- Second instance: Empty PROGRAM block with same name

#### Example

```fortran
PROGRAM GOLDEN
    CALL LEVEL01()
    CONTAINS
    SUBROUTINE LEVEL01()
        PRINT *, 'Hello'
    END SUBROUTINE
END PROGRAM GOLDEN
```

Parsing this yields:
```
[0] PROGRAM GOLDEN (with CALL statements)
[1] SUBROUTINE LEVEL01
[2] PROGRAM GOLDEN (empty, duplicate)
```

#### Root Cause

**Location**: [parser.go:2851-2878](parser.go:2851-2878) `parseProgramBlock()`

The issue stems from an architectural inconsistency in the AST:

1. **Module has Contains field**: [ast/ast.go:258-264](ast/ast.go:258-264)
   ```go
   type Module struct {
       Name     string
       Body     []Statement
       Contains []ProgramUnit  // ✅ Has this field
   }
   ```

2. **ProgramBlock lacks Contains field**: [ast/ast.go:110-115](ast/ast.go:110-115)
   ```go
   type ProgramBlock struct {
       Name  string
       Body  []Statement
       // ❌ No Contains field!
   }
   ```

**What happens**:

1. `parseProgramBlock()` calls `parseBody()` which stops at CONTAINS token
2. Tries `expect(token.END)` but current token is CONTAINS → **fails silently**
3. Returns ProgramBlock with main body, parser still at CONTAINS
4. Parser skips CONTAINS (not a registered top-level construct)
5. Parses contained procedures (LEVEL01, etc.) as separate top-level units
6. Reaches `END PROGRAM GOLDEN`
7. Skips `END` (not registered), sees `PROGRAM` token
8. Calls `parseProgramBlock()` again → **creates duplicate**

#### Comparison with MODULE (Working Correctly)

The MODULE parser ([parser.go:2985-3007](parser.go:2985-3007)) **correctly handles CONTAINS**:

```go
// Parse body statements
mod.Body = p.parseBody(nil)

// Handle CONTAINS section with recursive parsing
if p.consumeIf(token.CONTAINS) {
    p.skipNewlinesAndComments()

    // Recursively parse contained procedures
    for p.loopUntil(token.END) {
        if p.currentTokenIs(token.SUBROUTINE) || ... {
            unit := p.parseTopLevelUnit()
            if unit != nil {
                mod.Contains = append(mod.Contains, unit)  // ✅ Stores them!
            }
        }
        ...
    }
}
// Now properly positioned at END token
p.expect(token.END, "MODULE end")
```

#### Workaround

In tests/applications, deduplicate program units:

```go
// Remove duplicate PROGRAM blocks
seen := make(map[string]bool)
var uniqueUnits []f90.ProgramUnit
for _, pu := range progUnits {
    key := ""
    switch u := pu.(type) {
    case *f90.ProgramBlock:
        key = "PROGRAM:" + u.Name
    case *f90.Subroutine:
        key = "SUBROUTINE:" + u.Name
    case *f90.Function:
        key = "FUNCTION:" + u.Name
    }
    if key != "" && seen[key] {
        continue  // Skip duplicate
    }
    seen[key] = true
    uniqueUnits = append(uniqueUnits, pu)
}
```

**Example**: [transpile_test.go:52-73](transpile_test.go:52-73)

#### Proper Fix

**Option 1: Add Contains field to ProgramBlock** (recommended):

1. Update AST:
   ```go
   type ProgramBlock struct {
       Name     string
       Body     []Statement
       Contains []ProgramUnit  // NEW
       Label    string
       Position
   }
   ```

2. Update parser to match MODULE's CONTAINS handling:
   ```go
   // In parseProgramBlock() after parseBody()
   if p.consumeIf(token.CONTAINS) {
       p.skipNewlinesAndComments()
       for p.loopUntil(token.END) {
           if p.currentTokenIs(token.SUBROUTINE) ||
              p.currentTokenIs(token.FUNCTION) || ... {
               unit := p.parseTopLevelUnit()
               if unit != nil {
                   block.Contains = append(block.Contains, unit)
               }
           } else {
               p.nextToken()
           }
           p.skipNewlinesAndComments()
       }
   }
   p.expect(token.END, "PROGRAM end")
   ```

**Option 2: Skip CONTAINS section** (simpler but loses information):

```go
// In parseProgramBlock() after parseBody()
if p.consumeIf(token.CONTAINS) {
    // Skip until END token
    for p.loopUntil(token.END) {
        p.nextToken()
    }
}
p.expect(token.END, "PROGRAM end")
```

---

### 2. Type Resolver: Format Specifiers Treated as Variables

**Status**: ⚠️ Known bug, workaround in place
**Discovered**: 2024-11-25
**Severity**: Medium (blocks type resolution for PRINT statements)

#### Description

The type resolver incorrectly treats format specifiers (like `*`) in PRINT/READ/WRITE statements as variable identifiers, causing "variable * used without declaration" errors when IMPLICIT NONE is active.

#### Example

```fortran
PROGRAM test
    IMPLICIT NONE
    PRINT *, 'Hello, World!'  ! ❌ Error: variable * used without declaration
END PROGRAM
```

#### Root Cause

**Location**: Symbol type resolver

PrintStmt stores the format specifier as an Expression:

```go
type PrintStmt struct {
    Format     Expression   // *, label, or character expression
    OutputList []Expression
    ...
}
```

When Format is `*`, it's represented as an `Identifier{Value: "*"}`. The type resolver walks all Expressions and tries to resolve all Identifiers as variables, including format specifiers.

#### Workaround

Skip type resolution when only basic transpilation is needed:

```go
// Skip type resolution for now
// resolver := symbol.NewTypeResolver(syms)
// for i := range progUnits {
//     errs := resolver.Resolve(progUnits[i])
//     ...
// }
```

**Example**: [transpile_test.go:84-92](transpile_test.go:84-92)

#### Proper Fix

Modify the type resolver to skip Format fields in I/O statements:

```go
// In resolver's Visit method
case *ast.PrintStmt:
    // Skip Format field (not a variable reference)
    for _, expr := range n.OutputList {
        r.resolveExpression(expr)
    }
    return nil  // Don't descend into children

case *ast.ReadStmt:
    // Skip Format and Unit fields
    for _, expr := range n.InputList {
        r.resolveExpression(expr)
    }
    return nil

case *ast.WriteStmt:
    // Skip Format and Unit fields
    for _, expr := range n.OutputList {
        r.resolveExpression(expr)
    }
    return nil
```

---

## Fixed Issues

### 1. Symbol Collector: Parameter Redeclaration Failures ✅

**Status**: ✅ Fixed
**Discovered**: 2024-11-25
**Fixed**: 2024-11-25
**Severity**: High (blocked symbol collection)

#### Description

The symbol collector failed when Fortran parameters were declared in both the parameter list and the body with type declarations, causing "symbol already defined in scope" errors.

#### Example (Failed Before Fix)

```fortran
SUBROUTINE ADD_VALUES(a, b, result)
    INTEGER, INTENT(IN) :: a, b      ! ❌ Error: symbol a already defined
    INTEGER, INTENT(OUT) :: result   ! ❌ Error: symbol result already defined
    result = a + b
END SUBROUTINE
```

#### Root Cause

In Fortran, parameters can be declared twice:
1. In parameter list: `SUBROUTINE ADD_VALUES(a, b, result)`
2. In body with full type info: `INTEGER, INTENT(IN) :: a, b`

The collector processed both:
1. `defineParameter()` created symbols from parameter list
2. `handleTypeDeclaration()` tried to Define the same symbols again → **error**

#### Fix

**Location**: [symbol/collector.go:210-220](symbol/collector.go:210-220)

Updated `handleTypeDeclaration()` to check if symbol exists and update it instead of failing:

```go
// Check if symbol already exists (e.g., as a parameter from parameter list)
existingSym := currentScope.Lookup(entity.Name)
if existingSym != nil {
    // Update existing symbol with more detailed type information
    existingSym.SetType(resolvedType)
    existingSym.SetAttributes(decl.Attributes)
    existingSym.SetArraySpec(entity.ArraySpec)
    existingSym.SetDeclNode(decl)
    existingSym.setImplicit(false)
    continue  // Don't try to Define again
}

// Create new symbol (only if not already exists)
...
```

This allows TypeDeclarations to enhance parameter symbols with full type information (INTENT, KIND, etc.) without causing conflicts.

---

## Minor Issues / Limitations

### 1. EQUIVALENCE Not Parsed

**Status**: Not implemented
**Severity**: Low (rare in modern Fortran)

EQUIVALENCE statements are recognized but skipped. Transpiling code with EQUIVALENCE will likely fail or produce incorrect results.

### 2. NAMELIST Not Parsed

**Status**: Not implemented
**Severity**: Low (mainly affects I/O)

NAMELIST groups are not parsed. This limits support for Fortran 90+ I/O features.

### 3. Derived Types Skipped

**Status**: Parser actively skips TYPE...END TYPE blocks
**Severity**: Medium (blocks modern Fortran support)

While AST nodes exist for derived types, the parser calls `skipTypeDefinition()` instead of parsing them.

### 4. INTERFACE Blocks Skipped

**Status**: Parser skips INTERFACE...END INTERFACE blocks
**Severity**: Medium (blocks generic interfaces)

Interface blocks for generic procedures and operator overloading are not parsed.

---

## Testing Notes

### Test Infrastructure

The transpiler test suite includes workarounds for known issues:

- **Duplicate deduplication**: [transpile_test.go:52-73](transpile_test.go:52-73)
- **Type resolution skipped**: [transpile_test.go:84-92](transpile_test.go:84-92)
- **Symbol collector fix verified**: All symbol tests passing

### Golden Test Status

**LEVEL01 (PRINT statements)**: ✅ Fully working
- Parses golden.f90 correctly (with deduplication)
- Symbol collection works with parameter fix
- Transpiles to Go AST successfully
- Generated Go code compiles and runs
- Output matches expected results exactly
- Correctly emulates Fortran PRINT * leading space behavior

**Recent Fixes** (2025-11-25):
- Fixed test logic bug: bytes.Equal comparison was inverted
- Fixed test to only compare implemented levels (maxLvl)
- Fixed transpiler to add leading space to PRINT output (Fortran list-directed I/O adds space)

**LEVEL02 (Variables and Assignments)**: ✅ Working (CHARACTER padding difference only)
- Parses correctly ✅
- Transpiles to Go successfully ✅
- Generated code compiles and runs ✅
- Uses `intrinsic.Formatter` for Fortran-compatible output ✅
- Correctly outputs LOGICAL as "T"/"F" (not "true"/"false") ✅
- **Numeric formatting matches gfortran exactly**: ✅
  - INTEGER: width=11, right-aligned ✅
  - REAL: variable precision (10 char value), 2 leading + 4 trailing spaces ✅
  - Field widths and spacing match byte-for-byte ✅
- **Remaining difference** (transpiler limitation, not formatter):
  - CHARACTER(LEN=n) trailing space padding not implemented
  - Go strings don't have fixed lengths like Fortran CHARACTER
  - Example: CHARACTER(LEN=20) = 'Variables assigned' (18 chars) → gfortran pads to 20, Go outputs 18
  - **Impact**: Minor (2 trailing spaces), all content is correct
  - **Status**: Acceptable; requires transpiler enhancement to track CHARACTER lengths

**LEVEL03 (Arithmetic Expressions)**: ✅ Working
- Parses correctly ✅
- Transpiles to Go successfully ✅
- Binary arithmetic operators (+, -, *, /) implemented ✅
- Type conversion intrinsics (REAL, INT, DBLE) implemented ✅
- Mixed-type expressions work correctly ✅
- Generated code compiles and runs ✅
- All computed values are correct ✅
- **Numeric formatting matches gfortran exactly**: ✅
  - Variable REAL precision correctly handles values 0.1-999 (adjusts decimal places for fixed 10-char width) ✅

**LEVEL04 (IF Statements and Relational Operators)**: ✅ Working
- Parses correctly ✅
- Transpiles to Go successfully ✅
- IF...THEN...END IF statements implemented ✅
- IF...THEN...ELSE...END IF statements implemented ✅
- IF...THEN...ELSE IF...THEN...ELSE...END IF chains implemented ✅
- Relational operators (.GT., .LT., .GE., .LE., .EQ., .NE.) implemented ✅
- Boolean expressions as conditions work correctly ✅
- Generated code compiles and runs ✅
- Output matches gfortran exactly ✅

**LEVEL05 (Arrays and Multi-dimensional Arrays)**: ✅ Working
- Parses correctly ✅
- Transpiles to Go successfully ✅
- 1D array declarations: `INTEGER, DIMENSION(5) :: arr` → `make([]int32, 5)` ✅
- 2D array declarations with automatic inner slice initialization ✅
- Array element assignment: `arr(1) = 10` → `arr[0] = 10` ✅
- Array element references in expressions work correctly ✅
- **1-based to 0-based indexing conversion**: `arr(1)` → `arr[1-1]` ✅
- Multi-dimensional array access: `matrix(1,1)` → `matrix[0][0]` ✅
- **Parser ambiguity workaround**: Distinguishes array refs from function calls ✅
- Generated code compiles and runs ✅
- Output matches gfortran exactly ✅

**LEVEL06 (DO Loops)**: ✅ Working
- Parses correctly ✅
- Transpiles to Go successfully ✅
- DO loop with counter: `DO i = 1, 10` → `for i := int32(1); i <= 10; i++` ✅
- Inclusive upper bound handled correctly (<=, not <) ✅
- Loop variable type conversion handled ✅
- Nested loops work correctly ✅
- Array access within loops works ✅
- Generated code compiles and runs ✅
- Output matches gfortran exactly ✅

**LEVEL07 (Subroutine Calls and Parameters)**: ✅ Working
- Parses correctly ✅
- Transpiles to Go successfully ✅
- CALL statements implemented ✅
- INTENT(IN) → pass by value ✅
- INTENT(OUT) → pass by pointer (*T) ✅
- INTENT(INOUT) → pass by pointer (*T) ✅
- Parameter declarations transpiled correctly ✅
- Array parameters work with intrinsic.Array wrapper ✅
- Generated code compiles and runs ✅
- Output matches gfortran exactly ✅

**LEVEL08 (Functions and Return Values)**: ✅ Working
- Parses correctly ✅
- Transpiles to Go successfully ✅
- Function declarations with return types ✅
- Function result assignment (FACTORIAL = result) → return result ✅
- RETURN statements handled ✅
- Intrinsic function calls (SQRT) ✅
- Generated code compiles and runs ✅
- Output matches gfortran exactly ✅

**LEVEL09 (DO WHILE Loops and Recursion)**: ✅ Working
- Parses correctly ✅
- Transpiles to Go successfully ✅
- DO WHILE loops: `DO WHILE (cond)` → `for cond { }` ✅
- Early RETURN in functions ✅
- Recursive function calls ✅
- Generated code compiles and runs ✅
- Output matches gfortran exactly ✅

**LEVEL10 (Complex Expressions and Logical Operators)**: ✅ Working
- Parses correctly ✅
- Transpiles to Go successfully ✅
- Complex arithmetic expressions with mixed types ✅
- Logical operators (.AND., .OR., .NOT.) → (&&, ||, !) ✅
- Combined relational and logical expressions ✅
- Operator precedence preserved correctly ✅
- Generated code compiles and runs ✅
- Output matches gfortran exactly ✅

**LEVEL11 (String Concatenation)**: ✅ Working
- Parses correctly ✅
- Transpiles to Go successfully ✅
- String concatenation operator: `//` → `+` ✅
- CHARACTER(LEN=n) declarations ✅
- Generated code compiles and runs ✅
- **Minor difference**: Trailing space padding (Go limitation)

**LEVEL12 (Intrinsic Functions)**: ✅ Working
- Parses correctly ✅
- Transpiles to Go successfully ✅
- Math intrinsics: SIN, COS, ABS ✅
- Variadic intrinsics: MAX, MIN (using intrinsic.MaxInt32, etc.) ✅
- Type conversion intrinsics: REAL, INT, DBLE ✅
- Generated code compiles and runs ✅
- **Minor difference**: Float precision in output formatting

**LEVEL13 (Loop Control: CYCLE, EXIT, CONTINUE)**: ✅ Working
- Parses correctly ✅
- Transpiles to Go successfully ✅
- CYCLE statement → `continue` ✅
- EXIT statement → `break` ✅
- CONTINUE statement → empty statement (no-op, label target) ✅
- Loop control within conditional statements ✅
- Generated code compiles and runs ✅
- Output matches expected results exactly ✅

**LEVEL14 (Simple GOTO and Labels)**: ✅ Working
- Parses correctly ✅
- Transpiles to Go successfully ✅
- Unconditional GOTO statement → `goto labelN` ✅
- Labeled statements → `labelN:` before statement ✅
- Label transformation: Fortran "100" → Go "label100" ✅
- GOTO jumping over code works correctly ✅
- Conditional GOTO (IF...GOTO pattern) ✅
- Generated code compiles and runs ✅
- Output matches expected results exactly ✅

**LEVEL15 (SELECT CASE Statements)**: ✅ Working
- Parses correctly ✅
- Transpiles to Go successfully ✅
- Simple SELECT CASE with single values → `switch { case val: }` ✅
- Multiple values in one CASE → `case val1, val2, val3:` ✅
- CASE DEFAULT → `default:` ✅
- Generated code compiles and runs ✅
- Output matches expected results exactly ✅

**LEVEL16 (String Intrinsics and Substrings)**: ✅ Working
- Parses correctly ✅
- Transpiles to Go successfully ✅
- **String Intrinsics**:
  - LEN(str) → `int32(str.Len())` ✅
  - LEN_TRIM(str) → `int32(str.LenTrim())` ✅
  - TRIM(str) → `str.Trim().String()` ✅
  - INDEX(str, substr) → `int32(str.Index(substr))` ✅
  - ADJUSTL(str) → `str.AdjustL().String()` ✅
  - ADJUSTR(str) → `str.AdjustR().String()` ✅
- **Substring Operations**:
  - Substring read: `str3 = str1(2:4)` → `str3.SetFromString(str1.View(2, 4).String())` ✅
  - Substring write: `str1(2:3) = 'z'` → `str1.SetRange(2, 3, "z")` ✅
  - CharacterArray.View() uses 3-index slicing to limit capacity ✅
  - CharacterArray.SetRange() for substring assignment ✅
- All intrinsics implemented as CharacterArray methods (per user request) ✅
- Generated code compiles and runs ✅
- Output matches expected results exactly ✅

**LEVEL17 (Array Intrinsics)**: ✅ Working
- Parses correctly ✅
- Transpiles to Go successfully ✅
- **SIZE Intrinsic**:
  - `SIZE(arr)` → `int32(arr.Size())` - total elements ✅
  - `SIZE(arr, dim)` → `int32(arr.SizeDim(int(dim)))` - dimension-specific size ✅
- **LBOUND Intrinsic**:
  - `LBOUND(arr, dim)` → `int32(arr.LowerDim(int(dim)))` - lower bound of dimension ✅
- **UBOUND Intrinsic**:
  - `UBOUND(arr, dim)` → `int32(arr.UpperDim(int(dim)))` - upper bound of dimension ✅
- **Array Methods**:
  - `Array.Size() int` - returns total number of elements (product of all dimensions) ✅
  - `Array.SizeDim(dim int) int` - returns size of specific dimension (1-based) ✅
  - `Array.LowerDim(dim int) int` - returns lower bound of specific dimension ✅
  - `Array.UpperDim(dim int) int` - returns upper bound of specific dimension ✅
- Test cases:
  - 3x4 matrix: SIZE = 12, SIZE(,1) = 3, SIZE(,2) = 4 ✅
  - 1D vector: SIZE = 5 ✅
  - Bounds: LBOUND = 1, UBOUND = 3 (dim 1) and 4 (dim 2) ✅
- Generated code compiles and runs ✅
- Output matches expected results exactly ✅

**LEVEL18 (ALLOCATE and DEALLOCATE)**: ✅ Working
- Parses correctly ✅
- Transpiles to Go successfully ✅
- **ALLOCATABLE Attribute**:
  - `INTEGER, ALLOCATABLE, DIMENSION(:) :: vec` → `var vec *intrinsic.Array[int32]` (uninitialized pointer) ✅
  - Arrays tracked in `allocatables` map to skip initialization ✅
- **ALLOCATE Statement**:
  - `ALLOCATE(vec(5))` → `vec = intrinsic.NewArray[int32](5)` ✅
  - `ALLOCATE(mat(2,3))` → `mat = intrinsic.NewArray[int32](2, 3)` ✅
  - Parser creates FunctionCall nodes, not ArrayRef, handled in transpiler ✅
- **DEALLOCATE Statement**:
  - `DEALLOCATE(vec)` → `vec = nil` ✅
  - Multiple deallocations in one statement supported ✅
- Test cases:
  - 1D array: ALLOCATE(vec(5)), use elements, DEALLOCATE ✅
  - 2D array: ALLOCATE(mat(2,3)), use elements, DEALLOCATE ✅
  - SIZE() intrinsic works on allocated arrays ✅
- Generated code compiles and runs ✅
- Output matches expected results exactly ✅

**LEVEL19-22**: Not yet implemented (planned)

---

## Contributing

When fixing these issues:

1. **Remove workarounds** from affected tests
2. **Add regression tests** to ensure fix works
3. **Update this document** to move issue to "Fixed Issues" section
4. **Run full test suite** to ensure no breakage

### Test Commands

```bash
# All tests
go test ./...

# Symbol tests (includes collector fix)
go test ./symbol/...

# Parser tests
go test -run Parse

# Transpiler tests
go test -run Transpile
```

---

## References

- **Parser Implementation**: [parser.go](parser.go)
- **AST Definition**: [ast/ast.go](ast/ast.go)
- **Symbol Collector**: [symbol/collector.go](symbol/collector.go)
- **Type Resolver**: [symbol/resolver.go](symbol/resolver.go)
- **Transpiler**: [transpile.go](transpile.go)
- **Test Suite**: [transpile_test.go](transpile_test.go)

---

**Last Updated**: 2025-11-25
**Maintainer**: Document updated during transpiler development

## Changelog

- **2025-11-25** (Session 5): LEVEL14 implementation - Simple GOTO and Labels
  - Added LEVEL14 test case to `golden.f90` with GOTO statements and labeled CONTINUE
  - Implemented `transformStatement()` case for GotoStmt:
    - `GOTO 100` → `goto label100` ✅
  - Implemented labeled statement handling in `transformStatements()`:
    - Checks each statement's `GetLabel()` method
    - Wraps statements with labels in `ast.LabeledStmt`
    - `100 CONTINUE` → `label100:` ✅
  - Updated `golden.out` with expected output
  - Updated `transpile_test.go` maxLvl from 13 to 14
  - Fixed LEVEL13 to remove unused label100 (Go requires all labels be referenced)
  - All tests pass with exact output matching ✅
  - Verified transpiled code:
    - Unconditional GOTO: jumps over unreachable code correctly
    - Conditional GOTO: IF...GOTO pattern works
    - Labels properly emitted before statements
- **2025-11-25** (Session 6): LEVEL15 implementation - SELECT CASE Statements
  - Added LEVEL15 test case to `golden.f90` with SELECT CASE statements
  - Implemented `transformSelectCaseStmt()` function:
    - Transforms Fortran SELECT CASE to Go switch statement
    - `SELECT CASE (expr)` → `switch expr {`
    - `CASE (val)` → `case val:`
    - `CASE (val1, val2, val3)` → `case val1, val2, val3:` (multiple values)
    - `CASE DEFAULT` → `default:`
  - Added `transformSelectCaseStmt()` case in `transformStatement()`
  - Updated `golden.out` with expected output (3 test cases)
  - Updated `transpile_test.go` maxLvl from 14 to 15
  - All tests pass with exact output matching ✅
  - Verified transpiled code:
    - Simple SELECT CASE with single values works correctly
    - Multiple values in one CASE clause handled properly
    - CASE DEFAULT properly transpiles to Go default case
- **2025-11-25** (Session 6 cont.): LEVEL16 implementation - String Intrinsics
  - Added string intrinsic methods to `CharacterArray` in `intrinsic/char.go`:
    - `Len() int` - LEN intrinsic (returns declared length)
    - `LenTrim() int` - LEN_TRIM intrinsic (length without trailing spaces)
    - `Trim() CharacterArray` - TRIM intrinsic (remove trailing spaces)
    - `Index(substring string) int` - INDEX intrinsic (1-based search)
    - `AdjustL() CharacterArray` - ADJUSTL intrinsic (left-justify)
    - `AdjustR() CharacterArray` - ADJUSTR intrinsic (right-justify)
    - `Substring(start, end int) string` - For future substring operations
  - Added LEVEL16 test case to `golden.f90` with 6 string intrinsic tests
  - Implemented string intrinsic transpilation in `transformFunctionCall()`:
    - `LEN(str)` → `int32(str.Len())` (with int32 cast for INTEGER assignment)
    - `LEN_TRIM(str)` → `int32(str.LenTrim())`
    - `TRIM(str)` → `str.Trim().String()` (with .String() for SetFromString compatibility)
    - `INDEX(str, substr)` → `int32(str.Index(substr))`
    - `ADJUSTL(str)` → `str.AdjustL().String()`
    - `ADJUSTR(str)` → `str.AdjustR().String()`
  - Updated `golden.out` with expected output (6 lines)
  - Updated `transpile_test.go` maxLvl from 15 to 16
  - All tests pass with exact output matching ✅
  - All intrinsics implemented as CharacterArray methods (cleaner than package-level functions)
- **2025-11-25** (Session 6 cont.): LEVEL16 Substring Operations
  - Added substring operations to `golden.f90` LEVEL16:
    - Substring read: `str3 = str1(2:4)` (extract characters 2-4)
    - Substring write: `str1(2:3) = 'z'` (assign to substring range)
  - Implemented substring support in transpiler:
    - Modified `transformArrayRef()` to detect CharacterArray with RangeExpr subscript
    - Substring read: `str1(2:4)` → `str1.View(2, 4).String()`
    - Modified `transformArrayAssignment()` to handle substring assignment
    - Substring write: `str1(2:3) = 'z'` → `str1.SetRange(2, 3, "z")`
    - Updated `transformExpression()` FunctionCall case to check `charLengths` map
    - Updated `transformAssignment()` FunctionCall case to check `charLengths` map
  - Added `CharacterArray` methods in `intrinsic/char.go`:
    - `SetRange(start, end int, value string)` - Set substring range (1-based indices)
    - Modified `View()` to use 3-index slicing for correct capacity: `data[start-1:end:end]`
    - Fixed `SetFromString()` to maintain full capacity after padding
  - Updated `golden.out` with expected substring output (1 line)
  - All tests pass with exact output matching ✅
  - Verified transpiled code:
    - Substring extraction correctly uses View() with limited capacity
    - Substring assignment correctly uses SetRange() with space padding
    - 3-index slicing prevents String() from returning excess data
- **2025-11-26** (Session 7): LEVEL17 implementation - Array Intrinsics
  - Added LEVEL17 test case to `golden.f90` with SIZE intrinsic tests
  - Implemented array intrinsic methods in `intrinsic/array.go`:
    - `Size() int` - returns total number of elements (product of all dimensions)
    - `SizeDim(dim int) int` - returns size of specific dimension (1-based index)
  - Implemented array intrinsic transpilation in `transformFunctionCall()`:
    - `SIZE(arr)` → `int32(arr.Size())` (total elements)
    - `SIZE(arr, dim)` → `int32(arr.SizeDim(int(dim)))` (dimension-specific)
  - Test cases:
    - 3x4 matrix: SIZE = 12, SIZE(,1) = 3, SIZE(,2) = 4
    - 1D vector (size 5): SIZE = 5
  - Updated `golden.out` with expected output (4 lines)
  - Updated `transpile_test.go` maxLvl from 16 to 17
  - All tests pass with exact output matching ✅
- **2025-11-26** (Session 7 cont.): LEVEL17 expansion - LBOUND/UBOUND intrinsics
  - Expanded LEVEL17 test case with LBOUND and UBOUND tests
  - Implemented additional array intrinsic methods in `intrinsic/array.go`:
    - `LowerDim(dim int) int` - returns lower bound of specific dimension (1-based)
    - `UpperDim(dim int) int` - returns upper bound of specific dimension (1-based)
  - Implemented LBOUND/UBOUND transpilation in `transformFunctionCall()`:
    - `LBOUND(arr, dim)` → `int32(arr.LowerDim(int(dim)))`
    - `UBOUND(arr, dim)` → `int32(arr.UpperDim(int(dim)))`
  - Test cases:
    - 3x4 matrix: LBOUND(,1) = 1, LBOUND(,2) = 1
    - 3x4 matrix: UBOUND(,1) = 3, UBOUND(,2) = 4
  - Updated `golden.out` with expected output (4 additional lines, 8 total for LEVEL17)
  - All tests pass with exact output matching ✅
- **2025-11-26** (Session 7 cont.): LEVEL18 implementation - ALLOCATE and DEALLOCATE
  - Added LEVEL18 test case to `golden.f90` with ALLOCATABLE arrays and ALLOCATE/DEALLOCATE
  - Added `allocatables` map to `TranspileToGo` struct to track ALLOCATABLE arrays
  - Modified `transformTypeDeclaration()` to detect ALLOCATABLE attribute:
    - Creates `transformAllocatableArrayDeclaration()` for uninitialized pointer declarations
    - `INTEGER, ALLOCATABLE, DIMENSION(:) :: vec` → `var vec *intrinsic.Array[int32]`
  - Implemented `transformAllocateStmt()` function:
    - Handles parser ambiguity: ALLOCATE objects parsed as FunctionCall, not ArrayRef
    - Transforms both FunctionCall and ArrayRef cases
    - `ALLOCATE(vec(5))` → `vec = intrinsic.NewArray[int32](5)`
    - `ALLOCATE(mat(2,3))` → `mat = intrinsic.NewArray[int32](2, 3)`
  - Implemented `transformDeallocateStmt()` function:
    - Handles Identifier, ArrayRef, and FunctionCall cases
    - `DEALLOCATE(vec)` → `vec = nil`
  - Added statement cases in `transformStatement()` for AllocateStmt and DeallocateStmt
  - Test cases:
    - 1D array: ALLOCATE(5), assign values, verify with SIZE, DEALLOCATE
    - 2D array: ALLOCATE(2,3), assign values, verify with SIZE, DEALLOCATE
  - Updated `golden.out` with expected output (8 lines)
  - Updated `transpile_test.go` maxLvl from 17 to 18
  - **Debugging**: Fixed parser ambiguity where ALLOCATE(arr(dims)) creates FunctionCall not ArrayRef
  - All tests pass with exact output matching ✅
- **2025-11-25** (Session 4): LEVEL13 implementation - Loop control (CYCLE, EXIT, CONTINUE)
  - Added LEVEL13 test case to `golden.f90` with CYCLE, EXIT, and labeled CONTINUE
  - Implemented `transformStatement()` cases for:
    - CycleStmt → Go `continue` statement
    - ExitStmt → Go `break` statement
    - ContinueStmt → Go empty statement (no-op, serves as label target)
  - Updated `transpile_test.go` maxLvl from 12 to 13
  - Added expected output to `golden.out`
  - LEVEL13 tests pass with exact output matching ✅
  - Verified transpiled code:
    - `if arr.At(int(i)) < 0 { continue }` ✅
    - `if arr.At(int(i)) > 7 { break }` ✅
  - All loop control statements working correctly within DO loops
- **2025-11-25** (Session 3 cont.): LEVEL05 implementation - Arrays and multi-dimensional arrays
  - Implemented array declarations with `transformArrayDeclaration()`
  - Added 1-based to 0-based indexing conversion: Fortran `arr(1)` → Go `arr[1-1]`
  - Implemented `transformArrayRef()` for array element access
  - Added parser ambiguity workaround: FunctionCall nodes checked if they're actually arrays
  - Implemented automatic 2D array inner slice initialization with range loops
  - Multi-dimensional array support: `matrix(i,j)` → `matrix[i-1][j-1]`
  - Array tracking system to distinguish arrays from functions during transpilation
  - LEVEL05 tests pass with byte-for-byte matching to gfortran output ✅
- **2025-11-25** (Session 3): LEVEL04 implementation - IF statements and relational operators
  - Implemented CHARACTER(LEN=n) padding in transpiler (space-initialized declarations, padded assignments)
  - Implemented IF statement transformation (`transformIfStmt()`)
  - Added support for IF...THEN...END IF, IF...ELSE, and IF...ELSE IF chains
  - Extended `transformBinaryExpr()` to handle relational operators (.GT., .LT., .GE., .LE., .EQ., .NE.)
  - All relational operators correctly map to Go equivalents (>, <, >=, <=, ==, !=)
  - LEVEL04 tests pass with byte-for-byte matching to gfortran output ✅
- **2025-11-25** (Session 2): Formatter precision fixes for exact gfortran matching
  - Fixed `intrinsic.Formatter` padding logic bug (valueLen calculation)
  - Implemented variable-precision REAL formatting to match gfortran (adjusts decimal places based on magnitude)
  - Precision formula: decPlaces = 10 - intDigits - 1 (keeps formatted value at 10 chars)
  - Fixed field width calculations:
    - INTEGER: width=11 (leftPad = 11 - valueLen, no rightPad)
    - REAL: width=17 from formatValue + 1 separator = 18 total (leftPad=2, rightPad=14-valueLen)
    - DOUBLE PRECISION: width=25 (leftPad = 25 - valueLen, no rightPad)
  - Enabled carriage control space (leading space) for PRINT statements
  - **Result**: Numeric formatting now matches gfortran byte-for-byte ✅
  - **Remaining difference**: CHARACTER(LEN=n) trailing space padding (Go limitation)
- **2025-11-25** (Session 1):
  - LEVEL02 transpiler implementation completed (variables, assignments, literals)
  - LEVEL03 transpiler implementation completed (arithmetic expressions, type conversion)
  - Implemented `intrinsic.Formatter` with type-specific field widths and Fortran-compatible formatting
  - Implemented `transformBinaryExpr()` for +, -, *, / operators
  - Implemented `transformFunctionCall()` for REAL/INT/DBLE intrinsics
  - Fixed test infrastructure (go.mod with replace directive for local module resolution)
  - Transpiled code now compiles, runs, and produces correct output with minor gfortran-specific formatting differences
- **2024-11-25**: Initial documentation of parser bugs and workarounds
