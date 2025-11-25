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

**LEVEL05-12**: Not yet implemented in transpiler

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
