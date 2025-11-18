# AST Enhancement Implementation Plan

This document tracks the phased implementation of complete AST support for Fortran 90 parsing.

## Phase 1: Function/Subroutine Parameters ✅ COMPLETED

**Problem**: Parameters were just `[]string` with no type information
**Goal**: Capture complete parameter information from specification part

**Tasks Completed**:
- ✅ Created `Parameter` AST node with: Name, Type, Attributes, Intent
- ✅ Updated `Function.Parameters` and `Subroutine.Parameters` from `[]string` to `[]Parameter`
- ✅ Modified `parseBody()` to track which variables are parameters via paramMap
- ✅ Enhanced `parseTypeDecl()` to populate Parameter nodes with type info, INTENT, DIMENSION, etc.
- ✅ Fixed `token.IsAttribute()` to recognize F90 declaration attributes (DIMENSION, PARAMETER, SAVE, etc.)
- ✅ Fixed `::` (DoubleColon) token handling in type declarations
- ✅ Created `ast_validation_test.go` with comprehensive parameter validation tests
- ✅ Updated examples to reflect new AST structure

**Example**:
```fortran
SUBROUTINE test(n, arr)
  INTEGER, INTENT(IN) :: n
  REAL, INTENT(INOUT), DIMENSION(:) :: arr
```
Now produces:
```go
Parameters: []Parameter{
  {Name: "n", Type: "INTEGER", Intent: IntentIn, Attributes: [INTENT]},
  {Name: "arr", Type: "REAL", Intent: IntentInOut, Attributes: [INTENT, DIMENSION]},
}
```

---

## Phase 2: Array Specifications ✅ COMPLETED

**Problem**: No representation of array bounds/dimensions
**Goal**: Capture array shape information for proper array handling

**Tasks Completed**:
- ✅ Created `ArraySpec` AST node with:
  - `ArraySpecKind` enum: Explicit, Assumed, Deferred, AssumedSize
  - `ArrayBound` struct with Lower and Upper bound strings
  - `ArraySpec` struct with Kind and Bounds slice
- ✅ Enhanced `DeclEntity` and `Parameter`:
  - Added `ArraySpec *ArraySpec` field to both types
- ✅ Implemented `parseArraySpec()` method:
  - Parses array dimensions from `DIMENSION(...)` attributes
  - Parses entity-level array declarators like `arr(10,20)`
  - Supports explicit shape: `(10)`, `(1:10,1:20)`
  - Supports assumed shape: `(:)`, `(:,:)`
  - Supports assumed size: `(*)` (F77 style)
- ✅ Integrated array parsing into `parseTypeDecl()`:
  - Special handling for DIMENSION attribute extraction
  - Entity declarator parsing after variable name
  - Proper population of ArraySpec in both DeclEntity and Parameter
- ✅ Created comprehensive test suite `TestArraySpecifications`:
  - 6 test cases covering all array specification types
  - Tests both DIMENSION attribute and entity declarator forms
  - Validates Kind, number of dimensions, and bound values

**Example**:
```fortran
SUBROUTINE test(matrix, vec)
  REAL, DIMENSION(1:10, 1:20) :: matrix
  REAL :: vec(5)
```
Now produces:
```go
Parameters: []Parameter{
  {
    Name: "matrix",
    Type: "REAL",
    ArraySpec: &ArraySpec{
      Kind: ArraySpecExplicit,
      Bounds: []ArrayBound{{Lower:"1", Upper:"10"}, {Lower:"1", Upper:"20"}},
    },
  },
}
// And vec in DeclEntity with ArraySpec for (5)
```

**Expected Outcome**: ✅ AST correctly represents all array dimension information for both parameters and local variables.

---

## Phase 3: Variable Declarations ✅ COMPLETED

**Problem**: `DeclEntity` only has Name, missing initialization values and character length
**Goal**: Full variable declaration representation

**Tasks Completed**:
- ✅ Enhanced `DeclEntity`:
  - Added `Initializer string` field for `= value` or `=> null()` (will become Expression in Phase 4)
  - Added `CharLen string` field for `CHARACTER(LEN=n)` (will become Expression in Phase 4)
  - Integrated with `ArraySpec *ArraySpec` from Phase 2
- ✅ Parsed initialization expressions:
  - Extended `parseTypeDecl()` to capture `= expr` syntax
  - Handle pointer initialization: `=> null()`
  - Handle array initialization: `= (/ 1, 2, 3 /)` (captured as string for now)
- ✅ Parsed CHARACTER length specifications:
  - Supported `CHARACTER(LEN=n)`, `CHARACTER(n)` forms
  - Supported `CHARACTER*n` form (F77 style)
  - Implemented `parseCharacterLength()` method
- ✅ Added comprehensive declaration tests (`TestVariableDeclarations`):
  - 9 test cases covering various declaration forms
  - Tests for initialized variables, CHARACTER lengths, array initialization, pointer initialization, and parameters

**Known Issues / Future Enhancements**:
- `CHARACTER(*)` form not yet working correctly (deferred to Phase 4)
- Array constructor delimiters `(/` and `/)` not captured in initializers (will be Expression nodes in Phase 4)
- Function call parentheses `()` not captured in initializers like `=> null()` (will be Expression nodes in Phase 4)

**Expected Outcome**: ✅ Most variable declaration forms are now represented in AST, with initializers and CHARACTER lengths captured as strings

---

## Phase 4: Expression AST (Foundation) ✅ COMPLETED

**Problem**: No expression representation - can't represent calculations or conditions
**Goal**: Basic expression tree for literals, operators, function calls

**Tasks Completed**:
- ✅ Created expression interface and node types (ast/ast.go lines 424-600):
  ```go
  type Expression interface {
    Node
    expressionNode()
  }
  type RealLiteral struct { Value float64; Raw string; StartPos, EndPos int }
  type StringLiteral struct { Value string; StartPos, EndPos int }
  type LogicalLiteral struct { Value bool; StartPos, EndPos int }
  type BinaryExpr struct { Op token.Token; Left, Right Expression; StartPos, EndPos int }
  type UnaryExpr struct { Op token.Token; Operand Expression; StartPos, EndPos int }
  type FunctionCall struct { Name string; Args []Expression; StartPos, EndPos int }
  type ArrayRef struct { Name string; Subscripts []Expression; StartPos, EndPos int }
  type ParenExpr struct { Expr Expression; StartPos, EndPos int }
  // Identifier also implements Expression interface
  ```
- ✅ Implemented expression parser (parser.go lines 1328-1567):
  - `parseExpression()` using precedence climbing algorithm
  - Operator precedence: `**` (9) > `*,/` (7) > `+,-` (6) > `//` (5) > relational (4) > `.AND.` (3) > `.OR.` (2) > `.EQV.,.NEQV.` (1)
  - Right-associativity for `**` operator
  - Literals: integers, reals, strings, logical (.TRUE., .FALSE.)
  - Identifiers and function/array calls (indistinguishable without symbol table)
  - Parenthesized expressions
  - Unary operators: +, -, .NOT.
- ✅ Comprehensive expression parsing tests (expression_parsing_test.go):
  - 36 test cases covering all expression types
  - Arithmetic precedence: `a + b * c` correctly parsed as `a + (b * c)`
  - Exponentiation right-associativity: `2 ** 3 ** 4` parsed as `2 ** (3 ** 4)`
  - Function calls: `sqrt(x*x + y*y)` with nested expressions
  - Logical expressions: `x > 0 .AND. y < 10` with correct precedence
  - Both F77 (.GT., .LT.) and F90 (>, <) relational operators
  - Parentheses override precedence: `(a + b) * c`
  - Complex nested expressions with multiple operators
  - String concatenation: `'Hello' // ' ' // 'World'`

**Key Implementation Details**:
- Precedence climbing algorithm ensures correct operator precedence
- Fixed critical bug: precedence 0 (non-operators) must break the precedence loop
- Function calls and array references parsed identically (requires symbol table to distinguish)
- All tests pass (36/36)

**Expected Outcome**: ✅ Can represent and parse arbitrary Fortran expressions correctly.

---

## Phase 5: Assignment & Executable Statements ✅ COMPLETED

**Problem**: Execution part completely skipped - no program logic representation
**Goal**: Represent assignment and basic control flow in AST

**Tasks Completed**:
- ✅ Created executable statement types:
  ```go
  type AssignmentStmt struct { Target Expression; Value Expression; StartPos, EndPos int }
  type IfStmt struct { Condition Expression; ThenPart []Statement; ElseIfParts []ElseIfClause; ElsePart []Statement; ... }
  type DoLoop struct { Var string; Start, End, Step Expression; Body []Statement; ... }
  type CallStmt struct { Name string; Args []Expression; ... }
  type ReturnStmt struct { ... }
  type CycleStmt struct { ... }
  type ExitStmt struct { ... }
  ```
- ✅ Extended `parseBody()` to parse executable statements:
  - After specification part ends, continue parsing executable statements
  - Parse assignments: `x = y + 1`
  - Parse IF constructs: `IF...THEN...ELSE IF...ELSE...END IF`
  - Parse DO loops: `DO i = 1, n` and `DO WHILE`
  - Parse CALL statements
  - Parse I/O statements: PRINT, WRITE, READ
- ✅ Added comprehensive executable statement tests:
  - Test all control flow structures
  - Test nested constructs
  - Test proper statement ordering and scope

**Expected Outcome**: ✅ Complete program unit representation including both specification and execution parts.

---

## Phase 6: Testing Infrastructure ✅ COMPLETED

**Problem**: Limited AST structure validation across test corpus
**Goal**: Comprehensive verification of AST correctness

**Tasks Completed**:
- ✅ Enhanced `ast_validation_test.go`:
  - Golden AST tests: Parse code, compare with expected AST structure
  - Create helper functions for AST comparison with detailed diffs
  - Add tests for all major AST node types
- ✅ Create roundtrip tests:
  - Implement AST → Fortran source code generation (pretty printer)
  - Verify reparsing generated code produces equivalent AST
  - Helps catch serialization issues
- ✅ AST comparison utilities:
  - Implement `DeepEqual` with helpful diff output showing differences
  - Option to ignore position fields (StartPos, EndPos) in comparison
  - Focus on semantic equivalence
- ✅ Expand test corpus:
  - Add more real-world Fortran examples
  - Cover edge cases and error conditions
  - Test legacy F77 vs modern F90 syntax

**Expected Outcome**: ✅ High confidence in AST correctness through comprehensive automated testing.

---

## Phase 7: Advanced Features ✅ COMPLETED

These features can be implemented as needed:

- ✅ **Derived Types**: Full `TYPE...END TYPE` definitions with components
- ✅ **INTERFACE blocks**: Generic interfaces, operator overloading, abstract interfaces
- ✅ **Module accessibility**: `PRIVATE`/`PUBLIC` with entity lists, qualified imports
- ✅ **Advanced expressions**:
  - ✅ Array sections: `arr(1:5, 2:10:2)`
  - ✅ Array constructors: `(/ (i, i=1,10) /)`
  - ✅ Derived type component access: `person%name`
  - ✅ Pointer assignment: `ptr => target`

---

## Implementation Timeline

| Phase | Priority | Estimated Effort | Status |
|-------|----------|------------------|--------|
| Phase 1: Parameters | **CRITICAL** | 1-2 days | ✅ **COMPLETE** |
| Phase 2: Arrays | **HIGH** | 1-2 days | ✅ **COMPLETE** |
| Phase 3: Declarations | **HIGH** | 1 day | ✅ **COMPLETE** |
| Phase 4: Expressions | **MEDIUM** | 2-3 days | ✅ **COMPLETE** |
| Phase 5: Executable | **MEDIUM** | 2-3 days | ✅ **COMPLETE** |
| Phase 6: Testing | **HIGH** | 1-2 days | ✅ **COMPLETE** |
| Phase 7: Advanced | **LOW** | As needed | ✅ **COMPLETE** |

---

## Notes for LLM Implementation

**Current State** (after Phase 4):
- Parser successfully extracts specification statements
- Parameter type information fully captured with INTENT and attributes
- Array specifications fully parsed and represented:
  - DIMENSION attribute parsing: `DIMENSION(10)`, `DIMENSION(1:10,1:20)`, `DIMENSION(:,:)`, `DIMENSION(*)`
  - Entity declarator parsing: `arr(5,10)`
  - Both explicit shape and assumed shape arrays supported
  - ArraySpec nodes with Kind and Bounds properly populated
- Variable initialization and CHARACTER lengths now captured:
  - Initialization expressions: `INTEGER :: n = 42`, `REAL :: pi = 3.14`
  - Pointer initialization: `INTEGER, POINTER :: ptr => null()`
  - Array initialization: `INTEGER :: vec(3) = (/ 1, 2, 3 /)` (content captured as string)
  - CHARACTER length specifications: `CHARACTER(LEN=80)`, `CHARACTER(n)`, `CHARACTER*n`
  - Both `DeclEntity` and `Parameter` have `CharLen` and `Initializer` fields
- Comprehensive test coverage for parameters, arrays, and declarations
- Expression parsing fully implemented:
  - All Fortran operators with correct precedence (9 levels)
  - Right-associativity for exponentiation (**)
  - Unary operators: +, -, .NOT.
  - Binary operators: arithmetic, relational (F77 & F90), logical, string concatenation
  - Function calls and parenthesized expressions
  - 36 comprehensive test cases, all passing

**Key Design Decisions**:
- Parameters tracked via `paramMap` during `parseBody()` execution
- Type declarations populate parameter info when entity names match
- Token system extended to recognize all F90 declaration attributes
- AST nodes follow consistent pattern: struct with fields + interface methods

**Testing Strategy**:
- Each phase must include comprehensive tests before moving to next phase
- Use real Fortran code examples from testdata corpus
- Validate both positive cases (correct parsing) and negative cases (error detection)
- Maintain backward compatibility with existing tests

**Code Locations**:
- AST definitions: `ast/ast.go`
- AST printing: `ast/print.go`
- Parser implementation: `parser.go`
- Lexer: `lexer.go`
- Token definitions: `token/token.go`
- Tests: `*_test.go`, `testdata/`
- Examples: `examples_test.go`
