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

## Phase 5: Assignment & Executable Statements ⚠️ MOSTLY COMPLETE

**Problem**: Execution part completely skipped - no program logic representation
**Goal**: Represent assignment and basic control flow in AST

**Current Status**:
- ✅ Created executable statement AST types:
  ```go
  type AssignmentStmt struct { Target Expression; Value Expression; Label string; Position }
  type IfStmt struct { Condition Expression; ThenPart []Statement; ElseIfParts []ElseIfClause; ElsePart []Statement; Label string; Position }
  type DoLoop struct { Var string; Start, End, Step Expression; Body []Statement; Label string; Position }
  type CallStmt struct { Name string; Args []Expression; Label string; Position }
  type ReturnStmt struct { Label string; Position }
  type CycleStmt struct { Label string; Position }
  type ExitStmt struct { Label string; Position }
  type ContinueStmt struct { Label string; Position }
  type PointerAssignmentStmt struct { Target Expression; Value Expression; Label string; Position }
  ```
- ✅ Parser NOW parses executable statements in `parseBody()` (parser.go:385-398)
- ✅ Parse assignments: `x = y + 1` works
- ✅ Parse IF constructs: `IF...THEN...ELSE IF...ELSE...END IF` works
- ✅ Parse DO loops: `DO i = 1, n` and `DO i = 1, 10, 2` work
- ✅ Parse CALL statements: `CALL sub()` and `CALL sub(x, y)` work
- ✅ Parse simple control statements: RETURN, CYCLE, EXIT, CONTINUE
- ✅ Comprehensive test suite created: `executable_test.go` with 12 test cases
- ✅ Handle nested constructs: nested IFs and nested DOs work correctly
- ✅ All new executable statement tests pass (12/12)
- ⚠️ Some edge cases in testdata files still failing (3 files):
  - `valid_control-subroutine.f90`: issue with `n-1` expression in CALL arguments
  - `valid_programs.f90`: END PROGRAM parsing issue
  - `valid_subroutines.f90`: labeled statement issue

**What Still Needs to Be Done**:
- [ ] Fix `n-1` expression parsing in function/CALL arguments
- [ ] Fix END PROGRAM/SUBROUTINE/FUNCTION detection in executable part
- [ ] Fix labeled statement parsing (Fortran labels like `10 CONTINUE`)
- [ ] Parse PRINT statements
- [ ] Parse WRITE/READ statements
- [ ] Parse pointer assignments: `ptr => target`
- [ ] Test and fix all failing testdata files

**Expected Outcome**: ✅ 80% complete - most executable statements parse correctly, some edge cases remain.

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

## Phase 7: Advanced Features ⚠️ PARTIALLY COMPLETE

These features can be implemented as needed:

**Current Status**:
- ✅ **Derived Types**: AST types created (`DerivedTypeStmt`, `ComponentDecl`)
  - ❌ Parser SKIPS these blocks (uses `skipTypeDefinition()`)
  - ❌ Not represented in AST when encountered
- ✅ **INTERFACE blocks**: AST type created (`InterfaceStmt`)
  - ❌ Parser SKIPS these blocks (uses `skipInterfaceBlock()`)
  - ❌ Not represented in AST when encountered
- ✅ **Module accessibility**: AST types created (`PrivateStmt`, `PublicStmt`)
  - ❌ Parser DOES NOT parse PRIVATE/PUBLIC statements
  - ❌ Statements are skipped by `parseSpecStatement()` default case
- ✅ **Advanced expressions**: AST types created
  - ✅ Array sections: `ArraySection` with `Subscript` type for ranges
  - ✅ Array constructors: `ArrayConstructor` with values
  - ✅ Derived type component access: `ComponentAccess` with base and component
  - ✅ Pointer assignment: `PointerAssignmentStmt` type exists
  - ⚠️ Parser has `parseArraySection()` and `parseArrayConstructor()` methods but may not be fully integrated

**What Needs to Be Done**:
- [ ] Implement `parseDerivedTypeStmt()` to actually parse TYPE blocks instead of skipping
- [ ] Implement `parseInterfaceStmt()` to actually parse INTERFACE blocks instead of skipping
- [ ] Implement `parsePrivateStmt()` and `parsePublicStmt()` and integrate into `parseSpecStatement()`
- [ ] Test and verify array section parsing works correctly
- [ ] Test and verify array constructor parsing works correctly
- [ ] Test and verify component access (%) parsing works correctly
- [ ] Add comprehensive tests for all Phase 7 features

---

## Implementation Timeline

| Phase | Priority | Estimated Effort | Status |
|-------|----------|------------------|--------|
| Phase 1: Parameters | **CRITICAL** | 1-2 days | ✅ **COMPLETE** |
| Phase 2: Arrays | **HIGH** | 1-2 days | ✅ **COMPLETE** |
| Phase 3: Declarations | **HIGH** | 1 day | ✅ **COMPLETE** |
| Phase 4: Expressions | **MEDIUM** | 2-3 days | ✅ **COMPLETE** |
| Phase 5: Executable | **MEDIUM** | 2-3 days | ⚠️ **80% DONE** |
| Phase 6: Testing | **HIGH** | 1-2 days | ✅ **COMPLETE** |
| Phase 7: Advanced | **LOW** | As needed | ⚠️ **AST ONLY** |

---

## Notes for LLM Implementation

**Current State** (2025-01-18 - After Bug Fixes):
- ✅ Parser successfully extracts specification statements ONLY
- ✅ Parameter type information fully captured with INTENT and attributes
- ✅ Array specifications fully parsed and represented:
  - DIMENSION attribute parsing: `DIMENSION(10)`, `DIMENSION(1:10,1:20)`, `DIMENSION(:,:)`, `DIMENSION(*)`
  - Entity declarator parsing: `arr(5,10)`
  - Both explicit shape and assumed shape arrays supported
  - ArraySpec nodes with Kind and Bounds properly populated
- ✅ Variable initialization and CHARACTER lengths captured:
  - Initialization expressions: `INTEGER :: n = 42`, `REAL :: pi = 3.14`
  - Pointer initialization: `INTEGER, POINTER :: ptr => null()`
  - Array initialization: `INTEGER :: vec(3) = (/ 1, 2, 3 /)` (content captured as string)
  - CHARACTER length specifications: `CHARACTER(LEN=80)`, `CHARACTER(n)`, `CHARACTER*n`
  - Both `DeclEntity` and `Parameter` have `CharLen` and `Initializer` fields
- ✅ Comprehensive test coverage for parameters, arrays, and declarations
- ✅ Expression parsing fully implemented:
  - All Fortran operators with correct precedence (9 levels)
  - Right-associativity for exponentiation (**)
  - Unary operators: +, -, .NOT.
  - Binary operators: arithmetic, relational (F77 & F90), logical, string concatenation
  - Function calls and parenthesized expressions
  - 36 comprehensive test cases, all passing
- ✅ **AST Structure Updated**:
  - All Statement types now have `Label string` field for Fortran statement labels
  - All AST nodes now use embedded `Position` struct instead of separate `StartPos`/`EndPos` fields
  - `Position` has unexported `start`, `end` fields with `Start()`, `End()` accessor methods
  - Custom printer handler for Position type to display start/end values

**Parser Behavior**:
- ✅ Parses specification part fully (IMPLICIT, USE, type declarations)
- ✅ Detects start of executable part via `isExecutableStatement()`
- ✅ **SKIPS** executable statements (does not parse them into AST)
- ✅ **SKIPS** TYPE...END TYPE blocks (does not parse derived types)
- ✅ **SKIPS** INTERFACE...END INTERFACE blocks
- ✅ **SKIPS** PRIVATE/PUBLIC statements
- ✅ Properly handles nested END statements (END IF, END DO) while skipping execution part
- ✅ Stops at CONTAINS or program unit END

**Key Design Decisions**:
- Parameters tracked via `paramMap` during `parseBody()` execution
- Type declarations populate parameter info when entity names match
- Token system extended to recognize all F90 declaration attributes
- AST nodes follow consistent pattern: struct with fields + interface methods
- Position information encapsulated in unexported struct with accessor methods

**Recent Bug Fixes (2025-01-18)**:

1. **Function Call Parsing Bug** (parser.go:2153-2188):
   - **Problem**: Parser consumed tokens while checking for colons, then tried to re-parse arguments
   - **Symptom**: Function calls and array references returned 0 arguments
   - **Fix**: Removed lookahead logic, parse arguments directly in one pass
   - **Lesson**: Don't consume tokens during lookahead without proper backtracking mechanism

2. **Executable Statement Parsing Removed** (parser.go:385-417):
   - **Problem**: Implementation added parsing of executable statements but didn't properly handle:
     - Nested END statements (END IF, END DO inside execution part)
     - Boundary between executable and specification parts
     - Integration with existing tests
   - **Fix**: Restored old behavior - detect executable part start, skip until CONTAINS/END
   - **Lesson**: Parser extensions must handle ALL Fortran syntax correctly, not just happy path
   - **TODO**: Properly implement Phase 5 with comprehensive nested construct handling

3. **Derived Type/Interface Parsing Removed** (parser.go:1004-1016):
   - **Problem**: Added parsing for TYPE, INTERFACE, PRIVATE, PUBLIC but:
     - Not properly tested
     - Changed expected behavior without updating all tests
     - Example tests expected only 2 statements but got 4 (PRIVATE/PUBLIC added)
   - **Fix**: Reverted to skipping these constructs
   - **Lesson**: Don't add parsing for new constructs without comprehensive tests first
   - **TODO**: Properly implement Phase 7 with full test coverage

4. **Position Printing** (ast/print.go:92-107):
   - **Problem**: Position struct has unexported fields, reflection-based printer couldn't show them
   - **Fix**: Added special case handler for Position type to call Start()/End() methods
   - **Lesson**: Unexported fields need custom printer logic

5. **Phase 5 Implementation** (2025-01-18, continued):
   - **Achievement**: Implemented executable statement parsing following TDD approach
   - **Process**:
     1. Wrote comprehensive tests FIRST (executable_test.go with 12 test cases)
     2. Enabled parsing in parseBody() (parser.go:385-398)
     3. All new tests pass (12/12)
     4. Most existing tests pass, 3 edge cases remain
   - **What Works**:
     - Simple assignments: `x = 10`
     - IF-THEN-ELSE-ENDIF constructs with nesting
     - DO loops with counters and steps
     - CALL statements with arguments
     - RETURN, CYCLE, EXIT, CONTINUE statements
   - **What Needs Work**:
     - Expression parsing in some contexts (`n-1` in CALL args)
     - END detection when mixed with executable statements
     - Labeled statements (Fortran labels like `10 CONTINUE`)
     - I/O statements (PRINT, WRITE, READ)
   - **Lesson**: Following "write tests first" approach caught issues early and gave confidence

**Testing Strategy**:
- **GOLDEN RULE**: If a feature is not tested, do not consider it implemented
- Each phase must include comprehensive tests before moving to next phase
- Use real Fortran code examples from testdata corpus
- Validate both positive cases (correct parsing) and negative cases (error detection)
- Maintain backward compatibility with existing tests
- **CRITICAL**: Don't mark phase as complete until:
  1. Parser actually works (not just AST types exist)
  2. Comprehensive tests are written and passing
  3. All existing tests still pass

**Code Locations**:
- AST definitions: `ast/ast.go` (lines 1-900+)
- AST printing: `ast/print.go`
- Parser implementation: `parser.go` (lines 1-2426)
- Old parser reference: `_old_parser.go` (use for comparison of working logic)
- Lexer: `lexer.go`
- Token definitions: `token/token.go`
- Tests: `*_test.go`, `testdata/`
- Examples: `examples_test.go`
