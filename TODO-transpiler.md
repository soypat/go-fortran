# Fortran-to-Go Transpiler Development Plan

This document contains the comprehensive research findings and implementation plan for building a Fortran-to-Go transpiler based on the go-fortran parser.

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Current Parser State Analysis](#current-parser-state-analysis)
3. [Missing Type Information](#missing-type-information)
4. [High-Level Transpiler Architecture](#high-level-transpiler-architecture)
5. [Deep Dive: Symbol Table & Type Resolution](#deep-dive-symbol-table--type-resolution)
6. [Required Parser Enhancements](#required-parser-enhancements)
7. [Implementation Roadmap](#implementation-roadmap)
8. [Fortran-to-Go Mapping Challenges](#fortran-to-go-mapping-challenges)
9. [Success Criteria](#success-criteria)

---

## Executive Summary

### Goal
Build a Fortran 77/90 to Go transpiler that correctly handles type resolution, semantic analysis, and code generation.

### Current State
- ✅ **Parser**: Comprehensive AST for most Fortran constructs
- ✅ **Type Declarations**: Fully captured with attributes, arrays, intent
- ✅ **Expressions**: Complete with proper precedence
- ❌ **Symbol Table**: Does not exist
- ❌ **Type Resolution**: Not implemented
- ❌ **Semantic Analysis**: Not implemented

### Required Work
1. ✅ **Parser Enhancements** (COMPLETED 2025-11-24): Added KIND parameters, COMMON blocks, EXTERNAL/INTRINSIC, complete IMPLICIT parsing
2. **Symbol Table** (~1 week): Design and implement symbol table with scope management
3. **Type Resolution** (~1-2 weeks): Implement IMPLICIT rules, type inference, function/array disambiguation
4. **Semantic Validation** (~1 week): Type checking, array conformance, INTENT validation
5. **Code Generation** (~3-4 weeks): Fortran AST → Go code transformation

---

## Current Parser State Analysis

### What Type Information IS Currently Captured

#### 1. Basic Type Declarations (WORKING) ✅

**Location**: `ast/ast.go:438-473`, parser: `parser.go:2525` (`parseTypeDecl`)

**AST Structure**:
```go
type TypeDeclaration struct {
    TypeSpec   string        // "INTEGER", "REAL", "CHARACTER", etc.
    Attributes []token.Token // PARAMETER, SAVE, INTENT, DIMENSION, etc.
    Entities   []DeclEntity  // Variables being declared
    Label      string
    Position
}

type DeclEntity struct {
    Name        string     // Variable name
    ArraySpec   *ArraySpec // Array dimensions (nil if not an array)
    Initializer string     // "= expr" or "=> null()"
    CharLen     string     // CHARACTER length (will become Expression later)
}
```

**Test Evidence**: `ast_validation_test.go:162-434`
- INTEGER, REAL, LOGICAL declarations tested
- CHARACTER length: `CHARACTER(LEN=80)`, `CHARACTER(80)`, `CHARACTER*80` all work
- DOUBLE PRECISION handled (normalized to "DOUBLE PRECISION")
- Initializers: `INTEGER :: n = 42`, `REAL :: pi = 3.14159`
- Pointer initialization: `INTEGER, POINTER :: ptr => null()`

**Examples from tests**:
```fortran
INTEGER :: i, j, k
REAL, DIMENSION(10) :: arr
CHARACTER(LEN=80) :: name
DOUBLE PRECISION :: x
INTEGER :: count = 0
```

#### 2. Array Specifications (WORKING) ✅

**Location**: `ast/ast.go:504-513`, parser: `parser.go:2964` (`parseArraySpec`)

**AST Structure**:
```go
type ArraySpec struct {
    Kind   ArraySpecKind  // Explicit, Assumed, Deferred, AssumedSize
    Bounds []ArrayBound   // One per dimension
}

type ArrayBound struct {
    Lower Expression // Lower bound (nil if omitted or assumed)
    Upper Expression // Upper bound (nil if assumed, Identifier("*") for assumed size)
}

const (
    ArraySpecExplicit    // (1:10, 1:20) - explicit bounds
    ArraySpecAssumed     // (:, :) - assumed shape (F90)
    ArraySpecDeferred    // (:) - with ALLOCATABLE/POINTER
    ArraySpecAssumedSize // (*) - F77 assumed size
)
```

**Test Evidence**: `ast_validation_test.go:541-789` (6 comprehensive tests)

**Key Feature**: Array bounds are stored as **Expression nodes**, not strings! This means:
- Can represent: `DIMENSION(1:n)` where n is a variable
- Can represent: `DIMENSION(i+1:j*2)` with complex expressions
- Proper semantic analysis possible

**Examples**:
```fortran
DIMENSION(10)              ! Lower=1 (implicit), Upper=10
DIMENSION(1:10, 1:20)      ! Explicit both bounds
DIMENSION(:,:)             ! Assumed shape (F90 dummy args)
DIMENSION(*)               ! Assumed size (F77)
REAL :: matrix(n, n)       ! Bounds are expressions
```

#### 3. Function/Subroutine Parameters (WORKING) ✅

**Location**: `ast/ast.go:557-564`

**AST Structure**:
```go
type Parameter struct {
    Name       string        // Parameter name
    Type       string        // "INTEGER", "REAL", etc.
    Intent     IntentType    // INTENT(IN/OUT/INOUT)
    Attributes []token.Token // OPTIONAL, POINTER, TARGET, etc.
    ArraySpec  *ArraySpec    // Array dimensions for array parameters
    CharLen    string        // CHARACTER length (will become Expression)
}

type IntentType int
const (
    intentDefault IntentType = iota
    IntentInOut
    IntentIn
    IntentOut
)
```

**Test Evidence**: `ast_validation_test.go:13-160`

**What's Captured**:
- Parameter types from type declarations in procedure body
- INTENT attributes properly parsed and stored
- Array parameters with DIMENSION
- Mixed parameter types (INTEGER, REAL, LOGICAL)

**Parser Mechanism** (`parser.go:2720-2732`):
- Parser maintains `paramMap` during function/subroutine parsing
- When type declarations are parsed, updates parameters in paramMap
- Parameters get complete type information

**Example**:
```fortran
SUBROUTINE process(n, arr, flag)
    INTEGER, INTENT(IN) :: n
    REAL, DIMENSION(n), INTENT(INOUT) :: arr
    LOGICAL, INTENT(OUT) :: flag
END SUBROUTINE
```
→ All three parameters get full type info in Parameter structs

#### 4. Variable Attributes (WORKING) ✅

**Recognized Attributes** (`token/token.go:113-132`):
- INTENT, IN, OUT, INOUT
- OPTIONAL, POINTER, TARGET
- ALLOCATABLE, RECURSIVE, ELEMENTAL, PURE
- PARAMETER, SAVE, DIMENSION
- KIND, LEN, RESULT

**Parser Support** (`parser.go:2571-2642`):
- Parses comma-separated attributes in type declarations
- Stores as `[]token.Token` in TypeDeclaration and Parameter
- Special handling for INTENT extraction (lines 2581-2615)
- Special handling for DIMENSION (lines 2617-2624)

**Example**:
```fortran
REAL, SAVE, ALLOCATABLE :: global_array(:,:)
INTEGER, PARAMETER :: MAX = 1000
```

#### 5. IMPLICIT NONE Statement (WORKING) ✅

**Location**: `ast/ast.go:341-360`, parser: `parser.go:2473` (`parseImplicit`)

**AST Structure**:
```go
type ImplicitStatement struct {
    IsNone bool   // true for IMPLICIT NONE
    Label  string
    Position
}
```

**What Works**:
- `IMPLICIT NONE` is recognized and parsed
- Parser validates no duplicate IMPLICIT statements
- Parser validates IMPLICIT appears before type declarations

**What's Missing**:
- Cannot parse `IMPLICIT REAL (A-H, O-Z)`
- Cannot parse `IMPLICIT INTEGER (I-N)`
- No representation of implicit typing rules

**Test Evidence**: `testdata/valid_specification.f90:10`

#### 6. Expression System (COMPREHENSIVE) ✅

The parser has a complete expression system with proper precedence climbing:

**Literal Types**:
- IntegerLiteral (Value: int64, Raw: string)
- RealLiteral (Value: float64, Raw: string)
- StringLiteral (Value: string)
- LogicalLiteral (Value: bool) - .TRUE., .FALSE.

**Operators** (9 precedence levels with correct associativity):
- Arithmetic: +, -, *, /, ** (power)
- String: // (concatenation)
- Relational: .EQ., .NE., .LT., .LE., .GT., .GE., ==, /=, <, <=, >, >=
- Logical: .AND., .OR., .NOT., .EQV., .NEQV.

**Complex Expressions**:
- ArrayRef: `a(i, j, k)` with subscripts as Expressions
- ArraySection: `a(1:10:2, :)` with range specifications
- FunctionCall: `sin(x)` with arguments as Expressions
- ComponentAccess: `person%name` for derived types
- ImpliedDoLoop: `(a(i), i=1,n)` in array constructors
- ArrayConstructor: `(/ 1, 2, 3 /)` inline arrays

**Test Evidence**: `parsing_expr_test.go` - comprehensive expression tests

---

## Missing Type Information

### Critical Gaps That Block Type Resolution

#### 1. KIND Parameters (IMPLEMENTED) ✅

**Status**: Fully implemented as of 2025-11-24

**Problem**: KIND specifications were completely missing from AST

**Fortran 77/90 Specification**:
- F77: Type size with `*N` suffix: `INTEGER*4`, `REAL*8`, `COMPLEX*16`
- F90: Kind selector: `INTEGER(KIND=8)`, `REAL(KIND=dp)`, `REAL(8)`
- Kind inquiry: `KIND(variable)` intrinsic function

**Examples from testdata**:
```fortran
! From valid_gdyn.f90:84
POSPRT = REAL(NPERT, KIND=KIND(POSPRT))

! From valid_functions.f90:30-31
REAL*8 FUNCTION dbl(x)
    REAL*8 :: x
```

**What's Missing**:
1. TypeDeclaration has no field for KIND parameter
2. Function.ResultType is just a string, no KIND info
3. Parser skips KIND= in type declarations
4. Parser may skip `*8` suffix in F77 syntax

**Implementation Completed**:
```go
type TypeDeclaration struct {
    TypeSpec   string
    KindParam  Expression  // ✅ DONE: for KIND=8 or *8
    CharLen    Expression  // ✅ DONE: Changed from string to Expression
    Attributes []token.Token
    Entities   []DeclEntity
    Label      string
    Position
}

type Function struct {
    Name         string
    ResultType   string
    ResultKind   Expression // ✅ DONE: KIND parameter for result
    // ... rest unchanged
}

type Parameter struct {
    Name       string
    Type       string
    TypeKind   Expression // ✅ DONE: KIND parameter for parameters
    // ... rest unchanged
}
```

**Parser Changes Completed**:
- ✅ `parser.go:2540` (parseTypeDecl): Parses KIND= or `*N` after type keyword
- ✅ `parser.go:3630` (parseTypePrefixedConstruct): Parses function result type with KIND
- ✅ `parser.go:3078` (parseKindSelector): New function for parsing KIND specifications
- ✅ `parser.go:3802` (parseInt64): Helper for parsing integer literals with kind suffixes
- ✅ Comprehensive tests in `ast_validation_test.go:944-1230`

**Tests**: 13 test cases covering INTEGER, REAL, CHARACTER with various KIND syntaxes

#### 2. COMMON Blocks (IMPLEMENTED) ✅

**Status**: Fully implemented as of 2025-11-24

**Problem**: No AST node for COMMON blocks, statements were completely skipped

**Fortran 77 Specification**: COMMON blocks for shared memory between program units

**Examples from testdata** (`valid_specification.f90:43-44`):
```fortran
COMMON /block1/ a, b, c
COMMON /block2/ x1, y1, z1
COMMON // blank_common    ! Blank COMMON (no name)
```

**Implementation Completed**:
```go
// ast/ast.go:403-445 - New AST node
type CommonStmt struct {
    BlockName string   // Empty string for blank COMMON
    Variables []string // Variable names in this block
    Label     string
    Position
}

func (c *CommonStmt) statementNode() {}
func (c *CommonStmt) GetLabel() string { return c.Label }
func (c *CommonStmt) AppendString(dst []byte) []byte { ... }
```

**Parser Changes Completed**:
- ✅ `parser.go:3823` (parseCommonStmt): Parses COMMON statements
- ✅ `parser.go:2446`: Called from `parseSpecStatement()` when token.COMMON encountered
- ✅ Handles named COMMON: `COMMON /BLOCK1/ A, B, C`
- ✅ Handles blank COMMON with slashes: `COMMON // X, Y`
- ✅ Handles blank COMMON without slashes: `COMMON Z`
- ✅ Handles arrays with dimensions: `COMMON /ARRAYS/ ARR(10, 20)`
- ✅ Fixed tokenization issue where `//` is StringConcat token

**Tests**: 5 test cases in `ast_validation_test.go:1285-1405` covering all COMMON syntax variations

#### 3. EQUIVALENCE Statements (NOT PARSED) ❌

**Problem**: No AST node, statements skipped

**Fortran 77 Specification**: Storage association - forces variables to share memory

**Examples from testdata** (`valid_specification.f90:47`):
```fortran
EQUIVALENCE (i, j)           ! i and j share same memory
EQUIVALENCE (a(1), b(5))     ! Overlay arrays at different offsets
```

**What's Missing**:
- No `EquivalenceStmt` AST node
- Token exists (`token.EQUIVALENCE`) but parser ignores it
- Cannot detect storage aliases

**Required Changes**:
```go
type EquivalenceStmt struct {
    Sets  [][]Expression  // Each set is a list of equivalent variables
    Label string
    Position
}
```

**Parser Changes**:
- Add `parseEquivalenceStmt()` function
- Parse parenthesized groups of variables
- Handle array element specifications

**Impact**:
- Cannot correctly transpile F77 code using EQUIVALENCE
- Very difficult construct to map to Go (no equivalent)
- May need to refuse transpilation if EQUIVALENCE detected

#### 4. EXTERNAL and INTRINSIC Declarations (IMPLEMENTED) ✅

**Status**: Fully implemented as of 2025-11-24

**Problem**: No AST nodes, declarations were skipped

**Fortran 77/90 Specification**: Declare whether names are external procedures or intrinsic functions

**Examples from testdata** (`valid_specification.f90:81-82`):
```fortran
EXTERNAL external_func       ! User-defined external function
INTRINSIC sin, cos, sqrt     ! These are intrinsic, not user-defined
```

**Why Critical**:
- Without this, cannot distinguish: `CALL SIN(x)` - is SIN a subroutine or the intrinsic?
- Cannot determine what's user-defined vs built-in
- Required for correct function resolution

**Implementation Completed**:
```go
// ast/ast.go:447-511
type ExternalStmt struct {
    Names []string  // Procedure names declared EXTERNAL
    Label string
    Position
}

type IntrinsicStmt struct {
    Names []string  // Function names declared INTRINSIC
    Label string
    Position
}
```

**Parser Changes Completed**:
- ✅ `parser.go:3891` (parseExternalStmt): Parses EXTERNAL statements
- ✅ `parser.go:3916` (parseIntrinsicStmt): Parses INTRINSIC statements
- ✅ `parser.go:2448-2450`: Called from `parseSpecStatement()`
- ✅ Handles comma-separated lists of names
- ✅ Allows keywords like DATA as block names in COMMON (fixed CanBeUsedAsIdentifier check)

**Tests**: 5 test cases in `ast_validation_test.go:1407-1525` covering single and multiple names

#### 5. IMPLICIT Type Rules (IMPLEMENTED) ✅

**Status**: Fully implemented as of 2025-11-24

**Problem**: Only `IMPLICIT NONE` was parsed, not type range rules

**Fortran 77/90 Specification**:
- Default implicit rules (if no IMPLICIT statement):
  - Variables I-N → INTEGER
  - Variables A-H, O-Z → REAL
- Custom rules: `IMPLICIT REAL (A-H, O-Z)`, `IMPLICIT INTEGER (I-N)`
- Letter ranges: `IMPLICIT REAL (A-C, X-Z)`

**Implementation Completed**:
```go
// ast/ast.go:332-410
type LetterRange struct {
    Start byte  // 'A' to 'Z'
    End   byte  // 'A' to 'Z', Start <= End
}

type ImplicitRule struct {
    Type         string        // "INTEGER", "REAL", etc.
    Kind         Expression    // Optional KIND parameter
    CharLen      Expression    // Optional CHARACTER length
    LetterRanges []LetterRange // Letter ranges this applies to
}

type ImplicitStatement struct {
    IsNone bool            // true for IMPLICIT NONE
    Rules  []ImplicitRule  // ✅ DONE: custom type rules
    Label  string
    Position
}
```

**Parser Changes Completed** (`parser.go:2445-2580`):
- ✅ Extended `parseImplicit()` to parse type specifications
- ✅ Parses letter ranges: `(A-H, O-Z)` or `(I, J, K-N)` or `(I, J, K)`
- ✅ Parses optional KIND: `IMPLICIT REAL*8 (A-H)` or `IMPLICIT REAL(KIND=8) (A-H)`
- ✅ Handles multiple type specs: `IMPLICIT REAL (A-H), INTEGER (I-N)`
- ✅ Validates letter ranges (A-Z only, start <= end)
- ✅ Fixed ambiguity between KIND parens and letter range parens

**Tests**: 6 test cases in `ast_validation_test.go:1527-1736` covering all IMPLICIT syntax variations

**Note**: In IMPLICIT statements, KIND must use explicit `KIND=` form or `*` syntax to avoid ambiguity with letter ranges

#### 6. NAMELIST (NOT PARSED) ❌

**Problem**: No AST node for NAMELIST groups

**Fortran 90 Specification**: Named groups of variables for formatted I/O

**Examples from testdata** (`valid_specification.f90:85-86`):
```fortran
NAMELIST /input_data/ i, j, x, y
NAMELIST /output_data/ matrix, vec
```

**Required Changes**:
```go
type NamelistStmt struct {
    GroupName string   // Name of the namelist group
    Variables []string // Variable names in the group
    Label     string
    Position
}
```

**Impact**: Lower priority, mainly affects I/O operations

#### 7. Derived Type Definitions (SKIPPED) ❌

**Problem**: Parser actively skips TYPE...END TYPE blocks

**Fortran 90 Specification**: User-defined composite types (like structs)

**Example from testdata** (`valid_specification.f90:54-67`):
```fortran
TYPE person
    CHARACTER(LEN=50) :: name
    INTEGER :: age
    REAL :: height
END TYPE person

TYPE(person) :: employee
```

**Current State**:
- AST nodes exist: `DerivedTypeStmt`, `ComponentDecl`
- Parser has `skipTypeDefinition()` that skips entire block
- Not parsed into AST

**Required Changes**:
- Remove `skipTypeDefinition()` calls
- Implement full TYPE block parsing
- Parse component declarations

**Impact**:
- Needed for modern Fortran 90+ code
- Maps reasonably well to Go structs
- Medium priority (less common in F77)

#### 8. INTERFACE Blocks (SKIPPED) ❌

**Problem**: Parser skips INTERFACE...END INTERFACE blocks

**Fortran 90 Specification**: Generic interfaces, operator overloading

**Example from testdata** (`valid_specification.f90:70-78`):
```fortran
INTERFACE operator(+)
    MODULE PROCEDURE add_ints, add_reals
END INTERFACE
```

**Current State**:
- AST node exists: `InterfaceStmt`
- Parser skips these blocks

**Impact**: Needed for modern Fortran, lower priority for initial transpiler

#### 9. PRIVATE/PUBLIC Accessibility (SKIPPED) ❌

**Problem**: Module visibility control statements not parsed

**Fortran 90 Specification**: Control symbol visibility in modules

**Example from testdata** (`valid_specification.f90:114-115`):
```fortran
PRIVATE
PUBLIC :: public_var, public_func
```

**Current State**:
- AST nodes exist: `PrivateStmt`, `PublicStmt`
- Parser skips them

**Impact**: Needed for MODULE support, maps to Go capitalization convention

---

## High-Level Transpiler Architecture

### Five Phase Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                     Fortran Source Code                              │
└────────────────────────────┬────────────────────────────────────────┘
                             │
                             ▼
┌────────────────────────────────────────────────────────────────────┐
│  Phase 1: PARSER ENHANCEMENTS                                      │
│  - Add KIND parameter support                                      │
│  - Parse COMMON, EQUIVALENCE, EXTERNAL, INTRINSIC                  │
│  - Extend IMPLICIT statement parsing                               │
│  - Enable TYPE and INTERFACE parsing                               │
│                                                                     │
│  Output: Enhanced Fortran AST                                      │
└────────────────────────────┬───────────────────────────────────────┘
                             │
                             ▼
┌────────────────────────────────────────────────────────────────────┐
│  Phase 2: SYMBOL TABLE CONSTRUCTION                                │
│  - Build scope hierarchy (global → module → procedure → block)     │
│  - Collect all declarations                                        │
│  - Build COMMON block registry                                     │
│  - Process MODULE and USE statements                               │
│  - Mark EXTERNAL/INTRINSIC procedures                              │
│                                                                     │
│  Output: Symbol Table with Scopes                                  │
└────────────────────────────┬───────────────────────────────────────┘
                             │
                             ▼
┌────────────────────────────────────────────────────────────────────┐
│  Phase 3: TYPE RESOLUTION                                          │
│  - Implement IMPLICIT rule engine (default F77/F90 + custom)       │
│  - Resolve types of all identifiers                                │
│  - Distinguish functions from arrays                               │
│  - Build intrinsic function database                               │
│  - Compute expression types                                        │
│                                                                     │
│  Output: Fully Typed AST + Symbol Table                            │
└────────────────────────────┬───────────────────────────────────────┘
                             │
                             ▼
┌────────────────────────────────────────────────────────────────────┐
│  Phase 4: SEMANTIC VALIDATION                                      │
│  - Type compatibility checking                                     │
│  - Array conformance validation                                    │
│  - INTENT enforcement (IN not modified, OUT assigned)              │
│  - Control flow analysis                                           │
│  - Detect dead code, uninitialized variables                       │
│                                                                     │
│  Output: Validated AST + Error/Warning Reports                     │
└────────────────────────────┬───────────────────────────────────────┘
                             │
                             ▼
┌────────────────────────────────────────────────────────────────────┐
│  Phase 5: GO CODE GENERATION                                       │
│  - Map Fortran constructs to Go equivalents                        │
│  - Generate COMMON block wrappers                                  │
│  - Translate array indexing (1-based → 0-based)                    │
│  - Generate helper functions for FORMAT, I/O                       │
│  - Output Go AST or Go source code                                 │
│                                                                     │
│  Output: Go Source Code                                            │
└────────────────────────────────────────────────────────────────────┘
```

### Phase Dependencies

- **Phase 1** is prerequisite for all others (must have complete AST)
- **Phase 2** depends on Phase 1 (need enhanced AST to build symbols)
- **Phase 3** depends on Phases 1 & 2 (need symbols to resolve types)
- **Phase 4** depends on Phases 1-3 (need types to validate semantics)
- **Phase 5** depends on Phases 1-4 (need validated, typed AST to generate code)

### Estimated Effort

| Phase | Estimated Time | Complexity |
|-------|---------------|------------|
| Phase 1: Parser Enhancements | 1-2 weeks | Medium (AST changes + parser logic) |
| Phase 2: Symbol Table | 1 week | Medium (data structures + traversal) |
| Phase 3: Type Resolution | 1-2 weeks | High (complex semantics) |
| Phase 4: Semantic Validation | 1 week | Medium (rule checking) |
| Phase 5: Code Generation | 3-4 weeks | Very High (construct mapping) |
| **Total** | **7-10 weeks** | |

---

## Deep Dive: Symbol Table & Type Resolution

This section provides detailed designs for the core semantic analysis infrastructure.

### Symbol Table Architecture

#### Core Data Structures

**File**: `symbol/symbol.go` (new package)

```go
package symbol

import (
    "github.com/soypat/go-fortran/ast"
    "github.com/soypat/go-fortran/token"
)

// Symbol represents a declared entity (variable, function, type, etc.)
type Symbol struct {
    Name       string         // Symbol name (case-insensitive in Fortran)
    Type       *ResolvedType  // Fully resolved type information
    Kind       SymbolKind     // What kind of symbol this is
    Attributes []token.Token  // SAVE, POINTER, TARGET, etc.
    ArraySpec  *ast.ArraySpec // Array dimensions (nil if not an array)
    DeclNode   ast.Node       // Reference to declaration AST node
    Scope      *Scope         // Scope where this symbol is defined
    IsImplicit bool           // Type from implicit rules?
    IsUsed     bool           // Referenced in code?
}

// ResolvedType represents a fully resolved Fortran type
type ResolvedType struct {
    BaseType   string  // "INTEGER", "REAL", "LOGICAL", "CHARACTER", "TYPE"
    Kind       int     // Kind parameter value (0 = default kind)
    CharLen    int     // CHARACTER length (-1 = assumed, -2 = deferred)
    TypeName   string  // For derived types: TYPE(typename)
    IsPointer  bool    // POINTER attribute?
    IsTarget   bool    // TARGET attribute?
}

// SymbolKind classifies what kind of entity a symbol represents
type SymbolKind int

const (
    SymUnknown SymbolKind = iota
    SymVariable      // Regular variable
    SymParameter     // Compile-time constant (PARAMETER attribute)
    SymFunction      // Function (returns value)
    SymSubroutine    // Subroutine (no return value)
    SymModule        // Module
    SymProgram       // Main program
    SymCommonBlock   // COMMON block
    SymDerivedType   // User-defined TYPE
    SymIntrinsic     // Intrinsic function
    SymExternal      // External procedure
)

// Scope represents a lexical scope with symbol table
type Scope struct {
    Parent       *Scope              // Parent scope (nil for global)
    Children     []*Scope            // Nested scopes
    Symbols      map[string]*Symbol  // Symbol table (case-insensitive keys)
    Implicit     *ImplicitRules      // Implicit typing rules for this scope
    ProgramUnit  ast.ProgramUnit     // PROGRAM/SUBROUTINE/FUNCTION/MODULE
    ScopeType    ScopeType           // Global, Program, Procedure, Block
}

type ScopeType int

const (
    ScopeGlobal ScopeType = iota  // Global scope (entire file)
    ScopeProgram                   // PROGRAM unit
    ScopeProcedure                 // SUBROUTINE or FUNCTION
    ScopeModule                    // MODULE
    ScopeBlock                     // Block (IF, DO, etc.) - F90+
)

// ImplicitRules stores implicit typing rules for a scope
type ImplicitRules struct {
    IsNone      bool        // IMPLICIT NONE specified?
    LetterTypes [26]string  // Type for each letter A-Z (empty = no rule)
    LetterKinds [26]int     // Kind for each letter (0 = default)
}

// SymbolTable is the root of the symbol table hierarchy
type SymbolTable struct {
    GlobalScope   *Scope                   // Global scope
    CurrentScope  *Scope                   // Current scope during analysis
    CommonBlocks  map[string]*CommonBlock  // COMMON block registry
    Modules       map[string]*ModuleInfo   // Module registry
    Intrinsics    map[string]*Intrinsic    // Intrinsic function database
}

// CommonBlock represents a COMMON block shared between program units
type CommonBlock struct {
    Name      string    // Empty for blank COMMON
    Variables []string  // Variable names in order
    Sizes     []int     // Byte sizes (for storage sequence)
    TotalSize int       // Total bytes
}

// ModuleInfo represents a MODULE with its exported symbols
type ModuleInfo struct {
    Name          string
    PublicSymbols map[string]*Symbol  // PUBLIC symbols
    PrivateSymbols map[string]*Symbol // PRIVATE symbols
    Scope         *Scope
}

// Intrinsic represents an intrinsic function or subroutine
type Intrinsic struct {
    Name       string
    Kind       IntrinsicKind
    Signature  string  // Human-readable signature
    ReturnType string  // For functions
    GoMapping  string  // Go equivalent (if exists)
}

type IntrinsicKind int

const (
    IntrinsicFunction IntrinsicKind = iota
    IntrinsicSubroutine
)
```

#### Symbol Table Operations

```go
// NewSymbolTable creates a new symbol table with global scope
func NewSymbolTable() *SymbolTable {
    st := &SymbolTable{
        CommonBlocks: make(map[string]*CommonBlock),
        Modules:      make(map[string]*ModuleInfo),
        Intrinsics:   loadIntrinsics(),
    }
    st.GlobalScope = &Scope{
        Symbols:   make(map[string]*Symbol),
        Implicit:  defaultImplicitRules(),
        ScopeType: ScopeGlobal,
    }
    st.CurrentScope = st.GlobalScope
    return st
}

// EnterScope creates a new scope as child of current scope
func (st *SymbolTable) EnterScope(unit ast.ProgramUnit, scopeType ScopeType) *Scope {
    newScope := &Scope{
        Parent:      st.CurrentScope,
        Symbols:     make(map[string]*Symbol),
        Implicit:    st.CurrentScope.Implicit.Copy(), // Inherit implicit rules
        ProgramUnit: unit,
        ScopeType:   scopeType,
    }
    st.CurrentScope.Children = append(st.CurrentScope.Children, newScope)
    st.CurrentScope = newScope
    return newScope
}

// ExitScope returns to parent scope
func (st *SymbolTable) ExitScope() {
    if st.CurrentScope.Parent != nil {
        st.CurrentScope = st.CurrentScope.Parent
    }
}

// Lookup searches for symbol in current scope and parent scopes
func (st *SymbolTable) Lookup(name string) *Symbol {
    name = normalizeCase(name) // Fortran is case-insensitive

    // Search current scope
    if sym, ok := st.CurrentScope.Symbols[name]; ok {
        return sym
    }

    // Search parent scopes
    scope := st.CurrentScope.Parent
    for scope != nil {
        if sym, ok := scope.Symbols[name]; ok {
            return sym
        }
        scope = scope.Parent
    }

    // Check intrinsics
    if intrinsic, ok := st.Intrinsics[name]; ok {
        return &Symbol{
            Name: name,
            Kind: SymIntrinsic,
            Type: &ResolvedType{BaseType: intrinsic.ReturnType},
        }
    }

    return nil
}

// Define adds a symbol to current scope
func (st *SymbolTable) Define(sym *Symbol) error {
    name := normalizeCase(sym.Name)

    // Check for duplicate in current scope only
    if existing, ok := st.CurrentScope.Symbols[name]; ok {
        return fmt.Errorf("duplicate symbol %s (previous declaration at %v)",
            name, existing.DeclNode.SourcePos())
    }

    sym.Scope = st.CurrentScope
    st.CurrentScope.Symbols[name] = sym
    return nil
}

// LookupLocal searches only current scope (not parents)
func (st *SymbolTable) LookupLocal(name string) *Symbol {
    name = normalizeCase(name)
    return st.CurrentScope.Symbols[name]
}

func normalizeCase(name string) string {
    // Fortran is case-insensitive, normalize to uppercase
    return strings.ToUpper(name)
}

func defaultImplicitRules() *ImplicitRules {
    // Default F77/F90 rules: I-N are INTEGER, A-H and O-Z are REAL
    rules := &ImplicitRules{}
    for ch := 'A'; ch <= 'H'; ch++ {
        rules.LetterTypes[ch-'A'] = "REAL"
    }
    for ch := 'I'; ch <= 'N'; ch++ {
        rules.LetterTypes[ch-'A'] = "INTEGER"
    }
    for ch := 'O'; ch <= 'Z'; ch++ {
        rules.LetterTypes[ch-'A'] = "REAL"
    }
    return rules
}
```

### Type Resolution System

#### Phase 1: Declaration Collection

**File**: `symbol/collector.go` (new file)

```go
// DeclarationCollector walks AST and builds initial symbol table
type DeclarationCollector struct {
    symtab *SymbolTable
    errors []error
}

func CollectDeclarations(prog *ast.Program) (*SymbolTable, []error) {
    collector := &DeclarationCollector{
        symtab: NewSymbolTable(),
    }

    // Process all program units
    for _, unit := range prog.Units {
        collector.visitProgramUnit(unit)
    }

    return collector.symtab, collector.errors
}

func (c *DeclarationCollector) visitProgramUnit(unit ast.ProgramUnit) {
    switch u := unit.(type) {
    case *ast.ProgramBlock:
        c.symtab.EnterScope(unit, ScopeProgram)
        c.processStatements(u.Body)
        c.symtab.ExitScope()

    case *ast.Subroutine:
        c.symtab.EnterScope(unit, ScopeProcedure)
        // Add parameters as symbols
        for _, param := range u.Parameters {
            sym := c.parameterToSymbol(&param)
            c.symtab.Define(sym)
        }
        c.processStatements(u.Body)
        c.symtab.ExitScope()

    case *ast.Function:
        c.symtab.EnterScope(unit, ScopeProcedure)
        // Add parameters
        for _, param := range u.Parameters {
            sym := c.parameterToSymbol(&param)
            c.symtab.Define(sym)
        }
        // Add result variable
        resultName := u.ResultVariable
        if resultName == "" {
            resultName = u.Name
        }
        c.symtab.Define(&Symbol{
            Name: resultName,
            Type: c.parseTypeSpec(u.ResultType),
            Kind: SymVariable,
        })
        c.processStatements(u.Body)
        c.symtab.ExitScope()

    case *ast.Module:
        c.symtab.EnterScope(unit, ScopeModule)
        c.processStatements(u.Body)
        // Process CONTAINS section
        for _, contained := range u.Contains {
            c.visitProgramUnit(contained)
        }
        c.symtab.ExitScope()
    }
}

func (c *DeclarationCollector) processStatements(stmts []ast.Statement) {
    for _, stmt := range stmts {
        switch s := stmt.(type) {
        case *ast.TypeDeclaration:
            c.processTypeDeclaration(s)
        case *ast.ImplicitStatement:
            c.processImplicit(s)
        case *ast.CommonStmt:
            c.processCommon(s)
        case *ast.ExternalStmt:
            c.processExternal(s)
        case *ast.IntrinsicStmt:
            c.processIntrinsic(s)
        // Ignore executable statements in declaration pass
        }
    }
}

func (c *DeclarationCollector) processTypeDeclaration(decl *ast.TypeDeclaration) {
    baseType := &ResolvedType{
        BaseType: decl.TypeSpec,
        // TODO: Extract Kind from decl.KindParam
    }

    // Check for PARAMETER attribute
    isParameter := false
    for _, attr := range decl.Attributes {
        if attr == token.PARAMETER {
            isParameter = true
            break
        }
    }

    // Create symbol for each declared entity
    for _, entity := range decl.Entities {
        sym := &Symbol{
            Name:       entity.Name,
            Type:       baseType,
            Kind:       SymVariable,
            Attributes: decl.Attributes,
            ArraySpec:  entity.ArraySpec,
            DeclNode:   decl,
            IsImplicit: false,
        }

        if isParameter {
            sym.Kind = SymParameter
        }

        if err := c.symtab.Define(sym); err != nil {
            c.errors = append(c.errors, err)
        }
    }
}

func (c *DeclarationCollector) processImplicit(impl *ast.ImplicitStatement) {
    if impl.IsNone {
        c.symtab.CurrentScope.Implicit.IsNone = true
        // Clear all implicit type rules
        for i := range c.symtab.CurrentScope.Implicit.LetterTypes {
            c.symtab.CurrentScope.Implicit.LetterTypes[i] = ""
        }
    } else {
        // Process custom implicit rules
        for _, rule := range impl.Rules {
            for _, letterRange := range rule.LetterRanges {
                for ch := letterRange.Start; ch <= letterRange.End; ch++ {
                    c.symtab.CurrentScope.Implicit.LetterTypes[ch-'A'] = rule.Type
                    // TODO: Handle rule.Kind
                }
            }
        }
    }
}

func (c *DeclarationCollector) processCommon(common *ast.CommonStmt) {
    blockName := common.BlockName
    if blockName == "" {
        blockName = "__blank__" // Internal name for blank COMMON
    }

    block, exists := c.symtab.CommonBlocks[blockName]
    if !exists {
        block = &CommonBlock{Name: blockName}
        c.symtab.CommonBlocks[blockName] = block
    }

    block.Variables = append(block.Variables, common.Variables...)
}

func (c *DeclarationCollector) processExternal(ext *ast.ExternalStmt) {
    for _, name := range ext.Names {
        sym := &Symbol{
            Name: name,
            Kind: SymExternal,
        }
        c.symtab.Define(sym)
    }
}

func (c *DeclarationCollector) processIntrinsic(intr *ast.IntrinsicStmt) {
    for _, name := range intr.Names {
        sym := &Symbol{
            Name: name,
            Kind: SymIntrinsic,
        }
        c.symtab.Define(sym)
    }
}
```

#### Phase 2: Type Resolution

**File**: `symbol/resolver.go` (new file)

```go
// TypeResolver resolves types of all identifiers
type TypeResolver struct {
    symtab *SymbolTable
    errors []error
}

func ResolveTypes(prog *ast.Program, symtab *SymbolTable) []error {
    resolver := &TypeResolver{symtab: symtab}

    // Walk entire AST and resolve identifier types
    ast.Walk(&resolver, prog)

    return resolver.errors
}

func (r *TypeResolver) Visit(node ast.Node) ast.Visitor {
    switch n := node.(type) {
    case *ast.Identifier:
        r.resolveIdentifier(n)
    case *ast.FunctionCall:
        r.resolveFunctionCall(n)
    case *ast.ArrayRef:
        r.resolveArrayRef(n)
    }
    return r
}

func (r *TypeResolver) resolveIdentifier(id *ast.Identifier) {
    // Look up symbol
    sym := r.symtab.Lookup(id.Value)

    if sym != nil {
        // Symbol exists, already has type
        return
    }

    // Not found - apply implicit typing
    if r.symtab.CurrentScope.Implicit.IsNone {
        r.errors = append(r.errors, fmt.Errorf(
            "variable %s used without declaration (IMPLICIT NONE active)",
            id.Value))
        return
    }

    // Apply implicit rules based on first letter
    firstLetter := id.Value[0]
    if firstLetter >= 'a' && firstLetter <= 'z' {
        firstLetter = firstLetter - 'a' + 'A'
    }
    typeStr := r.symtab.CurrentScope.Implicit.LetterTypes[firstLetter-'A']

    if typeStr == "" {
        r.errors = append(r.errors, fmt.Errorf(
            "no implicit type for variable %s starting with %c",
            id.Value, firstLetter))
        return
    }

    // Create implicit symbol
    sym = &Symbol{
        Name: id.Value,
        Type: &ResolvedType{
            BaseType: typeStr,
        },
        Kind:       SymVariable,
        IsImplicit: true,
    }
    r.symtab.Define(sym)
}

func (r *TypeResolver) resolveFunctionCall(call *ast.FunctionCall) {
    sym := r.symtab.Lookup(call.Name)

    if sym == nil {
        // Could be implicit function or array access
        // Need more context to determine
        r.resolveIdentifier(&ast.Identifier{Value: call.Name})
        return
    }

    // Check symbol kind
    switch sym.Kind {
    case SymFunction, SymIntrinsic:
        // It's a function call - correct
    case SymVariable:
        if sym.ArraySpec != nil {
            // It's actually an array reference, not a function call
            // Caller should have used ArrayRef
        }
    case SymExternal:
        // External procedure - assume function
        sym.Kind = SymFunction
    default:
        r.errors = append(r.errors, fmt.Errorf(
            "%s is not a function", call.Name))
    }
}
```

#### Phase 3: Expression Type Inference

```go
// ExpressionTyper computes result types of expressions
type ExpressionTyper struct {
    symtab *SymbolTable
}

func (et *ExpressionTyper) TypeOf(expr ast.Expression) *ResolvedType {
    switch e := expr.(type) {
    case *ast.IntegerLiteral:
        return &ResolvedType{BaseType: "INTEGER"}

    case *ast.RealLiteral:
        return &ResolvedType{BaseType: "REAL"}

    case *ast.StringLiteral:
        return &ResolvedType{
            BaseType: "CHARACTER",
            CharLen:  len(e.Value),
        }

    case *ast.LogicalLiteral:
        return &ResolvedType{BaseType: "LOGICAL"}

    case *ast.Identifier:
        sym := et.symtab.Lookup(e.Value)
        if sym != nil {
            return sym.Type
        }
        return nil

    case *ast.BinaryExpr:
        return et.typeBinaryExpr(e)

    case *ast.UnaryExpr:
        return et.TypeOf(e.Operand)

    case *ast.FunctionCall:
        return et.typeFunctionCall(e)

    default:
        return nil
    }
}

func (et *ExpressionTyper) typeBinaryExpr(expr *ast.BinaryExpr) *ResolvedType {
    leftType := et.TypeOf(expr.Left)
    rightType := et.TypeOf(expr.Right)

    if leftType == nil || rightType == nil {
        return nil
    }

    // Fortran type promotion rules
    switch expr.Op {
    case token.Plus, token.Minus, token.Asterisk, token.Slash, token.Power:
        // Arithmetic operators
        return promoteNumericTypes(leftType, rightType)

    case token.Concat:
        // String concatenation
        return &ResolvedType{
            BaseType: "CHARACTER",
            CharLen:  leftType.CharLen + rightType.CharLen,
        }

    case token.EQ, token.NE, token.LT, token.LE, token.GT, token.GE:
        // Relational operators
        return &ResolvedType{BaseType: "LOGICAL"}

    case token.AND, token.OR, token.EQV, token.NEQV:
        // Logical operators
        return &ResolvedType{BaseType: "LOGICAL"}

    default:
        return nil
    }
}

func promoteNumericTypes(t1, t2 *ResolvedType) *ResolvedType {
    // Fortran type promotion hierarchy:
    // INTEGER < REAL < DOUBLE PRECISION < COMPLEX < DOUBLE COMPLEX

    rank := func(t *ResolvedType) int {
        switch t.BaseType {
        case "INTEGER":
            return 1
        case "REAL":
            if t.Kind == 8 {
                return 3 // DOUBLE PRECISION
            }
            return 2
        case "DOUBLE PRECISION":
            return 3
        case "COMPLEX":
            if t.Kind == 8 {
                return 5 // DOUBLE COMPLEX
            }
            return 4
        default:
            return 0
        }
    }

    r1, r2 := rank(t1), rank(t2)
    if r1 >= r2 {
        return t1
    }
    return t2
}
```

### IMPLICIT Rule Engine

**File**: `symbol/implicit.go` (new file)

```go
// ApplyImplicitRules applies implicit typing to a scope
func ApplyImplicitRules(scope *Scope) {
    // If IMPLICIT NONE, nothing to do
    if scope.Implicit.IsNone {
        return
    }

    // For each symbol without explicit type, apply implicit rules
    for name, sym := range scope.Symbols {
        if sym.Type != nil {
            continue // Already has type
        }

        firstLetter := name[0]
        if firstLetter >= 'a' && firstLetter <= 'z' {
            firstLetter = firstLetter - 'a' + 'A'
        }

        typeStr := scope.Implicit.LetterTypes[firstLetter-'A']
        if typeStr != "" {
            sym.Type = &ResolvedType{
                BaseType: typeStr,
                Kind:     scope.Implicit.LetterKinds[firstLetter-'A'],
            }
            sym.IsImplicit = true
        }
    }
}

// ParseImplicitRules creates ImplicitRules from ImplicitStatement
func ParseImplicitRules(stmt *ast.ImplicitStatement) *ImplicitRules {
    rules := &ImplicitRules{}

    if stmt.IsNone {
        rules.IsNone = true
        return rules
    }

    // Start with default F77 rules
    for ch := 'A'; ch <= 'H'; ch++ {
        rules.LetterTypes[ch-'A'] = "REAL"
    }
    for ch := 'I'; ch <= 'N'; ch++ {
        rules.LetterTypes[ch-'A'] = "INTEGER"
    }
    for ch := 'O'; ch <= 'Z'; ch++ {
        rules.LetterTypes[ch-'A'] = "REAL"
    }

    // Override with custom rules
    for _, rule := range stmt.Rules {
        for _, lr := range rule.LetterRanges {
            for ch := lr.Start; ch <= lr.End; ch++ {
                rules.LetterTypes[ch-'A'] = rule.Type
                // TODO: Handle rule.Kind when Expression is evaluated
            }
        }
    }

    return rules
}
```

---

## Required Parser Enhancements

### Priority 1: Essential for Type Resolution

#### 1. Add KIND Parameter Support

**Estimated Effort**: 3-4 days

**Files to Modify**:
1. `ast/ast.go`
2. `parser.go`

**Changes**:

```go
// ast/ast.go - Update TypeDeclaration
type TypeDeclaration struct {
    TypeSpec   string
    KindParam  Expression  // NEW: for KIND=8 or *8
    CharLen    Expression  // Change from string to Expression
    Attributes []token.Token
    Entities   []DeclEntity
    Label      string
    Position
}

// ast/ast.go - Update Function result type
type Function struct {
    Name           string
    ResultType     string      // Base type
    ResultKind     Expression  // NEW: KIND parameter for result
    ResultVariable string
    Parameters     []Parameter
    Attributes     []token.Token
    Body           []Statement
    Label          string
    Position
}

// ast/ast.go - Update Parameter
type Parameter struct {
    Name       string
    Type       string
    TypeKind   Expression  // NEW: KIND parameter
    Intent     IntentType
    Attributes []token.Token
    ArraySpec  *ArraySpec
    CharLen    Expression  // Change from string
}
```

**Parser Changes** (`parser.go`):

```go
// Add new function to parse kind selector
func (p *Parser90) parseKindSelector() Expression {
    // Handle: (KIND=expr), (expr), or *N

    if p.currentTokenIs(token.Asterisk) {
        p.nextToken() // consume *
        // Parse integer literal: *4, *8, etc.
        if p.currentTokenIs(token.IntLiteral) {
            return p.parseExpression(0)
        }
    }

    if p.currentTokenIs(token.LParen) {
        p.nextToken() // consume (

        // Check for KIND= prefix
        if p.currentTokenIs(token.KIND) && p.peekTokenIs(token.Equals) {
            p.nextToken() // consume KIND
            p.nextToken() // consume =
        }

        expr := p.parseExpression(0)
        p.expect(token.RParen)
        return expr
    }

    return nil
}

// Modify parseTypeDecl (line ~2525)
func (p *Parser90) parseTypeDecl(typeSpec string) *ast.TypeDeclaration {
    decl := &ast.TypeDeclaration{
        TypeSpec: typeSpec,
        Label:    p.getLabelIfPresent(),
        Position: ast.Pos(p.current.pos, p.current.pos),
    }

    // NEW: Parse KIND selector if present
    if p.currentTokenIs(token.Asterisk) || p.currentTokenIs(token.LParen) {
        decl.KindParam = p.parseKindSelector()
    }

    // Rest of function unchanged...
}
```

**Tests to Add** (`ast_validation_test.go`):

```go
{
    name: "INTEGER with KIND selector",
    src:  "INTEGER(KIND=8) :: bigint",
    validate: func(t *testing.T, stmt ast.Statement) {
        decl := stmt.(*ast.TypeDeclaration)
        if decl.KindParam == nil {
            t.Error("Expected KindParam to be non-nil")
        }
    },
},
{
    name: "REAL with F77 kind syntax",
    src:  "REAL*8 :: dbl",
    validate: func(t *testing.T, stmt ast.Statement) {
        decl := stmt.(*ast.TypeDeclaration)
        if decl.KindParam == nil {
            t.Error("Expected KindParam to be non-nil")
        }
    },
},
```

#### 2. Parse COMMON Blocks

**Estimated Effort**: 2 days

**Files to Modify**:
1. `ast/ast.go` - Add new node
2. `parser.go` - Add parsing function

**AST Node** (`ast/ast.go`):

```go
// CommonStmt represents a COMMON block declaration
//
// Examples:
//   COMMON /block1/ a, b, c
//   COMMON // x, y, z         ! Blank COMMON
//   COMMON /data/ array(100)
type CommonStmt struct {
    BlockName string   // Empty for blank COMMON
    Variables []string // Variable names in block
    Label     string
    Position
}

func (c *CommonStmt) statementNode() {}
func (c *CommonStmt) GetLabel() string { return c.Label }
func (c *CommonStmt) AppendTokenLiteral(dst []byte) []byte {
    return append(dst, "COMMON"...)
}
func (c *CommonStmt) AppendString(dst []byte) []byte {
    dst = append(dst, "COMMON"...)
    if c.BlockName != "" {
        dst = append(dst, " /"...)
        dst = append(dst, c.BlockName...)
        dst = append(dst, '/')
    }
    for i, v := range c.Variables {
        if i > 0 {
            dst = append(dst, ", "...)
        } else {
            dst = append(dst, ' ')
        }
        dst = append(dst, v...)
    }
    return dst
}
```

**Parser Function** (`parser.go`):

```go
// Add to parseSpecStatement() switch
case token.COMMON:
    return p.parseCommonStmt()

// New function
func (p *Parser90) parseCommonStmt() ast.Statement {
    startPos := p.current.pos
    p.nextToken() // consume COMMON

    stmt := &ast.CommonStmt{
        Label: p.getLabelIfPresent(),
    }

    // Check for named COMMON: /name/
    if p.currentTokenIs(token.Slash) {
        p.nextToken() // consume first /

        if !p.currentTokenIs(token.Slash) {
            // Named COMMON
            if p.canUseAsIdentifier() {
                stmt.BlockName = string(p.current.lit)
                p.nextToken()
            }
        }
        // else: blank COMMON (BlockName stays empty)

        p.expect(token.Slash) // consume second /
    }
    // else: blank COMMON without slashes (F77 extension)

    // Parse comma-separated variable list
    for {
        if !p.canUseAsIdentifier() {
            p.addError("expected variable name in COMMON block")
            break
        }

        varName := string(p.current.lit)
        stmt.Variables = append(stmt.Variables, varName)
        p.nextToken()

        // Check for array specification
        if p.currentTokenIs(token.LParen) {
            // Skip array spec for now (COMMON /block/ arr(10,20))
            // TODO: Parse and store array bounds
            p.skipBalancedParens()
        }

        if !p.consumeIf(token.Comma) {
            break
        }
    }

    stmt.Position = ast.Pos(startPos, p.current.pos)
    return stmt
}
```

**Tests** (`parsing_stmt_test.go`):

```go
{
    name: "Named COMMON block",
    src:  "COMMON /myblock/ a, b, c",
    validate: func(t *testing.T, stmt ast.Statement) {
        common, ok := stmt.(*ast.CommonStmt)
        if !ok {
            t.Fatalf("Expected CommonStmt, got %T", stmt)
        }
        if common.BlockName != "myblock" {
            t.Errorf("Expected block name 'myblock', got '%s'", common.BlockName)
        }
        if len(common.Variables) != 3 {
            t.Errorf("Expected 3 variables, got %d", len(common.Variables))
        }
    },
},
{
    name: "Blank COMMON block",
    src:  "COMMON // x, y, z",
    validate: func(t *testing.T, stmt ast.Statement) {
        common := stmt.(*ast.CommonStmt)
        if common.BlockName != "" {
            t.Errorf("Expected blank COMMON, got block name '%s'", common.BlockName)
        }
    },
},
```

#### 3. Parse EXTERNAL and INTRINSIC Statements

**Estimated Effort**: 1 day

**AST Nodes** (`ast/ast.go`):

```go
type ExternalStmt struct {
    Names []string  // Procedure names declared EXTERNAL
    Label string
    Position
}

type IntrinsicStmt struct {
    Names []string  // Function names declared INTRINSIC
    Label string
    Position
}

// Implement Statement interface for both...
```

**Parser Functions** (`parser.go`):

```go
case token.EXTERNAL:
    return p.parseExternalStmt()
case token.INTRINSIC:
    return p.parseIntrinsicStmt()

func (p *Parser90) parseExternalStmt() ast.Statement {
    startPos := p.current.pos
    p.nextToken() // consume EXTERNAL

    stmt := &ast.ExternalStmt{Label: p.getLabelIfPresent()}

    // Parse comma-separated name list
    stmt.Names = p.parseIdentifierList()

    stmt.Position = ast.Pos(startPos, p.current.pos)
    return stmt
}

func (p *Parser90) parseIntrinsicStmt() ast.Statement {
    // Similar to parseExternalStmt...
}

// Helper function
func (p *Parser90) parseIdentifierList() []string {
    var names []string
    for {
        if !p.canUseAsIdentifier() {
            p.addError("expected identifier")
            break
        }
        names = append(names, string(p.current.lit))
        p.nextToken()

        if !p.consumeIf(token.Comma) {
            break
        }
    }
    return names
}
```

#### 4. Extend IMPLICIT Statement Parsing

**Estimated Effort**: 2-3 days

**AST Changes** (`ast/ast.go`):

```go
type ImplicitStatement struct {
    IsNone bool          // true for IMPLICIT NONE
    Rules  []ImplicitRule // Custom type rules
    Label  string
    Position
}

type ImplicitRule struct {
    Type         string        // "INTEGER", "REAL", etc.
    KindParam    Expression    // Optional KIND selector
    LetterRanges []LetterRange // Letter ranges this applies to
}

type LetterRange struct {
    Start byte  // 'A' to 'Z'
    End   byte  // 'A' to 'Z', Start <= End (same letter if single)
}
```

**Parser Changes** (`parser.go:2473`):

```go
func (p *Parser90) parseImplicit() ast.Statement {
    startPos := p.current.pos
    p.nextToken() // consume IMPLICIT

    stmt := &ast.ImplicitStatement{
        Label: p.getLabelIfPresent(),
    }

    if p.currentTokenIs(token.NONE) {
        stmt.IsNone = true
        p.nextToken()
        stmt.Position = ast.Pos(startPos, p.current.pos)
        return stmt
    }

    // Parse type rules: IMPLICIT REAL (A-H, O-Z)
    for !p.currentTokenIs(token.NewLine) && !p.IsDone() {
        rule := ast.ImplicitRule{}

        // Parse type
        if !p.currentTokenIsType() {
            p.addError("expected type in IMPLICIT statement")
            break
        }
        rule.Type = string(p.current.lit)
        p.nextToken()

        // Optional KIND selector
        if p.currentTokenIs(token.LParen) || p.currentTokenIs(token.Asterisk) {
            rule.KindParam = p.parseKindSelector()
        }

        // Parse letter ranges: (A-H, X-Z, I)
        if !p.expect(token.LParen) {
            break
        }

        for !p.currentTokenIs(token.RParen) {
            letterRange := ast.LetterRange{}

            if !p.currentTokenIs(token.Identifier) {
                p.addError("expected letter in IMPLICIT range")
                break
            }

            letterStr := string(p.current.lit)
            if len(letterStr) != 1 {
                p.addError("IMPLICIT letter must be single character")
                break
            }
            letterRange.Start = letterStr[0]
            letterRange.End = letterStr[0]
            p.nextToken()

            // Check for range: A-H
            if p.currentTokenIs(token.Minus) {
                p.nextToken()
                if !p.currentTokenIs(token.Identifier) {
                    p.addError("expected end letter in IMPLICIT range")
                    break
                }
                endStr := string(p.current.lit)
                if len(endStr) != 1 {
                    p.addError("IMPLICIT letter must be single character")
                    break
                }
                letterRange.End = endStr[0]
                p.nextToken()
            }

            rule.LetterRanges = append(rule.LetterRanges, letterRange)

            if !p.consumeIf(token.Comma) {
                break
            }
        }

        p.expect(token.RParen)
        stmt.Rules = append(stmt.Rules, rule)

        // Multiple type rules separated by commas (rare)
        if !p.consumeIf(token.Comma) {
            break
        }
    }

    stmt.Position = ast.Pos(startPos, p.current.pos)
    return stmt
}
```

### Priority 2: Important for F77 Compatibility

#### 5. EQUIVALENCE Statement Parsing

**Estimated Effort**: 2 days

(Similar structure to COMMON - parse parenthesized groups)

#### 6. NAMELIST Statement Parsing

**Estimated Effort**: 1 day

(Similar to COMMON parsing)

### Priority 3: Modern Fortran Features

#### 7. Enable TYPE...END TYPE Parsing

**Estimated Effort**: 3-4 days

**Current State**: Parser has `skipTypeDefinition()` that skips these blocks

**What to Do**:
- Remove skip calls
- Implement full derived type parsing
- Parse component declarations
- Handle PRIVATE/PUBLIC for components

#### 8. Enable INTERFACE...END INTERFACE Parsing

**Estimated Effort**: 3-4 days

**Current State**: Parser skips these blocks

**What to Do**:
- Parse INTERFACE blocks
- Handle generic interfaces
- Handle operator overloading
- Parse module procedure lists

---

## Implementation Roadmap

### Week 1: Core Parser Enhancements

**Goals**:
- Add KIND parameter support (AST + parser)
- Parse COMMON blocks
- Parse EXTERNAL/INTRINSIC statements
- Extend IMPLICIT statement parsing

**Deliverables**:
- Updated AST nodes
- Parser functions for new statements
- 20+ new tests in `ast_validation_test.go` and `parsing_stmt_test.go`
- All tests pass

**Success Criteria**:
- `testdata/valid_specification.f90` fully parses (currently has skipped constructs)
- KIND parameters captured in AST
- IMPLICIT rules captured in AST

### Week 2: Symbol Table Foundation

**Goals**:
- Create `symbol/` package
- Implement Symbol, SymbolTable, Scope structures
- Implement scope operations (Enter, Exit, Lookup, Define)
- Implement IMPLICIT rule data structures

**Deliverables**:
- `symbol/symbol.go` - Core data structures
- `symbol/symbol_test.go` - Unit tests for symbol table operations
- 90%+ test coverage

**Success Criteria**:
- Can create scopes and nest them correctly
- Can define symbols and look them up
- Can handle case-insensitive lookups (Fortran)
- All unit tests pass

### Week 3: Declaration Collection Pass

**Goals**:
- Complete `ast/walk.go` with all node types
- Implement DeclarationCollector
- Build symbol table from AST
- Handle nested scopes (CONTAINS sections)

**Deliverables**:
- `ast/walk.go` - Complete walker
- `symbol/collector.go` - Declaration collector
- `symbol/collector_test.go` - Tests with sample Fortran programs

**Success Criteria**:
- Can build symbol table from `testdata/*.f90` files
- All declarations captured correctly
- Nested scopes work (modules with CONTAINS)
- Parameter types correctly populated

### Week 4: Type Resolution Implementation

**Goals**:
- Implement IMPLICIT rule engine
- Implement TypeResolver
- Implement ExpressionTyper
- Create intrinsic function database

**Deliverables**:
- `symbol/implicit.go` - IMPLICIT rule engine
- `symbol/resolver.go` - Type resolver
- `symbol/intrinsics.go` - Intrinsic function database
- `symbol/resolver_test.go` - Comprehensive tests

**Success Criteria**:
- Default F77 implicit rules (I-N = INTEGER, etc.) work
- Custom IMPLICIT rules applied correctly
- Undeclared variables get types from implicit rules
- IMPLICIT NONE causes errors for undeclared variables
- All identifiers resolved to types

### Week 5: Integration & Validation

**Goals**:
- Integrate all components
- Test on real Fortran code
- Validate type resolution correctness
- Performance testing

**Deliverables**:
- Integration tests with complete Fortran programs
- Performance benchmarks
- Documentation of type resolution system
- Bug fixes from real-world testing

**Success Criteria**:
- All `testdata/*.f90` files parse and resolve types
- Symbol table accurately represents program structure
- No regressions in parser tests
- < 100ms to parse and resolve types for typical files

---

## Fortran-to-Go Mapping Challenges

### Challenge 1: COMMON Blocks

**Fortran Pattern**:
```fortran
! In module1.f90
SUBROUTINE sub1
    COMMON /shared/ x, y, z
    x = 10
END SUBROUTINE

! In module2.f90
SUBROUTINE sub2
    COMMON /shared/ a, b, c  ! Same memory, different names!
    PRINT *, a  ! Prints 10
END SUBROUTINE
```

**Challenge**: Go has no shared global memory with implicit access

**Solution Options**:

1. **Global struct** (simplest):
```go
// Generated code
var commonShared = struct {
    X float64  // Assuming REAL
    Y float64
    Z float64
}{}

func sub1() {
    commonShared.X = 10
}

func sub2() {
    a := commonShared.X  // Map a -> X
    fmt.Println(a)
}
```

2. **Explicit parameter passing** (cleaner but requires analysis):
```go
type SharedData struct {
    X, Y, Z float64
}

func sub1(shared *SharedData) {
    shared.X = 10
}

func sub2(shared *SharedData) {
    fmt.Println(shared.X)
}
```

**Recommendation**: Use option 1 (global struct) initially, option 2 requires inter-procedural analysis

### Challenge 2: Array Indexing (1-based → 0-based)

**Fortran**:
```fortran
REAL :: arr(10)      ! Indices 1 to 10
arr(1) = 5.0         ! First element
arr(5) = arr(1) + 2  ! Fifth element
```

**Go Translation Options**:

1. **Direct translation with offset**:
```go
arr := make([]float64, 10)
arr[1-1] = 5.0    // arr[0]
arr[5-1] = arr[1-1] + 2  // arr[4]
```
Problem: Ugly, confusing

2. **Wrapper struct**:
```go
type FortranArray1D struct {
    data []float64
    lower int  // Lower bound (usually 1)
}

func (a *FortranArray1D) Get(i int) float64 {
    return a.data[i - a.lower]
}

func (a *FortranArray1D) Set(i int, v float64) {
    a.data[i - a.lower] = v
}

// Generated code
arr := NewFortranArray1D(10, 1)  // size=10, lower=1
arr.Set(1, 5.0)
arr.Set(5, arr.Get(1) + 2)
```
Problem: Verbose but correct

3. **Compile-time offset adjustment**:
```go
arr := make([]float64, 10)
arr[0] = 5.0  // Transpiler adjusts index
arr[4] = arr[0] + 2
```
Problem: Requires constant propagation to be reliable

**Recommendation**: Use option 3 for simple cases, option 2 for variable/expression indices

### Challenge 3: EQUIVALENCE Statement

**Fortran**:
```fortran
INTEGER :: i
REAL :: r
EQUIVALENCE (i, r)  ! i and r share same memory
i = 1065353216
PRINT *, r  ! Prints 1.0 (bit pattern of 1.0 as float)
```

**Challenge**: Go has no direct equivalent, requires unsafe pointer operations

**Solution Options**:

1. **Refuse to transpile**:
```go
// Error: EQUIVALENCE statement not supported in Go translation
// Please refactor Fortran code to remove EQUIVALENCE
```

2. **Use unsafe.Pointer** (advanced):
```go
import "unsafe"

var memory [4]byte
i := (*int32)(unsafe.Pointer(&memory[0]))
r := (*float32)(unsafe.Pointer(&memory[0]))
*i = 1065353216
fmt.Println(*r)  // Prints 1.0
```
Problem: Unsafe, platform-dependent

**Recommendation**: Option 1 (refuse), detect and report EQUIVALENCE as unsupported

### Challenge 4: FORMAT Statements

**Fortran**:
```fortran
100 FORMAT(I5, F10.2, A)
WRITE(*, 100) count, value, name
```

**Challenge**: Fortran FORMAT is very different from Go fmt

**Solution**:

Create FORMAT interpreter or translator:

```go
// Transpile FORMAT statement to fmt format string
format100 := "%5d %10.2f %s\n"  // Approximate
fmt.Printf(format100, count, value, name)
```

**Issues**:
- Fortran FORMAT has many features Go fmt doesn't (e.g., column positioning, repetition)
- May need runtime FORMAT interpreter in some cases

### Challenge 5: Subroutine Alternate Returns

**Fortran**:
```fortran
CALL SUB(x, *10, *20)  ! Return to label 10 or 20
```

**Challenge**: Go has no GOTO to labels in this way

**Solution**: Convert to return codes:

```go
func sub(x float64) int {
    if condition {
        return 1  // "return to label 10"
    }
    return 2  // "return to label 20"
}

// Call site
switch sub(x) {
case 1:
    goto label10
case 2:
    goto label20
}
```

### Challenge 6: ENTRY Statements

**Fortran**:
```fortran
SUBROUTINE foo(a, b)
    x = a + b
    RETURN
ENTRY bar(c)
    x = c * 2
    RETURN
END SUBROUTINE
```

**Challenge**: Multiple entry points into same subroutine

**Solution**: Split into multiple Go functions:

```go
func foo(a, b float64) {
    x := a + b
    // ...
}

func bar(c float64) {
    x := c * 2
    // ...
}
```

**Issues**: If ENTRY points share local variables, need to extract to struct

---

## Success Criteria

### Phase 1: Parser Enhancements

- ✅ KIND parameters captured in TypeDeclaration and Function
- ✅ COMMON blocks parsed into CommonStmt nodes
- ✅ EXTERNAL/INTRINSIC statements parsed
- ✅ IMPLICIT rules (beyond IMPLICIT NONE) parsed
- ✅ EQUIVALENCE statements parsed
- ✅ All `testdata/valid_specification.f90` constructs parse successfully
- ✅ 100+ new test cases added and passing
- ✅ No regressions in existing tests

### Phase 2: Symbol Table

- ✅ Symbol, SymbolTable, Scope structures implemented
- ✅ Scope nesting works correctly (global → module → procedure)
- ✅ Lookup finds symbols in current and parent scopes
- ✅ Case-insensitive symbol lookup (Fortran semantics)
- ✅ 90%+ test coverage for symbol package
- ✅ All unit tests pass

### Phase 3: Type Resolution

- ✅ IMPLICIT rules engine works (default F77/F90 + custom rules)
- ✅ All identifiers resolved to types
- ✅ IMPLICIT NONE prevents undeclared variable use
- ✅ Functions distinguished from arrays
- ✅ Intrinsic function database complete (50+ intrinsics)
- ✅ Expression type inference works for all operators
- ✅ All `testdata/*.f90` files fully type-resolved
- ✅ Performance: < 100ms for typical files

### Phase 4: Semantic Validation

- ✅ Type compatibility errors detected
- ✅ Array shape mismatches detected
- ✅ INTENT violations detected (IN modified, OUT not assigned)
- ✅ Comprehensive error messages with source positions
- ✅ 90%+ test coverage

### Phase 5: Code Generation

- ✅ Basic Fortran constructs → Go code
- ✅ Array indexing correctly translated
- ✅ COMMON blocks → Go global structs
- ✅ Can transpile simple Fortran programs end-to-end
- ✅ Generated Go code compiles and runs
- ✅ Test suite with 20+ complete program translations

---

## Appendix: Intrinsic Functions (Sample)

This is a subset - full database needs 100+ functions.

### Mathematical Functions

| Fortran | Return Type | Go Equivalent |
|---------|------------|---------------|
| ABS(x) | Same as x | math.Abs (for reals), abs for int |
| SQRT(x) | REAL | math.Sqrt |
| EXP(x) | REAL | math.Exp |
| LOG(x) | REAL | math.Log |
| LOG10(x) | REAL | math.Log10 |
| SIN(x) | REAL | math.Sin |
| COS(x) | REAL | math.Cos |
| TAN(x) | REAL | math.Tan |
| ASIN(x) | REAL | math.Asin |
| ACOS(x) | REAL | math.Acos |
| ATAN(x) | REAL | math.Atan |
| ATAN2(y,x) | REAL | math.Atan2 |
| MOD(a,p) | Same as a | Custom (% for int, math.Mod for float) |
| SIGN(a,b) | Same as a | Custom |
| MAX(a,b,...) | Same as args | math.Max (binary), custom for variadic |
| MIN(a,b,...) | Same as args | math.Min (binary), custom for variadic |

### Type Conversion

| Fortran | Return Type | Go Equivalent |
|---------|------------|---------------|
| INT(x) | INTEGER | int(), int32(), etc. |
| REAL(x) | REAL | float32(), float64() |
| DBLE(x) | DOUBLE PRECISION | float64() |
| CMPLX(x,y) | COMPLEX | complex(x, y) |
| CHAR(i) | CHARACTER | string(rune(i)) |
| ICHAR(c) | INTEGER | int(c[0]) |

### Array Functions

| Fortran | Return Type | Go Equivalent |
|---------|------------|---------------|
| SIZE(arr) | INTEGER | len(arr) |
| SIZE(arr,dim) | INTEGER | len(arr) for dim, custom for multi-D |
| SHAPE(arr) | INTEGER array | Custom: return []int{dims...} |
| LBOUND(arr) | INTEGER array | Custom: depends on array metadata |
| UBOUND(arr) | INTEGER array | Custom: depends on array metadata |

### String Functions

| Fortran | Return Type | Go Equivalent |
|---------|------------|---------------|
| LEN(s) | INTEGER | len(s) |
| LEN_TRIM(s) | INTEGER | len(strings.TrimRight(s, " ")) |
| TRIM(s) | CHARACTER | strings.TrimRight(s, " ") |
| ADJUSTL(s) | CHARACTER | strings.TrimLeft(s, " ") |
| ADJUSTR(s) | CHARACTER | Custom |
| INDEX(s,substr) | INTEGER | strings.Index(s, substr) + 1 |

---

## References

- **Fortran 77 Standard**: ANSI X3.9-1978
- **Fortran 90 Standard**: ISO/IEC 1539:1991
- **Current Codebase**: `/home/pato/Documents/src/ongoing/go-fortran/`
- **AST Definition**: `ast/ast.go`
- **Parser Implementation**: `parser.go`
- **Test Files**: `testdata/*.f90`, `*_test.go`

---

## Document Status

- **Created**: 2024-11-24
- **Last Updated**: 2024-11-24
- **Status**: Planning Phase
- **Next Review**: After Phase 1 completion

---

*End of Document*
