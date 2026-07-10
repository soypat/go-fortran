# Varinfo: Variable Semantic Information

Varinfo is the central semantic information store for Fortran variables. It bridges parsing, transpilation, and REPL evaluation.

## Struct Definition

```go
// parser.go:436-448
type Varinfo struct {
    decl     *ast.DeclEntity     // AST declaration (type, array specs, initializer)
    flags    VarFlags            // Semantic flags for variable attributes
    _varname string              // Variable name
    common   string              // COMMON block name (when VFlagCommon set)
    pointee  string              // Pointee name for Cray POINTER or EQUIVALENCE
    declPos  sourcePos           // Source location of declaration
    val      Value               // Runtime value for REPL evaluation
    kindFlag int                 // Kind parameter cache
    // Statement function fields (only if VFlagStmtFunc)
    stmtFuncExpr   ast.Expression
    stmtFuncParams []string
}
```

## VarFlags

Flags define semantic properties of variables:

| Flag | Meaning |
|------|---------|
| VFlagImplicit | Type inferred from IMPLICIT rules |
| VFlagUsed | Symbol referenced in code |
| VFlagPointer | Cray pointer variable (holds address, NOT auto-dereferenced) |
| VFlagTarget | Has TARGET attribute |
| VFlagParameter | Dummy argument of function/subroutine |
| VFlagAllocatable | Has ALLOCATABLE attribute |
| VFlagCommon | In a COMMON block |
| VFlagPointee | Cray pointee (accessed through pointer) |
| VFlagDimension | Is an array |
| VFlagIntentOut | Has INTENT(OUT) |
| VFlagIntentIn | Has INTENT(IN) |
| VFlagArrayInit | Array has initializer |
| VFlagArraySpec | ArraySpec in type declaration |
| VFlagReturned | Function return value |
| VFlagRecursive | RECURSIVE function |
| VFlagEquivalenced | Shares storage via EQUIVALENCE |
| VFlagConstantParameter | PARAMETER constant |
| VFlagStmtFunc | Statement function |

## Key Methods

**Type queries:**
- `TypeToken()` - Get type token from decl.Type
- `Dimensions()` - Get array specification
- `Kind()` - Get kind parameter
- `Charlen()` - Get CHARACTER length

**Predicate methods:**
- `IsArray()` - True if VFlagDimension set
- `IsChar()` - True if CHARACTER (not array of CHARACTER)
- `IsParameter()` - True if VFlagParameter set
- `IsAllocatable()` - True if VFlagAllocatable set
- `IsPointer()` - True for VFlagEquivalenced/VFlagPointee (NOT VFlagPointer, not arrays)

## Creation

Variables are created via `varInit`:

```go
p.varInit(name string, decl *ast.DeclEntity, initFlags VarFlags, namespace string)
```

**Common patterns:**
```go
// Explicit declaration
p.varInit(name, entity, VFlagDimension, "")

// Function parameter
p.varInit(param.Name, nil, VFlagParameter, "")

// COMMON variable
p.varInit(varName, decl, VFlagCommon, blockName)

// Equivalenced variable
p.varInit(varName, nil, VFlagEquivalenced, "")
```

## Target Types (_tgt*)

For transpilation, pre-built Varinfo templates are used as "target types":

```go
// intrinsics.go
_tgtInt32       = defaultVarinfo(f90token.INTEGER)
_tgtFloat32     = defaultVarinfo(f90token.REAL)
_tgtFloat64     = defaultVarinfo(f90token.DOUBLEPRECISION)
_tgtBool        = defaultVarinfo(f90token.LOGICAL)
_tgtStringLit   = defaultVarinfo(f90token.StringLit)
_tgtGenericFloat = defaultVarinfo(f90token.FloatLit)  // Promotes as needed
_tgtGenericInt   = defaultVarinfo(f90token.IntLit)    // Promotes as needed
```

**Array targets:**
```go
func _tgtArray(elem f90token.Token) *Varinfo {
    di := defaultVarinfo(elem)
    di.decl.ArraySpec = _tgtSpecDeferred
    di.flags |= VFlagDimension
    return di
}
```

## Transpilation Usage

**Expression transformation:**
```go
func (tg *ToGo) transformExpression(
    vitgt *Varinfo,     // Target type (what we're converting to)
    expr f90.Expression
) (result ast.Expr, resultType *Varinfo, err error)
```

The `vitgt` parameter tells transformExpression what type is expected. The result is then wrapped via `wrapConversion` if types don't match.

**Type conversion:**
```go
func (tg *ToGo) wrapConversion(
    target *Varinfo,      // What we're converting to
    sourceType *Varinfo,  // What we have
    expr ast.Expr         // The expression to wrap
) ast.Expr
```

**Go type generation:**
```go
func (tg *ToGo) goType(v *Varinfo) ast.Expr
// Generates:
// - intrinsic.PointerTo[T] for COMMON scalars, Cray pointers, EQUIVALENCE
// - *intrinsic.Array[T] for arrays
// - *T for INTENT(OUT) scalar parameters
// - T for regular scalars
```

## Important Distinctions

1. **Varinfo vs _tgt templates**: Varinfo stores actual variable info from parsing. _tgt* are synthetic Varinfo instances used during transpilation to indicate expected types. Both use the same struct but serve different purposes.

2. **IsPointer() semantics**: The `IsPointer()` method answers: "Does accessing this variable in Go require automatic dereferencing?"
   - Returns **true** for: `VFlagEquivalenced`, `VFlagPointee` - these are implemented as Go pointers that need dereferencing to access values
   - Returns **false** for: `VFlagPointer` (Cray pointer) - you want the address value itself, not what it points to
   - Returns **false** for: `VFlagDimension` (arrays) - arrays have their own `.At()` access pattern
   - Returns **false** for: `CHARACTER` types - use `intrinsic.CharacterArray` methods
   - Note: `VFlagIntentOut` is handled separately in function call transpilation via `wrapPointer()`

3. **Cray POINTER vs EQUIVALENCE**:
   ```fortran
   POINTER (NPAA, AA(1))  ! NPAA has VFlagPointer, AA has VFlagPointee
   EQUIVALENCE (A, B)      ! Both A and B have VFlagEquivalenced
   ```
   - `VFlagPointer`: The pointer variable itself. Accessing returns the address value.
   - `VFlagPointee`: Data accessed through a Cray pointer. Requires auto-dereferencing.
   - `VFlagEquivalenced`: Variables sharing storage. Both implemented as pointers needing deref.

4. **decl can be nil**: For implicitly typed variables or parameters without explicit declarations, decl may be nil initially and type inferred later via IMPLICIT rules.

5. **flags are merged**: Multiple calls to varInit for the same variable merge flags: `vi.flags |= initFlags`. A variable can have multiple flags (e.g., `VFlagParameter | VFlagIntentOut`).

## Go Type Mapping

| Fortran | Varinfo flags | Go Type |
|---------|---------------|---------|
| INTEGER :: x | (none) | int32 |
| INTEGER, DIMENSION(N) :: arr | VFlagDimension | *intrinsic.Array[int32] |
| INTEGER, INTENT(OUT) :: x | VFlagParameter, VFlagIntentOut | *int32 |
| COMMON /BLK/ x | VFlagCommon | intrinsic.PointerTo[int32] |
| EQUIVALENCE (a, b) | VFlagEquivalenced | intrinsic.PointerTo[T] |
| POINTER (ptr, pointee) | VFlagPointer (ptr), VFlagPointee (pointee) | intrinsic.PointerTo[T] |
