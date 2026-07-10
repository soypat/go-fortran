# Fortran Modularization & REPL API

## Fortran Program Unit Reachability

Fortran defines these **program units**, each a separate scoping unit:

| Unit | Description |
|------|-------------|
| `PROGRAM` | Main program |
| `MODULE` | Container for data, types, and procedures |
| `SUBMODULE` (F2008+) | Extension of module for separate implementation |
| `SUBROUTINE` / `FUNCTION` | External subprograms |
| `BLOCK DATA` | Initializes COMMON blocks |

### Association Types

| Type | Mechanism | Fortran rule |
|------|-----------|--------------|
| **Host association** | Automatic via `CONTAINS` | Internal procedures see host's entities without any declaration |
| **Use association** | `USE` statement required | Named entities from a MODULE become accessible |
| **Linkage association** | `COMMON` / `EXTERNAL` | Cross-compilation-unit storage sharing |

### Key Rules

1. `USE` required for modules — even within the same file.
2. `CONTAINS` gives automatic host association — internal procedures see parent scope.
3. External procedures are islands — no automatic data sharing.
4. File boundaries don't matter — only program unit boundaries.
5. If a contained procedure declares a local entity with the same name as a host entity, the local declaration shadows the host (no host association for that name).

---

## REPL Architecture

`REPL` manages symbol resolution and type inference for transpilation.

### Fields

```go
type REPL struct {
    scope        ParserUnitData    // Current scope (program unit being transpiled)
    registered   []f90.Unit        // All units registered via RegisterUnits
    _use         []*ParserUnitData // FLATTENED: all accessible units loaded via Use
    _contains    []*ParserUnitData // Contained procedures of current scope
    _hostScope   []Varinfo         // Host-associated vars from enclosing MODULE/PROGRAM
    commonblocks []commonBlockInfo // COMMON block tracking (not reset per procedure)
    formatSpecs  []ast.FormatStmt  // FORMAT statements for current unit
}
```

### `_use` Flattening

`_use` stores modules and their contained procedures in a flat list for O(n) name lookup.

```
RegisterUnits(moduleM) where moduleM.Contains = [subA, subB]
Use("moduleM")

_use = [&moduleM.data, &subA.data, &subB.data]  // 3 entries (flattened)
```

### Variable Resolution Order

`repl.Var(name)` searches in this order:
1. Current scope (`scope.vars`) — local declarations
2. Used modules (`_use`) — use association
3. Host scope (`_hostScope`) — host association

---

## Host Association in the Transpiler

Fortran host association is lexical: contained procedures see the host's entities as if declared locally. Go has no equivalent — closures capture values, not named identifiers usable as function parameters.

**Transpiler strategy:** host-associated variables are passed as explicit parameters to contained procedures. Scalars are passed by pointer (`*T`), arrays by pointer-to-array (`*intrinsic.Array[T]`).

### API

```go
// PushHostScope sets host-associated vars from the enclosing MODULE/PROGRAM.
// Returns pop to restore prior state. Call pop after CONTAINS processing.
func (repl *REPL) PushHostScope(vars []Varinfo) (pop func())

// HostScope returns the current host-associated variables.
func (repl *REPL) HostScope() []Varinfo
```

### Usage Pattern (TransformUnits)

```go
// Before processing CONTAINS:
pop := tg.repl.PushHostScope(tg.repl.scope.vars)
dst, err = tg.TransformUnits(dst, unit.Contains...)
pop()
```

```go
// In getScopeParams — host vars become first parameters:
hostScope := tg.repl.HostScope()
for i := range hostScope {
    vi := &hostScope[i]
    // scalars → *T, arrays → *intrinsic.Array[T]
}
```

---

## API Reference

### Unit Registration & Loading

```go
// RegisterUnits registers program units so they can be loaded via Use.
func (repl *REPL) RegisterUnits(pu ...f90.Unit) error

// Use loads a registered unit into _use scope (use association).
// only: optional list restricting which names are imported (ONLY clause).
func (repl *REPL) Use(name string, only ...string) error

// GetUsed returns ParserUnitData for a named unit from _use (flattened).
func (repl *REPL) GetUsed(name string) *ParserUnitData

// RegisteredUnit returns a unit from registered (before Use is called).
func (repl *REPL) RegisteredUnit(name string) *f90.Unit
```

### Scope Management

```go
// SetScope sets the current program unit being transpiled.
func (repl *REPL) SetScope(pu f90.Unit) error

// Contained returns data for a procedure in current scope's CONTAINS.
func (repl *REPL) Contained(name string) *ParserUnitData

// ContainedOrUsed searches _contains first, then _use.
func (repl *REPL) ContainedOrUsed(name string) *ParserUnitData

// ScopeParams returns dummy arguments of the current scope's procedure.
func (repl *REPL) ScopeParams() []Varinfo

// PushHostScope sets host-associated vars; returns restore function.
func (repl *REPL) PushHostScope(vars []Varinfo) (pop func())

// HostScope returns current host-associated variables.
func (repl *REPL) HostScope() []Varinfo
```

### Variable & Name Resolution

```go
// Var looks up a variable: local → use → host association.
func (repl *REPL) Var(name string) *Varinfo

// Namelist looks up a NAMELIST group by name.
func (repl *REPL) Namelist(name string) *ast.NamelistGroup

// PushVar temporarily adds a variable (e.g. loop variable); returns remove func.
func (repl *REPL) PushVar(v Varinfo) (remove func())
```
