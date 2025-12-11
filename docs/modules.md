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

| Type | Mechanism | Example |
|------|-----------|---------|
| **Host Association** | Automatic via `CONTAINS` | Internal procedures see host's entities |
| **Use Association** | `USE` statement required | Modules → other program units |
| **Linkage Association** | `COMMON` / `EXTERNAL` | Cross-compilation-unit sharing |

### Key Rules

1. **USE is required for modules** - even within the same file
2. **CONTAINS gives automatic host association** - internal procedures see parent scope
3. **External procedures are islands** - no automatic data sharing
4. **File boundaries don't matter** - only program unit boundaries

---

## REPL Architecture

The `REPL` struct manages symbol resolution and type inference for transpilation.

### Fields

```go
type REPL struct {
    scope        ParserUnitData    // Current scope (program unit being transpiled)
    _use         []*ParserUnitData // FLATTENED: all accessible units (modules + their contains)
    _contains    []*ParserUnitData // Current scope's CONTAINS procedures
    used         []f90.ProgramUnit // NOT FLATTENED: original program units for code gen
    commonblocks []commonBlockInfo // COMMON block tracking
}
```

### `_use` vs `used` Distinction

| Field | Flattened? | Purpose |
|-------|------------|---------|
| `_use` | **Yes** | Name lookup - includes module-contained procedures |
| `used` | **No** | Code generation - original program units only |

**Example**: Adding a module with 2 contained subroutines:
```
AddUse(moduleM) where moduleM.Contains = [subA, subB]

_use  = [&moduleM.data, &subA.data, &subB.data]  // 3 entries (flattened)
used  = [moduleM]                                  // 1 entry (not flattened)
```

This design allows:
- **Variable lookup**: `repl.Var("x")` searches `scope` then all `_use` entries
- **Procedure lookup**: `repl.ContainedOrUsed("subA")` finds flattened entries
- **Code generation**: `transformProcedures(used)` recursively handles modules

---

## API Methods

### Adding External Units

```go
// AddUse registers external program units (modules, subroutines, functions).
// Modules are flattened: contained procedures are also registered in _use.
func (repl *REPL) AddUse(pu ...f90.ProgramUnit) error

// GetUsed returns ParserUnitData for a named unit from _use (flattened).
func (repl *REPL) GetUsed(name string) *ParserUnitData
```

### Scope Management

```go
// SetScope sets the current program unit being transpiled.
// Populates _contains from the unit's CONTAINS section.
func (repl *REPL) SetScope(pu f90.ProgramUnit) error

// Contained returns data for a procedure in current scope's CONTAINS.
func (repl *REPL) Contained(name string) *ParserUnitData

// ContainedOrUsed searches _contains first, then _use.
func (repl *REPL) ContainedOrUsed(name string) *ParserUnitData
```

### Variable Resolution

```go
// Var searches scope first, then _use for variable by name.
func (repl *REPL) Var(name string) *Varinfo
```

---

## ToGo Wrapper

`ToGo` wraps `REPL` for transpilation, forwarding methods:

```go
func (tg *ToGo) AddUsed(pu ...f90.ProgramUnit) error  // → repl.AddUse
func (tg *ToGo) GetUsed(name string) *ParserUnitData  // → repl.GetUsed
func (tg *ToGo) Contained(name string) *ParserUnitData
func (tg *ToGo) ContainedOrExtern(name string) *ParserUnitData // → repl.ContainedOrUsed
```

### Code Generation Flow

```go
// In TransformProgram:
decls, err = tg.transformProcedures(decls, prog.Contains)  // CONTAINS section
decls, err = tg.transformProcedures(decls, tg.repl.used)   // External units

// transformProcedures recursively handles modules:
case *f90.Module:
    dst, err = tg.transformProcedures(dst, c.Contains)  // Recurse into module
```

---

## Usage Example

```go
// Parse external module
modUnit, _ := parser.ParseFile("mathlib.f90")
mod := modUnit.(*f90.Module)

// Add to transpiler
tg := &ToGo{}
tg.AddUsed(mod)  // Registers module + its contained procedures

// Now transpile main program - can resolve calls to module procedures
prog, _ := parser.ParseFile("main.f90")
decls, _ := tg.TransformProgram(prog.(*f90.ProgramBlock))
```

---

## Design Rationale

1. **Flattening `_use`**: Enables O(n) lookup for any accessible procedure name without traversing module hierarchies during transpilation.

2. **Keeping `used` unflattened**: `transformProcedures` needs the original structure to properly recurse into modules and generate declarations in correct order.

3. **Separate `_contains`**: Host association (CONTAINS) is different from USE association - `_contains` is reset per scope, while `_use` persists.
