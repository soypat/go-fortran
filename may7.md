# May 7 Session Notes

## Overview

Worked through `go test . -tags=gdyn` failures, fixing source-level Fortran bugs in `g2epp.f90` and transpiler bugs encountered along the way.

---

## g2epp.f90 Source Bugs

### TRAKRR → TRKEXT call (line 468593)
The call to TRKEXT inside TRAKRR was missing the last argument `L2SATL` (LOGICAL). Comparing with other TRKEXT call sites (lines 22597, 388695) confirmed the expected 49-arg signature. The call simply ended one argument early. Tagged `missingarg`, added `.FALSE.` as dummy.

### TRAKRR → TRKSPT call (line 468617)
TRKSPT expects 21 arguments. The call provided only 19, missing `DWRKDO` (work array) and `LOFFAJ` (LOGICAL). Other TRKSPT call sites (388772) pass `AA(KOFDRV)` and `LOFFA1` for these. TRAKRR has both `DUM` and `LOFFA1` in scope, which match the expected types. Tagged `missingarg`.

### TRAKRR → TRKGPS call (line 468623)
TRKGPS expects 21 arguments; the call provided 22. The last argument `.FALSE.` was extra. Additionally, the arg at index 20 (`L1ST`, LOGICAL) was passed as integer `0`. Other call sites (388776) pass `.TRUE.` or `.FALSE.` for L1ST. Fix: remove the trailing `.FALSE.` and change `0` to `.FALSE.`. Tagged `toomanyarg`.

### TRAKRR → TRKERS call (line 468628)
TRKERS expects 23 arguments, call provided 22. Missing `LDORANT` (LOGICAL) as final arg. `LDORANT` is declared within TRAKRR (`LOGICAL :: LDORANT` at 468761). Tagged `missingarg`.

### TRAKRR → TRKCR2 call (line 468684)
Same pattern as TRKERS — `LDORANT` missing as final argument. Tagged `missingarg`.

### TRKEXT: `LDVECT` typo (line 475985)
Inside TRKEXT, the condition `IF(LDVECT)` references an undefined identifier. The only related variable in scope (via `COMMON/LDATOR/`) is `LDVCT`. This was clearly a typo — `LDVECT` vs `LDVCT`. No ambiguity since `LDVCT` is used correctly elsewhere in TRKEXT (lines 475972, 476395, 476618).

### TRKGEOS3 → TDORTR call (line 482972)
TDORTR requires 10 arguments including `II` (integer dynamic array). TRKGEOS3 doesn't take `II` as a parameter and doesn't have it via common blocks. The function uses a local dummy `AA(1)` for the real array argument — the same pattern applied to `II`. Declared a local `INTEGER :: II_DUM(1)` and passed it. Tagged `missingarg`.

### U63 → DUMP1 call (line 509023)
DUMP1 expects 7 arguments: NM, NP, RESID, WT, PMPA, LNPNM, K. The call provided 6. Examining the DUMP1 body: it uses `K` (not `LNPNM`) to index into PMPA. The call passed `I` (loop counter) as the 6th arg where LNPNM belongs, but omitted K entirely. The logical interpretation: `I` is the column index K, and `LNPNM` (U63's own parameter) was simply dropped. Fix: insert `LNPNM` before `I`. Tagged `missingarg`.

### YAWGPS: `beta0` undefined (line 549005)
Condition `ABS(beta0).LT.1.D-6` references `beta0` which is never declared or assigned in YAWGPS. The variable `BETA` is computed at line 548983 (`BETA = PION2 - ACOS(COSALPHA0)`). The condition semantically guards against numerical overflow when the sun elevation angle (beta) approaches zero — consistent with `BETA` being the intended variable. Diagnosed as a typo: `beta0` → `BETA`.

---

## Transpiler Bugs

### Parser: `TYPE(name), DIMENSION(n) :: var` silently dropped entities
Observed that `vmf_array_input` was declared as `type(vmf_def), dimension(MAX_VMF) :: vmf_array_input` but the transpiler reported it as undefined. Traced to `expectTypeSpec` in `parser.go`: when parsing a derived type spec `TYPE(name)`, the function returned early before calling `parseTypeAttributess`. This meant attributes like `DIMENSION(MAX_VMF)` were not consumed, leaving the parser in a confused state where the entity name was never parsed. The variable was never registered in the symbol table. Fix: parse attributes before returning for the TYPE case.

### Transpiler: `makeArrayInitializer` panicked on derived type arrays
After the parser fix, derived type array declarations reached `makeArrayInitializer`, which called `baseGotype(TYPE, ...)` — an unsupported case that panicked. Observation: the non-allocatable array path and the allocatable array path both unconditionally called `baseGotype`. Fix: check for `TypeToken() == TYPE` in both paths and use `decl.Type.Name` as the element type identifier instead.

### REPL: `InferType` failed on `ComponentAccess` expressions
Assignments like `zdh_mm = vmf_array(idx)%zdh` failed because `repl.Eval` had no case for `*f90.ComponentAccess`. The error surfaced in `transformAssignment` which calls `InferType` on the RHS before switching on the target type. Fix: add a ComponentAccess case to Eval that resolves the base variable name (handling both Identifier and CallExpr bases) and returns that variable's type token as a best-effort approximation. This is imprecise (the actual field type is unknown without struct layout tracking) but allows transpilation to continue.

### Transpiler: `wrapConversion` would call `baseGotype(TYPE, ...)` 
With InferType returning TYPE token for component accesses, `wrapConversion` would hit the conversion path and call `baseGotype(TYPE, ...)`, panicking. Observation: we cannot know the conversion needed since we don't track field types. Fix: short-circuit when either source or target is TYPE — pass the expression through unchanged.

### Transpiler: allocatable derived type also panicked in `transformTypeDeclEntity`
The ALLOCATABLE case in `transformTypeDeclEntity` also unconditionally called `baseGotype`. Seen with `t_gpsatt_store, allocatable`. Same fix pattern as the non-allocatable case.

---

## In Progress

Next unresolved error: `subroutine not found: move_alloc`. MOVE_ALLOC is a Fortran intrinsic that transfers allocation from one allocatable variable to another and leaves the source unallocated. The use site in `gpsatt_store_add` is a standard realloc pattern: allocate larger temp, copy data, deallocate original, move_alloc temp into original. Not yet implemented in the transpiler.