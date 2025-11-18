# Known Issues

This file documents known bugs and unimplemented features with reproduction tests.
All tests in `known_issues_test.go` are currently skipped but document the expected behavior.

## 1. Expression Parsing After Type Declarations

**Status**: Bug identified, needs fix
**Affected**: CALL statements with arithmetic expressions as arguments
**Test Case**: `known_issues_test.go::TestKnownIssue1_ExpressionAfterTypeDecls`
**Debug Tests**: `claude-test/debug_minimal_test.go::TestWithUnrelatedDecls`

### Description
When a CALL statement contains an arithmetic expression like `n-1` as an argument, parsing fails IF there are any type declarations in the specification part before the CALL.

### Examples

**Works**:
```fortran
SUBROUTINE test()
  CALL fact(n-1, x)
END SUBROUTINE
```

**Fails**:
```fortran
SUBROUTINE test()
  INTEGER :: x
  CALL fact(n-1, result)
END SUBROUTINE
```

Error: `expected expression in argument list`

### Root Cause
- Type declaration parsing (`parseTypeDecl` or `parseSpecStatement`) corrupts parser state
- Leaves parser unable to correctly parse expressions in subsequent executable statements
- Only affects expressions with operators (`n-1`), not simple identifiers (`n`)

### Workaround
Use spaces: `CALL fact(n - 1, result)` may work, or avoid arithmetic in CALL arguments

### Priority
Medium - affects real Fortran code but workarounds exist

### Next Steps
1. Add debug logging to `parseTypeDecl` to track token consumption
2. Check if `skipNewlinesAndComments` is called correctly between phases
3. Verify `isExecutableStatement()` detection works after spec parsing

---

## 2. Continuation Lines

**Status**: Not implemented
**Affected**: Programs with line continuations using `&`
**Test Case**: `known_issues_test.go::TestKnownIssue2_ContinuationLines`
**Failing File**: `testdata/valid_programs.f90`

### Description
Fortran allows long lines to be continued using `&` at the end of a line:
```fortran
INTEGER :: very_long_variable_name &
           _continued_here
```

The lexer/parser doesn't handle continuation lines, which may cause:
- Incorrect parsing of the continued statement
- Failure to detect END of program units after continuation
- Parser left in bad state for subsequent statements

### Error Example
```
testdata/valid_programs.f90:24:1: expected '=' or '=>' for assignment statement
```
Position 24 is `END PROGRAM` which the parser tries to parse as an assignment.

### Priority
Medium - continuation lines are common in real Fortran code

### Next Steps
1. Implement continuation line handling in lexer
2. Join continued lines before parsing
3. Test with complex multi-line continuations

---

## 3. Labeled Statements (Fortran 77)

**Status**: Not implemented
**Affected**: F77-style labeled DO loops, FORMAT statements
**Test Cases**:
- `known_issues_test.go::TestKnownIssue3_LabeledDOLoops`
- `known_issues_test.go::TestKnownIssue3b_FormatStatements`
**Failing File**: `testdata/valid_subroutines.f90`

### Description
Fortran 77 uses statement labels for control flow:

```fortran
      DO 10 i = 1, n
        sum = sum + i
10    CONTINUE
```

And FORMAT statements:
```fortran
      WRITE(*,10) x
10    FORMAT(I5)
```

These features are not implemented:
- Labeled DO loops (`DO label var = start, end`)
- Statement labels (`10 CONTINUE`)
- FORMAT statements
- References to labels in I/O statements

### Error Example
```
testdata/valid_subroutines.f90:76:26: expected statement with label 10
```

### Priority
Low-Medium - F77 code is legacy but still common in scientific computing

### Next Steps
1. Add FORMAT statement AST node
2. Implement labeled DO loop parsing
3. Track label definitions and references
4. Validate label references point to valid statements
