We are trying to get a transpiler that converts a Fortran 77/90 program to a Go program with maximum correctness. It consists of the following Go codebase structure:

- lexer.go has Lexer
- parser.go has Parser
- parsing_*_test.go has Parser corpus tests
- repl.go has REPL
- Transpiler:
    - transpile2.go has statement transpiling logic
    - transpile_expr.go has expression transpiling logic
    - transpile_test.go has transpiler tests

To examine bugs and missing features we try to completely transpile the program in geodyn/g2efile.f90. to test we use locally available files local_ignore_test.go by running:

go test . -tags=gdyn

And examine errors. When we find bugs we add a minimal working example to our tests depending on what causes the issue:

- Simple single line parser bugs in test corpus
- More complex parser bugs shall first be reproduced in testdata/valid_temp.f90. use `go run ./cmd/fortrangrep -c` to copy parts of a program without comments.
- Missing transpiler features are added in minimal fashion to ./testdata/golden.f90, if possible in an existing LEVEL subroutine which are numbered subroutines (LEVEL01, LEVEL02 ...) called from main fortran program which when transpiled put transpiler to the test. If no existing LEVEL subroutine fits the new feature a new LEVEL is created and called from the PROGRAM. 

# Implementation Policy
We will not fix code that does not have an associated failing test. Test Driven Development is the name of the game. 

After observing failing test make fix as direct as possible with minimal code change footprint. We make atomic changes, minimizing code change to pass the test while considering room for future transpile feature additions. We always return errors on encountering unimplemented features using the makeErr* set of methods which document position of error in source can be found in debug.go.


Use fortrangrep with -c flag to extract fortran code without extraneous comments.

# Correctness
Do not make parsing less correct. Examples that make parsing less correct:
- Truncating arguments of a fortran function because our code models it with less arguments.
- Ignoring a identifier in a list of parameters because we were unable to find it declared
- Ignoring errors returned by functions to make operation more silent
- Autodeclaring implicitly declared variables
- Emitting a panic in transpiled code instead of returning an error when subroutine/intrinsic not found
- Omitting parts of source code in transpile like alternate return types. We either implement this functionality or return an error when encountered
- Replacing error returns with nil to get code to transpile albeit incorrectly.
- Adding nil checks to methods just because some random file crashes- we need to understand data flow. If something is nil maybe there is a wrong assumption in out code.

All these operations are prohibited. We require our implementation to be the most correct implementation out there. We allow temporarily using these operations to debug but they should immediately be removed once debugging is done. Errors indicating where the issue was found shall be returned and we shall not omit transpiling nor parsing Fortran Code.

We go as far as to not follow the Fortran specification: We treat implicit declarations as valid only on assignment, not on first use. We ask our users to fix their fortran code by declaring these variables beforehand. 

