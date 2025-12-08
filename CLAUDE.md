We are trying to get a transpiler that converts a Fortran 77/90 program to a Go program. It consists of the following Fortran modules

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
- More complex parser bugs shall first be reproduced in testdata/valid_temp.f90. use cmd/fortrangrep to copy parts of a program without comments.
- transpiler bugs are fixed by examining errors and editing transpilation code

You will not fix code that does not have an associated failing test. Test Driven Development is the name of the game. Use fortrangrep with -c flag to extract fortran code without extraneous comments.

After observing failing test make fix as simple and direct as possible.

