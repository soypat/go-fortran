# go-fortran
[![go.dev reference](https://pkg.go.dev/badge/github.com/soypat/go-fortran)](https://pkg.go.dev/github.com/soypat/go-fortran)
[![Go Report Card](https://goreportcard.com/badge/github.com/soypat/go-fortran)](https://goreportcard.com/report/github.com/soypat/go-fortran)
[![codecov](https://codecov.io/gh/soypat/go-fortran/branch/main/graph/badge.svg)](https://codecov.io/gh/soypat/go-fortran)
[![Go](https://github.com/soypat/go-fortran/actions/workflows/go.yml/badge.svg)](https://github.com/soypat/go-fortran/actions/workflows/go.yml)
[![sourcegraph](https://sourcegraph.com/github.com/soypat/go-fortran/-/badge.svg)](https://sourcegraph.com/github.com/soypat/go-fortran?badge)

Fortran source code parsing utilities for the Go programming language.

## Transpiler
Transpiler currently working for most common modern Fortran features. Work ongoing on supporting FORTRAN77 legacy features like statement functions (inline functions) and other ambiguous and tricky to detect ambiguous statements. File manipulation WIP. 

Feature set demonstration of around ~1000 lines of code shown in [`golden.f90`](testdata/golden.f90) which transpiles to [`golden.go`](testdata/golden.go). Tests pass if output matches byte-to-byte running Go and Fortran programs with gfortran.

See [`TestTranspileGolden`](transpile_Test.go) for example on how to transpile. WIP.

Note that not even [fortls](https://github.com/fortran-lang/fortls), the open source fortran language server parses some of these statements correctly and will show the transpiled Fortran file as having errors even though it compiles correctly.