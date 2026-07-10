package main

import (
	"bytes"
	"flag"
	"fmt"
	"go/ast"
	"go/printer"
	"go/token"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	fortran "github.com/soypat/go-fortran"
	f90 "github.com/soypat/go-fortran/ast"
	f90token "github.com/soypat/go-fortran/token"
)

type transpileLogger interface {
	Error(args ...interface{})
	Errorf(format string, args ...interface{})
	Fatal(args ...interface{})
	Fatalf(format string, args ...interface{})
	Helper()
	Logf(format string, args ...interface{})
}

type cliTB struct{}

func (cliTB) Error(args ...interface{})                 { fmt.Fprintln(os.Stderr, args...) }
func (cliTB) Errorf(format string, args ...interface{}) { fmt.Fprintf(os.Stderr, format+"\n", args...) }
func (cliTB) Fatal(args ...interface{})                 { fmt.Fprintln(os.Stderr, args...); os.Exit(1) }
func (cliTB) Fatalf(format string, args ...interface{}) {
	fmt.Fprintf(os.Stderr, format+"\n", args...)
	os.Exit(1)
}
func (cliTB) FailNow()                                { os.Exit(1) }
func (cliTB) Helper()                                 {}
func (cliTB) Log(args ...interface{})                 { fmt.Fprintln(os.Stderr, args...) }
func (cliTB) Logf(format string, args ...interface{}) { fmt.Fprintf(os.Stderr, format+"\n", args...) }

var (
	flagOutput = flag.String("o", "", "write output to file (default: input base name with .go)")
	flagEntry  = flag.String("entry", "", "Fortran program/subroutine/function name to call from generated main")
	flagModule modulePaths
)

type modulePaths []string

func (m *modulePaths) String() string {
	return strings.Join(*m, ",")
}

func (m *modulePaths) Set(value string) error {
	*m = append(*m, value)
	return nil
}

func main() {
	flag.Var(&flagModule, "module", "additional module file to include during transpilation (can be repeated)")
	flag.Parse()

	if flag.NArg() == 0 {
		fmt.Fprintln(os.Stderr, "usage: fortran2go [flags] program.f90 [module1.f90 module2.f90 ...]")
		flag.PrintDefaults()
		os.Exit(1)
	}

	programPath := flag.Arg(0)
	modules := append([]string(nil), flagModule...)
	modules = append(modules, flag.Args()[1:]...)

	output := *flagOutput
	if output == "" {
		output = strings.TrimSuffix(filepath.Base(programPath), filepath.Ext(programPath)) + ".go"
	}

	entry := *flagEntry
	if entry == "" {
		var err error
		entry, err = inferEntryPoint(programPath, modules)
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
	}

	helperTranspile(cliTB{}, entry, output, programPath, modules...)
}

func inferEntryPoint(programPath string, modules []string) (string, error) {
	parser := fortran.Parser90{}
	paths := append([]string{programPath}, modules...)

	// Prefer PROGRAM units for the entrypoint.
	for _, path := range paths {
		units := helperParseUnits(cliTB{}, &parser, path)
		for _, unit := range units {
			if unit.Token == f90token.PROGRAM {
				return unit.UnitName(), nil
			}
		}
	}

	// Fallback to the first SUBROUTINE or FUNCTION if no PROGRAM exists.
	for _, path := range paths {
		units := helperParseUnits(cliTB{}, &parser, path)
		for _, unit := range units {
			if unit.Token == f90token.SUBROUTINE || unit.Token == f90token.FUNCTION {
				return unit.UnitName(), nil
			}
		}
	}

	return "", fmt.Errorf("unable to infer entrypoint: specify -entry with the name of a PROGRAM, SUBROUTINE, or FUNCTION")
}

func helperTranspile(t transpileLogger, programName, dstfile, programPath string, modules ...string) {
	var ps fortran.Parser90
	var units []f90.Unit = helperParseUnits(t, &ps, programPath)
	for _, module := range modules {
		modunits := helperParseUnits(t, &ps, module)
		units = append(units, modunits...)
	}
	var tg fortran.ToGo
	decls := []ast.Decl{
		tg.ImportDecl(),
		&ast.FuncDecl{
			Name: ast.NewIdent("main"),
			Type: &ast.FuncType{
				Params: &ast.FieldList{}, // No parameters for main
			},
			Body: &ast.BlockStmt{
				List: []ast.Stmt{
					&ast.ExprStmt{X: &ast.CallExpr{
						Fun: ast.NewIdent(programName),
					}},
				},
			},
		},
	}
	decls, err := tg.TransformUnits(decls, units...)
	if err != nil {
		t.Fatal(err)
	}
	decls = tg.AppendCommonDecls(decls)
	var dst bytes.Buffer
	helperWriteGoAST(t, &dst, &ast.File{
		Name:  ast.NewIdent("main"),
		Decls: decls,
	})
	os.WriteFile(dstfile, dst.Bytes(), 0777)
	t.Logf("formatting Go source for %s", dstfile)
	helperFormatGoSrc(t, dstfile)
}

func helperFormatGoSrc(t transpileLogger, filePath string) {
	cmd := exec.Command("gofmt", "-w", filePath)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Error(string(out), err)
	}
}

func helperWriteGoAST(t transpileLogger, w *bytes.Buffer, f ast.Node) {
	t.Helper()

	// Use go/printer to write the function
	fset := token.NewFileSet()
	if err := printer.Fprint(w, fset, f); err != nil {
		t.Fatalf("failed to write Go function: %v", err)
	}
	w.WriteString("\n\n")
}

func helperParseUnits(t transpileLogger, ps *fortran.Parser90, programPath string) (units []f90.Unit) {
	file, err := os.Open(programPath)
	if err != nil {
		t.Fatal(err)
	}
	defer file.Close()
	err = ps.Reset(programPath, file)
	if err != nil {
		t.Fatal(err)
	}
	for !ps.IsDone() {
		unit := ps.ParseNextProgramUnit()
		if !unit.IsValid() {
			break
		}
		helperFatalErrors(t, ps, "parsing unit "+unit.UnitName())
		units = append(units, unit)
	}
	helperFatalErrors(t, ps, "parsing module "+programPath)
	return units
}

func helperFatalErrors(t transpileLogger, p *fortran.Parser90, msg string) {
	errs := p.Errors()
	if len(errs) == 0 {
		return
	}
	var sb strings.Builder
	sb.WriteString(msg)
	sb.WriteByte(':')
	for _, e := range errs {
		sb.WriteByte('\n')
		sb.WriteString(e.Error())
	}
	t.Fatal(sb.String())
}
