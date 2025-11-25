package fortran

import (
	"bytes"
	"fmt"
	"go/ast"
	"io"
	"os"
	"strings"
	"testing"

	_ "embed"

	f90 "github.com/soypat/go-fortran/ast"
	"github.com/soypat/go-fortran/symbol"
)

//go:embed testdata/golden.f90
var goldensrc string

func TestTranspileGolden(t *testing.T) {
	var parser Parser90
	err := parser.Reset("testdata/golden.f90", strings.NewReader(goldensrc))
	if err != nil {
		t.Fatal(err)
	}
	var progUnits []f90.ProgramUnit
	var collector symbol.DeclarationCollector
	for {
		pu := parser.ParseNextProgramUnit()
		if pu != nil {
			f90.Walk(&collector, pu)
			progUnits = append(progUnits, pu)
		} else {
			break
		}
	}
	syms, errs := collector.SymbolTable()
	if len(errs) > 0 {
		t.Fatalf("got errors collecting symbols in golden.f90: %v", errs)
	}
	const maxLvl = 1
	resolver := symbol.NewTypeResolver(syms)
	for i := range progUnits {
		errs := resolver.Resolve(progUnits[i])
		if errs != nil {
			t.Fatalf("got errors resolving prog unit %d %T: %v", i, progUnits[i], errs)
		}
	}
	var tp TranspileToGo
	err = tp.Reset(syms)
	if err != nil {
		t.Fatal(err)
	}
	var outsrc bytes.Buffer
	outsrc.WriteString("package main\n\nfunc main() {\n")
	for lvl := 1; lvl <= maxLvl; lvl++ {
		fmt.Fprintf(&outsrc, "\tLEVEL%02d()\n", lvl)
	}
	outsrc.WriteString("}\n")
	for lvl := 1; lvl <= maxLvl; lvl++ {
		routine := helperGetGoldenLevel(t, lvl, progUnits)
		if routine == nil {
			t.Fatal("failed to get routine")
		}
		gofunc, err := tp.TransformSubroutine(routine)
		if err != nil {
			t.Error(err)
		}
		helperWriteGoFunc(t, &outsrc, gofunc)
	}
	output := helperRunGo(t, &outsrc)
	expected, _ := os.ReadFile("testdata/golden.out")
	if bytes.Equal(expected, output) {
		t.Errorf("data mismatch")
	}
}

func helperGetGoldenLevel(t *testing.T, lvl int, pus []f90.ProgramUnit) *f90.Subroutine {
	return nil
}

func helperWriteGoFunc(t *testing.T, w *bytes.Buffer, f *ast.FuncDecl) {

}

func helperRunGo(t *testing.T, gosrc io.Reader) (output []byte) {
	return nil
}
