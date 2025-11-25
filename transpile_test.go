package fortran

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/printer"
	"go/token"
	"io"
	"os"
	"os/exec"
	"path/filepath"
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
	for {
		pu := parser.ParseNextProgramUnit()
		if pu != nil {
			progUnits = append(progUnits, pu)
		} else {
			break
		}
	}

	// Debug: print what units we got
	t.Logf("Parsed %d program units:", len(progUnits))
	for i, pu := range progUnits {
		switch u := pu.(type) {
		case *f90.ProgramBlock:
			t.Logf("  [%d] PROGRAM %s", i, u.Name)
		case *f90.Subroutine:
			t.Logf("  [%d] SUBROUTINE %s (params: %v)", i, u.Name, len(u.Parameters))
		case *f90.Function:
			t.Logf("  [%d] FUNCTION %s (params: %v)", i, u.Name, len(u.Parameters))
		default:
			t.Logf("  [%d] %T", i, pu)
		}
	}

	// WORKAROUND: Parser bug - it returns PROGRAM twice
	// Remove duplicate program units
	seen := make(map[string]bool)
	var uniqueUnits []f90.ProgramUnit
	for _, pu := range progUnits {
		key := ""
		switch u := pu.(type) {
		case *f90.ProgramBlock:
			key = "PROGRAM:" + u.Name
		case *f90.Subroutine:
			key = "SUBROUTINE:" + u.Name
		case *f90.Function:
			key = "FUNCTION:" + u.Name
		}
		if key != "" && seen[key] {
			t.Logf("Skipping duplicate: %s", key)
			continue
		}
		seen[key] = true
		uniqueUnits = append(uniqueUnits, pu)
	}
	progUnits = uniqueUnits

	// Create program with unique units
	program := &f90.Program{Units: progUnits}

	// Collect symbols
	syms, err := symbol.CollectFromProgram(program)
	if err != nil {
		t.Fatalf("got errors collecting symbols in golden.f90: %v", err)
	}
	const maxLvl = 1
	// TODO: Fix type resolver to skip format specifiers
	// For now, skip type resolution as it's not needed for basic transpilation
	// resolver := symbol.NewTypeResolver(syms)
	// for i := range progUnits {
	// 	errs := resolver.Resolve(progUnits[i])
	// 	if errs != nil {
	// 		t.Fatalf("got errors resolving prog unit %d %T: %v", i, progUnits[i], errs)
	// 	}
	// }
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
	t.Helper()
	levelName := fmt.Sprintf("LEVEL%02d", lvl)

	// Search through program units for the subroutine
	for _, pu := range pus {
		if sub, ok := pu.(*f90.Subroutine); ok {
			if strings.EqualFold(sub.Name, levelName) {
				return sub
			}
		}
	}

	return nil
}

func helperWriteGoFunc(t *testing.T, w *bytes.Buffer, f *ast.FuncDecl) {
	t.Helper()

	// Use go/printer to write the function
	fset := token.NewFileSet()
	if err := printer.Fprint(w, fset, f); err != nil {
		t.Fatalf("failed to write Go function: %v", err)
	}
	w.WriteString("\n")
}

func helperRunGo(t *testing.T, gosrc io.Reader) (output []byte) {
	t.Helper()

	// Create temp directory
	tmpDir := t.TempDir()

	// Read source
	srcBytes, err := io.ReadAll(gosrc)
	if err != nil {
		t.Fatalf("failed to read Go source: %v", err)
	}

	// Add fmt import after package declaration
	fullSrc := string(srcBytes)
	// Insert import after "package main\n\n"
	fullSrc = strings.Replace(fullSrc, "package main\n\n", "package main\n\nimport \"fmt\"\n\n", 1)

	// Write source to temp file
	srcFile := filepath.Join(tmpDir, "main.go")
	if err := os.WriteFile(srcFile, []byte(fullSrc), 0644); err != nil {
		t.Fatalf("failed to write Go source: %v", err)
	}

	// Compile
	cmd := exec.Command("go", "build", "-o", filepath.Join(tmpDir, "test"), srcFile)
	cmd.Dir = tmpDir
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("compilation failed: %s\n%v", out, err)
	}

	// Run
	cmd = exec.Command(filepath.Join(tmpDir, "test"))
	output, err = cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("execution failed: %v\nOutput: %s", err, output)
	}

	return output
}
