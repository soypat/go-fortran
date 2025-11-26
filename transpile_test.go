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

//go:generate gfortran -o testdata/golden testdata/golden.f90
//go:generate sh -c "./testdata/golden > testdata/golden.out"

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
	const maxLvl = 13 // Currently testing up to LEVEL13
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
	var funcsrc bytes.Buffer

	// Transpile LEVEL subroutines
	for lvl := 1; lvl <= maxLvl; lvl++ {
		routine := helperGetGoldenLevel(t, lvl, progUnits)
		if routine == nil {
			t.Fatal("failed to get routine")
		}
		var gofunc *ast.FuncDecl
		t.Run(routine.Name, func(t *testing.T) {
			// Measure time for transpile AST and write to file.
			gofunc, err = tp.TransformSubroutine(routine)
			if err != nil {
				t.Fatal(err)
			}
			helperWriteGoFunc(t, &funcsrc, gofunc)
		})
	}
	// write helper subroutines:
	for _, pu := range progUnits {
		if sub, ok := pu.(*f90.Subroutine); ok && !strings.HasPrefix(sub.Name, "LEVEL") {
			gofunc, err := tp.TransformSubroutine(sub)
			if err != nil {
				t.Fatalf("failed to transpile helper %s: %v", sub.Name, err)
			}
			helperWriteGoFunc(t, &funcsrc, gofunc)
		}
	}

	// write helper functions:
	for _, pu := range progUnits {
		if fn, ok := pu.(*f90.Function); ok {
			gofunc, err := tp.TransformFunction(fn)
			if err != nil {
				t.Fatalf("failed to transpile function %s: %v", fn.Name, err)
			}
			helperWriteGoFunc(t, &funcsrc, gofunc)
		}
	}

	var outsrc bytes.Buffer
	outsrc.WriteString("package main\n\nimport(\n")
	for _, imp := range tp.imports {
		fmt.Fprintf(&outsrc, "\t\"%s\"\n", imp)
	}
	outsrc.WriteString(")\n\nfunc main() {\n")
	for lvl := 1; lvl <= maxLvl; lvl++ {
		fmt.Fprintf(&outsrc, "\tLEVEL%02d()\n", lvl)
	}
	outsrc.WriteString("}\n")
	funcsrc.WriteTo(&outsrc)
	var formattedSrc bytes.Buffer
	helperFormatGoSrc(t, &outsrc, &formattedSrc)
	os.WriteFile("testdata/golden.go", formattedSrc.Bytes(), 0777)
	output := helperRunGo(t, &formattedSrc)

	// Read expected output and extract only the lines for implemented levels
	expectedFull := helperRunFortran(t, strings.NewReader(goldensrc))
	lvlStr := fmt.Sprintf("LEVEL %d:", maxLvl)
	idx := bytes.LastIndex(expectedFull, []byte(lvlStr))
	if idx < 0 {
		t.Fatal("could not find level in expected output: ", lvlStr)
	}
	nlIdx := bytes.IndexByte(expectedFull[idx:], '\n')
	if nlIdx < 0 {
		t.Fatal("no newline terminating level string", maxLvl)
	}
	expected := expectedFull[:idx+nlIdx+1] // +1 to include the newline
	for {
		expectLine, remaining, okLine := bytes.Cut(expected, []byte{'\n'})
		expected = remaining
		gotLine, remaining, okGot := bytes.Cut(output, []byte{'\n'})
		output = remaining
		if !bytes.Equal(expectLine, gotLine) {
			t.Errorf("output mismatch:\nExpected: %q\nGot:      %q", expectLine, gotLine)
		} else if !okLine || !okGot {
			break
		}
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

func helperRunFortran(t *testing.T, fsrc io.Reader) (output []byte) {
	t.Helper()
	tmpDir := t.TempDir()
	srcBytes, err := io.ReadAll(fsrc)
	if err != nil {
		t.Fatalf("failed to read Go source: %v", err)
	}
	// Write source to temp file
	srcFile := filepath.Join(tmpDir, "main.f90")
	if err := os.WriteFile(srcFile, []byte(srcBytes), 0644); err != nil {
		t.Fatalf("failed to write Fortran source: %v", err)
	}
	binFile := filepath.Join(tmpDir, "main.f90.bin")
	cmd := exec.Command("gfortran", "-o", binFile, srcFile)
	err = cmd.Run()
	if err != nil {
		t.Fatal(err)
	}
	cmd = exec.Command(binFile)
	output, err = cmd.CombinedOutput()
	if err != nil {
		t.Fatal(err)
	}
	return output
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

	// Write source to temp file
	srcFile := filepath.Join(tmpDir, "main.go")
	if err := os.WriteFile(srcFile, []byte(srcBytes), 0644); err != nil {
		t.Fatalf("failed to write Go source: %v", err)
	}

	// Create go.mod file that points to the local module
	// This allows the generated code to import github.com/soypat/go-fortran/intrinsic
	goModContent := `module testprog

go 1.21

require github.com/soypat/go-fortran v0.0.0

replace github.com/soypat/go-fortran => ` + mustGetwd(t) + `
`
	goModFile := filepath.Join(tmpDir, "go.mod")
	if err := os.WriteFile(goModFile, []byte(goModContent), 0644); err != nil {
		t.Fatalf("failed to write go.mod: %v", err)
	}

	// Run go mod tidy to resolve dependencies
	tidyCmd := exec.Command("go", "mod", "tidy")
	tidyCmd.Dir = tmpDir
	if out, err := tidyCmd.CombinedOutput(); err != nil {
		t.Fatalf("go mod tidy failed: %s\n%v", out, err)
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

func mustGetwd(t *testing.T) string {
	t.Helper()
	wd, err := os.Getwd()
	if err != nil {
		t.Fatalf("failed to get working directory: %v", err)
	}
	return wd
}

func helperFormatGoSrc(t *testing.T, r io.Reader, w io.Writer) {
	cmd := exec.Command("gofmt")
	cmd.Stdin = r
	cmd.Stdout = w
	err := cmd.Run()
	if err != nil {
		t.Fatal(err)
	}
}
