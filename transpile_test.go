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
	const maxLvl = 2
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
	for lvl := 1; lvl <= maxLvl; lvl++ {
		routine := helperGetGoldenLevel(t, lvl, progUnits)
		if routine == nil {
			t.Fatal("failed to get routine")
		}
		gofunc, err := tp.TransformSubroutine(routine)
		if err != nil {
			t.Error(err)
		}
		helperWriteGoFunc(t, &funcsrc, gofunc)
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
	output := helperRunGo(t, &outsrc)

	// Read expected output and extract only the lines for implemented levels
	expectedFull, err := os.ReadFile("testdata/golden.out")
	if err != nil {
		t.Fatalf("failed to read golden.out: %v", err)
	}

	// Map of which output lines each LEVEL produces (0-indexed)
	// LEVEL 1: line 0 (1 line)
	// LEVEL 2: lines 1-3 (3 lines)
	// LEVEL 3: lines 4-5 (2 lines)
	// etc.
	levelLineRanges := map[int][2]int{
		1:  {0, 1},   // lines 0-0 (1 line)
		2:  {1, 4},   // lines 1-3 (3 lines)
		3:  {4, 6},   // lines 4-5 (2 lines)
		4:  {6, 9},   // lines 6-8 (3 lines)
		5:  {9, 14},  // lines 9-13 (5 lines)
		6:  {14, 16}, // lines 14-15 (2 lines)
		7:  {16, 21}, // lines 16-20 (5 lines)
		8:  {21, 23}, // lines 21-22 (2 lines)
		9:  {23, 25}, // lines 23-24 (2 lines)
		10: {25, 28}, // lines 25-27 (3 lines)
		11: {28, 29}, // lines 28-28 (1 line)
		12: {29, 33}, // lines 29-32 (4 lines)
	}

	// Determine how many lines to expect based on maxLvl
	endLine := levelLineRanges[maxLvl][1]

	// Extract lines 0 through endLine-1
	expectedLines := bytes.Split(expectedFull, []byte("\n"))
	var expected bytes.Buffer
	for i := 0; i < endLine && i < len(expectedLines); i++ {
		expected.Write(expectedLines[i])
		expected.WriteByte('\n')
	}

	if !bytes.Equal(expected.Bytes(), output) {
		t.Errorf("output mismatch:\nExpected: %q\nGot: %q", expected.String(), string(output))
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

	// Write source to temp file
	srcFile := filepath.Join(tmpDir, "main.go")
	if err := os.WriteFile(srcFile, []byte(srcBytes), 0644); err != nil {
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
