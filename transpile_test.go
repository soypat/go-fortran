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
	program := parser.ParseNextProgramUnit().(*f90.ProgramBlock)
	progUnits := program.Contains
	// Check for parser errors
	helperPrintErrors(t, &parser)

	// Collect symbols
	syms, err := symbol.CollectFromProgram(&f90.Program{
		Units: progUnits,
		Label: program.Name,
	})
	if err != nil {
		t.Fatalf("got errors collecting symbols in golden.f90: %v", err)
	}

	var tp TranspileToGo
	err = tp.Reset(syms)
	if err != nil {
		t.Fatal(err)
	}
	procedureDecls, err := tp.TransformProgram(program)
	if err != nil {
		t.Fatal(err)
	}

	// Count LEVEL functions to verify completeness
	lvl := 0
	for _, decl := range procedureDecls {
		if fn, ok := decl.(*ast.FuncDecl); ok && strings.HasPrefix(fn.Name.Name, "LEVEL") {
			lvl++
		}
	}
	if lvl < 25 {
		t.Fatalf("expected at least 25 levels, got %d", lvl)
	}
	maxLvl := lvl

	// Write complete file to buffer
	var progSrc bytes.Buffer
	commons := tp.AppendCommonDecls(nil)
	imports := tp.AppendImportSpec(nil)
	t.Log("imports:", len(imports))
	helperWriteGoAST(t, &progSrc, &ast.File{
		Name:    ast.NewIdent("main"),
		Imports: imports,
		Decls:   append(commons, procedureDecls...),
	})

	var formattedSrc bytes.Buffer
	helperFormatGoSrc(t, &progSrc, &formattedSrc)
	const goFile = "testdata/golden.go"
	os.WriteFile(goFile, formattedSrc.Bytes(), 0777)
	output := helperRunGoFile(t, goFile)

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

func helperWriteGoAST(t *testing.T, w *bytes.Buffer, f ast.Node) {
	t.Helper()

	// Use go/printer to write the function
	fset := token.NewFileSet()
	if err := printer.Fprint(w, fset, f); err != nil {
		t.Fatalf("failed to write Go function: %v", err)
	}
	w.WriteString("\n\n")
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

func helperRunGoFile(t *testing.T, pathToFile string) (output []byte) {
	executable := pathToFile + ".bin"
	cmd := exec.Command("go", "build", "-o", executable, pathToFile)
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("compilation failed %s: %s\n%v", pathToFile, out, err)
	}

	// Run
	cmd = exec.Command(executable)
	output, err := cmd.CombinedOutput()
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
	var stderr bytes.Buffer
	var src bytes.Buffer
	src.ReadFrom(r)
	cmd := exec.Command("gofmt")
	cmd.Stdin = bytes.NewBuffer(src.Bytes())
	cmd.Stdout = w
	cmd.Stderr = &stderr
	err := cmd.Run()
	if err != nil {
		t.Error(stderr.String(), err)
		src.WriteTo(w) // Still output data
	}
}
