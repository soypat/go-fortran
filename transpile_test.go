package fortran

import (
	"bytes"
	"go/ast"
	"go/printer"
	"go/token"
	"os"
	"os/exec"
	"strings"
	"testing"

	_ "embed"

	f90 "github.com/soypat/go-fortran/ast"
)

//go:generate gfortran -o testdata/golden testdata/golden.f90
//go:generate sh -c "./testdata/golden > testdata/golden.out"

//go:embed testdata/golden.f90
var goldensrc string

func TestTranspileGolden2(t *testing.T) {
	const filename = "testdata/golden.f90"
	var parser Parser90
	err := parser.Reset(filename, strings.NewReader(goldensrc))
	if err != nil {
		t.Fatal(err)
	}
	program := parser.ParseNextProgramUnit().(*f90.ProgramBlock)
	var tg ToGo
	tg.SetSource(filename, strings.NewReader(goldensrc))
	decls, err := tg.TransformProgram(program)
	if err != nil {
		t.Fatal(err)
	}
	var progSrc bytes.Buffer
	helperWriteGoAST(t, &progSrc, &ast.File{
		Name:  ast.NewIdent("main"),
		Decls: decls, // DO NOT ADD IMPORTS. i.e: STOP statement adds output: then we create an intrinsic.Stop function that does the same.
	})
	const goFile = "testdata/golden.go"
	os.WriteFile(goFile, progSrc.Bytes(), 0777)
	helperFormatGoSrc(t, goFile)
	expectedFull := helperRunFortran(t, "testdata/golden.f90")
	os.WriteFile("testdata/golden.txt", expectedFull, 0777)
	output := helperRunGoFile(t, goFile)
	expected := expectedFull
	misses := 0
	for {
		expectLine, remaining, okLine := bytes.Cut(expected, []byte{'\n'})
		expected = remaining
		gotLine, remaining, _ := bytes.Cut(output, []byte{'\n'})
		output = remaining
		if !bytes.Equal(expectLine, gotLine) {
			misses++
			t.Errorf("output mismatch:\nExpected: %q\nGot:      %q", expectLine, gotLine)
			if misses >= 4 {
				t.Error("too many mismatches, end comparison")
				break // too many errors.
			}
		} else if !okLine {
			break
		}
	}
}

func helperWriteGoAST(t testing.TB, w *bytes.Buffer, f ast.Node) {
	t.Helper()

	// Use go/printer to write the function
	fset := token.NewFileSet()
	if err := printer.Fprint(w, fset, f); err != nil {
		t.Fatalf("failed to write Go function: %v", err)
	}
	w.WriteString("\n\n")
}

func helperRunFortran(t *testing.T, filepath string) (output []byte) {
	binFile := filepath + ".bin"
	cmd := exec.Command("gfortran", "-fcray-pointer", "-o", binFile, filepath)
	errMsg, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatal(string(errMsg), err)
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

func helperFormatGoSrc(t testing.TB, filePath string) {
	cmd := exec.Command("gofmt", "-w", filePath)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Error(string(out), err)
	}
}

// TestDataStmtTranspilation verifies that DATA statements with implicit variables
// can be transpiled without crashing. This captures the fix for nil pointer
// dereference in transformDataStmt when variables are not pre-declared.
func TestDataStmtTranspilation(t *testing.T) {
	tests := []struct {
		name string
		src  string
	}{
		{
			name: "DATA with implicitly typed scalar",
			src: `PROGRAM test
      DATA D40/1.0D40/
      PRINT *, D40
END PROGRAM`,
		},
		{
			name: "DATA with multiple implicit variables",
			src: `PROGRAM test
      DATA HALF/0.5D0/
      DATA NPREPW/0/,NORBVX/0/,NORBVK/0/
      PRINT *, HALF, NPREPW, NORBVX, NORBVK
END PROGRAM`,
		},
		{
			name: "DATA with array subscript",
			src: `PROGRAM test
      INTEGER :: ARR(3)
      DATA ARR(1)/10/
      DATA ARR(2),ARR(3)/20,30/
      PRINT *, ARR(1), ARR(2), ARR(3)
END PROGRAM`,
		},
		{
			name: "DATA with declared and undeclared mix",
			src: `PROGRAM test
      DOUBLE PRECISION :: DECLARED
      DATA DECLARED/1.0D0/
      DATA UNDECLARED/2.0D0/
      PRINT *, DECLARED, UNDECLARED
END PROGRAM`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var parser Parser90
			err := parser.Reset(tt.name+".f90", strings.NewReader(tt.src))
			if err != nil {
				t.Fatalf("Reset failed: %v", err)
			}

			unit := parser.ParseNextProgramUnit()
			if unit == nil {
				t.Fatal("ParseNextProgramUnit returned nil")
			}

			program, ok := unit.(*f90.ProgramBlock)
			if !ok {
				t.Fatalf("Expected *ProgramBlock, got %T", unit)
			}

			// Verify no parsing errors
			errs := parser.Errors()
			for _, e := range errs {
				t.Error("parse error:", e)
			}

			// Transpile - this should not panic
			var tg ToGo
			tg.SetSource(tt.name+".f90", strings.NewReader(tt.src))
			_, err = tg.TransformProgram(program)
			if err != nil {
				t.Errorf("TransformProgram failed: %v", err)
			}
		})
	}
}
