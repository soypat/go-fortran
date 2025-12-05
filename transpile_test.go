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
