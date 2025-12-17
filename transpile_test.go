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
	f90token "github.com/soypat/go-fortran/token"
)

//go:embed testdata/golden.f90
var goldensrc string

func TestTranspileGolden(t *testing.T) {
	const filename = "testdata/golden.f90"
	const goFilename = "testdata/golden.go"
	var parser Parser90
	err := parser.Reset(filename, strings.NewReader(goldensrc))
	if err != nil {
		t.Fatal(err)
	}
	// units := helperParseUnits(t, &parser, filename)
	helperTranspile(t, "GOLDEN", goFilename, filename)
	expectedFull := helperRunFortran(t, "testdata/golden.f90")
	os.WriteFile("testdata/golden.txt", expectedFull, 0777)
	output := helperRunGoFile(t, goFilename)
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
		t.Fatal("running fortran:", string(output), err)
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

func helperParseUnits(t testing.TB, ps *Parser90, programPath string) (units []f90.Unit) {
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

func helperTranspile(t testing.TB, programName, dstfile, programPath string, modules ...string) {
	var ps Parser90
	var units []f90.Unit = helperParseUnits(t, &ps, programPath)
	for _, module := range modules {
		modunits := helperParseUnits(t, &ps, module)
		units = append(units, modunits...)
	}
	var tg ToGo
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
			if !unit.IsValid() {
				t.Fatal("ParseNextProgramUnit returned invalid unit")
			}
			// Verify no parsing errors
			errs := parser.Errors()
			for _, e := range errs {
				t.Error("parse error:", e)
			}

			// Transpile - this should not panic
			var tg ToGo
			tg.SetSource(tt.name+".f90", strings.NewReader(tt.src))
			_, err = tg.TransformProgram(unit)
			if err != nil {
				t.Errorf("TransformProgram failed: %v", err)
			}
		})
	}
}

// TestComparisonOperatorReturnsLogical verifies that comparison operators (.GT., .LT., etc.)
// return LOGICAL type, not the operand type. This was causing "unpromotable combo INTEGER LOGICAL"
// when expressions like `NTIDE.GT.0.AND..NOT.LRAY` were transpiled - the .GT. was incorrectly
// returning INTEGER instead of LOGICAL.
func TestComparisonOperatorReturnsLogical(t *testing.T) {
	src := `      PROGRAM TEST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      COMMON/TIDALC/LRAY,LOLDST,NXTIDL
      COMMON/CTIDES/NTIDE,NSTADJ
      IF(NTIDE.GT.0.AND..NOT.LRAY) THEN
        PRINT *, 'TEST'
      ENDIF
      END PROGRAM`

	var parser Parser90
	err := parser.Reset("test.f90", strings.NewReader(src))
	if err != nil {
		t.Fatal(err)
	}

	unit := parser.ParseNextProgramUnit()
	if !unit.IsValid() {
		t.Fatal("ParseNextProgramUnit returned invalid")
	}
	data := unit.Data.(*ParserUnitData)
	// Verify LRAY gets LOGICAL type from IMPLICIT LOGICAL(L)
	lray := data.Var("LRAY")
	if lray == nil {
		t.Fatal("LRAY not found")
	}
	if lray.decl == nil || lray.decl.Type == nil || lray.decl.Type.Token != f90token.LOGICAL {
		t.Errorf("LRAY should be LOGICAL via IMPLICIT LOGICAL(L), got %v", lray.decl)
	}

	// Verify NTIDE gets INTEGER type (I-N default)
	ntide := data.Var("NTIDE")
	if ntide == nil {
		t.Fatal("NTIDE not found")
	}
	if ntide.decl == nil || ntide.decl.Type == nil || ntide.decl.Type.Token != f90token.INTEGER {
		t.Errorf("NTIDE should be INTEGER, got %v", ntide.decl)
	}

	// Transpile - this was failing with "unpromotable combo INTEGER LOGICAL"
	// because .GT. returned INTEGER instead of LOGICAL
	var tg ToGo
	tg.SetSource("test.f90", strings.NewReader(src))
	_, err = tg.TransformProgram(unit)
	if err != nil {
		t.Errorf("TransformProgram failed: %v", err)
	}
}

// TestModuleVariableImport verifies that variables from USE'd modules are accessible
// during transpilation. Without this, module variables cause "identifier not found".
func TestModuleVariableImport(t *testing.T) {
	src := `      module testmod
      LOGICAL :: flag = .TRUE.
      end module testmod

      PROGRAM TEST
      use testmod
      if(flag) then
        PRINT *, 'TEST'
      endif
      END PROGRAM`

	var parser Parser90
	err := parser.Reset("test.f90", strings.NewReader(src))
	if err != nil {
		t.Fatal(err)
	}

	// Parse all program units (module + program)
	var units []f90.Unit
	for !parser.IsDone() {
		unit := parser.ParseNextProgramUnit()
		if !unit.IsValid() {
			break
		}
		units = append(units, unit)
	}
	if len(units) != 2 {
		t.Fatalf("Expected 2 program units (module + program), got %d", len(units))
	}

	mod := units[0]
	program := units[1]
	if mod.Token != f90token.MODULE || program.Token != f90token.PROGRAM {
		t.Fatalf("Expected MDOULE followed by ProgramBlock, got %s %s", units[0].Token, units[1].Token)
	}

	// Transpile with module as extern
	var tg ToGo
	tg.SetSource("test.f90", strings.NewReader(src))

	_, err = tg.TransformUnits(nil, units...)
	if err != nil {
		t.Errorf("TransformProgram failed: %v", err)
	}
}

// TestStatementFunction verifies that statement functions are correctly
// detected and expanded during transpilation.
func TestStatementFunction(t *testing.T) {
	src := `      PROGRAM TEST
      INTEGER :: MAPARM
      MAPARM = 10
      INDXNO(M) = MAPARM*(M-1)-(M*(M-1))/2
      X = INDXNO(5)
      PRINT *, X
      END PROGRAM`

	var parser Parser90
	err := parser.Reset("test.f90", strings.NewReader(src))
	if err != nil {
		t.Fatal(err)
	}

	unit := parser.ParseNextProgramUnit()
	if !unit.IsValid() {
		t.Fatal("Expected valid program unit")
	}
	if unit.Token != f90token.PROGRAM {
		t.Fatalf("Expected ProgramBlock, got %s", unit.Token)
	}

	var tg ToGo
	tg.SetSource("test.f90", strings.NewReader(src))
	_, err = tg.TransformProgram(unit)
	if err != nil {
		t.Errorf("TransformProgram failed: %v", err)
	}
}
