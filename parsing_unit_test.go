package fortran

import (
	"fmt"
	"strings"
	"testing"

	"github.com/soypat/go-fortran/ast"
	"github.com/soypat/go-fortran/token"
)

// TestProgramUnitParsing verifies that program units are parsed correctly,
// including proper variable registration in the parser's symbol table.
func TestProgramUnitParsing(t *testing.T) {
	tests := []struct {
		name     string
		src      string
		validate func(t *testing.T, unit *ast.Unit, data *ParserUnitData)
	}{
		{
			name: "standalone DIMENSION statement registers array variable",
			src: `SUBROUTINE test(ICENTR)
      PARAMETER (NUMGRP=4)
      DIMENSION IPTBEG(NUMGRP)
      DATA IPTBEG/1, 9,11,19/
      IBEG=IPTBEG(ICENTR)
END SUBROUTINE`,
			validate: func(t *testing.T, unit *ast.Unit, data *ParserUnitData) {
				sub := helperWantUnit(t, unit, token.SUBROUTINE, "")

				// CRITICAL: Verify IPTBEG is registered as an array variable
				vi := data.Var("IPTBEG")
				if vi == nil {
					t.Fatalf("BUG: IPTBEG not registered in variable table after DIMENSION statement")
				}

				// Verify it has dimension information
				if vi.Dimensions() == nil {
					t.Errorf("BUG: IPTBEG registered but has no dimension info")
				}

				// Verify VFlagDimension is set
				if !vi.Flags().HasAny(VFlagDimension) {
					t.Errorf("BUG: IPTBEG missing VFlagDimension flag")
				}

				// Find the assignment statement: IBEG=IPTBEG(ICENTR)
				var assignStmt *ast.AssignmentStmt
				for _, stmt := range sub.Body {
					if assign, ok := stmt.(*ast.AssignmentStmt); ok {
						assignStmt = assign
						break
					}
				}
				if assignStmt == nil {
					t.Fatal("Expected assignment statement")
				}

				// CRITICAL: IPTBEG(ICENTR) must be parsed as ArrayRef, not FunctionCall
				arrayRef := helperWantNode[*ast.CallExpr](t, assignStmt.Value, "IPTBEG(ICENTR)")

				// Verify it's the right array
				if arrayRef.Name != "IPTBEG" {
					t.Errorf("Expected ArrayRef name 'IPTBEG', got %q", arrayRef.Name)
				}

				// Verify it has subscripts
				if len(arrayRef.Args) != 1 {
					t.Errorf("Expected 1 subscript, got %d", len(arrayRef.Args))
				}
			},
		},
		{
			name: "declared array produces ArrayRef not FunctionCall",
			src: `SUBROUTINE test()
  INTEGER, DIMENSION(10) :: arr
  INTEGER :: x
  x = arr(5)
  x = UNKNOWN_FUNC(5)
END SUBROUTINE`,
			validate: func(t *testing.T, unit *ast.Unit, data *ParserUnitData) {
				sub := helperWantUnit(t, unit, token.SUBROUTINE, "")

				// Verify arr is registered with VFlagDimension
				vi := data.Var("arr")
				if vi == nil {
					t.Fatalf("arr not registered in variable table")
				}
				if !vi.Flags().HasAny(VFlagDimension) {
					t.Errorf("arr missing VFlagDimension flag")
				}

				// Find the assignment statements
				var assignStmts []*ast.AssignmentStmt
				for _, stmt := range sub.Body {
					if assign, ok := stmt.(*ast.AssignmentStmt); ok {
						assignStmts = append(assignStmts, assign)
					}
				}
				if len(assignStmts) != 2 {
					t.Fatalf("expected 2 assignment statements, got %d", len(assignStmts))
				}

				// First assignment: x = arr(5) - arr should be ArrayRef
				arrRef := helperWantNode[*ast.CallExpr](t, assignStmts[0].Value, "arr(5)")
				if arrRef.Name != "arr" {
					t.Errorf("expected ArrayRef name 'arr', got %s", arrRef.Name)
				}

				// Second assignment: x = UNKNOWN_FUNC(5) - should be FunctionCall
				funcCall := helperWantNode[*ast.CallExpr](t, assignStmts[1].Value, "UNKNOWN_FUNC(5)")
				if funcCall.Name != "UNKNOWN_FUNC" {
					t.Errorf("expected FunctionCall name 'UNKNOWN_FUNC', got %s", funcCall.Name)
				}
			},
		},
		{
			name: "DATA with undeclared scalar variable uses implicit typing",
			src: `PROGRAM test
      DATA D40/1.0D40/
      PRINT *, D40
END PROGRAM`,
			validate: func(t *testing.T, unit *ast.Unit, data *ParserUnitData) {
				// Verify D40 is registered
				vi := data.Var("D40")
				if vi == nil {
					t.Errorf("Variable D40 not registered")
					return
				}
				if vi.decl == nil || vi.decl.Type == nil {
					t.Errorf("Variable D40 has no type declaration")
					return
				}
				gotType := vi.decl.Type.Token.String()
				if !strings.EqualFold(gotType, "REAL") {
					t.Errorf("Variable D40: expected type REAL, got %q", gotType)
				}
			},
		},
		{
			name: "DATA with multiple undeclared variables uses implicit typing",
			src: `PROGRAM test
      DATA HALF/0.5D0/
      DATA NPREPW/0/,NORBVX/0/
      PRINT *, HALF, NPREPW, NORBVX
END PROGRAM`,
			validate: func(t *testing.T, unit *ast.Unit, data *ParserUnitData) {
				expectedVars := []struct {
					name     string
					typeName string
				}{
					{name: "HALF", typeName: "REAL"},      // H -> REAL
					{name: "NPREPW", typeName: "INTEGER"}, // N -> INTEGER (I-N rule)
					{name: "NORBVX", typeName: "INTEGER"}, // N -> INTEGER
				}
				for _, expected := range expectedVars {
					vi := data.Var(expected.name)
					if vi == nil {
						t.Errorf("Variable %q not registered", expected.name)
						continue
					}
					if vi.decl == nil || vi.decl.Type == nil {
						t.Errorf("Variable %q has no type declaration", expected.name)
						continue
					}
					gotType := vi.decl.Type.Token.String()
					if !strings.EqualFold(gotType, expected.typeName) {
						t.Errorf("Variable %q: expected type %q, got %q", expected.name, expected.typeName, gotType)
					}
				}
			},
		},
		{
			name: "DATA with array element registers array with implicit typing",
			src: `PROGRAM test
      DATA I_DEFALT(1) /777/
      PRINT *, I_DEFALT(1)
END PROGRAM`,
			validate: func(t *testing.T, unit *ast.Unit, data *ParserUnitData) {
				vi := data.Var("I_DEFALT")
				if vi == nil {
					t.Errorf("Variable I_DEFALT not registered")
					return
				}
				if vi.decl == nil || vi.decl.Type == nil {
					t.Errorf("Variable I_DEFALT has no type declaration")
					return
				}
				gotType := vi.decl.Type.Token.String()
				if !strings.EqualFold(gotType, "INTEGER") {
					t.Errorf("Variable I_DEFALT: expected type INTEGER, got %q", gotType)
				}
			},
		},
		{
			name: "DATA with mix of declared and undeclared variables",
			src: `PROGRAM test
      DOUBLE PRECISION :: DECLARED
      DATA DECLARED/1.0D0/
      DATA UNDECLARED/2.0D0/
      PRINT *, DECLARED, UNDECLARED
END PROGRAM`,
			validate: func(t *testing.T, unit *ast.Unit, data *ParserUnitData) {
				expectedVars := []struct {
					name     string
					typeName string
				}{
					{name: "DECLARED", typeName: "DOUBLEPRECISION"},
					{name: "UNDECLARED", typeName: "REAL"}, // U -> REAL
				}
				for _, expected := range expectedVars {
					vi := data.Var(expected.name)
					if vi == nil {
						t.Errorf("Variable %q not registered", expected.name)
						continue
					}
					if vi.decl == nil || vi.decl.Type == nil {
						t.Errorf("Variable %q has no type declaration", expected.name)
						continue
					}
					gotType := vi.decl.Type.Token.String()
					if !strings.EqualFold(gotType, expected.typeName) {
						t.Errorf("Variable %q: expected type %q, got %q", expected.name, expected.typeName, gotType)
					}
				}
			},
		},
		{
			name: "END as parameter name in subroutine",
			src: `SUBROUTINE TOBNRY(IN,HDATAS,END,INTYPE,IDAT,MWORDS)
   X = 1
END SUBROUTINE`,
			validate: func(t *testing.T, unit *ast.Unit, data *ParserUnitData) {
				sub := helperWantUnit(t, unit, token.SUBROUTINE, "")
				params := sub.Parameters
				if len(params) != 6 {
					t.Errorf("Expected 6 parameters, got %d", len(params))
				}
				// Check that END is the third parameter
				if len(params) >= 3 && params[2].Name != "END" {
					t.Errorf("Expected third parameter to be 'END', got '%s'", params[2].Name)
				}
				// Verify END is registered as parameter variable
				vi := data.Var("END")
				if vi == nil {
					t.Errorf("Parameter END not registered in variable table")
				} else if !vi.Flags().HasAny(VFlagParameter) {
					t.Errorf("Parameter END missing VFlagParameter flag")
				}
			},
		},
		{
			name: "DATA as parameter name in subroutine",
			src: `SUBROUTINE EXAMPLE(IN,DATA,OUT)
   X = 1
END SUBROUTINE`,
			validate: func(t *testing.T, unit *ast.Unit, data *ParserUnitData) {
				sub := helperWantUnit(t, unit, token.SUBROUTINE, "")
				params := sub.Parameters
				if len(params) != 3 {
					t.Errorf("Expected 3 parameters, got %d", len(params))
				}
				// Check that DATA is the second parameter
				if len(params) >= 2 && params[1].Name != "DATA" {
					t.Errorf("Expected second parameter to be 'DATA', got '%s'", params[1].Name)
				}
				// Verify DATA is registered as parameter variable
				vi := data.Var("DATA")
				if vi == nil {
					t.Errorf("Parameter DATA not registered in variable table")
				} else if !vi.Flags().HasAny(VFlagParameter) {
					t.Errorf("Parameter DATA missing VFlagParameter flag")
				}
			},
		},
		{
			name: "Both END and DATA as parameter names",
			src: `SUBROUTINE TESTFUNC(START,END,DATA,RESULT)
   X = 1
END SUBROUTINE`,
			validate: func(t *testing.T, unit *ast.Unit, data *ParserUnitData) {
				sub := helperWantUnit(t, unit, token.SUBROUTINE, "")
				params := sub.Parameters
				if len(params) != 4 {
					t.Errorf("Expected 4 parameters, got %d", len(params))
				}
				// Check parameter names
				expectedNames := []string{"START", "END", "DATA", "RESULT"}
				for i, expected := range expectedNames {
					if i < len(params) && params[i].Name != expected {
						t.Errorf("Expected parameter %d to be '%s', got '%s'", i, expected, params[i].Name)
					}
				}
			},
		},
		{
			name: "END as parameter in function",
			src: `FUNCTION CALCULATE(BEGIN,END) RESULT(VALUE)
   VALUE = 1
END FUNCTION`,
			validate: func(t *testing.T, unit *ast.Unit, data *ParserUnitData) {
				fn := helperWantUnit(t, unit, token.FUNCTION, "")
				params := fn.Parameters
				if len(params) != 2 {
					t.Errorf("Expected 2 parameters, got %d", len(params))
				}
				// Check that END is the second parameter
				if len(params) >= 2 && params[1].Name != "END" {
					t.Errorf("Expected second parameter to be 'END', got '%s'", params[1].Name)
				}
				// Verify END is registered as parameter variable
				vi := data.Var("END")
				if vi == nil {
					t.Errorf("Parameter END not registered in variable table")
				} else if !vi.Flags().HasAny(VFlagParameter) {
					t.Errorf("Parameter END missing VFlagParameter flag")
				}
			},
		},
		{
			name: "bare FUNCTION sets returnType via function name",
			src: `FUNCTION add(x, y)
	INTEGER :: x, y, add
	add = x + y
END FUNCTION`,
			validate: func(t *testing.T, unit *ast.Unit, data *ParserUnitData) {
				fn := helperWantUnit(t, unit, token.FUNCTION, "")
				if fn.Name != "add" {
					t.Errorf("Expected function name 'add', got %q", fn.Name)
				}
				// CRITICAL: returnType must be set for bare functions
				if data.returnType == nil {
					t.Fatal("BUG: returnType is nil for bare FUNCTION - transpiler will crash")
				}
				// Verify the return type variable is the function name
				if data.returnType.Identifier() != "add" {
					t.Errorf("Expected returnType identifier 'add', got %q", data.returnType.Identifier())
				}
				// Verify it has VFlagReturned
				if !data.returnType.Flags().HasAny(VFlagReturned) {
					t.Errorf("Return variable missing VFlagReturned flag")
				}
			},
		},
		{
			name: "type-prefixed FUNCTION sets returnType",
			src: `INTEGER FUNCTION square(n)
	INTEGER :: n
	square = n * n
END FUNCTION`,
			validate: func(t *testing.T, unit *ast.Unit, data *ParserUnitData) {
				fn := helperWantUnit(t, unit, token.FUNCTION, "")
				if fn.Name != "square" {
					t.Errorf("Expected function name 'square', got %q", fn.Name)
				}
				// returnType must be set for type-prefixed functions
				if data.returnType == nil {
					t.Fatal("BUG: returnType is nil for INTEGER FUNCTION")
				}
				if data.returnType.Identifier() != "square" {
					t.Errorf("Expected returnType identifier 'square', got %q", data.returnType.Identifier())
				}
				// CRITICAL: returnType must have INTEGER type, not default REAL
				retDecl := data.returnType.decl
				if retDecl == nil {
					t.Fatal("BUG: returnType.decl is nil")
				}
				if retDecl.Type == nil {
					t.Fatal("BUG: returnType.decl.Type is nil")
				}
				if retDecl.Type.Token != token.INTEGER {
					t.Errorf("BUG: returnType should be INTEGER, got %v", retDecl.Type.Token)
				}
			},
		},
		{
			name: "FUNCTION with RESULT clause sets returnType",
			src: `FUNCTION compute(x) RESULT(res)
	REAL :: x, res
	res = x * 2.0
END FUNCTION`,
			validate: func(t *testing.T, unit *ast.Unit, data *ParserUnitData) {
				fn := helperWantUnit(t, unit, token.FUNCTION, "")
				if fn.Name != "compute" {
					t.Errorf("Expected function name 'compute', got %q", fn.Name)
				}
				// returnType must be set for RESULT functions
				if data.returnType == nil {
					t.Fatal("BUG: returnType is nil for FUNCTION with RESULT")
				}
				// Return type should be the RESULT variable, not the function name
				if data.returnType.Identifier() != "res" {
					t.Errorf("Expected returnType identifier 'res', got %q", data.returnType.Identifier())
				}
			},
		},
		{
			name: "MODULE CONTAINS FUNCTION with parameter has decl for parameter",
			src: `MODULE test_mod
  IMPLICIT NONE
  CONTAINS
  FUNCTION public_func(x) RESULT(res)
    REAL, INTENT(IN) :: x
    REAL :: res
    res = x * 2.0
  END FUNCTION public_func
END MODULE test_mod`,
			validate: func(t *testing.T, unit *ast.Unit, data *ParserUnitData) {
				mod := helperWantUnit(t, unit, token.MODULE, "")
				if len(mod.Contains) != 1 {
					t.Fatalf("Expected 1 contained unit, got %d", len(mod.Contains))
				}
				fn := helperWantUnit(t, &mod.Contains[0], token.FUNCTION, "Contains[0]")
				fnData, ok := fn.Data.(*ParserUnitData)
				if !ok {
					t.Fatalf("Expected *ParserUnitData for function, got %T", fn.Data)
				}
				// Debug: print all variables in function
				t.Logf("Function %s variables:", fn.Name)
				vars := fnData.AppendVarinfo(nil)
				for i, v := range vars {
					t.Logf("  [%d] %s: decl=%v flags=%v", i, v.Identifier(), v.decl != nil, v.Flags())
				}
				// Check that parameter x has a declaration
				xVar := fnData.Var("x")
				if xVar == nil {
					t.Fatal("BUG: parameter 'x' not found in function's variable table")
				}
				if xVar.decl == nil {
					t.Fatal("BUG: parameter 'x' has nil decl - SetScope will fail")
				}
				t.Logf("x decl: %+v", xVar.decl)
			},
		},
		{
			name: "standalone FUNCTION with parameter has decl for parameter",
			src: `FUNCTION standalone_func(x) RESULT(res)
    REAL, INTENT(IN) :: x
    REAL :: res
    res = x * 2.0
END FUNCTION standalone_func`,
			validate: func(t *testing.T, unit *ast.Unit, data *ParserUnitData) {
				fn := helperWantUnit(t, unit, token.FUNCTION, "")
				// Debug: print all variables in function
				t.Logf("Function %s variables:", fn.Name)
				vars := data.AppendVarinfo(nil)
				for i, v := range vars {
					t.Logf("  [%d] %s: decl=%v flags=%v", i, v.Identifier(), v.decl != nil, v.Flags())
				}
				// Check that parameter x has a declaration
				xVar := data.Var("x")
				if xVar == nil {
					t.Fatal("BUG: parameter 'x' not found in function's variable table")
				}
				if xVar.decl == nil {
					t.Fatal("BUG: parameter 'x' has nil decl - SetScope will fail")
				}
				t.Logf("x decl: %+v", xVar.decl)
			},
		},
		{
			name: "array parameter with inline array spec has VFlagDimension",
			src: `SUBROUTINE MATMUL_TEST(nk, a)
    INTEGER,INTENT(IN):: nk
    COMPLEX,INTENT(IN OUT):: a(nk,nk)
    INTEGER:: i, j
    DO i = 1, nk
        DO j = 1, nk
            a(i,j) = a(i,j) * 2.0
        END DO
    END DO
END SUBROUTINE`,
			validate: func(t *testing.T, unit *ast.Unit, data *ParserUnitData) {
				sub := helperWantUnit(t, unit, token.SUBROUTINE, "")
				if sub.Name != "MATMUL_TEST" {
					t.Errorf("Expected subroutine name 'MATMUL_TEST', got %q", sub.Name)
				}
				// Debug: print all variables
				t.Logf("Subroutine %s variables:", sub.Name)
				vars := data.AppendVarinfo(nil)
				for i, v := range vars {
					t.Logf("  [%d] %s: decl=%v flags=%v dims=%v",
						i, v.Identifier(), v.decl != nil, v.Flags(),
						v.Dimensions() != nil && len(v.Dimensions().Bounds) > 0)
				}
				// CRITICAL: Check that array parameter 'a' has VFlagDimension
				aVar := data.Var("a")
				if aVar == nil {
					t.Fatal("BUG: parameter 'a' not found in variable table")
				}
				if aVar.decl == nil {
					t.Fatal("BUG: parameter 'a' has nil decl")
				}
				// Check ArraySpec is set
				if aVar.decl.ArraySpec == nil {
					t.Error("BUG: parameter 'a' declaration missing ArraySpec for a(nk,nk)")
				} else {
					t.Logf("a ArraySpec bounds: %d", len(aVar.decl.ArraySpec.Bounds))
				}
				// Check VFlagDimension is set
				if !aVar.Flags().HasAny(VFlagDimension) {
					t.Error("BUG: array parameter 'a' missing VFlagDimension flag")
				}
				// Verify parameter flag is also set
				if !aVar.Flags().HasAny(VFlagParameter) {
					t.Error("BUG: parameter 'a' missing VFlagParameter flag")
				}
			},
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
				t.Fatal("ParseNextProgramUnit returned nil")
			}

			helperFatalErrors(t, &parser, "source:\n"+tt.src)

			// Get the ParserUnitData to check registered variables
			data, ok := unit.UnitData().(*ParserUnitData)
			if !ok {
				t.Fatalf("Expected *ParserUnitData, got %T", unit.UnitData())
			}

			tt.validate(t, &unit, data)
		})
	}
}
func helperWantUnit(t testing.TB, unit *ast.Unit, tok token.Token, context string) *ast.Unit {
	t.Helper()
	msg := ""
	if !unit.IsValid() {
		msg += "[invalid unit]"
	}
	if unit.Token != tok {
		msg += fmt.Sprintf("want %s unit, got %s", tok.String(), unit.Token.String())
	}
	if msg != "" {
		msg = context + ": " + msg
	}
	if msg != "" {
		t.Fatal(msg)
	}
	return unit // return same unit, just for renaming convenience.
}
