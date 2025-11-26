package symbol

import (
	"testing"

	"github.com/soypat/go-fortran/ast"
	"github.com/soypat/go-fortran/token"
)

// TestCollectSimpleTypeDeclarations verifies basic variable declarations
func TestCollectSimpleTypeDeclarations(t *testing.T) {
	// Create a simple program with variable declarations
	program := &ast.Program{
		Units: []ast.ProgramUnit{
			&ast.ProgramBlock{
				Name: "test",
				Body: []ast.Statement{
					&ast.TypeDeclaration{
						Type: ast.TypeSpec{Token: token.INTEGER},
						Entities: []ast.DeclEntity{
							{Name: "i"},
							{Name: "j"},
						},
					},
					&ast.TypeDeclaration{
						Type: ast.TypeSpec{Token: token.REAL},
						Entities: []ast.DeclEntity{
							{Name: "x"},
						},
					},
				},
			},
		},
	}

	table, err := CollectFromProgram(program)
	if err != nil {
		t.Fatalf("unexpected errors: %v", err)
	}

	// Should have global scope + program scope
	if table.GlobalScope() == nil {
		t.Fatal("GlobalScope is nil")
	}

	// Find the program scope
	if len(table.GlobalScope().Children()) != 1 {
		t.Fatalf("expected 1 child scope, got %d", len(table.GlobalScope().Children()))
	}

	progScope := table.GlobalScope().Children()[0]

	// Check INTEGER variables
	iSym := progScope.Symbols()["I"]
	if iSym == nil {
		t.Error("symbol 'i' not found")
	} else {
		if iSym.Type().BaseType != "INTEGER" {
			t.Errorf("i: expected INTEGER, got %s", iSym.Type().BaseType)
		}
		if iSym.Kind() != SymVariable {
			t.Errorf("i: expected SymVariable, got %v", iSym.Kind())
		}
	}

	jSym := progScope.Symbols()["J"]
	if jSym == nil {
		t.Error("symbol 'j' not found")
	}

	// Check REAL variable
	xSym := progScope.Symbols()["X"]
	if xSym == nil {
		t.Error("symbol 'x' not found")
	} else {
		if xSym.Type().BaseType != "REAL" {
			t.Errorf("x: expected REAL, got %s", xSym.Type().BaseType)
		}
	}
}

// TestCollectImplicitNone verifies IMPLICIT NONE handling
func TestCollectImplicitNone(t *testing.T) {
	program := &ast.Program{
		Units: []ast.ProgramUnit{
			&ast.ProgramBlock{
				Name: "test",
				Body: []ast.Statement{
					&ast.ImplicitStatement{
						IsNone: true,
					},
				},
			},
		},
	}

	table, err := CollectFromProgram(program)
	if err != nil {
		t.Fatalf("unexpected errors: %v", err)
	}

	progScope := table.GlobalScope().Children()[0]

	if !progScope.Implicit().IsNone {
		t.Error("IMPLICIT NONE not set")
	}

	// All letter types should be empty
	for i := 0; i < 26; i++ {
		if progScope.Implicit().LetterTypes[i] != "" {
			t.Errorf("letter %c: expected empty type, got %s", 'A'+i, progScope.Implicit().LetterTypes[i])
		}
	}
}

// TestCollectImplicitRules verifies custom implicit typing rules
func TestCollectImplicitRules(t *testing.T) {
	program := &ast.Program{
		Units: []ast.ProgramUnit{
			&ast.ProgramBlock{
				Name: "test",
				Body: []ast.Statement{
					&ast.ImplicitStatement{
						IsNone: false,
						Rules: []ast.ImplicitRule{
							{
								Type: ast.TypeSpec{Token: token.REAL},
								LetterRanges: []ast.LetterRange{
									{Start: 'A', End: 'H'},
								},
							},
							{
								Type: ast.TypeSpec{Token: token.INTEGER},
								LetterRanges: []ast.LetterRange{
									{Start: 'I', End: 'N'},
								},
							},
						},
					},
				},
			},
		},
	}

	table, err := CollectFromProgram(program)
	if err != nil {
		t.Fatalf("unexpected errors: %v", err)
	}

	progScope := table.GlobalScope().Children()[0]

	// Check A-H are REAL
	for letter := 'A'; letter <= 'H'; letter++ {
		idx := letter - 'A'
		if progScope.Implicit().LetterTypes[idx] != "REAL" {
			t.Errorf("letter %c: expected REAL, got %s", letter, progScope.Implicit().LetterTypes[idx])
		}
	}

	// Check I-N are INTEGER
	for letter := 'I'; letter <= 'N'; letter++ {
		idx := letter - 'A'
		if progScope.Implicit().LetterTypes[idx] != "INTEGER" {
			t.Errorf("letter %c: expected INTEGER, got %s", letter, progScope.Implicit().LetterTypes[idx])
		}
	}
}

// TestCollectFunction verifies function declaration collection
func TestCollectFunction(t *testing.T) {
	program := &ast.Program{
		Units: []ast.ProgramUnit{
			&ast.Function{
				Name:       "compute",
				Type: ast.TypeSpec{Token: token.REAL},
				Parameters: []ast.Parameter{
					{Name: "x", Type: ast.TypeSpec{Token: token.REAL}},
					{Name: "n", Type: ast.TypeSpec{Token: token.INTEGER}},
				},
				Body: []ast.Statement{
					&ast.TypeDeclaration{
						Type: ast.TypeSpec{Token: token.REAL},
						Entities: []ast.DeclEntity{
							{Name: "result"},
						},
					},
				},
			},
		},
	}

	table, err := CollectFromProgram(program)
	if err != nil {
		t.Fatalf("unexpected errors: %v", err)
	}

	// Function should be defined in global scope
	computeSym := table.GlobalScope().Symbols()["COMPUTE"]
	if computeSym == nil {
		t.Fatal("function 'compute' not found in global scope")
	}

	if computeSym.Kind() != SymFunction {
		t.Errorf("compute: expected SymFunction, got %v", computeSym.Kind())
	}

	if computeSym.Type().BaseType != "REAL" {
		t.Errorf("compute: expected REAL return type, got %s", computeSym.Type().BaseType)
	}

	// Should have one child scope (the function body)
	if len(table.GlobalScope().Children()) != 1 {
		t.Fatalf("expected 1 child scope, got %d", len(table.GlobalScope().Children()))
	}

	funcScope := table.GlobalScope().Children()[0]

	// Function name as result variable should be in function scope
	computeVar := funcScope.Symbols()["COMPUTE"]
	if computeVar == nil {
		t.Error("function result variable 'compute' not found in function scope")
	}

	// Parameters should be in function scope
	xSym := funcScope.Symbols()["X"]
	if xSym == nil {
		t.Error("parameter 'x' not found in function scope")
	} else {
		if xSym.Type().BaseType != "REAL" {
			t.Errorf("x: expected REAL, got %s", xSym.Type().BaseType)
		}
	}

	nSym := funcScope.Symbols()["N"]
	if nSym == nil {
		t.Error("parameter 'n' not found in function scope")
	} else {
		if nSym.Type().BaseType != "INTEGER" {
			t.Errorf("n: expected INTEGER, got %s", nSym.Type().BaseType)
		}
	}

	// Local variable should be in function scope
	resultSym := funcScope.Symbols()["RESULT"]
	if resultSym == nil {
		t.Error("local variable 'result' not found in function scope")
	}
}

// TestCollectSubroutine verifies subroutine declaration collection
func TestCollectSubroutine(t *testing.T) {
	program := &ast.Program{
		Units: []ast.ProgramUnit{
			&ast.Subroutine{
				Name: "process",
				Parameters: []ast.Parameter{
					{Name: "data", Type: ast.TypeSpec{Token: token.REAL}},
					{Name: "size", Type: ast.TypeSpec{Token: token.INTEGER}},
				},
				Body: []ast.Statement{},
			},
		},
	}

	table, err := CollectFromProgram(program)
	if err != nil {
		t.Fatalf("unexpected errors: %v", err)
	}

	// Subroutine should be defined in global scope
	processSym := table.GlobalScope().Symbols()["PROCESS"]
	if processSym == nil {
		t.Fatal("subroutine 'process' not found in global scope")
	}

	if processSym.Kind() != SymSubroutine {
		t.Errorf("process: expected SymSubroutine, got %v", processSym.Kind())
	}

	// Parameters should be in subroutine scope
	subScope := table.GlobalScope().Children()[0]

	dataSym := subScope.Symbols()["DATA"]
	if dataSym == nil {
		t.Error("parameter 'data' not found in subroutine scope")
	}

	sizeSym := subScope.Symbols()["SIZE"]
	if sizeSym == nil {
		t.Error("parameter 'size' not found in subroutine scope")
	}
}

// TestCollectCommonBlock verifies COMMON block handling
func TestCollectCommonBlock(t *testing.T) {
	program := &ast.Program{
		Units: []ast.ProgramUnit{
			&ast.ProgramBlock{
				Name: "test",
				Body: []ast.Statement{
					&ast.CommonStmt{
						BlockName: "shared",
						Variables: []string{"a", "b", "c"},
					},
				},
			},
		},
	}

	table, err := CollectFromProgram(program)
	if err != nil {
		t.Fatalf("unexpected errors: %v", err)
	}

	// Check common block was registered
	cb := table.CommonBlock("SHARED")
	if cb == nil {
		t.Fatal("COMMON block 'shared' not found")
	}

	if cb.Name() != "shared" {
		t.Errorf("common block name: expected 'shared', got %s", cb.Name())
	}

	if len(cb.Variables()) != 3 {
		t.Fatalf("expected 3 variables in common block, got %d", len(cb.Variables()))
	}

	// Variables should be defined with COMMON attribute
	progScope := table.GlobalScope().Children()[0]

	aSym := progScope.Symbols()["A"]
	if aSym == nil {
		t.Error("variable 'a' not found")
	} else {
		hasCommon := false
		for _, attr := range aSym.Attributes() {
			if attr == token.COMMON {
				hasCommon = true
				break
			}
		}
		if !hasCommon {
			t.Error("variable 'a' missing COMMON attribute")
		}
	}
}

// TestCollectExternalStmt verifies EXTERNAL declaration handling
func TestCollectExternalStmt(t *testing.T) {
	program := &ast.Program{
		Units: []ast.ProgramUnit{
			&ast.ProgramBlock{
				Name: "test",
				Body: []ast.Statement{
					&ast.ExternalStmt{
						Names: []string{"myfunc", "mysub"},
					},
				},
			},
		},
	}

	table, err := CollectFromProgram(program)
	if err != nil {
		t.Fatalf("unexpected errors: %v", err)
	}

	progScope := table.GlobalScope().Children()[0]

	// Check external procedures were defined
	myfuncSym := progScope.Symbols()["MYFUNC"]
	if myfuncSym == nil {
		t.Error("external procedure 'myfunc' not found")
	} else {
		if myfuncSym.Kind() != SymExternal {
			t.Errorf("myfunc: expected SymExternal, got %v", myfuncSym.Kind())
		}
	}

	mysubSym := progScope.Symbols()["MYSUB"]
	if mysubSym == nil {
		t.Error("external procedure 'mysub' not found")
	}
}

// TestCollectIntrinsicStmt verifies INTRINSIC declaration handling
func TestCollectIntrinsicStmt(t *testing.T) {
	program := &ast.Program{
		Units: []ast.ProgramUnit{
			&ast.ProgramBlock{
				Name: "test",
				Body: []ast.Statement{
					&ast.IntrinsicStmt{
						Names: []string{"SIN", "COS", "EXP"},
					},
				},
			},
		},
	}

	table, err := CollectFromProgram(program)
	if err != nil {
		t.Fatalf("unexpected errors: %v", err)
	}

	progScope := table.GlobalScope().Children()[0]

	// Check intrinsic functions were defined
	sinSym := progScope.Symbols()["SIN"]
	if sinSym == nil {
		t.Error("intrinsic function 'SIN' not found")
	} else {
		if sinSym.Kind() != SymIntrinsic {
			t.Errorf("SIN: expected SymIntrinsic, got %v", sinSym.Kind())
		}
		if sinSym.Type().BaseType != "REAL" {
			t.Errorf("SIN: expected REAL return type, got %s", sinSym.Type().BaseType)
		}
	}

	cosSym := progScope.Symbols()["COS"]
	if cosSym == nil {
		t.Error("intrinsic function 'COS' not found")
	}

	expSym := progScope.Symbols()["EXP"]
	if expSym == nil {
		t.Error("intrinsic function 'EXP' not found")
	}
}

// TestCollectNestedScopes verifies nested scope handling (Module with CONTAINS)
func TestCollectNestedScopes(t *testing.T) {
	program := &ast.Program{
		Units: []ast.ProgramUnit{
			&ast.Module{
				Name: "mymod",
				Body: []ast.Statement{
					&ast.TypeDeclaration{
						Type: ast.TypeSpec{Token: token.INTEGER},
						Entities: []ast.DeclEntity{
							{Name: "module_var"},
						},
					},
				},
				Contains: []ast.ProgramUnit{
					&ast.Subroutine{
						Name:       "modsub",
						Parameters: []ast.Parameter{{Name: "x", Type: ast.TypeSpec{Token: token.REAL}}},
						Body:       []ast.Statement{},
					},
				},
			},
		},
	}

	table, err := CollectFromProgram(program)
	if err != nil {
		t.Fatalf("unexpected errors: %v", err)
	}

	// Module should be in global scope
	modSym := table.GlobalScope().Symbols()["MYMOD"]
	if modSym == nil {
		t.Fatal("module 'mymod' not found in global scope")
	}

	// Should have module scope as child of global
	if len(table.GlobalScope().Children()) != 1 {
		t.Fatalf("expected 1 child of global scope, got %d", len(table.GlobalScope().Children()))
	}

	modScope := table.GlobalScope().Children()[0]

	// Module variable should be in module scope
	modVarSym := modScope.Symbols()["MODULE_VAR"]
	if modVarSym == nil {
		t.Error("module variable 'module_var' not found in module scope")
	}

	// Module should be registered
	modInfo := table.Module("MYMOD")
	if modInfo == nil {
		t.Error("module 'mymod' not registered in Modules map")
	}

	// Subroutine should be in module scope's children
	if len(modScope.Children()) != 1 {
		t.Fatalf("expected 1 child of module scope, got %d", len(modScope.Children()))
	}

	// Subroutine should be defined in module scope
	subSym := modScope.Symbols()["MODSUB"]
	if subSym == nil {
		t.Error("subroutine 'modsub' not found in module scope")
	}
}

// TestCollectParameterAttribute verifies PARAMETER attribute handling
func TestCollectParameterAttribute(t *testing.T) {
	program := &ast.Program{
		Units: []ast.ProgramUnit{
			&ast.ProgramBlock{
				Name: "test",
				Body: []ast.Statement{
					&ast.TypeDeclaration{
						Type:       ast.TypeSpec{Token: token.REAL},
						Attributes: []token.Token{token.PARAMETER},
						Entities: []ast.DeclEntity{
							{Name: "PI", Initializer: "3.14159"},
						},
					},
				},
			},
		},
	}

	table, err := CollectFromProgram(program)
	if err != nil {
		t.Fatalf("unexpected errors: %v", err)
	}

	progScope := table.GlobalScope().Children()[0]

	piSym := progScope.Symbols()["PI"]
	if piSym == nil {
		t.Fatal("parameter 'PI' not found")
	}

	if piSym.Kind() != SymParameter {
		t.Errorf("PI: expected SymParameter, got %v", piSym.Kind())
	}

	hasParam := false
	for _, attr := range piSym.Attributes() {
		if attr == token.PARAMETER {
			hasParam = true
			break
		}
	}
	if !hasParam {
		t.Error("PI missing PARAMETER attribute")
	}
}
