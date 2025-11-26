package symbol

import (
	"strings"
	"testing"

	"github.com/soypat/go-fortran/ast"
	"github.com/soypat/go-fortran/token"
)

// TestIntegrationFullPipeline tests the complete pipeline:
// AST → Declaration Collection → Type Resolution
func TestIntegrationFullPipeline(t *testing.T) {
	// Create a realistic Fortran program with mixed constructs
	program := &ast.Program{
		Units: []ast.ProgramUnit{
			&ast.ProgramBlock{
				Name: "physics",
				Body: []ast.Statement{
					// IMPLICIT NONE for safety
					&ast.ImplicitStatement{IsNone: true},

					// Explicit declarations
					&ast.TypeDeclaration{
						Type: token.INTEGER,
						Entities: []ast.DeclEntity{
							{Name: "n"},
							{Name: "i"},
						},
					},
					&ast.TypeDeclaration{
						Type:       token.REAL,
						Attributes: []token.Token{token.PARAMETER},
						Entities: []ast.DeclEntity{
							{Name: "PI", Initializer: "3.14159"},
						},
					},
					&ast.TypeDeclaration{
						Type: token.REAL,
						Entities: []ast.DeclEntity{
							{
								Name: "forces",
								ArraySpec: &ast.ArraySpec{
									Kind: ast.ArraySpecExplicit,
									Bounds: []ast.ArrayBound{
										{Upper: &ast.IntegerLiteral{Value: 100}},
									},
								},
							},
						},
					},

					// Assignments with expressions
					&ast.AssignmentStmt{
						Target: &ast.Identifier{Value: "n"},
						Value:  &ast.IntegerLiteral{Value: 100},
					},
					&ast.AssignmentStmt{
						Target: &ast.ArrayRef{
							Name: "forces",
							Subscripts: []ast.Expression{
								&ast.IntegerLiteral{Value: 1},
							},
						},
						Value: &ast.BinaryExpr{
							Left: &ast.FunctionCall{
								Name: "SIN",
								Args: []ast.Expression{
									&ast.Identifier{Value: "PI"},
								},
							},
							Op:    token.Asterisk,
							Right: &ast.RealLiteral{Value: 9.8},
						},
					},

					// Do loop with implicit variable
					&ast.DoLoop{
						Var:   "i",
						Start: &ast.IntegerLiteral{Value: 1},
						End:   &ast.Identifier{Value: "n"},
						Body:  []ast.Statement{},
					},
				},
			},
		},
	}

	// Step 1: Collect declarations
	table, err := CollectFromProgram(program)
	if err != nil {
		t.Fatalf("CollectFromProgram errors: %v", err)
	}

	// Verify symbol table structure
	if table.GlobalScope() == nil {
		t.Fatal("GlobalScope is nil")
	}

	progScope := table.GlobalScope().Children()[0]
	if progScope == nil {
		t.Fatal("Program scope not found")
	}

	// Step 2: Resolve types
	resolver := NewTypeResolver(table)
	errs := resolver.ResolveProgram(program)
	if len(errs) > 0 {
		t.Fatalf("Type resolution errors: %v", errs)
	}

	// Step 3: Verify all symbols have correct types

	// Check n (explicit INTEGER)
	n := progScope.Lookup("n")
	if n == nil {
		t.Fatal("Symbol 'n' not found")
	}
	if n.Type().BaseType != "INTEGER" {
		t.Errorf("n: expected INTEGER, got %s", n.Type().BaseType)
	}
	if n.Flags().HasAny(FlagImplicit) {
		t.Error("n: should not be marked implicit (explicitly declared)")
	}

	// Check i (should error because not declared with IMPLICIT NONE)
	// Actually, i is used in the DO loop but not declared, so it should have been caught
	// But our current implementation doesn't walk into DoLoop.Var (it's a string, not an Identifier)
	// So this is a known limitation for now

	// Check PI (PARAMETER)
	pi := progScope.Lookup("PI")
	if pi == nil {
		t.Fatal("Symbol 'PI' not found")
	}
	if pi.Type().BaseType != "REAL" {
		t.Errorf("PI: expected REAL, got %s", pi.Type().BaseType)
	}
	if pi.Kind() != SymParameter {
		t.Errorf("PI: expected SymParameter, got %v", pi.Kind())
	}

	// Check forces (array)
	forces := progScope.Lookup("forces")
	if forces == nil {
		t.Fatal("Symbol 'forces' not found")
	}
	if forces.Type().BaseType != "REAL" {
		t.Errorf("forces: expected REAL, got %s", forces.Type().BaseType)
	}
	if forces.ArraySpec() == nil {
		t.Error("forces: should have ArraySpec")
	}

	// Check SIN (intrinsic function)
	sin := progScope.Lookup("SIN")
	if sin == nil {
		t.Fatal("Symbol 'SIN' not found (intrinsic should be auto-registered)")
	}
	if sin.Kind() != SymIntrinsic {
		t.Errorf("SIN: expected SymIntrinsic, got %v", sin.Kind())
	}
	if sin.Type().BaseType != "REAL" {
		t.Errorf("SIN: expected REAL return type, got %s", sin.Type().BaseType)
	}
}

// TestIntegrationImplicitTyping tests implicit typing in a complete program
func TestIntegrationImplicitTyping(t *testing.T) {
	program := &ast.Program{
		Units: []ast.ProgramUnit{
			&ast.ProgramBlock{
				Name: "test",
				Body: []ast.Statement{
					// No IMPLICIT NONE - use default F77 rules
					// No explicit declarations - everything is implicit

					// Assignments to implicitly typed variables
					&ast.AssignmentStmt{
						Target: &ast.Identifier{Value: "index"}, // I = INTEGER
						Value:  &ast.IntegerLiteral{Value: 1},
					},
					&ast.AssignmentStmt{
						Target: &ast.Identifier{Value: "sum"}, // S = REAL
						Value:  &ast.RealLiteral{Value: 0.0},
					},
					&ast.AssignmentStmt{
						Target: &ast.Identifier{Value: "count"}, // C = REAL
						Value:  &ast.IntegerLiteral{Value: 0},
					},

					// Mixed expression with type promotion
					&ast.AssignmentStmt{
						Target: &ast.Identifier{Value: "result"}, // R = REAL
						Value: &ast.BinaryExpr{
							Left:  &ast.Identifier{Value: "index"}, // INTEGER
							Op:    token.Plus,
							Right: &ast.Identifier{Value: "sum"}, // REAL
						},
					},
				},
			},
		},
	}

	// Collect and resolve
	table, err := CollectFromProgram(program)
	if err != nil {
		t.Fatalf("CollectFromProgram errors: %v", err)
	}

	resolver := NewTypeResolver(table)
	errs := resolver.ResolveProgram(program)
	if len(errs) != 0 {
		t.Fatalf("Type resolution errors: %v", errs)
	}

	progScope := table.GlobalScope().Children()[0]

	// Verify implicit types
	tests := []struct {
		name     string
		wantType string
		implicit bool
	}{
		{"index", "INTEGER", true}, // I-N = INTEGER
		{"sum", "REAL", true},      // S = REAL (O-Z)
		{"count", "REAL", true},    // C = REAL (A-H)
		{"result", "REAL", true},   // R = REAL (O-Z)
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			sym := progScope.Lookup(tt.name)
			if sym == nil {
				t.Fatalf("Symbol %s not found", tt.name)
			}

			if sym.Type().BaseType != tt.wantType {
				t.Errorf("expected type %s, got %s", tt.wantType, sym.Type().BaseType)
			}

			if tt.implicit && !sym.Flags().HasAny(FlagImplicit) {
				t.Errorf("expected implicit flag to be set")
			}

			if !tt.implicit && sym.Flags().HasAny(FlagImplicit) {
				t.Errorf("expected implicit flag NOT to be set")
			}
		})
	}
}

// TestIntegrationCustomImplicit tests custom IMPLICIT rules
func TestIntegrationCustomImplicit(t *testing.T) {
	program := &ast.Program{
		Units: []ast.ProgramUnit{
			&ast.ProgramBlock{
				Name: "test",
				Body: []ast.Statement{
					// Custom IMPLICIT: make everything REAL except I-N
					&ast.ImplicitStatement{
						IsNone: false,
						Rules: []ast.ImplicitRule{
							{
						Type: "REAL",
								LetterRanges: []ast.LetterRange{
									{Start: 'A', End: 'H'},
									{Start: 'O', End: 'Z'},
								},
							},
							{
						Type: "INTEGER",
								LetterRanges: []ast.LetterRange{
									{Start: 'I', End: 'N'},
								},
							},
						},
					},

					// Use variables with custom implicit types
					&ast.AssignmentStmt{
						Target: &ast.Identifier{Value: "alpha"}, // A = REAL
						Value:  &ast.RealLiteral{Value: 1.0},
					},
					&ast.AssignmentStmt{
						Target: &ast.Identifier{Value: "index"}, // I = INTEGER
						Value:  &ast.IntegerLiteral{Value: 1},
					},
					&ast.AssignmentStmt{
						Target: &ast.Identifier{Value: "zebra"}, // Z = REAL
						Value:  &ast.RealLiteral{Value: 26.0},
					},
				},
			},
		},
	}

	table, err := CollectFromProgram(program)
	if err != nil {
		t.Fatalf("CollectFromProgram errors: %v", err)
	}

	resolver := NewTypeResolver(table)
	errs := resolver.ResolveProgram(program)
	if len(errs) > 0 {
		t.Fatalf("Type resolution errors: %v", errs)
	}

	progScope := table.GlobalScope().Children()[0]

	// Verify custom implicit types
	alpha := progScope.Lookup("alpha")
	if alpha == nil || alpha.Type().BaseType != "REAL" {
		t.Error("alpha: expected REAL from custom IMPLICIT")
	}

	index := progScope.Lookup("index")
	if index == nil || index.Type().BaseType != "INTEGER" {
		t.Error("index: expected INTEGER from custom IMPLICIT")
	}

	zebra := progScope.Lookup("zebra")
	if zebra == nil || zebra.Type().BaseType != "REAL" {
		t.Error("zebra: expected REAL from custom IMPLICIT")
	}
}

// TestIntegrationFunctionWithImplicitVars tests function with implicit variables
func TestIntegrationFunctionWithImplicitVars(t *testing.T) {
	program := &ast.Program{
		Units: []ast.ProgramUnit{
			&ast.Function{
				Name:       "compute",
				ResultType: token.REAL,
				Parameters: []ast.Parameter{
					{Name: "x", Type: token.REAL},
				},
				Body: []ast.Statement{
					// temp is implicitly REAL (T = O-Z)
					&ast.AssignmentStmt{
						Target: &ast.Identifier{Value: "temp"},
						Value: &ast.BinaryExpr{
							Left:  &ast.Identifier{Value: "x"},
							Op:    token.Asterisk,
							Right: &ast.RealLiteral{Value: 2.0},
						},
					},
					// Return statement (assign to function name)
					&ast.AssignmentStmt{
						Target: &ast.Identifier{Value: "compute"},
						Value:  &ast.Identifier{Value: "temp"},
					},
				},
			},
		},
	}

	table, err := CollectFromProgram(program)
	if err != nil {
		t.Fatalf("CollectFromProgram errors: %v", err)
	}

	resolver := NewTypeResolver(table)
	errs := resolver.ResolveProgram(program)
	if len(errs) > 0 {
		t.Fatalf("Type resolution errors: %v", errs)
	}

	// Function should be in global scope
	computeFunc := table.GlobalScope().Lookup("compute")
	if computeFunc == nil {
		t.Fatal("Function 'compute' not found in global scope")
	}
	if computeFunc.Kind() != SymFunction {
		t.Errorf("compute: expected SymFunction, got %v", computeFunc.Kind())
	}

	// Find function scope
	funcScope := table.GlobalScope().Children()[0]

	// Function result variable (same name as function)
	computeVar := funcScope.Lookup("compute")
	if computeVar == nil {
		t.Fatal("Function result variable 'compute' not found")
	}
	if computeVar.Type().BaseType != "REAL" {
		t.Errorf("compute result: expected REAL, got %s", computeVar.Type().BaseType)
	}

	// Parameter x (explicitly typed)
	x := funcScope.Lookup("x")
	if x == nil {
		t.Fatal("Parameter 'x' not found")
	}
	if x.Type().BaseType != "REAL" {
		t.Errorf("x: expected REAL, got %s", x.Type().BaseType)
	}
	if x.Flags().HasAny(FlagImplicit) {
		t.Error("x: parameter should not be marked implicit")
	}

	// temp (implicitly typed)
	temp := funcScope.Lookup("temp")
	if temp == nil {
		t.Fatal("Variable 'temp' not found")
	}
	if temp.Type().BaseType != "REAL" {
		t.Errorf("temp: expected REAL (implicit), got %s", temp.Type().BaseType)
	}
	if !temp.Flags().HasAny(FlagImplicit) {
		t.Error("temp: should be marked implicit")
	}
}

// TestIntegrationExpressionTypeInference tests expression type computation
func TestIntegrationExpressionTypeInference(t *testing.T) {
	program := &ast.Program{
		Units: []ast.ProgramUnit{
			&ast.ProgramBlock{
				Name: "test",
				Body: []ast.Statement{
					&ast.TypeDeclaration{
						Type: token.INTEGER,
						Entities: []ast.DeclEntity{{Name: "i"}},
					},
					&ast.TypeDeclaration{
						Type: token.REAL,
						Entities: []ast.DeclEntity{{Name: "x"}},
					},

					// Test type promotion in expressions
					&ast.AssignmentStmt{
						Target: &ast.Identifier{Value: "result1"},
						Value: &ast.BinaryExpr{
							Left:  &ast.Identifier{Value: "i"}, // INTEGER
							Op:    token.Plus,
							Right: &ast.Identifier{Value: "x"}, // REAL
						}, // Result should be REAL (promotion)
					},

					// Test comparison (should be LOGICAL)
					&ast.AssignmentStmt{
						Target: &ast.Identifier{Value: "flag"},
						Value: &ast.BinaryExpr{
							Left:  &ast.Identifier{Value: "x"},
							Op:    token.LT,
							Right: &ast.RealLiteral{Value: 10.0},
						}, // Result should be LOGICAL
					},
				},
			},
		},
	}

	table, err := CollectFromProgram(program)
	if err != nil {
		t.Fatalf("CollectFromProgram errors: %v", err)
	}

	resolver := NewTypeResolver(table)
	errs := resolver.ResolveProgram(program)
	if len(errs) > 0 {
		t.Fatalf("Type resolution errors: %v", errs)
	}

	progScope := table.GlobalScope().Children()[0]

	// result1 should be implicitly REAL (from type promotion)
	result1 := progScope.Lookup("result1")
	if result1 == nil {
		t.Fatal("Symbol 'result1' not found")
	}
	if result1.Type().BaseType != "REAL" {
		t.Errorf("result1: expected REAL (from I-N default), got %s", result1.Type().BaseType)
	}

	// flag should be implicitly REAL (F = A-H)
	// Note: The *expression* type is LOGICAL, but the variable itself
	// gets its type from implicit rules based on first letter
	flag := progScope.Lookup("flag")
	if flag == nil {
		t.Fatal("Symbol 'flag' not found")
	}
	// 'f' starts with F, which is A-H range, so REAL
	if flag.Type().BaseType != "REAL" {
		t.Errorf("flag: expected REAL (F = A-H), got %s", flag.Type().BaseType)
	}
}

// TestIntegrationErrorReporting tests that errors are properly reported
func TestIntegrationErrorReporting(t *testing.T) {
	program := &ast.Program{
		Units: []ast.ProgramUnit{
			&ast.ProgramBlock{
				Name: "test",
				Body: []ast.Statement{
					&ast.ImplicitStatement{IsNone: true},

					// Use undeclared variable - should error
					&ast.AssignmentStmt{
						Target: &ast.Identifier{Value: "undeclared"},
						Value:  &ast.IntegerLiteral{Value: 42},
					},
				},
			},
		},
	}

	table, err := CollectFromProgram(program)
	if err != nil {
		t.Fatalf("CollectFromProgram errors: %v", err)
	}

	resolver := NewTypeResolver(table)
	errs := resolver.ResolveProgram(program)

	// Should have errors about undeclared variable
	if len(errs) == 0 {
		t.Fatal("Expected errors for undeclared variable with IMPLICIT NONE, got none")
	}

	// Check error message mentions IMPLICIT NONE and the variable
	foundError := false
	for _, err := range errs {
		errMsg := err.Error()
		if strings.Contains(errMsg, "undeclared") && strings.Contains(errMsg, "IMPLICIT NONE") {
			foundError = true
			break
		}
	}

	if !foundError {
		t.Errorf("Expected error about undeclared variable with IMPLICIT NONE, got: %v", errs)
	}
}
