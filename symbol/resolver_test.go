package symbol

import (
	"strings"
	"testing"

	"github.com/soypat/go-fortran/ast"
	"github.com/soypat/go-fortran/token"
)

// TestResolveIdentifierImplicitTyping tests that undeclared identifiers get implicit types
func TestResolveIdentifierImplicitTyping(t *testing.T) {
	// Create a simple program with undeclared variables
	program := &ast.Program{
		Units: []ast.ProgramUnit{
			&ast.ProgramBlock{
				Name: "test",
				Body: []ast.Statement{
					// i and x are used but not declared - should get implicit types
					&ast.AssignmentStmt{
						Target: &ast.Identifier{Value: "i"},
						Value:  &ast.IntegerLiteral{Value: 10},
					},
					&ast.AssignmentStmt{
						Target: &ast.Identifier{Value: "x"},
						Value:  &ast.RealLiteral{Value: 3.14},
					},
				},
			},
		},
	}

	// Build symbol table
	table, errs := CollectFromProgram(program)
	if len(errs) > 0 {
		t.Fatalf("CollectFromProgram errors: %v", errs)
	}

	// Resolve types
	resolver := NewTypeResolver(table)
	errs = resolver.Resolve(program)
	if len(errs) > 0 {
		t.Fatalf("Resolve errors: %v", errs)
	}

	// Find program scope
	progScope := table.GlobalScope().Children()[0]

	// Check that 'i' was implicitly typed as INTEGER
	iSym := progScope.Lookup("i")
	if iSym == nil {
		t.Fatal("Symbol 'i' not found after resolution")
	}
	if iSym.Type().BaseType != "INTEGER" {
		t.Errorf("i: expected INTEGER, got %s", iSym.Type().BaseType)
	}
	if !iSym.Flags().HasAny(FlagImplicit) {
		t.Error("i: expected implicit flag to be set")
	}

	// Check that 'x' was implicitly typed as REAL
	xSym := progScope.Lookup("x")
	if xSym == nil {
		t.Fatal("Symbol 'x' not found after resolution")
	}
	if xSym.Type().BaseType != "REAL" {
		t.Errorf("x: expected REAL, got %s", xSym.Type().BaseType)
	}
	if !xSym.Flags().HasAny(FlagImplicit) {
		t.Error("x: expected implicit flag to be set")
	}
}

// TestResolveWithIMPLICITNONE tests that IMPLICIT NONE prevents implicit typing
func TestResolveWithIMPLICITNONE(t *testing.T) {
	program := &ast.Program{
		Units: []ast.ProgramUnit{
			&ast.ProgramBlock{
				Name: "test",
				Body: []ast.Statement{
					&ast.ImplicitStatement{IsNone: true},
					&ast.TypeDeclaration{
						TypeSpec: "INTEGER",
						Entities: []ast.DeclEntity{{Name: "i"}},
					},
					// x is used but not declared - should error with IMPLICIT NONE
					&ast.AssignmentStmt{
						Target: &ast.Identifier{Value: "x"},
						Value:  &ast.IntegerLiteral{Value: 10},
					},
				},
			},
		},
	}

	// Build symbol table
	table, errs := CollectFromProgram(program)
	if len(errs) > 0 {
		t.Fatalf("CollectFromProgram errors: %v", errs)
	}

	// Resolve types
	resolver := NewTypeResolver(table)
	errs = resolver.Resolve(program)

	// Should get an error about 'x' being undeclared
	if len(errs) == 0 {
		t.Fatal("Expected error for undeclared variable with IMPLICIT NONE, got none")
	}

	errorFound := false
	for _, err := range errs {
		if strings.Contains(err.Error(), "x") && strings.Contains(err.Error(), "IMPLICIT NONE") {
			errorFound = true
			break
		}
	}

	if !errorFound {
		t.Errorf("Expected error about 'x' with IMPLICIT NONE, got: %v", errs)
	}
}

// TestResolveWithCustomIMPLICIT tests custom IMPLICIT rules
func TestResolveWithCustomIMPLICIT(t *testing.T) {
	program := &ast.Program{
		Units: []ast.ProgramUnit{
			&ast.ProgramBlock{
				Name: "test",
				Body: []ast.Statement{
					// IMPLICIT REAL (A-Z) - everything is REAL
					&ast.ImplicitStatement{
						IsNone: false,
						Rules: []ast.ImplicitRule{
							{
								Type: "REAL",
								LetterRanges: []ast.LetterRange{
									{Start: 'A', End: 'Z'},
								},
							},
						},
					},
					// i should be REAL, not INTEGER (overrides default)
					&ast.AssignmentStmt{
						Target: &ast.Identifier{Value: "i"},
						Value:  &ast.IntegerLiteral{Value: 10},
					},
				},
			},
		},
	}

	// Build symbol table
	table, errs := CollectFromProgram(program)
	if len(errs) > 0 {
		t.Fatalf("CollectFromProgram errors: %v", errs)
	}

	// Resolve types
	resolver := NewTypeResolver(table)
	errs = resolver.Resolve(program)
	if len(errs) > 0 {
		t.Fatalf("Resolve errors: %v", errs)
	}

	// Find program scope
	progScope := table.GlobalScope().Children()[0]

	// Check that 'i' is REAL (not INTEGER)
	iSym := progScope.Lookup("i")
	if iSym == nil {
		t.Fatal("Symbol 'i' not found after resolution")
	}
	if iSym.Type().BaseType != "REAL" {
		t.Errorf("i: expected REAL (from custom IMPLICIT), got %s", iSym.Type().BaseType)
	}
}

// TestResolveExpressionTypeLiterals tests expression type resolution for literals
func TestResolveExpressionTypeLiterals(t *testing.T) {
	table := NewSymbolTable()
	resolver := NewTypeResolver(table)

	tests := []struct {
		expr     ast.Expression
		wantType string
	}{
		{&ast.IntegerLiteral{Value: 42}, "INTEGER"},
		{&ast.RealLiteral{Value: 3.14}, "REAL"},
		{&ast.StringLiteral{Value: "hello"}, "CHARACTER"},
		{&ast.LogicalLiteral{Value: true}, "LOGICAL"},
	}

	for _, tt := range tests {
		resolvedType := resolver.ResolveExpressionType(tt.expr)
		if resolvedType == nil {
			t.Errorf("ResolveExpressionType(%T): got nil type", tt.expr)
			continue
		}
		if resolvedType.BaseType != tt.wantType {
			t.Errorf("ResolveExpressionType(%T): expected %s, got %s",
				tt.expr, tt.wantType, resolvedType.BaseType)
		}
	}
}

// TestResolveExpressionTypePromotion tests type promotion in arithmetic expressions
func TestResolveExpressionTypePromotion(t *testing.T) {
	tests := []struct {
		name     string
		left     *ResolvedType
		right    *ResolvedType
		wantType string
	}{
		{
			name:     "INTEGER + INTEGER = INTEGER",
			left:     &ResolvedType{BaseType: "INTEGER"},
			right:    &ResolvedType{BaseType: "INTEGER"},
			wantType: "INTEGER",
		},
		{
			name:     "INTEGER + REAL = REAL",
			left:     &ResolvedType{BaseType: "INTEGER"},
			right:    &ResolvedType{BaseType: "REAL"},
			wantType: "REAL",
		},
		{
			name:     "REAL + INTEGER = REAL",
			left:     &ResolvedType{BaseType: "REAL"},
			right:    &ResolvedType{BaseType: "INTEGER"},
			wantType: "REAL",
		},
		{
			name:     "REAL + COMPLEX = COMPLEX",
			left:     &ResolvedType{BaseType: "REAL"},
			right:    &ResolvedType{BaseType: "COMPLEX"},
			wantType: "COMPLEX",
		},
		{
			name:     "REAL + DOUBLE PRECISION = DOUBLE PRECISION",
			left:     &ResolvedType{BaseType: "REAL"},
			right:    &ResolvedType{BaseType: "DOUBLE PRECISION"},
			wantType: "DOUBLE PRECISION",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			promoted := promoteNumericTypes(tt.left, tt.right)
			if promoted.BaseType != tt.wantType {
				t.Errorf("expected %s, got %s", tt.wantType, promoted.BaseType)
			}
		})
	}
}

// TestResolveBinaryExprType tests binary expression type resolution
func TestResolveBinaryExprType(t *testing.T) {
	table := NewSymbolTable()
	resolver := NewTypeResolver(table)

	tests := []struct {
		name     string
		expr     *ast.BinaryExpr
		wantType string
	}{
		{
			name: "INTEGER + INTEGER",
			expr: &ast.BinaryExpr{
				Left:  &ast.IntegerLiteral{Value: 1},
				Op:    token.Plus,
				Right: &ast.IntegerLiteral{Value: 2},
			},
			wantType: "INTEGER",
		},
		{
			name: "INTEGER + REAL",
			expr: &ast.BinaryExpr{
				Left:  &ast.IntegerLiteral{Value: 1},
				Op:    token.Plus,
				Right: &ast.RealLiteral{Value: 2.5},
			},
			wantType: "REAL",
		},
		{
			name: "REAL * INTEGER",
			expr: &ast.BinaryExpr{
				Left:  &ast.RealLiteral{Value: 3.14},
				Op:    token.Asterisk,
				Right: &ast.IntegerLiteral{Value: 2},
			},
			wantType: "REAL",
		},
		{
			name: "INTEGER < REAL",
			expr: &ast.BinaryExpr{
				Left:  &ast.IntegerLiteral{Value: 1},
				Op:    token.LT,
				Right: &ast.RealLiteral{Value: 2.5},
			},
			wantType: "LOGICAL",
		},
		{
			name: "STRING // STRING",
			expr: &ast.BinaryExpr{
				Left:  &ast.StringLiteral{Value: "hello"},
				Op:    token.StringConcat,
				Right: &ast.StringLiteral{Value: "world"},
			},
			wantType: "CHARACTER",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			resolvedType := resolver.resolveBinaryExprType(tt.expr)
			if resolvedType == nil {
				t.Fatal("got nil type")
			}
			if resolvedType.BaseType != tt.wantType {
				t.Errorf("expected %s, got %s", tt.wantType, resolvedType.BaseType)
			}
		})
	}
}

// TestResolveFunctionCallIntrinsic tests resolution of intrinsic function calls
func TestResolveFunctionCallIntrinsic(t *testing.T) {
	program := &ast.Program{
		Units: []ast.ProgramUnit{
			&ast.ProgramBlock{
				Name: "test",
				Body: []ast.Statement{
					&ast.AssignmentStmt{
						Target: &ast.Identifier{Value: "x"},
						Value: &ast.FunctionCall{
							Name: "SIN",
							Args: []ast.Expression{
								&ast.RealLiteral{Value: 1.0},
							},
						},
					},
				},
			},
		},
	}

	// Build symbol table
	table, errs := CollectFromProgram(program)
	if len(errs) > 0 {
		t.Fatalf("CollectFromProgram errors: %v", errs)
	}

	// Resolve types
	resolver := NewTypeResolver(table)
	errs = resolver.Resolve(program)
	if len(errs) > 0 {
		t.Fatalf("Resolve errors: %v", errs)
	}

	// Find program scope
	progScope := table.GlobalScope().Children()[0]

	// Check that SIN was recognized as intrinsic
	sinSym := progScope.Lookup("SIN")
	if sinSym == nil {
		t.Fatal("Symbol 'SIN' not found after resolution")
	}
	if sinSym.Kind() != SymIntrinsic {
		t.Errorf("SIN: expected SymIntrinsic, got %v", sinSym.Kind())
	}
	if sinSym.Type().BaseType != "REAL" {
		t.Errorf("SIN: expected REAL return type, got %s", sinSym.Type().BaseType)
	}

	// Check that x was implicitly typed as REAL
	xSym := progScope.Lookup("x")
	if xSym == nil {
		t.Fatal("Symbol 'x' not found after resolution")
	}
	if xSym.Type().BaseType != "REAL" {
		t.Errorf("x: expected REAL, got %s", xSym.Type().BaseType)
	}
}

// TestResolveArrayRef tests array reference resolution
func TestResolveArrayRef(t *testing.T) {
	program := &ast.Program{
		Units: []ast.ProgramUnit{
			&ast.ProgramBlock{
				Name: "test",
				Body: []ast.Statement{
					&ast.TypeDeclaration{
						TypeSpec: "REAL",
						Entities: []ast.DeclEntity{
							{
								Name: "arr",
								ArraySpec: &ast.ArraySpec{
									Kind: ast.ArraySpecExplicit,
									Bounds: []ast.ArrayBound{
										{Upper: &ast.IntegerLiteral{Value: 10}},
									},
								},
							},
						},
					},
					&ast.AssignmentStmt{
						Target: &ast.ArrayRef{
							Name: "arr",
							Subscripts: []ast.Expression{
								&ast.IntegerLiteral{Value: 5},
							},
						},
						Value: &ast.RealLiteral{Value: 3.14},
					},
				},
			},
		},
	}

	// Build symbol table
	table, errs := CollectFromProgram(program)
	if len(errs) > 0 {
		t.Fatalf("CollectFromProgram errors: %v", errs)
	}

	// Resolve types
	resolver := NewTypeResolver(table)
	errs = resolver.Resolve(program)
	if len(errs) > 0 {
		t.Fatalf("Resolve errors: %v", errs)
	}

	// Find program scope
	progScope := table.GlobalScope().Children()[0]

	// Check that arr exists and is an array
	arrSym := progScope.Lookup("arr")
	if arrSym == nil {
		t.Fatal("Symbol 'arr' not found")
	}
	if arrSym.Type().BaseType != "REAL" {
		t.Errorf("arr: expected REAL, got %s", arrSym.Type().BaseType)
	}
	if arrSym.ArraySpec() == nil {
		t.Error("arr: expected ArraySpec to be non-nil")
	}
}

// TestResolveCompleteProgram tests a complete program with mixed constructs
func TestResolveCompleteProgram(t *testing.T) {
	program := &ast.Program{
		Units: []ast.ProgramUnit{
			&ast.ProgramBlock{
				Name: "test",
				Body: []ast.Statement{
					// Explicit declarations
					&ast.TypeDeclaration{
						TypeSpec: "INTEGER",
						Entities: []ast.DeclEntity{{Name: "n"}},
					},
					&ast.TypeDeclaration{
						TypeSpec: "REAL",
						Entities: []ast.DeclEntity{
							{
								Name: "data",
								ArraySpec: &ast.ArraySpec{
									Kind: ast.ArraySpecExplicit,
									Bounds: []ast.ArrayBound{
										{Upper: &ast.IntegerLiteral{Value: 100}},
									},
								},
							},
						},
					},
					// Implicit variables
					&ast.AssignmentStmt{
						Target: &ast.Identifier{Value: "i"}, // Implicitly INTEGER
						Value:  &ast.IntegerLiteral{Value: 1},
					},
					&ast.AssignmentStmt{
						Target: &ast.Identifier{Value: "sum"}, // Implicitly REAL
						Value:  &ast.RealLiteral{Value: 0.0},
					},
					// Mixed expression
					&ast.AssignmentStmt{
						Target: &ast.Identifier{Value: "result"}, // Implicitly REAL
						Value: &ast.BinaryExpr{
							Left: &ast.FunctionCall{
								Name: "SQRT",
								Args: []ast.Expression{&ast.RealLiteral{Value: 2.0}},
							},
							Op:    token.Plus,
							Right: &ast.IntegerLiteral{Value: 1},
						},
					},
				},
			},
		},
	}

	// Build symbol table
	table, errs := CollectFromProgram(program)
	if len(errs) > 0 {
		t.Fatalf("CollectFromProgram errors: %v", errs)
	}

	// Resolve types
	resolver := NewTypeResolver(table)
	errs = resolver.Resolve(program)
	if len(errs) > 0 {
		t.Fatalf("Resolve errors: %v", errs)
	}

	// Find program scope
	progScope := table.GlobalScope().Children()[0]

	// Verify explicit declarations
	nSym := progScope.Lookup("n")
	if nSym == nil {
		t.Fatal("Symbol 'n' not found")
	}
	if nSym.Type().BaseType != "INTEGER" {
		t.Errorf("n: expected INTEGER, got %s", nSym.Type().BaseType)
	}
	if nSym.Flags().HasAny(FlagImplicit) {
		t.Error("n: should not be marked implicit")
	}

	// Verify implicit declarations
	iSym := progScope.Lookup("i")
	if iSym == nil {
		t.Fatal("Symbol 'i' not found")
	}
	if iSym.Type().BaseType != "INTEGER" {
		t.Errorf("i: expected INTEGER, got %s", iSym.Type().BaseType)
	}
	if !iSym.Flags().HasAny(FlagImplicit) {
		t.Error("i: should be marked implicit")
	}

	sumSym := progScope.Lookup("sum")
	if sumSym == nil {
		t.Fatal("Symbol 'sum' not found")
	}
	if sumSym.Type().BaseType != "REAL" {
		t.Errorf("sum: expected REAL, got %s", sumSym.Type().BaseType)
	}

	// Verify intrinsic function
	sqrtSym := progScope.Lookup("SQRT")
	if sqrtSym == nil {
		t.Fatal("Symbol 'SQRT' not found")
	}
	if sqrtSym.Kind() != SymIntrinsic {
		t.Errorf("SQRT: expected SymIntrinsic, got %v", sqrtSym.Kind())
	}
}
