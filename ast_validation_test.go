package fortran

import (
	"reflect"
	"strings"
	"testing"

	"github.com/soypat/go-fortran/ast"
	"github.com/soypat/go-fortran/token"
)

// TestParameterTypeInfo verifies that parameter type information is correctly
// captured in the AST for subroutines and functions.
func TestParameterTypeInfo(t *testing.T) {
	tests := []struct {
		name     string
		src      string
		unitType string // "subroutine" or "function"
		expected []struct {
			name   string
			typ    string
			intent ast.IntentType
		}
	}{
		{
			name: "subroutine with typed parameters",
			src: `
SUBROUTINE test(n, arr, flag)
  INTEGER, INTENT(IN) :: n
  REAL, INTENT(INOUT) :: arr
  LOGICAL, INTENT(OUT) :: flag
END SUBROUTINE test
`,
			unitType: "subroutine",
			expected: []struct {
				name   string
				typ    string
				intent ast.IntentType
			}{
				{name: "n", typ: "INTEGER", intent: ast.IntentIn},
				{name: "arr", typ: "REAL", intent: ast.IntentInOut},
				{name: "flag", typ: "LOGICAL", intent: ast.IntentOut},
			},
		},
		{
			name: "function with typed parameters",
			src: `
FUNCTION compute(x, y) RESULT(z)
  REAL, INTENT(IN) :: x, y
  REAL :: z
  z = x + y
END FUNCTION compute
`,
			unitType: "function",
			expected: []struct {
				name   string
				typ    string
				intent ast.IntentType
			}{
				{name: "x", typ: "REAL", intent: ast.IntentIn},
				{name: "y", typ: "REAL", intent: ast.IntentIn},
			},
		},
		{
			name: "subroutine with mixed parameter types",
			src: `
SUBROUTINE process(input, output, count, status)
  REAL, INTENT(IN) :: input
  REAL, INTENT(OUT) :: output
  INTEGER, INTENT(IN) :: count
  INTEGER, INTENT(OUT) :: status
END SUBROUTINE process
`,
			unitType: "subroutine",
			expected: []struct {
				name   string
				typ    string
				intent ast.IntentType
			}{
				{name: "input", typ: "REAL", intent: ast.IntentIn},
				{name: "output", typ: "REAL", intent: ast.IntentOut},
				{name: "count", typ: "INTEGER", intent: ast.IntentIn},
				{name: "status", typ: "INTEGER", intent: ast.IntentOut},
			},
		},
		{
			name: "subroutine with parameters without intent",
			src: `
SUBROUTINE old_style(a, b)
  REAL :: a, b
END SUBROUTINE old_style
`,
			unitType: "subroutine",
			expected: []struct {
				name   string
				typ    string
				intent ast.IntentType
			}{
				{name: "a", typ: "REAL"},
				{name: "b", typ: "REAL"},
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
			if unit == nil {
				t.Fatal("ParseNextProgramUnit returned nil")
			}

			if len(parser.Errors()) > 0 {
				t.Fatalf("Parse errors: %v", parser.Errors())
			}

			var params []ast.Parameter
			switch tt.unitType {
			case "subroutine":
				sub, ok := unit.(*ast.Subroutine)
				if !ok {
					t.Fatalf("Expected *ast.Subroutine, got %T", unit)
				}
				params = sub.Parameters
			case "function":
				fn, ok := unit.(*ast.Function)
				if !ok {
					t.Fatalf("Expected *ast.Function, got %T", unit)
				}
				params = fn.Parameters
			default:
				t.Fatalf("Unknown unit type: %s", tt.unitType)
			}

			if len(params) != len(tt.expected) {
				t.Fatalf("Expected %d parameters, got %d", len(tt.expected), len(params))
			}

			for i, exp := range tt.expected {
				param := params[i]

				if param.Name != exp.name {
					t.Errorf("Parameter %d: expected name %q, got %q", i, exp.name, param.Name)
				}

				if param.Type.Token.String() != exp.typ {
					t.Errorf("Parameter %d (%s): expected type %q, got %q", i, param.Name, exp.typ, param.Type.Token.String())
				}

				if param.Intent != exp.intent {
					t.Errorf("Parameter %d (%s): expected intent %v, got %v", i, param.Name, exp.intent, param.Intent)
				}
			}
		})
	}
}

// TestVariableDeclarations verifies that variable initialization and CHARACTER length
// specifications are correctly captured in the AST.
func TestVariableDeclarations(t *testing.T) {
	tests := []struct {
		name     string
		src      string
		expected []struct {
			name        string
			typ         string
			charLen     string
			initializer string
		}
	}{
		{
			name: "simple integer initialization",
			src: `
PROGRAM test
  INTEGER :: n = 42
END PROGRAM test
`,
			expected: []struct {
				name        string
				typ         string
				charLen     string
				initializer string
			}{
				{name: "n", typ: "INTEGER", initializer: "42"},
			},
		},
		{
			name: "array initialization",
			src: `
PROGRAM test
  INTEGER :: vec(3) = (/ 1, 2, 3 /)
END PROGRAM test
`,
			expected: []struct {
				name        string
				typ         string
				charLen     string
				initializer string
			}{
				{name: "vec", typ: "INTEGER", initializer: "(/1 , 2 , 3/)"}, // Captures array constructor delimiters and commas
			},
		},
		{
			name: "CHARACTER with LEN= form",
			src: `
PROGRAM test
  CHARACTER(LEN=80) :: line
END PROGRAM test
`,
			expected: []struct {
				name        string
				typ         string
				charLen     string
				initializer string
			}{
				{name: "line", typ: "CHARACTER", charLen: "80"},
			},
		},
		{
			name: "CHARACTER with parentheses form",
			src: `
PROGRAM test
  CHARACTER(80) :: line
END PROGRAM test
`,
			expected: []struct {
				name        string
				typ         string
				charLen     string
				initializer string
			}{
				{name: "line", typ: "CHARACTER", charLen: "80"},
			},
		},
		// TODO: CHARACTER(*) form not yet working correctly
		{
			name: "CHARACTER*n form",
			src: `
PROGRAM test
  CHARACTER*80 :: str
END PROGRAM test
`,
			expected: []struct {
				name        string
				typ         string
				charLen     string
				initializer string
			}{
				{name: "str", typ: "CHARACTER", charLen: "80"},
			},
		},
		{
			name: "pointer initialization with =>",
			src: `
PROGRAM test
  INTEGER, POINTER :: ptr => null()
END PROGRAM test
`,
			expected: []struct {
				name        string
				typ         string
				charLen     string
				initializer string
			}{
				{name: "ptr", typ: "INTEGER", initializer: "=>  null()"}, // Captures function call parentheses
			},
		},
		{
			name: "multiple initializations",
			src: `
PROGRAM test
  INTEGER :: a = 1, b = 2, c = 3
END PROGRAM test
`,
			expected: []struct {
				name        string
				typ         string
				charLen     string
				initializer string
			}{
				{name: "a", typ: "INTEGER", initializer: "1"},
				{name: "b", typ: "INTEGER", initializer: "2"},
				{name: "c", typ: "INTEGER", initializer: "3"},
			},
		},
		{
			name: "CHARACTER parameter with length",
			src: `
SUBROUTINE test(name)
  CHARACTER(LEN=20), INTENT(IN) :: name
END SUBROUTINE test
`,
			expected: []struct {
				name        string
				typ         string
				charLen     string
				initializer string
			}{
				{name: "name", typ: "CHARACTER", charLen: "20"},
			},
		},
		{
			name: "REAL initialization with expression",
			src: `
PROGRAM test
  REAL :: pi = 3.14159
END PROGRAM test
`,
			expected: []struct {
				name        string
				typ         string
				charLen     string
				initializer string
			}{
				{name: "pi", typ: "REAL", initializer: "3.14159"},
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
			if unit == nil {
				t.Fatal("ParseNextProgramUnit returned nil")
			}

			helperFatalErrors(t, &parser, "variable decl")

			// Extract entities from type declarations
			var entities []ast.DeclEntity
			var params []ast.Parameter

			switch u := unit.(type) {
			case *ast.ProgramBlock:
				for _, stmt := range u.Body {
					if typeDecl, ok := stmt.(*ast.TypeDeclaration); ok {
						entities = append(entities, typeDecl.Entities...)
					}
				}
			case *ast.Subroutine:
				params = u.Parameters
				for _, stmt := range u.Body {
					if typeDecl, ok := stmt.(*ast.TypeDeclaration); ok {
						entities = append(entities, typeDecl.Entities...)
					}
				}
			default:
				t.Fatalf("Unexpected unit type: %T", unit)
			}

			// Check expected entities
			for i, exp := range tt.expected {
				var found bool
				var actualCharLen, actualInit, actualType string
				var actualCharLenExpr ast.Expression

				// Check parameters first
				for _, param := range params {
					if param.Name == exp.name {
						found = true
						actualType = param.Type.Token.String()
						actualCharLenExpr = param.Type.KindOrLen
						// Convert Expression to string for comparison if present
						if actualCharLenExpr != nil {
							actualCharLen = exprToString(actualCharLenExpr)
						}
						// Parameters don't have initializers (they're arguments)
						break
					}
				}

				// Check entities
				if !found {
					for _, entity := range entities {
						if entity.Name == exp.name {
							found = true
							actualInit = string(entity.Init.AppendString(nil))
							actualCharLenExpr = entity.Type.KindOrLen
							// Convert Expression to string for comparison if present
							if actualCharLenExpr != nil {
								actualCharLen = exprToString(actualCharLenExpr)
							}
							// Get type from the TypeDeclaration that contains this entity
							// We need to find which TypeDeclaration this entity belongs to
							switch u := unit.(type) {
							case *ast.ProgramBlock:
								for _, stmt := range u.Body {
									if typeDecl, ok := stmt.(*ast.TypeDeclaration); ok {
										for _, e := range typeDecl.Entities {
											if e.Name == entity.Name {
												actualType = typeDecl.Type.Token.String()
												break
											}
										}
									}
								}
							case *ast.Subroutine:
								for _, stmt := range u.Body {
									if typeDecl, ok := stmt.(*ast.TypeDeclaration); ok {
										for _, e := range typeDecl.Entities {
											if e.Name == entity.Name {
												actualType = typeDecl.Type.Token.String()
												break
											}
										}
									}
								}
							}
							break
						}
					}
				}

				if !found {
					t.Errorf("Expected entity %d: %q not found", i, exp.name)
					continue
				}

				if actualType != exp.typ {
					t.Errorf("Entity %q: expected type %q, got %q", exp.name, exp.typ, actualType)
				}

				if actualCharLen != exp.charLen {
					t.Errorf("Entity %q: expected charLen %q, got %q", exp.name, exp.charLen, actualCharLen)
				}

				if actualInit != exp.initializer {
					t.Errorf("Entity %q: expected initializer %q, got %q", exp.name, exp.initializer, actualInit)
				}
			}
		})
	}
}

// TestParameterAttributesCapture verifies that parameter attributes beyond INTENT
// are correctly captured.
func TestParameterAttributesCapture(t *testing.T) {
	src := `
SUBROUTINE test(arr, opt)
  REAL, INTENT(IN), DIMENSION(:) :: arr
  INTEGER, INTENT(IN), OPTIONAL :: opt
END SUBROUTINE test
`

	var parser Parser90
	err := parser.Reset("test.f90", strings.NewReader(src))
	if err != nil {
		t.Fatalf("Reset failed: %v", err)
	}

	unit := parser.ParseNextProgramUnit()
	if unit == nil {
		t.Fatal("ParseNextProgramUnit returned nil")
	}

	if len(parser.Errors()) > 0 {
		t.Fatalf("Parse errors: %v", parser.Errors())
	}

	sub, ok := unit.(*ast.Subroutine)
	if !ok {
		t.Fatalf("Expected *ast.Subroutine, got %T", unit)
	}

	// Check that arr has DIMENSION attribute
	if len(sub.Parameters) < 1 {
		t.Fatal("Expected at least 1 parameter")
	}

	arrParam := sub.Parameters[0]
	if arrParam.Name != "arr" {
		t.Fatalf("Expected first parameter to be 'arr', got %q", arrParam.Name)
	}

	// Check for DIMENSION attribute in the attributes list
	hasDimension := false
	for _, attr := range arrParam.Attributes {
		if attr.Token == token.DIMENSION {
			hasDimension = true
			break
		}
	}
	if !hasDimension {
		t.Errorf("Parameter 'arr' should have DIMENSION attribute, got: %v", arrParam.Attributes)
	}

	// Check that opt has OPTIONAL attribute
	if len(sub.Parameters) < 2 {
		t.Fatal("Expected at least 2 parameters")
	}

	optParam := sub.Parameters[1]
	if optParam.Name != "opt" {
		t.Fatalf("Expected second parameter to be 'opt', got %q", optParam.Name)
	}

	hasOptional := false
	for _, attr := range optParam.Attributes {
		if attr.Token == token.OPTIONAL {
			hasOptional = true
			break
		}
	}
	if !hasOptional {
		t.Errorf("Parameter 'opt' should have OPTIONAL attribute, got: %v", optParam.Attributes)
	}
}

// exprToString converts an Expression to a string for testing purposes
// Returns empty string for nil expressions
func exprToString(expr ast.Expression) string {
	if expr == nil {
		return ""
	}
	switch e := expr.(type) {
	case *ast.Identifier:
		return e.Value
	case *ast.IntegerLiteral:
		return e.Raw
	case *ast.RealLiteral:
		return e.Raw
	case *ast.StringLiteral:
		return e.Value
	case *ast.LogicalLiteral:
		if e.Value {
			return ".TRUE."
		}
		return ".FALSE."
	case *ast.BinaryExpr:
		return exprToString(e.Left) + " " + e.Op.String() + " " + exprToString(e.Right)
	// Add more cases for other expression types as needed
	case *ast.UnaryExpr:
		return e.Op.String() + exprToString(e.Operand)

	default:
		return ""
	}
}

// TestArraySpecifications verifies that array dimension specifications are correctly
// captured in the AST for both DIMENSION attributes and entity declarators.
func TestArraySpecifications(t *testing.T) {
	tests := []struct {
		name     string
		src      string
		unitType string
		expected []struct {
			name      string
			arrayKind ast.ArraySpecKind
			numDims   int
			bounds    []struct{ lower, upper string }
		}
	}{
		{
			name: "explicit shape 1D array",
			src: `
SUBROUTINE test(arr)
  REAL, DIMENSION(10) :: arr
END SUBROUTINE test
`,
			unitType: "subroutine",
			expected: []struct {
				name      string
				arrayKind ast.ArraySpecKind
				numDims   int
				bounds    []struct{ lower, upper string }
			}{
				{
					name:      "arr",
					arrayKind: ast.ArraySpecExplicit,
					numDims:   1,
					bounds:    []struct{ lower, upper string }{{lower: "", upper: "10"}},
				},
			},
		},
		{
			name: "explicit shape 2D array with bounds",
			src: `
SUBROUTINE test(matrix)
  REAL, DIMENSION(1:10, 1:20) :: matrix
END SUBROUTINE test
`,
			unitType: "subroutine",
			expected: []struct {
				name      string
				arrayKind ast.ArraySpecKind
				numDims   int
				bounds    []struct{ lower, upper string }
			}{
				{
					name:      "matrix",
					arrayKind: ast.ArraySpecExplicit,
					numDims:   2,
					bounds: []struct{ lower, upper string }{
						{lower: "1", upper: "10"},
						{lower: "1", upper: "20"},
					},
				},
			},
		},
		{
			name: "assumed shape array",
			src: `
SUBROUTINE test(arr)
  REAL, DIMENSION(:,:) :: arr
END SUBROUTINE test
`,
			unitType: "subroutine",
			expected: []struct {
				name      string
				arrayKind ast.ArraySpecKind
				numDims   int
				bounds    []struct{ lower, upper string }
			}{
				{
					name:      "arr",
					arrayKind: ast.ArraySpecAssumed,
					numDims:   2,
					bounds: []struct{ lower, upper string }{
						{lower: "", upper: ""},
						{lower: "", upper: ""},
					},
				},
			},
		},
		{
			name: "assumed size array (F77 style)",
			src: `
SUBROUTINE test(arr)
  REAL, DIMENSION(*) :: arr
END SUBROUTINE test
`,
			unitType: "subroutine",
			expected: []struct {
				name      string
				arrayKind ast.ArraySpecKind
				numDims   int
				bounds    []struct{ lower, upper string }
			}{
				{
					name:      "arr",
					arrayKind: ast.ArraySpecAssumedSize,
					numDims:   1,
					bounds:    []struct{ lower, upper string }{{lower: "", upper: "*"}},
				},
			},
		},
		{
			name: "array with entity declarator (no DIMENSION attribute)",
			src: `
SUBROUTINE test()
  REAL :: arr(5, 10)
END SUBROUTINE test
`,
			unitType: "subroutine",
			expected: []struct {
				name      string
				arrayKind ast.ArraySpecKind
				numDims   int
				bounds    []struct{ lower, upper string }
			}{
				{
					name:      "arr",
					arrayKind: ast.ArraySpecExplicit,
					numDims:   2,
					bounds: []struct{ lower, upper string }{
						{lower: "", upper: "5"},
						{lower: "", upper: "10"},
					},
				},
			},
		},
		{
			name: "multiple arrays with different specs",
			src: `
PROGRAM test
  REAL, DIMENSION(10) :: vec
  REAL, DIMENSION(:,:) :: matrix
  REAL :: arr(3,3)
END PROGRAM test
`,
			unitType: "program",
			expected: []struct {
				name      string
				arrayKind ast.ArraySpecKind
				numDims   int
				bounds    []struct{ lower, upper string }
			}{
				{
					name:      "vec",
					arrayKind: ast.ArraySpecExplicit,
					numDims:   1,
					bounds:    []struct{ lower, upper string }{{lower: "", upper: "10"}},
				},
				{
					name:      "matrix",
					arrayKind: ast.ArraySpecAssumed,
					numDims:   2,
					bounds: []struct{ lower, upper string }{
						{lower: "", upper: ""},
						{lower: "", upper: ""},
					},
				},
				{
					name:      "arr",
					arrayKind: ast.ArraySpecExplicit,
					numDims:   2,
					bounds: []struct{ lower, upper string }{
						{lower: "", upper: "3"},
						{lower: "", upper: "3"},
					},
				},
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
			if unit == nil {
				t.Fatal("ParseNextProgramUnit returned nil")
			}

			if len(parser.Errors()) > 0 {
				t.Fatalf("Parse errors: %v", parser.Errors())
			}

			// Extract entities to check from body statements
			var entities []ast.DeclEntity
			var params []ast.Parameter

			switch tt.unitType {
			case "subroutine":
				sub, ok := unit.(*ast.Subroutine)
				if !ok {
					t.Fatalf("Expected *ast.Subroutine, got %T", unit)
				}
				params = sub.Parameters
				// Extract entities from type declarations in body
				for _, stmt := range sub.Body {
					if typeDecl, ok := stmt.(*ast.TypeDeclaration); ok {
						entities = append(entities, typeDecl.Entities...)
					}
				}
			case "program":
				prog, ok := unit.(*ast.ProgramBlock)
				if !ok {
					t.Fatalf("Expected *ast.ProgramBlock, got %T", unit)
				}
				// Extract entities from type declarations in body
				for _, stmt := range prog.Body {
					if typeDecl, ok := stmt.(*ast.TypeDeclaration); ok {
						entities = append(entities, typeDecl.Entities...)
					}
				}
			default:
				t.Fatalf("Unknown unit type: %s", tt.unitType)
			}

			// Check parameters first
			for i, exp := range tt.expected {
				var arraySpec *ast.ArraySpec

				// Look for the entity in parameters
				found := false
				for _, param := range params {
					if param.Name == exp.name {
						arraySpec = param.ArraySpec
						found = true
						break
					}
				}

				// If not found in parameters, look in entities
				if !found {
					for _, entity := range entities {
						if entity.Name == exp.name {
							arraySpec = entity.ArraySpec
							found = true
							break
						}
					}
				}

				if !found {
					t.Errorf("Expected entity %d: %q not found", i, exp.name)
					continue
				}

				if arraySpec == nil {
					t.Errorf("Entity %q: expected ArraySpec, got nil", exp.name)
					continue
				}

				if arraySpec.Kind != exp.arrayKind {
					t.Errorf("Entity %q: expected kind %v, got %v",
						exp.name, exp.arrayKind, arraySpec.Kind)
				}

				if len(arraySpec.Bounds) != exp.numDims {
					t.Errorf("Entity %q: expected %d dimensions, got %d",
						exp.name, exp.numDims, len(arraySpec.Bounds))
					continue
				}

				for j, expBound := range exp.bounds {
					if j >= len(arraySpec.Bounds) {
						break
					}
					bound := arraySpec.Bounds[j]
					lowerStr := exprToString(bound.Lower)
					upperStr := exprToString(bound.Upper)
					if lowerStr != expBound.lower {
						t.Errorf("Entity %q dim %d: expected lower=%q, got %q",
							exp.name, j, expBound.lower, lowerStr)
					}
					if upperStr != expBound.upper {
						t.Errorf("Entity %q dim %d: expected upper=%q, got %q",
							exp.name, j, expBound.upper, upperStr)
					}
				}
			}
		})
	}
}

func astEqual(t *testing.T, got, want ast.Node) bool {
	t.Helper()
	if got == nil && want == nil {
		return true
	}
	if got == nil || want == nil {
		t.Errorf("nil mismatch: got %v, want %v", got, want)
		return false
	}

	if reflect.TypeOf(got) != reflect.TypeOf(want) {
		t.Errorf("type mismatch: got %T, want %T", got, want)
		return false
	}

	// Handle expressions specifically
	switch g := got.(type) {
	case ast.Expression:
		w := want.(ast.Expression)
		return exprEqual(t, g, w)
	}

	g := reflect.ValueOf(got).Elem()
	w := reflect.ValueOf(want).Elem()

	for i := 0; i < g.NumField(); i++ {
		fieldName := g.Type().Field(i).Name
		if fieldName == "StartPos" || fieldName == "EndPos" {
			continue
		}

		gf := g.Field(i)
		wf := w.Field(i)

		if gf.Type().Kind() == reflect.Slice {
			if gf.Len() != wf.Len() {
				t.Errorf("slice length mismatch in %s: got %d, want %d", fieldName, gf.Len(), wf.Len())
				return false
			}
			for j := 0; j < gf.Len(); j++ {
				if gf.Index(j).Type().Implements(reflect.TypeOf((*ast.Node)(nil)).Elem()) {
					if !astEqual(t, gf.Index(j).Interface().(ast.Node), wf.Index(j).Interface().(ast.Node)) {
						return false
					}
				} else if !reflect.DeepEqual(gf.Index(j).Interface(), wf.Index(j).Interface()) {
					t.Errorf("slice element %d mismatch in %s: got %v, want %v", j, fieldName, gf.Index(j).Interface(), wf.Index(j).Interface())
					return false
				}
			}
		} else if gf.Type().Implements(reflect.TypeOf((*ast.Node)(nil)).Elem()) {
			if !astEqual(t, gf.Interface().(ast.Node), wf.Interface().(ast.Node)) {
				return false
			}
		} else if !reflect.DeepEqual(gf.Interface(), wf.Interface()) {
			t.Errorf("field %s mismatch: got %v, want %v", fieldName, gf.Interface(), wf.Interface())
			return false
		}
	}

	return true
}

func exprEqual(t *testing.T, got, want ast.Expression) bool {
	t.Helper()
	if got == nil && want == nil {
		return true
	}
	if got == nil || want == nil {
		t.Errorf("nil mismatch in expression: got %v, want %v", got, want)
		return false
	}

	if reflect.TypeOf(got) != reflect.TypeOf(want) {
		t.Errorf("expression type mismatch: got %T, want %T", got, want)
		return false
	}

	switch g := got.(type) {
	case *ast.Identifier:
		w := want.(*ast.Identifier)
		return g.Value == w.Value
	case *ast.IntegerLiteral:
		w := want.(*ast.IntegerLiteral)
		return g.Raw == w.Raw
	case *ast.BinaryExpr:
		w := want.(*ast.BinaryExpr)
		return g.Op == w.Op && exprEqual(t, g.Left, w.Left) && exprEqual(t, g.Right, w.Right)
	case *ast.FunctionCall:
		w := want.(*ast.FunctionCall)
		if g.Name != w.Name {
			t.Errorf("function call name mismatch: got %s, want %s", g.Name, w.Name)
			return false
		}
		if len(g.Args) != len(w.Args) {
			t.Errorf("function call arg count mismatch: got %d, want %d", len(g.Args), len(w.Args))
			return false
		}
		for i := range g.Args {
			if !exprEqual(t, g.Args[i], w.Args[i]) {
				return false
			}
		}
		return true
	default:
		t.Errorf("unhandled expression type in exprEqual: %T", got)
		return false
	}
}

// TestKindParameterSupport verifies that KIND parameters are correctly captured
// in type declarations for INTEGER, REAL, and other types
func TestKindParameterSupport(t *testing.T) {
	tests := []struct {
		name          string
		src           string
		expectKind    bool
		kindIsLiteral bool
		kindValue     int64
	}{
		{
			name: "INTEGER with KIND selector F90 syntax",
			src: `PROGRAM test
    INTEGER(KIND=8) :: bigint
END PROGRAM`,
			expectKind:    true,
			kindIsLiteral: true,
			kindValue:     8,
		},
		{
			name: "INTEGER with KIND shorthand",
			src: `PROGRAM test
    INTEGER(8) :: shorthand
END PROGRAM`,
			expectKind:    true,
			kindIsLiteral: true,
			kindValue:     8,
		},
		{
			name: "REAL with F77 kind syntax",
			src: `PROGRAM test
    REAL*8 :: dbl
END PROGRAM`,
			expectKind:    true,
			kindIsLiteral: true,
			kindValue:     8,
		},
		{
			name: "REAL with F90 KIND syntax",
			src: `PROGRAM test
    REAL(KIND=8) :: dbl
END PROGRAM`,
			expectKind:    true,
			kindIsLiteral: true,
			kindValue:     8,
		},
		{
			name: "INTEGER without KIND (default)",
			src: `PROGRAM test
    INTEGER :: normal
END PROGRAM`,
			expectKind: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var parser Parser90
			err := parser.Reset("test.f90", strings.NewReader(tt.src))
			if err != nil {
				t.Fatalf("Reset failed: %v", err)
			}

			unit := parser.ParseNextProgramUnit()
			if unit == nil {
				t.Fatal("ParseNextProgramUnit returned nil")
			}

			if len(parser.Errors()) > 0 {
				t.Fatalf("Parse errors: %v", parser.Errors())
			}

			prog, ok := unit.(*ast.ProgramBlock)
			if !ok {
				t.Fatalf("Expected ProgramBlock, got %T", unit)
			}

			if len(prog.Body) == 0 {
				t.Fatal("Expected at least one statement in body")
			}

			// Find the type declaration
			var typeDecl *ast.TypeDeclaration
			for _, stmt := range prog.Body {
				if td, ok := stmt.(*ast.TypeDeclaration); ok {
					typeDecl = td
					break
				}
			}

			if typeDecl == nil {
				t.Fatal("No TypeDeclaration found in program body")
			}

			// Check KIND parameter
			if tt.expectKind {
				if typeDecl.Type.KindOrLen == nil {
					t.Errorf("Expected KindParam to be non-nil")
					return
				}

				if tt.kindIsLiteral {
					lit, ok := typeDecl.Type.KindOrLen.(*ast.IntegerLiteral)
					if !ok {
						t.Errorf("Expected KIND to be IntegerLiteral, got %T", typeDecl.Type.KindOrLen)
						return
					}
					if lit.Value != tt.kindValue {
						t.Errorf("Expected KIND value %d, got %d", tt.kindValue, lit.Value)
					}
				}
			} else {
				if typeDecl.Type.KindOrLen != nil {
					t.Errorf("Expected KindOrLen to be nil for default kind, got %T", typeDecl.Type.KindOrLen)
				}
			}
		})
	}
}

// TestCharacterLengthAsExpression verifies that CHARACTER length is stored
// as an Expression node, not a string
func TestCharacterLengthAsExpression(t *testing.T) {
	tests := []struct {
		name         string
		src          string
		expectLength bool
		lengthValue  int64 // if length is integer literal
	}{
		{
			name: "CHARACTER with LEN= syntax",
			src: `PROGRAM test
    CHARACTER(LEN=80) :: str
END PROGRAM`,
			expectLength: true,
			lengthValue:  80,
		},
		{
			name: "CHARACTER shorthand syntax",
			src: `PROGRAM test
    CHARACTER(80) :: str
END PROGRAM`,
			expectLength: true,
			lengthValue:  80,
		},
		{
			name: "CHARACTER F77 syntax",
			src: `PROGRAM test
    CHARACTER*10 :: str
END PROGRAM`,
			expectLength: true,
			lengthValue:  10,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var parser Parser90
			err := parser.Reset("test.f90", strings.NewReader(tt.src))
			if err != nil {
				t.Fatalf("Reset failed: %v", err)
			}

			unit := parser.ParseNextProgramUnit()
			if unit == nil {
				t.Fatal("ParseNextProgramUnit returned nil")
			}

			if len(parser.Errors()) > 0 {
				t.Fatalf("Parse errors: %v", parser.Errors())
			}

			prog, ok := unit.(*ast.ProgramBlock)
			if !ok {
				t.Fatalf("Expected ProgramBlock, got %T", unit)
			}

			// Find the type declaration
			var typeDecl *ast.TypeDeclaration
			for _, stmt := range prog.Body {
				if td, ok := stmt.(*ast.TypeDeclaration); ok {
					typeDecl = td
					break
				}
			}

			if typeDecl == nil {
				t.Fatal("No TypeDeclaration found")
			}

			if len(typeDecl.Entities) == 0 {
				t.Fatal("No entities in type declaration")
			}

			entity := typeDecl.Entities[0]

			if tt.expectLength {
				if entity.Type.KindOrLen == nil {
					t.Errorf("Expected CharLen to be non-nil Expression")
					return
				}

				lit, ok := entity.Type.KindOrLen.(*ast.IntegerLiteral)
				if !ok {
					t.Errorf("Expected CharLen to be IntegerLiteral, got %T", entity.Type.KindOrLen)
					return
				}

				if lit.Value != tt.lengthValue {
					t.Errorf("Expected length %d, got %d", tt.lengthValue, lit.Value)
				}
			}
		})
	}
}

// TestFunctionKindParameter verifies that function result types capture KIND
func TestFunctionKindParameter(t *testing.T) {
	src := `REAL*8 FUNCTION compute(x)
    REAL*8 :: x
    compute = x * 2.0d0
END FUNCTION`

	var parser Parser90
	err := parser.Reset("test.f90", strings.NewReader(src))
	if err != nil {
		t.Fatalf("Reset failed: %v", err)
	}

	unit := parser.ParseNextProgramUnit()
	if unit == nil {
		t.Fatal("ParseNextProgramUnit returned nil")
	}

	if len(parser.Errors()) > 0 {
		t.Fatalf("Parse errors: %v", parser.Errors())
	}

	fn, ok := unit.(*ast.Function)
	if !ok {
		t.Fatalf("Expected Function, got %T", unit)
	}

	if fn.Type.Token.String() != "REAL" {
		t.Errorf("Expected Type 'REAL', got '%s'", fn.Type.Token)
	}

	if fn.Type.KindOrLen == nil {
		t.Errorf("Expected KindOrLen to be non-nil")
		return
	}

	lit, ok := fn.Type.KindOrLen.(*ast.IntegerLiteral)
	if !ok {
		t.Errorf("Expected KindOrLen to be IntegerLiteral, got %T", fn.Type.KindOrLen)
		return
	}

	if lit.Value != 8 {
		t.Errorf("Expected KindOrLen value 8, got %d", lit.Value)
	}
}

// TestParameterKindCapture verifies that parameter KIND is captured
func TestParameterKindCapture(t *testing.T) {
	src := `SUBROUTINE process(n, arr)
    INTEGER(KIND=4), INTENT(IN) :: n
    REAL(KIND=8), DIMENSION(n), INTENT(INOUT) :: arr
END SUBROUTINE`

	var parser Parser90
	err := parser.Reset("test.f90", strings.NewReader(src))
	if err != nil {
		t.Fatalf("Reset failed: %v", err)
	}

	unit := parser.ParseNextProgramUnit()
	if unit == nil {
		t.Fatal("ParseNextProgramUnit returned nil")
	}

	if len(parser.Errors()) > 0 {
		t.Fatalf("Parse errors: %v", parser.Errors())
	}

	sub, ok := unit.(*ast.Subroutine)
	if !ok {
		t.Fatalf("Expected Subroutine, got %T", unit)
	}

	if len(sub.Parameters) != 2 {
		t.Fatalf("Expected 2 parameters, got %d", len(sub.Parameters))
	}

	// Check first parameter (n)
	param0 := sub.Parameters[0]
	if param0.Name != "n" {
		t.Errorf("Expected parameter 0 name 'n', got '%s'", param0.Name)
	}
	if param0.Type.Token.String() != "INTEGER" {
		t.Errorf("Expected parameter 0 type 'INTEGER', got '%s'", param0.Type.Token)
	}
	if param0.Type.KindOrLen == nil {
		t.Errorf("Expected parameter 0 KindOrLen to be non-nil")
	} else {
		lit, ok := param0.Type.KindOrLen.(*ast.IntegerLiteral)
		if !ok {
			t.Errorf("Expected parameter 0 KindOrLen to be IntegerLiteral, got %T", param0.Type.KindOrLen)
		} else if lit.Value != 4 {
			t.Errorf("Expected parameter 0 KindOrLen value 4, got %d", lit.Value)
		}
	}

	// Check second parameter (arr)
	param1 := sub.Parameters[1]
	if param1.Name != "arr" {
		t.Errorf("Expected parameter 1 name 'arr', got '%s'", param1.Name)
	}
	if param1.Type.Token.String() != "REAL" {
		t.Errorf("Expected parameter 1 type 'REAL', got '%s'", param1.Type.Token)
	}
	if param1.Type.KindOrLen == nil {
		t.Errorf("Expected parameter 1 KindOrLen to be non-nil")
	} else {
		lit, ok := param1.Type.KindOrLen.(*ast.IntegerLiteral)
		if !ok {
			t.Errorf("Expected parameter 1 KindOrLen to be IntegerLiteral, got %T", param1.Type.KindOrLen)
		} else if lit.Value != 8 {
			t.Errorf("Expected parameter 1 KindOrLen value 8, got %d", lit.Value)
		}
	}
}

// TestCommonBlockParsing verifies that COMMON blocks are correctly parsed
func TestCommonBlockParsing(t *testing.T) {
	tests := []struct {
		name          string
		src           string
		expectedBlock string
		expectedVars  []string
	}{
		{
			name: "named COMMON block",
			src: `
PROGRAM test
  INTEGER A, B, C
  COMMON /BLOCK1/ A, B, C
END PROGRAM
`,
			expectedBlock: "BLOCK1",
			expectedVars:  []string{"A", "B", "C"},
		},
		{
			name: "blank COMMON with slashes",
			src: `
PROGRAM test
  REAL X, Y
  COMMON // X, Y
END PROGRAM
`,
			expectedBlock: "",
			expectedVars:  []string{"X", "Y"},
		},
		{
			name: "blank COMMON without slashes",
			src: `
PROGRAM test
  REAL Z
  COMMON Z
END PROGRAM
`,
			expectedBlock: "",
			expectedVars:  []string{"Z"},
		},
		{
			name: "COMMON with array",
			src: `
PROGRAM test
  REAL ARR(10, 20)
  COMMON /ARRAYS/ ARR
END PROGRAM
`,
			expectedBlock: "ARRAYS",
			expectedVars:  []string{"ARR"},
		},
		{
			name: "COMMON with multiple variables",
			src: `
PROGRAM test
  INTEGER I, J
  REAL X
  COMMON /DATA/ I, J, X
END PROGRAM
`,
			expectedBlock: "DATA",
			expectedVars:  []string{"I", "J", "X"},
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

			if len(parser.Errors()) > 0 {
				t.Fatalf("Parse errors: %v", parser.Errors())
			}

			prog, ok := unit.(*ast.ProgramBlock)
			if !ok {
				t.Fatalf("Expected ProgramBlock, got %T", unit)
			}

			// Find the COMMON statement in the program body
			var commonStmt *ast.CommonStmt
			for _, stmt := range prog.Body {
				if cs, ok := stmt.(*ast.CommonStmt); ok {
					commonStmt = cs
					break
				}
			}

			if commonStmt == nil {
				t.Fatalf("COMMON statement not found in program body")
			}

			// Verify block name
			if commonStmt.BlockName != tt.expectedBlock {
				t.Errorf("Expected block name '%s', got '%s'", tt.expectedBlock, commonStmt.BlockName)
			}

			// Verify variables
			if len(commonStmt.Variables) != len(tt.expectedVars) {
				t.Errorf("Expected %d variables, got %d", len(tt.expectedVars), len(commonStmt.Variables))
			}

			for i, expectedVar := range tt.expectedVars {
				if i >= len(commonStmt.Variables) {
					break
				}
				if commonStmt.Variables[i] != expectedVar {
					t.Errorf("Variable %d: expected '%s', got '%s'", i, expectedVar, commonStmt.Variables[i])
				}
			}
		})
	}
}

// TestExternalIntrinsicParsing verifies that EXTERNAL and INTRINSIC statements are correctly parsed
func TestExternalIntrinsicParsing(t *testing.T) {
	tests := []struct {
		name          string
		src           string
		stmtType      string // "external" or "intrinsic"
		expectedNames []string
	}{
		{
			name: "EXTERNAL single name",
			src: `
PROGRAM test
  EXTERNAL mysub
END PROGRAM
`,
			stmtType:      "external",
			expectedNames: []string{"mysub"},
		},
		{
			name: "EXTERNAL multiple names",
			src: `
PROGRAM test
  EXTERNAL mysub, myfunc, another
END PROGRAM
`,
			stmtType:      "external",
			expectedNames: []string{"mysub", "myfunc", "another"},
		},
		{
			name: "INTRINSIC single name",
			src: `
PROGRAM test
  INTRINSIC sin
END PROGRAM
`,
			stmtType:      "intrinsic",
			expectedNames: []string{"sin"},
		},
		{
			name: "INTRINSIC multiple names",
			src: `
PROGRAM test
  INTRINSIC sin, cos, sqrt, exp
END PROGRAM
`,
			stmtType:      "intrinsic",
			expectedNames: []string{"sin", "cos", "sqrt", "exp"},
		},
		{
			name: "EXTERNAL and INTRINSIC together",
			src: `
PROGRAM test
  EXTERNAL external_func
  INTRINSIC sin, cos
END PROGRAM
`,
			stmtType:      "external",
			expectedNames: []string{"external_func"},
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

			if len(parser.Errors()) > 0 {
				t.Fatalf("Parse errors: %v", parser.Errors())
			}

			prog, ok := unit.(*ast.ProgramBlock)
			if !ok {
				t.Fatalf("Expected ProgramBlock, got %T", unit)
			}

			// Find the EXTERNAL or INTRINSIC statement in the program body
			var names []string
			for _, stmt := range prog.Body {
				if tt.stmtType == "external" {
					if es, ok := stmt.(*ast.ExternalStmt); ok {
						names = es.Names
						break
					}
				} else if tt.stmtType == "intrinsic" {
					if is, ok := stmt.(*ast.IntrinsicStmt); ok {
						names = is.Names
						break
					}
				}
			}

			if names == nil {
				t.Fatalf("%s statement not found in program body", strings.ToUpper(tt.stmtType))
			}

			// Verify names
			if len(names) != len(tt.expectedNames) {
				t.Errorf("Expected %d names, got %d", len(tt.expectedNames), len(names))
			}

			for i, expectedName := range tt.expectedNames {
				if i >= len(names) {
					break
				}
				if names[i] != expectedName {
					t.Errorf("Name %d: expected '%s', got '%s'", i, expectedName, names[i])
				}
			}
		})
	}
}

// TestImplicitStatementParsing verifies that IMPLICIT statements are correctly parsed
func TestImplicitStatementParsing(t *testing.T) {
	tests := []struct {
		name          string
		src           string
		expectNone    bool
		expectedRules []struct {
			typ    string
			ranges []struct{ start, end byte }
		}
	}{
		{
			name: "IMPLICIT NONE",
			src: `
PROGRAM test
  IMPLICIT NONE
END PROGRAM
`,
			expectNone: true,
		},
		{
			name: "IMPLICIT REAL (A-H)",
			src: `
PROGRAM test
  IMPLICIT REAL (A-H)
END PROGRAM
`,
			expectNone: false,
			expectedRules: []struct {
				typ    string
				ranges []struct{ start, end byte }
			}{
				{
					typ: "REAL",
					ranges: []struct{ start, end byte }{
						{start: 'A', end: 'H'},
					},
				},
			},
		},
		{
			name: "IMPLICIT REAL (A-H, O-Z)",
			src: `
PROGRAM test
  IMPLICIT REAL (A-H, O-Z)
END PROGRAM
`,
			expectNone: false,
			expectedRules: []struct {
				typ    string
				ranges []struct{ start, end byte }
			}{
				{
					typ: "REAL",
					ranges: []struct{ start, end byte }{
						{start: 'A', end: 'H'},
						{start: 'O', end: 'Z'},
					},
				},
			},
		},
		{
			name: "IMPLICIT INTEGER (I-N)",
			src: `
PROGRAM test
  IMPLICIT INTEGER (I-N)
END PROGRAM
`,
			expectNone: false,
			expectedRules: []struct {
				typ    string
				ranges []struct{ start, end byte }
			}{
				{
					typ: "INTEGER",
					ranges: []struct{ start, end byte }{
						{start: 'I', end: 'N'},
					},
				},
			},
		},
		{
			name: "IMPLICIT with single letters",
			src: `
PROGRAM test
  IMPLICIT INTEGER (I, J, K)
END PROGRAM
`,
			expectNone: false,
			expectedRules: []struct {
				typ    string
				ranges []struct{ start, end byte }
			}{
				{
					typ: "INTEGER",
					ranges: []struct{ start, end byte }{
						{start: 'I', end: 'I'},
						{start: 'J', end: 'J'},
						{start: 'K', end: 'K'},
					},
				},
			},
		},
		{
			name: "IMPLICIT multiple type specs",
			src: `
PROGRAM test
  IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
END PROGRAM
`,
			expectNone: false,
			expectedRules: []struct {
				typ    string
				ranges []struct{ start, end byte }
			}{
				{
					typ: "REAL",
					ranges: []struct{ start, end byte }{
						{start: 'A', end: 'H'},
						{start: 'O', end: 'Z'},
					},
				},
				{
					typ: "INTEGER",
					ranges: []struct{ start, end byte }{
						{start: 'I', end: 'N'},
					},
				},
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
			if unit == nil {
				t.Fatal("ParseNextProgramUnit returned nil")
			}

			if len(parser.Errors()) > 0 {
				t.Fatalf("Parse errors: %v", parser.Errors())
			}

			prog, ok := unit.(*ast.ProgramBlock)
			if !ok {
				t.Fatalf("Expected ProgramBlock, got %T", unit)
			}

			// Find the IMPLICIT statement in the program body
			var implicitStmt *ast.ImplicitStatement
			for _, stmt := range prog.Body {
				if is, ok := stmt.(*ast.ImplicitStatement); ok {
					implicitStmt = is
					break
				}
			}

			if implicitStmt == nil {
				t.Fatalf("IMPLICIT statement not found in program body")
			}

			// Verify IsNone
			if implicitStmt.IsNone != tt.expectNone {
				t.Errorf("Expected IsNone=%v, got %v", tt.expectNone, implicitStmt.IsNone)
			}

			// Verify rules
			if !tt.expectNone {
				if len(implicitStmt.Rules) != len(tt.expectedRules) {
					t.Errorf("Expected %d rules, got %d", len(tt.expectedRules), len(implicitStmt.Rules))
				}

				for i, expectedRule := range tt.expectedRules {
					if i >= len(implicitStmt.Rules) {
						break
					}
					rule := implicitStmt.Rules[i]

					if rule.Type.Token.String() != expectedRule.typ {
						t.Errorf("Rule %d: expected type '%s', got '%s'", i, expectedRule.typ, rule.Type.Token.String())
					}

					if len(rule.LetterRanges) != len(expectedRule.ranges) {
						t.Errorf("Rule %d: expected %d ranges, got %d", i, len(expectedRule.ranges), len(rule.LetterRanges))
					}

					for j, expectedRange := range expectedRule.ranges {
						if j >= len(rule.LetterRanges) {
							break
						}
						lr := rule.LetterRanges[j]

						if lr.Start != expectedRange.start {
							t.Errorf("Rule %d, Range %d: expected start '%c', got '%c'", i, j, expectedRange.start, lr.Start)
						}
						if lr.End != expectedRange.end {
							t.Errorf("Rule %d, Range %d: expected end '%c', got '%c'", i, j, expectedRange.end, lr.End)
						}
					}
				}
			}
		})
	}
}
