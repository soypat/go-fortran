package fortran

import (
	"strings"
	"testing"

	"github.com/soypat/go-fortran/ast"
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

				if param.Type != exp.typ {
					t.Errorf("Parameter %d (%s): expected type %q, got %q", i, param.Name, exp.typ, param.Type)
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
				{name: "vec", typ: "INTEGER", initializer: "1  2  3"}, // TODO: Capture (/ /) delimiters
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
				{name: "ptr", typ: "INTEGER", initializer: "=>  null"}, // TODO: Capture () after function names
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

			if len(parser.Errors()) > 0 {
				t.Fatalf("Parse errors: %v", parser.Errors())
			}

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

				// Check parameters first
				for _, param := range params {
					if param.Name == exp.name {
						found = true
						actualType = param.Type
						actualCharLen = param.CharLen
						// Parameters don't have initializers (they're arguments)
						break
					}
				}

				// Check entities
				if !found {
					for _, entity := range entities {
						if entity.Name == exp.name {
							found = true
							actualInit = entity.Initializer
							actualCharLen = entity.CharLen
							// Get type from the TypeDeclaration that contains this entity
							// We need to find which TypeDeclaration this entity belongs to
							switch u := unit.(type) {
							case *ast.ProgramBlock:
								for _, stmt := range u.Body {
									if typeDecl, ok := stmt.(*ast.TypeDeclaration); ok {
										for _, e := range typeDecl.Entities {
											if e.Name == entity.Name {
												actualType = typeDecl.TypeSpec
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
												actualType = typeDecl.TypeSpec
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
		if attr.String() == "DIMENSION" {
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
		if attr.String() == "OPTIONAL" {
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
