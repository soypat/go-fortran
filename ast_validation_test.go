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
