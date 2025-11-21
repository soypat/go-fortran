package fortran

import (
	"strings"
	"testing"

	"github.com/soypat/go-fortran/ast"
)

// TestExpressionParsing verifies that the expression parser correctly constructs
// expression AST nodes with proper operator precedence and associativity.
func TestExpressionParsing(t *testing.T) {
	tests := []struct {
		name     string
		src      string
		validate func(t *testing.T, expr ast.Expression)
	}{
		// ===== Integer Literals =====
		{
			name: "integer literal",
			src:  "42",
			validate: func(t *testing.T, expr ast.Expression) {
				// Integer literals are parsed as IntegerLiteral nodes
				_, ok := expr.(*ast.IntegerLiteral)
				if !ok {
					t.Fatalf("Expected *ast.IntegerLiteral, got %T", expr)
				}
				// TODO: Parse actual integer value
			},
		},

		// ===== Real Literals =====
		{
			name: "real literal",
			src:  "3.14159",
			validate: func(t *testing.T, expr ast.Expression) {
				lit, ok := expr.(*ast.RealLiteral)
				if !ok {
					t.Fatalf("Expected *ast.RealLiteral, got %T", expr)
				}
				// TODO: Parse actual real value
				_ = lit
			},
		},
		{
			name: "real literal with exponent",
			src:  "1.23e-4",
			validate: func(t *testing.T, expr ast.Expression) {
				lit, ok := expr.(*ast.RealLiteral)
				if !ok {
					t.Fatalf("Expected *ast.RealLiteral, got %T", expr)
				}
				if lit.Raw != "1.23e-4" {
					t.Errorf("Expected raw '1.23e-4', got %s", lit.Raw)
				}
			},
		},

		// ===== String Literals =====
		{
			name: "string literal",
			src:  "'hello world'",
			validate: func(t *testing.T, expr ast.Expression) {
				lit, ok := expr.(*ast.StringLiteral)
				if !ok {
					t.Fatalf("Expected *ast.StringLiteral, got %T", expr)
				}
				if lit.Value != "hello world" {
					t.Errorf("Expected value 'hello world', got %s", lit.Value)
				}
			},
		},

		// ===== Logical Literals =====
		{
			name: "logical literal TRUE",
			src:  ".TRUE.",
			validate: func(t *testing.T, expr ast.Expression) {
				lit, ok := expr.(*ast.LogicalLiteral)
				if !ok {
					t.Fatalf("Expected *ast.LogicalLiteral, got %T", expr)
				}
				if !lit.Value {
					t.Errorf("Expected value true, got false")
				}
			},
		},
		{
			name: "logical literal FALSE",
			src:  ".FALSE.",
			validate: func(t *testing.T, expr ast.Expression) {
				lit, ok := expr.(*ast.LogicalLiteral)
				if !ok {
					t.Fatalf("Expected *ast.LogicalLiteral, got %T", expr)
				}
				if lit.Value {
					t.Errorf("Expected value false, got true")
				}
			},
		},

		// ===== Binary Expressions - Arithmetic Precedence =====
		{
			name: "addition and multiplication precedence: a + b * c",
			src:  "a + b * c",
			validate: func(t *testing.T, expr ast.Expression) {
				// Should parse as: a + (b * c)
				binExpr, ok := expr.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected *ast.BinaryExpr, got %T", expr)
				}
				if binExpr.Op.String() != "+" {
					t.Errorf("Expected top-level op '+', got %s", binExpr.Op)
				}

				// Left should be identifier 'a'
				leftId, ok := binExpr.Left.(*ast.Identifier)
				if !ok {
					t.Fatalf("Expected left to be *ast.Identifier, got %T", binExpr.Left)
				}
				if leftId.Value != "a" {
					t.Errorf("Expected left identifier 'a', got %s", leftId.Value)
				}

				// Right should be (b * c)
				rightMul, ok := binExpr.Right.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected right to be *ast.BinaryExpr, got %T", binExpr.Right)
				}
				if rightMul.Op.String() != "*" {
					t.Errorf("Expected right op '*', got %s", rightMul.Op)
				}
			},
		},
		{
			name: "subtraction and division precedence: a - b / c",
			src:  "a - b / c",
			validate: func(t *testing.T, expr ast.Expression) {
				// Should parse as: a - (b / c)
				binExpr, ok := expr.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected *ast.BinaryExpr, got %T", expr)
				}
				if binExpr.Op.String() != "-" {
					t.Errorf("Expected top-level op '-', got %s", binExpr.Op)
				}

				// Right should be (b / c)
				rightDiv, ok := binExpr.Right.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected right to be *ast.BinaryExpr, got %T", binExpr.Right)
				}
				if rightDiv.Op.String() != "/" {
					t.Errorf("Expected right op '/', got %s", rightDiv.Op)
				}
			},
		},

		// ===== Exponentiation Right-Associativity =====
		{
			name: "exponentiation right-associativity: 2 ** 3 ** 4",
			src:  "2 ** 3 ** 4",
			validate: func(t *testing.T, expr ast.Expression) {
				// Should parse as: 2 ** (3 ** 4)
				binExpr, ok := expr.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected *ast.BinaryExpr, got %T", expr)
				}
				if binExpr.Op.String() != "**" {
					t.Errorf("Expected top-level op '**', got %s", binExpr.Op)
				}

				// Right should be (3 ** 4)
				rightExp, ok := binExpr.Right.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected right to be *ast.BinaryExpr, got %T", binExpr.Right)
				}
				if rightExp.Op.String() != "**" {
					t.Errorf("Expected right op '**', got %s", rightExp.Op)
				}
			},
		},
		{
			name: "exponentiation and multiplication: a * b ** c",
			src:  "a * b ** c",
			validate: func(t *testing.T, expr ast.Expression) {
				// Should parse as: a * (b ** c) because ** has higher precedence
				binExpr, ok := expr.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected *ast.BinaryExpr, got %T", expr)
				}
				if binExpr.Op.String() != "*" {
					t.Errorf("Expected top-level op '*', got %s", binExpr.Op)
				}

				// Right should be (b ** c)
				rightExp, ok := binExpr.Right.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected right to be *ast.BinaryExpr, got %T", binExpr.Right)
				}
				if rightExp.Op.String() != "**" {
					t.Errorf("Expected right op '**', got %s", rightExp.Op)
				}
			},
		},

		// ===== Unary Operators =====
		{
			name: "unary minus",
			src:  "-x",
			validate: func(t *testing.T, expr ast.Expression) {
				unaryExpr, ok := expr.(*ast.UnaryExpr)
				if !ok {
					t.Fatalf("Expected *ast.UnaryExpr, got %T", expr)
				}
				if unaryExpr.Op.String() != "-" {
					t.Errorf("Expected op '-', got %s", unaryExpr.Op)
				}
				operand, ok := unaryExpr.Operand.(*ast.Identifier)
				if !ok {
					t.Fatalf("Expected operand to be *ast.Identifier, got %T", unaryExpr.Operand)
				}
				if operand.Value != "x" {
					t.Errorf("Expected operand 'x', got %s", operand.Value)
				}
			},
		},
		{
			name: "unary plus",
			src:  "+y",
			validate: func(t *testing.T, expr ast.Expression) {
				unaryExpr, ok := expr.(*ast.UnaryExpr)
				if !ok {
					t.Fatalf("Expected *ast.UnaryExpr, got %T", expr)
				}
				if unaryExpr.Op.String() != "+" {
					t.Errorf("Expected op '+', got %s", unaryExpr.Op)
				}
			},
		},
		{
			name: "logical NOT",
			src:  ".NOT. flag",
			validate: func(t *testing.T, expr ast.Expression) {
				unaryExpr, ok := expr.(*ast.UnaryExpr)
				if !ok {
					t.Fatalf("Expected *ast.UnaryExpr, got %T", expr)
				}
				if unaryExpr.Op.String() != ".NOT." {
					t.Errorf("Expected op '.NOT.', got %s", unaryExpr.Op)
				}
				operand, ok := unaryExpr.Operand.(*ast.Identifier)
				if !ok {
					t.Fatalf("Expected operand to be *ast.Identifier, got %T", unaryExpr.Operand)
				}
				if operand.Value != "flag" {
					t.Errorf("Expected operand 'flag', got %s", operand.Value)
				}
			},
		},

		// ===== Logical Expressions =====
		{
			name: "logical AND",
			src:  "a .AND. b",
			validate: func(t *testing.T, expr ast.Expression) {
				binExpr, ok := expr.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected *ast.BinaryExpr, got %T", expr)
				}
				if binExpr.Op.String() != ".AND." {
					t.Errorf("Expected op '.AND.', got %s", binExpr.Op)
				}
			},
		},
		{
			name: "logical OR",
			src:  "x .OR. y",
			validate: func(t *testing.T, expr ast.Expression) {
				binExpr, ok := expr.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected *ast.BinaryExpr, got %T", expr)
				}
				if binExpr.Op.String() != ".OR." {
					t.Errorf("Expected op '.OR.', got %s", binExpr.Op)
				}
			},
		},
		{
			name: "logical precedence: a .OR. b .AND. c",
			src:  "a .OR. b .AND. c",
			validate: func(t *testing.T, expr ast.Expression) {
				// Should parse as: a .OR. (b .AND. c) because .AND. has higher precedence
				binExpr, ok := expr.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected *ast.BinaryExpr, got %T", expr)
				}
				if binExpr.Op.String() != ".OR." {
					t.Errorf("Expected top-level op '.OR.', got %s", binExpr.Op)
				}

				// Right should be (b .AND. c)
				rightAnd, ok := binExpr.Right.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected right to be *ast.BinaryExpr, got %T", binExpr.Right)
				}
				if rightAnd.Op.String() != ".AND." {
					t.Errorf("Expected right op '.AND.', got %s", rightAnd.Op)
				}
			},
		},

		// ===== Relational Operators =====
		{
			name: "F77 greater than: x .GT. y",
			src:  "x .GT. y",
			validate: func(t *testing.T, expr ast.Expression) {
				binExpr, ok := expr.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected *ast.BinaryExpr, got %T", expr)
				}
				if binExpr.Op.String() != ".GT." {
					t.Errorf("Expected op '.GT.', got %s", binExpr.Op)
				}
			},
		},
		{
			name: "F77 less than: a .LT. b",
			src:  "a .LT. b",
			validate: func(t *testing.T, expr ast.Expression) {
				binExpr, ok := expr.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected *ast.BinaryExpr, got %T", expr)
				}
				if binExpr.Op.String() != ".LT." {
					t.Errorf("Expected op '.LT.', got %s", binExpr.Op)
				}
			},
		},
		{
			name: "F77 equals: x .EQ. 0",
			src:  "x .EQ. 0",
			validate: func(t *testing.T, expr ast.Expression) {
				binExpr, ok := expr.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected *ast.BinaryExpr, got %T", expr)
				}
				if binExpr.Op.String() != ".EQ." {
					t.Errorf("Expected op '.EQ.', got %s", binExpr.Op)
				}
			},
		},
		{
			name: "F90 greater than: x > y",
			src:  "x > y",
			validate: func(t *testing.T, expr ast.Expression) {
				binExpr, ok := expr.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected *ast.BinaryExpr, got %T", expr)
				}
				if binExpr.Op.String() != ">" {
					t.Errorf("Expected op '>', got %s", binExpr.Op)
				}
			},
		},
		{
			name: "F90 less than: a < b",
			src:  "a < b",
			validate: func(t *testing.T, expr ast.Expression) {
				binExpr, ok := expr.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected *ast.BinaryExpr, got %T", expr)
				}
				if binExpr.Op.String() != "<" {
					t.Errorf("Expected op '<', got %s", binExpr.Op)
				}
			},
		},
		{
			name: "F90 equals: x == 0",
			src:  "x == 0",
			validate: func(t *testing.T, expr ast.Expression) {
				binExpr, ok := expr.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected *ast.BinaryExpr, got %T", expr)
				}
				if binExpr.Op.String() != "==" {
					t.Errorf("Expected op '==', got %s", binExpr.Op)
				}
			},
		},
		{
			name: "relational and logical: x > 0 .AND. y < 10",
			src:  "x > 0 .AND. y < 10",
			validate: func(t *testing.T, expr ast.Expression) {
				// Should parse as: (x > 0) .AND. (y < 10)
				// .AND. has lower precedence than relational operators
				binExpr, ok := expr.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected *ast.BinaryExpr, got %T", expr)
				}
				if binExpr.Op.String() != ".AND." {
					t.Errorf("Expected top-level op '.AND.', got %s", binExpr.Op)
				}

				// Left should be (x > 0)
				leftRel, ok := binExpr.Left.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected left to be *ast.BinaryExpr, got %T", binExpr.Left)
				}
				if leftRel.Op.String() != ">" {
					t.Errorf("Expected left op '>', got %s", leftRel.Op)
				}

				// Right should be (y < 10)
				rightRel, ok := binExpr.Right.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected right to be *ast.BinaryExpr, got %T", binExpr.Right)
				}
				if rightRel.Op.String() != "<" {
					t.Errorf("Expected right op '<', got %s", rightRel.Op)
				}
			},
		},

		// ===== Function Calls =====
		{
			name: "function call with no arguments",
			src:  "random()",
			validate: func(t *testing.T, expr ast.Expression) {
				funcCall, ok := expr.(*ast.FunctionCall)
				if !ok {
					t.Fatalf("Expected *ast.FunctionCall, got %T", expr)
				}
				if funcCall.Name != "random" {
					t.Errorf("Expected function name 'random', got %s", funcCall.Name)
				}
				if len(funcCall.Args) != 0 {
					t.Errorf("Expected 0 arguments, got %d", len(funcCall.Args))
				}
			},
		},
		{
			name: "function call with one argument",
			src:  "sqrt(x)",
			validate: func(t *testing.T, expr ast.Expression) {
				funcCall, ok := expr.(*ast.FunctionCall)
				if !ok {
					t.Fatalf("Expected *ast.FunctionCall, got %T", expr)
				}
				if funcCall.Name != "sqrt" {
					t.Errorf("Expected function name 'sqrt', got %s", funcCall.Name)
				}
				if len(funcCall.Args) != 1 {
					t.Errorf("Expected 1 argument, got %d", len(funcCall.Args))
				}
			},
		},
		{
			name: "function call with multiple arguments",
			src:  "max(a, b, c)",
			validate: func(t *testing.T, expr ast.Expression) {
				funcCall, ok := expr.(*ast.FunctionCall)
				if !ok {
					t.Fatalf("Expected *ast.FunctionCall, got %T", expr)
				}
				if funcCall.Name != "max" {
					t.Errorf("Expected function name 'max', got %s", funcCall.Name)
				}
				if len(funcCall.Args) != 3 {
					t.Errorf("Expected 3 arguments, got %d", len(funcCall.Args))
				}
			},
		},
		{
			name: "nested function calls: sqrt(x*x + y*y)",
			src:  "sqrt(x*x + y*y)",
			validate: func(t *testing.T, expr ast.Expression) {
				funcCall, ok := expr.(*ast.FunctionCall)
				if !ok {
					t.Fatalf("Expected *ast.FunctionCall, got %T", expr)
				}
				if funcCall.Name != "sqrt" {
					t.Errorf("Expected function name 'sqrt', got %s", funcCall.Name)
				}
				if len(funcCall.Args) != 1 {
					t.Fatalf("Expected 1 argument, got %d", len(funcCall.Args))
				}

				// Argument should be (x*x + y*y)
				argExpr, ok := funcCall.Args[0].(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected argument to be *ast.BinaryExpr, got %T", funcCall.Args[0])
				}
				if argExpr.Op.String() != "+" {
					t.Errorf("Expected argument op '+', got %s", argExpr.Op)
				}
			},
		},
		{
			name: "function call with expression argument: fact(n-1, result)",
			src:  "fact(n-1, result)",
			validate: func(t *testing.T, expr ast.Expression) {
				funcCall, ok := expr.(*ast.FunctionCall)
				if !ok {
					t.Fatalf("Expected *ast.FunctionCall, got %T", expr)
				}
				if funcCall.Name != "fact" {
					t.Errorf("Expected function name 'fact', got %s", funcCall.Name)
				}
				if len(funcCall.Args) != 2 {
					t.Fatalf("Expected 2 arguments, got %d", len(funcCall.Args))
				}

				// First argument should be binary expression (n-1)
				binExpr, ok := funcCall.Args[0].(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected first arg to be *ast.BinaryExpr, got %T", funcCall.Args[0])
				}
				if binExpr.Op.String() != "-" {
					t.Errorf("Expected first arg op '-', got %s", binExpr.Op)
				}

				// Second argument should be identifier 'result'
				ident, ok := funcCall.Args[1].(*ast.Identifier)
				if !ok {
					t.Fatalf("Expected second arg to be *ast.Identifier, got %T", funcCall.Args[1])
				}
				if ident.Value != "result" {
					t.Errorf("Expected identifier 'result', got %s", ident.Value)
				}
			},
		},

		// ===== Array References =====
		// NOTE: Without a symbol table, array references are indistinguishable from function calls
		// so they're parsed as FunctionCall nodes
		{
			name: "array reference 1D",
			src:  "arr(i)",
			validate: func(t *testing.T, expr ast.Expression) {
				funcCall, ok := expr.(*ast.FunctionCall)
				if !ok {
					t.Fatalf("Expected *ast.FunctionCall, got %T", expr)
				}
				if funcCall.Name != "arr" {
					t.Errorf("Expected name 'arr', got %s", funcCall.Name)
				}
				if len(funcCall.Args) != 1 {
					t.Errorf("Expected 1 argument, got %d", len(funcCall.Args))
				}
			},
		},
		{
			name: "array reference 2D",
			src:  "matrix(i, j)",
			validate: func(t *testing.T, expr ast.Expression) {
				funcCall, ok := expr.(*ast.FunctionCall)
				if !ok {
					t.Fatalf("Expected *ast.FunctionCall, got %T", expr)
				}
				if funcCall.Name != "matrix" {
					t.Errorf("Expected name 'matrix', got %s", funcCall.Name)
				}
				if len(funcCall.Args) != 2 {
					t.Errorf("Expected 2 arguments, got %d", len(funcCall.Args))
				}
			},
		},
		{
			name: "array reference with expression subscript",
			src:  "arr(i + 1)",
			validate: func(t *testing.T, expr ast.Expression) {
				funcCall, ok := expr.(*ast.FunctionCall)
				if !ok {
					t.Fatalf("Expected *ast.FunctionCall, got %T", expr)
				}
				if len(funcCall.Args) != 1 {
					t.Fatalf("Expected 1 argument, got %d", len(funcCall.Args))
				}

				// Argument should be (i + 1)
				arg, ok := funcCall.Args[0].(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected argument to be *ast.BinaryExpr, got %T", funcCall.Args[0])
				}
				if arg.Op.String() != "+" {
					t.Errorf("Expected argument op '+', got %s", arg.Op)
				}
			},
		},

		// ===== Parenthesized Expressions =====
		{
			name: "parenthesized expression: (a + b)",
			src:  "(a + b)",
			validate: func(t *testing.T, expr ast.Expression) {
				parenExpr, ok := expr.(*ast.ParenExpr)
				if !ok {
					t.Fatalf("Expected *ast.ParenExpr, got %T", expr)
				}

				// Inner should be (a + b)
				innerExpr, ok := parenExpr.Expr.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected inner to be *ast.BinaryExpr, got %T", parenExpr.Expr)
				}
				if innerExpr.Op.String() != "+" {
					t.Errorf("Expected inner op '+', got %s", innerExpr.Op)
				}
			},
		},
		{
			name: "parentheses override precedence: (a + b) * c",
			src:  "(a + b) * c",
			validate: func(t *testing.T, expr ast.Expression) {
				// Should parse as: (a + b) * c, not a + (b * c)
				binExpr, ok := expr.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected *ast.BinaryExpr, got %T", expr)
				}
				if binExpr.Op.String() != "*" {
					t.Errorf("Expected top-level op '*', got %s", binExpr.Op)
				}

				// Left should be parenthesized (a + b)
				leftParen, ok := binExpr.Left.(*ast.ParenExpr)
				if !ok {
					t.Fatalf("Expected left to be *ast.ParenExpr, got %T", binExpr.Left)
				}

				// Inner of paren should be (a + b)
				innerAdd, ok := leftParen.Expr.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected paren inner to be *ast.BinaryExpr, got %T", leftParen.Expr)
				}
				if innerAdd.Op.String() != "+" {
					t.Errorf("Expected paren inner op '+', got %s", innerAdd.Op)
				}
			},
		},

		// ===== Complex Nested Expressions =====
		{
			name: "complex arithmetic: a + b * c - d / e",
			src:  "a + b * c - d / e",
			validate: func(t *testing.T, expr ast.Expression) {
				// Should parse as: (a + (b * c)) - (d / e)
				binExpr, ok := expr.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected *ast.BinaryExpr, got %T", expr)
				}
				if binExpr.Op.String() != "-" {
					t.Errorf("Expected top-level op '-', got %s", binExpr.Op)
				}

				// Left should be (a + (b * c))
				leftAdd, ok := binExpr.Left.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected left to be *ast.BinaryExpr, got %T", binExpr.Left)
				}
				if leftAdd.Op.String() != "+" {
					t.Errorf("Expected left op '+', got %s", leftAdd.Op)
				}

				// Right of left should be (b * c)
				leftRight, ok := leftAdd.Right.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected left.right to be *ast.BinaryExpr, got %T", leftAdd.Right)
				}
				if leftRight.Op.String() != "*" {
					t.Errorf("Expected left.right op '*', got %s", leftRight.Op)
				}

				// Right should be (d / e)
				rightDiv, ok := binExpr.Right.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected right to be *ast.BinaryExpr, got %T", binExpr.Right)
				}
				if rightDiv.Op.String() != "/" {
					t.Errorf("Expected right op '/', got %s", rightDiv.Op)
				}
			},
		},
		{
			name: "complex with functions and arrays: sqrt(arr(i)**2 + arr(j)**2)",
			src:  "sqrt(arr(i)**2 + arr(j)**2)",
			validate: func(t *testing.T, expr ast.Expression) {
				// Top level should be function call
				funcCall, ok := expr.(*ast.FunctionCall)
				if !ok {
					t.Fatalf("Expected *ast.FunctionCall, got %T", expr)
				}
				if funcCall.Name != "sqrt" {
					t.Errorf("Expected function name 'sqrt', got %s", funcCall.Name)
				}
				if len(funcCall.Args) != 1 {
					t.Fatalf("Expected 1 argument, got %d", len(funcCall.Args))
				}

				// Argument should be addition
				argAdd, ok := funcCall.Args[0].(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected argument to be *ast.BinaryExpr, got %T", funcCall.Args[0])
				}
				if argAdd.Op.String() != "+" {
					t.Errorf("Expected argument op '+', got %s", argAdd.Op)
				}

				// Left of addition should be exponentiation
				leftExp, ok := argAdd.Left.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected left to be *ast.BinaryExpr, got %T", argAdd.Left)
				}
				if leftExp.Op.String() != "**" {
					t.Errorf("Expected left op '**', got %s", leftExp.Op)
				}

				// Left of exponentiation should be function call (array reference without symbol table)
				_, ok = leftExp.Left.(*ast.FunctionCall)
				if !ok {
					t.Fatalf("Expected left.left to be *ast.FunctionCall, got %T", leftExp.Left)
				}
			},
		},

		// ===== Array Constructors (F90) =====
		{
			name: "array constructor with single element: (/ 0 /)",
			src:  "(/ 0 /)",
			validate: func(t *testing.T, expr ast.Expression) {
				// Array constructor is parsed as a special kind of expression
				// For now, we just verify it parses without error
				// The structure depends on how the parser handles array constructors
				constr, ok := expr.(*ast.ArrayConstructor)
				if !ok {
					t.Fatalf("failed to convert %T", expr)
				}
				v, ok := constr.Values[0].(*ast.IntegerLiteral)
				if !ok {
					t.Fatalf("failed to convert %T", v)
				} else if v.Raw != "0" {
					t.Errorf("expected raw 0, got %s", v.Raw)
				}
			},
		},
		{
			name: "array constructor in comparison: k == (/ 0 /)",
			src:  "k == (/ 0 /)",
			validate: func(t *testing.T, expr ast.Expression) {
				binExpr, ok := expr.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected *ast.BinaryExpr, got %T", expr)
				}
				if binExpr.Op.String() != "==" {
					t.Errorf("Expected op '==', got %s", binExpr.Op)
				}

				// Left should be identifier 'k'
				ident, ok := binExpr.Left.(*ast.Identifier)
				if !ok {
					t.Fatalf("Expected left to be *ast.Identifier, got %T", binExpr.Left)
				}
				if ident.Value != "k" {
					t.Errorf("Expected identifier 'k', got %s", ident.Value)
				}

				// Right should be the array constructor
				if binExpr.Right == nil {
					t.Fatal("Right side of comparison is nil")
				}
			},
		},
		{
			name: "string concatenation: 'Hello' // ' ' // 'World'",
			src:  "'Hello' // ' ' // 'World'",
			validate: func(t *testing.T, expr ast.Expression) {
				// Should parse as: ('Hello' // ' ') // 'World' (left-associative)
				binExpr, ok := expr.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected *ast.BinaryExpr, got %T", expr)
				}
				if binExpr.Op.String() != "//" {
					t.Errorf("Expected top-level op '//', got %s", binExpr.Op)
				}

				// Left should be another concatenation
				leftConcat, ok := binExpr.Left.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected left to be *ast.BinaryExpr, got %T", binExpr.Left)
				}
				if leftConcat.Op.String() != "//" {
					t.Errorf("Expected left op '//', got %s", leftConcat.Op)
				}

				// Right should be string literal
				_, ok = binExpr.Right.(*ast.StringLiteral)
				if !ok {
					t.Fatalf("Expected right to be *ast.StringLiteral, got %T", binExpr.Right)
				}
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var parser Parser90
			// We'll directly test the parseExpression method
			err := parser.Reset(tt.name+".f90", strings.NewReader(tt.src))
			if err != nil {
				t.Fatalf("Reset failed: %v", err)
			}

			// Reset() already initializes current and peek tokens, so we can parse directly
			// Parse the expression directly
			expr := parser.parseExpression(0)
			if expr == nil {
				t.Fatal("parseExpression returned nil")
			}

			helperFatalErrors(t, &parser, "expected valid expression: "+tt.src)

			// Run the validation function
			tt.validate(t, expr)
		})
	}
}

func helperPrintErrors(t *testing.T, p *Parser90) {
	t.Helper()
	for _, err := range p.Errors() {
		t.Error(&err)
	}
}

func helperFatalErrors(t *testing.T, p *Parser90, msg string) {
	t.Helper()
	helperPrintErrors(t, p)
	if len(p.Errors()) > 0 {
		t.Fatal(msg)
	}
}
