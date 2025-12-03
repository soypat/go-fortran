package fortran

import (
	"math"
	"strconv"
	"testing"

	f90 "github.com/soypat/go-fortran/ast"
	f90token "github.com/soypat/go-fortran/token"
)

func TestREPL_EvalResultTypes(t *testing.T) {
	tests := []struct {
		expr f90.Expression
		want f90token.Token
	}{
		0: {exprInt(42), f90token.INTEGER},
		1: {exprFloat(3.14), f90token.REAL},
		2: {exprDouble(2.718), f90token.DOUBLEPRECISION},
		3: {&f90.LogicalLiteral{Value: true}, f90token.LOGICAL},
		4: {&f90.StringLiteral{Value: "hello"}, f90token.CHARACTER},
	}
	var r REPL
	for i, test := range tests {
		var vi varinfo
		err := r.Eval(&vi, test.expr)
		if err != nil {
			t.Fatalf("%d: %s", i, err)
		}
		val := vi.Value()
		got := val.Token()
		if got != test.want {
			t.Errorf("%d: got %v, want %v", i, got, test.want)
		}
	}
}

func TestREPL_EvalLogicalExpressions(t *testing.T) {
	tests := []struct {
		expr f90.Expression
		want bool
	}{
		0: {&f90.LogicalLiteral{Value: true}, true},
		1: {&f90.LogicalLiteral{Value: false}, false},
		2: {&f90.UnaryExpr{Op: f90token.NOT, Operand: &f90.LogicalLiteral{Value: true}}, false},
		3: {&f90.UnaryExpr{Op: f90token.NOT, Operand: &f90.LogicalLiteral{Value: false}}, true},
		// Binary logical
		4:  {binLogical(f90token.AND, true, true), true},
		5:  {binLogical(f90token.AND, true, false), false},
		6:  {binLogical(f90token.AND, false, true), false},
		7:  {binLogical(f90token.AND, false, false), false},
		8:  {binLogical(f90token.OR, true, true), true},
		9:  {binLogical(f90token.OR, true, false), true},
		10: {binLogical(f90token.OR, false, true), true},
		11: {binLogical(f90token.OR, false, false), false},
		// Comparisons
		12: {binInt(f90token.EQ, 5, 5), true},
		13: {binInt(f90token.EQ, 5, 6), false},
		14: {binInt(f90token.NE, 5, 6), true},
		15: {binInt(f90token.NE, 5, 5), false},
		16: {binInt(f90token.LT, 3, 5), true},
		17: {binInt(f90token.LT, 5, 3), false},
		18: {binInt(f90token.LE, 5, 5), true},
		19: {binInt(f90token.LE, 3, 5), true},
		20: {binInt(f90token.LE, 6, 5), false},
		21: {binInt(f90token.GT, 5, 3), true},
		22: {binInt(f90token.GT, 3, 5), false},
		23: {binInt(f90token.GE, 5, 5), true},
		24: {binInt(f90token.GE, 6, 5), true},
		25: {binInt(f90token.GE, 3, 5), false},
	}
	var r REPL
	for i, test := range tests {
		var vi varinfo
		err := r.Eval(&vi, test.expr)
		if err != nil {
			t.Fatalf("%d: %s", i, err)
		}
		val := vi.Value()
		got := val.Bool()
		if got != test.want {
			t.Errorf("%d: got %v, want %v", i, got, test.want)
		}
	}
}

func TestREPL_EvalStringExpressions(t *testing.T) {
	tests := []struct {
		expr f90.Expression
		want string
	}{
		0: {&f90.StringLiteral{Value: "hello"}, "hello"},
		1: {&f90.StringLiteral{Value: ""}, ""},
		2: {&f90.StringLiteral{Value: "hello world"}, "hello world"},
	}
	var r REPL
	for i, test := range tests {
		var vi varinfo
		err := r.Eval(&vi, test.expr)
		if err != nil {
			t.Fatalf("%d: %s", i, err)
		}
		val := vi.Value()
		got := val.StringValue()
		if got != test.want {
			t.Errorf("%d: got %q, want %q", i, got, test.want)
		}
	}
}

func TestREPL_EvalErrors(t *testing.T) {
	tests := []struct {
		expr f90.Expression
	}{
		0: {binInt(f90token.Slash, 5, 0)},                                                // division by zero int
		1: {binFloat(f90token.Slash, 5.0, 0.0)},                                          // division by zero float
		2: {&f90.Identifier{Value: "UNDEFINED"}},                                         // undefined variable
		3: {&f90.FunctionCall{Name: "NOTAFUNCTION", Args: []f90.Expression{exprInt(1)}}}, // unknown intrinsic
		4: {&f90.FunctionCall{Name: "SQRT", Args: []f90.Expression{}}},                   // intrinsic no args
	}
	var r REPL
	for i, test := range tests {
		var vi varinfo
		err := r.Eval(&vi, test.expr)
		if err == nil {
			t.Errorf("%d: expected error for %q", i, test.expr.AppendString(nil))
		}
	}
}

func TestREPL_ConstantNumericExpressions(t *testing.T) {
	tests := []struct {
		expr f90.Expression
		want float64
	}{
		// Integer arithmetic
		0: {binInt(f90token.Plus, 3, 4), 7},
		1: {binInt(f90token.Minus, 10, 3), 7},
		2: {binInt(f90token.Asterisk, 6, 7), 42},
		3: {binInt(f90token.Slash, 20, 4), 5},
		4: {binInt(f90token.DoubleStar, 2, 10), 1024},
		5: {binInt(f90token.DoubleStar, 5, 0), 1},
		// Float arithmetic
		6:  {binFloat(f90token.Plus, 1.5, 2.5), 4.0},
		7:  {binFloat(f90token.Minus, 5.0, 1.5), 3.5},
		8:  {binFloat(f90token.Asterisk, 2.0, 3.5), 7.0},
		9:  {binFloat(f90token.Slash, 7.0, 2.0), 3.5},
		10: {binFloat(f90token.DoubleStar, 2.0, 3.0), 8.0},
		// Type promotion (int + real)
		11: {binMixed(f90token.Plus, 2, 3.5), 5.5},
		// Intrinsics
		12: {intrinsicF(4.0, "SQRT"), 2.0},
		13: {intrinsicF(0.0, "SIN"), 0.0},
		14: {intrinsicF(0.0, "COS"), 1.0},
		15: {intrinsicF(0.0, "TAN"), 0.0},
		16: {intrinsicF(0.0, "EXP"), 1.0},
		17: {intrinsicF(1.0, "LOG"), 0.0},
		18: {intrinsicF(10.0, "LOG10"), 1.0},
		19: {intrinsicF(-5.0, "ABS"), 5.0},
		20: {intrinsicF(0.0, "ASIN"), 0.0},
		21: {intrinsicF(1.0, "ACOS"), 0.0},
		22: {intrinsicF(0.0, "ATAN"), 0.0},
		23: {intrinsicF(3.7, "FLOOR"), 3.0},
		24: {intrinsicF(3.2, "CEILING"), 4.0},
		// MAX/MIN with integers
		25: {maxInt(3, 7, 2), 7},
		26: {minInt(3, 7, 2), 2},
		// Parenthesized expression: (2+3)*4 = 20
		27: {&f90.BinaryExpr{Left: &f90.ParenExpr{Expr: binInt(f90token.Plus, 2, 3)}, Op: f90token.Asterisk, Right: exprInt(4)}, 20},
		// Unary numeric
		28: {&f90.UnaryExpr{Op: f90token.Plus, Operand: exprInt(5)}, 5},
		29: {&f90.UnaryExpr{Op: f90token.Minus, Operand: exprInt(7)}, -7},
		30: {&f90.UnaryExpr{Op: f90token.Minus, Operand: exprFloat(2.5)}, -2.5},
		// Type casts
		31: {intrinsicF(3.7, "INT"), 3},
		32: {intrinsicF(3.7, "NINT"), 4},
		33: {intrinsicF(42, "REAL"), 42},
		34: {intrinsicF(42, "DBLE"), 42},
		// Type promotion real + double
		35: {binDouble(f90token.Plus, 1.0, 2.0), 3.0},
	}
	const tol = 1e-10
	var r REPL
	for i, test := range tests {
		var v varinfo
		err := r.Eval(&v, test.expr)
		if err != nil {
			t.Fatalf("%d %s: %q", i, err, test.expr.AppendString(nil))
		}
		value := v.Value()
		got := value.Float()
		want := test.want
		diff := math.Abs(got - want)
		if want != 0 && diff/math.Abs(want) > tol {
			t.Fatalf("%d got %f, want %f (diff %f)", i, got, want, diff)
		} else if want == 0 && diff > tol {
			t.Fatalf("%d got %f, want %f (diff %f)", i, got, want, diff)
		}
	}
}

func exprInt(v int64) *f90.IntegerLiteral {
	return &f90.IntegerLiteral{Value: v, Raw: strconv.FormatInt(v, 10)}
}

func exprFloat(v float64) *f90.RealLiteral {
	return &f90.RealLiteral{Value: v, Raw: strconv.FormatFloat(v, 'f', 16, 64)}
}

func binInt(op f90token.Token, l, r int64) *f90.BinaryExpr {
	return &f90.BinaryExpr{Op: op, Left: exprInt(l), Right: exprInt(r)}
}

func binFloat(op f90token.Token, l, r float64) *f90.BinaryExpr {
	return &f90.BinaryExpr{Op: op, Left: exprFloat(l), Right: exprFloat(r)}
}

func binMixed(op f90token.Token, l int64, r float64) *f90.BinaryExpr {
	return &f90.BinaryExpr{Op: op, Left: exprInt(l), Right: exprFloat(r)}
}

func binDouble(op f90token.Token, l, r float64) *f90.BinaryExpr {
	return &f90.BinaryExpr{Op: op, Left: exprFloat(l), Right: exprDouble(r)}
}

func exprDouble(v float64) *f90.RealLiteral {
	return &f90.RealLiteral{Value: v, Raw: strconv.FormatFloat(v, 'f', 16, 64) + "D0"}
}

func intrinsicF(arg float64, name string) *f90.FunctionCall {
	return &f90.FunctionCall{Name: name, Args: []f90.Expression{exprFloat(arg)}}
}

func maxInt(vals ...int64) *f90.FunctionCall {
	args := make([]f90.Expression, len(vals))
	for i, v := range vals {
		args[i] = exprInt(v)
	}
	return &f90.FunctionCall{Name: "MAX", Args: args}
}

func minInt(vals ...int64) *f90.FunctionCall {
	args := make([]f90.Expression, len(vals))
	for i, v := range vals {
		args[i] = exprInt(v)
	}
	return &f90.FunctionCall{Name: "MIN", Args: args}
}

func binLogical(op f90token.Token, l, r bool) *f90.BinaryExpr {
	return &f90.BinaryExpr{Op: op, Left: &f90.LogicalLiteral{Value: l}, Right: &f90.LogicalLiteral{Value: r}}
}
