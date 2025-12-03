package fortran

import (
	"math"
	"testing"

	f90 "github.com/soypat/go-fortran/ast"
	f90token "github.com/soypat/go-fortran/token"
)

func TestREPL_EvalLiterals(t *testing.T) {
	r := &REPL{}

	t.Run("integer", func(t *testing.T) {
		vi, err := r.Eval(&f90.IntegerLiteral{Value: 42})
		if err != nil {
			t.Fatal(err)
		}
		if !r.isInt(vi) {
			t.Error("expected INTEGER type")
		}
		if vi.val.i64 != 42 {
			t.Errorf("got %d, want 42", vi.val.i64)
		}
	})

	t.Run("real", func(t *testing.T) {
		vi, err := r.Eval(&f90.RealLiteral{Value: 3.14, Raw: "3.14"})
		if err != nil {
			t.Fatal(err)
		}
		if vi.decl.Type.Token != f90token.REAL {
			t.Error("expected REAL type")
		}
		if vi.val.f64 != 3.14 {
			t.Errorf("got %f, want 3.14", vi.val.f64)
		}
	})

	t.Run("double", func(t *testing.T) {
		vi, err := r.Eval(&f90.RealLiteral{Value: 2.718281828, Raw: "2.718281828D0"})
		if err != nil {
			t.Fatal(err)
		}
		if vi.decl.Type.Token != f90token.DOUBLEPRECISION {
			t.Error("expected DOUBLEPRECISION type")
		}
		if vi.val.f64 != 2.718281828 {
			t.Errorf("got %f, want 2.718281828", vi.val.f64)
		}
	})

	t.Run("logical true", func(t *testing.T) {
		vi, err := r.Eval(&f90.LogicalLiteral{Value: true})
		if err != nil {
			t.Fatal(err)
		}
		if vi.decl.Type.Token != f90token.LOGICAL {
			t.Error("expected LOGICAL type")
		}
		if !vi.val.b {
			t.Error("expected true")
		}
	})

	t.Run("logical false", func(t *testing.T) {
		vi, err := r.Eval(&f90.LogicalLiteral{Value: false})
		if err != nil {
			t.Fatal(err)
		}
		if vi.val.b {
			t.Error("expected false")
		}
	})

	t.Run("string", func(t *testing.T) {
		vi, err := r.Eval(&f90.StringLiteral{Value: "hello"})
		if err != nil {
			t.Fatal(err)
		}
		if vi.decl.Type.Token != f90token.CHARACTER {
			t.Error("expected CHARACTER type")
		}
		if vi.val.s != "hello" {
			t.Errorf("got %q, want %q", vi.val.s, "hello")
		}
	})
}

func TestREPL_EvalUnary(t *testing.T) {
	r := &REPL{}

	t.Run("plus int", func(t *testing.T) {
		vi, err := r.Eval(&f90.UnaryExpr{
			Op:      f90token.Plus,
			Operand: &f90.IntegerLiteral{Value: 5},
		})
		if err != nil {
			t.Fatal(err)
		}
		if vi.val.i64 != 5 {
			t.Errorf("got %d, want 5", vi.val.i64)
		}
	})

	t.Run("minus int", func(t *testing.T) {
		vi, err := r.Eval(&f90.UnaryExpr{
			Op:      f90token.Minus,
			Operand: &f90.IntegerLiteral{Value: 7},
		})
		if err != nil {
			t.Fatal(err)
		}
		if vi.val.i64 != -7 {
			t.Errorf("got %d, want -7", vi.val.i64)
		}
	})

	t.Run("minus float", func(t *testing.T) {
		vi, err := r.Eval(&f90.UnaryExpr{
			Op:      f90token.Minus,
			Operand: &f90.RealLiteral{Value: 2.5, Raw: "2.5"},
		})
		if err != nil {
			t.Fatal(err)
		}
		if vi.val.f64 != -2.5 {
			t.Errorf("got %f, want -2.5", vi.val.f64)
		}
	})

	t.Run("not true", func(t *testing.T) {
		vi, err := r.Eval(&f90.UnaryExpr{
			Op:      f90token.NOT,
			Operand: &f90.LogicalLiteral{Value: true},
		})
		if err != nil {
			t.Fatal(err)
		}
		if vi.val.b {
			t.Error("expected false")
		}
	})

	t.Run("not false", func(t *testing.T) {
		vi, err := r.Eval(&f90.UnaryExpr{
			Op:      f90token.NOT,
			Operand: &f90.LogicalLiteral{Value: false},
		})
		if err != nil {
			t.Fatal(err)
		}
		if !vi.val.b {
			t.Error("expected true")
		}
	})
}

func TestREPL_EvalBinaryArithmetic(t *testing.T) {
	r := &REPL{}

	intTests := []struct {
		name   string
		left   int64
		op     f90token.Token
		right  int64
		expect int64
	}{
		{"add", 3, f90token.Plus, 4, 7},
		{"sub", 10, f90token.Minus, 3, 7},
		{"mul", 6, f90token.Asterisk, 7, 42},
		{"div", 20, f90token.Slash, 4, 5},
		{"pow", 2, f90token.DoubleStar, 10, 1024},
		{"pow zero", 5, f90token.DoubleStar, 0, 1},
	}

	for _, tc := range intTests {
		t.Run("int "+tc.name, func(t *testing.T) {
			vi, err := r.Eval(&f90.BinaryExpr{
				Left:  &f90.IntegerLiteral{Value: tc.left},
				Op:    tc.op,
				Right: &f90.IntegerLiteral{Value: tc.right},
			})
			if err != nil {
				t.Fatal(err)
			}
			if !r.isInt(vi) {
				t.Error("expected INTEGER result")
			}
			if vi.val.i64 != tc.expect {
				t.Errorf("got %d, want %d", vi.val.i64, tc.expect)
			}
		})
	}

	floatTests := []struct {
		name   string
		left   float64
		op     f90token.Token
		right  float64
		expect float64
	}{
		{"add", 1.5, f90token.Plus, 2.5, 4.0},
		{"sub", 5.0, f90token.Minus, 1.5, 3.5},
		{"mul", 2.0, f90token.Asterisk, 3.5, 7.0},
		{"div", 7.0, f90token.Slash, 2.0, 3.5},
		{"pow", 2.0, f90token.DoubleStar, 3.0, 8.0},
	}

	for _, tc := range floatTests {
		t.Run("float "+tc.name, func(t *testing.T) {
			vi, err := r.Eval(&f90.BinaryExpr{
				Left:  &f90.RealLiteral{Value: tc.left, Raw: "x"},
				Op:    tc.op,
				Right: &f90.RealLiteral{Value: tc.right, Raw: "x"},
			})
			if err != nil {
				t.Fatal(err)
			}
			if vi.decl.Type.Token != f90token.REAL {
				t.Error("expected REAL result")
			}
			if vi.val.f64 != tc.expect {
				t.Errorf("got %f, want %f", vi.val.f64, tc.expect)
			}
		})
	}
}

func TestREPL_EvalBinaryComparison(t *testing.T) {
	r := &REPL{}

	tests := []struct {
		name   string
		left   int64
		op     f90token.Token
		right  int64
		expect bool
	}{
		{"eq true", 5, f90token.EQ, 5, true},
		{"eq false", 5, f90token.EQ, 6, false},
		{"ne true", 5, f90token.NE, 6, true},
		{"ne false", 5, f90token.NE, 5, false},
		{"lt true", 3, f90token.LT, 5, true},
		{"lt false", 5, f90token.LT, 3, false},
		{"le true eq", 5, f90token.LE, 5, true},
		{"le true lt", 3, f90token.LE, 5, true},
		{"le false", 6, f90token.LE, 5, false},
		{"gt true", 5, f90token.GT, 3, true},
		{"gt false", 3, f90token.GT, 5, false},
		{"ge true eq", 5, f90token.GE, 5, true},
		{"ge true gt", 6, f90token.GE, 5, true},
		{"ge false", 3, f90token.GE, 5, false},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			vi, err := r.Eval(&f90.BinaryExpr{
				Left:  &f90.IntegerLiteral{Value: tc.left},
				Op:    tc.op,
				Right: &f90.IntegerLiteral{Value: tc.right},
			})
			if err != nil {
				t.Fatal(err)
			}
			if vi.decl.Type.Token != f90token.LOGICAL {
				t.Error("expected LOGICAL result")
			}
			if vi.val.b != tc.expect {
				t.Errorf("got %v, want %v", vi.val.b, tc.expect)
			}
		})
	}
}

func TestREPL_EvalBinaryLogical(t *testing.T) {
	r := &REPL{}

	tests := []struct {
		name   string
		left   bool
		op     f90token.Token
		right  bool
		expect bool
	}{
		{"and tt", true, f90token.AND, true, true},
		{"and tf", true, f90token.AND, false, false},
		{"and ft", false, f90token.AND, true, false},
		{"and ff", false, f90token.AND, false, false},
		{"or tt", true, f90token.OR, true, true},
		{"or tf", true, f90token.OR, false, true},
		{"or ft", false, f90token.OR, true, true},
		{"or ff", false, f90token.OR, false, false},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			vi, err := r.Eval(&f90.BinaryExpr{
				Left:  &f90.LogicalLiteral{Value: tc.left},
				Op:    tc.op,
				Right: &f90.LogicalLiteral{Value: tc.right},
			})
			if err != nil {
				t.Fatal(err)
			}
			if vi.val.b != tc.expect {
				t.Errorf("got %v, want %v", vi.val.b, tc.expect)
			}
		})
	}
}

func TestREPL_EvalTypePromotion(t *testing.T) {
	r := &REPL{}

	t.Run("int + real = real", func(t *testing.T) {
		vi, err := r.Eval(&f90.BinaryExpr{
			Left:  &f90.IntegerLiteral{Value: 2},
			Op:    f90token.Plus,
			Right: &f90.RealLiteral{Value: 3.5, Raw: "3.5"},
		})
		if err != nil {
			t.Fatal(err)
		}
		if vi.decl.Type.Token != f90token.REAL {
			t.Error("expected REAL result from int+real")
		}
		if vi.val.f64 != 5.5 {
			t.Errorf("got %f, want 5.5", vi.val.f64)
		}
	})

	t.Run("real + double = double", func(t *testing.T) {
		vi, err := r.Eval(&f90.BinaryExpr{
			Left:  &f90.RealLiteral{Value: 1.0, Raw: "1.0"},
			Op:    f90token.Plus,
			Right: &f90.RealLiteral{Value: 2.0, Raw: "2.0D0"},
		})
		if err != nil {
			t.Fatal(err)
		}
		if vi.decl.Type.Token != f90token.DOUBLEPRECISION {
			t.Error("expected DOUBLEPRECISION result from real+double")
		}
	})
}

func TestREPL_EvalIntrinsics(t *testing.T) {
	r := &REPL{}
	const tol = 1e-10

	mathTests := []struct {
		name   string
		arg    float64
		expect float64
	}{
		{"SQRT", 4.0, 2.0},
		{"SIN", 0.0, 0.0},
		{"COS", 0.0, 1.0},
		{"TAN", 0.0, 0.0},
		{"EXP", 0.0, 1.0},
		{"LOG", 1.0, 0.0},
		{"LOG10", 10.0, 1.0},
		{"ABS", -5.0, 5.0},
		{"ASIN", 0.0, 0.0},
		{"ACOS", 1.0, 0.0},
		{"ATAN", 0.0, 0.0},
	}

	for _, tc := range mathTests {
		t.Run(tc.name, func(t *testing.T) {
			vi, err := r.Eval(&f90.FunctionCall{
				Name: tc.name,
				Args: []f90.Expression{&f90.RealLiteral{Value: tc.arg, Raw: "x"}},
			})
			if err != nil {
				t.Fatal(err)
			}
			if math.Abs(vi.val.f64-tc.expect) > tol {
				t.Errorf("got %f, want %f", vi.val.f64, tc.expect)
			}
		})
	}

	t.Run("REAL", func(t *testing.T) {
		vi, err := r.Eval(&f90.FunctionCall{
			Name: "REAL",
			Args: []f90.Expression{&f90.IntegerLiteral{Value: 42}},
		})
		if err != nil {
			t.Fatal(err)
		}
		if vi.decl.Type.Token != f90token.REAL {
			t.Error("expected REAL type")
		}
		if vi.val.f64 != 42.0 {
			t.Errorf("got %f, want 42.0", vi.val.f64)
		}
	})

	t.Run("DBLE", func(t *testing.T) {
		vi, err := r.Eval(&f90.FunctionCall{
			Name: "DBLE",
			Args: []f90.Expression{&f90.IntegerLiteral{Value: 42}},
		})
		if err != nil {
			t.Fatal(err)
		}
		if vi.decl.Type.Token != f90token.DOUBLEPRECISION {
			t.Error("expected DOUBLEPRECISION type")
		}
	})

	t.Run("INT", func(t *testing.T) {
		vi, err := r.Eval(&f90.FunctionCall{
			Name: "INT",
			Args: []f90.Expression{&f90.RealLiteral{Value: 3.7, Raw: "3.7"}},
		})
		if err != nil {
			t.Fatal(err)
		}
		if !r.isInt(vi) {
			t.Error("expected INTEGER type")
		}
		if vi.val.i64 != 3 {
			t.Errorf("got %d, want 3", vi.val.i64)
		}
	})

	t.Run("NINT", func(t *testing.T) {
		vi, err := r.Eval(&f90.FunctionCall{
			Name: "NINT",
			Args: []f90.Expression{&f90.RealLiteral{Value: 3.7, Raw: "3.7"}},
		})
		if err != nil {
			t.Fatal(err)
		}
		if vi.val.i64 != 4 {
			t.Errorf("got %d, want 4", vi.val.i64)
		}
	})

	t.Run("FLOOR", func(t *testing.T) {
		vi, err := r.Eval(&f90.FunctionCall{
			Name: "FLOOR",
			Args: []f90.Expression{&f90.RealLiteral{Value: 3.7, Raw: "3.7"}},
		})
		if err != nil {
			t.Fatal(err)
		}
		if vi.val.f64 != 3.0 {
			t.Errorf("got %f, want 3.0", vi.val.f64)
		}
	})

	t.Run("CEILING", func(t *testing.T) {
		vi, err := r.Eval(&f90.FunctionCall{
			Name: "CEILING",
			Args: []f90.Expression{&f90.RealLiteral{Value: 3.2, Raw: "3.2"}},
		})
		if err != nil {
			t.Fatal(err)
		}
		if vi.val.f64 != 4.0 {
			t.Errorf("got %f, want 4.0", vi.val.f64)
		}
	})
}

func TestREPL_EvalMaxMin(t *testing.T) {
	r := &REPL{}

	t.Run("MAX", func(t *testing.T) {
		vi, err := r.Eval(&f90.FunctionCall{
			Name: "MAX",
			Args: []f90.Expression{
				&f90.IntegerLiteral{Value: 3},
				&f90.IntegerLiteral{Value: 7},
				&f90.IntegerLiteral{Value: 2},
			},
		})
		if err != nil {
			t.Fatal(err)
		}
		if vi.val.i64 != 7 {
			t.Errorf("got %d, want 7", vi.val.i64)
		}
	})

	t.Run("MIN", func(t *testing.T) {
		vi, err := r.Eval(&f90.FunctionCall{
			Name: "MIN",
			Args: []f90.Expression{
				&f90.IntegerLiteral{Value: 3},
				&f90.IntegerLiteral{Value: 7},
				&f90.IntegerLiteral{Value: 2},
			},
		})
		if err != nil {
			t.Fatal(err)
		}
		if vi.val.i64 != 2 {
			t.Errorf("got %d, want 2", vi.val.i64)
		}
	})
}

func TestREPL_EvalParen(t *testing.T) {
	r := &REPL{}

	// (2 + 3) * 4 = 20
	vi, err := r.Eval(&f90.BinaryExpr{
		Left: &f90.ParenExpr{
			Expr: &f90.BinaryExpr{
				Left:  &f90.IntegerLiteral{Value: 2},
				Op:    f90token.Plus,
				Right: &f90.IntegerLiteral{Value: 3},
			},
		},
		Op:    f90token.Asterisk,
		Right: &f90.IntegerLiteral{Value: 4},
	})
	if err != nil {
		t.Fatal(err)
	}
	if vi.val.i64 != 20 {
		t.Errorf("got %d, want 20", vi.val.i64)
	}
}

func TestREPL_EvalErrors(t *testing.T) {
	r := &REPL{}

	t.Run("division by zero int", func(t *testing.T) {
		_, err := r.Eval(&f90.BinaryExpr{
			Left:  &f90.IntegerLiteral{Value: 5},
			Op:    f90token.Slash,
			Right: &f90.IntegerLiteral{Value: 0},
		})
		if err == nil {
			t.Error("expected division by zero error")
		}
	})

	t.Run("division by zero float", func(t *testing.T) {
		_, err := r.Eval(&f90.BinaryExpr{
			Left:  &f90.RealLiteral{Value: 5.0, Raw: "5.0"},
			Op:    f90token.Slash,
			Right: &f90.RealLiteral{Value: 0.0, Raw: "0.0"},
		})
		if err == nil {
			t.Error("expected division by zero error")
		}
	})

	t.Run("undefined variable", func(t *testing.T) {
		// Need a scope to test undefined variable (otherwise r.Var panics)
		var rWithScope REPL
		_, err := rWithScope.Eval(&f90.Identifier{Value: "UNDEFINED"})
		if err == nil {
			t.Error("expected undefined variable error")
		}
	})

	t.Run("unknown intrinsic", func(t *testing.T) {
		_, err := r.Eval(&f90.FunctionCall{
			Name: "NOTAFUNCTION",
			Args: []f90.Expression{&f90.IntegerLiteral{Value: 1}},
		})
		if err == nil {
			t.Error("expected unknown intrinsic error")
		}
	})

	t.Run("intrinsic no args", func(t *testing.T) {
		_, err := r.Eval(&f90.FunctionCall{
			Name: "SQRT",
			Args: []f90.Expression{},
		})
		if err == nil {
			t.Error("expected requires arguments error")
		}
	})
}

func TestREPL_IntPow(t *testing.T) {
	tests := []struct {
		base, exp, want int64
	}{
		{2, 0, 1},
		{2, 1, 2},
		{2, 10, 1024},
		{3, 4, 81},
		{5, 3, 125},
		{2, -1, 0}, // negative exponent returns 0
		{0, 0, 1},  // 0^0 = 1
		{0, 5, 0},
	}

	for _, tc := range tests {
		got := intPow(tc.base, tc.exp)
		if got != tc.want {
			t.Errorf("intPow(%d, %d) = %d, want %d", tc.base, tc.exp, got, tc.want)
		}
	}
}
