package fortran

import (
	"errors"
	"fmt"
	"math"
	"strings"

	f90 "github.com/soypat/go-fortran/ast"
	f90token "github.com/soypat/go-fortran/token"
)

// Value holds a runtime Fortran value for REPL evaluation.
// Type info comes from varinfo.decl.Type.Token (INTEGER, REAL, LOGICAL, CHARACTER, etc.)
type Value struct {
	tok   f90token.Token
	i64   int64   // INTEGER (all sizes)
	f64   float64 // REAL/DOUBLE PRECISION
	b     bool    // LOGICAL
	s     string  // CHARACTER
	arr   []Value // Array elements (flattened, row-major)
	shape []int   // Array dimensions
	set   bool    // true if value has been set
}

func (v *Value) Token() f90token.Token { return v.tok }
func (v *Value) StringValue() string   { return v.s }
func (v *Value) Bool() bool            { return v.b }
func (v *Value) Float() float64 {
	if v.i64 != 0 {
		return float64(v.i64)
	}
	return v.f64
}

type REPL struct {
	scope     ParserUnitData // currentScope variable data.
	_extern   []*ParserUnitData
	_contains []*ParserUnitData
}

func (tg *REPL) Var(name string) *varinfo {
	return tg.scope.Var(name)
}

func (tg *REPL) AddExtern(pu []f90.ProgramUnit) error {
	for i := range pu {
		data, ok := pu[i].UnitData().(*ParserUnitData)
		if !ok {
			return fmt.Errorf("extern program unit %s has incompatible UnitData", pu[i].UnitName())
		}
		exists := tg.Extern(data.name) != nil
		if exists {
			return fmt.Errorf("extern program unit %s with namespace %s already added", pu[i].UnitName(), data.name)
		}
		tg._extern = append(tg._extern, data)
	}
	return nil
}

func (tg *REPL) Extern(name string) *ParserUnitData {
	for i := range tg._extern {
		if strings.EqualFold(tg._extern[i].name, name) {
			return tg._extern[i]
		}
	}
	return nil
}

func (tg *REPL) Contained(name string) *ParserUnitData {
	for i := range tg._contains {
		if strings.EqualFold(tg._contains[i].name, name) {
			return tg._contains[i]
		}
	}
	return nil
}

func (tg *REPL) ContainedOrExtern(name string) *ParserUnitData {
	data := tg.Contained(name)
	if data == nil {
		data = tg.Extern(name)
	}
	return data
}

func (tg *REPL) ScopeParams() []varinfo {
	for i, v := range tg.scope.vars {
		if !v.flags.HasAny(flagParameter) {
			return tg.scope.vars[:i]
		}
	}
	return tg.scope.vars // all variables are parameters... or no variables.
}

func (tg *REPL) SetScope(pu f90.ProgramUnit) error {
	data, ok := pu.UnitData().(*ParserUnitData)
	if !ok {
		return errors.New("missing parser unit data")
	}

	tg.scope = data.clone()
	for i := range tg.scope.vars {
		tg.scope.vars[i]._varname = sanitizeIdent(tg.scope.vars[i]._varname)
		tg.scope.vars[i].common = sanitizeIdent(tg.scope.vars[i].common)
		tg.scope.vars[i].pointee = sanitizeIdent(tg.scope.vars[i].pointee)
	}

	var toAdd []f90.ProgramUnit
	switch unit := pu.(type) {
	case *f90.ProgramBlock:
		toAdd = unit.Contains
	case *f90.Module:
		toAdd = unit.Contains
	default:
		return nil
	}
	// reset contains on Module or Program block.
	tg._contains = tg._contains[:0]
	for i := range toAdd {
		pu, ok := toAdd[i].UnitData().(*ParserUnitData)
		if !ok {
			return fmt.Errorf("contains program unit %s incompatible unit data", toAdd[i].UnitName())
		}
		exists := tg.Contained(pu.name) != nil
		if exists {
			return fmt.Errorf("contains program unit %s duplicated", toAdd[i].UnitName())
		}
		tg._contains = append(tg._contains, pu)
	}
	return nil
}

// Eval evaluates a Fortran expression and returns a varinfo with the result.
func (r *REPL) Eval(expr f90.Expression) (vi *varinfo, err error) {
	switch e := expr.(type) {
	case *f90.IntegerLiteral:
		vi = r.makeInt(e.Value)
	case *f90.RealLiteral:
		if strings.ContainsAny(e.Raw, "Dd") {
			vi = r.makeFloat64(e.Value)
		} else {
			vi = r.makeFloat32(e.Value)
		}
	case *f90.LogicalLiteral:
		vi = r.makeBool(e.Value)
	case *f90.StringLiteral:
		vi = r.makeString(e.Value)
	case *f90.Identifier:
		vi := r.Var(e.Value)
		if vi == nil {
			err = fmt.Errorf("var %s undefined", e.Value)
		}
	case *f90.UnaryExpr:
		vi, err = r.evalUnary(e)
	case *f90.BinaryExpr:
		vi, err = r.evalBinary(e)
	case *f90.ParenExpr:
		vi, err = r.Eval(e.Expr)
	case *f90.FunctionCall:
		vi, err = r.evalIntrinsic(e)
	default:
		err = fmt.Errorf("unsupported expression: %T", expr)
	}
	if vi != nil && vi.decl != nil {
		vi.val.tok = vi.decl.Type.Token
	}
	return vi, err
}

func (r *REPL) evalUnary(e *f90.UnaryExpr) (*varinfo, error) {
	operand, err := r.Eval(e.Operand)
	if err != nil {
		return nil, err
	}
	switch e.Op {
	case f90token.Plus:
		return operand, nil
	case f90token.Minus:
		if r.isInt(operand) {
			return r.makeInt(-operand.val.i64), nil
		}
		return r.makeFloatLike(operand, -operand.val.f64), nil
	case f90token.NOT:
		return r.makeBool(!operand.val.b), nil
	}
	return nil, fmt.Errorf("unsupported unary operator: %v", e.Op)
}

func (r *REPL) evalBinary(e *f90.BinaryExpr) (*varinfo, error) {
	left, err := r.Eval(e.Left)
	if err != nil {
		return nil, err
	}
	right, err := r.Eval(e.Right)
	if err != nil {
		return nil, err
	}

	// Logical operations
	switch e.Op {
	case f90token.AND:
		return r.makeBool(left.val.b && right.val.b), nil
	case f90token.OR:
		return r.makeBool(left.val.b || right.val.b), nil
	}

	// Comparison operations
	switch e.Op {
	case f90token.EQ, f90token.EqEq:
		return r.makeBool(r.asFloat(left) == r.asFloat(right)), nil
	case f90token.NE, f90token.NotEquals:
		return r.makeBool(r.asFloat(left) != r.asFloat(right)), nil
	case f90token.LT, f90token.Less:
		return r.makeBool(r.asFloat(left) < r.asFloat(right)), nil
	case f90token.LE, f90token.LessEq:
		return r.makeBool(r.asFloat(left) <= r.asFloat(right)), nil
	case f90token.GT, f90token.Greater:
		return r.makeBool(r.asFloat(left) > r.asFloat(right)), nil
	case f90token.GE, f90token.GreaterEq:
		return r.makeBool(r.asFloat(left) >= r.asFloat(right)), nil
	}

	// Integer arithmetic (both operands are integers)
	if r.isInt(left) && r.isInt(right) {
		return r.evalIntBinary(left.val.i64, e.Op, right.val.i64)
	}
	// Float arithmetic (promote if mixed)
	return r.evalFloatBinary(r.asFloat(left), e.Op, r.asFloat(right), r.promoteTypes(left, right))
}

func (r *REPL) evalIntBinary(l int64, op f90token.Token, ri int64) (*varinfo, error) {
	var result int64
	switch op {
	case f90token.Plus:
		result = l + ri
	case f90token.Minus:
		result = l - ri
	case f90token.Asterisk:
		result = l * ri
	case f90token.Slash:
		if ri == 0 {
			return nil, errors.New("division by zero")
		}
		result = l / ri
	case f90token.DoubleStar:
		result = intPow(l, ri)
	default:
		return nil, fmt.Errorf("unsupported int op: %v", op)
	}
	return r.makeInt(result), nil
}

func (r *REPL) evalFloatBinary(l float64, op f90token.Token, rf float64, typ *varinfo) (*varinfo, error) {
	var result float64
	switch op {
	case f90token.Plus:
		result = l + rf
	case f90token.Minus:
		result = l - rf
	case f90token.Asterisk:
		result = l * rf
	case f90token.Slash:
		if rf == 0 {
			return nil, errors.New("division by zero")
		}
		result = l / rf
	case f90token.DoubleStar:
		result = math.Pow(l, rf)
	default:
		return nil, fmt.Errorf("unsupported float op: %v", op)
	}
	return r.makeFloatLike(typ, result), nil
}

func (r *REPL) evalIntrinsic(e *f90.FunctionCall) (*varinfo, error) {
	name := strings.ToUpper(e.Name)
	if len(e.Args) == 0 {
		return nil, fmt.Errorf("%s requires arguments", name)
	}
	arg0, err := r.Eval(e.Args[0])
	if err != nil {
		return nil, err
	}

	f0 := r.asFloat(arg0)
	var result float64
	switch name {
	case "SQRT":
		result = math.Sqrt(f0)
	case "SIN":
		result = math.Sin(f0)
	case "COS":
		result = math.Cos(f0)
	case "TAN":
		result = math.Tan(f0)
	case "ASIN":
		result = math.Asin(f0)
	case "ACOS":
		result = math.Acos(f0)
	case "ATAN":
		result = math.Atan(f0)
	case "EXP":
		result = math.Exp(f0)
	case "LOG":
		result = math.Log(f0)
	case "LOG10":
		result = math.Log10(f0)
	case "ABS":
		result = math.Abs(f0)
	case "REAL":
		return r.makeFloat32(f0), nil
	case "DBLE":
		return r.makeFloat64(f0), nil
	case "INT":
		return r.makeInt(int64(f0)), nil
	case "NINT":
		return r.makeInt(int64(math.Round(f0))), nil
	case "FLOOR":
		return r.makeFloatLike(arg0, math.Floor(f0)), nil
	case "CEILING":
		return r.makeFloatLike(arg0, math.Ceil(f0)), nil
	case "MAX":
		return r.evalMax(e.Args)
	case "MIN":
		return r.evalMin(e.Args)
	default:
		return nil, fmt.Errorf("unknown intrinsic: %s", name)
	}
	return r.makeFloatLike(arg0, result), nil
}

func (r *REPL) evalMax(args []f90.Expression) (*varinfo, error) {
	if len(args) == 0 {
		return nil, errors.New("MAX requires at least one argument")
	}
	result, err := r.Eval(args[0])
	if err != nil {
		return nil, err
	}
	for _, arg := range args[1:] {
		v, err := r.Eval(arg)
		if err != nil {
			return nil, err
		}
		if r.asFloat(v) > r.asFloat(result) {
			result = v
		}
	}
	return result, nil
}

func (r *REPL) evalMin(args []f90.Expression) (*varinfo, error) {
	if len(args) == 0 {
		return nil, errors.New("MIN requires at least one argument")
	}
	result, err := r.Eval(args[0])
	if err != nil {
		return nil, err
	}
	for _, arg := range args[1:] {
		v, err := r.Eval(arg)
		if err != nil {
			return nil, err
		}
		if r.asFloat(v) < r.asFloat(result) {
			result = v
		}
	}
	return result, nil
}

// Helper methods for creating varinfo with values

func (r *REPL) makeInt(v int64) *varinfo {
	vi := &varinfo{decl: _tgtInt32.decl}
	vi.val.i64 = v
	vi.val.set = true
	return vi
}

func (r *REPL) makeFloat32(v float64) *varinfo {
	vi := &varinfo{decl: _tgtFloat32.decl}
	vi.val.f64 = v
	vi.val.set = true
	return vi
}

func (r *REPL) makeFloat64(v float64) *varinfo {
	vi := &varinfo{decl: _tgtFloat64.decl}
	vi.val.f64 = v
	vi.val.set = true
	return vi
}

func (r *REPL) makeBool(v bool) *varinfo {
	vi := &varinfo{decl: _tgtBool.decl}
	vi.val.b = v
	vi.val.set = true
	return vi
}

func (r *REPL) makeString(v string) *varinfo {
	vi := &varinfo{decl: _tgtChar.decl}
	vi.val.s = v
	vi.val.set = true
	return vi
}

func (r *REPL) makeFloatLike(template *varinfo, v float64) *varinfo {
	vi := &varinfo{decl: template.decl}
	vi.val.f64 = v
	vi.val.set = true
	return vi
}

// Type checking helpers

func (r *REPL) isTok(vi *varinfo, tok f90token.Token) bool {
	return vi != nil && vi.decl != nil && vi.decl.Type.Token == tok
}

func (r *REPL) isInt(vi *varinfo) bool {
	return r.isTok(vi, f90token.INTEGER)
}

func (r *REPL) isFloat(vi *varinfo) bool {
	return r.isTok(vi, f90token.REAL) || r.isTok(vi, f90token.DOUBLEPRECISION)
}

func (r *REPL) asFloat(vi *varinfo) float64 {
	if r.isInt(vi) {
		return float64(vi.val.i64)
	}
	return vi.val.f64
}

// promoteTypes returns the wider of two numeric types (Fortran type promotion).
func (r *REPL) promoteTypes(a, b *varinfo) *varinfo {
	if a == nil || a.decl == nil {
		return b
	}
	if b == nil || b.decl == nil {
		return a
	}
	atok := a.decl.Type.Token
	btok := b.decl.Type.Token
	if atok == f90token.DOUBLEPRECISION || btok == f90token.DOUBLEPRECISION {
		return _tgtFloat64
	}
	if atok == f90token.REAL || btok == f90token.REAL {
		return _tgtFloat32
	}
	return a // both int or unknown
}

// intPow computes integer exponentiation.
func intPow(base, exp int64) int64 {
	if exp < 0 {
		return 0 // Integer division truncates negative exponents to 0
	}
	result := int64(1)
	for exp > 0 {
		if exp&1 == 1 {
			result *= base
		}
		base *= base
		exp >>= 1
	}
	return result
}
