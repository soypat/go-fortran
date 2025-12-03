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
//
// Two type concepts exist in varinfo:
//   - val.tok (Value.Token): The evaluated result's runtime type
//   - decl.Type.Token: The declared type from AST
//
// For declared variables both match. For ephemeral results (intermediates
// like 2+3.0), decl comes from templates (_tgtFloat32 etc), val.tok tracks
// the actual evaluated type after promotion.
type Value struct {
	tok   f90token.Token // Evaluated type token. Can be used to check if set in REPL.
	i64   int64          // INTEGER (all sizes)
	f64   float64        // REAL/DOUBLE PRECISION
	b     bool           // LOGICAL
	s     string         // CHARACTER
	arr   []Value        // Array elements (flattened, row-major)
	shape []int          // Array dimensions
}

func (v *Value) Token() f90token.Token { return v.tok }
func (v *Value) StringValue() string   { return v.s }
func (v *Value) Bool() bool            { return v.b }
func (v *Value) Int() int64            { return v.i64 }
func (v *Value) Floatlike() bool       { return v.tok == f90token.REAL || v.tok == f90token.DOUBLEPRECISION }
func (v *Value) IsInt() bool           { return v.tok == f90token.INTEGER }
func (v *Value) Float() float64 {
	if v.tok == f90token.INTEGER {
		return float64(v.i64)
	}
	return v.f64
}

type REPL struct {
	scope     ParserUnitData // currentScope variable data.
	_extern   []*ParserUnitData
	_contains []*ParserUnitData
}

func (repl *REPL) Var(name string) *varinfo {
	return repl.scope.Var(name)
}

func (repl *REPL) AddExtern(pu []f90.ProgramUnit) error {
	for i := range pu {
		data, ok := pu[i].UnitData().(*ParserUnitData)
		if !ok {
			return fmt.Errorf("extern program unit %s has incompatible UnitData", pu[i].UnitName())
		}
		exists := repl.Extern(data.name) != nil
		if exists {
			return fmt.Errorf("extern program unit %s with namespace %s already added", pu[i].UnitName(), data.name)
		}
		repl._extern = append(repl._extern, data)
	}
	return nil
}

func (repl *REPL) Extern(name string) *ParserUnitData {
	for i := range repl._extern {
		if strings.EqualFold(repl._extern[i].name, name) {
			return repl._extern[i]
		}
	}
	return nil
}

func (repl *REPL) Contained(name string) *ParserUnitData {
	for i := range repl._contains {
		if strings.EqualFold(repl._contains[i].name, name) {
			return repl._contains[i]
		}
	}
	return nil
}

func (repl *REPL) ContainedOrExtern(name string) *ParserUnitData {
	data := repl.Contained(name)
	if data == nil {
		data = repl.Extern(name)
	}
	return data
}

func (repl *REPL) ScopeParams() []varinfo {
	for i, v := range repl.scope.vars {
		if !v.flags.HasAny(flagParameter) {
			return repl.scope.vars[:i]
		}
	}
	return repl.scope.vars // all variables are parameters... or no variables.
}

func (repl *REPL) SetScope(pu f90.ProgramUnit) error {
	data, ok := pu.UnitData().(*ParserUnitData)
	if !ok {
		return errors.New("missing parser unit data")
	}

	repl.scope = data.clone()
	for i := range repl.scope.vars {
		v := &repl.scope.vars[i]
		if v.decl == nil {
			return fmt.Errorf("unresolved declaration for variable %s in %s", v.Identifier(), pu.UnitName())
		}
		v.val.tok = v.decl.Type.Token // Initialize repl value type.
		v._varname = sanitizeIdent(v._varname)
		v.common = sanitizeIdent(v.common)
		v.pointee = sanitizeIdent(v.pointee)
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
	repl._contains = repl._contains[:0]
	for i := range toAdd {
		pu, ok := toAdd[i].UnitData().(*ParserUnitData)
		if !ok {
			return fmt.Errorf("contains program unit %s incompatible unit data", toAdd[i].UnitName())
		}
		exists := repl.Contained(pu.name) != nil
		if exists {
			return fmt.Errorf("contains program unit %s duplicated", toAdd[i].UnitName())
		}
		repl._contains = append(repl._contains, pu)
	}
	return nil
}

// Eval evaluates a Fortran expression into dst. Caller provides dst to avoid
// allocations; dst can be reused across calls. On success dst.Value() holds
// the result with val.tok set to the evaluated type.
func (repl *REPL) Eval(dst *varinfo, expr f90.Expression) (err error) {
	switch e := expr.(type) {
	case *f90.IntegerLiteral:
		err = repl.assignInt(dst, e.Value)
	case *f90.RealLiteral:
		if strings.ContainsAny(e.Raw, "Dd") {
			err = repl.assignFloat64(dst, e.Value)
		} else {
			err = repl.assignFloat32(dst, e.Value)
		}
	case *f90.LogicalLiteral:
		err = repl.assignBool(dst, e.Value)
	case *f90.StringLiteral:
		err = repl.assignString(dst, e.Value)
	case *f90.Identifier:
		vi := repl.Var(e.Value)
		if vi == nil {
			err = fmt.Errorf("var %s undefined", e.Value)
		} else {
			*dst = *vi
		}
	case *f90.UnaryExpr:
		err = repl.evalUnary(dst, e)
	case *f90.BinaryExpr:
		err = repl.evalBinary(dst, e)
	case *f90.ParenExpr:
		err = repl.Eval(dst, e.Expr)
	case *f90.FunctionCall:
		err = repl.evalIntrinsic(dst, e)
	default:
		err = fmt.Errorf("unsupported expression: %T", expr)
	}
	return err
}

// InferType infers the type of expr without evaluating it.
// Sets dst.decl to indicate the result type.
func (repl *REPL) InferType(dst *varinfo, expr f90.Expression) error {
	*dst = varinfo{}
	return repl.Eval(dst, expr)
}

func (repl *REPL) evalUnary(dst *varinfo, e *f90.UnaryExpr) error {
	err := repl.Eval(dst, e.Operand)
	if err != nil {
		return err
	}
	switch e.Op {
	case f90token.Plus:
		return nil // No-op.
	case f90token.Minus:
		if dst.val.IsInt() {
			err = repl.assignInt(dst, -dst.val.i64)
		} else {
			err = repl.assignFloatLike(dst, dst, -dst.val.f64)
		}
	case f90token.NOT:
		err = repl.assignBool(dst, !dst.val.b)
	default:
		err = fmt.Errorf("unsupported unary operator %q: %v", e.AppendString(nil), e.Op)
	}
	return err
}

func (repl *REPL) evalBinary(dst *varinfo, e *f90.BinaryExpr) error {
	var left, right varinfo
	err := repl.Eval(&left, e.Left)
	if err != nil {
		return err
	}
	err = repl.Eval(&right, e.Right)
	if err != nil {
		return err
	}
	switch e.Op {
	// Logical operations
	case f90token.AND:
		err = repl.assignBool(dst, left.val.b && right.val.b)
	case f90token.OR:
		err = repl.assignBool(dst, left.val.b || right.val.b)

	// Comparison operations
	case f90token.EQ, f90token.EqEq:
		err = repl.assignBool(dst, left.val.Float() == right.val.Float())
	case f90token.NE, f90token.NotEquals:
		err = repl.assignBool(dst, left.val.Float() != right.val.Float())
	case f90token.LT, f90token.Less:
		err = repl.assignBool(dst, left.val.Float() < right.val.Float())
	case f90token.LE, f90token.LessEq:
		err = repl.assignBool(dst, left.val.Float() <= right.val.Float())
	case f90token.GT, f90token.Greater:
		err = repl.assignBool(dst, left.val.Float() > right.val.Float())
	case f90token.GE, f90token.GreaterEq:
		err = repl.assignBool(dst, left.val.Float() >= right.val.Float())
	default:
		// Integer arithmetic (both operands are integers)
		if left.val.Token() == f90token.INTEGER && right.val.Token() == f90token.INTEGER {
			err = repl.evalIntBinary(dst, left.val.i64, e.Op, right.val.i64)
		} else {
			err = repl.evalFloatBinary(dst, repl.promoteTypes(&left, &right), left.val.Float(), e.Op, right.val.Float())
		}
	}
	if err != nil {
		return fmt.Errorf("binary operation %s failed: %w", e.AppendString(nil), err)
	}
	return nil
}

func (repl *REPL) evalIntBinary(dst *varinfo, l int64, op f90token.Token, ri int64) error {
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
			return errors.New("division by zero")
		}
		result = l / ri
	case f90token.DoubleStar:
		result = intPow(l, ri)
	default:
		return fmt.Errorf("unsupported int op: %v", op)
	}
	return repl.assignInt(dst, result)
}

func (repl *REPL) evalFloatBinary(dst, typ *varinfo, l float64, op f90token.Token, rf float64) error {
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
			return errors.New("division by zero")
		}
		result = l / rf
	case f90token.DoubleStar:
		result = math.Pow(l, rf)
	default:
		return fmt.Errorf("unsupported float op: %v", op)
	}
	return repl.assignFloatLike(dst, typ, result)
}

func (repl *REPL) evalIntrinsic(dst *varinfo, e *f90.FunctionCall) error {
	name := strings.ToUpper(e.Name)
	if len(e.Args) == 0 {
		return fmt.Errorf("%s requires arguments", name)
	}
	var arg0 varinfo
	err := repl.Eval(&arg0, e.Args[0])
	if err != nil {
		return err
	}

	f0 := arg0.val.Float()
	var result float64
	isFn0 := true
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
	default:
		isFn0 = false
	}
	if isFn0 {
		return repl.assignFloatLike(dst, &arg0, result)
	}
	switch name {
	case "REAL":
		err = repl.assignFloat32(dst, f0)
	case "DBLE":
		err = repl.assignFloat64(dst, f0)
	case "INT":
		err = repl.assignInt(dst, int64(f0))
	case "NINT":
		err = repl.assignInt(dst, int64(math.Round(f0)))
	case "FLOOR":
		err = repl.assignFloatLike(dst, &arg0, math.Floor(f0))
	case "CEILING":
		err = repl.assignFloatLike(dst, &arg0, math.Ceil(f0))
	case "MAX":
		err = repl.evalMax(dst, e.Args)
	case "MIN":
		err = repl.evalMin(dst, e.Args)
	default:
		err = fmt.Errorf("unknown intrinsic: %s", name)
	}
	return err
}

func (repl *REPL) evalMax(dst *varinfo, args []f90.Expression) error {
	if len(args) == 0 {
		return errors.New("MAX requires at least one argument")
	}
	maxSoFar := dst
	err := repl.Eval(maxSoFar, args[0])
	if err != nil {
		return err
	}
	var next varinfo
	for _, arg := range args[1:] {
		err = repl.Eval(&next, arg)
		if err != nil {
			return err
		}
		if next.val.Float() > maxSoFar.val.Float() {
			*maxSoFar = next
		}
	}
	return nil
}

func (repl *REPL) evalMin(dst *varinfo, args []f90.Expression) error {
	if len(args) == 0 {
		return errors.New("MIN requires at least one argument")
	}
	minSoFar := dst
	err := repl.Eval(minSoFar, args[0])
	if err != nil {
		return err
	}
	var next varinfo
	for _, arg := range args[1:] {
		err := repl.Eval(&next, arg)
		if err != nil {
			return err
		}
		if next.val.Float() < minSoFar.val.Float() {
			*minSoFar = next
		}
	}
	return nil
}

// Helper methods for creating varinfo with values
func (repl *REPL) prepAssignment(dst *varinfo, src *varinfo) error {
	if dst.decl == nil {
		// Declaration of dst should remain unset. Conceptually it's declaration is ephemeral, part of a REPL evaluation.
	} else if dst.decl.Type.Token != src.decl.Type.Token {
		return fmt.Errorf("destination variable %q of type %s not assignable with type %s", dst.Identifier(), dst.decl.Type.Token.String(), src.decl.Type.Token.String())
	}
	dst.val.tok = src.decl.Type.Token
	return nil
}

// promote returns the resulting promoted type of a binary operation between two types.
func (repl *REPL) promote(dst, src *varinfo) (promotion f90token.Token) {
	switch dst.val.tok {
	case 0:
		// dst unset, use src type (prefer val.tok, fall back to decl)
		if src.val.tok != 0 {
			promotion = src.val.tok
		} else if src.decl != nil {
			promotion = src.decl.Type.Token
		}
	case f90token.DOUBLEPRECISION:
		if src.val.IsInt() || src.val.Floatlike() {
			promotion = f90token.DOUBLEPRECISION
		}
	case f90token.REAL:
		if src.val.IsInt() || src.val.tok == f90token.REAL {
			promotion = f90token.REAL
		} else if src.val.tok == f90token.DOUBLEPRECISION {
			promotion = f90token.DOUBLEPRECISION
		}
	case f90token.INTEGER:
		if src.val.IsInt() {
			promotion = f90token.INTEGER
		}
	default:
		if src.val.tok == dst.val.tok {
			promotion = src.val.tok
		}
	}
	return promotion
}

func (repl *REPL) assignInt(dst *varinfo, v int64) error {
	err := repl.prepAssignment(dst, _tgtInt32)
	if err != nil {
		return err
	}
	dst.val.i64 = v
	return nil
}

func (repl *REPL) assignFloat32(dst *varinfo, v float64) error {
	err := repl.prepAssignment(dst, _tgtFloat32)
	if err != nil {
		return err
	}
	dst.val.f64 = v
	return nil
}

func (repl *REPL) assignFloat64(dst *varinfo, v float64) error {
	err := repl.prepAssignment(dst, _tgtFloat64)
	if err != nil {
		return err
	}
	dst.val.f64 = v
	return nil
}

func (repl *REPL) assignBool(dst *varinfo, v bool) error {
	err := repl.prepAssignment(dst, _tgtBool)
	if err != nil {
		return err
	}
	dst.val.b = v
	return nil
}

func (repl *REPL) assignString(dst *varinfo, v string) error {
	err := repl.prepAssignment(dst, _tgtChar)
	if err != nil {
		return err
	}
	dst.val.s = v
	return nil
}

func (repl *REPL) assignFloatLike(dst, template *varinfo, v float64) error {
	dst.val.tok = repl.promote(dst, template)
	dst.val.f64 = v
	return nil
}

// Type checking helpers

// promoteTypes returns the wider of two numeric types (Fortran type promotion).
func (repl *REPL) promoteTypes(a, b *varinfo) *varinfo {
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
