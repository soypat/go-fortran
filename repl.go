package fortran

import (
	"errors"
	"fmt"
	"math"
	"slices"
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
	set   bool
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

// commonBlockInfo tracks variables in a COMMON block for transpilation.
type commonBlockInfo struct {
	Name   string // COMMON block name (empty string for blank COMMON)
	fields []Varinfo
}

func (cb *commonBlockInfo) getField(name string) *Varinfo {
	for i := range cb.fields {
		if strings.EqualFold(cb.fields[i]._varname, name) {
			return &cb.fields[i]
		}
	}
	return nil
}

func (cb *commonBlockInfo) addField(v *Varinfo) {
	if cb.getField(v._varname) != nil {
		panic("field already exists: " + v._varname)
	}
	cb.fields = append(cb.fields, *v)
}

type REPL struct {
	scope             ParserUnitData // currentScope variable data.
	_extern           []*ParserUnitData
	_contains         []*ParserUnitData
	commonblocks      []commonBlockInfo // COMMON block name -> info (file-level, not reset per procedure)
	noValueResolution bool              // When true, Eval skips value computation (type inference only)
	externUnits       []f90.ProgramUnit
}

func (repl *REPL) Reset() {
	*repl = REPL{
		_extern:      repl._extern[:0],
		_contains:    repl._contains[:0],
		scope:        repl.scope,
		commonblocks: repl.commonblocks[:0],
		externUnits:  repl.externUnits[:0],
	}
	repl.scope.reset()
}

// collectCommonBlocks scans program unit variables for COMMON block membership
// and builds the commonBlocks map with field types and array specs.
func (tg *REPL) collectCommonBlocks(vars []Varinfo) {
	for i := range vars {
		v := &vars[i]
		if !v.flags.HasAny(VFlagCommon) {
			continue // Not in a COMMON block
		}
		block := tg.getCommon(v.common)
		if block == nil {
			tg.commonblocks = append(tg.commonblocks, commonBlockInfo{Name: v.common})
			block = &tg.commonblocks[len(tg.commonblocks)-1]
			block.addField(v)
		} else {
			field := block.getField(v.Identifier())
			if field == nil {
				block.addField(v)
			}
		}
	}
	slices.SortStableFunc(tg.commonblocks, func(a, b commonBlockInfo) int {
		return strings.Compare(a.Name, b.Name)
	})
}

func (tg *REPL) getCommon(name string) *commonBlockInfo {
	for i := range tg.commonblocks {
		if strings.EqualFold(tg.commonblocks[i].Name, name) {
			return &tg.commonblocks[i]
		}
	}
	return nil
}

func (repl *REPL) Var(name string) *Varinfo {
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
		// Also register module-contained procedures so they can be found by ContainedOrExtern.
		if mod, ok := pu[i].(*f90.Module); ok {
			for _, contained := range mod.Contains {
				cdata, ok := contained.UnitData().(*ParserUnitData)
				if !ok {
					continue
				}
				if repl.Extern(cdata.name) == nil {
					repl._extern = append(repl._extern, cdata)
				}
			}
		}
	}
	repl.externUnits = append(repl.externUnits, pu...)
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

func (repl *REPL) ScopeParams() []Varinfo {
	for i, v := range repl.scope.vars {
		if !v.flags.HasAny(VFlagParameter) {
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

	repl.scope.copyFrom(data)
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
	repl.collectCommonBlocks(repl.scope.vars)

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
func (repl *REPL) Eval(dst *Varinfo, expr f90.Expression) (err error) {
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
	case *f90.ArrayConstructor:
		err = repl.evalArrayConstructor(dst, e)
	case *f90.ArrayRef:
		// Array element access: arr(i) - infer type from array variable
		vi := repl.Var(e.Name)
		if vi == nil {
			err = fmt.Errorf("var %s undefined", e.Name)
		} else {
			// Copy the type information but mark as element access (scalar)
			dst.decl = vi.decl
			dst.val.tok = vi.typeToken()
		}
	default:
		err = fmt.Errorf("unsupported expression: %T", expr)
	}
	return err
}

// InferType infers the type of expr without evaluating it.
// Sets dst.val.tok to the result type.
func (repl *REPL) InferType(dst *Varinfo, expr f90.Expression) error {
	*dst = Varinfo{}
	prev := repl.noValueResolution
	repl.noValueResolution = true
	err := repl.Eval(dst, expr)
	repl.noValueResolution = prev
	return err
}

// InferType infers the type of expr without evaluating it.
// Sets dst.val.tok to the result type.
func (repl *REPL) ensureEval(dst *Varinfo, expr f90.Expression) error {
	*dst = Varinfo{}
	prev := repl.noValueResolution
	repl.noValueResolution = false
	err := repl.Eval(dst, expr)
	repl.noValueResolution = prev
	return err
}

func (repl *REPL) evalUnary(dst *Varinfo, e *f90.UnaryExpr) error {
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

func (repl *REPL) evalBinary(dst *Varinfo, e *f90.BinaryExpr) error {
	var left, right Varinfo
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

func (repl *REPL) evalIntBinary(dst *Varinfo, l int64, op f90token.Token, ri int64) error {
	if repl.noValueResolution {
		return repl.assignInt(dst, 0)
	}
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

func (repl *REPL) evalFloatBinary(dst, typ *Varinfo, l float64, op f90token.Token, rf float64) error {
	if repl.noValueResolution {
		return repl.assignFloatLike(dst, typ, 0)
	}
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

func (repl *REPL) evalIntrinsic(dst *Varinfo, e *f90.FunctionCall) error {
	name := strings.ToUpper(e.Name)
	if len(e.Args) == 0 {
		return fmt.Errorf("%s intrinsic requires arguments", name)
	}
	var arg0 Varinfo
	err := repl.Eval(&arg0, e.Args[0])
	if err != nil {
		return err
	}

	f0 := arg0.val.Float()
	var fn0 func(float64) float64
	switch name {
	case "SQRT":
		fn0 = math.Sqrt
	case "SIN":
		fn0 = math.Sin
	case "COS":
		fn0 = math.Cos
	case "TAN":
		fn0 = math.Tan
	case "ASIN":
		fn0 = math.Asin
	case "ACOS":
		fn0 = math.Acos
	case "ATAN":
		fn0 = math.Atan
	case "EXP":
		fn0 = math.Exp
	case "LOG":
		fn0 = math.Log
	case "LOG10":
		fn0 = math.Log10
	case "ABS":
		fn0 = math.Abs
	}
	if fn0 != nil {
		return repl.assignFloatLike(dst, &arg0, repl.evalFloatFn0(fn0, f0))
	}
	switch name {
	case "REAL":
		err = repl.assignFloat32(dst, f0)
	case "DBLE":
		err = repl.assignFloat64(dst, f0)
	case "INT":
		err = repl.assignInt(dst, int64(f0))
	case "NINT":
		err = repl.assignInt(dst, int64(repl.evalFloatFn0(math.Round, f0)))
	case "FACTORIAL":
		err = repl.assignInt(dst, repl.evalFactorial(arg0.val.Int()))
	case "FLOOR":
		err = repl.assignFloatLike(dst, &arg0, repl.evalFloatFn0(math.Floor, f0))
	case "CEILING":
		err = repl.assignFloatLike(dst, &arg0, repl.evalFloatFn0(math.Ceil, f0))
	case "MAX":
		err = repl.evalMax(dst, e.Args)
	case "MIN":
		err = repl.evalMin(dst, e.Args)
	case "LEN", "LEN_TRIM", "INDEX", "ICHAR", "IACHAR", "SIZE", "LBOUND", "UBOUND", "MALLOC":
		err = repl.assignInt(dst, int64(len(arg0.val.StringValue())))
	case "TRIM", "ADJUSTL", "ADJUSTR", "CHAR", "ACHAR":
		err = repl.assignString(dst, arg0.val.StringValue())
	case "MOD":
		if len(e.Args) < 2 {
			return fmt.Errorf("MOD requires 2 arguments")
		}
		var arg1 Varinfo
		if err := repl.Eval(&arg1, e.Args[1]); err != nil {
			return err
		}
		err = repl.assignInt(dst, arg0.val.Int()%arg1.val.Int())
	default:
		// Check for user-defined functions
		if fn := repl.ContainedOrExtern(e.Name); fn != nil && fn.returnType != nil {
			dst.val.tok = fn.returnType.typeToken()
			return nil
		}
		err = fmt.Errorf("unknown intrinsic: %s", name)
	}
	return err
}

func (repl *REPL) evalFloatFn0(fn func(float64) float64, val float64) float64 {
	if repl.noValueResolution {
		return 0
	}
	return fn(val)
}

func (repl *REPL) evalMax(dst *Varinfo, args []f90.Expression) error {
	if len(args) == 0 {
		return errors.New("MAX requires at least one argument")
	}
	err := repl.Eval(dst, args[0])
	if err != nil {
		return err
	}
	if repl.noValueResolution {
		return nil // Type determined from first arg
	}
	var next Varinfo
	for _, arg := range args[1:] {
		err = repl.Eval(&next, arg)
		if err != nil {
			return err
		}
		if next.val.Float() > dst.val.Float() {
			*dst = next
		}
	}
	return nil
}

func (repl *REPL) evalMin(dst *Varinfo, args []f90.Expression) error {
	if len(args) == 0 {
		return errors.New("MIN requires at least one argument")
	}
	err := repl.Eval(dst, args[0])
	if err != nil {
		return err
	}
	if repl.noValueResolution {
		return nil // Type determined from first arg
	}
	var next Varinfo
	for _, arg := range args[1:] {
		err := repl.Eval(&next, arg)
		if err != nil {
			return err
		}
		if next.val.Float() < dst.val.Float() {
			*dst = next
		}
	}
	return nil
}

func (repl *REPL) evalArrayConstructor(dst *Varinfo, e *f90.ArrayConstructor) error {
	if len(e.Values) == 0 {
		dst.val.tok = f90token.INTEGER // Default to integer for empty array
		return nil
	}
	// Infer element type from first element
	return repl.Eval(dst, e.Values[0])
}

// Helper methods for creating varinfo with values
func (repl *REPL) prepAssignment(dst *Varinfo, src *Varinfo) error {
	if dst.decl == nil {
		// Declaration of dst should remain unset. Conceptually it's declaration is ephemeral, part of a REPL evaluation.
	} else if dst.decl.Type.Token != src.decl.Type.Token {
		return fmt.Errorf("destination variable %q of type %s not assignable with type %s", dst.Identifier(), dst.decl.Type.Token.String(), src.decl.Type.Token.String())
	}
	dst.val.tok = src.decl.Type.Token
	dst.val.set = true
	return nil
}

func (repl *REPL) getOrResolveKind(v *Varinfo) (int, error) {
	if v.kindFlag != 0 {
		if v.kindFlag == -1 {
			return 0, nil
		}
		return v.kindFlag, nil
	}
	kind := v.Kind()
	if kind != nil {
		var evaled Varinfo
		err := repl.ensureEval(&evaled, kind)
		if err != nil {
			return -2, err
		}
		if evaled.val.tok != f90token.INTEGER {
			return -3, fmt.Errorf("expected integer kind for %s", v._varname)
		} else if evaled.val.i64 == 0 {
			return -4, fmt.Errorf("got zero kind for %s", v._varname)
		}
		v.kindFlag = int(evaled.val.i64)
		return v.kindFlag, nil
	}
	v.kindFlag = -1 // No
	return 0, nil
}

// promote returns the resulting promoted type of a binary operation between two types.
func (repl *REPL) promote(dst, src *Varinfo) (promotion f90token.Token, kind int) {
	dtok := dst.typeToken()
	stok := src.typeToken()
	switch dtok {
	case 0:
		promotion = stok
	case f90token.DOUBLEPRECISION:
		switch stok {
		case f90token.INTEGER, f90token.REAL:
			promotion = f90token.DOUBLEPRECISION
		}
	case f90token.REAL:
		switch stok {
		case f90token.INTEGER:
			promotion = f90token.REAL
		case f90token.DOUBLEPRECISION:
			promotion = f90token.DOUBLEPRECISION
		}
	case f90token.INTEGER:
		switch stok {
		case f90token.INTEGER, f90token.REAL, f90token.DOUBLEPRECISION:
			promotion = stok
		}
	default:
		if stok == dtok {
			promotion = stok
		}
	}
	return promotion, 0
}

// typeToken returns the effective type token (prefers val.tok, falls back to decl).
func (v *Varinfo) typeToken() f90token.Token {
	if v.val.tok != 0 {
		return v.val.tok
	}
	if v.decl != nil {
		return v.decl.Type.Token
	}
	return 0
}

func (repl *REPL) assignInt(dst *Varinfo, v int64) error {
	err := repl.prepAssignment(dst, _tgtInt32)
	if err != nil {
		return err
	}
	dst.val.i64 = v
	return nil
}

func (repl *REPL) assignFloat32(dst *Varinfo, v float64) error {
	err := repl.prepAssignment(dst, _tgtFloat32)
	if err != nil {
		return err
	}
	dst.val.f64 = v
	return nil
}

func (repl *REPL) assignFloat64(dst *Varinfo, v float64) error {
	err := repl.prepAssignment(dst, _tgtFloat64)
	if err != nil {
		return err
	}
	dst.val.f64 = v
	return nil
}

func (repl *REPL) assignBool(dst *Varinfo, v bool) error {
	err := repl.prepAssignment(dst, _tgtBool)
	if err != nil {
		return err
	}
	dst.val.b = v
	return nil
}

func (repl *REPL) assignString(dst *Varinfo, v string) error {
	err := repl.prepAssignment(dst, _tgtChar)
	if err != nil {
		return err
	}
	dst.val.s = v
	return nil
}

func (repl *REPL) assignFloatLike(dst, template *Varinfo, v float64) error {
	dst.val.tok, _ = repl.promote(dst, template)
	dst.val.f64 = v
	return nil
}

// Type checking helpers

// promoteTypes returns the wider of two numeric types (Fortran type promotion).
func (repl *REPL) promoteTypes(a, b *Varinfo) *Varinfo {
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

func (repl *REPL) evalFactorial(n int64) int64 {
	if repl.noValueResolution || n <= 1 {
		return 1
	}
	result := int64(1)
	for i := int64(2); i <= n; i++ {
		result *= i
	}
	return result
}
