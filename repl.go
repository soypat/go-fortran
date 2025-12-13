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
	scope ParserUnitData // currentScope variable data.
	// registered stores all modules that have been registered
	// using RegisterModule since Reset call. These modules can then be
	// loaded to the USE scope via "Use".
	registered []f90.ProgramUnit

	// use stores modules/subroutines/functions/data blocks that have been loaded via Use.
	// The used modules are flattened here, so _use contains functions/subroutines contained within modules as well.
	_use              []*ParserUnitData
	_contains         []*ParserUnitData
	commonblocks      []commonBlockInfo // COMMON block name -> info (file-level, not reset per procedure)
	noValueResolution bool              // When true, Eval skips value computation (type inference only)
}

func (repl *REPL) Reset() {
	*repl = REPL{
		_use:         repl._use[:0],
		_contains:    repl._contains[:0],
		scope:        repl.scope,
		commonblocks: repl.commonblocks[:0],
		registered:   repl.registered[:0],
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
	vi := repl.scope.Var(name)
	if vi != nil {
		return vi
	}
	for _, mod := range repl._use {
		vi = mod.Var(name)
		if vi != nil {
			return vi
		}
	}
	return nil
}

// DefineStmtFunc registers a statement function in the current scope.
// Statement functions are one-line inline functions like: FUNCNAME(X) = expr
func (repl *REPL) DefineStmtFunc(name string, params []string, expr f90.Expression, decl *f90.DeclEntity) {
	vi := repl.scope.Var(name)
	if vi == nil {
		// Add new varinfo for the statement function
		repl.scope.vars = append(repl.scope.vars, Varinfo{
			_varname: name,
			decl:     decl,
		})
		vi = &repl.scope.vars[len(repl.scope.vars)-1]
	}
	vi.flags |= VFlagStmtFunc
	vi.stmtFuncExpr = expr
	vi.stmtFuncParams = params
}

// RegisteredUnit returns a program unit that was previously registered with RegisterUnit.
func (repl *REPL) RegisteredUnit(name string) f90.ProgramUnit {
	for i := range repl.registered {
		if strings.EqualFold(repl.registered[i].UnitName(), name) {
			return repl.registered[i]
		}
	}
	return nil
}

// RegisterUnits registers a set of program units to the REPL. These units
// are registered at a top level and are not accessed except in the case of function/subroutine lookup.
func (repl *REPL) RegisterUnits(pu ...f90.ProgramUnit) error {
	for i := range pu {
		name := pu[i].UnitName()
		exists := repl.RegisteredUnit(name)
		if exists != nil {
			return fmt.Errorf("%s(%T) already added as %s(%s)", pu[i].UnitName(), pu[i], name, exists.AppendTokenLiteral(nil))
		}
	}
	repl.registered = append(repl.registered, pu...)
	return nil
}

// Use loads a registered unit to the REPL scope until scope is reset.
func (repl *REPL) Use(name string, only ...string) (err error) {
	unit := repl.RegisteredUnit(name)
	if unit == nil {
		return errors.New("unit " + name + " not registered")
	}
	repl._use, err = repl.appendUnitData(repl._use, only, unit)
	if err != nil {
		return fmt.Errorf("using unit %s: %w", name, err)
	}
	return nil
}

func (repl *REPL) appendUnitData(dst []*ParserUnitData, only []string, pu f90.ProgramUnit) (_ []*ParserUnitData, err error) {
	data, ok := pu.UnitData().(*ParserUnitData)
	if !ok {
		return dst, fmt.Errorf("program unit %s has incompatible UnitData %T", pu.UnitName(), pu.UnitData())
	}
	for i := range dst {
		if strings.EqualFold(dst[i].name, data.name) {
			return dst, fmt.Errorf("%s(%T) already added as %s(%s)", pu.UnitName(), pu, dst[i].name, dst[i].tok.String())
		}
	}
	if only != nil {
		var filtered ParserUnitData
		filtered.copyFrom(data)
		filtered.vars = filtered.vars[:0]
		for i := range data.vars {
			if identifierIn(only, data.vars[i].Identifier()) {
				filtered.vars = append(filtered.vars, data.vars[i])
			}
		}
		data = &filtered
	}
	dst = append(dst, data)
	// Also register module-contained procedures so they can be found by ContainedOrExtern.
	if mod, ok := pu.(*f90.Module); ok {
		for _, contained := range mod.Contains {
			if only != nil && !identifierIn(only, contained.UnitName()) {
				continue
			}
			dst, err = repl.appendUnitData(dst, only, contained)
			if err != nil {
				return dst, fmt.Errorf("%s contained within %s: %w", contained.UnitName(), mod.Name, err)
			}
		}
	}
	return dst, nil
}

func (repl *REPL) GetUsed(name string) *ParserUnitData {
	for i := range repl._use {
		if strings.EqualFold(repl._use[i].name, name) {
			return repl._use[i]
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

func (repl *REPL) ContainedOrUsed(name string) *ParserUnitData {
	data := repl.Contained(name)
	if data == nil {
		data = repl.GetUsed(name)
	}
	if data == nil {
		// Search registered external procedures (globally visible in Fortran)
		unit := repl.RegisteredUnit(name)
		if unit != nil {
			data, _ = unit.UnitData().(*ParserUnitData)
		}
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

func (repl *REPL) SetScope(pu f90.ProgramUnit) (err error) {
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
	repl._use = repl._use[:0]
	var toAdd []f90.ProgramUnit
	switch unit := pu.(type) {
	case *f90.ProgramBlock:
		repl._contains = repl._contains[:0]
		toAdd = unit.Contains
	case *f90.Module:
		repl._contains = repl._contains[:0]
		toAdd = unit.Contains
	case *f90.Function:
		if slices.Contains(unit.Attributes, f90token.RECURSIVE) {
			toAdd = []f90.ProgramUnit{unit}
		}
	case *f90.Subroutine:
		if slices.Contains(unit.Attributes, f90token.RECURSIVE) {
			toAdd = []f90.ProgramUnit{unit}
		}
	default:
		return nil
	}
	// reset contains on Module or Program block.
	for _, pu := range toAdd {
		repl._contains, err = repl.appendUnitData(repl._contains, nil, pu)
		if err != nil {
			return fmt.Errorf("adding contains %s: %w", pu.UnitName(), err)
		}
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
	case *f90.CallExpr:
		// CallExpr can be array access or function call - disambiguate
		if vi := repl.Var(e.Name); vi != nil {
			// It's a variable (array element access) - infer element type
			dst.decl = vi.decl
			dst.val.tok = vi.typeToken()
		} else {
			// It's a function call (intrinsic or external)
			err = repl.evalIntrinsic(dst, e)
		}
	case *f90.ArrayConstructor:
		err = repl.evalArrayConstructor(dst, e)
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

func (repl *REPL) evalIntrinsic(dst *Varinfo, e *f90.CallExpr) error {
	intr := f90token.LookupIntrinsic(e.Name)
	if repl.noValueResolution {
		// No value resolution short circuit.
		intr := getIntrinsic(intr, len(e.Args))
		if intr != nil {
			if intr.returnType != nil {
				*dst = *intr.returnType
				return nil
			} else if len(e.Args) > 0 {
				return repl.Eval(dst, e.Args[0])
			}
		}
	}
	name := strings.ToUpper(e.Name)
	if len(e.Args) == 0 {
		return fmt.Errorf("%s intrinsic requires arguments", name)
	}
	var arg0 Varinfo
	err := repl.Eval(&arg0, e.Args[0])
	if err != nil {
		return err
	}

	// Check single-argument float function table.
	if int(intr) < len(_intrinsicEvalf1) {
		if fn1 := _intrinsicEvalf1[intr]; fn1 != nil {
			return repl.assignFloatLike(dst, &arg0, repl.evalFloatFn0(fn1, arg0.val.Float()))
		}
	}

	// Check two-argument float function table.
	if int(intr) < len(_intrinsicEvalf2) {
		if fn2 := _intrinsicEvalf2[intr]; fn2 != nil {
			if len(e.Args) < 2 {
				return fmt.Errorf("%s requires 2 arguments", name)
			}
			var arg1 Varinfo
			if err := repl.Eval(&arg1, e.Args[1]); err != nil {
				return err
			}
			return repl.assignFloatLike(dst, &arg0, repl.evalFloatFn2(fn2, arg0.val.Float(), arg1.val.Float()))
		}
	}

	// Handle special cases not covered by lookup tables.
	f0 := arg0.val.Float()
	switch intr {
	case f90token.IntrinsicREAL, f90token.IntrinsicFLOAT, f90token.IntrinsicSNGL:
		err = repl.assignFloat32(dst, f0)
	case f90token.IntrinsicDBLE:
		err = repl.assignFloat64(dst, f0)
	case f90token.IntrinsicINT, f90token.IntrinsicIFIX, f90token.IntrinsicIDINT:
		err = repl.assignInt(dst, int64(f0))
	case f90token.IntrinsicNINT, f90token.IntrinsicIDNINT:
		err = repl.assignInt(dst, int64(repl.evalFloatFn0(math.Round, f0)))
	case f90token.IntrinsicMAX, f90token.IntrinsicMAX0, f90token.IntrinsicMAX1, f90token.IntrinsicAMAX0, f90token.IntrinsicAMAX1, f90token.IntrinsicDMAX1:
		err = repl.evalMax(dst, e.Args)
	case f90token.IntrinsicMIN, f90token.IntrinsicMIN0, f90token.IntrinsicMIN1, f90token.IntrinsicAMIN0, f90token.IntrinsicAMIN1, f90token.IntrinsicDMIN1:
		err = repl.evalMin(dst, e.Args)
	case f90token.IntrinsicLEN, f90token.IntrinsicLEN_TRIM, f90token.IntrinsicINDEX, f90token.IntrinsicICHAR, f90token.IntrinsicSIZE, f90token.IntrinsicLBOUND, f90token.IntrinsicUBOUND:
		err = repl.assignInt(dst, int64(len(arg0.val.StringValue())))
	case f90token.IntrinsicTRIM, f90token.IntrinsicADJUSTL, f90token.IntrinsicADJUSTR, f90token.IntrinsicCHAR:
		err = repl.assignString(dst, arg0.val.StringValue())
	case f90token.IntrinsicIABS:
		err = repl.assignInt(dst, int64(math.Abs(float64(arg0.val.Int()))))
	case f90token.IntrinsicISIGN, f90token.IntrinsicDSIGN, f90token.IntrinsicSIGN:
		if len(e.Args) < 2 {
			return fmt.Errorf("%s requires 2 arguments", name)
		}
		var arg1 Varinfo
		if err := repl.Eval(&arg1, e.Args[1]); err != nil {
			return err
		}
		err = repl.assignFloatLike(dst, &arg0, math.Copysign(f0, arg1.val.Float()))
	case f90token.IntrinsicDPROD:
		if len(e.Args) < 2 {
			return fmt.Errorf("%s requires 2 arguments", name)
		}
		var arg1 Varinfo
		if err := repl.Eval(&arg1, e.Args[1]); err != nil {
			return err
		}
		err = repl.assignFloat64(dst, f0*arg1.val.Float())
	default:
		// Check for user-defined functions.
		if fn := repl.ContainedOrUsed(e.Name); fn != nil && fn.returnType != nil {
			dst.val.tok = fn.returnType.typeToken()
			return nil
		}
		// Handle non-standard extensions by string matching.
		switch name {
		case "MALLOC", "IACHAR":
			err = repl.assignInt(dst, int64(len(arg0.val.StringValue())))
		case "ACHAR":
			err = repl.assignString(dst, arg0.val.StringValue())
		default:
			if f90token.IsIntrinsic(name) {
				err = fmt.Errorf("intrinsic not yet implemented: %s", name)
			} else {
				err = fmt.Errorf("unknown intrinsic: %s", name)
			}
		}
	}
	return err
}

func (repl *REPL) evalFloatFn0(fn func(float64) float64, val float64) float64 {
	if repl.noValueResolution {
		return 0
	}
	return fn(val)
}

func (repl *REPL) evalFloatFn2(fn func(float64, float64) float64, a, b float64) float64 {
	if repl.noValueResolution {
		return 0
	}
	return fn(a, b)
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

// Single-argument float intrinsics: f(float64) float64
var _intrinsicEvalf1 = [...]func(float64) float64{
	// Fortran 66 - Mathematical
	f90token.IntrinsicABS:  math.Abs,
	f90token.IntrinsicSQRT: math.Sqrt,
	// Fortran 66 - Trigonometric
	f90token.IntrinsicSIN:  math.Sin,
	f90token.IntrinsicCOS:  math.Cos,
	f90token.IntrinsicTAN:  math.Tan,
	f90token.IntrinsicASIN: math.Asin,
	f90token.IntrinsicACOS: math.Acos,
	f90token.IntrinsicATAN: math.Atan,
	// Fortran 66 - Hyperbolic
	f90token.IntrinsicSINH: math.Sinh,
	f90token.IntrinsicCOSH: math.Cosh,
	f90token.IntrinsicTANH: math.Tanh,
	// Fortran 66 - Exponential/Logarithmic
	f90token.IntrinsicEXP:   math.Exp,
	f90token.IntrinsicLOG:   math.Log,
	f90token.IntrinsicLOG10: math.Log10,
	// Fortran 66 - Truncation/Rounding
	f90token.IntrinsicAINT:  math.Trunc,
	f90token.IntrinsicANINT: math.Round,
	// Fortran 66 - Double precision specific names
	f90token.IntrinsicDABS:   math.Abs,
	f90token.IntrinsicDSQRT:  math.Sqrt,
	f90token.IntrinsicDSIN:   math.Sin,
	f90token.IntrinsicDCOS:   math.Cos,
	f90token.IntrinsicDTAN:   math.Tan,
	f90token.IntrinsicDASIN:  math.Asin,
	f90token.IntrinsicDACOS:  math.Acos,
	f90token.IntrinsicDATAN:  math.Atan,
	f90token.IntrinsicDSINH:  math.Sinh,
	f90token.IntrinsicDCOSH:  math.Cosh,
	f90token.IntrinsicDTANH:  math.Tanh,
	f90token.IntrinsicDEXP:   math.Exp,
	f90token.IntrinsicDLOG:   math.Log,
	f90token.IntrinsicDLOG10: math.Log10,
	f90token.IntrinsicDNINT:  math.Round,
	// Fortran 90
	f90token.IntrinsicCEILING: math.Ceil,
	f90token.IntrinsicFLOOR:   math.Floor,
	// Fortran 2008
	f90token.IntrinsicACOSH:     math.Acosh,
	f90token.IntrinsicASINH:     math.Asinh,
	f90token.IntrinsicATANH:     math.Atanh,
	f90token.IntrinsicBESSEL_J0: math.J0,
	f90token.IntrinsicBESSEL_J1: math.J1,
	f90token.IntrinsicBESSEL_Y0: math.Y0,
	f90token.IntrinsicBESSEL_Y1: math.Y1,
	f90token.IntrinsicERF:       math.Erf,
	f90token.IntrinsicERFC:      math.Erfc,
	f90token.IntrinsicGAMMA:     math.Gamma,
}

// Two-argument float intrinsics: f(float64, float64) float64
var _intrinsicEvalf2 = [...]func(float64, float64) float64{
	// Fortran 66
	f90token.IntrinsicATAN2:  math.Atan2,
	f90token.IntrinsicDATAN2: math.Atan2,
	f90token.IntrinsicDIM:    math.Dim,
	f90token.IntrinsicDDIM:   math.Dim,
	f90token.IntrinsicMOD:    math.Mod,
	// Fortran 90
	f90token.IntrinsicMODULO: math.Mod,
	// Fortran 2008
	f90token.IntrinsicHYPOT: math.Hypot,
}

func identifierIn(s []string, id string) bool {
	for i := range s {
		if strings.EqualFold(s[i], id) {
			return true
		}
	}
	return false
}
