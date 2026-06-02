package fortran

import (
	"errors"
	"fmt"
	"go/ast"
	"go/token"
	"strconv"
	"strings"

	f90 "github.com/soypat/go-fortran/ast"
	f90token "github.com/soypat/go-fortran/token"
)

// transformExpression transforms a single Fortran expression to a Go expression
func (tg *ToGo) transformExpression(vitgt *Varinfo, expr f90.Expression) (result ast.Expr, resultType *Varinfo, err error) {
	if expr == nil {
		return nil, nil, tg.makeErrAtStmt("nil expression")
	} else if vitgt == nil || vitgt.TypeToken() == 0 {
		panic("vitgt cannot be nil")
	}
	switch e := expr.(type) {
	case *f90.StringLiteral:
		resultType = _tgtStringLit
		result = &ast.BasicLit{
			Kind:  token.STRING,
			Value: fmt.Sprintf("%q", e.Value),
		}
	case *f90.IntegerLiteral:
		resultType = _tgtGenericInt
		result = &ast.BasicLit{
			Kind:  token.INT,
			Value: strconv.FormatInt(e.Value, 10),
		}
	case *f90.RealLiteral:
		// Check if this is a double precision literal (D exponent like 3.0D0)
		if strings.ContainsAny(e.Raw, "Dd") {
			resultType = _tgtFloat64
		} else {
			resultType = _tgtGenericFloat
		}
		// Format float - ensure it has a decimal point so Go recognizes it as float
		floatStr := strconv.FormatFloat(e.Value, 'g', 16, 64)
		if !strings.ContainsAny(floatStr, ".eE") {
			floatStr += ".0"
		}
		result = &ast.BasicLit{
			Kind:  token.FLOAT,
			Value: floatStr,
		}
	case *f90.LogicalLiteral:
		// .TRUE. → true, .FALSE. → false
		resultType = _tgtBool
		if e.Value {
			result = _astTrue
		} else {
			result = _astFalse
		}

	case *f90.Identifier:
		result, resultType, err = tg.transformExprIdentifer(vitgt, e)
	case *f90.CallExpr:
		// CallExpr: disambiguate between array access and function call
		result, resultType, err = tg.transformFunctionCall(vitgt, e)
	case *f90.BinaryExpr:
		if e.Op == f90token.StringConcat {
			result, err = tg.transformStringConcatExpr(e)
			resultType = _tgtStringLit
		} else {
			result, resultType, err = tg.transformBinaryExpr(vitgt, e)
		}

	case *f90.UnaryExpr:
		result, resultType, err = tg.transformUnaryExpr(vitgt, e)
	case *f90.ParenExpr:
		if e.Imag != nil {
			// Complex literal: (real, imag) -> complex(real, imag)
			realExpr, _, err := tg.transformExpression(_tgtGenericFloat, e.Expr)
			if err != nil {
				return nil, nil, err
			}
			imagExpr, _, err := tg.transformExpression(_tgtGenericFloat, e.Imag)
			if err != nil {
				return nil, nil, err
			}
			resultType = _tgtComplex64
			result = &ast.CallExpr{
				Fun:  ast.NewIdent("complex"),
				Args: []ast.Expr{realExpr, imagExpr},
			}
		} else {
			var inner ast.Expr
			// Parentheses for grouping - transform the inner expression and wrap in parens
			inner, resultType, err = tg.transformExpression(vitgt, e.Expr)
			if err != nil {
				return nil, nil, err
			}
			result = &ast.ParenExpr{X: inner}
		}

	case *f90.ArrayConstructor:
		result, resultType, err = tg.transformArrayConstructor(vitgt, e)
	case *f90.RangeExpr:
		// Range expressions in subscripts (e.g., arr(1:5), str(2:3))
		// TODO: implement proper range transformation
		err = tg.makeErr(expr, "RangeExpr not yet implemented in transpiler")
	case *f90.ComponentAccess:
		// Component access: p%age → p.age
		result, resultType, err = tg.transformComponentAccess(vitgt, e)
	default:
		err = tg.makeErr(expr, "unsupported expression")
	}
	if (result == nil || resultType == nil) && err == nil {
		err = tg.makeErr(expr, "unhandled expression type, result or result type is nil: "+string(expr.AppendString(nil)))
	}
	if err != nil {
		return nil, nil, err
	}
	doWrap := vitgt == _tgtInt || resultType.IsPointer() != vitgt.IsPointer() ||
		vitgt == _tgtStringLit && resultType.IsChar()
	if doWrap {
		result = tg.wrapConversion(vitgt, resultType, result)
	}
	return result, resultType, err
}

// wrapConversion wraps expr with a type conversion if target type differs from sourceType.
func (tg *ToGo) wrapConversion(target *Varinfo, sourceType *Varinfo, expr ast.Expr) ast.Expr {
	ptrDerefFirst := sourceType.IsPointer()
	// isArray := tg.varIsArray(sourceType)
	switch {
	case ptrDerefFirst:
		expr = &ast.CallExpr{
			Fun: &ast.SelectorExpr{
				X:   expr,
				Sel: ast.NewIdent("At"),
			},
			Args: []ast.Expr{_astOne},
		}
	case target == nil:
		panic(tg.makeErrAtStmt("nil target variable: " + sourceType._varname))
	case target == _tgtInt:
		if sourceType != _tgtInt && sourceType != _tgtGenericInt {
			expr = &ast.CallExpr{Fun: ast.NewIdent("int"), Args: []ast.Expr{expr}}
		}
		return expr
	case target == _tgtStringLit:
		if sourceType != _tgtStringLit && sourceType.IsChar() {
			// Something that receives a string is receiving a character array,
			// so call String method on it.
			expr = &ast.CallExpr{
				Fun: &ast.SelectorExpr{
					X:   expr,
					Sel: ast.NewIdent("String"),
				},
			}
			return expr
		}
	}
	srcType := sourceType.TypeToken()
	targetType := target.TypeToken()
	// FloatLit/IntLit are untyped constants compatible with any matching numeric type.
	compatibleLiteral := (srcType == f90token.FloatLit && (targetType == f90token.REAL || targetType == f90token.DOUBLEPRECISION)) ||
		(srcType == f90token.IntLit && targetType == f90token.INTEGER)
	if srcType == targetType || targetType == f90token.FloatLit ||
		compatibleLiteral ||
		// Derived types or arrays: type unknown or non-scalar, pass through without conversion
		srcType == f90token.TYPE || targetType == f90token.TYPE ||
		targetType == f90token.DIMENSION {
		return expr
	}
	// Special case: converting real to complex requires complex(real, 0)
	if (targetType == f90token.COMPLEX || targetType == f90token.DOUBLECOMPLEX) &&
		(srcType == f90token.REAL || srcType == f90token.DOUBLEPRECISION ||
			srcType == f90token.INTEGER || srcType == f90token.FloatLit || srcType == f90token.IntLit) {
		return &ast.CallExpr{
			Fun:  ast.NewIdent("complex"),
			Args: []ast.Expr{expr, _astZero},
		}
	}
	conv := tg.baseGotype(targetType, tg.resolveKind(target))
	return &ast.CallExpr{
		Fun:  conv,
		Args: []ast.Expr{expr},
	}
}

func (tg *ToGo) transformExprIdentifer(vitgt *Varinfo, e *f90.Identifier) (result ast.Expr, resultType *Varinfo, err error) {
	resultType = tg.repl.Var(e.Value)
	if resultType == nil {
		err = tg.makeErr(e, "identifier not found")
		return nil, nil, err
	}
	// PARAMETER constant from host/use scope (not declared locally): inline its value.
	if resultType.flags.HasAny(VFlagConstantParameter) &&
		resultType.decl != nil && resultType.decl.Init != nil &&
		tg.repl.scope.Var(e.Value) == nil {
		result, resultType, err = tg.transformExpression(vitgt, resultType.decl.Init)
		return result, resultType, err
	}
	result = tg.astVarExpr(resultType)
	// Dereference INTENT(OUT/INOUT) scalar parameters when used as values
	if tg.isGoPointer(resultType) {
		result = &ast.StarExpr{X: result}
	}
	// COMMON scalars (not EQUIVALENCED) are PointerTo[T], need .At(1) to read value.
	// EQUIVALENCED scalars are handled by wrapConversion via IsPointer().
	if !resultType.IsArray() && resultType.TypeToken() != f90token.CHARACTER &&
		resultType.flags.HasAny(VFlagCommon) && !resultType.flags.HasAny(VFlagEquivalenced) {
		result = &ast.CallExpr{
			Fun:  &ast.SelectorExpr{X: result, Sel: ast.NewIdent("At")},
			Args: []ast.Expr{&ast.BasicLit{Kind: token.INT, Value: "1"}},
		}
	}
	return result, resultType, nil
}

func (tg *ToGo) transformArrayConstructor(vitgt *Varinfo, e *f90.ArrayConstructor) (result ast.Expr, resultType *Varinfo, err error) {
	var elemType ast.Expr
	var elemVinfo *Varinfo

	// Always infer element type from values when present (priority over target type)
	if len(e.Values) > 0 {
		// Infer element type from first value. Use _tgtGenericInt as placeholder vitgt
		// since literals always return their own type regardless of target.
		_, elemVinfo, err = tg.transformExpression(_tgtGenericInt, e.Values[0])
		if err != nil {
			return nil, nil, err
		}
		elemType = tg.baseGotype(elemVinfo.TypeToken(), tg.resolveKind(elemVinfo))
	} else {
		// No values - try to use target type (but not DIMENSION which means "array of unknown type")
		targetTok := vitgt.TypeToken()
		if targetTok != f90token.Undefined && targetTok != f90token.DIMENSION {
			elemVinfo = vitgt
			elemType = tg.baseGotype(targetTok, tg.resolveKind(vitgt))
		} else {
			return nil, nil, tg.makeErr(e, "cannot infer array constructor element type")
		}
	}

	// Transform values
	var elts []ast.Expr
	for _, val := range e.Values {
		elt, _, err := tg.transformExpression(elemVinfo, val)
		if err != nil {
			return nil, nil, err
		}
		elts = append(elts, elt)
	}

	// Create result type: array of the inferred element type
	// Normalize literal tokens to concrete types (IntLit → INTEGER, FloatLit → REAL, StringLit → CHARACTER)
	elemTok := elemVinfo.TypeToken()
	switch elemTok {
	case f90token.IntLit:
		elemTok = f90token.INTEGER
	case f90token.FloatLit:
		elemTok = f90token.REAL
	case f90token.StringLit:
		// CHARACTER array constructor - use NewCharacterArrayFromStrings
		// Get charlen from target type (vitgt has the declaration info)
		var charlenExpr ast.Expr = _astOne
		if charLen := vitgt.Charlen(); charLen != nil {
			charlenExpr, _, err = tg.transformExpression(_tgtInt, charLen)
			if err != nil {
				return nil, nil, err
			}
		}
		// Generate: intrinsic.NewCharacterArrayFromStrings(charlen, []string{elts...}, len)
		return &ast.CallExpr{
			Fun: _astFnNewCharacterArrayFromStrings,
			Args: []ast.Expr{
				charlenExpr,
				&ast.CompositeLit{Type: &ast.ArrayType{Elt: ast.NewIdent("string")}, Elts: elts},
				&ast.BasicLit{Kind: token.INT, Value: strconv.Itoa(len(e.Values))},
			},
		}, _tgtArray(f90token.CHARACTER), nil
	}
	resultType = _tgtArray(elemTok)

	// Generate: intrinsic.NewArray[T]([]T{elts...}, len)
	// Returns pointer which matches array pointer types
	return &ast.CallExpr{
		Fun: &ast.IndexExpr{X: _astFnNewArray, Index: elemType},
		Args: []ast.Expr{
			&ast.CompositeLit{
				Type: &ast.ArrayType{Elt: elemType},
				Elts: elts,
			},
			&ast.BasicLit{Kind: token.INT, Value: strconv.Itoa(len(e.Values))},
		},
	}, resultType, nil
}

// transformComponentAccess transforms Fortran component access (p%age) to Go field access (p.age).
func (tg *ToGo) transformComponentAccess(vitgt *Varinfo, e *f90.ComponentAccess) (result ast.Expr, resultType *Varinfo, err error) {
	// Get the base variable info for transformation
	var baseVinfo *Varinfo
	if ident, ok := e.Base.(*f90.Identifier); ok {
		baseVinfo = tg.repl.Var(ident.Value)
	} else if call, ok := e.Base.(*f90.CallExpr); ok {
		// Array element access like vmf_array(idx)%field — look up the array variable
		baseVinfo = tg.repl.Var(call.Name)
	}

	// Transform the base expression using the base variable info
	base, _, err := tg.transformExpression(baseVinfo, e.Base)
	if err != nil {
		return nil, nil, err
	}
	// TODO: add field ot varinfo: fields []struct{name string; type *TypeDeclaration} and search for field matches in case insensitive fashion.
	// Create Go selector expression: base.Component
	result = &ast.SelectorExpr{
		X:   base,
		Sel: ast.NewIdent(e.Component), // This can fail on a field case mismatch. Varinfo should have a fields slice and then component be searched in there.
	}

	// Apply subscripts for array component access: obj%v(i) → obj.v.At(i)
	// If all args are full-range (:), the whole array is referenced — no subscript needed.
	if len(e.Args) > 0 {
		allFullRange := true
		for _, arg := range e.Args {
			if r, ok := arg.(*f90.RangeExpr); !ok || r.Start != nil || r.End != nil {
				allFullRange = false
				break
			}
		}
		if !allFullRange {
			var argExprs []ast.Expr
			for _, arg := range e.Args {
				argExpr, _, err := tg.transformExpression(_tgtInt, arg)
				if err != nil {
					return nil, nil, err
				}
				argExprs = append(argExprs, argExpr)
			}
			result = &ast.CallExpr{
				Fun:  &ast.SelectorExpr{X: result, Sel: ast.NewIdent("At")},
				Args: argExprs,
			}
		}
	}

	// For now, return vitgt as resultType since we don't track derived type field types
	// This works for simple cases where the target type is known
	return result, vitgt, nil
}

func (tg *ToGo) transformUnaryExpr(vitgt *Varinfo, e *f90.UnaryExpr) (result ast.Expr, resultType *Varinfo, err error) {
	operand, resultType, err := tg.transformExpression(vitgt, e.Operand)
	if err != nil {
		return nil, nil, err
	}

	var op token.Token
	switch e.Op {
	case f90token.Plus:
		// Unary plus: just return the operand
		return operand, resultType, nil
	case f90token.Minus:
		// Unary negation of array → intrinsic.ArrayNeg[T](a)
		if resultType.IsArray() && resultType.TypeToken() != f90token.TYPE {
			elemTok := resultType.TypeToken()
			goType := goTypeBasic(elemTok, 0)
			sel := &ast.SelectorExpr{X: _astIntrinsic, Sel: ast.NewIdent("ArrayNeg")}
			return &ast.CallExpr{
				Fun:  &ast.IndexExpr{X: sel, Index: goType},
				Args: []ast.Expr{operand},
			}, resultType, nil
		}
		op = token.SUB
	case f90token.NOT:
		op = token.NOT
	default:
		return nil, nil, tg.makeErr(e, fmt.Sprintf("unsupported unary operator %v", e.Op))
	}

	return &ast.UnaryExpr{
		Op: op,
		X:  operand,
	}, resultType, nil
}

func (tg *ToGo) checkPromotion(left, right *Varinfo) (lPromote, rPromote *Varinfo, err error) {
	if right == _tgtGenericFloat || right == _tgtGenericInt ||
		left == _tgtGenericFloat || left == _tgtGenericInt {
		return nil, nil, nil // Do not promote literals, go auto promotes them.
	}
	if left.TypeToken() == f90token.TYPE || right.TypeToken() == f90token.TYPE {
		return nil, nil, nil // Derived type fields: Go resolves actual types, no promotion needed.
	}
	ltok := left.TypeToken()
	rtok := right.TypeToken()
	lKind, err := tg.repl.getOrResolveKind(left)
	if err != nil {
		return nil, nil, err
	}
	rKind, err := tg.repl.getOrResolveKind(right)
	if err != nil {
		return nil, nil, err
	}
	ltok, lKind = normalizeTokenKind(ltok, lKind)
	rtok, rKind = normalizeTokenKind(rtok, rKind)
	if rtok == ltok {
		if lKind > rKind {
			return nil, left, nil
		} else if lKind < rKind {
			return right, nil, nil
		}
		return nil, nil, nil // Identical types.
	}
	if rtok == f90token.INTEGER && ltok == f90token.REAL {
		return nil, left, nil
	} else if ltok == f90token.INTEGER && rtok == f90token.REAL {
		return right, nil, nil
	}
	return nil, nil, errors.New("unpromotable combo " + ltok.String() + " " + rtok.String())
}

// normalizeTokenKind sets kind to non-zero size and DOUBLEPRECISION becomes REAL.
func normalizeTokenKind(tok f90token.Token, kind int) (f90token.Token, int) {
	var defaultKind int
	switch tok {
	case f90token.DOUBLEPRECISION:
		tok = f90token.REAL
		defaultKind = 8
	case f90token.REAL:
		defaultKind = 4
	case f90token.INTEGER:
		defaultKind = 4
	case f90token.LOGICAL:
		defaultKind = 4
	}
	if kind == 0 {
		kind = defaultKind
	}
	return tok, kind
}

func (tg *ToGo) transformBinaryExpr(vitgt *Varinfo, e *f90.BinaryExpr) (result ast.Expr, resultType *Varinfo, err error) {
	exprTarget := vitgt
	if vitgt.TypeToken() == f90token.LOGICAL && e.Op.IsNumericalOperator() {
		// We are targeting boolean but likely have numerical values, infer type.
		var leftType Varinfo
		err = tg.repl.InferType(&leftType, e.Left)
		if err != nil {
			return nil, nil, tg.makeErr(e.Left, err.Error())
		}
		exprTarget = &leftType
	}
	left, leftType, err := tg.transformExpression(exprTarget, e.Left)
	if err != nil {
		return nil, nil, err
	}
	right, rightType, err := tg.transformExpression(exprTarget, e.Right)
	if err != nil {
		return nil, nil, err
	}
	if rightType.IsChar() || leftType.IsChar() {
		return tg.transformBinaryExprChar(vitgt, e.Op, left, right, leftType, rightType)
	}
	lpromote, rpromote, err := tg.checkPromotion(leftType, rightType)
	if err != nil {
		return nil, nil, tg.makeErr(e.Left, err.Error())
	}
	// Result type is result of promotion. In switch/case statement boolean returnType is set for logical operations.
	resultType = leftType
	if lpromote != nil {
		resultType = lpromote
	} else if rpromote != nil {
		resultType = rpromote
	}
	needsPromotion := lpromote != nil || rpromote != nil
	// Whole-array arithmetic: both operands arrays → intrinsic.ArrayAdd/Sub/Mul/Div[T](a, b)
	if leftType != nil && rightType != nil && leftType.IsArray() && rightType.IsArray() &&
		leftType.TypeToken() != f90token.TYPE && rightType.TypeToken() != f90token.TYPE {
		var funcName string
		switch e.Op {
		case f90token.Plus:
			funcName = "ArrayAdd"
		case f90token.Minus:
			funcName = "ArraySub"
		case f90token.Asterisk:
			funcName = "ArrayMul"
		case f90token.Slash:
			funcName = "ArrayDiv"
		}
		if funcName != "" {
			elemTok := leftType.TypeToken()
			goType := goTypeBasic(elemTok, 0)
			sel := &ast.SelectorExpr{X: _astIntrinsic, Sel: ast.NewIdent(funcName)}
			return &ast.CallExpr{
				Fun:  &ast.IndexExpr{X: sel, Index: goType},
				Args: []ast.Expr{left, right},
			}, leftType, nil
		}
	}
	// Map Fortran operator to Go operator
	var op token.Token
	switch e.Op {
	case f90token.Plus:
		op = token.ADD
	case f90token.Minus:
		op = token.SUB
	case f90token.Asterisk:
		op = token.MUL
	case f90token.Slash:
		op = token.QUO
	case f90token.DoubleStar:
		// Power operator: x ** y → intrinsic.POW[T](x, y)
		left = tg.wrapConversion(vitgt, leftType, left)
		right = tg.wrapConversion(vitgt, rightType, right)
		fnName := "POW"
		switch leftType.TypeToken() {
		case f90token.COMPLEX, f90token.DOUBLECOMPLEX:
			fnName = "CPOW"
		}
		sel := &ast.SelectorExpr{X: _astIntrinsic, Sel: ast.NewIdent(fnName)}
		var funcExpr ast.Expr = sel
		typTok := vitgt.TypeToken()
		if typTok == f90token.DIMENSION {
			typTok = leftType.TypeToken() // use element type for array power
		}
		if vitgt != nil && !isGenericVarinfo(vitgt) && typTok != f90token.DIMENSION {
			goType := goTypeBasic(typTok, 0)
			funcExpr = &ast.IndexExpr{X: sel, Index: goType}
		}
		return &ast.CallExpr{
			Fun:  funcExpr,
			Args: []ast.Expr{left, right},
		}, vitgt, nil
	case f90token.EQ, f90token.EqEq:
		// Special case: array comparison → ArraySetEqual
		if leftType != nil && rightType != nil && leftType.IsArray() && rightType.IsArray() {
			return tg.transformArrayComparison("ArraySetEqual", left, right, leftType)
		}
		// Special case: pointer comparison to 0 → ptr.DataUnsafe() == nil
		if isPointerZeroComparison(leftType, rightType, e.Right) {
			return tg.pointerNilComparison(left, token.EQL), _tgtBool, nil
		}
		if isPointerZeroComparison(rightType, leftType, e.Left) {
			return tg.pointerNilComparison(right, token.EQL), _tgtBool, nil
		}
		op = token.EQL
		resultType = _tgtBool
	case f90token.NE, f90token.NotEquals:
		// Special case: pointer comparison to 0 → ptr.DataUnsafe() != nil
		if isPointerZeroComparison(leftType, rightType, e.Right) {
			return tg.pointerNilComparison(left, token.NEQ), _tgtBool, nil
		}
		if isPointerZeroComparison(rightType, leftType, e.Left) {
			return tg.pointerNilComparison(right, token.NEQ), _tgtBool, nil
		}
		op = token.NEQ
		resultType = _tgtBool
	case f90token.LT, f90token.Less:
		op = token.LSS
		resultType = _tgtBool
	case f90token.LE, f90token.LessEq:
		op = token.LEQ
		resultType = _tgtBool
	case f90token.GT, f90token.Greater:
		op = token.GTR
		resultType = _tgtBool
	case f90token.GE, f90token.GreaterEq:
		op = token.GEQ
		resultType = _tgtBool
	case f90token.AND:
		op = token.LAND
		needsPromotion = false
		resultType = _tgtBool
	case f90token.OR:
		op = token.LOR
		needsPromotion = false
		resultType = _tgtBool
	case f90token.EQV:
		op = token.EQL
		needsPromotion = false
		resultType = _tgtBool
	case f90token.NEQV:
		op = token.NEQ
		needsPromotion = false
		resultType = _tgtBool
	case f90token.StringConcat:
		return nil, nil, tg.makeErr(e, "string concat handled in transformExpression")
	default:
		return nil, nil, tg.makeErr(e, fmt.Sprintf("unsupported binary operator %v", e.Op))
	}

	// Promote operands to common type for arithmetic/comparison ops
	if needsPromotion {
		if lpromote != nil {
			left = tg.wrapConversion(lpromote, leftType, left)
		} else if rpromote != nil {
			right = tg.wrapConversion(rpromote, rightType, right)
		}
	}
	return &ast.BinaryExpr{
		X:  left,
		Op: op,
		Y:  right,
	}, resultType, nil
}

// transformArrayComparison generates intrinsic.ArraySetEqual(nil, left, right) or similar
// for element-wise array comparison operations.
func (tg *ToGo) transformArrayComparison(funcName string, left, right ast.Expr, leftType *Varinfo) (ast.Expr, *Varinfo, error) {
	// Generate: intrinsic.ArraySetEqual[T](nil, left, right)
	sel := &ast.SelectorExpr{X: _astIntrinsic, Sel: ast.NewIdent(funcName)}
	goType := goTypeBasic(leftType.TypeToken(), 0)
	funcExpr := &ast.IndexExpr{X: sel, Index: goType}
	call := &ast.CallExpr{
		Fun:  funcExpr,
		Args: []ast.Expr{ast.NewIdent("nil"), left, right},
	}
	// Result is an array of bool with same shape as input
	return call, _tgtArray(f90token.LOGICAL), nil
}

func (tg *ToGo) transformBinaryExprChar(vitgt *Varinfo, op f90token.Token, left, right ast.Expr, leftType, rightType *Varinfo) (result ast.Expr, resultType *Varinfo, err error) {
	// Convert operands to Go strings for comparison
	// - String literals are already Go strings
	// - CharacterArray variables need .String() method call
	leftStr := tg.charToGoString(left, leftType)
	rightStr := tg.charToGoString(right, rightType)

	// Map Fortran comparison operator to Go operator
	var goOp token.Token
	switch op {
	case f90token.EQ, f90token.EqEq:
		goOp = token.EQL
	case f90token.NE, f90token.NotEquals:
		goOp = token.NEQ
	case f90token.LT, f90token.Less:
		goOp = token.LSS
	case f90token.LE, f90token.LessEq:
		goOp = token.LEQ
	case f90token.GT, f90token.Greater:
		goOp = token.GTR
	case f90token.GE, f90token.GreaterEq:
		goOp = token.GEQ
	default:
		return nil, nil, tg.makeErrAtStmt("unsupported operator for character types: " + op.String())
	}

	return &ast.BinaryExpr{
		X:  leftStr,
		Op: goOp,
		Y:  rightStr,
	}, _tgtBool, nil
}

// charToGoString converts a character expression to a Go string expression.
// String literals are already Go strings, CharacterArray variables need .String() call.
func (tg *ToGo) charToGoString(expr ast.Expr, exprType *Varinfo) ast.Expr {
	if exprType.TypeToken() == f90token.StringLit {
		// Already a Go string literal
		return expr
	}
	// CharacterArray - call .String() method
	return &ast.CallExpr{
		Fun: &ast.SelectorExpr{
			X:   expr,
			Sel: ast.NewIdent("String"),
		},
	}
}

// isPointerZeroComparison checks if ptrType is a Cray pointer and other is literal 0.
func isPointerZeroComparison(ptrType, otherType *Varinfo, otherExpr f90.Expression) bool {
	if ptrType == nil || ptrType.pointee == "" {
		return false // Not a pointer variable
	}
	// Check if other is an integer literal 0
	if lit, ok := otherExpr.(*f90.IntegerLiteral); ok && lit.Value == 0 {
		return true
	}
	return false
}

// pointerNilComparison generates: ptr.DataUnsafe() == nil (or !=)
func (tg *ToGo) pointerNilComparison(ptrExpr ast.Expr, op token.Token) ast.Expr {
	return &ast.BinaryExpr{
		X: &ast.CallExpr{
			Fun: &ast.SelectorExpr{X: ptrExpr, Sel: ast.NewIdent("DataUnsafe")},
		},
		Op: op,
		Y:  ast.NewIdent("nil"),
	}
}

func (tg *ToGo) transformFunctionCall(vitgt *Varinfo, e *f90.CallExpr) (result ast.Expr, resultType *Varinfo, err error) {
	vi := tg.repl.Var(e.Name)
	if vi != nil {
		// Check if it's a statement function
		if vi.IsStmtFunc() {
			return tg.expandStatementFunction(vitgt, vi, e.Args)
		}
		// It's a declared variable - route to array access handler
		// which properly handles both element access and range expressions
		result, err = tg.transformArrayRef(vitgt, e)
		// Substring access of a CHARACTER scalar (ranged: str(s:e)) returns Go string, not CharacterArray.
		if vi.decl.Type.Token == f90token.CHARACTER && !vi.IsArray() && f90.IsRanged(e.Args...) {
			return result, _tgtStringLit, err
		}
		return result, vi, err
	}

	fi := tg.ContainedOrUsed(e.Name)
	if fi == nil {
		// Fall back to containedStack search (host-association from USEd modules not in direct scope)
		for i := range tg.containedStack {
			if strings.EqualFold(tg.containedStack[i].UnitName(), e.Name) {
				if d, ok := tg.containedStack[i].UnitData().(*ParserUnitData); ok {
					fi = d
				}
				break
			}
		}
	}
	if fi == nil {
		// Try standard intrinsic first
		lookup := f90token.LookupIntrinsic(e.Name)
		args := e.Args
		if fnV2 := getIntrinsic(lookup); fnV2 != nil {
			// Strip KIND= keyword arg and redirect intrinsic if needed (e.g. REAL(x, KIND=8) → float64)
			if stripped, kindExpr := stripKindArg(args); kindExpr != nil {
				args = stripped
				if kindTok, _ := tg.resolveKindToken(kindExpr); kindTok != 0 {
					lookup = redirectIntrinsicForKind(lookup, kindTok)
					fnV2 = getIntrinsic(lookup)
				}
			}
			// Infer argument types for better matching
			argTypes := make([]*Varinfo, len(args))
			for i, arg := range args {
				var vi Varinfo
				if err := tg.repl.InferType(&vi, arg); err == nil {
					argTypes[i] = &vi
				}
			}
			// Try type-aware matching first, fall back to arg count matching
			call := fnV2.findBestCallWithTypes(argTypes)
			if call == nil {
				call = fnV2.findBestCall(len(args))
			}
			if call != nil {
				return tg.intrinsicExprV2(vitgt, fnV2, call, args...)
			}
		}

		// Try vendor intrinsic
		vendorTok := f90token.LookupVendorIntrinsic(e.Name)
		if vendorTok != 0 {
			// Special handling for MALLOC - type parameter comes from target's pointee
			if vendorTok == f90token.VendorMALLOC {
				return tg.transformMALLOC(vitgt, e)
			}
			// Generic vendor intrinsic handling
			if fnV2 := getVendoredIntrinsic(vendorTok); fnV2 != nil {
				argTypes := make([]*Varinfo, len(e.Args))
				for i, arg := range e.Args {
					var vi Varinfo
					if err := tg.repl.InferType(&vi, arg); err == nil {
						argTypes[i] = &vi
					}
				}
				call := fnV2.findBestCallWithTypes(argTypes)
				if call == nil {
					call = fnV2.findBestCall(len(e.Args))
				}
				if call != nil {
					return tg.intrinsicExprV2(vitgt, fnV2, call, e.Args...)
				}
			}
			return nil, nil, tg.makeErr(e, "vendor intrinsic "+e.Name+" not implemented")
		}

		if lookup == f90token.IntrinsicPRESENT && len(e.Args) == 1 {
			// PRESENT(arg) → arg != nil (optional args are pointers; always true if not optional)
			if ident, ok := e.Args[0].(*f90.Identifier); ok {
				if vi := tg.repl.Var(ident.Value); vi != nil && tg.isGoPointer(vi) {
					argExpr := tg.astVarExpr(vi)
					return &ast.BinaryExpr{X: argExpr, Op: token.NEQ, Y: ast.NewIdent("nil")}, _tgtBool, nil
				}
			}
			return _astTrue, _tgtBool, nil
		}
		return nil, nil, tg.makeErr(e, "unknown intrinsic: "+e.Name)
	}

	params := fi.ProcedureParams()
	if len(e.Args) != len(params) {
		return nil, nil, tg.makeErr(e, "parameter length mismatch")
	}
	// Transform args
	var args []ast.Expr
	for i, arg := range e.Args {
		argExpr, _, err := tg.transformExpression(&params[i], arg)
		if err != nil {
			return nil, nil, err
		}
		args = append(args, argExpr)
	}
	// Regular function call
	return &ast.CallExpr{
		Fun:  ast.NewIdent(fi.name),
		Args: args,
	}, fi.returnType, nil
}

// expandStatementFunction expands a statement function call by substituting
// parameters with arguments and transforming the expression.
func (tg *ToGo) expandStatementFunction(vitgt *Varinfo, vi *Varinfo, args []f90.Expression) (ast.Expr, *Varinfo, error) {
	params := vi.StmtFuncParams()
	expr := vi.StmtFuncExpr()
	if len(args) != len(params) {
		return nil, nil, fmt.Errorf("statement function %s: expected %d args, got %d", vi.Identifier(), len(params), len(args))
	}
	// Substitute parameters in expression
	substituted := substituteParams(expr, params, args)
	// Transform the substituted expression
	return tg.transformExpression(vitgt, substituted)
}

// substituteParams replaces identifier references to parameters with argument expressions.
func substituteParams(expr f90.Expression, params []string, args []f90.Expression) f90.Expression {
	switch e := expr.(type) {
	case *f90.Identifier:
		for i := range params {
			if strings.EqualFold(params[i], e.Value) {
				return args[i]
			}
		}
		return e
	case *f90.BinaryExpr:
		return &f90.BinaryExpr{
			Left:     substituteParams(e.Left, params, args),
			Op:       e.Op,
			Right:    substituteParams(e.Right, params, args),
			Position: e.Position,
		}
	case *f90.UnaryExpr:
		return &f90.UnaryExpr{
			Op:       e.Op,
			Operand:  substituteParams(e.Operand, params, args),
			Position: e.Position,
		}
	case *f90.CallExpr:
		newArgs := make([]f90.Expression, len(e.Args))
		for i, arg := range e.Args {
			newArgs[i] = substituteParams(arg, params, args)
		}
		return &f90.CallExpr{
			Name:     e.Name,
			Args:     newArgs,
			Position: e.Position,
		}
	case *f90.ParenExpr:
		return &f90.ParenExpr{
			Expr:     substituteParams(e.Expr, params, args),
			Position: e.Position,
		}
	default:
		// Literals and other expressions don't need substitution
		return e
	}
}

// transformMALLOC handles MALLOC intrinsic specially.
// MALLOC returns PointerTo[T] where T comes from the target's pointee type.
// Generates: intrinsic.MALLOC[T](size)
func (tg *ToGo) transformMALLOC(vitgt *Varinfo, e *f90.CallExpr) (result ast.Expr, resultType *Varinfo, err error) {
	if len(e.Args) != 1 {
		return nil, nil, tg.makeErr(e, "MALLOC requires 1 argument")
	}

	// Get the size argument
	sizeArg, _, err := tg.transformExpression(_tgtInt32, e.Args[0])
	if err != nil {
		return nil, nil, err
	}

	// Determine element type from target's pointee
	var elemType ast.Expr
	if vitgt.pointee != "" {
		pointeeVar := tg.repl.Var(vitgt.pointee)
		if pointeeVar != nil {
			elemType = tg.baseGotype(pointeeVar.TypeToken(), tg.resolveKind(pointeeVar))
		}
	}
	if elemType == nil {
		// Fallback: try to get type from target directly if it's a known pointer type
		return nil, nil, tg.makeErr(e, "MALLOC: cannot determine element type from target")
	}

	// Generate: intrinsic.MALLOC[T](size)
	call := &ast.CallExpr{
		Fun: &ast.IndexExpr{
			X:     &ast.SelectorExpr{X: _astIntrinsic, Sel: ast.NewIdent("MALLOC")},
			Index: elemType,
		},
		Args: []ast.Expr{sizeArg},
	}
	return call, vitgt, nil
}

func (tg *ToGo) transformArrayRef(vitgt *Varinfo, e *f90.CallExpr) (result ast.Expr, err error) {
	vi := tg.repl.Var(e.Name)
	isRanged := f90.IsRanged(e.Args...)
	if isRanged {
		// Check if it's a 1D character substring: str(2:4) → str.Substring(start, end)
		if len(e.Args) == 1 && vi.decl.Type.Token == f90token.CHARACTER && !vi.IsArray() {
			args, err := tg.transformRangeExprToArgs(e.Args[0].(*f90.RangeExpr), vi)
			receiver := tg.astVarExpr(vi)
			return &ast.CallExpr{
				Fun:  &ast.SelectorExpr{X: receiver, Sel: ast.NewIdent("Substring")},
				Args: args,
			}, err
		}
		// Multi-dimensional range access: arr(1:N, 2:M) → arr.View(R(...), R(...))
		return tg.transformArrayView(e, vi)
	}
	// Regular element access: arr(i,j) → arr.At(i, j)
	var args []ast.Expr
	for _, expr := range e.Args {
		arg, _, err := tg.transformExpression(_tgtInt, expr)
		if err != nil {
			return nil, err
		}
		args = append(args, arg)
	}
	receiver := tg.astVarExpr(vi)
	// Derived-type array elements must use AtPtr so struct fields are addressable.
	atMethod := "At"
	if vi.TypeToken() == f90token.TYPE {
		atMethod = "AtPtr"
	}
	atCall := &ast.CallExpr{
		Fun:  &ast.SelectorExpr{X: receiver, Sel: ast.NewIdent(atMethod)},
		Args: args,
	}
	if e.SecondaryAccess == nil {
		return atCall, nil
	}
	// arr(i,j)(s:e) → arr.At(i,j).Substring(s, e)
	rangeExpr, ok := e.SecondaryAccess.(*f90.RangeExpr)
	if !ok {
		return nil, tg.makeErr(e, "chained CallExpr with non-range secondary access not yet implemented")
	}
	subArgs, err := tg.transformRangeExprToArgs(rangeExpr, vi)
	if err != nil {
		return nil, err
	}
	return &ast.CallExpr{
		Fun:  &ast.SelectorExpr{X: atCall, Sel: ast.NewIdent("Substring")},
		Args: subArgs,
	}, nil
}

// astVarExpr returns the AST expression for a variable.
// COMMON block variables are local PointerTo[T] or *Array[T] variables.
func (tg *ToGo) astVarExpr(vi *Varinfo) ast.Expr {
	// COMMON variables are now local pointer variables (declared via DeclareCommon)
	// No longer use BlockName.fieldName selector access
	return ast.NewIdent(vi.Identifier())
}

// astSetCall generates: receiver.Set(value, indices...)
// Works for Array[T], PointerTo[T], and CharacterArray.
func (tg *ToGo) astSetCall(receiver, value ast.Expr, indices ...ast.Expr) *ast.CallExpr {
	args := make([]ast.Expr, 0, 1+len(indices))
	args = append(args, value)
	args = append(args, indices...)
	return &ast.CallExpr{
		Fun:  &ast.SelectorExpr{X: receiver, Sel: _astSet},
		Args: args,
	}
}

func (tg *ToGo) transformSetArrayRef(dst []ast.Stmt, fexpr *f90.CallExpr, rhs ast.Expr) (_ []ast.Stmt, err error) {
	if fexpr.SecondaryAccess != nil {
		return dst, tg.makeErr(fexpr, "chained CallExpr assignment not yet implemented")
	}
	vitgt := tg.repl.Var(fexpr.Name)
	if vitgt == nil {
		return dst, tg.makeErr(fexpr, "unknown array variable: "+fexpr.Name)
	}
	switch vitgt.decl.Type.Token {
	case f90token.CHARACTER:
		return tg.transformSetCharacterArray(dst, fexpr, rhs)
	}

	// Check for ranged array assignment: arr(1:N, 2:M) = v → arr.View(...).SetFrom(v)
	isRanged := f90.IsRanged(fexpr.Args...)
	if isRanged {
		// Check if it's a simple whole-array assignment (single ":" subscript with no bounds)
		if len(fexpr.Args) == 1 {
			if rng, ok := fexpr.Args[0].(*f90.RangeExpr); ok && rng.Start == nil && rng.End == nil {
				receiver := tg.astVarExpr(vitgt)
				gstmt := &ast.ExprStmt{
					X: &ast.CallExpr{
						Fun:  &ast.SelectorExpr{X: receiver, Sel: ast.NewIdent("SetAll")},
						Args: []ast.Expr{rhs},
					},
				}
				dst = append(dst, gstmt)
				return dst, nil
			}
		}
		// Partial range assignment: arr(1:N, 2:M) = v → arr.View(...).SetFrom(v)
		viewExpr, err := tg.transformArrayView(fexpr, vitgt)
		if err != nil {
			return dst, err
		}
		gstmt := &ast.ExprStmt{
			X: &ast.CallExpr{
				Fun:  &ast.SelectorExpr{X: viewExpr, Sel: ast.NewIdent("SetFrom")},
				Args: []ast.Expr{rhs},
			},
		}
		dst = append(dst, gstmt)
		return dst, nil
	}

	// Regular element assignment: arr(i) = v → arr.Set(value, int(indices)...)
	indices := make([]ast.Expr, 0, len(fexpr.Args))
	for _, expr := range fexpr.Args {
		arg, _, err := tg.transformExpression(_tgtInt, expr)
		if err != nil {
			return dst, err
		}
		indices = append(indices, arg)
	}
	receiver := tg.astVarExpr(vitgt)
	dst = append(dst, &ast.ExprStmt{X: tg.astSetCall(receiver, rhs, indices...)})
	return dst, nil
}

func (tg *ToGo) transformSetCharacterArray(dst []ast.Stmt, fexpr *f90.CallExpr, rhs ast.Expr) (_ []ast.Stmt, err error) {
	vi := tg.repl.Var(fexpr.Name)
	isRanged := f90.IsRanged(fexpr.Args...)

	// If this is an array of characters with integer subscripts (not range), use AtPtr().SetFromString()
	if len(fexpr.Args) > 0 && !isRanged {
		// CHARACTER array element assignment: arr(i,j) = 'ABC' → arr.AtPtr(i,j).SetFromString("ABC")
		indices := make([]ast.Expr, 0, len(fexpr.Args))
		for _, expr := range fexpr.Args {
			arg, _, err := tg.transformExpression(_tgtInt, expr)
			if err != nil {
				return dst, err
			}
			indices = append(indices, arg)
		}
		receiver := tg.astVarExpr(vi)
		// Generate: arr.AtPtr(indices...).SetFromString(rhs)
		atPtrCall := &ast.CallExpr{
			Fun:  &ast.SelectorExpr{X: receiver, Sel: ast.NewIdent("AtPtr")},
			Args: indices,
		}
		dst = append(dst, &ast.ExprStmt{
			X: &ast.CallExpr{
				Fun:  &ast.SelectorExpr{X: atPtrCall, Sel: ast.NewIdent("SetFromString")},
				Args: []ast.Expr{rhs},
			},
		})
		return dst, nil
	}

	// CHARACTER array with a single range subscript: ctmp(1:2) = arr → ctmp.View(R(1,2)).SetFrom(arr)
	if vi.IsArray() && isRanged && len(fexpr.Args) == 1 && fexpr.SecondaryAccess == nil {
		viewExpr, err := tg.transformArrayView(fexpr, vi)
		if err != nil {
			return dst, err
		}
		dst = append(dst, &ast.ExprStmt{
			X: &ast.CallExpr{
				Fun:  &ast.SelectorExpr{X: viewExpr, Sel: ast.NewIdent("SetFrom")},
				Args: []ast.Expr{rhs},
			},
		})
		return dst, nil
	}
	if vi.IsArray() || isRanged && len(fexpr.Args) > 1 || fexpr.SecondaryAccess != nil {
		return dst, tg.makeErrWithPos(fexpr.Position, "unsupported character type attributes for range set")
	}
	args, err := tg.transformRangeExprToArgs(fexpr.Args[0].(*f90.RangeExpr), vi)
	if err != nil {
		return dst, err
	}

	args = append(args, rhs)
	receiver := tg.astVarExpr(vi)
	gstmt := &ast.ExprStmt{
		X: &ast.CallExpr{
			Fun:  &ast.SelectorExpr{X: receiver, Sel: ast.NewIdent("SetSubstring")},
			Args: args,
		},
	}
	dst = append(dst, gstmt)
	return dst, nil
}

func (tg *ToGo) transformRangeExprToArgs(rng *f90.RangeExpr, vi *Varinfo) (_ []ast.Expr, err error) {
	if vi == _tgtInt32 || vi == _tgtInt {
		panic("misuse of varinfo")
	}
	var start, end ast.Expr = _astOne, nil
	if rng.Start != nil {
		start, _, err = tg.transformExpression(_tgtInt, rng.Start)
	}
	if err == nil && rng.End != nil {
		end, _, err = tg.transformExpression(_tgtInt, rng.End)
		return []ast.Expr{start, end}, err
	}
	// Set default end size.
	if err == nil && vi.decl.Type.Token == f90token.CHARACTER && vi.decl.Charlen() != nil {
		end, _, err = tg.transformExpression(_tgtInt, vi.decl.Charlen())
	} else if err == nil && vi.decl.Dimension().CanExpr() {
		end, _, err = tg.transformExpression(_tgtInt, vi.decl.Dimension().Expr())
	} else {
		err = tg.makeErrWithPos(rng.Position, "unsupported variable dimension for assignment")
	}
	if err != nil {
		return nil, err
	}
	return []ast.Expr{start, end}, nil
}

// transformRangeToViewArg transforms a RangeExpr to intrinsic.R() or RS() call.
// Returns ast.Expr for: intrinsic.R(start, end) or intrinsic.RS(start, end, stride)
func (tg *ToGo) transformRangeToViewArg(rng *f90.RangeExpr, vi *Varinfo, dim int) (ast.Expr, error) {
	var start, end ast.Expr
	var err error

	// Start: nil → 1, else transform
	if rng.Start == nil {
		start = _astOne
	} else {
		start, _, err = tg.transformExpression(_tgtInt, rng.Start)
		if err != nil {
			return nil, err
		}
	}

	// End: nil → use variable's upper bound for this dimension, else transform
	if rng.End == nil {
		// Get upper bound from variable's dimension declaration
		dims := vi.decl.Dimension()
		if dims != nil && dim < len(dims.Bounds) && dims.Bounds[dim].Upper != nil {
			end, _, err = tg.transformExpression(_tgtInt, dims.Bounds[dim].Upper)
		} else {
			// TODO(pato): Does this merit a warning or is it fine?
			// warn(tg.forceStrPos(rng.Position) + " Allocatable/assumed-shape array: use runtime length vi.Len()")
			// Allocatable/assumed-shape array: use runtime length vi.Len()
			end = &ast.CallExpr{
				Fun: &ast.SelectorExpr{
					X:   tg.astVarExpr(vi),
					Sel: ast.NewIdent("Len"),
				},
			}
		}
	} else {
		end, _, err = tg.transformExpression(_tgtInt, rng.End)
	}
	if err != nil {
		return nil, err
	}

	// Stride: nil → use R(), else use RS()
	if rng.Stride == nil {
		// intrinsic.R(start, end)
		return &ast.CallExpr{
			Fun:  &ast.SelectorExpr{X: ast.NewIdent("intrinsic"), Sel: ast.NewIdent("R")},
			Args: []ast.Expr{start, end},
		}, nil
	}

	// intrinsic.RS(start, end, stride)
	stride, _, err := tg.transformExpression(_tgtInt, rng.Stride)
	if err != nil {
		return nil, err
	}
	return &ast.CallExpr{
		Fun:  &ast.SelectorExpr{X: ast.NewIdent("intrinsic"), Sel: ast.NewIdent("RS")},
		Args: []ast.Expr{start, end, stride},
	}, nil
}

// transformArrayView generates arr.View(R(...), R(...), ...) for ranged subscripts.
// Each subscript that is a RangeExpr becomes a Range argument.
// Each subscript that is a scalar index becomes R(idx, idx) to select that single row/col.
func (tg *ToGo) transformArrayView(call *f90.CallExpr, vi *Varinfo) (ast.Expr, error) {
	rangeArgs := make([]ast.Expr, 0, len(call.Args))

	for dim, arg := range call.Args {
		if rng, ok := arg.(*f90.RangeExpr); ok {
			// Range expression: transform to R() or RS()
			rangeArg, err := tg.transformRangeToViewArg(rng, vi, dim)
			if err != nil {
				return nil, err
			}
			rangeArgs = append(rangeArgs, rangeArg)
		} else {
			// Scalar index: convert to R(idx, idx) for single element selection
			idx, _, err := tg.transformExpression(_tgtInt, arg)
			if err != nil {
				return nil, err
			}
			rangeArgs = append(rangeArgs, &ast.CallExpr{
				Fun:  &ast.SelectorExpr{X: ast.NewIdent("intrinsic"), Sel: ast.NewIdent("R")},
				Args: []ast.Expr{idx, idx},
			})
		}
	}

	receiver := tg.astVarExpr(vi)
	return &ast.CallExpr{
		Fun:  &ast.SelectorExpr{X: receiver, Sel: ast.NewIdent("View")},
		Args: rangeArgs,
	}, nil
}

// transformRangedArrayBinaryOp handles array range binary operations:
// arr(1:N) = arr(1:N) + other(1:N) → intrinsic.ArraySetAdd(arr.View(...), arr.View(...), other.View(...))
// Returns error if pattern doesn't match (caller should fall back to normal handling).
func (tg *ToGo) transformRangedArrayBinaryOp(dst []ast.Stmt, target *f90.CallExpr, binop *f90.BinaryExpr, targetVinfo *Varinfo) ([]ast.Stmt, error) {
	// Determine the function name based on operator
	var funcName string
	switch binop.Op {
	case f90token.Plus:
		funcName = "ArraySetAdd"
	case f90token.Minus:
		funcName = "ArraySetSub"
	case f90token.Asterisk:
		funcName = "ArraySetMul"
	case f90token.Slash:
		funcName = "ArraySetDiv"
	default:
		return nil, tg.makeErr(binop, "unsupported binary operator for array range operation")
	}

	// Transform target to View expression (destination)
	dstView, err := tg.transformArrayView(target, targetVinfo)
	if err != nil {
		return nil, err
	}

	// Transform LHS operand
	lhsView, err := tg.transformRangedOperand(binop.Left)
	if err != nil {
		return nil, err
	}

	// Transform RHS operand
	rhsView, err := tg.transformRangedOperand(binop.Right)
	if err != nil {
		return nil, err
	}

	// Generate: intrinsic.ArraySetAdd(dst, lhs, rhs)
	gstmt := &ast.ExprStmt{
		X: &ast.CallExpr{
			Fun:  &ast.SelectorExpr{X: ast.NewIdent("intrinsic"), Sel: ast.NewIdent(funcName)},
			Args: []ast.Expr{dstView, lhsView, rhsView},
		},
	}
	dst = append(dst, gstmt)
	return dst, nil
}

// transformRangedOperand transforms an operand that should be a ranged array access.
func (tg *ToGo) transformRangedOperand(expr f90.Expression) (ast.Expr, error) {
	call, ok := expr.(*f90.CallExpr)
	if !ok {
		return nil, tg.makeErr(expr, "expected array access in ranged binary operation")
	}
	vi := tg.repl.Var(call.Name)
	if vi == nil {
		return nil, tg.makeErr(expr, "unknown variable: "+call.Name)
	}
	if f90.IsRanged(call.Args...) {
		return tg.transformArrayView(call, vi)
	}
	// Not ranged - fall back to normal expression transformation
	return nil, tg.makeErr(expr, "expected ranged array access")
}

// goTypeBasic supports primitive type transformation from fortran to Go. Anything more complex requires being a method on [ToGo].
func goTypeBasic(tok f90token.Token, kind int) (goType ast.Expr) {
	switch tok {
	case f90token.CHARACTER:
		goType = _astTypeCharArray
	case f90token.FloatLit:
		goType = ast.NewIdent("float32") // untyped float literal defaults to float32
	case f90token.IntLit:
		goType = ast.NewIdent("int32") // untyped int literal defaults to int32
	case f90token.INTEGER:
		switch kind {
		case 1:
			goType = ast.NewIdent("int8")
		case 2:
			goType = ast.NewIdent("int16")
		case 8:
			goType = ast.NewIdent("int64")
		default:
			goType = ast.NewIdent("int32")
		}
	case f90token.REAL:
		switch kind {
		case 8, 16:
			goType = ast.NewIdent("float64")
		default:
			goType = ast.NewIdent("float32")
		}
	case f90token.DOUBLEPRECISION:
		goType = ast.NewIdent("float64")
	case f90token.COMPLEX:
		switch kind {
		case 16:
			goType = ast.NewIdent("complex128")
		default:
			goType = ast.NewIdent("complex64")
		}
	default:
		panic("not a basic type: " + tok.String())
	}
	return goType
}

// typeCompatible checks if argType is compatible with paramType.
func typeCompatible(paramType, argType *Varinfo) bool {
	if paramType == nil || argType == nil {
		return true
	}
	// Generic types accept anything of their category
	if paramType == _tgtGenericFloat {
		tok := argType.TypeToken()
		return tok == f90token.REAL || tok == f90token.DOUBLEPRECISION || tok == f90token.FloatLit
	}
	if paramType == _tgtGenericInt {
		tok := argType.TypeToken()
		return tok == f90token.INTEGER || tok == f90token.IntLit
	}
	// Exact type match
	return paramType.TypeToken() == argType.TypeToken()
}

// stripKindArg removes a KIND= keyword argument from args, returning (positional args, kind value expr).
// Returns (args, nil) if no KIND= argument is found.
func stripKindArg(args []f90.Expression) ([]f90.Expression, f90.Expression) {
	for i, arg := range args {
		binExpr, ok := arg.(*f90.BinaryExpr)
		if !ok || binExpr.Op != f90token.Equals {
			continue
		}
		ident, ok := binExpr.Left.(*f90.Identifier)
		if !ok || strings.ToUpper(ident.Value) != "KIND" {
			continue
		}
		stripped := make([]f90.Expression, 0, len(args)-1)
		stripped = append(stripped, args[:i]...)
		stripped = append(stripped, args[i+1:]...)
		return stripped, binExpr.Right
	}
	return args, nil
}

// resolveKindToken infers the Fortran type token corresponding to a KIND expression.
// Handles: KIND=8 → DOUBLEPRECISION, KIND=4 → REAL, KIND=KIND(expr) → type of expr.
func (tg *ToGo) resolveKindToken(kindExpr f90.Expression) (f90token.Token, error) {
	if lit, ok := kindExpr.(*f90.IntegerLiteral); ok {
		switch lit.Value {
		case 4:
			return f90token.REAL, nil
		case 8:
			return f90token.DOUBLEPRECISION, nil
		}
		return 0, fmt.Errorf("unsupported KIND literal: %d", lit.Value)
	}
	if call, ok := kindExpr.(*f90.CallExpr); ok && strings.ToUpper(call.Name) == "KIND" && len(call.Args) == 1 {
		var vi Varinfo
		if err := tg.repl.InferType(&vi, call.Args[0]); err != nil {
			return 0, fmt.Errorf("resolving KIND(expr): %w", err)
		}
		return vi.TypeToken(), nil
	}
	return 0, fmt.Errorf("unsupported KIND expression")
}

// redirectIntrinsicForKind remaps a type conversion intrinsic based on a resolved KIND type.
// Example: IntrinsicREAL + DOUBLEPRECISION → IntrinsicDBLE.
func redirectIntrinsicForKind(base f90token.Intrinsic, kindTok f90token.Token) f90token.Intrinsic {
	switch base {
	case f90token.IntrinsicREAL:
		if kindTok == f90token.DOUBLEPRECISION {
			return f90token.IntrinsicDBLE
		}
	}
	return base
}
