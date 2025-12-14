package fortran

import (
	"errors"
	"fmt"
	"go/ast"
	"go/token"
	"math"
	"slices"
	"strconv"
	"strings"

	f90 "github.com/soypat/go-fortran/ast"
	f90token "github.com/soypat/go-fortran/token"
)

// transformExpression transforms a single Fortran expression to a Go expression
func (tg *ToGo) transformExpression(vitgt *Varinfo, expr f90.Expression) (result ast.Expr, resultType *Varinfo, err error) {
	if expr == nil {
		return nil, nil, tg.makeErrAtStmt("nil expression")
	} else if vitgt == nil {
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
		resultType = _tgtGenericFloat
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
			err = tg.makeErrAtStmt("string concat special handling needed")
		} else {
			result, resultType, err = tg.transformBinaryExpr(vitgt, e)
		}

	case *f90.UnaryExpr:
		result, resultType, err = tg.transformUnaryExpr(vitgt, e)
	case *f90.ParenExpr:
		var inner ast.Expr
		// Parentheses for grouping - transform the inner expression and wrap in parens
		inner, resultType, err = tg.transformExpression(vitgt, e.Expr)
		if err != nil {
			return nil, nil, err
		}
		result = &ast.ParenExpr{X: inner}

	case *f90.ArrayConstructor:
		resultType = vitgt
		result, err = tg.transformArrayConstructor(vitgt, e)
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
	doWrap := vitgt == _tgtInt || resultType.IsPointer() != vitgt.IsPointer()
	if doWrap {
		result = tg.wrapConversion(vitgt, resultType, result)
	}
	return result, resultType, err
}

func (tg *ToGo) transformExprIdentifer(vitgt *Varinfo, e *f90.Identifier) (result ast.Expr, resultType *Varinfo, err error) {
	resultType = tg.repl.Var(e.Value)
	if resultType == nil {
		err = tg.makeErr(e, "identifier not found")
		return nil, nil, err
	}
	return tg.astVarExpr(resultType), resultType, nil
}

func (tg *ToGo) transformArrayConstructor(vitgt *Varinfo, e *f90.ArrayConstructor) (result ast.Expr, err error) {
	var elemType ast.Expr
	var elemVinfo *Varinfo = vitgt

	// Try to infer element type from target, but not DIMENSION (means "array of unknown type")
	targetTok := vitgt.typeToken()
	if targetTok != f90token.Undefined && targetTok != f90token.DIMENSION {
		elemType = tg.baseGotype(targetTok, tg.resolveKind(vitgt))
	} else if len(e.Values) > 0 {
		// Infer element type from first value. Use _tgtGenericInt as placeholder vitgt
		// since literals always return their own type regardless of target.
		_, elemVinfo, err = tg.transformExpression(_tgtGenericInt, e.Values[0])
		if err != nil {
			return nil, err
		}
		elemType = tg.baseGotype(elemVinfo.typeToken(), tg.resolveKind(elemVinfo))
	} else {
		return nil, tg.makeErr(e, "cannot infer array constructor element type")
	}

	// Transform values
	var elts []ast.Expr
	for _, val := range e.Values {
		elt, _, err := tg.transformExpression(elemVinfo, val)
		if err != nil {
			return nil, err
		}
		elts = append(elts, elt)
	}

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
	}, nil
}

// transformComponentAccess transforms Fortran component access (p%age) to Go field access (p.age).
func (tg *ToGo) transformComponentAccess(vitgt *Varinfo, e *f90.ComponentAccess) (result ast.Expr, resultType *Varinfo, err error) {
	// Get the base variable info for transformation
	var baseVinfo *Varinfo
	if ident, ok := e.Base.(*f90.Identifier); ok {
		baseVinfo = tg.repl.Var(ident.Value)
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
	ltok := left.typeToken()
	rtok := right.typeToken()
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
	if vitgt == _tgtBool && e.Op.IsNumericalOperator() {
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
		// Power operator: x ** y → intrinsic.POW[float T](x, y)
		left = tg.wrapConversion(vitgt, leftType, left)
		right = tg.wrapConversion(vitgt, rightType, right)
		sel := intrinsicSelGeneric("POW")
		return &ast.CallExpr{
			Fun:  sel(vitgt),
			Args: []ast.Expr{left, right},
		}, vitgt, nil
	case f90token.EQ, f90token.EqEq:
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
	if exprType.typeToken() == f90token.StringLit {
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
		return result, vi, err
	}

	// Special handling for MALLOC - type parameter comes from target's pointee
	if strings.EqualFold(e.Name, "MALLOC") {
		return tg.transformMALLOC(vitgt, e)
	}

	fi := tg.ContainedOrUsed(e.Name)
	if fi == nil {
		lookup := f90token.LookupIntrinsic(e.Name)
		// Try V2 system first with type-aware matching
		if fnV2 := getIntrinsicV2(lookup); fnV2 != nil {
			// Infer argument types for better matching
			argTypes := make([]*Varinfo, len(e.Args))
			for i, arg := range e.Args {
				var vi Varinfo
				if err := tg.repl.InferType(&vi, arg); err == nil {
					argTypes[i] = &vi
				}
			}
			call := fnV2.findBestCallWithTypes(argTypes)
			if call != nil {
				return tg.intrinsicExprV2(vitgt, fnV2, call, e.Args...)
			}
		}
		// Fall back to V1 system
		fn := getIntrinsic(lookup, len(e.Args))
		if fn == nil {
			return nil, nil, tg.makeErr(e, "unknown intrinsic: "+e.Name)
		}
		expr, resultType, err := tg.intrinsicExpr(vitgt, fn, e.Args...)
		if err != nil {
			return nil, nil, err
		}
		return expr, resultType, err
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
			elemType = tg.baseGotype(pointeeVar.typeToken(), tg.resolveKind(pointeeVar))
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
	if e.SecondaryAccess != nil {
		return nil, tg.makeErr(e, "chained CallExpr expression not yet implemented")
	}
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
	// Regular element access: arr(i) → arr.At(int(indices)...)
	var args []ast.Expr
	for _, expr := range e.Args {
		arg, _, err := tg.transformExpression(_tgtInt, expr)
		if err != nil {
			return nil, err
		}
		// Wrap in int() conversion for Go's array methods
		args = append(args, arg)
	}
	receiver := tg.astVarExpr(vi)
	return &ast.CallExpr{
		Fun:  &ast.SelectorExpr{X: receiver, Sel: ast.NewIdent("At")},
		Args: args,
	}, nil
}

// astVarExpr returns the AST expression for a variable, handling COMMON block access.
// It does not handle access patterns.
func (tg *ToGo) astVarExpr(vi *Varinfo) ast.Expr {
	if vi.common != "" {
		return &ast.SelectorExpr{
			X:   ast.NewIdent(vi.common),
			Sel: ast.NewIdent(vi.Identifier()),
		}
	}
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
		// Get upper bound from variable's dimension
		dims := vi.decl.Dimension()
		if dims != nil && dim < len(dims.Bounds) && dims.Bounds[dim].Upper != nil {
			end, _, err = tg.transformExpression(_tgtInt, dims.Bounds[dim].Upper)
		} else {
			return nil, tg.makeErrWithPos(rng.Position, "cannot determine upper bound for dimension")
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

func (tg *ToGo) transformExprSlice(vitgt *Varinfo, dst []ast.Expr, src []f90.Expression) (_ []ast.Expr, err error) {
	for i := range src {
		expr, _, err := tg.transformExpression(vitgt, src[i])
		if err != nil {
			return dst, err
		}
		dst = append(dst, expr)
	}
	return dst, nil
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
	}
	srcType := sourceType.typeToken()
	targetType := target.typeToken()
	if srcType == targetType || targetType == f90token.FloatLit {
		return expr
	}
	conv := tg.baseGotype(targetType, tg.resolveKind(target))
	return &ast.CallExpr{
		Fun:  conv,
		Args: []ast.Expr{expr},
	}
}

// wrapMethodIntrinsic wraps method intrinsic calls to match Fortran types.
// Go methods return native types (int, CharacterArray) but Fortran expects specific types.
func (tg *ToGo) wrapMethodIntrinsic(fn *intrinsicFn, call *ast.CallExpr) *ast.CallExpr {
	if fn.returnType == nil {
		return call
	}
	switch fn.returnType.typeToken() {
	case f90token.INTEGER:
		// Go method returns int, Fortran expects INTEGER (int32)
		return &ast.CallExpr{Fun: ast.NewIdent("int32"), Args: []ast.Expr{call}}
	case f90token.CHARACTER:
		// Go method returns CharacterArray, convert to string via .String()
		return &ast.CallExpr{
			Fun: &ast.SelectorExpr{X: call, Sel: ast.NewIdent("String")},
		}
	}
	return call
}

func (tg *ToGo) intrinsicExpr(vitgt *Varinfo, fn *intrinsicFn, args ...f90.Expression) (call *ast.CallExpr, resultType *Varinfo, err error) {
	if fn.isVariadic {
		if len(args) < len(fn.params) {
			return nil, nil, fmt.Errorf("intrinsic %s requires at least %d arguments, got %d", fn.name, len(fn.params), len(args))
		}
	} else if len(args) != len(fn.params) {
		return nil, nil, fmt.Errorf("intrinsic %s requires %d arguments, got %d", fn.name, len(fn.params), len(args))
	}
	var gargs []ast.Expr
	var firstArgType *Varinfo // Capture first argument's type for generic intrinsics
	for i := range args {
		// For variadic, use the first param type for extra args
		paramIdx := i
		if paramIdx >= len(fn.params) {
			paramIdx = 0
		}
		expr, argType, err := tg.transformExpression(fn.params[paramIdx], args[i])
		if err != nil {
			return nil, nil, err
		}
		if i == 0 {
			firstArgType = argType
		}
		gargs = append(gargs, expr)
	}

	if fn.method != "" {
		resultType = vitgt
		call = &ast.CallExpr{
			Fun: &ast.SelectorExpr{
				X:   gargs[0],
				Sel: ast.NewIdent(fn.method),
			},
			Args: gargs[1:],
		}
		// Method intrinsics need type conversion: Go methods return native types
		call = tg.wrapMethodIntrinsic(fn, call)
	} else {
		resultType = fn.returnType
		funcExpr := fn.expr
		if funcExpr == nil && fn.exprGeneric != nil {
			// For generic intrinsics, determine the type parameter:
			// - Variadic intrinsics (MIN/MAX): use target type if valid, else first arg type
			// - Other generics (ABS, etc.): use first argument's actual type
			genericType := firstArgType
			if fn.isVariadic && !isGenericVarinfo(vitgt) {
				genericType = vitgt
			}
			funcExpr = fn.exprGeneric(genericType)
			// Only use generic type as return type if no explicit return type was set
			if resultType == nil {
				resultType = genericType
			}
		}
		call = &ast.CallExpr{
			Fun:  funcExpr,
			Args: gargs,
		}
	}
	return call, resultType, nil
}

// intrinsicSel creates a selector expression for intrinsic.NAME
func intrinsicSel(name f90token.Intrinsic) func(*Varinfo) ast.Expr {
	return func(tp *Varinfo) ast.Expr {
		return &ast.SelectorExpr{X: _astIntrinsic, Sel: ast.NewIdent(name.String())}
	}
}

// intrinsicSelGeneric creates a type-parameterized selector: intrinsic.NAME[T]
func intrinsicSelGeneric(name string) func(*Varinfo) ast.Expr {
	return func(tp *Varinfo) ast.Expr {
		sel := &ast.SelectorExpr{X: _astIntrinsic, Sel: ast.NewIdent(name)}
		if tp == nil || isGenericVarinfo(tp) {
			return sel
		}
		goType := goTypeBasic(tp.typeToken(), 0)
		return &ast.IndexExpr{X: sel, Index: goType}
	}
}

// goTypeBasic supports primitive type transformation from fortran to Go. Anything more complex requires being a method on [ToGo].
func goTypeBasic(tok f90token.Token, kind int) (goType ast.Expr) {
	switch tok {
	case f90token.CHARACTER:
		goType = _astTypeCharArray
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

// makeIntrinsicFn creates an intrinsic that calls intrinsic.NAME (e.g., SQRT, SIN).
// returnType is nil if return type matches first param.
func makeIntrinsicFn(name f90token.Intrinsic, returnType *Varinfo, params ...*Varinfo) intrinsicFn {
	return intrinsicFn{
		name:        name,
		exprGeneric: intrinsicSel(name),
		returnType:  returnType,
		params:      params,
	}
}

// makeIntrinsicVariadic creates a variadic intrinsic like MAX, MIN.
func makeIntrinsicVariadic(name f90token.Intrinsic, returnType *Varinfo, variadicType *Varinfo) intrinsicFn {
	return intrinsicFn{
		name:        name,
		exprGeneric: intrinsicSel(name),
		returnType:  returnType,
		params:      []*Varinfo{variadicType},
		isVariadic:  true,
	}
}

// makeIntrinsicFnGeneric creates a generic intrinsic that emits intrinsic.NAME[T].
func makeIntrinsicFnGeneric(name f90token.Intrinsic, returnType *Varinfo, params ...*Varinfo) intrinsicFn {
	return intrinsicFn{
		name:        name,
		exprGeneric: intrinsicSelGeneric(name.String()),
		returnType:  returnType,
		params:      params,
	}
}

// makeIntrinsicVariadicGeneric creates a variadic generic intrinsic like MAX, MIN.
func makeIntrinsicVariadicGeneric(name f90token.Intrinsic, returnType *Varinfo, variadicType *Varinfo) intrinsicFn {
	return intrinsicFn{
		name:        name,
		exprGeneric: intrinsicSelGeneric(name.String()),
		returnType:  returnType,
		params:      []*Varinfo{variadicType},
		isVariadic:  true,
	}
}

// makeIntrinsicCast creates a type cast intrinsic like REAL, INT, DBLE.
func makeIntrinsicCast(name f90token.Intrinsic, goType string, returnType, paramType *Varinfo) intrinsicFn {
	return intrinsicFn{
		name:       name,
		expr:       ast.NewIdent(goType),
		returnType: returnType,
		params:     []*Varinfo{paramType},
	}
}

// makeIntrinsicMethod creates a method-call intrinsic like LEN, TRIM.
func makeIntrinsicMethod(name f90token.Intrinsic, methodName string, returnType, receiverType *Varinfo, params ...*Varinfo) intrinsicFn {
	allParams := make([]*Varinfo, 0, 1+len(params))
	allParams = append(allParams, receiverType)
	allParams = append(allParams, params...)
	return intrinsicFn{
		name:       name,
		method:     methodName,
		returnType: returnType,
		params:     allParams,
	}
}

func (fn *intrinsicFn) inferReturnType(tgt *Varinfo) (inferred *Varinfo) {
	if fn.returnType != nil {
		inferred = fn.returnType
	} else {
		inferred = fn.params[0]
	}
	var dflt *Varinfo
	switch inferred {
	case _tgtGenericFloat:
		dflt = _tgtFloat32
	case _tgtGenericInt:
		dflt = _tgtInt32
	}
	if dflt != nil {
		if tgt == nil {
			inferred = dflt
		} else {
			inferred = tgt
		}
	}
	return inferred
}

func init() {
	slices.SortStableFunc(intrinsics, func(a, b intrinsicFn) int {
		diff := int(a.name) - int(b.name)
		if diff == 0 {
			diff = len(a.params) - len(b.params)
		}
		return diff
	})
}

func getIntrinsic(name f90token.Intrinsic, nargs int) *intrinsicFn {
	if name == 0 {
		return nil
	}
	idx, found := slices.BinarySearchFunc(intrinsics, name, func(e intrinsicFn, a f90token.Intrinsic) int {
		diff := int(e.name) - int(a)
		if diff == 0 {
			if e.isVariadic {
				return 0 // Prioritize isVariadic.
			}
			diff = len(e.params) - nargs
		}
		return diff
	})
	if !found {
		return nil
	} else if nargs == len(intrinsics[idx].params) || intrinsics[idx].isVariadic {
		return &intrinsics[idx]
	}
	return nil
}

type intrinsicFn struct {
	name        f90token.Intrinsic
	expr        ast.Expr
	exprGeneric func(tp *Varinfo) ast.Expr
	method      string
	returnType  *Varinfo
	params      []*Varinfo
	isVariadic  bool
}

var intrinsics = []intrinsicFn{
	// Type conversions
	makeIntrinsicCast(f90token.IntrinsicREAL, "float32", _tgtFloat32, _tgtGenericFloat),
	makeIntrinsicCast(f90token.IntrinsicDBLE, "float64", _tgtFloat64, _tgtGenericFloat),
	makeIntrinsicCast(f90token.IntrinsicINT, "int32", _tgtInt32, _tgtGenericInt),
	makeIntrinsicCast(f90token.IntrinsicIFIX, "int32", _tgtInt32, _tgtGenericFloat), // IFIX truncates REAL to INTEGER

	// Math intrinsics - single float argument (return type matches input)
	makeIntrinsicFn(f90token.IntrinsicSQRT, nil, _tgtGenericFloat),
	makeIntrinsicFn(f90token.IntrinsicSIN, nil, _tgtGenericFloat),
	makeIntrinsicFn(f90token.IntrinsicCOS, nil, _tgtGenericFloat),
	makeIntrinsicFn(f90token.IntrinsicTAN, nil, _tgtGenericFloat),
	makeIntrinsicFn(f90token.IntrinsicASIN, nil, _tgtGenericFloat),
	makeIntrinsicFn(f90token.IntrinsicACOS, nil, _tgtGenericFloat),
	makeIntrinsicFn(f90token.IntrinsicATAN, nil, _tgtGenericFloat),
	makeIntrinsicFn(f90token.IntrinsicATAN2, nil, _tgtGenericFloat),
	makeIntrinsicFn(f90token.IntrinsicEXP, nil, _tgtGenericFloat),
	makeIntrinsicFn(f90token.IntrinsicLOG, nil, _tgtGenericFloat),
	makeIntrinsicFn(f90token.IntrinsicLOG10, nil, _tgtGenericFloat),
	makeIntrinsicFn(f90token.IntrinsicSINH, nil, _tgtGenericFloat),
	makeIntrinsicFn(f90token.IntrinsicCOSH, nil, _tgtGenericFloat),
	makeIntrinsicFn(f90token.IntrinsicTANH, nil, _tgtGenericFloat),
	makeIntrinsicFn(f90token.IntrinsicFLOOR, nil, _tgtGenericFloat),
	makeIntrinsicFn(f90token.IntrinsicCEILING, nil, _tgtGenericFloat),
	makeIntrinsicFn(f90token.IntrinsicAINT, nil, _tgtGenericFloat),
	makeIntrinsicFn(f90token.IntrinsicANINT, nil, _tgtGenericFloat),
	makeIntrinsicFn(f90token.IntrinsicNINT, _tgtInt32, _tgtGenericFloat), // NINT returns INTEGER

	// Math intrinsics - two float arguments
	makeIntrinsicFn(f90token.IntrinsicATAN2, nil, _tgtGenericFloat, _tgtGenericFloat),

	// Math intrinsics - signed/numeric
	makeIntrinsicFnGeneric(f90token.IntrinsicABS, nil, _tgtGenericFloat),
	makeIntrinsicFnGeneric(f90token.IntrinsicIABS, nil, _tgtGenericInt), // IABS for integers
	makeIntrinsicFn(f90token.IntrinsicCABS, _tgtFloat32, _tgtComplex64), // CABS returns REAL
	makeIntrinsicFn(f90token.IntrinsicSIGN, nil, _tgtGenericFloat, _tgtGenericFloat),
	makeIntrinsicFn(f90token.IntrinsicMOD, nil, _tgtGenericInt, _tgtGenericInt),
	makeIntrinsicFn(f90token.IntrinsicDIM, nil, _tgtGenericFloat, _tgtGenericFloat),
	makeIntrinsicFn(f90token.IntrinsicDPROD, _tgtFloat64, _tgtGenericFloat, _tgtGenericFloat), // DPROD returns DOUBLE

	// Variadic intrinsics
	makeIntrinsicVariadicGeneric(f90token.IntrinsicMAX, nil, _tgtGenericFloat),
	makeIntrinsicVariadicGeneric(f90token.IntrinsicMIN, nil, _tgtGenericFloat),

	// Character methods
	makeIntrinsicMethod(f90token.IntrinsicLEN, "Len", _tgtInt32, _tgtChar),
	makeIntrinsicMethod(f90token.IntrinsicLEN_TRIM, "LenTrim", _tgtInt32, _tgtChar),
	makeIntrinsicMethod(f90token.IntrinsicTRIM, "Trim", _tgtChar, _tgtChar),
	makeIntrinsicMethod(f90token.IntrinsicADJUSTL, "AdjustL", _tgtChar, _tgtChar),
	makeIntrinsicMethod(f90token.IntrinsicADJUSTR, "AdjustR", _tgtChar, _tgtChar),
	makeIntrinsicMethod(f90token.IntrinsicINDEX, "Index", _tgtInt32, _tgtChar, _tgtChar),

	// Array methods - 1 arg versions
	makeIntrinsicMethod(f90token.IntrinsicSIZE, "Size", _tgtInt32, _tgtArray),
	makeIntrinsicMethod(f90token.IntrinsicSHAPE, "Shape", nil, _tgtArray), // returns array
	// Array methods - 2 arg versions (with dimension)
	makeIntrinsicMethod(f90token.IntrinsicSIZE, "SizeDim", _tgtInt32, _tgtArray, _tgtInt32),
	makeIntrinsicMethod(f90token.IntrinsicLBOUND, "LowerDim", _tgtInt32, _tgtArray, _tgtInt32),
	makeIntrinsicMethod(f90token.IntrinsicUBOUND, "UpperDim", _tgtInt32, _tgtArray, _tgtInt32),

	// Array reduction intrinsics
	makeIntrinsicFnGeneric(f90token.IntrinsicDOT_PRODUCT, nil, _tgtArray, _tgtArray),
	makeIntrinsicFn(f90token.IntrinsicALL, _tgtBool, _tgtArray),

	// Note: MALLOC is handled specially in transformMALLOC, not here
}

type intrinsicFnV2 struct {
	redirectTo f90token.Intrinsic // signal this intrinsic is effectively replaced by another.
	calls      []intrinsicCall
	isVariadic bool
	f1         func(float64) float64
	f2         func(float64, float64) float64
}

type intrinsicCall struct {
	args []*Varinfo
	// if is nil return type decided by args[0] on instantiation.
	returnType *Varinfo
	// Rules for deciding how to use this prioritized in order of appearance:
	//  - If args[0] is array or character we know this is a method.
	//  - If first letter is upper case then is function in intrinsic package.
	//  - Else is a built in in Go.
	methodOrCall string
	allowKindArg bool
}

func makeCall(methodOrGoCall string, returnType *Varinfo, args ...*Varinfo) intrinsicCall {
	return intrinsicCall{
		methodOrCall: methodOrGoCall,
		args:         args,
		returnType:   returnType,
	}
}

// getIntrinsicV2 looks up a V2 intrinsic by token.
// Returns nil if not found or if redirectTo chain leads to undefined.
func getIntrinsicV2(lookup f90token.Intrinsic) *intrinsicFnV2 {
	if int(lookup) >= len(intrinsicsv2) {
		return nil
	}
	fn := &intrinsicsv2[lookup]
	// Follow redirect chain
	for fn.redirectTo != 0 {
		if int(fn.redirectTo) >= len(intrinsicsv2) {
			return nil
		}
		fn = &intrinsicsv2[fn.redirectTo]
	}
	if len(fn.calls) == 0 {
		return nil // Not defined in V2
	}
	return fn
}

// findBestCall finds the best matching intrinsicCall for the given arguments.
// Returns nil if no match found.
func (fn *intrinsicFnV2) findBestCall(nargs int) *intrinsicCall {
	for i := range fn.calls {
		c := &fn.calls[i]
		if fn.isVariadic {
			if nargs >= len(c.args) {
				return c
			}
		} else if len(c.args) == nargs {
			return c
		}
	}
	return nil
}

// findBestCallWithTypes finds the best matching intrinsicCall considering argument types.
func (fn *intrinsicFnV2) findBestCallWithTypes(argTypes []*Varinfo) *intrinsicCall {
	nargs := len(argTypes)
	var genericMatch *intrinsicCall
	for i := range fn.calls {
		c := &fn.calls[i]
		if fn.isVariadic {
			if nargs < len(c.args) {
				continue
			}
		} else if len(c.args) != nargs {
			continue
		}
		// Check if all argument types are compatible
		match := true
		isGeneric := false
		for j := range c.args {
			argIdx := j
			if argIdx >= nargs {
				break
			}
			paramType := c.args[j]
			argType := argTypes[argIdx]
			if !typeCompatible(paramType, argType) {
				match = false
				break
			}
			if isGenericVarinfo(paramType) {
				isGeneric = true
			}
		}
		if match {
			if !isGeneric {
				return c // Prefer exact matches over generic matches
			}
			if genericMatch == nil {
				genericMatch = c
			}
		}
	}
	return genericMatch
}

// typeCompatible checks if argType is compatible with paramType.
func typeCompatible(paramType, argType *Varinfo) bool {
	if paramType == nil || argType == nil {
		return true
	}
	// Generic types accept anything of their category
	if paramType == _tgtGenericFloat {
		tok := argType.typeToken()
		return tok == f90token.REAL || tok == f90token.DOUBLEPRECISION || tok == f90token.FloatLit
	}
	if paramType == _tgtGenericInt {
		tok := argType.typeToken()
		return tok == f90token.INTEGER || tok == f90token.IntLit
	}
	// Exact type match
	return paramType.typeToken() == argType.typeToken()
}

// intrinsicExprV2 transforms an intrinsic call using the V2 system.
func (tg *ToGo) intrinsicExprV2(vitgt *Varinfo, fn *intrinsicFnV2, call *intrinsicCall, args ...f90.Expression) (*ast.CallExpr, *Varinfo, error) {
	// Transform arguments
	var gargs []ast.Expr
	var firstArgType *Varinfo
	for i := range args {
		paramIdx := i
		if paramIdx >= len(call.args) {
			paramIdx = 0 // For variadic, use first param type
		}
		expr, argType, err := tg.transformExpression(call.args[paramIdx], args[i])
		if err != nil {
			return nil, nil, err
		}
		if i == 0 {
			firstArgType = argType
		}
		gargs = append(gargs, expr)
	}

	// Determine return type
	resultType := call.returnType
	if resultType == nil {
		resultType = firstArgType
	}

	// Determine function expression based on methodOrCall convention
	var funcExpr ast.Expr
	name := call.methodOrCall

	// Check if first arg is array/char → method call
	if len(call.args) > 0 && (call.args[0] == _tgtArray || call.args[0] == _tgtChar) {
		// Method call: gargs[0].methodName(gargs[1:]...)
		return &ast.CallExpr{
			Fun: &ast.SelectorExpr{
				X:   gargs[0],
				Sel: ast.NewIdent(name),
			},
			Args: gargs[1:],
		}, resultType, nil
	}

	// Check naming convention
	if len(name) > 0 && name[0] >= 'A' && name[0] <= 'Z' {
		// Uppercase: intrinsic.NAME or intrinsic.NAME[T]
		sel := &ast.SelectorExpr{X: _astIntrinsic, Sel: ast.NewIdent(name)}
		// Add type parameter only if first param is generic (function needs type instantiation)
		isGenericFn := len(call.args) > 0 && isGenericVarinfo(call.args[0])
		if isGenericFn && firstArgType != nil && !isGenericVarinfo(firstArgType) {
			goType := goTypeBasic(firstArgType.typeToken(), 0)
			funcExpr = &ast.IndexExpr{X: sel, Index: goType}
		} else {
			funcExpr = sel
		}
	} else {
		// Lowercase: Go builtin (int32, float32, etc.)
		funcExpr = ast.NewIdent(name)
	}

	return &ast.CallExpr{
		Fun:  funcExpr,
		Args: gargs,
	}, resultType, nil
}

var intrinsicsv2 = []intrinsicFnV2{
	f90token.IntrinsicREAL: {
		calls: []intrinsicCall{
			makeCall("float32", _tgtFloat32, _tgtGenericInt),
			makeCall("float32", _tgtFloat32, _tgtGenericFloat),
			makeCall("REALPART", _tgtFloat32, _tgtComplex64), // REAL(complex) returns real part
		},
	},
	f90token.IntrinsicINT: {
		calls: []intrinsicCall{
			makeCall("int32", _tgtInt32, _tgtGenericInt),
			makeCall("int32", _tgtInt32, _tgtGenericFloat),
		},
	},
	f90token.IntrinsicIFIX: {redirectTo: f90token.IntrinsicINT},
	f90token.IntrinsicMAX: {
		isVariadic: true,
		calls:      []intrinsicCall{makeCall("MAX", nil, _tgtGenericFloat)},
	},
	f90token.IntrinsicCMPLX: {
		calls: []intrinsicCall{
			// CMPLX(x) - single arg, imaginary = 0
			makeCall("CMPLX", _tgtComplex64, _tgtGenericFloat),
			makeCall("CMPLX", _tgtComplex64, _tgtGenericInt),
			// CMPLX(x, y) - two args, real and imaginary parts
			makeCall("CMPLX2", _tgtComplex64, _tgtGenericFloat, _tgtGenericFloat),
			makeCall("CMPLX2", _tgtComplex64, _tgtGenericInt, _tgtGenericInt),
		},
	},
	f90token.IntrinsicSQRT: {
		calls: []intrinsicCall{makeCall("SQRT", nil, _tgtGenericFloat)},
		f1:    math.Sqrt,
	},
	f90token.IntrinsicDSQRT: {redirectTo: f90token.IntrinsicSQRT},
	f90token.IntrinsicAIMAG: {
		calls: []intrinsicCall{makeCall("AIMAG", _tgtFloat32, _tgtComplex64)},
	},

	// Type conversions
	f90token.IntrinsicDBLE: {
		calls: []intrinsicCall{
			makeCall("float64", _tgtFloat64, _tgtGenericInt),
			makeCall("float64", _tgtFloat64, _tgtGenericFloat),
		},
	},

	// Trig intrinsics
	f90token.IntrinsicSIN: {
		calls: []intrinsicCall{makeCall("SIN", nil, _tgtGenericFloat)},
		f1:    math.Sin,
	},
	f90token.IntrinsicDSIN: {redirectTo: f90token.IntrinsicSIN},
	f90token.IntrinsicCOS: {
		calls: []intrinsicCall{makeCall("COS", nil, _tgtGenericFloat)},
		f1:    math.Cos,
	},
	f90token.IntrinsicDCOS: {redirectTo: f90token.IntrinsicCOS},
	f90token.IntrinsicTAN: {
		calls: []intrinsicCall{makeCall("TAN", nil, _tgtGenericFloat)},
		f1:    math.Tan,
	},
	f90token.IntrinsicDTAN: {redirectTo: f90token.IntrinsicTAN},
	f90token.IntrinsicASIN: {
		calls: []intrinsicCall{makeCall("ASIN", nil, _tgtGenericFloat)},
		f1:    math.Asin,
	},
	f90token.IntrinsicDASIN: {redirectTo: f90token.IntrinsicASIN},
	f90token.IntrinsicACOS: {
		calls: []intrinsicCall{makeCall("ACOS", nil, _tgtGenericFloat)},
		f1:    math.Acos,
	},
	f90token.IntrinsicDACOS: {redirectTo: f90token.IntrinsicACOS},
	f90token.IntrinsicATAN: {
		calls: []intrinsicCall{makeCall("ATAN", nil, _tgtGenericFloat)},
		f1:    math.Atan,
	},
	f90token.IntrinsicDATAN: {redirectTo: f90token.IntrinsicATAN},
	f90token.IntrinsicATAN2: {
		calls: []intrinsicCall{makeCall("ATAN2", nil, _tgtGenericFloat, _tgtGenericFloat)},
		f2:    math.Atan2,
	},
	f90token.IntrinsicDATAN2: {redirectTo: f90token.IntrinsicATAN2},

	// Hyperbolic intrinsics
	f90token.IntrinsicSINH: {
		calls: []intrinsicCall{makeCall("SINH", nil, _tgtGenericFloat)},
		f1:    math.Sinh,
	},
	f90token.IntrinsicDSINH: {redirectTo: f90token.IntrinsicSINH},
	f90token.IntrinsicCOSH: {
		calls: []intrinsicCall{makeCall("COSH", nil, _tgtGenericFloat)},
		f1:    math.Cosh,
	},
	f90token.IntrinsicDCOSH: {redirectTo: f90token.IntrinsicCOSH},
	f90token.IntrinsicTANH: {
		calls: []intrinsicCall{makeCall("TANH", nil, _tgtGenericFloat)},
		f1:    math.Tanh,
	},
	f90token.IntrinsicDTANH: {redirectTo: f90token.IntrinsicTANH},

	// Exponential and logarithmic
	f90token.IntrinsicEXP: {
		calls: []intrinsicCall{makeCall("EXP", nil, _tgtGenericFloat)},
		f1:    math.Exp,
	},
	f90token.IntrinsicDEXP: {redirectTo: f90token.IntrinsicEXP},
	f90token.IntrinsicLOG: {
		calls: []intrinsicCall{makeCall("LOG", nil, _tgtGenericFloat)},
		f1:    math.Log,
	},
	f90token.IntrinsicDLOG: {redirectTo: f90token.IntrinsicLOG},
	f90token.IntrinsicLOG10: {
		calls: []intrinsicCall{makeCall("LOG10", nil, _tgtGenericFloat)},
		f1:    math.Log10,
	},
	f90token.IntrinsicDLOG10: {redirectTo: f90token.IntrinsicLOG10},

	// Truncation and rounding
	f90token.IntrinsicFLOOR: {
		calls: []intrinsicCall{makeCall("FLOOR", nil, _tgtGenericFloat)},
		f1:    math.Floor,
	},
	f90token.IntrinsicCEILING: {
		calls: []intrinsicCall{makeCall("CEILING", nil, _tgtGenericFloat)},
		f1:    math.Ceil,
	},
	f90token.IntrinsicAINT: {
		calls: []intrinsicCall{makeCall("AINT", nil, _tgtGenericFloat)},
		f1:    math.Trunc,
	},
	f90token.IntrinsicANINT: {
		calls: []intrinsicCall{makeCall("ANINT", nil, _tgtGenericFloat)},
		f1:    math.Round,
	},
	f90token.IntrinsicNINT: {
		calls: []intrinsicCall{makeCall("NINT", _tgtInt32, _tgtGenericFloat)},
	},
	f90token.IntrinsicDNINT:  {redirectTo: f90token.IntrinsicANINT},
	f90token.IntrinsicIDNINT: {redirectTo: f90token.IntrinsicNINT},

	// Absolute value and sign
	f90token.IntrinsicABS: {
		calls: []intrinsicCall{makeCall("ABS", nil, _tgtGenericFloat)},
		f1:    math.Abs,
	},
	f90token.IntrinsicDABS: {redirectTo: f90token.IntrinsicABS},
	f90token.IntrinsicIABS: {
		calls: []intrinsicCall{makeCall("IABS", nil, _tgtGenericInt)},
	},
	f90token.IntrinsicCABS: {
		calls: []intrinsicCall{makeCall("CABS", _tgtFloat32, _tgtComplex64)},
	},
	f90token.IntrinsicSIGN: {
		calls: []intrinsicCall{makeCall("SIGN", nil, _tgtGenericFloat, _tgtGenericFloat)},
	},
	f90token.IntrinsicISIGN: {
		calls: []intrinsicCall{makeCall("SIGN", nil, _tgtGenericInt, _tgtGenericInt)},
	},
	f90token.IntrinsicDSIGN: {redirectTo: f90token.IntrinsicSIGN},

	// Modulo and remainder
	f90token.IntrinsicMOD: {
		calls: []intrinsicCall{
			makeCall("MOD", nil, _tgtGenericInt, _tgtGenericInt),
			makeCall("MODREAL", nil, _tgtGenericFloat, _tgtGenericFloat),
		},
	},
	f90token.IntrinsicDIM: {
		calls: []intrinsicCall{makeCall("DIM", nil, _tgtGenericFloat, _tgtGenericFloat)},
	},
	f90token.IntrinsicIDIM: {
		calls: []intrinsicCall{makeCall("DIM", nil, _tgtGenericInt, _tgtGenericInt)},
	},
	f90token.IntrinsicDDIM: {redirectTo: f90token.IntrinsicDIM},
	f90token.IntrinsicDPROD: {
		calls: []intrinsicCall{makeCall("DPROD", _tgtFloat64, _tgtFloat32, _tgtFloat32)},
	},

	// Variadic min
	f90token.IntrinsicMIN: {
		isVariadic: true,
		calls:      []intrinsicCall{makeCall("MIN", nil, _tgtGenericFloat)},
	},
	f90token.IntrinsicMAX0: {
		isVariadic: true,
		calls:      []intrinsicCall{makeCall("MAX", nil, _tgtGenericInt)},
	},
	f90token.IntrinsicMIN0: {
		isVariadic: true,
		calls:      []intrinsicCall{makeCall("MIN", nil, _tgtGenericInt)},
	},
	f90token.IntrinsicAMAX1: {redirectTo: f90token.IntrinsicMAX},
	f90token.IntrinsicAMIN1: {redirectTo: f90token.IntrinsicMIN},
	f90token.IntrinsicDMAX1: {redirectTo: f90token.IntrinsicMAX},
	f90token.IntrinsicDMIN1: {redirectTo: f90token.IntrinsicMIN},

	// Note: Character methods (LEN, TRIM, etc.) and array methods (SIZE, LBOUND, etc.)
	// are handled by V1 which has special method call handling.
}

func defaultVarinfo(tok f90token.Token) *Varinfo {
	return &Varinfo{
		_varname: fmt.Sprintf("<default %s varinfo>", tok.String()),
		decl:     &f90.DeclEntity{Type: &f90.TypeSpec{Token: tok}},
	}
}

var (
	_astFalse        = ast.NewIdent("false")
	_astTrue         = ast.NewIdent("true")
	_astSet          = ast.NewIdent("Set")
	_astOne          = &ast.BasicLit{Kind: token.INT, Value: "1"}
	_tgtInt32        = defaultVarinfo(f90token.INTEGER)
	_tgtInt          = defaultVarinfo(f90token.INTEGER)
	_tgtFloat32      = defaultVarinfo(f90token.REAL)
	_tgtBool         = defaultVarinfo(f90token.LOGICAL)
	_tgtFloat64      = defaultVarinfo(f90token.DOUBLEPRECISION)
	_tgtChar         = defaultVarinfo(f90token.CHARACTER)
	_tgtStringLit    = defaultVarinfo(f90token.StringLit)
	_tgtGenericFloat = defaultVarinfo(f90token.FloatLit)
	_tgtGenericInt   = defaultVarinfo(f90token.IntLit)
	_tgtComplex64    = defaultVarinfo(f90token.COMPLEX)
	_tgtArray        = defaultVarinfo(f90token.DIMENSION)
)

func isGenericVarinfo(vi *Varinfo) bool {
	return vi == _tgtGenericFloat || vi == _tgtGenericInt
}
