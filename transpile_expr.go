package fortran

import (
	"fmt"
	"go/ast"
	"go/token"
	"strconv"
	"strings"

	f90 "github.com/soypat/go-fortran/ast"
	f90token "github.com/soypat/go-fortran/token"
)

// transformExpression transforms a single Fortran expression to a Go expression
func (tg *ToGo) transformExpression(vitgt *varinfo, expr f90.Expression) (result ast.Expr, err error) {
	switch e := expr.(type) {
	case *f90.StringLiteral:
		result = &ast.BasicLit{
			Kind:  token.STRING,
			Value: fmt.Sprintf("%q", e.Value),
		}
	case *f90.IntegerLiteral:
		result = &ast.BasicLit{
			Kind:  token.INT,
			Value: strconv.FormatInt(e.Value, 10),
		}
	case *f90.RealLiteral:
		result = &ast.BasicLit{
			Kind:  token.FLOAT,
			Value: strconv.FormatFloat(e.Value, 'g', 16, 64),
		}
	case *f90.LogicalLiteral:
		// .TRUE. → true, .FALSE. → false
		if e.Value {
			result = _astTrue
		} else {
			result = _astFalse
		}

	case *f90.Identifier:
		result, err = tg.transformExprIdentifer(vitgt, e)
	case *f90.ArrayRef:
		result, err = tg.transformArrayRef(vitgt, e)
	case *f90.BinaryExpr:
		if e.Op == f90token.StringConcat {
			err = tg.makeErrAtStmt("string concat special handling needed")
		} else {
			result, err = tg.transformBinaryExpr(vitgt, e)
		}

	case *f90.UnaryExpr:
		result, err = tg.transformUnaryExpr(vitgt, e)
	case *f90.ParenExpr:
		// Parentheses for grouping - transform the inner expression and wrap in parens
		inner, err := tg.transformExpression(vitgt, e.Expr)
		if err != nil {
			return nil, err
		}
		result = &ast.ParenExpr{X: inner}
	case *f90.FunctionCall:
		// FunctionCall in expression: could be actual function or COMMON block array access
		result, err = tg.transformFunctionCall(vitgt, e)

	case *f90.ArrayConstructor:
		result, err = tg.transformArrayConstructor(vitgt, e)
	case *f90.RangeExpr:
		// Range expressions in subscripts (e.g., arr(1:5), str(2:3))
		// TODO: implement proper range transformation
		err = tg.makeErr(expr, "RangeExpr not yet implemented in transpiler")
	default:
		err = tg.makeErr(expr, "unsupported expression")
	}

	if result == nil && err == nil {
		err = tg.makeErr(expr, "unhandled expression type, result is nil")
	}
	return result, err
}

func (tg *ToGo) transformExprIdentifer(vitgt *varinfo, e *f90.Identifier) (result ast.Expr, err error) {
	vi := tg.repl.Var(e.Value)
	if vi == nil {
		err = tg.makeErr(e, "identifier not found")
		return nil, err
	}

	// Check if this identifier is in a COMMON block
	if vi.common != "" {
		blockName := strings.ToUpper(vi.common)
		varName := strings.ToUpper(vi.Identifier())
		result = &ast.SelectorExpr{
			X:   ast.NewIdent(blockName),
			Sel: ast.NewIdent(varName),
		}
	} else {
		result = ast.NewIdent(vi.Identifier())
	}
	return result, err
}

func (tg *ToGo) transformArrayConstructor(vitgt *varinfo, e *f90.ArrayConstructor) (result ast.Expr, err error) {
	// Transform (/ 1, 2, 3 /) → []T{1, 2, 3}
	var elts []ast.Expr
	for _, val := range e.Values {
		elt, err := tg.transformExpression(vitgt, val)
		if err != nil {
			return nil, err
		}
		elts = append(elts, elt)
	}

	// Create slice literal with inferred type
	return &ast.CompositeLit{
		Type: &ast.ArrayType{
			Elt: ast.NewIdent("int"), // TODO: infer element type from context
		},
		Elts: elts,
	}, nil
}

func (tg *ToGo) transformUnaryExpr(vitgt *varinfo, e *f90.UnaryExpr) (result ast.Expr, err error) {
	operand, err := tg.transformExpression(vitgt, e.Operand)
	if err != nil {
		return nil, err
	}

	var op token.Token
	switch e.Op {
	case f90token.Plus:
		// Unary plus: just return the operand
		return operand, nil
	case f90token.Minus:
		op = token.SUB
	case f90token.NOT:
		op = token.NOT
	default:
		return nil, tg.makeErr(e, fmt.Sprintf("unsupported unary operator %v", e.Op))
	}

	return &ast.UnaryExpr{
		Op: op,
		X:  operand,
	}, nil
}

func (tg *ToGo) transformBinaryExpr(vitgt *varinfo, e *f90.BinaryExpr) (result ast.Expr, err error) {
	// Infer types for type promotion
	var leftType, rightType varinfo
	if err := tg.repl.InferType(&leftType, e.Left); err != nil {
		return nil, err
	}
	if err := tg.repl.InferType(&rightType, e.Right); err != nil {
		return nil, err
	}
	promotion := tg.repl.promote(&leftType, &rightType)
	if promotion == 0 {
		return nil, tg.makeErr(e, fmt.Sprintf("cant promote %s to %s %q", leftType.val.tok, rightType.val.tok, e.AppendString(nil)))
	}

	left, err := tg.transformExpression(vitgt, e.Left)
	if err != nil {
		return nil, err
	}
	right, err := tg.transformExpression(vitgt, e.Right)
	if err != nil {
		return nil, err
	}

	// Map Fortran operator to Go operator
	var op token.Token
	needsPromotion := promotion != leftType.val.tok // Most operators need type-matched operands
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
		// Power operator: x ** y → math.Pow(x, y)
		// math.Pow requires float64 arguments
		left = tg.wrapConversion(vitgt.decl.Type.Token, &leftType, left)
		right = tg.wrapConversion(vitgt.decl.Type.Token, &rightType, right)
		sel := intrinsicSel("POW")
		return &ast.CallExpr{
			Fun:  sel(vitgt),
			Args: []ast.Expr{left, right},
		}, nil
	case f90token.EQ, f90token.EqEq:
		op = token.EQL
	case f90token.NE, f90token.NotEquals:
		op = token.NEQ
	case f90token.LT, f90token.Less:
		op = token.LSS
	case f90token.LE, f90token.LessEq:
		op = token.LEQ
	case f90token.GT, f90token.Greater:
		op = token.GTR
	case f90token.GE, f90token.GreaterEq:
		op = token.GEQ
	case f90token.AND:
		op = token.LAND
		needsPromotion = false // Logical ops don't need numeric promotion
	case f90token.OR:
		op = token.LOR
		needsPromotion = false
	case f90token.StringConcat:
		return nil, tg.makeErr(e, "string concat handled in transformExpression")
	default:
		return nil, tg.makeErr(e, fmt.Sprintf("unsupported binary operator %v", e.Op))
	}

	// Promote operands to common type for arithmetic/comparison ops
	if needsPromotion {
		left = tg.wrapConversion(promotion, &leftType, left)
		right = tg.wrapConversion(promotion, &rightType, right)
	}

	return &ast.BinaryExpr{
		X:  left,
		Op: op,
		Y:  right,
	}, nil
}

func (tg *ToGo) transformFunctionCall(vitgt *varinfo, e *f90.FunctionCall) (result ast.Expr, err error) {
	vi := tg.repl.Var(e.Name)
	if vi != nil {
		// Check if this is a declared variable (COMMON block array access)
		args, err := tg.transformExprSlice(_tgtInt32, nil, e.Args)
		// Treat as array element access: arr.At(indices...)
		return tg.astMethodCall(e.Name, "At", args...), err
	}
	fi := tg.ContainedOrExtern(e.Name)
	if fi == nil {
		fn := getIntrinsic(e.Name, len(e.Args))
		if fn == nil {
			return result, tg.makeErr(e, "function/intrinsic not found: "+e.Name)
		}
		expr, err := tg.intrinsicExpr(vitgt, fn, e.Args...)
		if err != nil {
			return nil, err
		}
		return expr, err
	}

	params := fi.ProcedureParams()
	if len(e.Args) != len(params) {
		return result, tg.makeErr(e, "parameter length mismatch")
	}
	// Transform args
	var args []ast.Expr
	for i, arg := range e.Args {
		argExpr, err := tg.transformExpression(&params[i], arg)
		if err != nil {
			return nil, err
		}
		args = append(args, argExpr)
	}
	// Regular function call
	return &ast.CallExpr{
		Fun:  ast.NewIdent(fi.name),
		Args: args,
	}, nil
}

func (tg *ToGo) transformArrayRef(vitgt *varinfo, e *f90.ArrayRef) (result ast.Expr, err error) {
	if e.Base != nil {
		return nil, tg.makeErr(e, "chained ArrayRef expression not yet implemented")
	}
	vi := tg.repl.Var(e.Name)
	isRanged := e.IsRanged()
	if isRanged && len(e.Subscripts) == 1 {
		// Substring access: str(2:4) → str.Substring(start, end)
		args, err := tg.transformRangeExprToArgs(e.Subscripts[0].(*f90.RangeExpr), vi)
		receiver := tg.astVarExpr(vi)
		return &ast.CallExpr{
			Fun:  &ast.SelectorExpr{X: receiver, Sel: ast.NewIdent("Substring")},
			Args: args,
		}, err
	}
	// Regular element access: arr(i) → arr.At(indices...)
	var args []ast.Expr
	for _, expr := range e.Subscripts {
		arg, err := tg.transformExpression(_tgtInt32, expr)
		if err != nil {
			return nil, err
		}
		args = append(args, arg)
	}
	receiver := tg.astVarExpr(vi)
	return &ast.CallExpr{
		Fun:  &ast.SelectorExpr{X: receiver, Sel: ast.NewIdent("At")},
		Args: args,
	}, nil
}

// astVarExpr returns the AST expression for a variable, handling COMMON block access.
func (tg *ToGo) astVarExpr(vi *varinfo) ast.Expr {
	if vi.common != "" {
		return &ast.SelectorExpr{
			X:   ast.NewIdent(vi.common),
			Sel: ast.NewIdent(vi.Identifier()),
		}
	}
	return ast.NewIdent(vi.Identifier())
}

func (tg *ToGo) transformSetArrayRef(dst []ast.Stmt, fexpr *f90.ArrayRef, rhs ast.Expr) (_ []ast.Stmt, err error) {
	if fexpr.Base != nil {
		return dst, tg.makeErr(fexpr, "chained ArrayRef assignment not yet implemented")
	}
	vitgt := tg.repl.Var(fexpr.Name)
	switch vitgt.decl.Type.Token {
	case f90token.CHARACTER:
		return tg.transformSetCharacterArray(dst, fexpr, rhs)
	}

	// Regular element assignment: arr(i) = v → arr.Set(value, indices...)
	args, err := tg.transformExprSlice(_tgtInt32, []ast.Expr{rhs}, fexpr.Subscripts)
	if err != nil {
		return dst, err
	}
	receiver := tg.astVarExpr(vitgt)
	gstmt := &ast.ExprStmt{
		X: &ast.CallExpr{
			Fun:  &ast.SelectorExpr{X: receiver, Sel: ast.NewIdent("Set")},
			Args: args,
		},
	}
	dst = append(dst, gstmt)
	return dst, nil
}

func (tg *ToGo) transformSetCharacterArray(dst []ast.Stmt, fexpr *f90.ArrayRef, rhs ast.Expr) (_ []ast.Stmt, err error) {
	vi := tg.repl.Var(fexpr.Name)
	dim := vi.decl.Dimension()
	isRanged := fexpr.IsRanged()
	if dim != nil || isRanged && len(fexpr.Subscripts) > 1 || fexpr.Base != nil {
		return dst, tg.makeErrWithPos(fexpr.Position, "unsupported character type attributes for range set")
	}
	args, err := tg.transformRangeExprToArgs(fexpr.Subscripts[0].(*f90.RangeExpr), vi)
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

func (tg *ToGo) transformRangeExprToArgs(rng *f90.RangeExpr, vi *varinfo) (_ []ast.Expr, err error) {
	if vi == _tgtInt32 {
		panic("misuse of varinfo")
	}
	var start, end ast.Expr = _astOne, nil
	if rng.Start != nil {
		start, err = tg.transformExpression(_tgtInt32, rng.Start)
	}
	if err == nil && rng.End != nil {
		end, err = tg.transformExpression(_tgtInt32, rng.End)
		return []ast.Expr{start, end}, err
	}
	// Set default end size.
	if err == nil && vi.decl.Type.Token == f90token.CHARACTER && vi.decl.Charlen() != nil {
		end, err = tg.transformExpression(_tgtInt32, vi.decl.Charlen())
	} else if err == nil && vi.decl.Dimension().CanExpr() {
		end, err = tg.transformExpression(_tgtInt32, vi.decl.Dimension().Expr())
	} else {
		err = tg.makeErrWithPos(rng.Position, "unsupported variable dimension for assignment")
	}
	if err != nil {
		return nil, err
	}
	return []ast.Expr{start, end}, nil
}

func (tg *ToGo) transformExprSlice(vitgt *varinfo, dst []ast.Expr, src []f90.Expression) (_ []ast.Expr, err error) {
	for i := range src {
		expr, err := tg.transformExpression(vitgt, src[i])
		if err != nil {
			return dst, err
		}
		dst = append(dst, expr)
	}
	return dst, nil
}

// wrapConversion wraps expr with a type conversion if targetType differs from sourceType.
func (tg *ToGo) wrapConversion(targetType f90token.Token, sourceType *varinfo, expr ast.Expr) ast.Expr {
	srcType := sourceType.typeToken()
	if srcType == 0 {
		err := tg.makeErrAtStmt("undefined source type for target type: " + targetType.String())
		panic(err.Error())
	}
	if srcType == targetType {
		return expr // No conversion needed
	}
	conv := "invalid"
	switch targetType {
	case f90token.REAL:
		conv = "float32"
	case f90token.DOUBLEPRECISION:
		conv = "float64"
	case f90token.INTEGER:
		conv = "int32"
	case f90token.LOGICAL:
		return expr // bool → bool, no conversion needed
	}
	return &ast.CallExpr{
		Fun:  ast.NewIdent(conv),
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

type intrinsicFn struct {
	name        string
	expr        ast.Expr
	exprGeneric func(tp *varinfo) ast.Expr
	method      string
	returnType  *varinfo
	params      []*varinfo
	isVariadic  bool
}

func (tg *ToGo) intrinsicExpr(vitgt *varinfo, fn *intrinsicFn, args ...f90.Expression) (call *ast.CallExpr, err error) {
	if fn.isVariadic {
		if len(args) < len(fn.params) {
			return nil, fmt.Errorf("intrinsic %s requires at least %d arguments, got %d", fn.name, len(fn.params), len(args))
		}
	} else if len(args) != len(fn.params) {
		return nil, fmt.Errorf("intrinsic %s requires %d arguments, got %d", fn.name, len(fn.params), len(args))
	}
	var gargs []ast.Expr
	for i := range args {
		// For variadic, use the first param type for extra args
		paramIdx := i
		if paramIdx >= len(fn.params) {
			paramIdx = 0
		}
		expr, err := tg.transformExpression(fn.params[paramIdx], args[i])
		if err != nil {
			return nil, err
		}
		gargs = append(gargs, expr)
	}

	if fn.method != "" {
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
		funcExpr := fn.expr
		if funcExpr == nil && fn.exprGeneric != nil {
			funcExpr = fn.exprGeneric(vitgt)
		}
		call = &ast.CallExpr{
			Fun:  funcExpr,
			Args: gargs,
		}
	}
	return call, nil
}

func getIntrinsic(name string, nargs int) *intrinsicFn {
	for i := range intrinsics {
		fn := &intrinsics[i]
		if !strings.EqualFold(name, fn.name) {
			continue
		}
		// Check arg count matches (for non-variadic) or meets minimum (for variadic)
		if fn.isVariadic {
			if nargs < len(fn.params) {
				continue // try next entry
			}
		} else if nargs != len(fn.params) {
			continue // try next entry with same name
		}
		return fn
	}
	return nil
}

// intrinsicSel creates a selector expression for intrinsic.NAME
func intrinsicSel(name string) func(*varinfo) ast.Expr {
	return func(tp *varinfo) ast.Expr {
		return &ast.SelectorExpr{X: _astIntrinsic, Sel: ast.NewIdent(name)}
	}
}

// intrinsicSelGeneric creates a type-parameterized selector: intrinsic.NAME[T]
func intrinsicSelGeneric(name string) func(*varinfo) ast.Expr {
	return func(tp *varinfo) ast.Expr {
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
	default:
		panic("not a basic type")
	}
	return goType
}

// makeIntrinsicFn creates an intrinsic that calls intrinsic.NAME (e.g., SQRT, SIN).
// returnType is nil if return type matches first param.
func makeIntrinsicFn(name string, returnType *varinfo, params ...*varinfo) intrinsicFn {
	return intrinsicFn{
		name:        name,
		exprGeneric: intrinsicSel(name),
		returnType:  returnType,
		params:      params,
	}
}

// makeIntrinsicVariadic creates a variadic intrinsic like MAX, MIN.
func makeIntrinsicVariadic(name string, returnType *varinfo, variadicType *varinfo) intrinsicFn {
	return intrinsicFn{
		name:        name,
		exprGeneric: intrinsicSel(name),
		returnType:  returnType,
		params:      []*varinfo{variadicType},
		isVariadic:  true,
	}
}

// makeIntrinsicFnGeneric creates a generic intrinsic that emits intrinsic.NAME[T].
func makeIntrinsicFnGeneric(name string, returnType *varinfo, params ...*varinfo) intrinsicFn {
	return intrinsicFn{
		name:        name,
		exprGeneric: intrinsicSelGeneric(name),
		returnType:  returnType,
		params:      params,
	}
}

// makeIntrinsicVariadicGeneric creates a variadic generic intrinsic like MAX, MIN.
func makeIntrinsicVariadicGeneric(name string, returnType *varinfo, variadicType *varinfo) intrinsicFn {
	return intrinsicFn{
		name:        name,
		exprGeneric: intrinsicSelGeneric(name),
		returnType:  returnType,
		params:      []*varinfo{variadicType},
		isVariadic:  true,
	}
}

// makeIntrinsicCast creates a type cast intrinsic like REAL, INT, DBLE.
func makeIntrinsicCast(name, goType string, returnType, paramType *varinfo) intrinsicFn {
	return intrinsicFn{
		name:       name,
		expr:       ast.NewIdent(goType),
		returnType: returnType,
		params:     []*varinfo{paramType},
	}
}

// makeIntrinsicMethod creates a method-call intrinsic like LEN, TRIM.
func makeIntrinsicMethod(name, methodName string, returnType, receiverType *varinfo, params ...*varinfo) intrinsicFn {
	allParams := make([]*varinfo, 0, 1+len(params))
	allParams = append(allParams, receiverType)
	allParams = append(allParams, params...)
	return intrinsicFn{
		name:       name,
		method:     methodName,
		returnType: returnType,
		params:     allParams,
	}
}

func (fn *intrinsicFn) inferReturnType(tgt *varinfo) (inferred *varinfo) {
	if fn.returnType != nil {
		inferred = fn.returnType
	} else {
		inferred = fn.params[0]
	}
	var dflt *varinfo
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

var intrinsics = []intrinsicFn{
	// Type conversions
	makeIntrinsicCast("REAL", "float32", _tgtFloat32, _tgtGenericFloat),
	makeIntrinsicCast("DBLE", "float64", _tgtFloat64, _tgtGenericFloat),
	makeIntrinsicCast("INT", "int32", _tgtInt32, _tgtGenericInt),
	makeIntrinsicCast("INT32", "int32", _tgtInt32, _tgtGenericInt),

	// Math intrinsics - single float argument (return type matches input)
	makeIntrinsicFn("SQRT", nil, _tgtGenericFloat),
	makeIntrinsicFn("SIN", nil, _tgtGenericFloat),
	makeIntrinsicFn("COS", nil, _tgtGenericFloat),
	makeIntrinsicFn("TAN", nil, _tgtGenericFloat),
	makeIntrinsicFn("ASIN", nil, _tgtGenericFloat),
	makeIntrinsicFn("ACOS", nil, _tgtGenericFloat),
	makeIntrinsicFn("ATAN", nil, _tgtGenericFloat),
	makeIntrinsicFn("EXP", nil, _tgtGenericFloat),
	makeIntrinsicFn("LOG", nil, _tgtGenericFloat),
	makeIntrinsicFn("LOG10", nil, _tgtGenericFloat),
	makeIntrinsicFn("SINH", nil, _tgtGenericFloat),
	makeIntrinsicFn("COSH", nil, _tgtGenericFloat),
	makeIntrinsicFn("TANH", nil, _tgtGenericFloat),
	makeIntrinsicFn("FLOOR", nil, _tgtGenericFloat),
	makeIntrinsicFn("CEILING", nil, _tgtGenericFloat),
	makeIntrinsicFn("AINT", nil, _tgtGenericFloat),
	makeIntrinsicFn("ANINT", nil, _tgtGenericFloat),
	makeIntrinsicFn("NINT", _tgtInt32, _tgtGenericFloat), // NINT returns INTEGER
	makeIntrinsicFn("POW", nil, _tgtGenericFloat, _tgtGenericFloat),

	// Math intrinsics - two float arguments
	makeIntrinsicFn("ATAN2", nil, _tgtGenericFloat, _tgtGenericFloat),

	// Math intrinsics - signed/numeric
	makeIntrinsicFnGeneric("ABS", nil, _tgtGenericFloat),
	makeIntrinsicFn("SIGN", nil, _tgtGenericFloat, _tgtGenericFloat),
	makeIntrinsicFn("MOD", nil, _tgtGenericInt, _tgtGenericInt),
	makeIntrinsicFn("DIM", nil, _tgtGenericFloat, _tgtGenericFloat),
	makeIntrinsicFn("DPROD", _tgtFloat64, _tgtGenericFloat, _tgtGenericFloat), // DPROD returns DOUBLE

	// Variadic intrinsics
	makeIntrinsicVariadicGeneric("MAX", nil, _tgtGenericFloat),
	makeIntrinsicVariadicGeneric("MIN", nil, _tgtGenericFloat),

	// Character methods
	makeIntrinsicMethod("LEN", "Len", _tgtInt32, _tgtChar),
	makeIntrinsicMethod("LEN_TRIM", "LenTrim", _tgtInt32, _tgtChar),
	makeIntrinsicMethod("TRIM", "Trim", _tgtChar, _tgtChar),
	makeIntrinsicMethod("ADJUSTL", "AdjustL", _tgtChar, _tgtChar),
	makeIntrinsicMethod("ADJUSTR", "AdjustR", _tgtChar, _tgtChar),
	makeIntrinsicMethod("INDEX", "Index", _tgtInt32, _tgtChar, _tgtChar),

	// Array methods - 1 arg versions
	makeIntrinsicMethod("SIZE", "Size", _tgtInt32, _tgtArray),
	makeIntrinsicMethod("SHAPE", "Shape", nil, _tgtArray), // returns array
	// Array methods - 2 arg versions (with dimension)
	makeIntrinsicMethod("SIZE", "SizeDim", _tgtInt32, _tgtArray, _tgtInt32),
	makeIntrinsicMethod("LBOUND", "LowerDim", _tgtInt32, _tgtArray, _tgtInt32),
	makeIntrinsicMethod("UBOUND", "UpperDim", _tgtInt32, _tgtArray, _tgtInt32),

	// Special
	makeIntrinsicFn("MALLOC", nil, _tgtInt32),
}

func defaultVarinfo(tok f90token.Token) *varinfo {
	return &varinfo{
		_varname: fmt.Sprintf("<default %s varinfo>", tok.String()),
		decl:     &f90.DeclEntity{Type: &f90.TypeSpec{Token: tok}},
	}
}

var (
	_astFalse        = ast.NewIdent("false")
	_astTrue         = ast.NewIdent("true")
	_astOne          = &ast.BasicLit{Kind: token.INT, Value: "1"}
	_tgtInt32        = defaultVarinfo(f90token.INTEGER)
	_tgtFloat32      = defaultVarinfo(f90token.REAL)
	_tgtBool         = defaultVarinfo(f90token.LOGICAL)
	_tgtFloat64      = defaultVarinfo(f90token.DOUBLEPRECISION)
	_tgtChar         = defaultVarinfo(f90token.CHARACTER)
	_tgtGenericFloat = defaultVarinfo(f90token.FloatLit)
	_tgtGenericInt   = defaultVarinfo(f90token.IntLit)
	_tgtArray        = defaultVarinfo(f90token.DIMENSION)
)

func isGenericVarinfo(vi *varinfo) bool {
	return vi == _tgtGenericFloat || vi == _tgtGenericInt
}
