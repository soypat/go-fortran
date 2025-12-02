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
		result, err = tg.transformBinaryExpr(vitgt, e)
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
	vi := tg.scope.Var(e.Value)
	if vi == nil {
		err = tg.makeErr(e, "identifier not found")
	}
	result = ast.NewIdent(vi.Identifier()) // TODO: transform identifier to target type if necessary.
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
		return &ast.CallExpr{
			Fun: &ast.SelectorExpr{
				X:   ast.NewIdent("math"),
				Sel: ast.NewIdent("Pow"),
			},
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
	case f90token.OR:
		op = token.LOR
	case f90token.StringConcat:
		// String concatenation: a // b → a + b
		op = token.ADD
	default:
		return nil, tg.makeErr(e, fmt.Sprintf("unsupported binary operator %v", e.Op))
	}

	return &ast.BinaryExpr{
		X:  left,
		Op: op,
		Y:  right,
	}, nil
}

func (tg *ToGo) transformFunctionCall(vitgt *varinfo, e *f90.FunctionCall) (result ast.Expr, err error) {
	vi := tg.scope.Var(e.Name)
	if vi != nil {
		// Check if this is a declared variable (COMMON block array access)
		args, err := tg.transformExprSlice(_tgtInt, nil, e.Args)
		// Treat as array element access: arr.At(indices...)
		return tg.astMethodCall(e.Name, "At", args...), err
	}
	fi := tg.ContainedOrExtern(e.Name)
	if fi == nil {
		for i := range intrinsics {
			if strings.EqualFold(e.Name, intrinsics[i].name) {
				expr, err := tg.intrinsicExpr(vitgt, &intrinsics[i], e.Args...)
				if err != nil {
					return nil, err
				}
				return expr, nil
			}
		}
		return result, tg.makeErr(e, "function/intrinsic not found: "+e.Name)
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
	isRanged := e.IsRanged()
	if isRanged && len(e.Subscripts) == 1 {
		// Substring access: str(2:4) → str.Substring(start, end)
		args, err := tg.transformRangeExprToArgs(e.Subscripts[0].(*f90.RangeExpr), tg.scope.Var(e.Name))
		return tg.astMethodCall(e.Name, "Substring", args...), err
	}
	vi := tg.scope.Var(e.Name)
	// Regular element access: arr(i) → arr.At(indices...)
	var args []ast.Expr
	for _, expr := range e.Subscripts {
		arg, err := tg.transformExpression(_tgtInt, expr)
		if err != nil {
			return nil, err
		}
		args = append(args, arg)
	}
	return tg.astMethodCall(vi.Identifier(), "At", args...), nil
}

func (tg *ToGo) transformSetArrayRef(dst []ast.Stmt, fexpr *f90.ArrayRef, rhs ast.Expr) (_ []ast.Stmt, err error) {

	if fexpr.Base != nil {
		return dst, tg.makeErr(fexpr, "chained ArrayRef assignment not yet implemented")
	}
	vitgt := tg.scope.Var(fexpr.Name)
	switch vitgt.decl.Type.Token {
	case f90token.CHARACTER:
		return tg.transformSetCharacterArray(dst, fexpr, rhs)
	}

	// Regular element assignment: arr(i) = v → arr.Set(value, indices...)
	args, err := tg.transformExprSlice(_tgtInt, []ast.Expr{rhs}, fexpr.Subscripts)
	if err != nil {
		return dst, err
	}
	gstmt := &ast.ExprStmt{
		X: tg.astMethodCall(vitgt.Identifier(), "Set", args...),
	}
	dst = append(dst, gstmt)
	return dst, nil
}

func (tg *ToGo) transformSetCharacterArray(dst []ast.Stmt, fexpr *f90.ArrayRef, rhs ast.Expr) (_ []ast.Stmt, err error) {
	vi := tg.scope.Var(fexpr.Name)
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
	gstmt := &ast.ExprStmt{
		X: tg.astMethodCall(vi.Identifier(), "SetSubstring", args...),
	}
	dst = append(dst, gstmt)
	return dst, nil
}

func (tg *ToGo) transformRangeExprToArgs(rng *f90.RangeExpr, vi *varinfo) (_ []ast.Expr, err error) {
	if vi == _tgtInt {
		panic("misuse of varinfo")
	}
	var start, end ast.Expr = _astOne, nil
	if rng.Start != nil {
		start, err = tg.transformExpression(_tgtInt, rng.Start)
	}
	if err == nil && rng.End != nil {
		end, err = tg.transformExpression(_tgtInt, rng.End)
		return []ast.Expr{start, end}, err
	}
	// Set default end size.
	if err == nil && vi.decl.Type.Token == f90token.CHARACTER && vi.decl.Charlen() != nil {
		end, err = tg.transformExpression(_tgtInt, vi.decl.Charlen())
	} else if err == nil && vi.decl.Dimension().CanExpr() {
		end, err = tg.transformExpression(_tgtInt, vi.decl.Dimension().Expr())
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

type intrinsicFn struct {
	name        string
	expr        ast.Expr
	exprGeneric func(tp *varinfo) ast.Expr
	method      string
	params      []*varinfo
}

func (tg *ToGo) intrinsicExpr(vitgt *varinfo, fn *intrinsicFn, args ...f90.Expression) (call *ast.CallExpr, err error) {
	if len(args) != len(fn.params) {
		return nil, fmt.Errorf("intrinsic %s requires %d arguments, got %d", fn.name, len(fn.params), len(args))
	}
	var gargs []ast.Expr
	for i := range args {
		expr, err := tg.transformExpression(fn.params[i], args[i])
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
	} else {
		funcExpr := fn.expr
		if funcExpr == nil && fn.expr != nil {
			funcExpr = fn.exprGeneric(vitgt)
		}
		call = &ast.CallExpr{
			Fun:  funcExpr,
			Args: gargs,
		}
	}
	return call, nil
}

var intrinsics = []intrinsicFn{
	{
		name: "REAL",
		expr: ast.NewIdent("float32"),
		params: []*varinfo{
			_tgtGenericFloat,
		},
	},
	{
		name: "SQRT",
		exprGeneric: func(tp *varinfo) ast.Expr {
			return &ast.SelectorExpr{
				X:   _astIntrinsic,
				Sel: ast.NewIdent("SQRT"),
			}
		},
		params: []*varinfo{
			_tgtGenericFloat,
		},
	},
	{
		name:   "LEN_TRIM",
		method: "LenTrim",
		params: []*varinfo{
			_tgtChar,
		},
	},
}

var (
	_astFalse = ast.NewIdent("false")
	_astTrue  = ast.NewIdent("true")
	_astOne   = &ast.BasicLit{Kind: token.INT, Value: "1"}
	_tgtInt   = &varinfo{
		_varname: "<default int varinfo>",
		decl:     &f90.DeclEntity{Type: &f90.TypeSpec{Token: f90token.INTEGER}},
	}
	_tgtChar = &varinfo{
		_varname: "<default character>",
		decl:     &f90.DeclEntity{Type: &f90.TypeSpec{Token: f90token.CHARACTER}},
	}
	_tgtGenericFloat = &varinfo{
		_varname: "<default character>",
		decl:     &f90.DeclEntity{Type: &f90.TypeSpec{Token: f90token.FloatLit}},
	}
	_tgtGenericInt = &varinfo{
		_varname: "<default character>",
		decl:     &f90.DeclEntity{Type: &f90.TypeSpec{Token: f90token.IntLit}},
	}
)
