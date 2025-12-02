package fortran

import (
	"fmt"
	"go/ast"
	"go/token"
	"strconv"

	f90 "github.com/soypat/go-fortran/ast"
	f90token "github.com/soypat/go-fortran/token"
)

// transformExpression transforms a single Fortran expression to a Go expression
func (tg *ToGo) transformExpression(targetType *f90.DeclEntity, expr f90.Expression) (result ast.Expr, err error) {
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
		result, err = tg.transformExprIdentifer(e)
	case *f90.ArrayRef:
		result, err = tg.transformArrayRef(e)
	case *f90.BinaryExpr:
		result, err = tg.transformBinaryExpr(e)
	case *f90.UnaryExpr:
		result, err = tg.transformUnaryExpr(e)
	case *f90.ParenExpr:
		// Parentheses for grouping - transform the inner expression and wrap in parens
		inner, err := tg.transformExpression(targetType, e.Expr)
		if err != nil {
			return nil, err
		}
		result = &ast.ParenExpr{X: inner}
	case *f90.FunctionCall:
		// FunctionCall in expression: could be actual function or COMMON block array access
		result, err = tg.transformFunctionCall(e)

	case *f90.ArrayConstructor:
		result, err = tg.transformArrayConstructor(e)
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

func (tg *ToGo) transformExprIdentifer(e *f90.Identifier) (result ast.Expr, err error) {
	vi := tg.scope.Var(e.Value)
	if vi == nil {
		err = tg.makeErr(e, "identifier not found")
	}
	result = ast.NewIdent(e.Value) // TODO: transform identifier to target type if necessary.
	return result, err
}

func (tg *ToGo) transformArrayConstructor(e *f90.ArrayConstructor) (result ast.Expr, err error) {
	// Transform (/ 1, 2, 3 /) → []T{1, 2, 3}
	var elts []ast.Expr
	for _, val := range e.Values {
		elt, err := tg.transformExpression(nil, val)
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

func (tg *ToGo) transformUnaryExpr(e *f90.UnaryExpr) (result ast.Expr, err error) {
	operand, err := tg.transformExpression(nil, e.Operand)
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

func (tg *ToGo) transformBinaryExpr(e *f90.BinaryExpr) (result ast.Expr, err error) {
	left, err := tg.transformExpression(nil, e.Left)
	if err != nil {
		return nil, err
	}
	right, err := tg.transformExpression(nil, e.Right)
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

func (tg *ToGo) transformFunctionCall(e *f90.FunctionCall) (result ast.Expr, err error) {
	// Transform args
	var args []ast.Expr
	for _, arg := range e.Args {
		argExpr, err := tg.transformExpression(nil, arg)
		if err != nil {
			return nil, err
		}
		args = append(args, argExpr)
	}

	// Check if this is a declared variable (COMMON block array access)
	vi := tg.scope.Var(e.Name)
	if vi != nil {
		// Treat as array element access: arr.At(indices...)
		return tg.astMethodCall(e.Name, "At", args...), nil
	}

	// Regular function call
	return &ast.CallExpr{
		Fun:  ast.NewIdent(sanitizeIdent(e.Name)),
		Args: args,
	}, nil
}

func (tg *ToGo) transformArrayRef(e *f90.ArrayRef) (result ast.Expr, err error) {
	if e.Base != nil {
		return nil, tg.makeErr(e, "chained ArrayRef expression not yet implemented")
	}

	// Check if any subscript is a RangeExpr (substring/slice access)
	var hasRange bool
	for _, expr := range e.Subscripts {
		if _, ok := expr.(*f90.RangeExpr); ok {
			hasRange = true
			break
		}
	}

	if hasRange && len(e.Subscripts) == 1 {
		// Substring access: str(2:4) → str.Substring(start, end)
		rng := e.Subscripts[0].(*f90.RangeExpr)
		var args []ast.Expr
		if rng.Start != nil {
			start, err := tg.transformExpression(_astTgtInt, rng.Start)
			if err != nil {
				return nil, err
			}
			args = append(args, start)
		} else {
			args = append(args, &ast.BasicLit{Kind: token.INT, Value: "1"})
		}
		if rng.End != nil {
			end, err := tg.transformExpression(_astTgtInt, rng.End)
			if err != nil {
				return nil, err
			}
			args = append(args, end)
		}
		return tg.astMethodCall(e.Name, "Substring", args...), nil
	}

	// Regular element access: arr(i) → arr.At(indices...)
	var args []ast.Expr
	for _, expr := range e.Subscripts {
		arg, err := tg.transformExpression(_astTgtInt, expr)
		if err != nil {
			return nil, err
		}
		args = append(args, arg)
	}
	return tg.astMethodCall(e.Name, "At", args...), nil
}

var (
	_astFalse = ast.NewIdent("false")
	_astTrue  = ast.NewIdent("true")
)
