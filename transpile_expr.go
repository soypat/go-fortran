package fortran

import (
	"fmt"
	"go/ast"
	"go/token"
	"strconv"

	f90 "github.com/soypat/go-fortran/ast"
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
		// result = tg.transformArrayRef(e)
	case *f90.BinaryExpr:
		// result = tg.transformBinaryExpr(e)
	case *f90.UnaryExpr:
		// result = tg.transformUnaryExpr(e)
	case *f90.ParenExpr:
		// Parentheses for grouping - just transform the inner expression
		// Go will preserve the necessary parentheses based on operator precedence
		// result = tg.transformExpression(e.Expr)
	case *f90.FunctionCall:
		// Check if this is actually an array reference (parser ambiguity)

	case *f90.ArrayConstructor:
		// result = tg.transformArrayConstructor(e)
	default:
		err = tg.makeErr(expr, "unsupported expression")
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

var (
	_astFalse = ast.NewIdent("false")
	_astTrue  = ast.NewIdent("true")
)
