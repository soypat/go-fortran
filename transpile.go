package fortran

import (
	"errors"
	"go/ast"

	f90 "github.com/soypat/go-fortran/ast"
	"github.com/soypat/go-fortran/symbol"
)

type TranspileToGo struct {
}

func (tg *TranspileToGo) Reset(symTable *symbol.Table) error {
	return nil
}

func (tg *TranspileToGo) TransformSubroutine(AST *f90.Subroutine) (*ast.FuncDecl, error) {
	return nil, errors.ErrUnsupported
}
