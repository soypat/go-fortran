package fortran

import (
	"errors"
	"fmt"
	"slices"
	"strings"

	f90 "github.com/soypat/go-fortran/ast"
)

type REPL struct {
	scope     *ParserUnitData // currentScope variable data.
	_extern   []*ParserUnitData
	_contains []*ParserUnitData
}

func (tg *REPL) Var(name string) *varinfo {
	return tg.scope.Var(name)
}

func (tg *REPL) AddExtern(pu []f90.ProgramUnit) error {
	for i := range pu {
		data, ok := pu[i].UnitData().(*ParserUnitData)
		if !ok {
			return fmt.Errorf("extern program unit %s has incompatible UnitData", pu[i].UnitName())
		}
		exists := tg.Extern(data.name) != nil
		if exists {
			return fmt.Errorf("extern program unit %s with namespace %s already added", pu[i].UnitName(), data.name)
		}
		tg._extern = append(tg._extern, data)
	}
	return nil
}

func (tg *REPL) Extern(name string) *ParserUnitData {
	for i := range tg._extern {
		if strings.EqualFold(tg._extern[i].name, name) {
			return tg._extern[i]
		}
	}
	return nil
}

func (tg *REPL) Contained(name string) *ParserUnitData {
	for i := range tg._contains {
		if strings.EqualFold(tg._contains[i].name, name) {
			return tg._contains[i]
		}
	}
	return nil
}

func (tg *REPL) ContainedOrExtern(name string) *ParserUnitData {
	data := tg.Contained(name)
	if data == nil {
		data = tg.Extern(name)
	}
	return data
}

func (tg *REPL) SetScope(pu f90.ProgramUnit) error {
	data, ok := pu.UnitData().(*ParserUnitData)
	if !ok {
		return errors.New("missing parser unit data")
	}

	tg.scope = data
	tg.scope.vars = slices.Clone(tg.scope.vars)
	for i := range tg.scope.vars {
		tg.scope.vars[i]._varname = sanitizeIdent(tg.scope.vars[i]._varname)
		tg.scope.vars[i].common = sanitizeIdent(tg.scope.vars[i].common)
		tg.scope.vars[i].pointee = sanitizeIdent(tg.scope.vars[i].pointee)
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
	tg._contains = tg._contains[:0]
	for i := range toAdd {
		pu, ok := toAdd[i].UnitData().(*ParserUnitData)
		if !ok {
			return fmt.Errorf("contains program unit %s incompatible unit data", toAdd[i].UnitName())
		}
		exists := tg.Contained(pu.name) != nil
		if exists {
			return fmt.Errorf("contains program unit %s duplicated", toAdd[i].UnitName())
		}
		tg._contains = append(tg._contains, pu)
	}
	return nil
}
