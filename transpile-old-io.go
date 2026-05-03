package fortran

import (
	"go/ast"
	"go/token"
	"strconv"
	"strings"

	f90 "github.com/soypat/go-fortran/ast"
)

// This file stores first iteration of IO routine transformation.
// Still needed for proper operation but should be replaced soon.

func (tg *ToGo) transformWriteStmtOld(dst []ast.Stmt, stmt *f90.WriteStmt) (_ []ast.Stmt, err error) {
	// Determine format expression
	var formatExpr ast.Expr
	switch format := stmt.Format.(type) {
	case *f90.Identifier:
		if format.Value == "*" {
			// List-directed output
			formatExpr = &ast.CallExpr{Fun: _astFortioDefaultFormat}
		} else if nml := tg.repl.Namelist(format.Value); nml != nil {
			// Namelist-directed output
			return tg.transformWriteNamelist(dst, stmt, nml)
		} else if vi := tg.repl.Var(format.Value); vi != nil && vi.IsChar() {
			varExpr, _, err := tg.transformExpression(_tgtChar, format)
			if err != nil {
				return dst, err
			}
			formatExpr = &ast.CallExpr{
				Fun: _astFortioNewFormat,
				Args: []ast.Expr{
					&ast.CallExpr{Fun: &ast.SelectorExpr{X: varExpr, Sel: ast.NewIdent("String")}},
				},
			}
		} else {
			return dst, tg.makeErr(stmt, "unknown write format "+format.Value)
		}
	case *f90.IntegerLiteral:
		// Format label reference - look up FORMAT statement
		label := strconv.FormatInt(format.Value, 10)
		fmtInfo := tg.repl.getFormat(label)
		if fmtInfo != nil {
			// Generate: fortio.NewFormat(parsed components...)
			formatArgs := formatSpecsToGoAST(fmtInfo.Specs)
			formatExpr = &ast.CallExpr{
				Fun:  _astFortioNewFormat,
				Args: formatArgs,
			}
		}
	case *f90.StringLiteral:
		// Inline format string: WRITE(*,'(I5)') x
		specs := f90.ParseFormatString(format.Value)
		formatArgs := formatSpecsToGoAST(specs)
		formatExpr = &ast.CallExpr{
			Fun:  _astFortioNewFormat,
			Args: formatArgs,
		}
	}
	if formatExpr == nil {
		// Unsupported format type, skip
		return dst, nil
	}

	// Check if any output list item is an implied DO loop
	hasImpliedDoLoop := false
	for _, expr := range stmt.OutputList {
		if _, ok := expr.(*f90.ImpliedDoLoop); ok {
			hasImpliedDoLoop = true
			break
		}
	}

	if hasImpliedDoLoop {
		// Generate code that builds args slice at runtime
		return tg.transformWriteStmtWithImpliedDoLoop(dst, stmt, formatExpr)
	}

	// Simple case: no implied DO loops
	unitExpr, err := tg.transformIOUnitExpr(stmt.Unit)
	if err != nil {
		return dst, err
	}

	// Check for I/O specifiers
	var iostatVar ast.Expr
	if iostatExpr := findIOSpecifier(stmt.Specifiers, "IOSTAT"); iostatExpr != nil {
		iostatVar, _, err = tg.transformExpression(_tgtInt32, iostatExpr)
		if err != nil {
			return dst, err
		}
	}

	// Build output arguments
	outputArgs := make([]ast.Expr, 0, len(stmt.OutputList))
	for _, expr := range stmt.OutputList {
		var exprType Varinfo
		if err := tg.repl.InferType(&exprType, expr); err != nil {
			return dst, err
		}
		goExpr, _, err := tg.transformExpression(&exprType, expr)
		if err != nil {
			return dst, err
		}
		outputArgs = append(outputArgs, goExpr)
	}

	// Generate call based on whether specifiers are present
	if iostatVar != nil {
		// Build IOSpec with IOSTAT pointer
		specFields := []ast.Expr{
			&ast.KeyValueExpr{Key: ast.NewIdent("UNIT"), Value: unitExpr},
			&ast.KeyValueExpr{Key: ast.NewIdent("IOSTAT"), Value: &ast.UnaryExpr{Op: token.AND, X: iostatVar}},
		}
		ioSpec := &ast.CompositeLit{Type: _astFortioIOSpec, Elts: specFields}

		// Generate: iostatVar = int32(fenv.WriteWithSpec(fortio.IOSpec{...}, format, args...))
		args := append([]ast.Expr{ioSpec, formatExpr}, outputArgs...)
		writeCall := &ast.CallExpr{Fun: _astFenvWriteWithSpec, Args: args}
		dst = append(dst, &ast.AssignStmt{
			Lhs: []ast.Expr{iostatVar},
			Tok: token.ASSIGN,
			Rhs: []ast.Expr{&ast.CallExpr{Fun: ast.NewIdent("int32"), Args: []ast.Expr{writeCall}}},
		})
	} else {
		// Simple case: fenv.Write(unit, format, args...)
		args := append([]ast.Expr{unitExpr, formatExpr}, outputArgs...)
		writeCall := &ast.CallExpr{Fun: _astFenvWrite, Args: args}
		dst = append(dst, &ast.ExprStmt{X: writeCall})
	}
	return dst, nil
}

// transformWriteStmtWithImpliedDoLoop handles WRITE statements containing implied DO loops.
// Generates: { writeArgs := make([]any, 0); ...; fenv.Write(unit, format, writeArgs...) }
func (tg *ToGo) transformWriteStmtWithImpliedDoLoop(dst []ast.Stmt, stmt *f90.WriteStmt, formatExpr ast.Expr) (_ []ast.Stmt, err error) {
	writeArgsVar := ast.NewIdent("writeArgs")

	// writeArgs := make([]any, 0)
	initStmt := &ast.AssignStmt{
		Lhs: []ast.Expr{writeArgsVar},
		Tok: token.DEFINE,
		Rhs: []ast.Expr{
			&ast.CallExpr{
				Fun: ast.NewIdent("make"),
				Args: []ast.Expr{
					&ast.ArrayType{Elt: ast.NewIdent("any")},
					_astZero,
				},
			},
		},
	}

	blockStmts := []ast.Stmt{initStmt}

	// Process each output list item
	for _, expr := range stmt.OutputList {
		if idl, ok := expr.(*f90.ImpliedDoLoop); ok {
			// Generate for loop that appends values
			loopStmts, err := tg.transformImpliedDoLoopAppend(idl, writeArgsVar)
			if err != nil {
				return dst, err
			}
			blockStmts = append(blockStmts, loopStmts...)
		} else {
			// Regular expression: writeArgs = append(writeArgs, expr)
			var exprType Varinfo
			if err := tg.repl.InferType(&exprType, expr); err != nil {
				return dst, err
			}
			goExpr, _, err := tg.transformExpression(&exprType, expr)
			if err != nil {
				return dst, err
			}
			appendStmt := &ast.AssignStmt{
				Lhs: []ast.Expr{writeArgsVar},
				Tok: token.ASSIGN,
				Rhs: []ast.Expr{
					&ast.CallExpr{
						Fun:  ast.NewIdent("append"),
						Args: []ast.Expr{writeArgsVar, goExpr},
					},
				},
			}
			blockStmts = append(blockStmts, appendStmt)
		}
	}

	// fenv.Write(unit, format, writeArgs...)
	unitExpr, err := tg.transformIOUnitExpr(stmt.Unit)
	if err != nil {
		return dst, err
	}
	writeCall := &ast.CallExpr{
		Fun: _astFenvWrite,
		Args: []ast.Expr{
			unitExpr,
			formatExpr,
			writeArgsVar,
		},
		Ellipsis: 1, // Enable ... expansion
	}
	blockStmts = append(blockStmts, &ast.ExprStmt{X: writeCall})

	// Wrap in a block to scope writeArgs
	dst = append(dst, &ast.BlockStmt{List: blockStmts})
	return dst, nil
}

// transformIOUnitExpr generates a unit number expression for IO operations.
// Returns int32 literal 6 for stdout (*), or the evaluated unit expression.
func (tg *ToGo) transformIOUnitExpr(unit f90.Expression) (ast.Expr, error) {
	if unit == nil {
		// Default to stdout (unit 6)
		return &ast.BasicLit{Kind: token.INT, Value: "6"}, nil
	}
	if ident, ok := unit.(*f90.Identifier); ok && ident.Value == "*" {
		// * means stdout (unit 6)
		return &ast.BasicLit{Kind: token.INT, Value: "6"}, nil
	}
	// File unit - just return the unit number expression
	unitExpr, _, err := tg.transformExpression(_tgtInt32, unit)
	if err != nil {
		return nil, err
	}
	return unitExpr, nil
}

// fortioStatusFromString converts a Fortran STATUS string to a fortio enum AST.
func fortioStatusFromString(s string) ast.Expr {
	switch strings.ToUpper(s) {
	case "OLD":
		return _astFortioStatusOLD
	case "NEW":
		return _astFortioStatusNEW
	case "REPLACE":
		return _astFortioStatusREPLACE
	case "SCRATCH":
		return _astFortioStatusSCRATCH
	default:
		return _astFortioStatusUNKNOWN
	}
}

// fortioActionFromString converts a Fortran ACTION string to a fortio enum AST.
func fortioActionFromString(s string) ast.Expr {
	switch strings.ToUpper(s) {
	case "READ":
		return _astFortioActionREAD
	case "WRITE":
		return _astFortioActionWRITE
	default:
		return _astFortioActionREADWRITE
	}
}

// transformReadStmtOld generates code for READ statements.
// READ(unit, fmt) vars => fenv.Read(unit, format, &var1, &var2, ...)
func (tg *ToGo) transformReadStmtOld(dst []ast.Stmt, stmt *f90.ReadStmt) (_ []ast.Stmt, err error) {
	// Determine unit expression
	var unitArg ast.Expr
	if stmt.Unit != nil {
		if ident, ok := stmt.Unit.(*f90.Identifier); ok && ident.Value == "*" {
			// stdin (unit 5)
			unitArg = &ast.BasicLit{Kind: token.INT, Value: "5"}
		} else {
			unitArg, _, err = tg.transformExpression(_tgtInt32, stmt.Unit)
			if err != nil {
				return dst, err
			}
		}
	} else {
		// Default to stdin (unit 5)
		unitArg = &ast.BasicLit{Kind: token.INT, Value: "5"}
	}

	// Determine format expression
	var formatExpr ast.Expr
	switch format := stmt.Format.(type) {
	case *f90.Identifier:
		if format.Value == "*" {
			// List-directed input
			formatExpr = &ast.CallExpr{Fun: _astFortioDefaultFormat}
		} else if nml := tg.repl.Namelist(format.Value); nml != nil {
			// Namelist-directed input
			return tg.transformReadNamelist(dst, stmt, nml)
		} else if vi := tg.repl.Var(format.Value); vi != nil && vi.IsChar() {
			varExpr, _, err := tg.transformExpression(_tgtChar, format)
			if err != nil {
				return dst, err
			}
			formatExpr = &ast.CallExpr{
				Fun: _astFortioNewFormat,
				Args: []ast.Expr{
					&ast.CallExpr{Fun: &ast.SelectorExpr{X: varExpr, Sel: ast.NewIdent("String")}},
				},
			}
		} else {
			return dst, tg.makeErr(stmt, "unknown read format "+format.Value)
		}
	case *f90.IntegerLiteral:
		// Format label reference
		label := strconv.FormatInt(format.Value, 10)
		fmtInfo := tg.repl.getFormat(label)
		if fmtInfo != nil {
			formatArgs := formatSpecsToGoAST(fmtInfo.Specs)
			formatExpr = &ast.CallExpr{
				Fun:  _astFortioNewFormat,
				Args: formatArgs,
			}
		} else {
			formatExpr = &ast.CallExpr{Fun: _astFortioDefaultFormat}
		}
	case *f90.StringLiteral:
		// Inline format string
		specs := f90.ParseFormatString(format.Value)
		formatArgs := formatSpecsToGoAST(specs)
		formatExpr = &ast.CallExpr{
			Fun:  _astFortioNewFormat,
			Args: formatArgs,
		}
	default:
		formatExpr = &ast.CallExpr{Fun: _astFortioDefaultFormat}
	}

	// Check for I/O specifiers
	var iostatVar ast.Expr
	if iostatExpr := findIOSpecifier(stmt.Specifiers, "IOSTAT"); iostatExpr != nil {
		iostatVar, _, err = tg.transformExpression(_tgtInt32, iostatExpr)
		if err != nil {
			return dst, err
		}
	}

	// Build input arguments: &var1, &var2, ...
	inputArgs := make([]ast.Expr, 0, len(stmt.InputList))
	for _, expr := range stmt.InputList {
		var exprType Varinfo
		if err := tg.repl.InferType(&exprType, expr); err != nil {
			return dst, err
		}
		goExpr, _, err := tg.transformExpression(&exprType, expr)
		if err != nil {
			return dst, err
		}
		// Take address of variable for READ
		inputArgs = append(inputArgs, &ast.UnaryExpr{Op: token.AND, X: goExpr})
	}

	// Generate call based on whether specifiers are present
	if iostatVar != nil {
		// Build IOSpec with IOSTAT pointer
		specFields := []ast.Expr{
			&ast.KeyValueExpr{Key: ast.NewIdent("UNIT"), Value: unitArg},
			&ast.KeyValueExpr{Key: ast.NewIdent("IOSTAT"), Value: &ast.UnaryExpr{Op: token.AND, X: iostatVar}},
		}
		ioSpec := &ast.CompositeLit{Type: _astFortioIOSpec, Elts: specFields}

		// Generate: iostatVar = int32(fenv.ReadWithSpec(fortio.IOSpec{...}, format, &var1, ...))
		args := append([]ast.Expr{ioSpec, formatExpr}, inputArgs...)
		readCall := &ast.CallExpr{Fun: _astFenvReadWithSpec, Args: args}
		dst = append(dst, &ast.AssignStmt{
			Lhs: []ast.Expr{iostatVar},
			Tok: token.ASSIGN,
			Rhs: []ast.Expr{&ast.CallExpr{Fun: ast.NewIdent("int32"), Args: []ast.Expr{readCall}}},
		})
	} else {
		// Simple case: fenv.Read(unit, format, &var1, &var2, ...)
		args := append([]ast.Expr{unitArg, formatExpr}, inputArgs...)
		readCall := &ast.CallExpr{Fun: _astFenvRead, Args: args}
		dst = append(dst, &ast.ExprStmt{X: readCall})
	}

	return dst, nil
}

// transformReadNamelist generates code for namelist-directed READ.
// READ(unit, NML) => fenv.ReadNamelist(unit, "NML", []fortio.NamelistVar{{Name: "A", Ptr: &a}, ...})
func (tg *ToGo) transformReadNamelist(dst []ast.Stmt, stmt *f90.ReadStmt, nml *f90.NamelistGroup) ([]ast.Stmt, error) {
	// Get unit expression
	var unitArg ast.Expr
	if stmt.Unit != nil {
		if ident, ok := stmt.Unit.(*f90.Identifier); ok && ident.Value == "*" {
			unitArg = &ast.BasicLit{Kind: token.INT, Value: "5"}
		} else {
			var err error
			unitArg, _, err = tg.transformExpression(_tgtInt32, stmt.Unit)
			if err != nil {
				return dst, err
			}
		}
	} else {
		unitArg = &ast.BasicLit{Kind: token.INT, Value: "5"}
	}

	// Build []fortio.NamelistVar slice literal
	var elts []ast.Expr
	for _, varName := range nml.Variables {
		vi := tg.repl.Var(varName)
		if vi == nil {
			return dst, tg.makeErr(stmt, "undefined variable in namelist: "+varName)
		}
		goName := tg.astIdent(varName)
		elts = append(elts, &ast.CompositeLit{
			Elts: []ast.Expr{
				&ast.KeyValueExpr{Key: ast.NewIdent("Name"), Value: &ast.BasicLit{Kind: token.STRING, Value: `"` + varName + `"`}},
				&ast.KeyValueExpr{Key: ast.NewIdent("Ptr"), Value: &ast.UnaryExpr{Op: token.AND, X: goName}},
			},
		})
	}

	varsSlice := &ast.CompositeLit{
		Type: &ast.ArrayType{Elt: _astFortioNamelistVar},
		Elts: elts,
	}

	// fenv.ReadNamelist(unit, "GROUPNAME", vars)
	readCall := &ast.CallExpr{
		Fun: _astFenvReadNamelist,
		Args: []ast.Expr{
			unitArg,
			&ast.BasicLit{Kind: token.STRING, Value: `"` + nml.Name + `"`},
			varsSlice,
		},
	}
	dst = append(dst, &ast.ExprStmt{X: readCall})
	return dst, nil
}

// transformWriteNamelist generates code for namelist-directed WRITE.
// WRITE(unit, NML) => fenv.WriteNamelist(unit, "NML", []fortio.NamelistVar{{Name: "A", Ptr: &a}, ...})
func (tg *ToGo) transformWriteNamelist(dst []ast.Stmt, stmt *f90.WriteStmt, nml *f90.NamelistGroup) ([]ast.Stmt, error) {
	// Get unit expression
	unitArg, err := tg.transformIOUnitExpr(stmt.Unit)
	if err != nil {
		return dst, err
	}

	// Build []fortio.NamelistVar slice literal
	var elts []ast.Expr
	for _, varName := range nml.Variables {
		vi := tg.repl.Var(varName)
		if vi == nil {
			return dst, tg.makeErr(stmt, "undefined variable in namelist: "+varName)
		}
		goName := tg.astIdent(varName)
		elts = append(elts, &ast.CompositeLit{
			Elts: []ast.Expr{
				&ast.KeyValueExpr{Key: ast.NewIdent("Name"), Value: &ast.BasicLit{Kind: token.STRING, Value: `"` + varName + `"`}},
				&ast.KeyValueExpr{Key: ast.NewIdent("Ptr"), Value: &ast.UnaryExpr{Op: token.AND, X: goName}},
			},
		})
	}

	varsSlice := &ast.CompositeLit{
		Type: &ast.ArrayType{Elt: _astFortioNamelistVar},
		Elts: elts,
	}

	// fenv.WriteNamelist(unit, "GROUPNAME", vars)
	writeCall := &ast.CallExpr{
		Fun: _astFenvWriteNamelist,
		Args: []ast.Expr{
			unitArg,
			&ast.BasicLit{Kind: token.STRING, Value: `"` + nml.Name + `"`},
			varsSlice,
		},
	}
	dst = append(dst, &ast.ExprStmt{X: writeCall})
	return dst, nil
}

// transformImpliedDoLoopAppend generates statements that append values from an implied DO loop to argsVar.
// For (expr, i=start,end,stride) generates:
//
//	for i := start; i <= end; i += stride { argsVar = append(argsVar, expr) }
func (tg *ToGo) transformImpliedDoLoopAppend(idl *f90.ImpliedDoLoop, argsVar *ast.Ident) ([]ast.Stmt, error) {
	loopVar := ast.NewIdent(idl.LoopVar)

	// Register the loop variable temporarily so it can be resolved in expressions
	popLoopVar := tg.repl.PushVar(Varinfo{
		decl:     _tgtInt.decl,
		_varname: idl.LoopVar,
	})
	defer popLoopVar()

	// Transform start expression
	startExpr, _, err := tg.transformExpression(_tgtInt, idl.Start)
	if err != nil {
		return nil, err
	}

	// Transform end expression
	endExpr, _, err := tg.transformExpression(_tgtInt, idl.End)
	if err != nil {
		return nil, err
	}

	// Transform stride (default to 1 if not specified)
	var strideExpr ast.Expr = _astOne
	if idl.Stride != nil {
		strideExpr, _, err = tg.transformExpression(_tgtInt, idl.Stride)
		if err != nil {
			return nil, err
		}
	}

	// Build the body: append each expression in the implied DO loop
	var bodyStmts []ast.Stmt
	for _, expr := range idl.Expressions {
		// Handle nested implied DO loops
		if nestedIdl, ok := expr.(*f90.ImpliedDoLoop); ok {
			nestedStmts, err := tg.transformImpliedDoLoopAppend(nestedIdl, argsVar)
			if err != nil {
				return nil, err
			}
			bodyStmts = append(bodyStmts, nestedStmts...)
		} else {
			var exprType Varinfo
			if err := tg.repl.InferType(&exprType, expr); err != nil {
				return nil, err
			}
			goExpr, _, err := tg.transformExpression(&exprType, expr)
			if err != nil {
				return nil, err
			}
			appendStmt := &ast.AssignStmt{
				Lhs: []ast.Expr{argsVar},
				Tok: token.ASSIGN,
				Rhs: []ast.Expr{
					&ast.CallExpr{
						Fun:  ast.NewIdent("append"),
						Args: []ast.Expr{argsVar, goExpr},
					},
				},
			}
			bodyStmts = append(bodyStmts, appendStmt)
		}
	}

	// for loopVar := start; loopVar <= end; loopVar += stride { ... }
	forStmt := &ast.ForStmt{
		Init: &ast.AssignStmt{
			Lhs: []ast.Expr{loopVar},
			Tok: token.DEFINE,
			Rhs: []ast.Expr{startExpr},
		},
		Cond: &ast.BinaryExpr{
			X:  loopVar,
			Op: token.LEQ,
			Y:  endExpr,
		},
		Post: &ast.AssignStmt{
			Lhs: []ast.Expr{loopVar},
			Tok: token.ADD_ASSIGN,
			Rhs: []ast.Expr{strideExpr},
		},
		Body: &ast.BlockStmt{List: bodyStmts},
	}

	return []ast.Stmt{forStmt}, nil
}
