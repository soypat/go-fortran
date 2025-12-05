package fortran

import (
	"fmt"
	"go/ast"
	"go/token"
	"io"
	"strconv"
	"strings"

	f90 "github.com/soypat/go-fortran/ast"
	f90token "github.com/soypat/go-fortran/token"
)

type ToGo struct {
	repl        REPL
	source      string
	sourceFile  io.ReaderAt
	currentStmt f90.Statement
}

func (tg *ToGo) SetSource(source string, r io.ReaderAt) {
	tg.source = source
	tg.sourceFile = r
}

func (tg *ToGo) AddExtern(pu []f90.ProgramUnit) error {
	return tg.repl.AddExtern(pu)
}

func (tg *ToGo) Extern(name string) *ParserUnitData {
	return tg.repl.Extern(name)
}

func (tg *ToGo) Contained(name string) *ParserUnitData {
	return tg.repl.Contained(name)
}

func (tg *ToGo) ContainedOrExtern(name string) *ParserUnitData {
	return tg.repl.ContainedOrExtern(name)
}

func (tg *ToGo) TransformProgram(prog *f90.ProgramBlock) ([]ast.Decl, error) {
	err := tg.repl.SetScope(prog)
	if err != nil {
		return nil, err
	}

	mainBody, err := tg.transformStatements(nil, prog.Body)
	// Create main function declaration
	mainFunc := &ast.FuncDecl{
		Name: ast.NewIdent("main"),
		Type: &ast.FuncType{
			Params: &ast.FieldList{}, // No parameters for main
		},
		Body: &ast.BlockStmt{
			List: mainBody,
		},
	}

	// Start with import and main function
	decls := []ast.Decl{
		&ast.GenDecl{
			Tok: token.IMPORT,
			Specs: []ast.Spec{&ast.ImportSpec{
				Path: &ast.BasicLit{Value: fmt.Sprintf("%q", "github.com/soypat/go-fortran/intrinsic")},
			}},
		},
		mainFunc,
	}
	if err != nil {
		return decls, err
	}
	// Append COMMON block declarations at start.

	// Transform contained procedures (CONTAINS section)
	for _, contained := range prog.Contains {
		switch c := contained.(type) {
		case *f90.Subroutine:
			funcDecl, err := tg.TransformSubroutine(c)
			if err != nil {
				return nil, err
			}
			if funcDecl != nil {
				decls = append(decls, funcDecl)
			}
		case *f90.Function:
			funcDecl, err := tg.TransformFunction(c)
			if err != nil {
				return nil, err
			}
			if funcDecl != nil {
				decls = append(decls, funcDecl)
			}
		default:
			return decls, fmt.Errorf("unknown CONTAINS declaration: %T", c)
		}
	}
	decls = tg.AppendCommonDecls(decls)
	return decls, nil
}

func (tg *ToGo) astIdent(name string) *ast.Ident {
	return ast.NewIdent(name)
}

// TransformSubroutine transforms a Fortran SUBROUTINE to a Go function declaration
func (tg *ToGo) TransformSubroutine(sub *f90.Subroutine) (_ *ast.FuncDecl, err error) {
	return tg.transformProcedure(sub)
}

// TransformSubroutine transforms a Fortran SUBROUTINE to a Go function declaration
func (tg *ToGo) TransformFunction(fn *f90.Function) (_ *ast.FuncDecl, err error) {
	return tg.transformProcedure(fn)
}

func (tg *ToGo) transformProcedure(subroutineOrFunc f90.ProgramUnit) (_ *ast.FuncDecl, err error) {
	err = tg.repl.SetScope(subroutineOrFunc)
	if err != nil {
		return nil, err
	}
	// Collect COMMON blocks from this procedure's scope
	var body []f90.Statement
	var returned *ast.FieldList
	if fn, ok := subroutineOrFunc.(*f90.Function); ok {
		body = fn.Body
		field := tg.getReturnParam()
		if field == nil {
			return nil, fmt.Errorf("failed to acquire return parameter type for %s", fn.Name)
		}
		returned = &ast.FieldList{List: []*ast.Field{field}}
	} else if sub, ok := subroutineOrFunc.(*f90.Subroutine); ok {
		body = sub.Body
	} else {
		panic("unexpected argument")
	}

	fn := &ast.FuncDecl{
		Name: ast.NewIdent(subroutineOrFunc.UnitName()),
		Type: &ast.FuncType{
			Params: &ast.FieldList{
				List: tg.getScopeParams(nil),
			},
			Results: returned,
		},
		Body: &ast.BlockStmt{},
	}
	fn.Body.List, err = tg.transformStatements(nil, body)
	if err != nil {
		return fn, err
	}
	// Add return statement for functions with return values (uses named return)
	if returned != nil {
		fn.Body.List = append(fn.Body.List, &ast.ReturnStmt{})
	}
	return fn, nil
}

func (tg *ToGo) getScopeParams(dst []*ast.Field) []*ast.Field {
	params := tg.repl.ScopeParams()
	for i := range params {
		vi := &params[i]
		tp := tg.goType(vi)
		// For INTENT(OUT) or INTENT(INOUT) non-array scalars, use pointer type
		intent := vi.decl.Type.Intent()
		isArray := tg.varIsArray(vi)
		if !isArray && (intent == f90.IntentOut || intent == f90.IntentInOut) {
			tp = &ast.StarExpr{X: tp}
		}
		dst = append(dst, &ast.Field{
			Type:  tp,
			Names: []*ast.Ident{tg.astIdent(vi.decl.Name)},
		})
	}
	return dst
}

func (tg *ToGo) getReturnParam() *ast.Field {
	ret := tg.repl.scope.returnType
	tp := tg.goType(ret)
	return &ast.Field{
		Type:  tp,
		Names: []*ast.Ident{tg.astIdent(ret.Identifier())},
	}

}

func (tg *ToGo) astLabel(f90Label string) *ast.Ident { return ast.NewIdent("label" + f90Label) }

func (tg *ToGo) makeErrAtStmt(msg string) error {
	return tg.makeErr(tg.currentStmt, msg)
}

func (tg *ToGo) makeErr(node f90.Node, msg string) error {
	tok := node.AppendTokenLiteral(nil)
	pos := node.SourcePos()
	return tg.makeErrWithPos(pos, fmt.Sprintf("%s in %T %s", msg, node, tok))
}

func (tg *ToGo) makeErrWithPos(pos f90.Position, msg string) error {
	// If source is available, compute line:column
	if tg.sourceFile != nil {
		callStr := getCallStack(2)
		var buf [1024]byte
		line, col, _, err := pos.ToLineCol(tg.sourceFile, buf[:])
		if err == nil && line > 0 {
			return fmt.Errorf("%s:%d:%d: %s\n%s", tg.source, line, col, msg, callStr)
		}
	}
	return fmt.Errorf("%s @ %d", msg, pos.Start())
}

func (tg *ToGo) transformStatements(dst []ast.Stmt, stmts []f90.Statement) (_ []ast.Stmt, err error) {
	for _, stmt := range stmts {
		label := stmt.GetLabel()
		if label == "" {
			dst, err = tg.transformStatement(dst, stmt)
		} else {
			var gstmts []ast.Stmt
			gstmts, err = tg.transformStatement(nil, stmt)
			lab := tg.astLabel(label)
			useGoto := &ast.BranchStmt{
				Label: lab,
				Tok:   token.GOTO, // Use the goto label so compiler does not complain.
			}
			labelled := &ast.LabeledStmt{
				Label: lab,
				Stmt:  &ast.BlockStmt{List: gstmts},
			}
			dst = append(dst, useGoto, labelled)
		}
		if err != nil {
			return dst, err
		}
	}
	return dst, nil
}

func (tg *ToGo) transformStatement(dst []ast.Stmt, stmt f90.Statement) (_ []ast.Stmt, err error) {
	if stmt != nil {
		tg.currentStmt = stmt
	}
	switch s := stmt.(type) {
	case *f90.TypeDeclaration:
		dst, err = tg.transformTypeDeclaration(dst, s)
	// case *f90.DerivedTypeStmt:
	// 	gostmt = tg.transformDerivedType(s)
	case *f90.AssignmentStmt:
		dst, err = tg.transformAssignment(dst, s)
	case *f90.CallStmt:
		dst, err = tg.transformCallStmt(dst, s)
	case *f90.PrintStmt:
		dst, err = tg.transformPrintStmt(dst, s)
	case *f90.IfStmt:
		dst, err = tg.transformIfStmt(dst, s)
	case *f90.DoLoop:
		dst, err = tg.transformDoLoop(dst, s)
	case *f90.ReturnStmt:
		// RETURN statement in functions will be handled by convertFunctionResultToReturn
		// For now, just generate empty return (will be filled with result value later)
		// gostmt = &ast.ReturnStmt{}
	case *f90.CycleStmt:
		// CYCLE → continue
		dst = append(dst, &ast.BranchStmt{Tok: token.CONTINUE})
	case *f90.ExitStmt:
		// EXIT → break
		dst = append(dst, &ast.BranchStmt{Tok: token.BREAK})
	case *f90.ContinueStmt:
		// CONTINUE → empty statement (no-op in Go)
		// If there's a label, it will be handled by label processing later
		// gostmt = &ast.EmptyStmt{}
	case *f90.GotoStmt:
		// GOTO label → goto labelN
		dst = append(dst, &ast.BranchStmt{
			Tok:   token.GOTO,
			Label: tg.astLabel(s.Target),
		})
	case *f90.AllocateStmt:
		// gostmt = tg.transformAllocateStmt(s)
	case *f90.DeallocateStmt:
		// gostmt = tg.transformDeallocateStmt(s)
	case *f90.SelectCaseStmt:
		dst, err = tg.transformSelectCaseStmt(dst, s)
	case *f90.CommonStmt:
		// COMMON blocks are processed separately, no code generation in function body
	case *f90.DimensionStmt:
		// DIMENSION statements are processed in preScanCommonBlocks, no code generation in function body
	case *f90.EquivalenceStmt:
		dst, err = tg.transformEquivalenceStmt(dst, s)
	case *f90.PointerCrayStmt:
		dst, err = tg.transformPointerCrayStmt(dst, s)
	case *f90.DataStmt:
		dst, err = tg.transformDataStmt(dst, s)
	case *f90.ArithmeticIfStmt:
		dst, err = tg.transformArithmeticIfStmt(dst, s)
	case *f90.ComputedGotoStmt:
		dst, err = tg.transformComputedGotoStmt(dst, s)
	case *f90.StopStmt:
		var code ast.Expr
		code, _, err = tg.transformExpression(_tgtInt, s.Code)
		dst = append(dst, &ast.ExprStmt{X: &ast.CallExpr{Fun: _astIntrinsicStop, Args: []ast.Expr{code}}})

	case *f90.WriteStmt:
		// gostmt = tg.transformWriteStmt(s)
	case *f90.FormatStmt:
		// FORMAT statements are compile-time format definitions, no runtime code
	case *f90.OpenStmt, *f90.CloseStmt, *f90.ReadStmt, *f90.BackspaceStmt, *f90.RewindStmt, *f90.EndfileStmt, *f90.InquireStmt:
		// File I/O statements - not yet implemented, skip silently
	case *f90.EntryStmt:
		// ENTRY statements (multiple entry points) - not supported
	case *f90.AssignStmt:
		// ASSIGN label TO variable (Fortran 77 feature) - not supported, skip silently
	case *f90.AssignedGotoStmt:
		// GOTO variable (assigned GOTO using label from ASSIGN statement) - not supported
	case *f90.ImplicitStatement, *f90.UseStatement, *f90.ExternalStmt, *f90.IntrinsicStmt:
		// Specification statement - no code generation
	default:
		// For now, unsupported statements are skipped
		err = tg.makeErr(s, "unsupported transpile statement")
	}
	return dst, err
}

func (tg *ToGo) makeArrayInitializer(typ *varinfo, initializer ast.Expr) (ast.Expr, error) {
	if !tg.varIsArray(typ) {
		return nil, tg.makeErrWithPos(typ.decl.Position, "invalid type declaration dimensions for array creation")
	}
	dims := typ.Dimensions()
	if dims.IsDeferred() {
		return nil, tg.makeErrWithPos(typ.decl.Position, "cannot create array from deferred shape")
	}
	args := []ast.Expr{initializer} // First argument is array initializer
	for _, bound := range dims.Bounds {
		size, _, err := tg.transformExpression(_tgtInt, bound.Upper)
		if err != nil {
			return nil, tg.makeErrWithPos(typ.decl.Position, "unable to make array: "+err.Error())
		}
		args = append(args, size)
	}
	// Get element type - handle CHARACTER specially
	elemType := tg.baseGotype(typ.typeToken(), tg.resolveKind(typ))
	expr := &ast.CallExpr{
		Fun: &ast.IndexExpr{
			X:     _astFnNewArray,
			Index: elemType,
		},
		Args: args,
	}
	return expr, nil
}

func (tg *ToGo) transformTypeDeclaration(dst []ast.Stmt, stmt *f90.TypeDeclaration) (_ []ast.Stmt, err error) {
	decl := &ast.GenDecl{
		Tok:   token.VAR,
		Specs: make([]ast.Spec, 0, len(stmt.Entities)),
	}
	var useSpecs ast.ValueSpec
	var arrayInits []ast.Stmt // Array initialization statements
	nouse := tg.astIdent("_")
	for i := range stmt.Entities {
		ent := &stmt.Entities[i]
		vi := tg.repl.Var(ent.Name)
		// Check if this is a PARAMETER constant (compile-time constant)
		isParamConst := stmt.Type.Attr(f90token.PARAMETER) != nil
		if vi.IsParameter() && !isParamConst {
			continue // Skip function parameters, they're already declared in function signature
		}
		tp := tg.goType(vi)
		ident := ast.NewIdent(vi.Identifier())
		spec := &ast.ValueSpec{
			Names: []*ast.Ident{ident},
			Type:  tp,
		}
		// For PARAMETER constants, add the initializer value
		if isParamConst && ent.Init != nil {
			initVal, _, err := tg.transformExpression(vi, ent.Init)
			if err != nil {
				return nil, err
			}
			spec.Values = []ast.Expr{initVal}
		}
		decl.Specs = append(decl.Specs, spec)
		useSpecs.Names = append(useSpecs.Names, nouse)
		// TODO: check usage flag.
		useSpecs.Values = append(useSpecs.Values, ident)
		// Generate array initialization for arrays with fixed dimensions
		// Skip allocatable arrays - they're initialized by ALLOCATE statements

		if tg.varIsArray(vi) && !vi.IsAllocatable() {
			newArrExpr, err := tg.makeArrayInitializer(vi, ast.NewIdent("nil"))
			if err != nil {
				return nil, err
			}
			// Generate: arr = intrinsic.NewArray[T](nil, sizes...)
			arrayInits = append(arrayInits, &ast.AssignStmt{
				Lhs: []ast.Expr{ident},
				Tok: token.ASSIGN,
				Rhs: []ast.Expr{newArrExpr},
			})
		} else if vi.decl.Type.Token == f90token.CHARACTER {
			// Generate CHARACTER initialization: str = intrinsic.NewCharacterArray(len)
			charLen := ent.Charlen()
			if charLen != nil {
				lenExpr, _, err := tg.transformExpression(_tgtInt, charLen)
				if err != nil {
					return nil, err
				}
				arrayInits = append(arrayInits, &ast.AssignStmt{
					Lhs: []ast.Expr{ident},
					Tok: token.ASSIGN,
					Rhs: []ast.Expr{&ast.CallExpr{
						Fun:  _astFnNewCharArray,
						Args: []ast.Expr{lenExpr},
					}},
				})
			}
		}
	}
	if len(decl.Specs) == 0 {
		return dst, nil // No local variables to declare
	}
	decl.Specs = append(decl.Specs, &useSpecs)
	dst = append(dst, &ast.DeclStmt{
		Decl: decl,
	})
	dst = append(dst, arrayInits...)
	return dst, nil
}

func (tg *ToGo) transformCallStmt(dst []ast.Stmt, stmt *f90.CallStmt) (_ []ast.Stmt, err error) {
	fninfo := tg.ContainedOrExtern(stmt.Name)
	if fninfo == nil {
		return dst, tg.makeErr(stmt, "subroutine not found: "+stmt.Name)
	}
	params := fninfo.ProcedureParams()
	if len(params) != len(stmt.Args) {
		return dst, tg.makeErr(stmt, "mismatched number of args with declaration")
	}
	gstmt := &ast.CallExpr{
		Fun: tg.astIdent(fninfo.name),
	}
	for i := range stmt.Args {
		info := &params[i]
		goexpr, _, err := tg.transformExpression(info, stmt.Args[i])
		if err != nil {
			return dst, err
		}
		// For INTENT(OUT/INOUT) non-array scalar parameters, pass address
		intent := info.decl.Type.Intent()
		isArray := tg.varIsArray(info)
		if !isArray && (intent == f90.IntentOut || intent == f90.IntentInOut) {
			goexpr = &ast.UnaryExpr{Op: token.AND, X: goexpr}
		}
		gstmt.Args = append(gstmt.Args, goexpr)
	}
	dst = append(dst, &ast.ExprStmt{
		X: gstmt,
	})
	return dst, nil
}

func (tg *ToGo) transformAssignment(dst []ast.Stmt, stmt *f90.AssignmentStmt) (_ []ast.Stmt, err error) {
	var targetVinfo *varinfo
	var lhs ast.Expr
	switch tgt := stmt.Target.(type) {
	case *f90.ArrayRef:
		targetVinfo = tg.repl.Var(tgt.Name)
	case *f90.Identifier:
		targetVinfo = tg.repl.Var(tgt.Value)
		lhs = tg.astVarExpr(targetVinfo)
		// Dereference INTENT(OUT/INOUT) non-array scalar parameters
		intent := targetVinfo.decl.Type.Intent()
		isArray := tg.varIsArray(targetVinfo)
		if !isArray && (intent == f90.IntentOut || intent == f90.IntentInOut) {
			lhs = &ast.StarExpr{X: lhs}
		}
		if binop, ok := stmt.Value.(*f90.BinaryExpr); ok && binop.Op == f90token.StringConcat {
			return tg.transformStringConcat(dst, targetVinfo._varname, binop)
		}
	case *f90.FunctionCall:
		// FunctionCall as assignment target occurs for CHARACTER substring: str(1:5) = 'x'
		targetVinfo = tg.repl.Var(tgt.Name)
	default:
		err = tg.makeErr(tgt, "unknown target expression in assignment")
	}
	if err != nil {
		return nil, err
	} else if targetVinfo == nil {
		return nil, tg.makeErr(stmt.Target, "unknown identifier in target expression of assignment")
	} else if targetVinfo.decl == nil {
		return nil, tg.makeErr(stmt.Target, "identifier with no corresponding type declaration:"+targetVinfo.Identifier())
	}
	rhs, _, err := tg.transformExpression(targetVinfo, stmt.Value)
	if err != nil {
		return dst, err
	}
	if targetVinfo.decl.Type.Token == f90token.CHARACTER {
		receiver := tg.astVarExpr(targetVinfo)
		stmt := &ast.ExprStmt{
			X: &ast.CallExpr{
				Fun:  &ast.SelectorExpr{X: receiver, Sel: ast.NewIdent("SetFromString")},
				Args: []ast.Expr{rhs},
			},
		}
		dst = append(dst, stmt)
		return dst, nil
	}

	switch tgt := stmt.Target.(type) {
	case *f90.ArrayRef:
		return tg.transformSetArrayRef(dst, tgt, rhs)
	case *f90.FunctionCall:
		// FunctionCall as target: COMMON block arrays or undeclared arrays
		// Convert to ArrayRef-like handling
		syntheticRef := &f90.ArrayRef{
			Name:       tgt.Name,
			Subscripts: tgt.Args,
			Position:   tgt.Position,
		}
		return tg.transformSetArrayRef(dst, syntheticRef, rhs)
	default:
		if lhs == nil {
			return dst, tg.makeErr(stmt.Target, "unsupported assignment target")
		}
	}

	// Convert RHS to target type if needed
	var rhsType varinfo
	if err := tg.repl.InferType(&rhsType, stmt.Value); err != nil {
		return dst, err
	}

	// Special case: cross-type pointer assignment (npii = npaa where types differ)
	// Generates: npii = intrinsic.PointerFrom[T](npaa)
	if targetVinfo.pointee != "" && rhsType.pointee != "" {
		// Both are pointer variables - check if pointee types differ
		targetPointee := tg.repl.Var(targetVinfo.pointee)
		rhsPointee := tg.repl.Var(rhsType.pointee)
		if targetPointee == nil || rhsPointee == nil {
			return dst, tg.makeErr(stmt, "pointee(s) not found: "+targetVinfo.pointee+", "+rhsType.pointee)
		}
		tgtTok := targetPointee.typeToken()
		rhsTok := rhsPointee.typeToken()
		if tgtTok != rhsTok {
			// Different types - use PointerFrom for conversion
			elemType := tg.baseGotype(tgtTok, tg.resolveKind(targetPointee))
			rhs = &ast.CallExpr{
				Fun: &ast.IndexExpr{
					X:     &ast.SelectorExpr{X: _astIntrinsic, Sel: ast.NewIdent("PointerFrom")},
					Index: elemType,
				},
				Args: []ast.Expr{rhs},
			}
		}
	} else {
		rhs = tg.wrapConversion(targetVinfo, &rhsType, rhs)
	}

	// Handle equivalenced scalar assignment: f = value → f.Set(1, value)
	// CHARACTER types are excluded as they use SetFromString
	isArray := tg.varIsArray(targetVinfo)
	isCharacter := targetVinfo.typeToken() == f90token.CHARACTER
	if !isArray && !isCharacter && targetVinfo.flags.HasAny(flagEquivalenced) {
		dst = append(dst, &ast.ExprStmt{
			X: &ast.CallExpr{
				Fun:  &ast.SelectorExpr{X: lhs, Sel: ast.NewIdent("Set")},
				Args: []ast.Expr{&ast.BasicLit{Kind: token.INT, Value: "1"}, rhs},
			},
		})
		return dst, nil
	}

	gstmt := &ast.AssignStmt{
		Tok: token.ASSIGN,
		Rhs: []ast.Expr{rhs},
		Lhs: []ast.Expr{lhs},
	}
	dst = append(dst, gstmt)

	// If target is a Cray pointer variable and RHS is not another pointer,
	// also assign to the pointee variable so it shares the same memory.
	// e.g., NPAA = MALLOC(...) → npaa = ...; aa = npaa
	if targetVinfo.pointee != "" {
		pointeeIdent := ast.NewIdent(targetVinfo.pointee)
		ptrIdent := ast.NewIdent(targetVinfo.Identifier())
		dst = append(dst, &ast.AssignStmt{
			Tok: token.ASSIGN,
			Lhs: []ast.Expr{pointeeIdent},
			Rhs: []ast.Expr{ptrIdent},
		})
	}

	return dst, nil
}

func (tg *ToGo) transformPrintStmt(dst []ast.Stmt, stmt *f90.PrintStmt) (_ []ast.Stmt, err error) {
	// Transform output list expressions to Go expressions
	var args []ast.Expr
	var tgt varinfo
	for _, expr := range stmt.OutputList {
		err = tg.repl.InferType(&tgt, expr)
		if err != nil {
			return dst, tg.makeErr(stmt, err.Error())
		}
		goExpr, tp, err := tg.transformExpression(&tgt, expr)
		if err != nil {
			return dst, err
		}
		if tg.varIsPointerTo(tp) {
			// Print intrinsic can't just receive pointers willy nilly.
			goExpr = &ast.CallExpr{
				Fun: &ast.SelectorExpr{
					X:   goExpr,
					Sel: ast.NewIdent("At"),
				},
				Args: []ast.Expr{_astOne},
			}
		}
		args = append(args, goExpr)
	}
	// Generate: intrinsic.Print(args...)
	callExpr := &ast.CallExpr{
		Fun:  _astFnPrint,
		Args: args,
	}
	dst = append(dst, &ast.ExprStmt{X: callExpr})
	return dst, nil
}

func (tg *ToGo) transformIfStmt(dst []ast.Stmt, stmt *f90.IfStmt) (_ []ast.Stmt, err error) {
	// Transform the main condition
	cond, _, err := tg.transformExpression(_tgtBool, stmt.Condition)
	if err != nil {
		return dst, err
	}

	// Transform THEN part statements
	thenBody, err := tg.transformStatements(nil, stmt.ThenPart)
	if err != nil {
		return dst, err
	}

	// Build the Go if statement
	goIf := &ast.IfStmt{
		Cond: cond,
		Body: &ast.BlockStmt{List: thenBody},
	}

	// Handle ELSE IF parts (chain them)
	currentIf := goIf
	for _, elseIf := range stmt.ElseIfParts {
		elseIfCond, _, err := tg.transformExpression(_tgtBool, elseIf.Condition)
		if err != nil {
			return dst, err
		}
		elseIfBody, err := tg.transformStatements(nil, elseIf.ThenPart)
		if err != nil {
			return dst, err
		}
		nextIf := &ast.IfStmt{
			Cond: elseIfCond,
			Body: &ast.BlockStmt{List: elseIfBody},
		}
		currentIf.Else = nextIf
		currentIf = nextIf
	}

	// Handle ELSE part
	if len(stmt.ElsePart) > 0 {
		elseBody, err := tg.transformStatements(nil, stmt.ElsePart)
		if err != nil {
			return dst, err
		}
		currentIf.Else = &ast.BlockStmt{List: elseBody}
	}

	dst = append(dst, goIf)
	return dst, nil
}

func (tg *ToGo) transformDoLoop(dst []ast.Stmt, stmt *f90.DoLoop) (_ []ast.Stmt, err error) {
	// Transform body statements
	bodyStmts, err := tg.transformStatements(nil, stmt.Body)
	if err != nil {
		return dst, err
	}

	// Handle DO WHILE (no loop variable, condition in Start)
	if stmt.Var == "" {
		// DO WHILE: for condition { ... }
		condExpr, _, err := tg.transformExpression(_tgtBool, stmt.Start)
		if err != nil {
			return dst, err
		}
		forStmt := &ast.ForStmt{
			Cond: condExpr,
			Body: &ast.BlockStmt{List: bodyStmts},
		}
		dst = append(dst, forStmt)
		return dst, nil
	}

	// Counter-controlled DO loop: DO i = start, end [, step]
	loopVar := tg.repl.Var(stmt.Var)
	if loopVar == nil {
		return dst, tg.makeErr(stmt, "unknown loop variable: "+stmt.Var)
	}
	loopVarIdent := ast.NewIdent(loopVar.Identifier())

	// Transform start expression
	startExpr, _, err := tg.transformExpression(loopVar, stmt.Start)
	if err != nil {
		return dst, err
	}

	// Transform end expression
	endExpr, _, err := tg.transformExpression(loopVar, stmt.End)
	if err != nil {
		return dst, err
	}

	// Transform step expression (defaults to 1 if not provided)
	var stepExpr ast.Expr = &ast.BasicLit{Kind: token.INT, Value: "1"}
	if stmt.Step != nil {
		stepExpr, _, err = tg.transformExpression(loopVar, stmt.Step)
		if err != nil {
			return dst, err
		}
	}

	// Build Go for loop: for i := start; i <= end; i += step { ... }
	// Init: i = start
	initStmt := &ast.AssignStmt{
		Lhs: []ast.Expr{loopVarIdent},
		Tok: token.ASSIGN,
		Rhs: []ast.Expr{startExpr},
	}

	// Condition: i <= end
	condExpr := &ast.BinaryExpr{
		X:  loopVarIdent,
		Op: token.LEQ,
		Y:  endExpr,
	}

	// Post: i += step (or i++ if step is 1)
	var postStmt ast.Stmt
	if basicLit, ok := stepExpr.(*ast.BasicLit); ok && basicLit.Value == "1" {
		postStmt = &ast.IncDecStmt{X: loopVarIdent, Tok: token.INC}
	} else {
		postStmt = &ast.AssignStmt{
			Lhs: []ast.Expr{loopVarIdent},
			Tok: token.ADD_ASSIGN,
			Rhs: []ast.Expr{stepExpr},
		}
	}

	forStmt := &ast.ForStmt{
		Init: initStmt,
		Cond: condExpr,
		Post: postStmt,
		Body: &ast.BlockStmt{List: bodyStmts},
	}

	dst = append(dst, forStmt)
	return dst, nil
}

func (tg *ToGo) transformSelectCaseStmt(dst []ast.Stmt, stmt *f90.SelectCaseStmt) (_ []ast.Stmt, err error) {
	// Transform the selector expression
	var viSelector varinfo
	err = tg.repl.InferType(&viSelector, stmt.Expression)
	if err != nil {
		return dst, tg.makeErr(stmt.Expression, err.Error())
	}
	tagExpr, _, err := tg.transformExpression(&viSelector, stmt.Expression)
	if err != nil {
		return dst, err
	}

	// Build Go switch statement
	switchStmt := &ast.SwitchStmt{
		Tag:  tagExpr,
		Body: &ast.BlockStmt{},
	}

	for _, clause := range stmt.Cases {
		// Transform body statements
		bodyStmts, err := tg.transformStatements(nil, clause.Body)
		if err != nil {
			return dst, err
		}

		caseClause := &ast.CaseClause{
			Body: bodyStmts,
		}

		if clause.IsDefault {
			// CASE DEFAULT → default:
			caseClause.List = nil
		} else {
			// CASE (val1, val2, ...) → case val1, val2, ...:
			for _, val := range clause.Values {
				valExpr, _, err := tg.transformExpression(&viSelector, val)
				if err != nil {
					return dst, err
				}
				caseClause.List = append(caseClause.List, valExpr)
			}
		}

		switchStmt.Body.List = append(switchStmt.Body.List, caseClause)
	}

	dst = append(dst, switchStmt)
	return dst, nil
}

func (tg *ToGo) transformDataStmt(dst []ast.Stmt, stmt *f90.DataStmt) (_ []ast.Stmt, err error) {
	// DATA statements initialize variables: DATA a, b, c / 10, 20, 30 /
	// Generate assignment statements for each variable-value pair
	if len(stmt.Variables) != len(stmt.Values) {
		return dst, tg.makeErr(stmt, "DATA statement variable/value count mismatch")
	}

	for i := range stmt.Variables {
		varExpr := stmt.Variables[i]
		valExpr := stmt.Values[i]

		// Get target variable info and LHS expression
		var targetVinfo *varinfo
		var lhs ast.Expr
		var isArrayElement bool

		switch v := varExpr.(type) {
		case *f90.Identifier:
			targetVinfo = tg.repl.Var(v.Value)
			lhs = tg.astVarExpr(targetVinfo)
		case *f90.ArrayRef:
			targetVinfo = tg.repl.Var(v.Name)
			if len(v.Subscripts) > 0 {
				// Array element with indices: use Set method
				isArrayElement = true
			} else {
				// No subscripts - treat as scalar variable
				lhs = tg.astVarExpr(targetVinfo)
			}
		case *f90.FunctionCall:
			// May be parsed as function call for array access
			targetVinfo = tg.repl.Var(v.Name)
			if len(v.Args) > 0 {
				isArrayElement = true
			} else {
				lhs = tg.astVarExpr(targetVinfo)
			}
		default:
			return dst, tg.makeErr(stmt, "unsupported DATA statement variable type")
		}

		if targetVinfo == nil {
			return dst, tg.makeErr(stmt, "unknown variable in DATA statement")
		}

		// Transform the value expression
		rhs, _, err := tg.transformExpression(targetVinfo, valExpr)
		if err != nil {
			return dst, err
		}

		if isArrayElement {
			// Array element: generate arr.Set(value, indices)
			switch v := varExpr.(type) {
			case *f90.ArrayRef:
				dst, err = tg.transformSetArrayRef(dst, v, rhs)
			case *f90.FunctionCall:
				syntheticRef := &f90.ArrayRef{
					Name:       v.Name,
					Subscripts: v.Args,
					Position:   v.Position,
				}
				dst, err = tg.transformSetArrayRef(dst, syntheticRef, rhs)
			}
			if err != nil {
				return dst, err
			}
		} else {
			// Scalar: generate simple assignment
			dst = append(dst, &ast.AssignStmt{
				Lhs: []ast.Expr{lhs},
				Tok: token.ASSIGN,
				Rhs: []ast.Expr{rhs},
			})
		}
	}

	return dst, nil
}

func (tg *ToGo) transformArithmeticIfStmt(dst []ast.Stmt, stmt *f90.ArithmeticIfStmt) (_ []ast.Stmt, err error) {
	// Arithmetic IF: IF (x) neg, zero, pos
	// Becomes: if x < 0 { goto neg } else if x == 0 { goto zero } else { goto pos }
	condExpr, _, err := tg.transformExpression(_tgtInt, stmt.Condition)
	if err != nil {
		return dst, err
	}

	zero := &ast.BasicLit{Kind: token.INT, Value: "0"}

	// if condition < 0 { goto negLabel }
	negIf := &ast.IfStmt{
		Cond: &ast.BinaryExpr{X: condExpr, Op: token.LSS, Y: zero},
		Body: &ast.BlockStmt{List: []ast.Stmt{
			&ast.BranchStmt{Tok: token.GOTO, Label: tg.astLabel(stmt.NegativeLabel)},
		}},
	}

	// else if condition == 0 { goto zeroLabel }
	zeroIf := &ast.IfStmt{
		Cond: &ast.BinaryExpr{X: condExpr, Op: token.EQL, Y: zero},
		Body: &ast.BlockStmt{List: []ast.Stmt{
			&ast.BranchStmt{Tok: token.GOTO, Label: tg.astLabel(stmt.ZeroLabel)},
		}},
		Else: &ast.BlockStmt{List: []ast.Stmt{
			// else { goto posLabel }
			&ast.BranchStmt{Tok: token.GOTO, Label: tg.astLabel(stmt.PositiveLabel)},
		}},
	}

	negIf.Else = zeroIf
	dst = append(dst, negIf)
	return dst, nil
}

func (tg *ToGo) transformComputedGotoStmt(dst []ast.Stmt, stmt *f90.ComputedGotoStmt) (_ []ast.Stmt, err error) {
	// Computed GOTO: GO TO (100, 200, 300), choice
	// Becomes: switch choice { case 1: goto label100; case 2: goto label200; ... }
	tagExpr, _, err := tg.transformExpression(_tgtInt, stmt.Expression)
	if err != nil {
		return dst, err
	}

	switchStmt := &ast.SwitchStmt{
		Tag:  tagExpr,
		Body: &ast.BlockStmt{},
	}

	for i, label := range stmt.Labels {
		caseClause := &ast.CaseClause{
			List: []ast.Expr{&ast.BasicLit{Kind: token.INT, Value: strconv.Itoa(i + 1)}},
			Body: []ast.Stmt{
				&ast.BranchStmt{Tok: token.GOTO, Label: tg.astLabel(label)},
			},
		}
		switchStmt.Body.List = append(switchStmt.Body.List, caseClause)
	}

	dst = append(dst, switchStmt)
	return dst, nil
}

func (tg *ToGo) transformEquivalenceStmt(dst []ast.Stmt, stmt *f90.EquivalenceStmt) (_ []ast.Stmt, err error) {
	for _, set := range stmt.Sets {
		if len(set) < 2 {
			continue // Need at least 2 items to equivalence
		}

		// Analyze the set to find the primary storage provider
		// Priority: arrays > character arrays > largest scalar
		var primaryIdx int
		var primarySize int
		var hasArray bool
		for i, ref := range set {
			vinfo := tg.repl.Var(ref.Name)
			if vinfo == nil {
				continue
			}
			isArray := tg.varIsArray(vinfo)
			if isArray {
				hasArray = true
				primaryIdx = i
				break
			}
			// For scalars, track the largest by type size
			size := tg.typeSize(vinfo)
			if size > primarySize {
				primarySize = size
				primaryIdx = i
			}
		}

		primaryRef := set[primaryIdx]
		primaryVinfo := tg.repl.Var(primaryRef.Name)
		primaryExpr := tg.astVarExpr(primaryVinfo)

		if !hasArray {
			// All scalars - allocate memory for primary and share with others
			// Generate: primary = intrinsic.MALLOC[T](size)
			primaryType := tg.baseGotype(primaryVinfo.typeToken(), tg.resolveKind(primaryVinfo))
			dst = append(dst, &ast.AssignStmt{
				Tok: token.ASSIGN,
				Lhs: []ast.Expr{primaryExpr},
				Rhs: []ast.Expr{
					&ast.CallExpr{
						Fun: &ast.IndexExpr{
							X:     &ast.SelectorExpr{X: _astIntrinsic, Sel: ast.NewIdent("MALLOC")},
							Index: primaryType,
						},
						Args: []ast.Expr{&ast.BasicLit{Kind: token.INT, Value: fmt.Sprintf("%d", primarySize)}},
					},
				},
			})

			// Generate: other = intrinsic.PointerFrom[T](primary) for each other scalar
			for i, ref := range set {
				if i == primaryIdx {
					continue
				}
				vinfo := tg.repl.Var(ref.Name)
				if vinfo == nil {
					return dst, tg.makeErrWithPos(stmt.Position, "unknown variable in EQUIVALENCE: "+ref.Name)
				}
				varExpr := tg.astVarExpr(vinfo)
				elemType := tg.baseGotype(vinfo.typeToken(), tg.resolveKind(vinfo))

				// Generate: other = intrinsic.PointerFrom[T](primary)
				dst = append(dst, &ast.AssignStmt{
					Tok: token.ASSIGN,
					Lhs: []ast.Expr{varExpr},
					Rhs: []ast.Expr{
						&ast.CallExpr{
							Fun: &ast.IndexExpr{
								X:     &ast.SelectorExpr{X: _astIntrinsic, Sel: ast.NewIdent("PointerFrom")},
								Index: elemType,
							},
							Args: []ast.Expr{primaryExpr},
						},
					},
				})
			}
		} else {
			// Has array - use Equivalence with PointerOff for offsets
			args := make([]ast.Expr, len(set))
			for i, ref := range set {
				vinfo := tg.repl.Var(ref.Name)
				if vinfo == nil {
					return dst, tg.makeErrWithPos(stmt.Position, "unknown variable in EQUIVALENCE: "+ref.Name)
				}

				varExpr := tg.astVarExpr(vinfo)
				isArray := tg.varIsArray(vinfo)
				isCharacter := vinfo.typeToken() == f90token.CHARACTER
				isPointerTo := tg.varIsPointerTo(vinfo)

				if len(ref.Subscripts) == 0 {
					if isArray {
						args[i] = varExpr // *Array implements PointerSetter
					} else if isCharacter {
						args[i] = &ast.UnaryExpr{Op: token.AND, X: varExpr}
					} else if isPointerTo {
						// Equivalenced scalar is already PointerTo[T], use &var
						args[i] = &ast.UnaryExpr{Op: token.AND, X: varExpr}
					} else {
						return dst, tg.makeErrWithPos(stmt.Position, "unexpected non-equivalenced scalar in EQUIVALENCE: "+ref.Name)
					}
				} else {
					// Subscripted: PointerOff(var, var.AtOffset(...))
					var offsetArgs []ast.Expr
					for _, sub := range ref.Subscripts {
						arg, _, err := tg.transformExpression(_tgtInt, sub)
						if err != nil {
							return dst, err
						}
						offsetArgs = append(offsetArgs, arg)
					}
					offsetCall := &ast.CallExpr{
						Fun:  &ast.SelectorExpr{X: varExpr, Sel: ast.NewIdent("AtOffset")},
						Args: offsetArgs,
					}
					args[i] = &ast.CallExpr{
						Fun: &ast.SelectorExpr{
							X:   _astIntrinsic,
							Sel: ast.NewIdent("PointerOff"),
						},
						Args: []ast.Expr{varExpr, offsetCall},
					}
				}
			}

			dst = append(dst, &ast.ExprStmt{
				X: &ast.CallExpr{
					Fun: &ast.SelectorExpr{
						X:   _astIntrinsic,
						Sel: ast.NewIdent("Equivalence"),
					},
					Args: args,
				},
			})
		}
	}
	return dst, nil
}

// typeSize returns the size in bytes for a variable's base type.
func (tg *ToGo) typeSize(v *varinfo) int {
	kind := tg.resolveKind(v)
	switch v.typeToken() {
	case f90token.REAL:
		if kind == 8 {
			return 8
		}
		return 4
	case f90token.DOUBLE, f90token.DOUBLEPRECISION:
		return 8
	case f90token.INTEGER:
		switch kind {
		case 1:
			return 1
		case 2:
			return 2
		case 8:
			return 8
		default:
			return 4
		}
	case f90token.COMPLEX:
		if kind == 8 {
			return 16
		}
		return 8
	case f90token.LOGICAL:
		return 4
	default:
		return 4
	}
}

// transformPointerCrayStmt generates declarations for Cray-style POINTER statements.
// For POINTER (NPAA, AA(1)), generates:
//
//	var npaa intrinsic.PointerTo[float64]
//	var aa intrinsic.PointerTo[float64]
func (tg *ToGo) transformPointerCrayStmt(dst []ast.Stmt, stmt *f90.PointerCrayStmt) (_ []ast.Stmt, err error) {
	for _, pair := range stmt.Pointers {
		ptrVar := tg.repl.Var(pair.PointerVar)
		pointeeVar := tg.repl.Var(pair.Pointee)
		if ptrVar == nil || pointeeVar == nil {
			return dst, tg.makeErr(stmt, "pointer or pointee variable not found")
		}

		// Get the pointee's base type for PointerTo[T]
		pointeeType := tg.baseGotype(pointeeVar.typeToken(), tg.resolveKind(pointeeVar))
		ptrType := goTypePointerTo(pointeeType)

		// Generate: var ptrName intrinsic.PointerTo[T]
		ptrIdent := ast.NewIdent(ptrVar.Identifier())
		pointeeIdent := ast.NewIdent(pointeeVar.Identifier())
		nouse := tg.astIdent("_")

		decl := &ast.GenDecl{
			Tok: token.VAR,
			Specs: []ast.Spec{
				&ast.ValueSpec{Names: []*ast.Ident{ptrIdent}, Type: ptrType},
				&ast.ValueSpec{Names: []*ast.Ident{pointeeIdent}, Type: ptrType},
				&ast.ValueSpec{Names: []*ast.Ident{nouse, nouse}, Values: []ast.Expr{ptrIdent, pointeeIdent}},
			},
		}
		dst = append(dst, &ast.DeclStmt{Decl: decl})
	}
	return dst, nil
}

// goType converts a varinfo to Go type, considering KIND parameter.
// KIND mappings:
//
//	INTEGER(KIND=1) → int8, INTEGER(KIND=2) → int16
//	INTEGER(KIND=4) → int32, INTEGER(KIND=8) → int64
//	REAL(KIND=4) → float32, REAL(KIND=8) → float64
//
// Arrays are returned as pointer types (*intrinsic.Array[T]).
func (tg *ToGo) goType(v *varinfo) ast.Expr {
	tok := v.typeToken()
	isArray := tg.varIsArray(v)
	// Handle Cray-style pointer variables (POINTER (ptr, pointee))
	// The pointer variable's type is PointerTo[pointee_type]
	if tg.varIsPointerTo(v) {
		if v.pointee != "" {
			pointeeVar := tg.repl.Var(v.pointee)
			if pointeeVar == nil {
				panic(tg.makeErrWithPos(v.decl.Position, "pointee variable not found: "+v.pointee))
			}
			pointeeType := tg.baseGotype(pointeeVar.typeToken(), tg.resolveKind(pointeeVar))
			return goTypePointerTo(pointeeType)
		}
		return goTypePointerTo(tg.baseGotype(v.typeToken(), tg.resolveKind(v)))
	}

	// Handle TYPE
	if tok == f90token.TYPE {
		if v.decl.Type.Name != "" {
			return ast.NewIdent(v.decl.Type.Name)
		}
		tg.makeErrWithPos(v.decl.Position, "TYPE without name: "+v.decl.Name)
		return ast.NewIdent("")
	}

	// Get base type for numeric types
	baseType := tg.baseGotype(tok, tg.resolveKind(v))
	if baseType == nil {
		tg.makeErrWithPos(v.decl.Position, "unable to determine goType from: "+v.decl.Name)
		return nil
	}

	// Handle equivalenced scalars - they become PointerTo[T] for memory sharing
	// CHARACTER types are excluded as they have their own memory management (CharacterArray)
	if !isArray && tok != f90token.CHARACTER && v.flags.HasAny(flagEquivalenced) {
		return goTypePointerTo(baseType)
	}

	if !isArray {
		return baseType
	}

	// Is an array - return pointer type
	return &ast.StarExpr{
		X: &ast.IndexExpr{
			X:     _astTypeArray,
			Index: baseType,
		},
	}
}

// goTypePointerTo returns intrinsic.PointerTo[elementType] for Cray-style pointers.
func goTypePointerTo(elementType ast.Expr) ast.Expr {
	return &ast.IndexExpr{
		X:     _astTypePointerTo,
		Index: elementType,
	}
}

func (tg *ToGo) baseGotype(tok f90token.Token, kindValue int) (goType ast.Expr) {
	switch tok {
	default:
		err := tg.makeErrAtStmt("unsupported type token: " + tok.String())
		panic(err)
	case f90token.INTEGER:
		switch kindValue {
		case 1:
			goType = ast.NewIdent("int8")
		case 2:
			goType = ast.NewIdent("int16")
		case 8:
			goType = ast.NewIdent("int64")
		default: // 4 or unspecified
			goType = ast.NewIdent("int32")
		}
	case f90token.LOGICAL:
		goType = ast.NewIdent("bool")
	case f90token.DOUBLEPRECISION:
		goType = ast.NewIdent("float64")
	case f90token.REAL:
		switch kindValue {
		case 8:
			goType = ast.NewIdent("float64")
		default: // 4 or unspecified
			goType = ast.NewIdent("float32")
		}
	case f90token.CHARACTER:
		goType = _astTypeCharArray
	}
	return goType
}

// varIsArray returns true if the variable's Go type is *intrinsic.Array[T].
// This applies to Fortran variables declared with DIMENSION attribute or
// explicit array bounds in their type declaration.
func (tg *ToGo) varIsArray(v *varinfo) bool {
	return v.flags.HasAny(flagDimension)
}

// varIsPointerTo returns true if the variable's Go type is intrinsic.PointerTo[T]
// AND should be automatically dereferenced when accessed.
//
// This applies to:
//   - Equivalenced scalar variables (flagEquivalenced) - need dereferencing for value access
//   - Non-array Cray-style pointee variables (flagPointee) - rare, usually pointees are arrays
//
// Excluded:
//   - Cray-style pointer variables (flagPointer) - represent the pointer object, not pointee data
//   - Arrays (flagDimension) - have their own access patterns (*intrinsic.Array[T])
//   - CHARACTER types - use intrinsic.CharacterArray
func (tg *ToGo) varIsPointerTo(v *varinfo) bool {
	// Pointer variables (like NPAA) represent the address holder, not the data.
	// They should not be auto-dereferenced.
	if v.flags.HasAny(flagPointer) {
		return false
	}
	return v.flags.HasAny(flagEquivalenced|flagPointee) && !tg.varIsArray(v) && v.typeToken() != f90token.CHARACTER
}

func (tg *ToGo) transformStringConcat(dst []ast.Stmt, receiver string, root *f90.BinaryExpr) (_ []ast.Stmt, err error) {
	// Flatten operands in left-to-right order (non-recursive)
	var operands []f90.Expression
	pending := []f90.Expression{root}
	for len(pending) > 0 {
		expr := pending[len(pending)-1]
		pending = pending[:len(pending)-1]
		bin, ok := expr.(*f90.BinaryExpr)
		if !ok || bin.Op != f90token.StringConcat {
			operands = append(operands, expr)
			continue
		}
		// Push right first, then left (LIFO → left processed first)
		pending = append(pending, bin.Right, bin.Left)
	}

	// Transform operands
	var args []ast.Expr
	for _, op := range operands {
		switch e := op.(type) {
		case *f90.StringLiteral:
			args = append(args, &ast.BasicLit{Kind: token.STRING, Value: strconv.Quote(e.Value)})
		case *f90.Identifier:
			args = append(args, tg.astMethodCall(e.Value, "String"))
		default:
			return dst, tg.makeErr(op, "unsupported expression for string concat")
		}
	}

	gstmt := &ast.ExprStmt{X: tg.astMethodCall(receiver, "SetConcatString", args...)}
	dst = append(dst, gstmt)
	return dst, nil
}

// astMethodCall creates: receiver.methodName(args...)
func (tg *ToGo) astMethodCall(receiver, methodName string, args ...ast.Expr) *ast.CallExpr {
	return &ast.CallExpr{
		Fun: &ast.SelectorExpr{
			X:   ast.NewIdent(sanitizeIdent(receiver)),
			Sel: ast.NewIdent(methodName),
		},
		Args: args,
	}
}

// Common intrinsic identifiers.
var (
	_astIntrinsicStop  = &ast.SelectorExpr{X: _astIntrinsic, Sel: ast.NewIdent("Stop")}
	_astIntrinsic      = ast.NewIdent("intrinsic")
	_astFnNewCharArray = &ast.SelectorExpr{
		X:   ast.NewIdent("intrinsic"),
		Sel: ast.NewIdent("NewCharacterArray"),
	}
	_astFnNewArrayFromValues = &ast.SelectorExpr{
		X:   ast.NewIdent("intrinsic"),
		Sel: ast.NewIdent("NewArrayFromValues"),
	}
	_astFnNewArray = &ast.SelectorExpr{
		X:   ast.NewIdent("intrinsic"),
		Sel: ast.NewIdent("NewArray"),
	}
	_astFnPrint = &ast.SelectorExpr{
		X:   ast.NewIdent("intrinsic"),
		Sel: ast.NewIdent("Print"),
	}
	_astTypeCharArray = &ast.SelectorExpr{
		X:   ast.NewIdent("intrinsic"),
		Sel: ast.NewIdent("CharacterArray"),
	}
	_astTypeArray = &ast.SelectorExpr{
		X:   ast.NewIdent("intrinsic"),
		Sel: ast.NewIdent("Array"),
	}
	_astTypePointer = &ast.SelectorExpr{
		X:   ast.NewIdent("intrinsic"),
		Sel: ast.NewIdent("Pointer"),
	}
	_astTypePointerTo = &ast.SelectorExpr{
		X:   ast.NewIdent("intrinsic"),
		Sel: ast.NewIdent("PointerTo"),
	}
)

// sanitizeIdent returns a valid Go identifier.
// Fortran is case-insensitive, so we normalize to lowercase.
// If the result is a Go keyword, capitalize the first letter.
func sanitizeIdent(name string) string {
	if name == "" {
		return name
	}
	// Normalize to lowercase (Fortran is case-insensitive)
	name = strings.ToLower(name)
	// Check if it's a Go keyword
	if token.IsKeyword(name) {
		// Capitalize first letter to avoid conflict
		return strings.ToUpper(name[:1]) + name[1:]
	}
	return name
}

// AppendCommonDecls appends COMMON block struct declarations to dst.
// Should be called after all program units have been processed.
func (tg *ToGo) AppendCommonDecls(dst []ast.Decl) []ast.Decl {
	for _, block := range tg.repl.commonblocks {
		// fmt.Println("DECL", block.Name)
		if len(block.fields) == 0 {
			continue
		}
		blockIdent := ast.NewIdent(block.Name)
		var compositeLitElts []ast.Expr
		structType := &ast.StructType{Fields: &ast.FieldList{}}
		for i := range block.fields {
			v := &block.fields[i]
			goType := tg.goType(v)
			elemType := tg.baseGotype(v.typeToken(), tg.resolveKind(v))
			fieldIdent := ast.NewIdent(v.Identifier())

			structType.Fields.List = append(structType.Fields.List, &ast.Field{
				Names: []*ast.Ident{fieldIdent},
				Type:  goType,
			})

			if !tg.varIsArray(v) {
				continue
			}
			args := []ast.Expr{ast.NewIdent("nil")} // First argument is array initializer, we always initialize to zeroes.
			arrspec := v.Dimensions()
			for _, bound := range arrspec.Bounds {
				if bound.Upper != nil {
					size, _, err := tg.transformExpression(_tgtInt, bound.Upper)
					if err != nil {
						continue
					}
					args = append(args, size)
				}
			}
			compositeLitElts = append(compositeLitElts, &ast.KeyValueExpr{
				Key: fieldIdent,
				Value: &ast.CallExpr{
					Fun: &ast.IndexExpr{
						X:     _astFnNewArray,
						Index: elemType,
					},
					Args: args,
				},
			})
		}
		// Create variable declaration
		var valueSpec *ast.ValueSpec
		if len(compositeLitElts) > 0 {
			// var BLOCKNAME = struct{...}{field: value, ...}
			valueSpec = &ast.ValueSpec{
				Names: []*ast.Ident{blockIdent},
				Values: []ast.Expr{
					&ast.CompositeLit{
						Type: structType,
						Elts: compositeLitElts,
					},
				},
			}
		} else {
			// var BLOCKNAME struct{...}
			valueSpec = &ast.ValueSpec{
				Names: []*ast.Ident{blockIdent},
				Type:  structType,
			}
		}
		dst = append(dst, &ast.GenDecl{
			Tok:   token.VAR,
			Specs: []ast.Spec{valueSpec},
		})
	}

	return dst
}

func (tg *ToGo) resolveKind(v *varinfo) int {
	if v.decl == nil {
		panic(tg.makeErrAtStmt("nil declaration for variable " + v.Identifier()))
	}
	return tg.resolveKindFromDecl(v.decl)
}

func (tg *ToGo) resolveKindFromDecl(decl *f90.DeclEntity) int {
	if decl == nil {
		return 0
	}
	kind := decl.Kind()
	if kind == nil {
		return 0
	}
	var dst varinfo
	if err := tg.repl.Eval(&dst, kind); err != nil {
		return 0
	}
	return int(dst.val.i64)
}
