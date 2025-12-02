package fortran

import (
	"errors"
	"fmt"
	"go/ast"
	"go/token"
	"io"
	"slices"

	f90 "github.com/soypat/go-fortran/ast"
	f90token "github.com/soypat/go-fortran/token"
)

type ToGo struct {
	scope      *ParserUnitData // currentScope variable data.
	errors     []error
	source     string
	sourceFile io.ReaderAt
}

func (tg *ToGo) SetSource(source string, r io.ReaderAt) {
	tg.source = source
	tg.sourceFile = r
}

func (tg *ToGo) enterProgramUnit(pu f90.ProgramUnit) error {
	data, ok := pu.UnitData().(*ParserUnitData)
	if !ok {
		return errors.New("missing parser unit data")
	}
	tg.scope = data
	tg.scope.vars = slices.Clone(tg.scope.vars)
	for i := range tg.scope.vars {
		tg.scope.vars[i].sname = sanitizeIdent(tg.scope.vars[i].sname)
		tg.scope.vars[i].common = sanitizeIdent(tg.scope.vars[i].common)
		tg.scope.vars[i].pointee = sanitizeIdent(tg.scope.vars[i].pointee)
	}
	return nil
}

func (tg *ToGo) TransformProgram(prog *f90.ProgramBlock) ([]ast.Decl, error) {
	err := tg.enterProgramUnit(prog)
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
	// Start with main function
	decls := []ast.Decl{mainFunc}
	if err != nil {
		return decls, err
	}
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
		// 	funcDecl, err := tg.TransformFunction(c)
		// 	if err != nil {
		// 		return nil, err
		// 	}
		// 	if funcDecl != nil {
		// 		decls = append(decls, funcDecl)
		// 	}
		default:
			return decls, fmt.Errorf("unknown CONTAINS declaration: %T", c)
		}
	}
	return decls, nil
}

func (tg *ToGo) astIdent(name string) *ast.Ident {
	return ast.NewIdent(name)
}

// TransformSubroutine transforms a Fortran SUBROUTINE to a Go function declaration
func (tg *ToGo) TransformSubroutine(sub *f90.Subroutine) (_ *ast.FuncDecl, err error) {
	err = tg.enterProgramUnit(sub)
	if err != nil {
		return nil, err
	}
	fn := &ast.FuncDecl{
		Name: ast.NewIdent(sub.Name),
		Type: &ast.FuncType{
			Params: &ast.FieldList{
				List: []*ast.Field{},
			},
		},
		Body: &ast.BlockStmt{},
	}
	for i := range tg.scope.vars {
		vi := tg.scope.vars[i]
		if vi.flags&flagParameter != 0 {
			tp := tg.fortranTypeToGoWithKind(vi.decl)
			fn.Type.Params.List = append(fn.Type.Params.List, &ast.Field{
				Type:  tp,
				Names: []*ast.Ident{tg.astIdent(vi.decl.Name)},
			})
		}
	}

	fn.Body.List, err = tg.transformStatements(nil, sub.Body)
	if err != nil {
		return fn, err
	}
	return fn, nil
}

func (tg *ToGo) astLabel(f90Label string) *ast.Ident { return ast.NewIdent("label" + f90Label) }

func (tg *ToGo) makeErr(node f90.Node, msg string) error {
	tok := node.AppendTokenLiteral(nil)
	pos := node.SourcePos()

	// If source is available, compute line:column
	if tg.sourceFile != nil {
		var buf [1024]byte
		line, col, _, err := pos.ToLineCol(tg.sourceFile, buf[:])
		if err == nil && line > 0 {
			return fmt.Errorf("%s:%d:%d: %s in %T %s", tg.source, line, col, msg, node, tok)
		}
	}
	return fmt.Errorf("%s in %T %s @ %d", msg, node, tok, pos.Start())
}

func (tg *ToGo) transformStatements(dst []ast.Stmt, stmts []f90.Statement) (_ []ast.Stmt, err error) {
	for _, stmt := range stmts {
		label := stmt.GetLabel()
		if label == "" {
			dst, err = tg.transformStatement(dst, stmt)
		} else {
			var gstmts []ast.Stmt
			gstmts, err = tg.transformStatement(nil, stmt)
			labelled := &ast.LabeledStmt{
				Label: tg.astLabel(label),
				Stmt:  &ast.BlockStmt{List: gstmts},
			}
			dst = append(dst, labelled)
		}
		if err != nil {
			return dst, err
		}
	}
	return dst, nil
}

func (tg *ToGo) transformStatement(dst []ast.Stmt, stmt f90.Statement) (_ []ast.Stmt, err error) {
	switch s := stmt.(type) {
	case *f90.TypeDeclaration:
		dst, err = tg.transformTypeDeclaration(dst, s)
	// case *f90.DerivedTypeStmt:
	// 	gostmt = tg.transformDerivedType(s)
	case *f90.AssignmentStmt:
		dst, err = tg.transformAssignment(dst, s)
	// case *f90.PrintStmt:
	// 	gostmt = tg.transformPrint(s)
	// case *f90.IfStmt:
	// 	gostmt = tg.transformIfStmt(s)
	// case *f90.DoLoop:
	// 	gostmt = tg.transformDoLoop(s)
	// case *f90.CallStmt:
	// 	gostmt = tg.transformCallStmt(s)
	case *f90.ReturnStmt:
		// RETURN statement in functions will be handled by convertFunctionResultToReturn
		// For now, just generate empty return (will be filled with result value later)
		// gostmt = &ast.ReturnStmt{}
	case *f90.CycleStmt:
		// CYCLE → continue
		// gostmt = &ast.BranchStmt{
		// 	Tok: token.CONTINUE,
		// }
	case *f90.ExitStmt:
		// EXIT → break
		// gostmt = &ast.BranchStmt{
		// 	Tok: token.BREAK,
		// }
	case *f90.ContinueStmt:
		// CONTINUE → empty statement (no-op in Go)
		// If there's a label, it will be handled by label processing later
		// gostmt = &ast.EmptyStmt{}
	case *f90.GotoStmt:
		// GOTO label → goto labelN
		// gostmt = &ast.BranchStmt{
		// 	Tok:   token.GOTO,
		// 	Label: ast.NewIdent("label" + s.Target),
		// }
	case *f90.AllocateStmt:
		// gostmt = tg.transformAllocateStmt(s)
	case *f90.DeallocateStmt:
		// gostmt = tg.transformDeallocateStmt(s)
	case *f90.SelectCaseStmt:
		// gostmt = tg.transformSelectCaseStmt(s)
	case *f90.CommonStmt:
		// COMMON blocks are processed separately, no code generation in function body
		return nil, nil
	case *f90.DimensionStmt:
		// DIMENSION statements are processed in preScanCommonBlocks, no code generation in function body
		return nil, nil
	case *f90.EquivalenceStmt:
		// EQUIVALENCE statements are processed in preScanEquivalences, no code generation in function body
		return nil, nil
	case *f90.PointerCrayStmt:
		// Cray-style POINTER statements are processed in preScanCommonBlocks, no code generation in function body
		return nil, nil
	case *f90.DataStmt:
		// gostmt = tg.transformDataStmt(s)
	case *f90.ArithmeticIfStmt:
		// gostmt = tg.transformArithmeticIfStmt(s)
	case *f90.ComputedGotoStmt:
		// gostmt = tg.transformComputedGotoStmt(s)
	case *f90.StopStmt:
		// gostmt = tg.transformStopStmt(s)
	case *f90.WriteStmt:
		// gostmt = tg.transformWriteStmt(s)
	case *f90.FormatStmt:
		// FORMAT statements are compile-time format definitions, no runtime code
		return nil, nil
	case *f90.OpenStmt, *f90.CloseStmt, *f90.ReadStmt, *f90.BackspaceStmt, *f90.RewindStmt, *f90.EndfileStmt, *f90.InquireStmt:
		// File I/O statements - not yet implemented, skip silently
		return nil, nil
	case *f90.EntryStmt:
		// ENTRY statements (multiple entry points) - not supported
		return nil, nil
	case *f90.AssignStmt:
		// ASSIGN label TO variable (Fortran 77 feature) - not supported, skip silently
		return nil, nil
	case *f90.AssignedGotoStmt:
		// GOTO variable (assigned GOTO using label from ASSIGN statement) - not supported
		return nil, nil
	case *f90.ImplicitStatement, *f90.UseStatement, *f90.ExternalStmt, *f90.IntrinsicStmt:
		// Specification statement - no code generation (intentionally nil)
		return nil, nil
	default:
		// For now, unsupported statements are skipped
		// err = fmt.Errorf("unsupported transpile statement: %T", s)
	}
	return dst, err
}

// fortranTypeToGoWithKind converts Fortran type to Go type, considering KIND parameter
// KIND mappings:
//
//	INTEGER(KIND=1) → int8, INTEGER(KIND=2) → int16
//	INTEGER(KIND=4) → int32, INTEGER(KIND=8) → int64
//	REAL(KIND=4) → float32, REAL(KIND=8) → float64
func (tg *ToGo) fortranTypeToGoWithKind(decl *f90.DeclEntity) (goType ast.Expr) {
	// Extract KIND value if present
	var kindValue int64
	if decl == nil {
		panic("nil declaration")
	}
	ft := decl.Type
	if kind, ok := ft.KindOrLen.(*f90.IntegerLiteral); ok {
		kindValue = kind.Value
	}
	switch ft.Token {
	default:
		// Unknown type - skip with warning rather than fail
		tokenStr := ft.Token.String()
		if tokenStr != "<undefined>" && tokenStr != "TYPE" {
			// tg.addError(fmt.Sprintf("unable to resolve token as go type: %s", tokenStr))
		}
		return nil // Skip this declaration
	case f90token.TYPE:
		// User-defined TYPE - use the type name as the Go struct name
		if ft.Name != "" {
			return ast.NewIdent(ft.Name)
		}
		return nil // TYPE without name - skip
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
	case f90token.REAL:
		switch kindValue {
		case 8:
			goType = ast.NewIdent("float64")
		default: // 4 or unspecified
			goType = ast.NewIdent("float32")
		}
	case f90token.DOUBLEPRECISION:
		goType = ast.NewIdent("float64")
	case f90token.LOGICAL:
		goType = ast.NewIdent("bool")
	case f90token.CHARACTER:
		// CHARACTER(LEN=n) maps to intrinsic.CharacterArray
		goType = _astTypeCharArray
		// CHARACTER(LEN=n) length is specified per-entity in entity.CharLen
		// We'll handle initialization per-entity below
	}
	return goType
}

func (tg *ToGo) transformTypeDeclaration(dst []ast.Stmt, stmt *f90.TypeDeclaration) (_ []ast.Stmt, err error) {
	decl := &ast.GenDecl{
		Tok:   token.VAR,
		Specs: make([]ast.Spec, 0, len(stmt.Entities)),
	}
	var useSpecs ast.ValueSpec
	nouse := tg.astIdent("_")
	for i := range stmt.Entities {
		ent := &stmt.Entities[i]
		vi := tg.scope.Var(ent.Name)
		if vi.decl == nil {
			fmt.Printf("nil declaration, skipping %s\n", ent.Name)
			continue
		}
		tp := tg.fortranTypeToGoWithKind(vi.decl)
		ident := ast.NewIdent(ent.Name)
		spec := &ast.ValueSpec{
			Names: []*ast.Ident{ident},
			Type:  tp,
		}
		decl.Specs = append(decl.Specs, spec)
		useSpecs.Names = append(useSpecs.Names, nouse)
		// TODO: check usage flag.
		useSpecs.Values = append(useSpecs.Values, ident)
	}
	decl.Specs = append(decl.Specs, &useSpecs)
	dst = append(dst, &ast.DeclStmt{
		Decl: decl,
	})
	return dst, nil
}

func (tg *ToGo) transformAssignment(dst []ast.Stmt, stmt *f90.AssignmentStmt) (_ []ast.Stmt, err error) {
	var targetVinfo *varinfo
	var lhs ast.Expr
	switch tgt := stmt.Target.(type) {
	case *f90.ArrayRef:
		targetVinfo = tg.scope.Var(tgt.Name)
	case *f90.Identifier:
		targetVinfo = tg.scope.Var(tgt.Value)
		lhs = tg.astIdent(tgt.Value)
	case *f90.FunctionCall:
		// FunctionCall as assignment target occurs for CHARACTER substring: str(1:5) = 'x'
		targetVinfo = tg.scope.Var(tgt.Name)
	default:
		err = tg.makeErr(tgt, "unknown target expression in assignment")
	}
	if err != nil {
		return nil, err
	} else if targetVinfo == nil {
		return nil, tg.makeErr(stmt.Target, "unknown identifier in target expression of assignment")
	}
	rhs, err := tg.transformExpression(targetVinfo.decl, stmt.Value)
	if err != nil {
		return dst, err
	}
	switch tgt := stmt.Target.(type) {
	case *f90.ArrayRef:
		return tg.transformSetArrayRef(dst, tgt, rhs)
	default:
		if lhs == nil {
			return dst, tg.makeErr(stmt.Target, "unsupported assignment target")
		}
	}

	gstmt := &ast.AssignStmt{
		Rhs: []ast.Expr{rhs},
		Lhs: []ast.Expr{lhs},
	}
	dst = append(dst, gstmt)
	return dst, nil
}

func (tg *ToGo) transformSetArrayRef(dst []ast.Stmt, fexpr *f90.ArrayRef, setVal ast.Expr) (_ []ast.Stmt, err error) {
	// vinfo := tg.scope.Var(fexpr.Name)
	// decl := vinfo.decl
	// dim := decl.Dimension()
	// var kindOrCharlen f90.Expression
	// tp := decl.Type.Token
	// if tp == f90token.CHARACTER {
	// 	kindOrCharlen = decl.Charlen()
	// } else {
	// 	kindOrCharlen = decl.Kind()
	// }
	var args []ast.Expr = []ast.Expr{setVal}
	for _, expr := range fexpr.Subscripts {
		result, err := tg.transformExpression(_astTgtInt, expr)
		if err != nil {
			return dst, err
		}
		args = append(args, result)
	}
	gstmt := &ast.ExprStmt{
		X: tg.astMethodCall(fexpr.Name, "Set", setVal),
	}
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

var (
	_astTgtInt = &f90.DeclEntity{}
)
