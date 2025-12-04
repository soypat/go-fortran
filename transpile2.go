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
		vi := params[i]
		tp := tg.fortranTypeToGoWithKind(vi.decl)
		dst = append(dst, &ast.Field{
			Type:  tp,
			Names: []*ast.Ident{tg.astIdent(vi.decl.Name)},
		})
	}
	return dst
}

func (tg *ToGo) getReturnParam() *ast.Field {
	ret := tg.repl.scope.returnType
	tp := tg.fortranTypeToGoWithKind(ret.decl)
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
	// 	gostmt = tg.transformPrint(s)
	case *f90.IfStmt:
	// 	gostmt = tg.transformIfStmt(s)
	case *f90.DoLoop:
	// 	gostmt = tg.transformDoLoop(s)

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
		err = tg.makeErr(s, "unsupported transpile statement")
	}
	return dst, err
}

func (tg *ToGo) makeArrayInitializer(typ *varinfo, initializer ast.Expr) (ast.Expr, error) {
	dims := typ.Dimensions()
	if dims == nil || len(dims.Bounds) == 0 {
		return nil, tg.makeErrWithPos(typ.decl.Position, "invalid type declaration dimensions for array creation")
	} else if dims.IsDeferred() {
		return nil, tg.makeErrWithPos(typ.decl.Position, "cannot create array from deferred shape")
	}
	args := []ast.Expr{initializer} // First argument is array initializer
	for _, bound := range dims.Bounds {
		size, err := tg.transformExpression(_tgtInt32, bound.Upper)
		if err != nil {
			return nil, tg.makeErrWithPos(typ.decl.Position, "unable to make array: "+err.Error())
		}
		args = append(args, size)
	}
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
		if vi.IsParameter() {
			continue // Skip parameters, they're already declared in function signature
		}
		arrspec := vi.Dimensions()
		tp := tg.fortranTypeToGoWithKind(vi.decl)
		if arrspec != nil {
			tp = &ast.StarExpr{X: tp}
		}
		ident := ast.NewIdent(vi.Identifier())
		spec := &ast.ValueSpec{
			Names: []*ast.Ident{ident},
			Type:  tp,
		}
		decl.Specs = append(decl.Specs, spec)
		useSpecs.Names = append(useSpecs.Names, nouse)
		// TODO: check usage flag.
		useSpecs.Values = append(useSpecs.Values, ident)
		// Generate array initialization for arrays with fixed dimensions
		// Skip allocatable arrays - they're initialized by ALLOCATE statements

		if arrspec != nil && len(arrspec.Bounds) > 0 && !vi.IsAllocatable() {
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
		goexpr, err := tg.transformExpression(info, stmt.Args[i])
		if err != nil {
			return dst, err
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
	rhs, err := tg.transformExpression(targetVinfo, stmt.Value)
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
	rhs = tg.wrapConversion(targetVinfo, &rhsType, rhs)
	gstmt := &ast.AssignStmt{
		Tok: token.ASSIGN,
		Rhs: []ast.Expr{rhs},
		Lhs: []ast.Expr{lhs},
	}
	dst = append(dst, gstmt)
	return dst, nil
}

// fortranTypeToGoWithKind converts Fortran type to Go type, considering KIND parameter
// KIND mappings:
//
//	INTEGER(KIND=1) → int8, INTEGER(KIND=2) → int16
//	INTEGER(KIND=4) → int32, INTEGER(KIND=8) → int64
//	REAL(KIND=4) → float32, REAL(KIND=8) → float64
func (tg *ToGo) fortranTypeToGoWithKind(decl *f90.DeclEntity) (goType ast.Expr) {
	kindValue := tg.resolveKindFromDecl(decl)
	baseType := tg.baseGotype(decl.Type.Token, kindValue)
	dim := decl.Dimension()
	if baseType != nil && dim == nil {
		return baseType
	}
	ft := decl.Type
	switch ft.Token {
	case f90token.INTEGER, f90token.REAL:
		goType = &ast.IndexExpr{
			X:     _astTypeArray,
			Index: baseType,
		}
	case f90token.CHARACTER:
		goType = _astTypeCharArray
	case f90token.TYPE:
		if ft.Name != "" {
			goType = ast.NewIdent(ft.Name)
		}
	}
	if goType == nil {
		tg.makeErrWithPos(decl.SourcePos(), "unable to determine go type: "+decl.Name)
		goType = ast.NewIdent("")
	}
	return goType
}

func (tg *ToGo) goType(v *varinfo) ast.Expr {
	dim := v.Dimensions()
	if dim == nil || len(dim.Bounds) == 0 {
		goType := tg.baseGotype(v.typeToken(), tg.resolveKind(v))
		if goType == nil {
			tg.makeErrWithPos(v.decl.Position, "unable to determine goType from: "+v.decl.Name)
			return nil
		}
		return goType
	}
	return intrinsicSelGeneric("Array")(v)

}

func (tg *ToGo) baseGotype(tok f90token.Token, kindValue int) (goType *ast.Ident) {
	switch tok {
	default:
		// Unknown type - skip with warning rather than fail
		tokenStr := tok.String()
		if tokenStr != "<undefined>" && tokenStr != "TYPE" {
			// tg.addError(fmt.Sprintf("unable to resolve token as go type: %s", tokenStr))
		}
		return nil // Skip this declaration
	case f90token.TYPE:
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
	}
	return goType
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
)

// sanitizeIdent returns a valid Go identifier by capitalizing the first letter if it's a Go keyword
func sanitizeIdent(name string) string {
	if name == "" {
		return name
	}
	// Check if it's a Go keyword (case-insensitive since Fortran is case-insensitive)
	if token.IsKeyword(name) {
		// Capitalize first letter
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

			arrspec := v.Dimensions()
			if arrspec != nil && len(arrspec.Bounds) > 0 {
				// Arrays need pointer type since NewArray returns *Array[T]
				goType = &ast.StarExpr{X: goType}
			}
			structType.Fields.List = append(structType.Fields.List, &ast.Field{
				Names: []*ast.Ident{fieldIdent},
				Type:  goType,
			})

			if arrspec == nil || len(arrspec.Bounds) == 0 {
				continue
			}
			args := []ast.Expr{ast.NewIdent("nil")} // First argument is array initializer, we always initialize to zeroes.
			for _, bound := range arrspec.Bounds {
				if bound.Upper != nil {
					size, err := tg.transformExpression(nil, bound.Upper)
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
