package fortran

import (
	"errors"
	"fmt"
	"go/ast"
	"go/token"
	"io"
	"strconv"
	"strings"

	f90 "github.com/soypat/go-fortran/ast"
	"github.com/soypat/go-fortran/intrinsic/fortio"
	f90token "github.com/soypat/go-fortran/token"
)

type ToGo struct {
	repl           REPL
	containedStack []f90.Unit
	source         string
	sourceFile     io.ReaderAt
	currentNode    f90.Node
	globalCommon   string
}

// findIOSpecifier finds a specifier by name in a []f90.IOSpecifier slice (case-insensitive).
func findIOSpecifier(specs []f90.IOSpecifier, name string) f90.Expression {
	for _, spec := range specs {
		if strings.EqualFold(spec.Name, name) {
			return spec.Value
		}
	}
	return nil
}

func (tg *ToGo) Reset() {
	*tg = ToGo{
		repl: tg.repl,
	}
	tg.repl.Reset()
}

func (tg *ToGo) SetSource(source string, r io.ReaderAt) {
	tg.source = source
	tg.sourceFile = r
}

func (tg *ToGo) SetDeferredSource(source string) {
	tg.SetSource(source, nil)
}

func (tg *ToGo) Contained(name string) *ParserUnitData {
	for i := len(tg.containedStack) - 1; i >= 0; i-- {
		if strings.EqualFold(tg.containedStack[i].Name, name) {
			return tg.containedStack[i].Data.(*ParserUnitData)
		}
	}
	return tg.repl.Contained(name)
}

func (tg *ToGo) ContainedOrUsed(name string) *ParserUnitData {
	data := tg.Contained(name)
	if data == nil {
		data = tg.repl.GetUsed(name)
	}
	return data
}

func (tg *ToGo) ImportDecl() ast.Decl {
	return &ast.GenDecl{
		Tok: token.IMPORT,
		Specs: []ast.Spec{
			&ast.ImportSpec{
				Path: &ast.BasicLit{Value: fmt.Sprintf("%q", "github.com/soypat/go-fortran/intrinsic")},
			},
			&ast.ImportSpec{
				Path: &ast.BasicLit{Value: fmt.Sprintf("%q", "github.com/soypat/go-fortran/intrinsic/fortio")},
			},
		},
	}
}

func (tg *ToGo) TransformUnits(dst []ast.Decl, units ...f90.Unit) (_ []ast.Decl, err error) {
	if tg.globalCommon == "" {
		tg.globalCommon = "Global_Common"
	}
	origLen := len(tg.containedStack)
	tg.containedStack = append(tg.containedStack, units...)
	defer func() {
		tg.containedStack = tg.containedStack[:origLen]
	}()
	err = tg.repl.RegisterUnits(units...)
	if err != nil {
		return dst, err
	}
	for i := range units {
		unit := &units[i]
		if !unit.IsValid() {
			return dst, errors.New("invalid program unit")
		}
		data, ok := unit.Data.(*ParserUnitData)
		if !ok {
			return dst, fmt.Errorf("program unit does not have compatible data: %T", unit.Data)
		}
		tg.SetDeferredSource(data.source)
		tg.currentNode = unit
		var fn *ast.FuncDecl
		var results *ast.FieldList
		err = tg.repl.SetScope(*unit)
		if err != nil {
			return dst, fmt.Errorf("setting scope for %s: %w", unit.Name, err)
		}
		switch unit.Token {
		default:
			panic("unsupported unit token: " + unit.Token.String())
		case f90token.FUNCTION:
			field := tg.getReturnParam()
			if field == nil {
				return dst, fmt.Errorf("failed to acquire return parameter type for FUNCTION %s", unit.Name)
			}
			results = &ast.FieldList{List: []*ast.Field{field}}
			fallthrough
		case f90token.SUBROUTINE, f90token.PROGRAM:
			fn = &ast.FuncDecl{
				Name: ast.NewIdent(unit.Name),
				Type: &ast.FuncType{
					Params:  &ast.FieldList{List: tg.getScopeParams(nil)},
					Results: results,
				},
				Body: &ast.BlockStmt{},
			}
			fn.Body.List, err = tg.transformImplicitTypeDeclarations(nil)
			if err != nil {
				return dst, fmt.Errorf("transforming implicit type declarations of %s: %w", unit.Name, err)
			}
			fn.Body.List, err = tg.transformStatements(fn.Body.List, unit.Body)
			if err != nil {
				return dst, tg.makeErrAtStmt("transforming statements of unit " + unit.Name + ": " + err.Error())
			}
			if results != nil {
				fn.Body.List = append(fn.Body.List, &ast.ReturnStmt{}) // FUNCTION has return value.
			}
			dst = append(dst, fn)
			if unit.Token == f90token.SUBROUTINE || unit.Token == f90token.FUNCTION {
				break // No contains statements, continue.
			}
			fallthrough
		case f90token.MODULE:
			// CONTAINS
			dst, err = tg.TransformUnits(dst, unit.Contains...)
			if err != nil {
				return dst, fmt.Errorf("transforming CONTAINS of %s: %w", unit.Name, err)
			}
		case f90token.BLOCK:
			// TODO: support.
		}
	}
	return dst, nil
}

func (tg *ToGo) astIdent(name string) *ast.Ident {
	return ast.NewIdent(name)
}

func (tg *ToGo) getScopeParams(dst []*ast.Field) []*ast.Field {
	params := tg.repl.ScopeParams()
	for i := range params {
		vi := &params[i]
		if vi.decl.Name == "" || vi.decl.Name == "*" {
			warn(vi.declPos.String() + " skip alternate parameters")
			continue // skip alternate return parameters (*)
		}
		tp := tg.goType(vi)
		// For INTENT(OUT) or INTENT(INOUT) non-array scalars, use pointer type
		intent := vi.decl.Type.Intent()
		isArray := vi.IsArray()
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

// isGoPointer returns true if vi is a scalar INTENT(OUT/INOUT) parameter
// that becomes a Go pointer (*T) and needs dereferencing when used as a value.
// This is distinct from Varinfo.IsPointer() which handles Fortran-level pointers.
func (tg *ToGo) isGoPointer(vi *Varinfo) bool {
	if vi == nil || vi.decl == nil {
		return false
	}
	return !vi.IsArray() && vi.flags.HasAny(VFlagIntentOut)
}

func (tg *ToGo) transformStatements(dst []ast.Stmt, stmts []f90.Statement) (_ []ast.Stmt, err error) {
	for _, stmt := range stmts {
		label := stmt.GetLabel()
		if label == nil || *label == "" {
			dst, err = tg.transformStatement(dst, stmt)
		} else {
			var gstmts []ast.Stmt
			gstmts, err = tg.transformStatement(nil, stmt)
			lab := tg.astLabel(*label)
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
	reachedEnd := false
	defer func() {
		if !reachedEnd {
			fmt.Println(tg.makeErrAtStmt("detected panic"))
		}
	}()
	if stmt != nil {
		tg.currentNode = stmt
	}
	switch s := stmt.(type) {
	case *f90.TypeDeclaration:
		dst, err = tg.transformTypeDeclaration(dst, s)
	case *f90.DerivedTypeStmt:
		dst, err = tg.transformDerivedType(dst, s)
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
		dst, err = tg.transformAllocateStmt(dst, s)
	case *f90.DeallocateStmt:
		dst, err = tg.transformDeallocateStmt(dst, s)
	case *f90.SelectCaseStmt:
		dst, err = tg.transformSelectCaseStmt(dst, s)
	case *f90.CommonStmt:
		dst, err = tg.transformCommonStmt(dst, s)
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
		if s.Code != nil {
			code, _, err = tg.transformExpression(_tgtInt, s.Code)
		} else {
			code = _astZero
		}
		dst = append(dst, &ast.ExprStmt{X: &ast.CallExpr{Fun: _astFenvStop, Args: []ast.Expr{code}}})
	case *f90.ParameterStmt:
		dst, err = tg.transformParameterStmt(dst, s)
	case *f90.WriteStmt:
		dst, err = tg.transformWriteStmt(dst, s)
	case *f90.FormatStmt:
		// FORMAT statements are compile-time format definitions, no runtime code
	case *f90.OpenStmt:
		dst, err = tg.transformOpenStmt(dst, s)
	case *f90.CloseStmt:
		dst, err = tg.transformCloseStmt(dst, s)
	case *f90.ReadStmt:
		dst, err = tg.transformReadStmt(dst, s)
		// dst, err = tg.transformReadStmt(dst, s)
	case *f90.BackspaceStmt, *f90.RewindStmt, *f90.EndfileStmt, *f90.InquireStmt:
		// File I/O statements - not yet implemented, skip silently
	case *f90.EntryStmt:
		// ENTRY statements (multiple entry points) - not supported
	case *f90.AssignStmt:
		// ASSIGN label TO variable (Fortran 77 feature) - not supported, skip silently
	case *f90.AssignedGotoStmt:
		// GOTO variable (assigned GOTO using label from ASSIGN statement) - not supported
	case *f90.UseStatement:
		// USE statement - load module into scope.
		err = tg.repl.Use(s.ModuleName, s.Only...)
		if err != nil {
			err = tg.makeErr(stmt, err.Error())
		}
	case *f90.ImplicitStatement, *f90.ExternalStmt, *f90.IntrinsicStmt, *f90.NamelistStmt:
		// Specification statement - no code generation
	default:
		// For now, unsupported statements are skipped
		err = tg.makeErr(s, "unsupported transpile statement")
	}
	reachedEnd = true
	return dst, err
}

func (tg *ToGo) makeArrayInitializer(typ *Varinfo, initializer ast.Expr) (ast.Expr, error) {
	if !typ.IsArray() {
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

	// CHARACTER arrays need special initialization with charlen
	if typ.typeToken() == f90token.CHARACTER {
		// Get charlen (default to 1)
		var charlenExpr ast.Expr = _astOne
		if charLen := typ.Charlen(); charLen != nil {
			var err error
			charlenExpr, _, err = tg.transformExpression(_tgtInt, charLen)
			if err != nil {
				return nil, tg.makeErrWithPos(typ.decl.Position, "unable to get character length: "+err.Error())
			}
		}
		// Generate: intrinsic.NewCharacterArrayArray(charlen, dims...)
		charArgs := append([]ast.Expr{charlenExpr}, args[1:]...) // Skip nil initializer
		return &ast.CallExpr{
			Fun:  &ast.SelectorExpr{X: _astIntrinsic, Sel: ast.NewIdent("NewCharacterArrayArray")},
			Args: charArgs,
		}, nil
	}

	// Get element type for non-CHARACTER arrays
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

func (tg *ToGo) transformImplicitTypeDeclarations(dst []ast.Stmt) (_ []ast.Stmt, err error) {
	implicitDecl := &ast.GenDecl{
		Doc:   &ast.CommentGroup{List: []*ast.Comment{{Text: "\n// Implicit declarations."}}},
		Tok:   token.VAR,
		Specs: make([]ast.Spec, 0, 10),
	}
	var useSpecs ast.ValueSpec // _ = var1, var2, ... to avoid unused variable errors
	nouse := tg.astIdent("_")
	for i := range tg.repl.scope.vars {
		v := &tg.repl.scope.vars[i]
		// Only process truly implicit variables
		if !v.flags.HasAny(VFlagImplicit) ||
			v.flags.HasAny(VFlagParameter|VFlagReturned| // Skip function parameters and return values - they're in function signature
				VFlagPointee| // Skip pointees - they're accessed through their pointer
				VFlagConstantParameter) { // Skip PARAMETER constants - they're handled as const by explicit decl{
			continue
		}
		spec, err := tg.transformTypeDeclEntity(v.decl)
		if err != nil {
			return dst, err
		}
		implicitDecl.Specs = append(implicitDecl.Specs, spec)
		useSpecs.Names = append(useSpecs.Names, nouse)
		useSpecs.Values = append(useSpecs.Values, spec.Names[0])
	}
	if len(implicitDecl.Specs) != 0 {
		// implicitDecl.Specs = append(implicitDecl.Specs, &)
		dst = append(dst, &ast.DeclStmt{Decl: implicitDecl})
		dst = append(dst, &ast.DeclStmt{Decl: &ast.GenDecl{
			Tok:   token.VAR,
			Specs: []ast.Spec{&useSpecs},
		}})
	}
	return dst, nil
}

func (tg *ToGo) transformTypeDeclaration(dst []ast.Stmt, stmt *f90.TypeDeclaration) (_ []ast.Stmt, err error) {
	decl := &ast.GenDecl{
		Tok:   token.VAR,
		Specs: make([]ast.Spec, 0, len(stmt.Entities)),
	}
	var useSpecs ast.ValueSpec // _ = var1, var2, ... to avoid unused variable errors
	nouse := tg.astIdent("_")
	for i := range stmt.Entities {
		ent := &stmt.Entities[i]
		vi := tg.repl.Var(ent.Name)
		if vi.flags.HasAny(VFlagParameter | VFlagImplicit | VFlagCommon) {
			continue // VFlagParameter: function arguments declared in signature. VFlagImplicit: declared in implicit section. VFlagCommon: declared in COMMON handling.
		}
		spec, err := tg.transformTypeDeclEntity(ent)
		if err != nil {
			return dst, err
		}
		decl.Specs = append(decl.Specs, spec)
		useSpecs.Names = append(useSpecs.Names, nouse)
		useSpecs.Values = append(useSpecs.Values, spec.Names[0])
	}
	if len(decl.Specs) == 0 {
		return dst, nil // No local variables to declare
	}
	decl.Specs = append(decl.Specs, &useSpecs)
	dst = append(dst, &ast.DeclStmt{Decl: decl})
	return dst, nil
}

func (tg *ToGo) transformTypeDeclEntity(ent *f90.DeclEntity) (spec *ast.ValueSpec, err error) {
	vi := tg.repl.Var(ent.Name)
	// Check if this is a PARAMETER constant (compile-time constant)
	tp := tg.goType(vi)
	ident := ast.NewIdent(vi.Identifier())
	spec = &ast.ValueSpec{
		Names: []*ast.Ident{ident},
		Type:  tp,
	}
	isArray := vi.IsArray()
	isChar := vi.IsChar()
	isAlloc := vi.IsAllocatable()
	var initExpr ast.Expr
	switch {
	case isChar:
		var lenExpr ast.Expr = _astOne
		if charLen := ent.Charlen(); charLen != nil {
			// Check for assumed-length character (*) - use default length
			if charIdent, ok := charLen.(*f90.Identifier); ok && charIdent.Value == "*" {
				// Assumed-length: keep default length 1
			} else {
				lenExpr, _, err = tg.transformExpression(_tgtInt, charLen)
			}
		}
		initExpr = &ast.CallExpr{
			Fun:  _astFnNewCharArray,
			Args: []ast.Expr{lenExpr},
		}
	case ent.Init != nil:
		// PARAMETER constants or non-array initializers.
		// Skip direct initialization for COMMON scalars - they use PointerTo[T] and need .Set()
		if !isArray && vi.flags.HasAny(VFlagCommon) {
			// COMMON scalar variables are initialized after DeclareCommon
			break
		}
		initExpr, _, err = tg.transformExpression(vi, ent.Init)
	case isArray && !isAlloc:
		spec.Type = nil // Cleaner.
		initExpr, err = tg.makeArrayInitializer(vi, ast.NewIdent("nil"))
	case isArray && isAlloc:
		spec.Type = nil // Cleaner.
		initExpr = &ast.CallExpr{
			Fun: ast.NewIdent("new"),
			Args: []ast.Expr{
				&ast.IndexExpr{X: _astTypeArray, Index: tg.baseGotype(vi.typeToken(), tg.resolveKind(vi))},
			},
		}
	}
	if err != nil {
		return nil, err
	}
	if initExpr != nil {
		spec.Values = []ast.Expr{initExpr}
	}
	return spec, nil
}

// transformDerivedType transforms a Fortran TYPE definition into a Go struct type.
// Example:
//
//	TYPE :: person
//	  CHARACTER(LEN=50) :: name
//	  INTEGER :: age
//	END TYPE person
//
// Becomes:
//
//	type person struct {
//	    name *intrinsic.CharacterArray
//	    age  int32
//	}
func (tg *ToGo) transformDerivedType(dst []ast.Stmt, stmt *f90.DerivedTypeStmt) (_ []ast.Stmt, err error) {
	fields := &ast.FieldList{
		List: make([]*ast.Field, 0, len(stmt.Components)),
	}

	for _, comp := range stmt.Components {
		for _, ent := range comp.Components {
			fieldType := tg.componentGoType(&comp.Type, &ent)
			field := &ast.Field{
				Names: []*ast.Ident{ast.NewIdent(ent.Name)},
				Type:  fieldType,
			}
			fields.List = append(fields.List, field)
		}
	}

	typeSpec := &ast.TypeSpec{
		Name: ast.NewIdent(stmt.Name),
		Type: &ast.StructType{
			Fields: fields,
		},
	}

	decl := &ast.GenDecl{
		Tok:   token.TYPE,
		Specs: []ast.Spec{typeSpec},
	}

	dst = append(dst, &ast.DeclStmt{Decl: decl})
	return dst, nil
}

// componentGoType returns the Go type for a derived type component.
func (tg *ToGo) componentGoType(ts *f90.TypeSpec, ent *f90.DeclEntity) ast.Expr {
	// TODO: integrate with goType method likely candidate for simplification.
	tok := ts.Token
	kind := 0
	if kindExpr := ts.Kind(); kindExpr != nil {
		if lit, ok := kindExpr.(*f90.IntegerLiteral); ok {
			kind = int(lit.Value)
		}
	}

	// Handle CHARACTER type
	if tok == f90token.CHARACTER {
		return &ast.StarExpr{X: _astTypeCharArray}
	}

	// Handle arrays
	if ent.ArraySpec != nil && len(ent.ArraySpec.Bounds) > 0 {
		baseType := tg.baseGotype(tok, kind)
		return &ast.StarExpr{
			X: &ast.IndexExpr{
				X:     _astTypeArray,
				Index: baseType,
			},
		}
	}

	return tg.baseGotype(tok, kind)
}

func (tg *ToGo) transformAllocateStmt(dst []ast.Stmt, stmt *f90.AllocateStmt) (_ []ast.Stmt, err error) {
	for _, obj := range stmt.Objects {
		arrRef, ok := obj.(*f90.CallExpr)
		if !ok {
			return dst, tg.makeErr(stmt, "ALLOCATE requires array reference")
		}
		vi := tg.repl.Var(arrRef.Name)
		if vi == nil {
			return dst, tg.makeErr(stmt, "unknown variable: "+arrRef.Name)
		}
		var args []ast.Expr
		for _, sub := range arrRef.Args {
			arg, _, err := tg.transformExpression(_tgtInt, sub)
			if err != nil {
				return dst, err
			}
			args = append(args, arg)
		}
		// Generate: varname.Allocate(dims...)
		call := &ast.CallExpr{
			Fun:  &ast.SelectorExpr{X: tg.astVarExpr(vi), Sel: ast.NewIdent("Allocate")},
			Args: args,
		}
		dst = append(dst, &ast.ExprStmt{X: call})
	}
	return dst, nil
}

func (tg *ToGo) transformDeallocateStmt(dst []ast.Stmt, stmt *f90.DeallocateStmt) (_ []ast.Stmt, err error) {
	for _, obj := range stmt.Objects {
		var varname string
		switch e := obj.(type) {
		case *f90.Identifier:
			varname = e.Value
		case *f90.CallExpr:
			varname = e.Name
		default:
			return dst, tg.makeErr(stmt, "DEALLOCATE requires variable")
		}
		vi := tg.repl.Var(varname)
		if vi == nil {
			return dst, tg.makeErr(stmt, "DEALLOCATE identifier not found: "+varname)
		}
		// Generate: varname.Deallocate()
		call := &ast.CallExpr{
			Fun: &ast.SelectorExpr{X: tg.astVarExpr(vi), Sel: ast.NewIdent("Deallocate")},
		}
		dst = append(dst, &ast.ExprStmt{X: call})
	}
	return dst, nil
}

func (tg *ToGo) transformCallStmt(dst []ast.Stmt, stmt *f90.CallStmt) (_ []ast.Stmt, err error) {
	fninfo := tg.ContainedOrUsed(stmt.Name)
	if fninfo == nil {
		dst, ok, err := tg.transformIntrinsicCallStmt(dst, stmt)
		if ok || err != nil {
			return dst, err
		}
		return dst, tg.makeErr(stmt, "subroutine not found: "+stmt.Name)
	}
	params := fninfo.ProcedureParams()
	// Legacy Fortran allows calling with fewer arguments (undefined behavior but permitted).
	// We handle this by using type inference for excess arguments.
	if len(stmt.Args) > len(params) {
		return dst, tg.makeErr(stmt, fmt.Sprintf("too many args in call (expected %d, got %d)", len(params), len(stmt.Args)))
	}
	gstmt := &ast.CallExpr{
		Fun: tg.astIdent(fninfo.name),
	}
	for i := range stmt.Args {
		var info *Varinfo
		if i < len(params) {
			info = &params[i]
		}
		goexpr, _, err := tg.transformExpression(info, stmt.Args[i])
		if err != nil {
			return dst, err
		}
		// For INTENT(OUT/INOUT) non-array scalar parameters, pass address.
		// Convert .At() to .AtPtr() for array element access.
		if info != nil && info.decl != nil {
			intent := info.decl.Type.Intent()
			if !info.IsArray() && (intent == f90.IntentOut || intent == f90.IntentInOut) {
				goexpr = wrapPointer(goexpr)
			}
		}
		gstmt.Args = append(gstmt.Args, goexpr)
	}
	dst = append(dst, &ast.ExprStmt{
		X: gstmt,
	})
	return dst, nil
}

// transformIntrinsicCallStmt handles CALL statements for intrinsic and vendor
// subroutines marked isEnvSubroutine, emitting fenv.Method(args...).
// Returns (dst, true, err) when the intrinsic was recognised, (dst, false, nil) otherwise.
func (tg *ToGo) transformIntrinsicCallStmt(dst []ast.Stmt, stmt *f90.CallStmt) ([]ast.Stmt, bool, error) {
	var fn *intrinsicFn
	if tok := f90token.LookupIntrinsic(stmt.Name); tok != 0 {
		fn = getIntrinsic(tok)
	}
	if fn == nil {
		if tok := f90token.LookupVendorIntrinsic(stmt.Name); tok != 0 {
			fn = getVendoredIntrinsic(tok)
		}
	}
	if fn == nil || !fn.isEnvSubroutine {
		return dst, false, nil
	}
	call := fn.findBestCall(len(stmt.Args))
	if call == nil {
		return dst, true, tg.makeErr(stmt, "no matching call signature for env subroutine: "+stmt.Name)
	}
	var args []ast.Expr
	for i, argExpr := range stmt.Args {
		var info *Varinfo
		if i < len(call.args) {
			info = call.args[i]
		}
		goexpr, _, err := tg.transformExpression(info, argExpr)
		if err != nil {
			return dst, true, err
		}
		if i < len(call.outArgs) && call.outArgs[i] {
			goexpr = wrapPointer(goexpr)
		}
		args = append(args, goexpr)
	}
	callExpr := &ast.CallExpr{
		Fun:  &ast.SelectorExpr{X: ast.NewIdent("fenv"), Sel: ast.NewIdent(call.methodOrCall)},
		Args: args,
	}
	return append(dst, &ast.ExprStmt{X: callExpr}), true, nil
}

// wrapPointer converts .At() to .AtPtr() or adds & prefix for pointer passing.
func wrapPointer(expr ast.Expr) ast.Expr {
	if callExpr, ok := expr.(*ast.CallExpr); ok {
		if sel, ok := callExpr.Fun.(*ast.SelectorExpr); ok && sel.Sel.Name == "At" {
			sel.Sel = ast.NewIdent("AtPtr")
			return expr
		}
	}
	return &ast.UnaryExpr{Op: token.AND, X: expr}
}

// allIdentifierArgs returns true if all args are simple Identifier nodes.
func allIdentifierArgs(args []f90.Expression) bool {
	for _, arg := range args {
		if _, ok := arg.(*f90.Identifier); !ok {
			return false
		}
	}
	return len(args) > 0 // Must have at least one parameter
}

// defineStatementFunction registers a statement function definition.
// Statement functions are one-line inline functions: FUNCNAME(X, Y) = expr
func (tg *ToGo) defineStatementFunction(call *f90.CallExpr, expr f90.Expression) ([]ast.Stmt, error) {
	params := make([]string, len(call.Args))
	for i, arg := range call.Args {
		params[i] = arg.(*f90.Identifier).Value
	}
	// Get the variable's declaration for type info (may be implicitly typed)
	vi := tg.repl.Var(call.Name)
	var decl *f90.DeclEntity
	if vi != nil {
		decl = vi.decl
	}
	tg.repl.DefineStmtFunc(call.Name, params, expr, decl)
	return nil, nil // No Go code generated at definition site
}

func (tg *ToGo) transformAssignment(dst []ast.Stmt, stmt *f90.AssignmentStmt) (_ []ast.Stmt, err error) {
	var targetVinfo *Varinfo
	var lhs ast.Expr
	var isIdentifier bool
	switch tgt := stmt.Target.(type) {
	case *f90.CallExpr:
		// CallExpr as assignment target: array access, substring, or statement function definition
		targetVinfo = tg.repl.Var(tgt.Name)
		// Check for statement function definition: NAME(args) = expr
		// where NAME is not an array and all args are simple identifiers
		if (targetVinfo == nil || !targetVinfo.IsArray()) && allIdentifierArgs(tgt.Args) {
			return tg.defineStatementFunction(tgt, stmt.Value)
		}
	case *f90.Identifier:
		targetVinfo = tg.repl.Var(tgt.Value)
		isIdentifier = true
	case *f90.ComponentAccess:
		// Component access: p%age = 30 → p.age = 30
		// Get the base variable for type info
		if ident, ok := tgt.Base.(*f90.Identifier); ok {
			targetVinfo = tg.repl.Var(ident.Value)
		}
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
	if isIdentifier {
		lhs = tg.astVarExpr(targetVinfo)
		if tg.isGoPointer(targetVinfo) {
			// Dereference INTENT(OUT/INOUT) non-array scalar parameters
			lhs = &ast.StarExpr{X: lhs}
		}
		if binop, ok := stmt.Value.(*f90.BinaryExpr); ok && binop.Op == f90token.StringConcat {
			return tg.transformStringConcat(dst, targetVinfo._varname, binop)
		}
	}
	rhs, _, err := tg.transformExpression(targetVinfo, stmt.Value)
	if err != nil {
		return dst, err
	}
	// SetFromString only for scalar CHARACTER (identifier target), not array elements
	if targetVinfo.decl.Type.Token == f90token.CHARACTER && isIdentifier {
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

	// Infer RHS type for conversions (needed by ArrayRef/FunctionCall and default path)
	var rhsType Varinfo
	if err := tg.repl.InferType(&rhsType, stmt.Value); err != nil {
		return dst, tg.makeErr(stmt, "inferring type: "+err.Error())
	}

	switch tgt := stmt.Target.(type) {
	case *f90.CallExpr:
		// Check for ranged array binary operation: arr(1:N) = arr(1:N) + other(1:N)
		if f90.IsRanged(tgt.Args...) {
			if binop, ok := stmt.Value.(*f90.BinaryExpr); ok {
				if result, err := tg.transformRangedArrayBinaryOp(dst, tgt, binop, targetVinfo); err == nil {
					return result, nil
				}
				// Fall through to normal handling if pattern doesn't match
			}
		}
		// CallExpr as target: array element access or substring
		rhs = tg.wrapConversion(targetVinfo, &rhsType, rhs)
		return tg.transformSetArrayRef(dst, tgt, rhs)
	case *f90.ComponentAccess:
		// Component access: p%age = 30 → p.age = 30
		// Handle component access directly and return - no type conversion needed
		lhs, _, err = tg.transformComponentAccess(nil, tgt)
		if err != nil {
			return dst, err
		}
		gstmt := &ast.AssignStmt{
			Tok: token.ASSIGN,
			Lhs: []ast.Expr{lhs},
			Rhs: []ast.Expr{rhs},
		}
		dst = append(dst, gstmt)
		return dst, nil
	default:
		if lhs == nil {
			return dst, tg.makeErr(stmt.Target, "unsupported assignment target")
		}
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
	}
	rhs = tg.wrapConversion(targetVinfo, &rhsType, rhs)
	// Handle equivalenced/COMMON scalar assignment: f = value → f.Set(value, 1)
	// CHARACTER types are excluded as they use SetFromString
	isArray := targetVinfo.IsArray()
	isCharacter := targetVinfo.typeToken() == f90token.CHARACTER
	if !isArray && !isCharacter && targetVinfo.flags.HasAny(VFlagEquivalenced|VFlagCommon) {
		dst = append(dst, &ast.ExprStmt{
			X: tg.astSetCall(lhs, rhs, &ast.BasicLit{Kind: token.INT, Value: "1"}),
		})
		return dst, nil
	}

	gstmt := &ast.AssignStmt{
		Tok: token.ASSIGN,
		Lhs: []ast.Expr{lhs},
		Rhs: []ast.Expr{rhs},
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
	var tgt Varinfo
	for _, expr := range stmt.OutputList {
		err = tg.repl.InferType(&tgt, expr)
		if err != nil {
			return dst, tg.makeErr(stmt, err.Error())
		}
		goExpr, tp, err := tg.transformExpression(&tgt, expr)
		if err != nil {
			return dst, err
		}
		if tp.IsPointer() {
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
	// Generate: fenv.Print(args...)
	callExpr := &ast.CallExpr{
		Fun:  _astFenvPrint,
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

	// Handle DO WHILE (no loop variable, condition in Start) or infinite DO
	if stmt.Var == "" {
		var condExpr ast.Expr
		if stmt.Start != nil {
			// DO WHILE: for condition { ... }
			condExpr, _, err = tg.transformExpression(_tgtBool, stmt.Start)
			if err != nil {
				return dst, err
			}
		}
		// If stmt.Start is nil, condExpr remains nil => infinite for { ... }
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

	// If END DO has a label, add it at the end of the body
	// Also add a goto to the label to ensure it's "used" (Go requires labels to be used)
	if stmt.EndLabel != "" {
		label := tg.astLabel(stmt.EndLabel)
		bodyStmts = append(bodyStmts,
			&ast.BranchStmt{Tok: token.GOTO, Label: label},
			&ast.LabeledStmt{Label: label, Stmt: &ast.EmptyStmt{}},
		)
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
	var viSelector Varinfo
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

// dataValueIter iterates through DATA statement values, expanding DataRepeatExpr.
// It provides a flat view of values where repeat specifiers are expanded.
type dataValueIter struct {
	values      []f90.Expression // Original value list from DATA statement
	idx         int              // Current index in values slice
	repeatIdx   int              // Current index within a repeat (0 if not in repeat)
	repeatCount int              // Total repeat count (1 if not a repeat expr)
	currentExpr f90.Expression   // Current expression (inner value if in repeat)
}

func newDataValueIter(values []f90.Expression) *dataValueIter {
	return &dataValueIter{values: values}
}

// Next advances to the next value. Returns false when exhausted.
func (it *dataValueIter) Next() bool {
	// If we're in a repeat and haven't exhausted it, advance within repeat
	if it.repeatIdx < it.repeatCount-1 {
		it.repeatIdx++
		return true
	}

	// Move to next value in the list
	if it.idx >= len(it.values) {
		return false
	}

	valExpr := it.values[it.idx]
	it.idx++
	it.repeatIdx = 0

	// Check if this is a DataRepeatExpr
	if repeatExpr, ok := valExpr.(*f90.DataRepeatExpr); ok {
		if countLit, ok := repeatExpr.Count.(*f90.IntegerLiteral); ok {
			it.repeatCount = int(countLit.Value)
		} else {
			it.repeatCount = 1 // Fallback, error will be caught later
		}
		it.currentExpr = repeatExpr.Value
	} else {
		it.repeatCount = 1
		it.currentExpr = valExpr
	}

	return true
}

// Value returns the current value expression.
func (it *dataValueIter) Value() f90.Expression {
	return it.currentExpr
}

// RawValue returns the current raw value (DataRepeatExpr if applicable).
func (it *dataValueIter) RawValue() f90.Expression {
	if it.idx > 0 && it.idx <= len(it.values) {
		return it.values[it.idx-1]
	}
	return nil
}

// TotalExpanded returns the total number of values when all repeats are expanded.
func (it *dataValueIter) TotalExpanded() int {
	total := 0
	for _, v := range it.values {
		if repeatExpr, ok := v.(*f90.DataRepeatExpr); ok {
			if countLit, ok := repeatExpr.Count.(*f90.IntegerLiteral); ok {
				total += int(countLit.Value)
			} else {
				total++ // Fallback
			}
		} else {
			total++
		}
	}
	return total
}

func (tg *ToGo) transformDataStmt(dst []ast.Stmt, stmt *f90.DataStmt) (_ []ast.Stmt, err error) {
	// DATA statements initialize variables with values:
	// 1. DATA a, b, c / 10, 20, 30 /       - multiple scalars
	// 2. DATA arr / 1, 2, 3, 4, 5 /        - whole array initialization
	// 3. DATA arr(1), arr(2) / 1, 2 /      - specific array elements
	// 4. DATA arr / 10*0.0 /               - repeat specifier (10 zeros)
	// Each Varlist is a var-list / value-list / pair

	for i := range stmt.Varlists {
		dst, err = tg.transformDataVarlist(dst, stmt, &stmt.Varlists[i])
		if err != nil {
			return dst, err
		}
	}

	return dst, nil
}

// transformDataVarlist processes a single var-list / value-list / pair.
func (tg *ToGo) transformDataVarlist(dst []ast.Stmt, stmt *f90.DataStmt, varlist *f90.Varlist) ([]ast.Stmt, error) {
	iter := newDataValueIter(varlist.Values)
	totalValues := iter.TotalExpanded()
	valuesConsumed := 0

	for varIdx, varExpr := range varlist.Variables {
		varsRemaining := len(varlist.Variables) - varIdx
		valuesRemaining := totalValues - valuesConsumed
		var err error
		var consumed int
		dst, consumed, err = tg.transformDataVarInit(dst, stmt, varExpr, iter, valuesRemaining, varsRemaining)
		if err != nil {
			return dst, err
		}
		valuesConsumed += consumed
	}

	return dst, nil
}

// transformDataVarInit initializes a single variable from DATA statement values.
// Returns the number of values consumed.
func (tg *ToGo) transformDataVarInit(dst []ast.Stmt, stmt *f90.DataStmt, varExpr f90.Expression,
	iter *dataValueIter, valuesRemaining, varsRemaining int) ([]ast.Stmt, int, error) {

	// Get target variable info
	var targetVinfo *Varinfo
	var varName string
	var isArrayElement bool

	switch v := varExpr.(type) {
	case *f90.Identifier:
		targetVinfo = tg.repl.Var(v.Value)
		varName = v.Value
	case *f90.CallExpr:
		targetVinfo = tg.repl.Var(v.Name)
		varName = v.Name
		if len(v.Args) > 0 {
			isArrayElement = true
		}
	case *f90.ImpliedDoLoop:
		return tg.transformDataImpliedDo(dst, stmt, v, iter, targetVinfo)
	default:
		return dst, 0, tg.makeErr(stmt, "unsupported DATA statement variable type")
	}

	if targetVinfo == nil {
		return dst, 0, tg.makeErr(stmt, fmt.Sprintf("unknown variable %q in DATA statement", varName))
	}

	if isArrayElement {
		// Specific array element: arr(i,j) - consume one value
		return tg.transformDataArrayElement(dst, stmt, varExpr.(*f90.CallExpr), iter, targetVinfo)
	}

	isArray := targetVinfo.IsArray()
	if !isArray && valuesRemaining <= varsRemaining {
		// Scalar variable - consume one value
		return tg.transformDataScalar(dst, stmt, varExpr, iter, targetVinfo)
	}

	// Array variable - consume multiple values
	return tg.transformDataArray(dst, stmt, varExpr, varName, iter, targetVinfo, valuesRemaining, varsRemaining)
}

// transformDataScalar initializes a scalar variable with one value.
// Returns the number of values consumed (always 1 on success).
func (tg *ToGo) transformDataScalar(dst []ast.Stmt, stmt *f90.DataStmt, varExpr f90.Expression,
	iter *dataValueIter, targetVinfo *Varinfo) ([]ast.Stmt, int, error) {

	if !iter.Next() {
		return dst, 0, tg.makeErr(stmt, "not enough values in DATA statement")
	}

	lhs := tg.astVarExpr(targetVinfo)
	rhs, _, err := tg.transformExpression(targetVinfo, iter.Value())
	if err != nil {
		return dst, 0, err
	}

	dst = append(dst, &ast.AssignStmt{
		Lhs: []ast.Expr{lhs},
		Tok: token.ASSIGN,
		Rhs: []ast.Expr{rhs},
	})
	return dst, 1, nil
}

// transformDataArrayElement initializes a specific array element.
// Returns the number of values consumed (always 1 on success).
func (tg *ToGo) transformDataArrayElement(dst []ast.Stmt, stmt *f90.DataStmt, callExpr *f90.CallExpr,
	iter *dataValueIter, targetVinfo *Varinfo) ([]ast.Stmt, int, error) {

	if !iter.Next() {
		return dst, 0, tg.makeErr(stmt, "not enough values in DATA statement")
	}

	rhs, _, err := tg.transformExpression(targetVinfo, iter.Value())
	if err != nil {
		return dst, 0, err
	}

	dst, err = tg.transformSetArrayRef(dst, callExpr, rhs)
	return dst, 1, err
}

// transformDataArray initializes an array with multiple values.
// Returns the number of values consumed.
func (tg *ToGo) transformDataArray(dst []ast.Stmt, stmt *f90.DataStmt, varExpr f90.Expression,
	varName string, iter *dataValueIter, targetVinfo *Varinfo, valuesRemaining, varsRemaining int) ([]ast.Stmt, int, error) {

	// Calculate how many values this array should consume
	valuesToConsume := valuesRemaining / varsRemaining
	consumed := 0

	for arrayIdx := 0; arrayIdx < valuesToConsume; arrayIdx++ {
		if !iter.Next() {
			break
		}

		rhs, _, err := tg.transformExpression(targetVinfo, iter.Value())
		if err != nil {
			return dst, consumed, err
		}

		// Create array reference with 1-based index (Fortran convention)
		syntheticRef := &f90.CallExpr{
			Name:     varName,
			Args:     []f90.Expression{&f90.IntegerLiteral{Value: int64(arrayIdx + 1)}},
			Position: varExpr.SourcePos(),
		}

		dst, err = tg.transformSetArrayRef(dst, syntheticRef, rhs)
		if err != nil {
			return dst, consumed, err
		}
		consumed++
	}

	return dst, consumed, nil
}

// transformDataImpliedDo handles implied DO loops in DATA statements.
// Returns the number of values consumed.
func (tg *ToGo) transformDataImpliedDo(dst []ast.Stmt, stmt *f90.DataStmt, loop *f90.ImpliedDoLoop,
	iter *dataValueIter, targetVinfo *Varinfo) ([]ast.Stmt, int, error) {
	warn(tg.forceStrPos(stmt.Position) + " claudish transformDataImpliedDo ")
	// Evaluate loop bounds as integer constants.
	var startVI, endVI, strideVI Varinfo
	if err := tg.repl.Eval(&startVI, loop.Start); err != nil {
		return dst, 0, tg.makeErr(stmt, "DATA implied-DO: evaluating start: "+err.Error())
	}
	if err := tg.repl.Eval(&endVI, loop.End); err != nil {
		return dst, 0, tg.makeErr(stmt, "DATA implied-DO: evaluating end: "+err.Error())
	}
	stride := int64(1)
	if loop.Stride != nil {
		if err := tg.repl.Eval(&strideVI, loop.Stride); err != nil {
			return dst, 0, tg.makeErr(stmt, "DATA implied-DO: evaluating stride: "+err.Error())
		}
		stride = strideVI.val.i64
	}
	start := startVI.val.i64
	end := endVI.val.i64
	if stride == 0 {
		stride = 1
	}
	consumed := 0
	for k := start; (stride > 0 && k <= end) || (stride < 0 && k >= end); k += stride {
		// Push loop variable as integer constant into scope.
		loopVar := Varinfo{}
		loopVar.decl = &f90.DeclEntity{Name: loop.LoopVar, Type: &f90.TypeSpec{Token: f90token.INTEGER}}
		loopVar._varname = loop.LoopVar
		if err := tg.repl.assignInt(&loopVar, k); err != nil {
			return dst, consumed, tg.makeErr(stmt, "DATA implied-DO: setting loop var: "+err.Error())
		}
		remove := tg.repl.PushVar(loopVar)
		for _, expr := range loop.Expressions {
			callExpr, ok := expr.(*f90.CallExpr)
			if !ok {
				remove()
				return dst, consumed, tg.makeErr(stmt, "DATA implied-DO: expected array element expression")
			}
			vi := tg.repl.Var(callExpr.Name)
			if vi == nil {
				remove()
				return dst, consumed, tg.makeErr(stmt, "DATA implied-DO: unknown variable "+callExpr.Name)
			}
			var n int
			var err error
			dst, n, err = tg.transformDataArrayElement(dst, stmt, callExpr, iter, vi)
			if err != nil {
				remove()
				return dst, consumed, err
			}
			consumed += n
		}
		remove()
	}
	return dst, consumed, nil
}

func (tg *ToGo) transformParameterStmt(dst []ast.Stmt, stmt *f90.ParameterStmt) ([]ast.Stmt, error) {
	decl := &ast.GenDecl{
		Tok: token.CONST,
	}
	for _, v := range stmt.Decls {
		vi := tg.repl.Var(v.Name)
		if vi == nil {
			return dst, tg.makeErr(stmt, "undeclared parameter: "+v.Name)
		}
		tp := tg.goType(vi)
		initVal, _, err := tg.transformExpression(vi, vi.decl.Init)
		if err != nil {
			return dst, err
		}
		decl.Specs = append(decl.Specs, &ast.ValueSpec{
			Names:  []*ast.Ident{ast.NewIdent(vi.Identifier())},
			Type:   tp,
			Values: []ast.Expr{initVal},
		})
	}
	dst = append(dst, &ast.DeclStmt{Decl: decl})
	return dst, nil
}

func (tg *ToGo) transformArithmeticIfStmt(dst []ast.Stmt, stmt *f90.ArithmeticIfStmt) (_ []ast.Stmt, err error) {
	// Arithmetic IF: IF (x) neg, zero, pos
	// Becomes: if jmpSelect := x; jmpSelect < 0 { goto neg } else if jmpSelect == 0 { goto zero } else { goto pos }
	var condType Varinfo
	if err := tg.repl.InferType(&condType, stmt.Condition); err != nil {
		return dst, err
	}
	condExpr, _, err := tg.transformExpression(&condType, stmt.Condition)
	if err != nil {
		return dst, err
	}

	jmpSelect := ast.NewIdent("jmpSelect")
	zero := _astZero

	// if jmpSelect := condition; jmpSelect < 0 { goto negLabel }
	negIf := &ast.IfStmt{
		Init: &ast.AssignStmt{Lhs: []ast.Expr{jmpSelect}, Tok: token.DEFINE, Rhs: []ast.Expr{condExpr}},
		Cond: &ast.BinaryExpr{X: jmpSelect, Op: token.LSS, Y: zero},
		Body: &ast.BlockStmt{List: []ast.Stmt{
			&ast.BranchStmt{Tok: token.GOTO, Label: tg.astLabel(stmt.NegativeLabel)},
		}},
	}

	// else if jmpSelect == 0 { goto zeroLabel }
	zeroIf := &ast.IfStmt{
		Cond: &ast.BinaryExpr{X: jmpSelect, Op: token.EQL, Y: zero},
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
			isArray := vinfo.IsArray()
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
				isArray := vinfo.IsArray()
				isCharacter := vinfo.typeToken() == f90token.CHARACTER
				isPointerTo := vinfo.IsPointer()

				if len(ref.Args) == 0 {
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
					for _, sub := range ref.Args {
						arg, _, err := tg.transformExpression(_tgtInt, sub)
						if err != nil {
							return dst, err
						}
						offsetArgs = append(offsetArgs, arg)
					}
					var arg ast.Expr
					if isCharacter {
						// CharacterArray implements PointerSetter interface.
						arg = &ast.CallExpr{
							Fun:  &ast.SelectorExpr{X: varExpr, Sel: ast.NewIdent("AtPtr")},
							Args: offsetArgs,
						}
					} else {
						offsetCall := &ast.CallExpr{
							Fun:  &ast.SelectorExpr{X: varExpr, Sel: ast.NewIdent("AtOffset")},
							Args: offsetArgs,
						}
						arg = &ast.CallExpr{
							Fun: &ast.SelectorExpr{
								X:   _astIntrinsic,
								Sel: ast.NewIdent("PointerOff"),
							},
							Args: []ast.Expr{varExpr, offsetCall},
						}

					}
					args[i] = arg
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
func (tg *ToGo) typeSize(v *Varinfo) int {
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
			missing := pair.PointerVar
			if ptrVar != nil {
				missing = pair.Pointee
			}
			return dst, tg.makeErr(stmt, "pointer or pointee variable not found: "+missing)
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
func (tg *ToGo) goType(v *Varinfo) ast.Expr {
	tok := v.typeToken()
	isArray := v.IsArray()
	// Handle Cray-style pointer variables (POINTER (ptr, pointee))
	// The pointer variable's type is PointerTo[pointee_type]
	if v.IsPointer() {
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

	// Handle equivalenced and COMMON scalars - they become PointerTo[T] for memory sharing
	// CHARACTER types are excluded as they have their own memory management (CharacterArray)
	if !isArray && tok != f90token.CHARACTER && v.flags.HasAny(VFlagEquivalenced|VFlagCommon) {
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
	case f90token.IntLit, f90token.INTEGER:
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
	case f90token.DOUBLECOMPLEX:
		goType = ast.NewIdent("complex128")
	case f90token.DOUBLEPRECISION:
		goType = ast.NewIdent("float64")
	case f90token.FloatLit, f90token.REAL:
		switch kindValue {
		case 8:
			goType = ast.NewIdent("float64")
		default: // 4 or unspecified
			goType = ast.NewIdent("float32")
		}
	case f90token.COMPLEX:
		switch kindValue {
		case 8, 16:
			goType = ast.NewIdent("complex128")
		default: // 4 or unspecified
			goType = ast.NewIdent("complex64")
		}
	case f90token.StringLit, f90token.CHARACTER:
		goType = _astTypeCharArray
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
			goexpr, _, err := tg.transformExpression(_tgtStringLit, e)
			if err != nil {
				return dst, tg.makeErr(op, "unsupported expression for string concat: "+err.Error())
			}
			warn(tg.forceStrPos(e.SourcePos()) + " potential unsupported expression for string concat")
			args = append(args, goexpr)
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

// formatSpecsToGoAST converts parsed FormatSpec slice to Go AST expressions.
func formatSpecsToGoAST(specs []f90.FormatSpec) []ast.Expr {
	var exprs []ast.Expr
	for i := range specs {
		exprs = appendFormatSpecToGoAST(exprs, &specs[i])
	}
	return exprs
}

// appendFormatSpecToGoAST converts a single FormatSpec to Go AST expressions.
func appendFormatSpecToGoAST(exprs []ast.Expr, spec *f90.FormatSpec) []ast.Expr {
	// Handle string literal
	if spec.StringLit != "" {
		exprs = append(exprs, &ast.CompositeLit{
			Type: _astFortioFormatDescriptor,
			Elts: []ast.Expr{
				&ast.KeyValueExpr{
					Key:   ast.NewIdent("Type"),
					Value: &ast.BasicLit{Kind: token.CHAR, Value: "'S'"},
				},
				&ast.KeyValueExpr{
					Key:   ast.NewIdent("Literal"),
					Value: &ast.BasicLit{Kind: token.STRING, Value: strconv.Quote(spec.StringLit)},
				},
			},
		})
		return exprs
	}

	// Handle grouped repeat: 3(I3,F6.2)
	if len(spec.Group) > 0 {
		for range spec.Repeat {
			for i := range spec.Group {
				exprs = appendFormatSpecToGoAST(exprs, &spec.Group[i])
			}
		}
		return exprs
	}

	// Handle newline control /
	if spec.Descriptor[0] == '/' {
		repeat := spec.Repeat
		if repeat == 0 {
			repeat = 1
		}
		for range repeat {
			exprs = append(exprs, _astFortioFmtNewline)
		}
		return exprs
	}

	// Handle control characters with no output (:, $)
	if spec.Descriptor[0] == ':' || spec.Descriptor[0] == '$' {
		// These are control characters, skip for now
		return exprs
	}

	// Handle regular descriptors
	if spec.Descriptor[0] != 0 {
		typ := spec.Descriptor[0]
		if typ >= 'a' && typ <= 'z' {
			typ -= 32 // uppercase
		}

		elts := []ast.Expr{
			&ast.KeyValueExpr{
				Key:   ast.NewIdent("Type"),
				Value: &ast.BasicLit{Kind: token.CHAR, Value: "'" + string(typ) + "'"},
			},
		}
		if spec.Width > 0 {
			elts = append(elts, &ast.KeyValueExpr{
				Key:   ast.NewIdent("Width"),
				Value: &ast.BasicLit{Kind: token.INT, Value: strconv.Itoa(int(spec.Width))},
			})
		}
		if spec.Decimals > 0 {
			elts = append(elts, &ast.KeyValueExpr{
				Key:   ast.NewIdent("Precision"),
				Value: &ast.BasicLit{Kind: token.INT, Value: strconv.Itoa(int(spec.Decimals))},
			})
		}
		if spec.Repeat > 0 {
			elts = append(elts, &ast.KeyValueExpr{
				Key:   ast.NewIdent("Repeat"),
				Value: &ast.BasicLit{Kind: token.INT, Value: strconv.Itoa(spec.Repeat)},
			})
		}

		exprs = append(exprs, &ast.CompositeLit{
			Type: _astFortioFormatDescriptor,
			Elts: elts,
		})
	}

	return exprs
}

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

// AppendCommonDecls appends COMMON block declarations and fenv to dst.
// Generates: var fenv = fortio.NewEnvironment()
// Generates: var BLK = intrinsic.NewCommonBlock("BLK", totalSize)
// Should be called after all program units have been processed.
func (tg *ToGo) AppendCommonDecls(dst []ast.Decl) []ast.Decl {
	// Generate: var fenv = fortio.NewEnvironment()
	fenvSpec := &ast.ValueSpec{
		Names: []*ast.Ident{ast.NewIdent("fenv")},
		Values: []ast.Expr{
			&ast.CallExpr{Fun: _astFortioNewEnvironment},
		},
	}
	dst = append(dst, &ast.GenDecl{
		Tok:   token.VAR,
		Specs: []ast.Spec{fenvSpec},
	})

	for _, block := range tg.repl.commonblocks {
		if len(block.fields) == 0 {
			continue
		}
		// Handle blank COMMON (no name)
		blockName := block.Name
		if blockName == "" {
			blockName = tg.globalCommon
		}
		blockIdent := ast.NewIdent(blockName)

		// Calculate total size with alignment
		totalSize := tg.calculateCommonBlockSize(&block)

		// Generate: var BLK = intrinsic.NewCommonBlock("BLK", totalSize)
		valueSpec := &ast.ValueSpec{
			Names: []*ast.Ident{blockIdent},
			Values: []ast.Expr{
				&ast.CallExpr{
					Fun: &ast.SelectorExpr{
						X:   ast.NewIdent("intrinsic"),
						Sel: ast.NewIdent("NewCommonBlock"),
					},
					Args: []ast.Expr{
						&ast.BasicLit{Kind: token.STRING, Value: `"` + blockName + `"`},
						&ast.BasicLit{Kind: token.INT, Value: strconv.Itoa(totalSize)},
					},
				},
			},
		}
		dst = append(dst, &ast.GenDecl{
			Tok:   token.VAR,
			Specs: []ast.Spec{valueSpec},
		})
	}

	return dst
}

// calculateCommonBlockSize calculates the total size of a COMMON block in bytes.
// Per Fortran standard, COMMON blocks are packed without padding.
func (tg *ToGo) calculateCommonBlockSize(block *commonBlockInfo) int {
	total := 0
	for i := range block.fields {
		v := &block.fields[i]
		elemSize := tg.typeSize(v)
		numElems := 1
		if v.IsArray() {
			numElems = tg.arrayNumElements(v)
		}
		total += elemSize * numElems
	}
	return total
}

// arrayNumElements returns the total number of elements in an array.
func (tg *ToGo) arrayNumElements(v *Varinfo) int {
	if !v.IsArray() {
		return 1
	}
	arrspec := v.Dimensions()
	total := 1
	for _, bound := range arrspec.Bounds {
		if bound.Upper != nil {
			var dst Varinfo
			if err := tg.repl.Eval(&dst, bound.Upper); err == nil {
				total *= int(dst.val.i64)
			}
		}
	}
	return total
}

// transformCommonStmt generates local variable declarations and DeclareCommon calls
// for COMMON block variables in the current subroutine.
//
// For COMMON /BLK/ d1k, d2k, d3k generates:
//
//	d1k := intrinsic.UnallocatedPtr[float32](1)
//	d2k := intrinsic.UnallocatedPtr[float32](1)
//	d3k := intrinsic.UnallocatedPtr[float32](1)
//	BLK.Reset()
//	intrinsic.DeclareCommon(&d1k, &BLK)
//	intrinsic.DeclareCommon(&d2k, &BLK)
//	intrinsic.DeclareCommon(&d3k, &BLK)
func (tg *ToGo) transformCommonStmt(dst []ast.Stmt, stmt *f90.CommonStmt) (_ []ast.Stmt, err error) {
	// Look up the COMMON block to get the canonical name (case-insensitive match)
	block := tg.repl.getCommon(stmt.BlockName)
	blockName := stmt.BlockName
	if block != nil {
		blockName = block.Name // Use stored name for consistency with AppendCommonDecls
	}
	if blockName == "" {
		blockName = tg.globalCommon
	}
	blockIdent := ast.NewIdent(blockName)

	// Collect variable declarations and DeclareCommon calls
	var declareStmts []ast.Stmt

	for i, varName := range stmt.Variables {
		vi := tg.repl.Var(varName)
		if vi == nil {
			return dst, tg.makeErr(stmt, "unknown variable in COMMON: "+varName)
		}

		varIdent := ast.NewIdent(vi.Identifier())
		elemType := tg.baseGotype(vi.typeToken(), tg.resolveKind(vi))

		var initExpr ast.Expr
		if vi.IsArray() {
			// Array: varname := intrinsic.UnallocatedArray[T](dims...)
			args := []ast.Expr{}
			arrspec := vi.Dimensions()
			if arrspec == nil && i < len(stmt.ArraySpecs) {
				arrspec = stmt.ArraySpecs[i]
			}
			if arrspec != nil {
				for _, bound := range arrspec.Bounds {
					if bound.Upper != nil {
						size, _, err := tg.transformExpression(_tgtInt, bound.Upper)
						if err != nil {
							return dst, err
						}
						args = append(args, size)
					}
				}
			}
			initExpr = &ast.CallExpr{
				Fun: &ast.IndexExpr{
					X: &ast.SelectorExpr{
						X:   ast.NewIdent("intrinsic"),
						Sel: ast.NewIdent("UnallocatedArray"),
					},
					Index: elemType,
				},
				Args: args,
			}
		} else {
			// Scalar: varname := intrinsic.UnallocatedPtr[T](1)
			initExpr = &ast.CallExpr{
				Fun: &ast.IndexExpr{
					X: &ast.SelectorExpr{
						X:   ast.NewIdent("intrinsic"),
						Sel: ast.NewIdent("UnallocatedPtr"),
					},
					Index: elemType,
				},
				Args: []ast.Expr{&ast.BasicLit{Kind: token.INT, Value: "1"}},
			}
		}

		// Generate: varname = intrinsic.Unallocated... or varname := intrinsic.Unallocated...
		// Use ASSIGN (=) for VFlagImplicit vars (already declared in implicit section)
		// Use DEFINE (:=) for vars with explicit type declarations (not yet declared)
		assignTok := token.ASSIGN
		if !vi.flags.HasAny(VFlagImplicit) {
			assignTok = token.DEFINE
		}
		dst = append(dst, &ast.AssignStmt{
			Lhs: []ast.Expr{varIdent},
			Tok: assignTok,
			Rhs: []ast.Expr{initExpr},
		})

		// Generate: intrinsic.DeclareCommon(&varname, &BLK) for scalars
		// or: intrinsic.DeclareCommon(varname, &BLK) for arrays (already pointer)
		var varArg ast.Expr
		if vi.IsArray() {
			varArg = varIdent // *Array[T] already implements PointerSetter
		} else {
			varArg = &ast.UnaryExpr{Op: token.AND, X: varIdent}
		}
		declareStmts = append(declareStmts, &ast.ExprStmt{
			X: &ast.CallExpr{
				Fun: &ast.SelectorExpr{
					X:   ast.NewIdent("intrinsic"),
					Sel: ast.NewIdent("DeclareCommon"),
				},
				Args: []ast.Expr{
					varArg,
					&ast.UnaryExpr{Op: token.AND, X: blockIdent},
				},
			},
		})
	}

	// Generate: BLK.Reset()
	dst = append(dst, &ast.ExprStmt{
		X: &ast.CallExpr{
			Fun: &ast.SelectorExpr{
				X:   blockIdent,
				Sel: ast.NewIdent("Reset"),
			},
		},
	})

	// Append all DeclareCommon calls
	dst = append(dst, declareStmts...)

	// Note: COMMON block variables should NOT be initialized inline.
	// In Fortran, COMMON blocks share memory across program units,
	// so initialization is done via BLOCK DATA subprograms, not inline declarations.
	// Inline initializers in declarations (e.g., REAL :: x = 1.0) are ignored for COMMON variables.

	return dst, nil
}

func (tg *ToGo) resolveKind(v *Varinfo) int {
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
	var dst Varinfo
	if err := tg.repl.Eval(&dst, kind); err != nil {
		return 0
	}
	return int(dst.val.i64)
}

func (tg *ToGo) transformOpenStmt(dst []ast.Stmt, stmt *f90.OpenStmt) ([]ast.Stmt, error) {
	_, errLabel, specs := extractBranchLabels(stmt.Specifiers)
	dst, err := tg.transformIO(dst, _astFortioOpenSpec, _astFenvOpen, specs, nil, false)
	if err != nil || errLabel == "" {
		return dst, err
	}
	return tg.appendIOBranchStmts(dst, "", errLabel)
}
func (tg *ToGo) transformCloseStmt(dst []ast.Stmt, stmt *f90.CloseStmt) ([]ast.Stmt, error) {
	_, errLabel, specs := extractBranchLabels(stmt.Specifiers)
	dst, err := tg.transformIO(dst, _astFortioCloseSpec, _astFenvClose, specs, nil, false)
	if err != nil || errLabel == "" {
		return dst, err
	}
	return tg.appendIOBranchStmts(dst, "", errLabel)
}

func (tg *ToGo) transformWriteStmt(dst []ast.Stmt, stmt *f90.WriteStmt) ([]ast.Stmt, error) {
	// Check for implied DO loops or namelist - fall back to old implementation
	// TODO: eliminate old implementation
	for _, expr := range stmt.OutputList {
		if _, ok := expr.(*f90.ImpliedDoLoop); ok {
			return tg.transformWriteStmtOld(dst, stmt)
		}
	}
	if ident, ok := stmt.Format.(*f90.Identifier); ok && ident.Value != "*" {
		if nml := tg.repl.Namelist(ident.Value); nml != nil {
			return tg.transformWriteNamelist(dst, stmt, nml)
		}
	}
	unit := ioUnitOrDefault(stmt.Unit, 6) // stdout
	specs := append([]f90.IOSpecifier{
		{Name: "UNIT", Value: unit},
		{Name: "FMT", Value: stmt.Format},
	}, stmt.Specifiers...)
	return tg.transformIO(dst, _astFortioIOSpec, _astFenvWriteWithSpec, specs, stmt.OutputList, false)
}

func (tg *ToGo) transformReadStmt(dst []ast.Stmt, stmt *f90.ReadStmt) ([]ast.Stmt, error) {
	// Check for implied DO loops or namelist - fall back to old implementation
	// TODO: eliminate old implementation
	for _, expr := range stmt.InputList {
		if _, ok := expr.(*f90.ImpliedDoLoop); ok {
			return tg.transformReadStmtOld(dst, stmt)
		}
	}
	if ident, ok := stmt.Format.(*f90.Identifier); ok && ident.Value != "*" {
		if nml := tg.repl.Namelist(ident.Value); nml != nil {
			return tg.transformReadNamelist(dst, stmt, nml)
		}
	}
	unit := ioUnitOrDefault(stmt.Unit, 5) // stdin
	endLabel, errLabel, filtered := extractBranchLabels(stmt.Specifiers)
	specs := append([]f90.IOSpecifier{
		{Name: "UNIT", Value: unit},
		{Name: "FMT", Value: stmt.Format},
	}, filtered...)
	var err error
	dst, err = tg.transformIO(dst, _astFortioIOSpec, _astFenvReadWithSpec, specs, stmt.InputList, true)
	if err != nil || (endLabel == "" && errLabel == "") {
		return dst, err
	}
	return tg.appendIOBranchStmts(dst, endLabel, errLabel)
}

// extractBranchLabels splits END= and ERR= out of a specifier list.
// Returns the label strings (empty if absent) and the remaining specifiers.
func extractBranchLabels(specs []f90.IOSpecifier) (endLabel, errLabel string, rest []f90.IOSpecifier) {
	for _, s := range specs {
		switch strings.ToUpper(s.Name) {
		case "END":
			if lit, ok := s.Value.(*f90.IntegerLiteral); ok {
				endLabel = strconv.FormatInt(lit.Value, 10)
			}
		case "ERR":
			if lit, ok := s.Value.(*f90.IntegerLiteral); ok {
				errLabel = strconv.FormatInt(lit.Value, 10)
			}
		default:
			rest = append(rest, s)
		}
	}
	return
}

// appendIOBranchStmts replaces the last ExprStmt (an IO call) with an
// assignment capturing the IOStat return value, followed by conditional
// gotos for END= (EOF) and ERR= (error).
func (tg *ToGo) appendIOBranchStmts(dst []ast.Stmt, endLabel, errLabel string) ([]ast.Stmt, error) {
	last, ok := dst[len(dst)-1].(*ast.ExprStmt)
	if !ok {
		return dst, errors.New("appendIOBranchStmts: last statement is not an ExprStmt")
	}
	dst = dst[:len(dst)-1]
	tmp := ast.NewIdent("_iostat")
	dst = append(dst, &ast.AssignStmt{
		Lhs: []ast.Expr{tmp},
		Tok: token.DEFINE,
		Rhs: []ast.Expr{last.X},
	})
	var elseStmt ast.Stmt
	if errLabel != "" {
		elseStmt = &ast.IfStmt{
			Cond: &ast.CallExpr{Fun: &ast.SelectorExpr{X: tmp, Sel: ast.NewIdent("IsError")}},
			Body: &ast.BlockStmt{List: []ast.Stmt{
				&ast.BranchStmt{Tok: token.GOTO, Label: tg.astLabel(errLabel)},
			}},
		}
	}
	if endLabel != "" {
		dst = append(dst, &ast.IfStmt{
			Cond: &ast.BinaryExpr{
				X:  tmp,
				Op: token.EQL,
				Y:  &ast.SelectorExpr{X: ast.NewIdent("fortio"), Sel: ast.NewIdent("IOStatEOF")},
			},
			Body: &ast.BlockStmt{List: []ast.Stmt{
				&ast.BranchStmt{Tok: token.GOTO, Label: tg.astLabel(endLabel)},
			}},
			Else: elseStmt,
		})
	} else if elseStmt != nil {
		dst = append(dst, elseStmt)
	}
	return dst, nil
}

// ioUnitOrDefault returns the unit expression, or an integer literal with defaultUnit if unit is nil or *.
func ioUnitOrDefault(unit f90.Expression, defaultUnit int64) f90.Expression {
	if unit == nil {
		return &f90.IntegerLiteral{Value: defaultUnit}
	}
	if ident, ok := unit.(*f90.Identifier); ok && ident.Value == "*" {
		return &f90.IntegerLiteral{Value: defaultUnit}
	}
	return unit
}

// transformReadStmt2 generates: fenv.ReadWithSpec(fortio.IOSpec{UNIT:..., FMT:..., ...}, &x, &y)
func (tg *ToGo) transformIO(dst []ast.Stmt, specSel, fenvSel *ast.SelectorExpr, specs []f90.IOSpecifier, inputs []f90.Expression, refInputs bool) ([]ast.Stmt, error) {
	specFields, err := tg.specifiersToFields(specs)
	if err != nil {
		return nil, err
	}
	spec := &ast.CompositeLit{Type: specSel, Elts: specFields}
	var args []ast.Expr = []ast.Expr{spec}
	var vitgt Varinfo
	for _, input := range inputs {
		err = tg.repl.InferType(&vitgt, input)
		if err != nil {
			return nil, tg.makeErrAtStmt("inferring type of IO statement input: " + err.Error())
		}
		arg, _, err := tg.transformExpression(&vitgt, input)
		if err != nil {
			return nil, err
		}
		if refInputs {
			arg = &ast.UnaryExpr{Op: token.AND, X: arg}
		}
		args = append(args, arg)
	}
	call := &ast.CallExpr{Fun: fenvSel, Args: args}
	dst = append(dst, &ast.ExprStmt{X: call})
	return dst, nil
}

// specifiersToFields transforms Fortran IOSpecifiers to Go composite literal fields.
func (tg *ToGo) specifiersToFields(specs []f90.IOSpecifier) ([]ast.Expr, error) {
	var fields []ast.Expr
	for _, spec := range specs {
		name := strings.ToUpper(spec.Name)
		cfg, ok := ioSpecifierConfig[name]
		if !ok {
			return fields, errors.New("unknown specifier: " + spec.Name)
		}
		var valueExpr ast.Expr
		var err error
		if cfg.enumMap != nil {
			// Handle enum: ACTION='READ' -> fortio.ActionREAD
			if strLit, ok := spec.Value.(*f90.StringLiteral); ok {
				valueExpr = cfg.enumMap[strings.ToUpper(strLit.Value)]
				if valueExpr == nil {
					return fields, errors.New("unknown specifier value: " + spec.Name + ":" + strLit.Value)
				}
			} else {
				return fields, errors.New("can't map specifier value: " + spec.Name + ":" + string(spec.Value.AppendString(nil)))
			}
		} else {
			if cfg.specialConv != nil {
				valueExpr, err = cfg.specialConv(tg, spec.Value)
				if err != nil {
					return fields, err
				}
			} else {
				// Transform expression with target type
				valueExpr, _, err = tg.transformExpression(cfg.target, spec.Value)
				if err != nil {
					return fields, err
				}
			}
		}
		if cfg.needsAddr {
			valueExpr = &ast.UnaryExpr{Op: token.AND, X: valueExpr}
		}
		fields = append(fields, &ast.KeyValueExpr{
			Key:   ast.NewIdent(name),
			Value: valueExpr,
		})
	}
	return fields, nil
}

// ioSpecField describes how to transform a Fortran I/O specifier to a Go struct field.
type ioSpecField struct {
	target      *Varinfo            // Type for expression transformation (nil for enums)
	needsAddr   bool                // Take address (&var) for pointer fields
	enumMap     map[string]ast.Expr // For enum values like ACTION='READ'
	specialConv func(*ToGo, f90.Expression) (ast.Expr, error)
}

// ioSpecifierConfig defines transformation rules for each I/O specifier.
var ioSpecifierConfig = map[string]ioSpecField{
	// Common to all statements
	"UNIT": {target: _tgtInt32},
	"FMT": {target: _tgtStringLit, specialConv: func(tg *ToGo, format f90.Expression) (ast.Expr, error) {
		switch format := format.(type) {
		case *f90.Identifier:
			if format.Value == "*" {
				return &ast.CallExpr{Fun: _astFortioDefaultFormat}, nil
			}
			if vi := tg.repl.Var(format.Value); vi != nil {
				varExpr := tg.astVarExpr(vi)
				var fmtStrExpr ast.Expr
				if vi.IsCharArray() {
					fmtStrExpr = &ast.CallExpr{Fun: _astFnCharacterArrayJoin, Args: []ast.Expr{varExpr}}
				} else if vi.IsChar() {
					fmtStrExpr = &ast.CallExpr{Fun: &ast.SelectorExpr{X: varExpr, Sel: ast.NewIdent("String")}}
				} else {
					return nil, fmt.Errorf("unknown FMT identifier %q", format.Value)
				}
				return &ast.CallExpr{Fun: _astFortioNewFormat, Args: []ast.Expr{fmtStrExpr}}, nil
			}
			return nil, fmt.Errorf("unknown FMT identifier %q", format.Value)
		case *f90.IntegerLiteral:
			label := strconv.FormatInt(format.Value, 10)
			if fmtInfo := tg.repl.getFormat(label); fmtInfo != nil {
				return &ast.CallExpr{Fun: _astFortioNewFormat, Args: formatSpecsToGoAST(fmtInfo.Specs)}, nil
			}
		case *f90.StringLiteral:
			specs := f90.ParseFormatString(format.Value)
			return &ast.CallExpr{Fun: _astFortioNewFormat, Args: formatSpecsToGoAST(specs)}, nil
		}
		return &ast.CallExpr{Fun: _astFortioDefaultFormat}, nil
	}},

	"IOSTAT": {target: _tgtInt32, needsAddr: true},
	"IOMSG":  {target: _tgtChar, needsAddr: true}, // CharacterArray.SetString sets IO message.

	// OPEN/CLOSE specific
	"FILE": {target: _tgtStringLit},
	"RECL": {target: _tgtInt32},

	// READ/WRITE specific
	"REC":  {target: _tgtInt32},
	"SIZE": {target: _tgtInt32, needsAddr: true},

	"ACCESS":       {enumMap: makeEnumMap[fortio.AccessMode]("Access")},
	"STATUS":       {enumMap: makeEnumMap[fortio.FileStatus]("Status")},
	"ACTION":       {enumMap: makeEnumMap[fortio.ActionMode]("Action")},
	"FORM":         {enumMap: makeEnumMap[fortio.FormMode]("Form")},
	"POSITION":     {enumMap: makeEnumMap[fortio.PositionMode]("Position")},
	"BLANK":        {enumMap: makeEnumMap[fortio.BlankMode]("Blank")},
	"DELIM":        {enumMap: makeEnumMap[fortio.DelimMode]("Delim")},
	"PAD":          {enumMap: makeEnumMap[fortio.PadMode]("Pad")},
	"DECIMAL":      {enumMap: makeEnumMap[fortio.DecimalMode]("Decimal")},
	"ROUND":        {enumMap: makeEnumMap[fortio.RoundMode]("Round")},
	"SIGN":         {enumMap: makeEnumMap[fortio.SignMode]("Sign")},
	"ADVANCE":      {enumMap: makeEnumMap[fortio.AdvanceMode]("Advance")},
	"ASYNCHRONOUS": {enumMap: makeEnumMap[fortio.AsyncMode]("Async")},
	"ENCODING":     {enumMap: makeEnumMap[fortio.EncodingMode]("Encoding")},
}

func makeEnumMap[T interface {
	String() string
	~int | ~uint8
}](prefix string) map[string]ast.Expr {
	s := make(map[string]ast.Expr)
	var k T
	for {
		name := k.String()
		if strings.ToUpper(name) != name {
			// Will break on non-stringer generated value (invalid value)
			break
		}
		s[name] = &ast.SelectorExpr{
			X:   ast.NewIdent("fortio"),
			Sel: ast.NewIdent(prefix + name),
		}
		k++
	}
	return s
}
