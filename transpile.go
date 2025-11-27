package fortran

import (
	"fmt"
	"go/ast"
	"go/token"
	"maps"
	"slices"
	"strconv"
	"strings"

	f90 "github.com/soypat/go-fortran/ast"
	"github.com/soypat/go-fortran/symbol"
	f90token "github.com/soypat/go-fortran/token"
)

// TranspileToGo transforms Fortran AST to Go AST
type TranspileToGo struct {
	symTable      *symbol.Table
	imports       []string
	charLengths   map[string]int              // Track CHARACTER(LEN=n) for padding assignments
	arrays        map[string]*f90.ArraySpec   // Track array dimensions for initialization
	arrayTypes    map[string]ast.Expr         // Track element types for arrays
	allocatables  map[string]bool             // Track ALLOCATABLE arrays (don't initialize)
	parameters    map[string]bool             // Track parameter names to avoid redeclaration
	pointerParams map[string]bool             // Track OUT/INOUT scalar parameters (need dereference in assignments)
	commonBlocks  map[string]*commonBlockInfo // COMMON block name -> info
	commonVars    map[string]string           // Variable name -> COMMON block name (for lookups)
}

// Reset initializes the transpiler with a symbol table
func (tg *TranspileToGo) Reset(symTable *symbol.Table) error {
	tg.imports = tg.imports[:0]
	tg.useImport("github.com/soypat/go-fortran/intrinsic")
	tg.symTable = symTable
	tg.charLengths = make(map[string]int)
	tg.commonBlocks = make(map[string]*commonBlockInfo)
	tg.commonVars = make(map[string]string)
	return nil
}

// commonBlockInfo tracks variables in a COMMON block
type commonBlockInfo struct {
	Name   string // COMMON block name (empty for blank COMMON)
	Fields []*ast.Field
}

func (cbi commonBlockInfo) goVarname() string {
	return cbi.Name
}

// enterProcedure initializes tracking maps for a function or subroutine
// and marks parameters to avoid redeclaration in the body.
// additionalParams can be used to mark additional names as parameters
// (e.g., function result variable)
func (tg *TranspileToGo) enterProcedure(params []f90.Parameter, additionalParams ...string) {
	// Reset tracking maps
	tg.arrays = make(map[string]*f90.ArraySpec)
	tg.arrayTypes = make(map[string]ast.Expr)
	tg.allocatables = make(map[string]bool)
	tg.parameters = make(map[string]bool)
	tg.pointerParams = make(map[string]bool)

	// Mark all parameters to avoid redeclaration in body
	// Also mark pointer parameters (OUT/INOUT scalars) for dereference in assignments
	// Also track array parameters for proper array reference handling
	for _, param := range params {
		tg.parameters[strings.ToUpper(param.Name)] = true
		// Track array parameters so array references work correctly
		if param.ArraySpec != nil {
			tg.arrays[param.Name] = param.ArraySpec
			// Array element type will be set from parameter type
			tp := tg.fortranTypeToGoWithKind(param.Type.Token, param.Type.KindOrLen)
			if tp == nil {
				panic(fmt.Sprintf("unknown parameter type: %s", param.Type.Token))
			}
			tg.arrayTypes[param.Name] = tp
		}
		// Scalar OUT/INOUT parameters are pointers, need dereference in assignments
		if param.ArraySpec == nil && (param.Intent == f90.IntentOut || param.Intent == f90.IntentInOut) {
			tg.pointerParams[strings.ToUpper(param.Name)] = true
		}
	}

	// Mark additional parameters (e.g., function result variable)
	for _, name := range additionalParams {
		tg.parameters[strings.ToUpper(name)] = true
	}
}

func (tg *TranspileToGo) AppendImportSpec(dst []*ast.ImportSpec) []*ast.ImportSpec {
	slices.SortFunc(tg.imports, func(a, b string) int {
		aIsThird := strings.Contains(a, ".")
		bIsThird := strings.Contains(b, ".")
		if aIsThird == bIsThird {
			return strings.Compare(a, b)
		} else if aIsThird {
			return -1
		}
		return 1
	})
	for _, importPath := range tg.imports {
		dst = append(dst, &ast.ImportSpec{
			Path: &ast.BasicLit{
				Kind:  token.STRING,
				Value: fmt.Sprintf("%q", importPath),
			},
		})
	}
	return dst
}

func (tg *TranspileToGo) AppendImportDecl(dst []ast.Decl) []ast.Decl {
	importSpecs := tg.AppendImportSpec(nil)
	if len(importSpecs) > 0 {
		importDecl := &ast.GenDecl{
			Tok:   token.IMPORT,
			Specs: make([]ast.Spec, len(importSpecs)),
		}
		for i, spec := range importSpecs {
			importDecl.Specs[i] = spec
		}
		dst = append(dst, importDecl)
	}
	return dst
}

func (tg *TranspileToGo) AppendCommonDecls(dst []ast.Decl) []ast.Decl {
	// Ensure output is ordered and repeatable, we do not want to generate diffs on every transpile.
	names := slices.AppendSeq([]string{}, maps.Keys(tg.commonBlocks))
	slices.Sort(names)
	for _, blockName := range names {
		block := tg.commonBlocks[blockName]
		structType := &ast.StructType{
			Fields: &ast.FieldList{
				List: block.Fields,
			},
		}

		decl := &ast.GenDecl{
			Tok: token.VAR,
			Specs: []ast.Spec{
				&ast.ValueSpec{
					Names: []*ast.Ident{ast.NewIdent(block.goVarname())},
					Type:  structType,
				},
			},
		}

		dst = append(dst, decl)
	}

	return dst
}

// MakeFile creates a complete Go AST file with package declaration, imports, and declarations
func (tg *TranspileToGo) MakeFile(packageName string, decls []ast.Decl) *ast.File {
	file := &ast.File{
		Name:  ast.NewIdent(packageName),
		Decls: []ast.Decl{},
	}

	// Add import declaration if there are any imports
	importSpecs := tg.AppendImportSpec(nil)
	if len(importSpecs) > 0 {
		importDecl := &ast.GenDecl{
			Tok:   token.IMPORT,
			Specs: make([]ast.Spec, len(importSpecs)),
		}
		for i, spec := range importSpecs {
			importDecl.Specs[i] = spec
		}
		file.Decls = append(file.Decls, importDecl)
	}

	// Add COMMON block declarations
	file.Decls = tg.AppendCommonDecls(file.Decls)

	// Add provided declarations
	file.Decls = append(file.Decls, decls...)

	return file
}

// TransformSubroutine transforms a Fortran SUBROUTINE to a Go function declaration
func (tg *TranspileToGo) TransformSubroutine(sub *f90.Subroutine) (*ast.FuncDecl, error) {
	// Initialize procedure tracking
	tg.enterProcedure(sub.Parameters)

	// Clear COMMON variable tracking for this subroutine
	// (variables are only COMMON if they appear in a COMMON statement in THIS subroutine)

	// Pre-scan for COMMON blocks to know which variables to skip in type declarations
	tg.preScanCommonBlocks(sub.Body)

	// Transform parameters
	paramFields := tg.transformParameters(sub.Parameters)

	// Transform body statements
	bodyStmts := tg.transformStatements(sub.Body)

	// Create function declaration
	funcDecl := &ast.FuncDecl{
		Name: ast.NewIdent(sub.Name),
		Type: &ast.FuncType{
			Params: &ast.FieldList{
				List: paramFields,
			},
		},
		Body: &ast.BlockStmt{
			List: bodyStmts,
		},
	}

	return funcDecl, nil
}

// TransformFunction transforms a Fortran FUNCTION to a Go function declaration
// Functions are like subroutines but return a value
func (tg *TranspileToGo) TransformFunction(fn *f90.Function) (*ast.FuncDecl, error) {
	// Initialize procedure tracking
	// Mark function name as additional parameter (Fortran uses it as result variable)
	tg.enterProcedure(fn.Parameters, fn.Name)

	// Pre-scan for COMMON blocks to know which variables to skip in type declarations
	tg.preScanCommonBlocks(fn.Body)

	// Transform parameters
	paramFields := tg.transformParameters(fn.Parameters)

	// Transform body statements
	bodyStmts := tg.transformStatements(fn.Body)

	// Add return statement if function assigns to its name
	// In Fortran: FACTORIAL = result
	// In Go: return result
	bodyStmts = tg.convertFunctionResultToReturn(fn.Name, bodyStmts)

	// Map Fortran result type to Go type, considering KIND parameter
	resultType := tg.fortranTypeToGoWithKind(fn.Type.Token, fn.Type.KindOrLen)
	if resultType == nil {
		return nil, fmt.Errorf("unknown fortran result type: %s", fn.Type.Token)
	}

	// Create function declaration
	funcDecl := &ast.FuncDecl{
		Name: ast.NewIdent(fn.Name),
		Type: &ast.FuncType{
			Params: &ast.FieldList{
				List: paramFields,
			},
			Results: &ast.FieldList{
				List: []*ast.Field{
					{Type: resultType},
				},
			},
		},
		Body: &ast.BlockStmt{
			List: bodyStmts,
		},
	}

	return funcDecl, nil
}

// TransformProgram transforms a Fortran PROGRAM block to Go declarations
// Returns a list of declarations: func main() {...} and any contained procedures
func (tg *TranspileToGo) TransformProgram(prog *f90.ProgramBlock) ([]ast.Decl, error) {
	// Initialize procedure tracking for PROGRAM scope (no parameters)
	tg.enterProcedure(nil)

	// Pre-scan for COMMON blocks to know which variables to skip in declarations
	tg.preScanCommonBlocks(prog.Body)

	// Transform all statements in program body
	bodyStmts := tg.transformStatements(prog.Body)

	// Create main function declaration
	mainFunc := &ast.FuncDecl{
		Name: ast.NewIdent("main"),
		Type: &ast.FuncType{
			Params: &ast.FieldList{}, // No parameters for main
		},
		Body: &ast.BlockStmt{
			List: bodyStmts,
		},
	}

	// Start with main function
	decls := []ast.Decl{mainFunc}

	// Transform contained procedures (CONTAINS section)
	for _, contained := range prog.Contains {
		switch c := contained.(type) {
		case *f90.Subroutine:
			funcDecl, err := tg.TransformSubroutine(c)
			if err != nil {
				return nil, err
			}
			decls = append(decls, funcDecl)
		case *f90.Function:
			funcDecl, err := tg.TransformFunction(c)
			if err != nil {
				return nil, err
			}
			decls = append(decls, funcDecl)
		default:
			return decls, fmt.Errorf("unknown CONTAINS declaration: %T", c)
		}
	}

	return decls, nil
}

// convertFunctionResultToReturn converts assignments to function name into return statements
// Fortran: FACTORIAL = result → Go: return result
// Also handles early RETURN statements
// Processes statements recursively to handle nested blocks (IF, DO, etc.)
func (tg *TranspileToGo) convertFunctionResultToReturn(funcName string, stmts []ast.Stmt) []ast.Stmt {
	funcNameUpper := strings.ToUpper(funcName)
	return tg.convertStatementsRecursive(funcNameUpper, stmts)
}

// convertStatementsRecursive recursively processes statements to convert function assignments
func (tg *TranspileToGo) convertStatementsRecursive(funcNameUpper string, stmts []ast.Stmt) []ast.Stmt {
	var result []ast.Stmt
	var lastAssignValue ast.Expr

	for i, stmt := range stmts {
		// Check if this is an assignment to the function name
		if assignStmt, ok := stmt.(*ast.AssignStmt); ok {
			if len(assignStmt.Lhs) == 1 && len(assignStmt.Rhs) == 1 {
				if ident, ok := assignStmt.Lhs[0].(*ast.Ident); ok {
					if strings.ToUpper(ident.Name) == funcNameUpper {
						// This is an assignment to function result
						// Check if next statement is RETURN
						if i+1 < len(stmts) {
							if _, isReturn := stmts[i+1].(*ast.ReturnStmt); isReturn {
								// Skip this assignment, next iteration will handle return
								lastAssignValue = assignStmt.Rhs[0]
								continue
							}
						}
						// Save value for end-of-function return
						lastAssignValue = assignStmt.Rhs[0]
						continue
					}
				}
			}
		}

		// Check for RETURN statement
		if retStmt, ok := stmt.(*ast.ReturnStmt); ok {
			// If we have a saved result value and no explicit return value, use it
			if len(retStmt.Results) == 0 && lastAssignValue != nil {
				retStmt.Results = []ast.Expr{lastAssignValue}
			}
			result = append(result, retStmt)
			lastAssignValue = nil
			continue
		}

		// Recursively process nested blocks (IF statements, DO loops, etc.)
		if ifStmt, ok := stmt.(*ast.IfStmt); ok {
			// Process THEN block
			if ifStmt.Body != nil {
				ifStmt.Body.List = tg.convertStatementsRecursive(funcNameUpper, ifStmt.Body.List)
			}
			// Process ELSE block
			if ifStmt.Else != nil {
				if elseBlock, ok := ifStmt.Else.(*ast.BlockStmt); ok {
					elseBlock.List = tg.convertStatementsRecursive(funcNameUpper, elseBlock.List)
				} else if elseIf, ok := ifStmt.Else.(*ast.IfStmt); ok {
					// Recursive ELSE IF
					converted := tg.convertStatementsRecursive(funcNameUpper, []ast.Stmt{elseIf})
					if len(converted) > 0 {
						ifStmt.Else = converted[0]
					}
				}
			}
		} else if forStmt, ok := stmt.(*ast.ForStmt); ok {
			// Process DO loop body
			if forStmt.Body != nil {
				forStmt.Body.List = tg.convertStatementsRecursive(funcNameUpper, forStmt.Body.List)
			}
		}

		result = append(result, stmt)
	}

	// If function ends without explicit RETURN, add return with last assigned value
	if lastAssignValue != nil {
		result = append(result, &ast.ReturnStmt{
			Results: []ast.Expr{lastAssignValue},
		})
	}

	return result
}

// transformParameters transforms Fortran subroutine parameters to Go function parameters
// Handles INTENT attributes:
// - INTENT(IN) → pass by value (e.g., a int32)
// - INTENT(OUT) → pass by pointer (e.g., result *int32)
// - INTENT(INOUT) → pass by pointer (e.g., arr *intrinsic.Array[int32])
// - No INTENT → assume INOUT (pass by pointer)
func (tg *TranspileToGo) transformParameters(params []f90.Parameter) []*ast.Field {
	var fields []*ast.Field

	for _, param := range params {
		// Get the Go type for this parameter, considering KIND
		goType := tg.fortranTypeToGoWithKind(param.Type.Token, param.Type.KindOrLen)
		if goType == nil {
			panic(fmt.Sprintf("unhandled type %s", param.Type.Token))
		}
		// Handle arrays - always use intrinsic.Array[T]
		if param.ArraySpec != nil {
			// Array parameters: *intrinsic.Array[T]
			goType = &ast.StarExpr{
				X: &ast.IndexExpr{
					X:     _astTypeArray,
					Index: goType,
				},
			}
		} else {
			// Scalar parameters: check INTENT
			// INTENT(OUT) or INTENT(INOUT) require pointer
			if param.Intent == f90.IntentOut || param.Intent == f90.IntentInOut {
				goType = &ast.StarExpr{X: goType}
			}
			// INTENT(IN) or default intent use pass by value (no pointer)
		}

		// Create parameter field
		field := &ast.Field{
			Names: []*ast.Ident{ast.NewIdent(param.Name)},
			Type:  goType,
		}
		fields = append(fields, field)
	}

	return fields
}

// transformStatements transforms a slice of Fortran statements to Go statements
// Handles labeled statements by wrapping them in LabeledStmt nodes
func (tg *TranspileToGo) transformStatements(stmts []f90.Statement) []ast.Stmt {
	var goStmts []ast.Stmt

	for _, stmt := range stmts {
		goStmt := tg.transformStatement(stmt)
		if goStmt != nil {
			// Check if the Fortran statement has a label (e.g., "100" in "100 CONTINUE")
			if label := stmt.GetLabel(); label != "" {
				// Wrap the Go statement with a label: labelN:
				goStmt = &ast.LabeledStmt{
					Label: ast.NewIdent("label" + label),
					Stmt:  goStmt,
				}
			}
			goStmts = append(goStmts, goStmt)
		}
	}

	return goStmts
}

// transformStatement transforms a single Fortran statement to a Go statement
func (tg *TranspileToGo) transformStatement(stmt f90.Statement) ast.Stmt {
	switch s := stmt.(type) {
	case *f90.TypeDeclaration:
		return tg.transformTypeDeclaration(s)
	case *f90.AssignmentStmt:
		return tg.transformAssignment(s)
	case *f90.PrintStmt:
		return tg.transformPrint(s)
	case *f90.IfStmt:
		return tg.transformIfStmt(s)
	case *f90.DoLoop:
		return tg.transformDoLoop(s)
	case *f90.CallStmt:
		return tg.transformCallStmt(s)
	case *f90.ReturnStmt:
		// RETURN statement in functions will be handled by convertFunctionResultToReturn
		// For now, just generate empty return (will be filled with result value later)
		return &ast.ReturnStmt{}
	case *f90.CycleStmt:
		// CYCLE → continue
		return &ast.BranchStmt{
			Tok: token.CONTINUE,
		}
	case *f90.ExitStmt:
		// EXIT → break
		return &ast.BranchStmt{
			Tok: token.BREAK,
		}
	case *f90.ContinueStmt:
		// CONTINUE → empty statement (no-op in Go)
		// If there's a label, it will be handled by label processing later
		return &ast.EmptyStmt{}
	case *f90.GotoStmt:
		// GOTO label → goto labelN
		return &ast.BranchStmt{
			Tok:   token.GOTO,
			Label: ast.NewIdent("label" + s.Target),
		}
	case *f90.AllocateStmt:
		return tg.transformAllocateStmt(s)
	case *f90.DeallocateStmt:
		return tg.transformDeallocateStmt(s)
	case *f90.SelectCaseStmt:
		return tg.transformSelectCaseStmt(s)
	case *f90.CommonStmt:
		return tg.transformCommonStmt(s)
	case *f90.DataStmt:
		return tg.transformDataStmt(s)
	case *f90.ArithmeticIfStmt:
		return tg.transformArithmeticIfStmt(s)
	case *f90.ComputedGotoStmt:
		return tg.transformComputedGotoStmt(s)
	case *f90.StopStmt:
		return tg.transformStopStmt(s)
	case *f90.ImplicitStatement:
		// Specification statement - no code generation
		return nil
	case *f90.UseStatement:
		// Specification statement - no code generation
		return nil
	case *f90.ExternalStmt:
		// Specification statement - no code generation
		return nil
	case *f90.IntrinsicStmt:
		// Specification statement - no code generation
		return nil
	default:
		// For now, unsupported statements are skipped
		panic(fmt.Sprintf("unsupported statement: %T", s))
		return nil
	}
}

// transformPrint transforms a Fortran PRINT statement to intrinsic.Formatter.Print call
// Note: Fortran PRINT * uses list-directed I/O formatting
func (tg *TranspileToGo) transformPrint(print *f90.PrintStmt) ast.Stmt {
	// Transform output expressions
	args := tg.transformExpressions(print.OutputList)

	// Use intrinsic.Print() for Fortran-compatible formatting
	// This handles: leading space, T/F for LOGICAL, field widths, spacing between items

	return &ast.ExprStmt{
		X: &ast.CallExpr{
			Fun: &ast.SelectorExpr{
				X:   ast.NewIdent("intrinsic"),
				Sel: ast.NewIdent("Print"),
			},
			Args: args,
		},
	}
}

// transformIfStmt transforms a Fortran IF statement to Go if/else statement
func (tg *TranspileToGo) transformIfStmt(ifStmt *f90.IfStmt) ast.Stmt {
	// Transform the condition
	condition := tg.transformExpression(ifStmt.Condition)
	if condition == nil {
		return nil
	}

	// Create the Go if statement
	goIfStmt := &ast.IfStmt{
		Cond: condition,
		Body: &ast.BlockStmt{
			List: tg.transformStatements(ifStmt.ThenPart),
		},
	}

	// Handle ELSE IF clauses
	if len(ifStmt.ElseIfParts) > 0 {
		// Build nested if/else chain
		currentElse := goIfStmt
		for _, elseIfClause := range ifStmt.ElseIfParts {
			elseIfCond := tg.transformExpression(elseIfClause.Condition)
			if elseIfCond == nil {
				continue
			}

			elseIfStmt := &ast.IfStmt{
				Cond: elseIfCond,
				Body: &ast.BlockStmt{
					List: tg.transformStatements(elseIfClause.ThenPart),
				},
			}

			// Link as else clause
			currentElse.Else = elseIfStmt
			currentElse = elseIfStmt
		}

		// Handle final ELSE clause if present
		if len(ifStmt.ElsePart) > 0 {
			currentElse.Else = &ast.BlockStmt{
				List: tg.transformStatements(ifStmt.ElsePart),
			}
		}
	} else if len(ifStmt.ElsePart) > 0 {
		// No ELSE IF, just ELSE
		goIfStmt.Else = &ast.BlockStmt{
			List: tg.transformStatements(ifStmt.ElsePart),
		}
	}

	return goIfStmt
}

// transformSelectCaseStmt transforms a Fortran SELECT CASE statement to Go switch
//
// Fortran SELECT CASE:
//
//	SELECT CASE (expr)
//	CASE (val1)
//	  statements
//	CASE (val2, val3)
//	  statements
//	CASE DEFAULT
//	  statements
//	END SELECT
//
// Go switch:
//
//	switch expr {
//	case val1:
//	  statements
//	case val2, val3:
//	  statements
//	default:
//	  statements
//	}
func (tg *TranspileToGo) transformSelectCaseStmt(selectStmt *f90.SelectCaseStmt) ast.Stmt {
	// Transform the select expression
	tagExpr := tg.transformExpression(selectStmt.Expression)
	if tagExpr == nil {
		return nil
	}

	// Create Go switch statement
	switchStmt := &ast.SwitchStmt{
		Tag:  tagExpr,
		Body: &ast.BlockStmt{},
	}

	// Transform each case clause
	for _, caseClause := range selectStmt.Cases {
		clause := &ast.CaseClause{
			Body: tg.transformStatements(caseClause.Body),
		}

		if caseClause.IsDefault {
			// CASE DEFAULT → default:
			clause.List = nil // nil List means default case in Go
		} else {
			// CASE (val1, val2, ...) → case val1, val2, ...:
			for _, val := range caseClause.Values {
				transformedVal := tg.transformExpression(val)
				if transformedVal != nil {
					clause.List = append(clause.List, transformedVal)
				}
			}
		}

		switchStmt.Body.List = append(switchStmt.Body.List, clause)
	}

	return switchStmt
}

// transformDoLoop transforms a Fortran DO loop to Go for loop
// Handles both counter-controlled and DO WHILE loops:
//
// Counter-controlled:
// Fortran: DO var = start, end [, step]
// transformAllocateStmt transforms ALLOCATE(arr(dims)) to arr = intrinsic.NewArray[T](dims...)
func (tg *TranspileToGo) transformAllocateStmt(stmt *f90.AllocateStmt) ast.Stmt {
	// For now, handle simple case: single object allocation
	if len(stmt.Objects) == 0 {
		return nil
	}

	// Process each allocation
	var stmts []ast.Stmt
	for _, obj := range stmt.Objects {
		var name string
		var dimExprs []f90.Expression

		// The parser may create either ArrayRef or FunctionCall for ALLOCATE(arr(dims))
		// because it doesn't have type information yet
		switch o := obj.(type) {
		case *f90.ArrayRef:
			name = o.Name
			dimExprs = o.Subscripts
		case *f90.FunctionCall:
			// Parser treats arr(5) as a function call
			name = o.Name
			dimExprs = o.Args
		default:
			continue
		}

		elemType := tg.arrayTypes[name]
		if elemType == nil {
			continue
		}

		// Transform dimensions from subscripts/args
		var dims []ast.Expr
		for _, dimExpr := range dimExprs {
			dim := tg.transformExpression(dimExpr)
			if dim == nil {
				continue
			}
			dims = append(dims, dim)
		}

		// Generate: name = intrinsic.NewArray[elemType](dims...)
		assignment := &ast.AssignStmt{
			Lhs: []ast.Expr{ast.NewIdent(name)},
			Tok: token.ASSIGN,
			Rhs: []ast.Expr{
				&ast.CallExpr{
					Fun: &ast.IndexExpr{
						X: &ast.SelectorExpr{
							X:   ast.NewIdent("intrinsic"),
							Sel: ast.NewIdent("NewArray"),
						},
						Index: elemType,
					},
					Args: dims,
				},
			},
		}
		stmts = append(stmts, assignment)
	}

	if len(stmts) == 1 {
		return stmts[0]
	}
	return &ast.BlockStmt{List: stmts}
}

// transformDeallocateStmt transforms DEALLOCATE(arr) to arr = nil
func (tg *TranspileToGo) transformDeallocateStmt(stmt *f90.DeallocateStmt) ast.Stmt {
	// Process each deallocation
	var stmts []ast.Stmt
	for _, obj := range stmt.Objects {
		// The object should be an identifier, ArrayRef, or FunctionCall
		var name string
		switch o := obj.(type) {
		case *f90.Identifier:
			name = o.Value
		case *f90.ArrayRef:
			name = o.Name
		case *f90.FunctionCall:
			// Parser may treat arr as a function call
			name = o.Name
		default:
			continue
		}

		// Generate: name = nil
		assignment := &ast.AssignStmt{
			Lhs: []ast.Expr{ast.NewIdent(name)},
			Tok: token.ASSIGN,
			Rhs: []ast.Expr{ast.NewIdent("nil")},
		}
		stmts = append(stmts, assignment)
	}

	if len(stmts) == 1 {
		return stmts[0]
	}
	return &ast.BlockStmt{List: stmts}
}

// Go:      for var = start; var <= end; var += step { }
//
// DO WHILE:
// Fortran: DO WHILE (condition)
// Go:      for condition { }
//
// CRITICAL: Fortran DO loops have INCLUSIVE upper bounds, so we use <= not <
// Example: DO i = 1, 5 iterates over i = 1, 2, 3, 4, 5 (includes 5)
//
// IMPORTANT: In Fortran, loop variables are typically declared before the loop
// (e.g., INTEGER :: i), so we use assignment (=) not declaration (:=) in the
// for loop init to avoid redeclaration errors.
func (tg *TranspileToGo) transformDoLoop(loop *f90.DoLoop) ast.Stmt {
	// Transform loop body (common to both types)
	body := tg.transformStatements(loop.Body)

	// Check if this is DO WHILE (Var is empty, Start contains condition)
	if loop.Var == "" {
		// DO WHILE loop: for condition { }
		condition := tg.transformExpression(loop.Start)
		if condition == nil {
			return nil
		}
		return &ast.ForStmt{
			Cond: condition,
			Body: &ast.BlockStmt{
				List: body,
			},
		}
	}

	// Counter-controlled DO loop
	start := tg.transformExpression(loop.Start)
	if start == nil {
		return nil
	}

	end := tg.transformExpression(loop.End)
	if end == nil {
		return nil
	}

	// Step defaults to 1 if not specified
	var step ast.Expr
	if loop.Step != nil {
		step = tg.transformExpression(loop.Step)
	} else {
		step = &ast.BasicLit{
			Kind:  token.INT,
			Value: "1",
		}
	}

	// Create Go for loop: for var = start; var <= end; var += step { }
	// Use assignment (=) not declaration (:=) since loop vars are pre-declared in Fortran
	return &ast.ForStmt{
		Init: &ast.AssignStmt{
			Lhs: []ast.Expr{ast.NewIdent(loop.Var)},
			Tok: token.ASSIGN, // = (not :=)
			Rhs: []ast.Expr{start},
		},
		Cond: &ast.BinaryExpr{
			X:  ast.NewIdent(loop.Var),
			Op: token.LEQ, // <= for inclusive upper bound (Fortran semantics)
			Y:  end,
		},
		Post: &ast.AssignStmt{
			Lhs: []ast.Expr{ast.NewIdent(loop.Var)},
			Tok: token.ADD_ASSIGN, // +=
			Rhs: []ast.Expr{step},
		},
		Body: &ast.BlockStmt{
			List: body,
		},
	}
}

// transformCallStmt transforms a Fortran CALL statement to Go function call
// Fortran: CALL subname(arg1, arg2, ...)
// Go:      subname(arg1, &arg2, ...)  (with & for OUT/INOUT parameters)
//
// IMPORTANT: INTENT(OUT) and INTENT(INOUT) parameters must be passed by pointer in Go
// to match Fortran's reference semantics. We look up the subroutine in the symbol table
// to determine which arguments need & (address-of operator).
func (tg *TranspileToGo) transformCallStmt(call *f90.CallStmt) ast.Stmt {
	// Transform arguments, adding & for OUT/INOUT parameters
	var args []ast.Expr

	// Look up the subroutine in the symbol table to get parameter INTENT info
	var params []f90.Parameter
	if tg.symTable != nil {
		subSym := tg.symTable.GlobalScope().Lookup(call.Name)
		if subSym != nil {
			// Get the Subroutine AST node from the symbol
			if subNode, ok := subSym.DeclNode().(*f90.Subroutine); ok {
				params = subNode.Parameters
			}
		}
	}

	for i, arg := range call.Args {
		goArg := tg.transformExpression(arg)
		if goArg == nil {
			continue
		}

		// Check if this parameter needs & (OUT or INOUT)
		needsAddressOf := false
		if i < len(params) {
			param := params[i]
			// Arrays are already pointers, don't add &
			if param.ArraySpec == nil {
				// Scalar parameters with INTENT(OUT) or INTENT(INOUT) need &
				if param.Intent == f90.IntentOut || param.Intent == f90.IntentInOut {
					needsAddressOf = true
				}
			}
		}

		if needsAddressOf {
			// Wrap with & (address-of operator)
			goArg = &ast.UnaryExpr{
				Op: token.AND,
				X:  goArg,
			}
		}

		args = append(args, goArg)
	}

	// Generate: subname(args...)
	return &ast.ExprStmt{
		X: &ast.CallExpr{
			Fun:  ast.NewIdent(call.Name),
			Args: args,
		},
	}
}

// transformExpressions transforms a slice of Fortran expressions to Go expressions
func (tg *TranspileToGo) transformExpressions(exprs []f90.Expression) []ast.Expr {
	var goExprs []ast.Expr

	for _, expr := range exprs {
		goExpr := tg.transformExpression(expr)
		if goExpr != nil {
			goExprs = append(goExprs, goExpr)
		}
	}

	return goExprs
}

// fortranTypeToGo converts Fortran type to Go type, ignoring KIND parameter
func (tg *TranspileToGo) fortranTypeToGo(ft f90token.Token) (goType ast.Expr) {
	return tg.fortranTypeToGoWithKind(ft, nil)
}

// fortranTypeToGoWithKind converts Fortran type to Go type, considering KIND parameter
// KIND mappings:
//
//	INTEGER(KIND=1) → int8, INTEGER(KIND=2) → int16
//	INTEGER(KIND=4) → int32, INTEGER(KIND=8) → int64
//	REAL(KIND=4) → float32, REAL(KIND=8) → float64
func (tg *TranspileToGo) fortranTypeToGoWithKind(ft f90token.Token, kindParam f90.Expression) (goType ast.Expr) {
	// Extract KIND value if present
	kindValue := tg.extractKindValue(kindParam)

	switch ft {
	default:
		return nil //
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

// extractKindValue extracts the integer KIND value from a KIND parameter expression
// Returns 0 if KIND is not specified or cannot be evaluated
func (tg *TranspileToGo) extractKindValue(kindParam f90.Expression) int {
	if kindParam == nil {
		return 0
	}

	// Handle integer literals
	if intLit, ok := kindParam.(*f90.IntegerLiteral); ok {
		return int(intLit.Value)
	}

	// For more complex expressions, default to 0 (use default KIND)
	// TODO: Handle named constants, expressions, etc.
	return 0
}

// cleanIntegerLiteral removes Fortran KIND suffix from integer literals
// Example: 9223372036854775807_8 → 9223372036854775807
func (tg *TranspileToGo) cleanIntegerLiteral(raw string) string {
	// Remove _<kind> suffix if present
	if idx := strings.LastIndex(raw, "_"); idx >= 0 {
		return raw[:idx]
	}
	return raw
}

// cleanRealLiteral converts Fortran double precision literals to Go format
// Examples:
//
//	3.141592653589793D0 → 3.141592653589793
//	1.5D+10 → 1.5e+10
//	2.0D-5 → 2.0e-5
func (tg *TranspileToGo) cleanRealLiteral(raw string) string {
	// Replace D exponent notation with e notation
	// Fortran uses D for double precision: 1.5D+10
	// Go uses e: 1.5e+10
	result := strings.ReplaceAll(raw, "D", "e")
	result = strings.ReplaceAll(result, "d", "e")

	// Handle D0 suffix (no exponent) by removing it
	if strings.HasSuffix(result, "e0") {
		result = result[:len(result)-2]
	}

	return result
}

// fortranConstantToGoExpr converts a Fortran constant expression string to Go AST expression
// This is a simple converter for PARAMETER constants that handles basic cases
func (tg *TranspileToGo) fortranConstantToGoExpr(fortranExpr string) ast.Expr {
	fortranExpr = strings.TrimSpace(fortranExpr)

	// Handle boolean literals
	if fortranExpr == ".TRUE." {
		return ast.NewIdent("true")
	}
	if fortranExpr == ".FALSE." {
		return ast.NewIdent("false")
	}

	// Handle numeric literals (integer or float)
	if _, err := strconv.ParseInt(fortranExpr, 10, 64); err == nil {
		// It's an integer literal
		return &ast.BasicLit{Kind: token.INT, Value: fortranExpr}
	}
	if _, err := strconv.ParseFloat(fortranExpr, 64); err == nil {
		// It's a float literal
		return &ast.BasicLit{Kind: token.FLOAT, Value: fortranExpr}
	}

	// Handle string literals (quoted strings)
	if strings.HasPrefix(fortranExpr, "'") || strings.HasPrefix(fortranExpr, "\"") {
		// Remove Fortran quotes and add Go quotes
		unquoted := strings.Trim(fortranExpr, "'\"")
		return &ast.BasicLit{Kind: token.STRING, Value: strconv.Quote(unquoted)}
	}

	// Handle simple expressions like "2.0 * PI"
	// For now, check for simple binary operations and identifiers
	if strings.Contains(fortranExpr, "*") && !strings.Contains(fortranExpr, "**") {
		parts := strings.SplitN(fortranExpr, "*", 2)
		if len(parts) == 2 {
			left := tg.fortranConstantToGoExpr(strings.TrimSpace(parts[0]))
			right := tg.fortranConstantToGoExpr(strings.TrimSpace(parts[1]))
			return &ast.BinaryExpr{
				X:  left,
				Op: token.MUL,
				Y:  right,
			}
		}
	}

	// Otherwise, assume it's an identifier (like another PARAMETER constant)
	return ast.NewIdent(fortranExpr)
}

// transformTypeDeclaration transforms a Fortran type declaration to Go var declaration
func (tg *TranspileToGo) transformTypeDeclaration(decl *f90.TypeDeclaration) ast.Stmt {
	// Map Fortran type to Go type, considering KIND parameter
	goType := tg.fortranTypeToGoWithKind(decl.Type.Token, decl.Type.KindOrLen)
	if goType == nil {
		return nil
	}
	// Check for special attributes
	isAllocatable := false
	isParameter := false
	for _, attr := range decl.Attributes {
		if attr == f90token.ALLOCATABLE {
			isAllocatable = true
		}
		if attr == f90token.PARAMETER {
			isParameter = true
		}
	}

	// Create var declarations for each entity
	specs := make([]ast.Spec, 0, len(decl.Entities))
	for i := range decl.Entities {
		entity := &decl.Entities[i]

		// If this variable is in a COMMON block, record its type
		if blockName, inCommon := tg.commonVars[entity.Name]; inCommon {
			if block, exists := tg.commonBlocks[blockName]; exists {
				// Find the field for this variable and set its type
				for _, field := range block.Fields {
					if len(field.Names) > 0 && field.Names[0].Name == entity.Name {
						field.Type = goType
						break
					}
				}
			}
			// Variables in COMMON blocks will be accessed via the global struct
			// Don't generate local declarations for them
			continue
		}

		// Skip parameters - they're already declared in function signature
		if tg.parameters[strings.ToUpper(entity.Name)] {
			continue
		}

		// Check if this is an array
		if entity.ArraySpec != nil {
			if isAllocatable {
				// Mark as allocatable and generate uninitialized pointer declaration
				tg.allocatables[entity.Name] = true
				spec := tg.transformAllocatableArrayDeclaration(entity.Name, goType)
				specs = append(specs, spec)
			} else {
				// Handle regular array declarations
				spec := tg.transformArrayDeclaration(entity.Name, goType, entity.ArraySpec)
				specs = append(specs, spec)
			}
			continue
		}

		spec := &ast.ValueSpec{
			Names: []*ast.Ident{ast.NewIdent(entity.Name)},
			Type:  goType,
		}

		// For PARAMETER constants, convert initializer to Go expression
		if isParameter {
			if entity.Initializer != "" {
				// Strip = or => prefix
				initStr := strings.TrimSpace(entity.Initializer)
				if strings.HasPrefix(initStr, "=") {
					initStr = strings.TrimSpace(initStr[1:])
				} else if strings.HasPrefix(initStr, "=>") {
					initStr = strings.TrimSpace(initStr[2:])
				}

				// Convert Fortran constant expression to Go expression
				spec.Values = []ast.Expr{tg.fortranConstantToGoExpr(initStr)}
			}
		} else if decl.Type.Token == f90token.CHARACTER && entity.CharLen != nil {
			// For CHARACTER variables (not constants), initialize with CharacterArray
			if length := tg.extractIntLiteral(entity.CharLen); length > 0 {
				// Track CHARACTER length for later use in assignments
				tg.charLengths[entity.Name] = length
				// Initialize with intrinsic.NewCharacterArray(length)
				spec.Values = []ast.Expr{
					&ast.CallExpr{
						Fun: _astFnNewCharArray,
						Args: []ast.Expr{&ast.BasicLit{
							Kind:  token.INT,
							Value: strconv.Itoa(length),
						}},
					},
				}
			}
		}

		specs = append(specs, spec)
	}

	// If all entities were skipped (all were parameters), don't generate empty var statement
	if len(specs) == 0 {
		return nil
	}

	// Use CONST token for PARAMETER declarations, VAR for regular variables
	tok := token.VAR
	if isParameter {
		tok = token.CONST
	}

	return &ast.DeclStmt{
		Decl: &ast.GenDecl{
			Tok:   tok,
			Specs: specs,
		},
	}
}

// transformArrayDeclaration creates an intrinsic.Array[T] declaration with constructor call
// Generates: var arr *intrinsic.Array[int32] = intrinsic.NewArray[int32](3, 4)
func (tg *TranspileToGo) transformArrayDeclaration(name string, elemType ast.Expr, arraySpec *f90.ArraySpec) *ast.ValueSpec {
	// Track this array for disambiguation (FunctionCall vs ArrayRef)
	tg.arrays[name] = arraySpec
	tg.arrayTypes[name] = elemType

	// Build type: *intrinsic.Array[elemType]
	arrayType := &ast.StarExpr{
		X: &ast.IndexExpr{
			X:     _astTypeArray,
			Index: elemType, // Type parameter
		},
	}

	// Get dimensions (sizes) from array bounds
	var dims []ast.Expr
	for _, bound := range arraySpec.Bounds {
		size := tg.transformExpression(bound.Upper)
		if size == nil {
			// If we can't determine size, return uninitialized
			return &ast.ValueSpec{
				Names: []*ast.Ident{ast.NewIdent(name)},
				Type:  arrayType,
			}
		}
		dims = append(dims, size)
	}

	// Create constructor call: intrinsic.NewArray[elemType](dim1, dim2, ...)
	// Uses the variadic NewArray function that handles any number of dimensions
	constructorCall := &ast.CallExpr{
		Fun: &ast.IndexExpr{
			X: &ast.SelectorExpr{
				X:   ast.NewIdent("intrinsic"),
				Sel: ast.NewIdent("NewArray"),
			},
			Index: elemType, // Type parameter
		},
		Args: dims,
	}

	return &ast.ValueSpec{
		Names:  []*ast.Ident{ast.NewIdent(name)},
		Values: []ast.Expr{constructorCall},
	}
}

// transformAllocatableArrayDeclaration generates an uninitialized pointer declaration for ALLOCATABLE arrays
// These will be initialized later by ALLOCATE statements
func (tg *TranspileToGo) transformAllocatableArrayDeclaration(name string, elemType ast.Expr) *ast.ValueSpec {
	// Track this array for disambiguation (FunctionCall vs ArrayRef)
	tg.arrays[name] = nil // ArraySpec not needed for allocatable
	tg.arrayTypes[name] = elemType

	// Build type: *intrinsic.Array[elemType]
	arrayType := &ast.StarExpr{
		X: &ast.IndexExpr{
			X:     _astTypeArray,
			Index: elemType, // Type parameter
		},
	}

	// Return uninitialized pointer (will be set by ALLOCATE statement)
	return &ast.ValueSpec{
		Names: []*ast.Ident{ast.NewIdent(name)},
		Type:  arrayType,
	}
}

// extractIntLiteral extracts an integer value from a simple expression
// Returns 0 if the expression is not a simple integer literal
func (tg *TranspileToGo) extractIntLiteral(expr f90.Expression) int {
	if lit, ok := expr.(*f90.IntegerLiteral); ok {
		return int(lit.Value)
	}
	return 0
}

// transformAssignment transforms a Fortran assignment to Go assignment
// For array assignments, generates array.Set(value, indices...) calls
func (tg *TranspileToGo) transformAssignment(assign *f90.AssignmentStmt) ast.Stmt {
	// Check if LHS is an array reference - need to generate .Set() call
	if arrayRef, ok := assign.Target.(*f90.ArrayRef); ok {
		return tg.transformArrayAssignment(arrayRef, assign.Value)
	}

	// Check for FunctionCall that's actually an array reference (parser ambiguity)
	if funcCall, ok := assign.Target.(*f90.FunctionCall); ok {
		_, isArray := tg.arrays[funcCall.Name]
		_, isChar := tg.charLengths[funcCall.Name]
		if isArray || isChar {
			// Convert to ArrayRef and generate .Set() or .SetRange() call
			// (handles both array element and CHARACTER substring assignment)
			arrayRef := &f90.ArrayRef{
				Name:       funcCall.Name,
				Subscripts: funcCall.Args,
			}
			return tg.transformArrayAssignment(arrayRef, assign.Value)
		}
	}

	// Regular assignment (not to array element)
	lhs := tg.transformExpression(assign.Target)
	rhs := tg.transformExpression(assign.Value)

	if lhs == nil || rhs == nil {
		return nil
	}

	// Check if LHS is a pointer parameter (OUT/INOUT scalar) - need to dereference
	if ident, ok := lhs.(*ast.Ident); ok {
		// Check if this is a CHARACTER assignment - use SetFromString() method
		if _, isChar := tg.charLengths[ident.Name]; isChar {
			// Generate: name.SetFromString(value)
			return &ast.ExprStmt{
				X: &ast.CallExpr{
					Fun: &ast.SelectorExpr{
						X:   lhs,
						Sel: ast.NewIdent("SetFromString"),
					},
					Args: []ast.Expr{rhs},
				},
			}
		}
		// Check if this is a pointer parameter - need to dereference for assignment
		if tg.pointerParams[strings.ToUpper(ident.Name)] {
			lhs = &ast.StarExpr{X: lhs}
		}
	}

	return &ast.AssignStmt{
		Lhs: []ast.Expr{lhs},
		Tok: token.ASSIGN,
		Rhs: []ast.Expr{rhs},
	}
}

// transformArrayAssignment generates array.Set(value, indices...) call
func (tg *TranspileToGo) transformArrayAssignment(ref *f90.ArrayRef, value f90.Expression) ast.Stmt {
	// Transform RHS value
	rhs := tg.transformExpression(value)
	if rhs == nil {
		return nil
	}

	// Check if this is a CHARACTER variable with substring assignment
	if _, isChar := tg.charLengths[ref.Name]; isChar && len(ref.Subscripts) == 1 {
		// Check if the subscript is a RangeExpr (substring assignment)
		if rangeExpr, isRange := ref.Subscripts[0].(*f90.RangeExpr); isRange {
			// This is a substring assignment: str(start:end) = value
			// Generate: str.SetRange(int(start), int(end), value)

			// Transform start and end expressions
			start := tg.transformExpression(rangeExpr.Start)
			end := tg.transformExpression(rangeExpr.End)
			if start == nil || end == nil {
				return nil
			}

			setRangeCall := &ast.CallExpr{
				Fun: &ast.SelectorExpr{
					X:   ast.NewIdent(ref.Name),
					Sel: ast.NewIdent("SetRange"),
				},
				Args: []ast.Expr{
					&ast.CallExpr{Fun: ast.NewIdent("int"), Args: []ast.Expr{start}},
					&ast.CallExpr{Fun: ast.NewIdent("int"), Args: []ast.Expr{end}},
					rhs,
				},
			}

			return &ast.ExprStmt{X: setRangeCall}
		}
	}

	// Regular array element assignment
	// Transform all indices
	var indices []ast.Expr
	for _, subscript := range ref.Subscripts {
		index := tg.transformExpression(subscript)
		if index == nil {
			return nil
		}
		// Cast to int for Array.Set() which expects int indices
		// (Fortran INTEGER constants/variables map to int32, but array indices are int)
		indices = append(indices, &ast.CallExpr{
			Fun:  ast.NewIdent("int"),
			Args: []ast.Expr{index},
		})
	}

	// Generate: arrayName.Set(value, int(i), int(j), int(k))
	// Note: Fortran indexing preserved - Array.Set() handles it internally
	setCall := &ast.CallExpr{
		Fun: &ast.SelectorExpr{
			X:   ast.NewIdent(ref.Name),
			Sel: ast.NewIdent("Set"),
		},
		Args: append([]ast.Expr{rhs}, indices...),
	}

	return &ast.ExprStmt{X: setCall}
}

// wrapCharArrayToString wraps a CharacterArray expression with .String() method call
// to convert it to a Go string for use in string concatenation expressions.
func (tg *TranspileToGo) wrapCharArrayToString(expr ast.Expr) ast.Expr {
	// If expr is a CharacterArray identifier, wrap with .String() call
	if ident, ok := expr.(*ast.Ident); ok {
		if _, isChar := tg.charLengths[ident.Name]; isChar {
			return &ast.CallExpr{
				Fun: &ast.SelectorExpr{
					X:   ident,
					Sel: ast.NewIdent("String"),
				},
			}
		}
	}
	return expr
}

// transformExpression transforms a single Fortran expression to a Go expression
func (tg *TranspileToGo) transformExpression(expr f90.Expression) ast.Expr {
	switch e := expr.(type) {
	case *f90.StringLiteral:
		return &ast.BasicLit{
			Kind:  token.STRING,
			Value: fmt.Sprintf("%q", e.Value),
		}
	case *f90.IntegerLiteral:
		return &ast.BasicLit{
			Kind:  token.INT,
			Value: tg.cleanIntegerLiteral(e.Raw),
		}
	case *f90.RealLiteral:
		return &ast.BasicLit{
			Kind:  token.FLOAT,
			Value: tg.cleanRealLiteral(e.Raw),
		}
	case *f90.LogicalLiteral:
		// .TRUE. → true, .FALSE. → false
		if e.Value {
			return ast.NewIdent("true")
		}
		return ast.NewIdent("false")
	case *f90.Identifier:
		// Check if this identifier is in a COMMON block
		if blockName, inCommon := tg.commonVars[e.Value]; inCommon {
			block := tg.commonBlocks[blockName]
			return &ast.SelectorExpr{
				X:   ast.NewIdent(block.goVarname()),
				Sel: ast.NewIdent(e.Value),
			}
		}
		return ast.NewIdent(e.Value)
	case *f90.ArrayRef:
		return tg.transformArrayRef(e)
	case *f90.BinaryExpr:
		return tg.transformBinaryExpr(e)
	case *f90.UnaryExpr:
		return tg.transformUnaryExpr(e)
	case *f90.ParenExpr:
		// Parentheses for grouping - just transform the inner expression
		// Go will preserve the necessary parentheses based on operator precedence
		return tg.transformExpression(e.Expr)
	case *f90.FunctionCall:
		// Check if this is actually an array reference (parser ambiguity)
		_, isArray := tg.arrays[e.Name]
		_, isChar := tg.charLengths[e.Name]
		if isArray || isChar {
			// Convert FunctionCall to ArrayRef
			// (handles both array element access and CHARACTER substring operations)
			arrayRef := &f90.ArrayRef{
				Name:       e.Name,
				Subscripts: e.Args,
			}
			return tg.transformArrayRef(arrayRef)
		}
		return tg.transformFunctionCall(e)
	case *f90.ArrayConstructor:
		return tg.transformArrayConstructor(e)
	default:
		// For now, unsupported expressions return nil
		return nil
	}
}

// transformArrayConstructor transforms a Fortran array constructor (/ ... /) to Go array initialization
// Example: (/ 10, 20, 30 /) → intrinsic.NewArrayFromValues([]int32{10, 20, 30})
func (tg *TranspileToGo) transformArrayConstructor(ac *f90.ArrayConstructor) ast.Expr {
	// Transform each value in the constructor
	goValues := make([]ast.Expr, len(ac.Values))
	for i, val := range ac.Values {
		goValues[i] = tg.transformExpression(val)
	}

	// Create slice literal: []T{values...}
	// We'll infer the type from the first element or default to int32
	var elemType ast.Expr = ast.NewIdent("int32") // Default type

	// Create composite literal for the slice
	sliceLit := &ast.CompositeLit{
		Type: &ast.ArrayType{
			Elt: elemType,
		},
		Elts: goValues,
	}

	// Generate: intrinsic.NewArrayFromValues[int32]([]int32{...})
	return &ast.CallExpr{
		Fun: &ast.IndexExpr{
			X:     _astFnNewArrayFromValues,
			Index: elemType,
		},
		Args: []ast.Expr{sliceLit},
	}
}

// transformArrayRef transforms a Fortran array reference to intrinsic.Array.At() call
// Fortran indexing is preserved (1-based) since Array.At() handles it internally
func (tg *TranspileToGo) transformArrayRef(ref *f90.ArrayRef) ast.Expr {
	// Check if this is a CHARACTER variable with substring operation
	if _, isChar := tg.charLengths[ref.Name]; isChar && len(ref.Subscripts) == 1 {
		// Check if the subscript is a RangeExpr (substring operation)
		if rangeExpr, isRange := ref.Subscripts[0].(*f90.RangeExpr); isRange {
			// This is a substring operation: str(start:end)
			// Generate: str.View(start, end).String()

			// Transform start and end expressions
			start := tg.transformExpression(rangeExpr.Start)
			end := tg.transformExpression(rangeExpr.End)
			if start == nil || end == nil {
				return nil
			}

			// Generate: str.View(int(start), int(end)).String()
			return &ast.CallExpr{
				Fun: &ast.SelectorExpr{
					X: &ast.CallExpr{
						Fun: &ast.SelectorExpr{
							X:   ast.NewIdent(ref.Name),
							Sel: ast.NewIdent("View"),
						},
						Args: []ast.Expr{
							&ast.CallExpr{Fun: ast.NewIdent("int"), Args: []ast.Expr{start}},
							&ast.CallExpr{Fun: ast.NewIdent("int"), Args: []ast.Expr{end}},
						},
					},
					Sel: ast.NewIdent("String"),
				},
			}
		}
	}

	// Regular array element access
	// Transform all subscripts to expressions
	var indices []ast.Expr
	for _, subscript := range ref.Subscripts {
		index := tg.transformExpression(subscript)
		if index == nil {
			return nil
		}
		// Cast to int for Array.At() which expects int indices
		// (Fortran INTEGER loop variables map to int32, but array indices are int)
		indices = append(indices, &ast.CallExpr{
			Fun:  ast.NewIdent("int"),
			Args: []ast.Expr{index},
		})
	}

	// Generate: arrayName.At(int(i), int(j), int(k))
	// Note: No index adjustment needed - Array.At() handles Fortran indexing
	return &ast.CallExpr{
		Fun: &ast.SelectorExpr{
			X:   ast.NewIdent(ref.Name),
			Sel: ast.NewIdent("At"),
		},
		Args: indices,
	}
}

// transformBinaryExpr transforms a Fortran binary expression to Go binary expression
func (tg *TranspileToGo) transformBinaryExpr(expr *f90.BinaryExpr) ast.Expr {
	left := tg.transformExpression(expr.Left)
	right := tg.transformExpression(expr.Right)

	if left == nil || right == nil {
		return nil
	}

	// Map Fortran operator to Go operator
	var goOp token.Token
	switch expr.Op {
	case f90token.Plus:
		goOp = token.ADD
	case f90token.Minus:
		goOp = token.SUB
	case f90token.Asterisk:
		goOp = token.MUL
	case f90token.Slash:
		goOp = token.QUO
	case f90token.GT:
		goOp = token.GTR
	case f90token.LT:
		goOp = token.LSS
	case f90token.GE:
		goOp = token.GEQ
	case f90token.LE:
		goOp = token.LEQ
	case f90token.EQ:
		goOp = token.EQL
	case f90token.NE:
		goOp = token.NEQ
	case f90token.AND:
		goOp = token.LAND // .AND. → &&
	case f90token.OR:
		goOp = token.LOR // .OR. → ||
	case f90token.StringConcat:
		// String concatenation: // → +
		// Convert CharacterArray operands to strings using String() method
		left = tg.wrapCharArrayToString(left)
		right = tg.wrapCharArrayToString(right)
		goOp = token.ADD
	default:
		// Unsupported operator
		return nil
	}

	return &ast.BinaryExpr{
		X:  left,
		Op: goOp,
		Y:  right,
	}
}

// transformUnaryExpr transforms a Fortran unary expression to Go unary expression
// Handles .NOT. (logical negation) and unary +/- operators
func (tg *TranspileToGo) transformUnaryExpr(expr *f90.UnaryExpr) ast.Expr {
	operand := tg.transformExpression(expr.Operand)
	if operand == nil {
		return nil
	}

	// Map Fortran unary operator to Go operator
	var goOp token.Token
	switch expr.Op {
	case f90token.NOT:
		goOp = token.NOT // .NOT. → !
	case f90token.Plus:
		goOp = token.ADD // Unary +
	case f90token.Minus:
		goOp = token.SUB // Unary -
	default:
		// Unsupported unary operator
		return nil
	}

	return &ast.UnaryExpr{
		Op: goOp,
		X:  operand,
	}
}

// transformFunctionCall transforms a Fortran function call to Go expression
func (tg *TranspileToGo) transformFunctionCall(call *f90.FunctionCall) ast.Expr {
	// Check if it's a type conversion intrinsic
	funcName := call.Name
	switch funcName {
	case "REAL":
		// REAL(x) → float32(x)
		if len(call.Args) != 1 {
			return nil
		}
		arg := tg.transformExpression(call.Args[0])
		if arg == nil {
			return nil
		}
		return &ast.CallExpr{
			Fun:  ast.NewIdent("float32"),
			Args: []ast.Expr{arg},
		}
	case "INT", "INT32":
		// INT(x) → int32(x)
		if len(call.Args) != 1 {
			return nil
		}
		arg := tg.transformExpression(call.Args[0])
		if arg == nil {
			return nil
		}
		return &ast.CallExpr{
			Fun:  ast.NewIdent("int32"),
			Args: []ast.Expr{arg},
		}
	case "DBLE":
		// DBLE(x) → float64(x)
		if len(call.Args) != 1 {
			return nil
		}
		arg := tg.transformExpression(call.Args[0])
		if arg == nil {
			return nil
		}
		return &ast.CallExpr{
			Fun:  ast.NewIdent("float64"),
			Args: []ast.Expr{arg},
		}
	case "SQRT", "SIN", "COS", "TAN", "ASIN", "ACOS", "ATAN",
		"EXP", "LOG", "LOG10", "SINH", "COSH", "TANH":
		// Math intrinsics: SQRT(x) → intrinsic.SQRT[float32](x)
		if len(call.Args) != 1 {
			return nil
		}
		arg := tg.transformExpression(call.Args[0])
		if arg == nil {
			return nil
		}
		tg.useImport("github.com/soypat/go-fortran/intrinsic")
		// Call: intrinsic.SQRT[float32](arg)
		return &ast.CallExpr{
			Fun: &ast.IndexListExpr{
				X: &ast.SelectorExpr{
					X:   ast.NewIdent("intrinsic"),
					Sel: ast.NewIdent(funcName),
				},
				Indices: []ast.Expr{ast.NewIdent("float32")},
			},
			Args: []ast.Expr{arg},
		}
	case "ABS":
		// ABS can work with signed integers or floats
		// For now, use float32 variant
		if len(call.Args) != 1 {
			return nil
		}
		arg := tg.transformExpression(call.Args[0])
		if arg == nil {
			return nil
		}
		tg.useImport("github.com/soypat/go-fortran/intrinsic")
		return &ast.CallExpr{
			Fun: &ast.IndexListExpr{
				X: &ast.SelectorExpr{
					X:   ast.NewIdent("intrinsic"),
					Sel: ast.NewIdent("ABS"),
				},
				Indices: []ast.Expr{ast.NewIdent("float32")},
			},
			Args: []ast.Expr{arg},
		}
	case "MAX", "MIN":
		// MAX/MIN are variadic: MAX(a, b, c, ...) → intrinsic.MAX[int32](a, b, c, ...)
		if len(call.Args) < 2 {
			return nil
		}
		args := tg.transformExpressions(call.Args)
		tg.useImport("github.com/soypat/go-fortran/intrinsic")
		// Assume int32 for now (would need type resolution for better handling)
		return &ast.CallExpr{
			Fun: &ast.IndexListExpr{
				X: &ast.SelectorExpr{
					X:   ast.NewIdent("intrinsic"),
					Sel: ast.NewIdent(funcName),
				},
				Indices: []ast.Expr{ast.NewIdent("int32")},
			},
			Args: args,
		}
	case "LEN":
		// LEN(str) → int32(str.Len())
		if len(call.Args) != 1 {
			return nil
		}
		arg := tg.transformExpression(call.Args[0])
		if arg == nil {
			return nil
		}
		return &ast.CallExpr{
			Fun: ast.NewIdent("int32"),
			Args: []ast.Expr{
				&ast.CallExpr{
					Fun: &ast.SelectorExpr{
						X:   arg,
						Sel: ast.NewIdent("Len"),
					},
				},
			},
		}
	case "LEN_TRIM":
		// LEN_TRIM(str) → int32(str.LenTrim())
		if len(call.Args) != 1 {
			return nil
		}
		arg := tg.transformExpression(call.Args[0])
		if arg == nil {
			return nil
		}
		return &ast.CallExpr{
			Fun: ast.NewIdent("int32"),
			Args: []ast.Expr{
				&ast.CallExpr{
					Fun: &ast.SelectorExpr{
						X:   arg,
						Sel: ast.NewIdent("LenTrim"),
					},
				},
			},
		}
	case "TRIM":
		// TRIM(str) → str.Trim().String()
		if len(call.Args) != 1 {
			return nil
		}
		arg := tg.transformExpression(call.Args[0])
		if arg == nil {
			return nil
		}
		return &ast.CallExpr{
			Fun: &ast.SelectorExpr{
				X: &ast.CallExpr{
					Fun: &ast.SelectorExpr{
						X:   arg,
						Sel: ast.NewIdent("Trim"),
					},
				},
				Sel: ast.NewIdent("String"),
			},
		}
	case "INDEX":
		// INDEX(str, substring) → int32(str.Index(substring))
		if len(call.Args) != 2 {
			return nil
		}
		str := tg.transformExpression(call.Args[0])
		substr := tg.transformExpression(call.Args[1])
		if str == nil || substr == nil {
			return nil
		}
		return &ast.CallExpr{
			Fun: ast.NewIdent("int32"),
			Args: []ast.Expr{
				&ast.CallExpr{
					Fun: &ast.SelectorExpr{
						X:   str,
						Sel: ast.NewIdent("Index"),
					},
					Args: []ast.Expr{substr},
				},
			},
		}
	case "ADJUSTL":
		// ADJUSTL(str) → str.AdjustL().String()
		if len(call.Args) != 1 {
			return nil
		}
		arg := tg.transformExpression(call.Args[0])
		if arg == nil {
			return nil
		}
		return &ast.CallExpr{
			Fun: &ast.SelectorExpr{
				X: &ast.CallExpr{
					Fun: &ast.SelectorExpr{
						X:   arg,
						Sel: ast.NewIdent("AdjustL"),
					},
				},
				Sel: ast.NewIdent("String"),
			},
		}
	case "ADJUSTR":
		// ADJUSTR(str) → str.AdjustR().String()
		if len(call.Args) != 1 {
			return nil
		}
		arg := tg.transformExpression(call.Args[0])
		if arg == nil {
			return nil
		}
		return &ast.CallExpr{
			Fun: &ast.SelectorExpr{
				X: &ast.CallExpr{
					Fun: &ast.SelectorExpr{
						X:   arg,
						Sel: ast.NewIdent("AdjustR"),
					},
				},
				Sel: ast.NewIdent("String"),
			},
		}

	case "SIZE":
		// SIZE(arr) → int32(arr.Size())
		// SIZE(arr, dim) → int32(arr.SizeDim(dim))
		if len(call.Args) == 0 || len(call.Args) > 2 {
			return nil
		}

		arrArg := tg.transformExpression(call.Args[0])
		if arrArg == nil {
			return nil
		}

		var methodCall *ast.CallExpr
		if len(call.Args) == 1 {
			// SIZE(arr) → arr.Size()
			methodCall = &ast.CallExpr{
				Fun: &ast.SelectorExpr{
					X:   arrArg,
					Sel: ast.NewIdent("Size"),
				},
			}
		} else {
			// SIZE(arr, dim) → arr.SizeDim(int(dim))
			dimArg := tg.transformExpression(call.Args[1])
			if dimArg == nil {
				return nil
			}
			methodCall = &ast.CallExpr{
				Fun: &ast.SelectorExpr{
					X:   arrArg,
					Sel: ast.NewIdent("SizeDim"),
				},
				Args: []ast.Expr{
					&ast.CallExpr{
						Fun:  ast.NewIdent("int"),
						Args: []ast.Expr{dimArg},
					},
				},
			}
		}

		// Wrap with int32 conversion for INTEGER result
		return &ast.CallExpr{
			Fun:  ast.NewIdent("int32"),
			Args: []ast.Expr{methodCall},
		}

	case "SHAPE":
		// SHAPE(arr) → arr.Shape()
		// Returns []int which can be assigned to INTEGER array
		if len(call.Args) != 1 {
			return nil
		}
		arrArg := tg.transformExpression(call.Args[0])
		if arrArg == nil {
			return nil
		}
		return &ast.CallExpr{
			Fun: &ast.SelectorExpr{
				X:   arrArg,
				Sel: ast.NewIdent("Shape"),
			},
		}

	case "LBOUND":
		// LBOUND(arr, dim) → int32(arr.LowerDim(int(dim)))
		// Note: LBOUND without dim returns array, which we don't support yet
		if len(call.Args) != 2 {
			return nil
		}

		arrArg := tg.transformExpression(call.Args[0])
		dimArg := tg.transformExpression(call.Args[1])
		if arrArg == nil || dimArg == nil {
			return nil
		}

		methodCall := &ast.CallExpr{
			Fun: &ast.SelectorExpr{
				X:   arrArg,
				Sel: ast.NewIdent("LowerDim"),
			},
			Args: []ast.Expr{
				&ast.CallExpr{
					Fun:  ast.NewIdent("int"),
					Args: []ast.Expr{dimArg},
				},
			},
		}

		// Wrap with int32 conversion for INTEGER result
		return &ast.CallExpr{
			Fun:  ast.NewIdent("int32"),
			Args: []ast.Expr{methodCall},
		}

	case "UBOUND":
		// UBOUND(arr, dim) → int32(arr.UpperDim(int(dim)))
		// Note: UBOUND without dim returns array, which we don't support yet
		if len(call.Args) != 2 {
			return nil
		}

		arrArg := tg.transformExpression(call.Args[0])
		dimArg := tg.transformExpression(call.Args[1])
		if arrArg == nil || dimArg == nil {
			return nil
		}

		methodCall := &ast.CallExpr{
			Fun: &ast.SelectorExpr{
				X:   arrArg,
				Sel: ast.NewIdent("UpperDim"),
			},
			Args: []ast.Expr{
				&ast.CallExpr{
					Fun:  ast.NewIdent("int"),
					Args: []ast.Expr{dimArg},
				},
			},
		}

		// Wrap with int32 conversion for INTEGER result
		return &ast.CallExpr{
			Fun:  ast.NewIdent("int32"),
			Args: []ast.Expr{methodCall},
		}

	default:
		// For other functions, assume they're user-defined and call directly
		// Transform arguments
		args := tg.transformExpressions(call.Args)
		return &ast.CallExpr{
			Fun:  ast.NewIdent(funcName),
			Args: args,
		}
	}
}

func (tg *TranspileToGo) useImport(pkg string) {
	if !slices.Contains(tg.imports, pkg) {
		tg.imports = append(tg.imports, pkg)
	}
}

// transformCommonStmt processes a COMMON statement and records the block information.
// COMMON blocks will be generated as global structs separately, not as statements within functions.
func (tg *TranspileToGo) transformCommonStmt(stmt *f90.CommonStmt) ast.Stmt {
	// COMMON blocks are already processed during preScanCommonBlocks
	// This statement doesn't generate code in the function body
	// The global structs will be generated separately via AppendCommonDecls
	return nil
}

// transformDataStmt transforms a Fortran DATA statement to Go assignment statements
// DATA statements initialize variables with compile-time constant values
// Fortran: DATA a, b, c / 10, 20, 30 /
// Go:      a = 10; b = 20; c = 30 (as separate statements)
func (tg *TranspileToGo) transformDataStmt(stmt *f90.DataStmt) ast.Stmt {
	// Generate assignment statements for each variable/value pair
	// If there are multiple pairs, we need to return a block statement

	if len(stmt.Variables) == 0 || len(stmt.Values) == 0 {
		return nil
	}

	// Create assignment statements
	var stmts []ast.Stmt
	for i := 0; i < len(stmt.Variables) && i < len(stmt.Values); i++ {
		// Transform the variable (left-hand side)
		lhs := tg.transformExpression(stmt.Variables[i])
		if lhs == nil {
			continue
		}

		// Transform the value (right-hand side)
		rhs := tg.transformExpression(stmt.Values[i])
		if rhs == nil {
			continue
		}

		// Create assignment statement: var = value
		assignStmt := &ast.AssignStmt{
			Lhs: []ast.Expr{lhs},
			Tok: token.ASSIGN,
			Rhs: []ast.Expr{rhs},
		}
		stmts = append(stmts, assignStmt)
	}

	// If only one assignment, return it directly
	if len(stmts) == 1 {
		return stmts[0]
	}

	// If multiple assignments, wrap in a block statement
	if len(stmts) > 1 {
		return &ast.BlockStmt{
			List: stmts,
		}
	}

	return nil
}

// transformArithmeticIfStmt transforms a Fortran arithmetic IF statement to Go if-else chain
// Fortran: IF (expr) neg_label, zero_label, pos_label
// Go:      if expr < 0 { goto neg_label } else if expr == 0 { goto zero_label } else { goto pos_label }
func (tg *TranspileToGo) transformArithmeticIfStmt(stmt *f90.ArithmeticIfStmt) ast.Stmt {
	// Transform the condition expression
	condition := tg.transformExpression(stmt.Condition)
	if condition == nil {
		return nil
	}

	// Generate: if condition < 0 { goto neg_label }
	negCondition := &ast.BinaryExpr{
		X:  condition,
		Op: token.LSS, // <
		Y:  &ast.BasicLit{Kind: token.INT, Value: "0"},
	}

	negBranch := &ast.BranchStmt{
		Tok:   token.GOTO,
		Label: ast.NewIdent("label" + stmt.NegativeLabel),
	}

	// Generate: else if condition == 0 { goto zero_label }
	zeroCondition := &ast.BinaryExpr{
		X:  condition,
		Op: token.EQL, // ==
		Y:  &ast.BasicLit{Kind: token.INT, Value: "0"},
	}

	zeroBranch := &ast.BranchStmt{
		Tok:   token.GOTO,
		Label: ast.NewIdent("label" + stmt.ZeroLabel),
	}

	// Generate: else { goto pos_label }
	posBranch := &ast.BranchStmt{
		Tok:   token.GOTO,
		Label: ast.NewIdent("label" + stmt.PositiveLabel),
	}

	// Build if-else chain
	return &ast.IfStmt{
		Cond: negCondition,
		Body: &ast.BlockStmt{
			List: []ast.Stmt{negBranch},
		},
		Else: &ast.IfStmt{
			Cond: zeroCondition,
			Body: &ast.BlockStmt{
				List: []ast.Stmt{zeroBranch},
			},
			Else: &ast.BlockStmt{
				List: []ast.Stmt{posBranch},
			},
		},
	}
}

// transformComputedGotoStmt transforms a Fortran computed GOTO to Go switch statement
// Fortran: GO TO (10, 20, 30) choice
// Go:      switch choice { case 1: goto label10; case 2: goto label20; case 3: goto label30 }
func (tg *TranspileToGo) transformComputedGotoStmt(stmt *f90.ComputedGotoStmt) ast.Stmt {
	// Transform the index expression
	indexExpr := tg.transformExpression(stmt.Expression)
	if indexExpr == nil {
		return nil
	}

	// Generate switch cases
	var cases []ast.Stmt
	for i, label := range stmt.Labels {
		// case i+1: goto labelN
		caseClause := &ast.CaseClause{
			List: []ast.Expr{
				&ast.BasicLit{
					Kind:  token.INT,
					Value: fmt.Sprintf("%d", i+1), // Fortran uses 1-based indexing
				},
			},
			Body: []ast.Stmt{
				&ast.BranchStmt{
					Tok:   token.GOTO,
					Label: ast.NewIdent("label" + label),
				},
			},
		}
		cases = append(cases, caseClause)
	}

	return &ast.SwitchStmt{
		Tag: indexExpr,
		Body: &ast.BlockStmt{
			List: cases,
		},
	}
}

// transformStopStmt transforms a Fortran STOP statement to Go os.Exit call
// Fortran: STOP [code]
// Go:      os.Exit(code) or os.Exit(0) if no code
func (tg *TranspileToGo) transformStopStmt(stmt *f90.StopStmt) ast.Stmt {
	// Default exit code is 0
	var exitCode ast.Expr = &ast.BasicLit{
		Kind:  token.INT,
		Value: "0",
	}

	// If a code is specified, use it
	if stmt.Code != nil {
		code := tg.transformExpression(stmt.Code)
		if code != nil {
			// If it's a string, we can't use it directly with os.Exit
			// For now, just use 1 for any non-integer code
			// TODO: Could print the string message before exiting
			exitCode = code
		}
	}
	return &ast.ExprStmt{
		X: &ast.CallExpr{
			Fun: &ast.SelectorExpr{
				X:   ast.NewIdent("intrinsic"),
				Sel: ast.NewIdent("Exit"),
			},
			Args: []ast.Expr{exitCode},
		},
	}
}

// preScanCommonBlocks scans statements for COMMON blocks before processing type declarations.
// This allows us to know which variables are in COMMON blocks before we generate declarations.
func (tg *TranspileToGo) preScanCommonBlocks(stmts []f90.Statement) {
	// Clear COMMON variable tracking for this procedure
	// (variables are only COMMON if they appear in a COMMON statement in THIS procedure)
	tg.commonVars = make(map[string]string)
	for _, stmt := range stmts {
		if commonStmt, ok := stmt.(*f90.CommonStmt); ok {
			blockName := commonStmt.BlockName
			if blockName == "" {
				blockName = "__blank__"
			}

			// Get or create COMMON block info
			_, exists := tg.commonBlocks[blockName]
			if !exists {
				// Create fields for each variable (types will be set later)
				var fields []*ast.Field
				for _, varName := range commonStmt.Variables {
					fields = append(fields, &ast.Field{
						Names: []*ast.Ident{ast.NewIdent(varName)},
						Type:  nil, // Will be set when we see the type declaration
					})
				}
				block := &commonBlockInfo{
					Name:   blockName,
					Fields: fields,
				}
				tg.commonBlocks[blockName] = block
			}

			// Record each variable in the COMMON block
			for _, varName := range commonStmt.Variables {
				tg.commonVars[varName] = blockName
			}
		}
	}
}

// Common intrinsic identifiers.
var (
	_astFnNewCharArray = &ast.SelectorExpr{
		X:   ast.NewIdent("intrinsic"),
		Sel: ast.NewIdent("NewCharacterArray"),
	}
	_astFnNewArrayFromValues = &ast.SelectorExpr{
		X:   ast.NewIdent("intrinsic"),
		Sel: ast.NewIdent("NewArrayFromValues"),
	}
	_astTypeCharArray = &ast.SelectorExpr{
		X:   ast.NewIdent("intrinsic"),
		Sel: ast.NewIdent("CharacterArray"),
	}
	_astTypeArray = &ast.SelectorExpr{
		X:   ast.NewIdent("intrinsic"),
		Sel: ast.NewIdent("Array"),
	}
)
