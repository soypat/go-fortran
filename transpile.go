package fortran

import (
	"fmt"
	"go/ast"
	"go/token"
	"slices"
	"strconv"
	"strings"

	f90 "github.com/soypat/go-fortran/ast"
	"github.com/soypat/go-fortran/symbol"
	f90token "github.com/soypat/go-fortran/token"
)

// TranspileToGo transforms Fortran AST to Go AST
type TranspileToGo struct {
	symTable       *symbol.Table
	imports        []string
	charLengths    map[string]int            // Track CHARACTER(LEN=n) for padding assignments
	arrays         map[string]*f90.ArraySpec // Track array dimensions for initialization
	arrayTypes     map[string]ast.Expr       // Track element types for arrays
	parameters     map[string]bool           // Track parameter names to avoid redeclaration
	pointerParams  map[string]bool           // Track OUT/INOUT scalar parameters (need dereference in assignments)
}

// Reset initializes the transpiler with a symbol table
func (tg *TranspileToGo) Reset(symTable *symbol.Table) error {
	tg.imports = tg.imports[:0]
	tg.useImport("github.com/soypat/go-fortran/intrinsic")
	tg.symTable = symTable
	tg.charLengths = make(map[string]int)
	return nil
}

// enterProcedure initializes tracking maps for a function or subroutine
// and marks parameters to avoid redeclaration in the body.
// additionalParams can be used to mark additional names as parameters
// (e.g., function result variable)
func (tg *TranspileToGo) enterProcedure(params []f90.Parameter, additionalParams ...string) {
	// Reset tracking maps
	tg.arrays = make(map[string]*f90.ArraySpec)
	tg.arrayTypes = make(map[string]ast.Expr)
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
			tg.arrayTypes[param.Name] = tg.fortranTypeToGoType(param.Type)
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

// TransformSubroutine transforms a Fortran SUBROUTINE to a Go function declaration
func (tg *TranspileToGo) TransformSubroutine(sub *f90.Subroutine) (*ast.FuncDecl, error) {
	// Initialize procedure tracking
	tg.enterProcedure(sub.Parameters)

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

	// Transform parameters
	paramFields := tg.transformParameters(fn.Parameters)

	// Transform body statements
	bodyStmts := tg.transformStatements(fn.Body)

	// Add return statement if function assigns to its name
	// In Fortran: FACTORIAL = result
	// In Go: return result
	bodyStmts = tg.convertFunctionResultToReturn(fn.Name, bodyStmts)

	// Map Fortran result type to Go type
	resultType := tg.fortranTypeToGoType(fn.ResultType)

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
		// Get the Go type for this parameter
		goType := tg.fortranTypeToGoType(param.Type)

		// Handle arrays - always use intrinsic.Array[T]
		if param.ArraySpec != nil {
			// Array parameters: *intrinsic.Array[T]
			goType = &ast.StarExpr{
				X: &ast.IndexExpr{
					X: &ast.SelectorExpr{
						X:   ast.NewIdent("intrinsic"),
						Sel: ast.NewIdent("Array"),
					},
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

// fortranTypeToGoType maps a Fortran type name to a Go type expression
func (tg *TranspileToGo) fortranTypeToGoType(fortranType string) ast.Expr {
	switch fortranType {
	case "INTEGER":
		return ast.NewIdent("int32")
	case "REAL":
		return ast.NewIdent("float32")
	case "DOUBLE PRECISION":
		return ast.NewIdent("float64")
	case "LOGICAL":
		return ast.NewIdent("bool")
	case "CHARACTER":
		// CHARACTER(LEN=n) maps to intrinsic.CharacterArray
		return &ast.SelectorExpr{
			X:   ast.NewIdent("intrinsic"),
			Sel: ast.NewIdent("CharacterArray"),
		}
	default:
		// Unknown type, default to interface{}
		return ast.NewIdent("interface{}")
	}
}

// transformStatements transforms a slice of Fortran statements to Go statements
func (tg *TranspileToGo) transformStatements(stmts []f90.Statement) []ast.Stmt {
	var goStmts []ast.Stmt

	for _, stmt := range stmts {
		goStmt := tg.transformStatement(stmt)
		if goStmt != nil {
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
	default:
		// For now, unsupported statements are skipped
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

// transformDoLoop transforms a Fortran DO loop to Go for loop
// Handles both counter-controlled and DO WHILE loops:
//
// Counter-controlled:
// Fortran: DO var = start, end [, step]
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

// transformTypeDeclaration transforms a Fortran type declaration to Go var declaration
func (tg *TranspileToGo) transformTypeDeclaration(decl *f90.TypeDeclaration) ast.Stmt {
	// Map Fortran type to Go type
	var goType ast.Expr

	switch decl.TypeSpec {
	case "INTEGER":
		goType = ast.NewIdent("int32")
	case "REAL":
		goType = ast.NewIdent("float32")
	case "DOUBLE PRECISION":
		goType = ast.NewIdent("float64")
	case "LOGICAL":
		goType = ast.NewIdent("bool")
	case "CHARACTER":
		// CHARACTER(LEN=n) maps to intrinsic.CharacterArray
		goType = &ast.SelectorExpr{
			X:   ast.NewIdent("intrinsic"),
			Sel: ast.NewIdent("CharacterArray"),
		}
		// CHARACTER(LEN=n) length is specified per-entity in entity.CharLen
		// We'll handle initialization per-entity below
	default:
		// Unknown type, skip
		return nil
	}

	// Create var declarations for each entity
	specs := make([]ast.Spec, 0, len(decl.Entities))
	for i := range decl.Entities {
		entity := &decl.Entities[i]

		// Skip parameters - they're already declared in function signature
		if tg.parameters[strings.ToUpper(entity.Name)] {
			continue
		}

		// Check if this is an array
		if entity.ArraySpec != nil {
			// Handle array declarations
			spec := tg.transformArrayDeclaration(entity.Name, goType, entity.ArraySpec)
			specs = append(specs, spec)
			continue
		}

		spec := &ast.ValueSpec{
			Names: []*ast.Ident{ast.NewIdent(entity.Name)},
			Type:  goType,
		}

		// For CHARACTER, check entity-specific length and initialize with CharacterArray
		if decl.TypeSpec == "CHARACTER" && entity.CharLen != nil {
			if length := tg.extractIntLiteral(entity.CharLen); length > 0 {
				// Track CHARACTER length for later use in assignments
				tg.charLengths[entity.Name] = length
				// Initialize with intrinsic.NewCharacterArray(length)
				spec.Values = []ast.Expr{
					&ast.CallExpr{
						Fun: &ast.SelectorExpr{
							X:   ast.NewIdent("intrinsic"),
							Sel: ast.NewIdent("NewCharacterArray"),
						},
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

	return &ast.DeclStmt{
		Decl: &ast.GenDecl{
			Tok:   token.VAR,
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
			X: &ast.SelectorExpr{
				X:   ast.NewIdent("intrinsic"),
				Sel: ast.NewIdent("Array"),
			},
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
		if _, isArray := tg.arrays[funcCall.Name]; isArray {
			// Convert to ArrayRef and generate .Set() call
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

// padStringLiteral pads a string literal to the specified length with trailing spaces
func (tg *TranspileToGo) padStringLiteral(expr ast.Expr, targetLen int) ast.Expr {
	if lit, ok := expr.(*ast.BasicLit); ok && lit.Kind == token.STRING {
		// Properly unquote the string to handle escape sequences
		str, err := strconv.Unquote(lit.Value)
		if err != nil {
			return expr // Return original if unquoting fails
		}
		if len(str) < targetLen {
			// Pad with spaces to target length
			str = str + strings.Repeat(" ", targetLen-len(str))
			return &ast.BasicLit{
				Kind:  token.STRING,
				Value: strconv.Quote(str), // Re-quote with proper escaping
			}
		}
	}
	return expr
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
			Value: e.Raw,
		}
	case *f90.RealLiteral:
		return &ast.BasicLit{
			Kind:  token.FLOAT,
			Value: e.Raw,
		}
	case *f90.LogicalLiteral:
		// .TRUE. → true, .FALSE. → false
		if e.Value {
			return ast.NewIdent("true")
		}
		return ast.NewIdent("false")
	case *f90.Identifier:
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
		if _, isArray := tg.arrays[e.Name]; isArray {
			// Convert FunctionCall to ArrayRef
			arrayRef := &f90.ArrayRef{
				Name:       e.Name,
				Subscripts: e.Args,
			}
			return tg.transformArrayRef(arrayRef)
		}
		return tg.transformFunctionCall(e)
	default:
		// For now, unsupported expressions return nil
		return nil
	}
}

// transformArrayRef transforms a Fortran array reference to intrinsic.Array.At() call
// Fortran indexing is preserved (1-based) since Array.At() handles it internally
func (tg *TranspileToGo) transformArrayRef(ref *f90.ArrayRef) ast.Expr {
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
