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
	symTable    *symbol.Table
	imports     []string
	charLengths map[string]int            // Track CHARACTER(LEN=n) for padding assignments
	arrays      map[string]*f90.ArraySpec // Track array dimensions for initialization
	arrayTypes  map[string]ast.Expr       // Track element types for arrays
}

// Reset initializes the transpiler with a symbol table
func (tg *TranspileToGo) Reset(symTable *symbol.Table) error {
	tg.imports = tg.imports[:0]
	tg.useImport("github.com/soypat/go-fortran/intrinsic")
	tg.symTable = symTable
	tg.charLengths = make(map[string]int)
	return nil
}

// TransformSubroutine transforms a Fortran SUBROUTINE to a Go function declaration
func (tg *TranspileToGo) TransformSubroutine(sub *f90.Subroutine) (*ast.FuncDecl, error) {
	// Reset array tracking for this subroutine
	tg.arrays = make(map[string]*f90.ArraySpec)
	tg.arrayTypes = make(map[string]ast.Expr)

	// Transform body statements
	bodyStmts := tg.transformStatements(sub.Body)

	// Create function declaration
	funcDecl := &ast.FuncDecl{
		Name: ast.NewIdent(sub.Name),
		Type: &ast.FuncType{
			Params: &ast.FieldList{}, // TODO: handle parameters later
		},
		Body: &ast.BlockStmt{
			List: bodyStmts,
		},
	}

	return funcDecl, nil
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
// Fortran: DO var = start, end [, step]
// Go:      for var = start; var <= end; var += step { }
//
// CRITICAL: Fortran DO loops have INCLUSIVE upper bounds, so we use <= not <
// Example: DO i = 1, 5 iterates over i = 1, 2, 3, 4, 5 (includes 5)
//
// IMPORTANT: In Fortran, loop variables are typically declared before the loop
// (e.g., INTEGER :: i), so we use assignment (=) not declaration (:=) in the
// for loop init to avoid redeclaration errors.
func (tg *TranspileToGo) transformDoLoop(loop *f90.DoLoop) ast.Stmt {
	// Transform start, end expressions
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

	// Transform loop body
	body := tg.transformStatements(loop.Body)

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
		goType = ast.NewIdent("string")
		// CHARACTER(LEN=n) is specified per-entity in entity.CharLen
		// We'll handle initialization per-entity below
	default:
		// Unknown type, skip
		return nil
	}

	// Create var declarations for each entity
	specs := make([]ast.Spec, 0, len(decl.Entities))
	for i := range decl.Entities {
		entity := &decl.Entities[i]

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

		// For CHARACTER, check entity-specific length and initialize with spaces
		if decl.TypeSpec == "CHARACTER" && entity.CharLen != nil {
			if length := tg.extractIntLiteral(entity.CharLen); length > 0 {
				// Track CHARACTER length for padding assignments
				tg.charLengths[entity.Name] = length
				// Initialize with spaces
				spaces := strings.Repeat(" ", length)
				spec.Values = []ast.Expr{&ast.BasicLit{
					Kind:  token.STRING,
					Value: fmt.Sprintf("%q", spaces),
				}}
			}
		}

		specs = append(specs, spec)
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

	// Pad string literals when assigning to CHARACTER(LEN=n) variables
	if ident, ok := lhs.(*ast.Ident); ok {
		if charLen, isChar := tg.charLengths[ident.Name]; isChar {
			rhs = tg.padStringLiteral(rhs, charLen)
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
	default:
		// For now, unsupported functions return nil
		return nil
	}
}

func (tg *TranspileToGo) useImport(pkg string) {
	if !slices.Contains(tg.imports, pkg) {
		tg.imports = append(tg.imports, pkg)
	}
}
