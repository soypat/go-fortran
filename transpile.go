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
	charLengths map[string]int // Track CHARACTER(LEN=n) for padding assignments
}

// Reset initializes the transpiler with a symbol table
func (tg *TranspileToGo) Reset(symTable *symbol.Table) error {
	tg.symTable = symTable
	tg.charLengths = make(map[string]int)
	return nil
}

// TransformSubroutine transforms a Fortran SUBROUTINE to a Go function declaration
func (tg *TranspileToGo) TransformSubroutine(sub *f90.Subroutine) (*ast.FuncDecl, error) {
	// Create function declaration
	funcDecl := &ast.FuncDecl{
		Name: ast.NewIdent(sub.Name),
		Type: &ast.FuncType{
			Params: &ast.FieldList{}, // TODO: handle parameters later
		},
		Body: &ast.BlockStmt{
			List: tg.transformStatements(sub.Body),
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
	tg.useImport("github.com/soypat/go-fortran/intrinsic")

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

// extractIntLiteral extracts an integer value from a simple expression
// Returns 0 if the expression is not a simple integer literal
func (tg *TranspileToGo) extractIntLiteral(expr f90.Expression) int {
	if lit, ok := expr.(*f90.IntegerLiteral); ok {
		return int(lit.Value)
	}
	return 0
}

// transformAssignment transforms a Fortran assignment to Go assignment
func (tg *TranspileToGo) transformAssignment(assign *f90.AssignmentStmt) ast.Stmt {
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
	case *f90.BinaryExpr:
		return tg.transformBinaryExpr(e)
	case *f90.FunctionCall:
		return tg.transformFunctionCall(e)
	default:
		// For now, unsupported expressions return nil
		return nil
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
