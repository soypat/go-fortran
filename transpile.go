package fortran

import (
	"fmt"
	"go/ast"
	"go/token"
	"slices"

	f90 "github.com/soypat/go-fortran/ast"
	"github.com/soypat/go-fortran/symbol"
)

// TranspileToGo transforms Fortran AST to Go AST
type TranspileToGo struct {
	symTable *symbol.Table
	imports  []string
}

// Reset initializes the transpiler with a symbol table
func (tg *TranspileToGo) Reset(symTable *symbol.Table) error {
	tg.symTable = symTable
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
	default:
		// For now, unsupported statements are skipped
		return nil
	}
}

// transformPrint transforms a Fortran PRINT statement to fmt.Print call
// Note: Fortran PRINT * adds a leading space and newline
func (tg *TranspileToGo) transformPrint(print *f90.PrintStmt) ast.Stmt {
	// Transform output expressions
	args := tg.transformExpressions(print.OutputList)

	// Fortran PRINT * adds a leading space before list-directed output
	// If first arg is a string literal, prepend space to it
	// Otherwise add space as separate argument
	if len(args) > 0 {
		if strLit, ok := args[0].(*ast.BasicLit); ok && strLit.Kind == token.STRING {
			// Remove quotes, prepend space, re-quote
			original := strLit.Value[1 : len(strLit.Value)-1] // Remove surrounding quotes
			strLit.Value = fmt.Sprintf(`" %s"`, original)
		}
	}

	// Create fmt.Println call (adds newline at end)
	tg.useImport("fmt")
	return &ast.ExprStmt{
		X: &ast.CallExpr{
			Fun: &ast.SelectorExpr{
				X:   ast.NewIdent("fmt"),
				Sel: ast.NewIdent("Println"),
			},
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
		goType = ast.NewIdent("string")
	default:
		// Unknown type, skip
		return nil
	}

	// Create var declarations for each entity
	specs := make([]ast.Spec, 0, len(decl.Entities))
	for _, entity := range decl.Entities {
		spec := &ast.ValueSpec{
			Names: []*ast.Ident{ast.NewIdent(entity.Name)},
			Type:  goType,
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

// transformAssignment transforms a Fortran assignment to Go assignment
func (tg *TranspileToGo) transformAssignment(assign *f90.AssignmentStmt) ast.Stmt {
	lhs := tg.transformExpression(assign.Target)
	rhs := tg.transformExpression(assign.Value)

	if lhs == nil || rhs == nil {
		return nil
	}

	return &ast.AssignStmt{
		Lhs: []ast.Expr{lhs},
		Tok: token.ASSIGN,
		Rhs: []ast.Expr{rhs},
	}
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
	default:
		// For now, unsupported expressions return nil
		return nil
	}
}

func (tg *TranspileToGo) useImport(pkg string) {
	if !slices.Contains(tg.imports, pkg) {
		tg.imports = append(tg.imports, pkg)
	}
}
