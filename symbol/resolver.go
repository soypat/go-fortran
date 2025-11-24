package symbol

import (
	"fmt"

	"github.com/soypat/go-fortran/ast"
	"github.com/soypat/go-fortran/token"
)

// TypeResolver walks an AST and resolves all identifier types using
// explicit declarations and implicit typing rules.
type TypeResolver struct {
	table  *SymbolTable
	errors []error
}

// NewTypeResolver creates a new type resolver for the given symbol table.
func NewTypeResolver(table *SymbolTable) *TypeResolver {
	return &TypeResolver{
		table:  table,
		errors: nil,
	}
}

// Resolve performs type resolution on a program AST.
// Returns any errors encountered during resolution.
func (tr *TypeResolver) Resolve(program *ast.Program) []error {
	tr.errors = nil
	ast.Walk(tr, program)
	return tr.errors
}

// Visit implements the ast.Visitor interface for traversing the AST.
func (tr *TypeResolver) Visit(node ast.Node) ast.Visitor {
	if node == nil {
		return nil
	}

	switch n := node.(type) {
	case *ast.ProgramBlock:
		// Enter program scope
		tr.enterScopeForNode(n)
		return tr

	case *ast.Function:
		// Enter function scope
		tr.enterScopeForNode(n)
		return tr

	case *ast.Subroutine:
		// Enter subroutine scope
		tr.enterScopeForNode(n)
		return tr

	case *ast.Module:
		// Enter module scope
		tr.enterScopeForNode(n)
		return tr

	case *ast.Identifier:
		// Resolve identifier type
		tr.resolveIdentifier(n)
		return nil // Don't descend into identifier

	case *ast.FunctionCall:
		// Resolve function call - this handles the function name and arguments
		tr.resolveFunctionCall(n)
		return tr // Continue to resolve arguments

	case *ast.ArrayRef:
		// Resolve array reference
		tr.resolveArrayRef(n)
		return tr // Continue to resolve subscripts

	case *ast.AssignmentStmt:
		// Resolve both sides of assignment
		return tr

	case *ast.BinaryExpr:
		// Will resolve operands when visiting them
		return tr

	case *ast.UnaryExpr:
		// Will resolve operand when visiting it
		return tr
	}

	return tr
}

// enterScopeForNode finds the scope corresponding to a program unit node
func (tr *TypeResolver) enterScopeForNode(node ast.Node) {
	// Find the scope that corresponds to this node
	tr.table.currentScope = tr.findScopeForNode(tr.table.GlobalScope(), node)
}

// findScopeForNode recursively searches for a scope with the given program unit
func (tr *TypeResolver) findScopeForNode(scope *Scope, node ast.Node) *Scope {
	if scope.ProgramUnit() == node {
		return scope
	}

	// Search children
	for _, child := range scope.Children() {
		if found := tr.findScopeForNode(child, node); found != nil {
			return found
		}
	}

	return nil
}

// resolveIdentifier resolves the type of an identifier expression
func (tr *TypeResolver) resolveIdentifier(ident *ast.Identifier) {
	// Look up symbol in current scope
	sym := tr.table.CurrentScope().Lookup(ident.Value)
	if sym != nil {
		// Symbol already declared, mark as used
		sym.markUsed()
		return
	}

	// Symbol not found - apply implicit typing
	scope := tr.table.CurrentScope()
	implicitRules := scope.Implicit()

	// Try to apply implicit type
	resolvedType, err := ApplyImplicitType(ident.Value, implicitRules)
	if err != nil {
		tr.errors = append(tr.errors, fmt.Errorf("at %v: %w", ident.SourcePos(), err))
		return
	}

	// Create new symbol with implicit type
	sym = NewSymbol(ident.Value, SymVariable)
	sym.SetType(resolvedType)
	sym.setImplicit(true)

	// Add to current scope
	if err := scope.Define(sym); err != nil {
		tr.errors = append(tr.errors, fmt.Errorf("at %v: %w", ident.SourcePos(), err))
	}
}

// resolveFunctionCall resolves the type of a function call
func (tr *TypeResolver) resolveFunctionCall(call *ast.FunctionCall) {
	scope := tr.table.CurrentScope()
	sym := scope.Lookup(call.Name)

	if sym != nil {
		// Symbol exists - check if it's really a function
		sym.markUsed()
		switch sym.Kind() {
		case SymFunction, SymIntrinsic:
			// Correct - it's a function
			return
		case SymVariable:
			if sym.ArraySpec() != nil {
				// This is actually an array reference, not a function call
				// Parser couldn't distinguish, but we can now
				// For now, just note it - proper disambiguation would require
				// changing the AST node type
			}
		case SymExternal:
			// External procedure - assume it's a function
			return
		}
		return
	}

	// Not found - check if it's an intrinsic
	intrinsic := tr.table.Intrinsic(call.Name)
	if intrinsic != nil {
		// Create symbol for this intrinsic usage
		sym = NewSymbol(call.Name, SymIntrinsic)
		sym.SetType(&ResolvedType{BaseType: intrinsic.ReturnType()})
		scope.Define(sym)
		return
	}

	// Not found and not intrinsic - might be implicitly typed function
	// Apply implicit typing rules
	implicitRules := scope.Implicit()
	resolvedType, err := ApplyImplicitType(call.Name, implicitRules)
	if err != nil {
		tr.errors = append(tr.errors, fmt.Errorf("at %v: %w", call.SourcePos(), err))
		return
	}

	// Create symbol for implicitly typed function
	sym = NewSymbol(call.Name, SymFunction)
	sym.SetType(resolvedType)
	sym.setImplicit(true)

	if err := scope.Define(sym); err != nil {
		tr.errors = append(tr.errors, fmt.Errorf("at %v: %w", call.SourcePos(), err))
	}
}

// resolveArrayRef resolves the type of an array reference
func (tr *TypeResolver) resolveArrayRef(ref *ast.ArrayRef) {
	scope := tr.table.CurrentScope()
	sym := scope.Lookup(ref.Name)

	if sym != nil {
		// Symbol exists - mark as used
		sym.markUsed()

		// Verify it's actually an array
		if sym.ArraySpec() == nil && sym.Kind() != SymFunction {
			// Not an array - might be function call misidentified as array ref
			// For now, just continue
		}
		return
	}

	// Not found - apply implicit typing (arrays can be implicitly typed too)
	implicitRules := scope.Implicit()
	resolvedType, err := ApplyImplicitType(ref.Name, implicitRules)
	if err != nil {
		tr.errors = append(tr.errors, fmt.Errorf("at %v: %w", ref.SourcePos(), err))
		return
	}

	// Create symbol for implicitly typed array
	// Note: We don't know the array spec yet - that requires more analysis
	sym = NewSymbol(ref.Name, SymVariable)
	sym.SetType(resolvedType)
	sym.setImplicit(true)

	if err := scope.Define(sym); err != nil {
		tr.errors = append(tr.errors, fmt.Errorf("at %v: %w", ref.SourcePos(), err))
	}
}

// ResolveExpressionType computes the result type of an expression.
// This is useful for type checking and code generation.
func (tr *TypeResolver) ResolveExpressionType(expr ast.Expression) *ResolvedType {
	if expr == nil {
		return nil
	}

	scope := tr.table.CurrentScope()

	switch e := expr.(type) {
	case *ast.IntegerLiteral:
		return &ResolvedType{BaseType: "INTEGER"}

	case *ast.RealLiteral:
		return &ResolvedType{BaseType: "REAL"}

	case *ast.StringLiteral:
		return &ResolvedType{
			BaseType: "CHARACTER",
			CharLen:  len(e.Value),
		}

	case *ast.LogicalLiteral:
		return &ResolvedType{BaseType: "LOGICAL"}

	case *ast.Identifier:
		sym := scope.Lookup(e.Value)
		if sym != nil {
			return sym.Type()
		}
		// Try implicit typing
		implicitType, _ := ApplyImplicitType(e.Value, scope.Implicit())
		return implicitType

	case *ast.BinaryExpr:
		return tr.resolveBinaryExprType(e)

	case *ast.UnaryExpr:
		return tr.resolveUnaryExprType(e)

	case *ast.FunctionCall:
		return tr.resolveFunctionCallType(e)

	case *ast.ArrayRef:
		sym := scope.Lookup(e.Name)
		if sym != nil {
			return sym.Type() // Element type is same as array type
		}
		return nil

	case *ast.ParenExpr:
		return tr.ResolveExpressionType(e.Expr)

	default:
		return nil
	}
}

// resolveBinaryExprType determines the result type of a binary expression
func (tr *TypeResolver) resolveBinaryExprType(expr *ast.BinaryExpr) *ResolvedType {
	leftType := tr.ResolveExpressionType(expr.Left)
	rightType := tr.ResolveExpressionType(expr.Right)

	if leftType == nil || rightType == nil {
		return nil
	}

	// Handle different operator types
	switch expr.Op {
	case token.Plus, token.Minus, token.Asterisk, token.Slash, token.DoubleStar:
		// Arithmetic operators - apply type promotion
		return promoteNumericTypes(leftType, rightType)

	case token.StringConcat:
		// String concatenation
		charLen := leftType.CharLen + rightType.CharLen
		return &ResolvedType{
			BaseType: "CHARACTER",
			CharLen:  charLen,
		}

	case token.EQ, token.NE, token.LT, token.LE, token.GT, token.GE:
		// Relational operators - always return LOGICAL
		return &ResolvedType{BaseType: "LOGICAL"}

	case token.AND, token.OR, token.EQV, token.NEQV:
		// Logical operators - return LOGICAL
		return &ResolvedType{BaseType: "LOGICAL"}

	default:
		return nil
	}
}

// resolveUnaryExprType determines the result type of a unary expression
func (tr *TypeResolver) resolveUnaryExprType(expr *ast.UnaryExpr) *ResolvedType {
	operandType := tr.ResolveExpressionType(expr.Operand)
	if operandType == nil {
		return nil
	}

	switch expr.Op {
	case token.Plus, token.Minus:
		// Unary +/- preserves type
		return operandType

	case token.NOT:
		// Logical NOT - return LOGICAL
		return &ResolvedType{BaseType: "LOGICAL"}

	default:
		return operandType
	}
}

// resolveFunctionCallType determines the return type of a function call
func (tr *TypeResolver) resolveFunctionCallType(call *ast.FunctionCall) *ResolvedType {
	scope := tr.table.CurrentScope()
	sym := scope.Lookup(call.Name)

	if sym != nil {
		return sym.Type()
	}

	// Check if it's an intrinsic
	intrinsic := tr.table.Intrinsic(call.Name)
	if intrinsic != nil {
		return &ResolvedType{BaseType: intrinsic.ReturnType()}
	}

	// Try implicit typing
	implicitType, _ := ApplyImplicitType(call.Name, scope.Implicit())
	return implicitType
}

// promoteNumericTypes applies Fortran type promotion rules for mixed-type arithmetic.
// Promotion hierarchy: INTEGER < REAL < DOUBLE PRECISION < COMPLEX
func promoteNumericTypes(t1, t2 *ResolvedType) *ResolvedType {
	rank := func(t *ResolvedType) int {
		switch t.BaseType {
		case "INTEGER":
			return 1
		case "REAL":
			if t.Kind == 8 {
				return 3 // DOUBLE PRECISION (REAL*8)
			}
			return 2
		case "DOUBLE PRECISION":
			return 3
		case "COMPLEX":
			if t.Kind == 8 {
				return 5 // DOUBLE COMPLEX
			}
			return 4
		default:
			return 0
		}
	}

	r1, r2 := rank(t1), rank(t2)

	// Return the higher-ranked type
	if r1 >= r2 {
		return t1
	}
	return t2
}
