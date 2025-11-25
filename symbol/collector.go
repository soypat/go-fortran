package symbol

import (
	"errors"
	"fmt"

	"github.com/soypat/go-fortran/ast"
	"github.com/soypat/go-fortran/token"
)

// DeclarationCollector traverses an AST and populates a SymbolTable with
// all declarations, handling scope management and implicit typing rules.
type DeclarationCollector struct {
	table      *Table
	errors     []error
	scopeStack []ast.Node // Track which nodes opened scopes for proper exit
}

// NewDeclarationCollector creates a new collector for building a symbol table.
func NewDeclarationCollector() *DeclarationCollector {
	return &DeclarationCollector{
		table:  NewSymbolTable(),
		errors: nil,
	}
}

// Collect processes an AST and returns the populated symbol table.
// Any errors encountered during collection are accumulated and returned.
func (dc *DeclarationCollector) Collect(program ast.ProgramUnit) []error {
	dc.errors = nil
	ast.Walk(dc, program)
	if len(dc.errors) != 0 {
		return dc.errors
	}
	return nil
}

func (dc *DeclarationCollector) SymbolTable() (*Table, []error) {
	return dc.table, dc.errors
}

// Visit implements the ast.Visitor interface.
func (dc *DeclarationCollector) Visit(node ast.Node) ast.Visitor {
	if node == nil {
		// Exiting a node - check if we need to exit a scope
		if len(dc.scopeStack) > 0 {
			dc.table.ExitScope()
			dc.scopeStack = dc.scopeStack[:len(dc.scopeStack)-1]
		}
		return nil
	}

	switch n := node.(type) {
	// Program units - create new scopes
	case *ast.ProgramBlock:
		dc.table.EnterScope(n, ScopeProgram)
		dc.scopeStack = append(dc.scopeStack, n)
		return dc

	case *ast.Subroutine:
		dc.table.EnterScope(n, ScopeProcedure)
		dc.scopeStack = append(dc.scopeStack, n)
		// Define the subroutine itself in parent scope
		parentScope := dc.table.CurrentScope().Parent()
		if parentScope != nil {
			sym := NewSymbol(n.Name, SymSubroutine)
			sym.SetDeclNode(n)
			if err := parentScope.Define(sym); err != nil {
				dc.addError(err)
			}
		}
		// Define parameters in the subroutine's scope
		for _, param := range n.Parameters {
			dc.defineParameter(param)
		}
		return dc

	case *ast.Function:
		dc.table.EnterScope(n, ScopeProcedure)
		dc.scopeStack = append(dc.scopeStack, n)
		// Define the function itself in parent scope
		parentScope := dc.table.CurrentScope().Parent()
		if parentScope != nil {
			funcType := &ResolvedType{
				BaseType: n.ResultType,
			}
			sym := NewSymbol(n.Name, SymFunction)
			sym.SetType(funcType)
			sym.SetDeclNode(n)
			if err := parentScope.Define(sym); err != nil {
				dc.addError(err)
			}
		}
		// Define the implicit result variable in function scope
		currentScope := dc.table.CurrentScope()
		if n.ResultVariable != "" {
			// RESULT clause: define the result variable
			resultType := &ResolvedType{
				BaseType: n.ResultType,
			}
			resultSym := NewSymbol(n.ResultVariable, SymVariable)
			resultSym.SetType(resultType)
			resultSym.SetDeclNode(n)
			if err := currentScope.Define(resultSym); err != nil {
				dc.addError(err)
			}
		} else {
			// No RESULT clause: function name is the result variable
			funcType := &ResolvedType{
				BaseType: n.ResultType,
			}
			funcVar := NewSymbol(n.Name, SymVariable)
			funcVar.SetType(funcType)
			funcVar.SetDeclNode(n)
			if err := currentScope.Define(funcVar); err != nil {
				dc.addError(err)
			}
		}
		// Define parameters in the function's scope
		for _, param := range n.Parameters {
			dc.defineParameter(param)
		}
		return dc

	case *ast.Module:
		dc.table.EnterScope(n, ScopeModule)
		dc.scopeStack = append(dc.scopeStack, n)
		// Define the module in parent scope
		parentScope := dc.table.CurrentScope().Parent()
		if parentScope != nil {
			sym := NewSymbol(n.Name, SymModule)
			sym.SetDeclNode(n)
			if err := parentScope.Define(sym); err != nil {
				dc.addError(err)
			}
		}
		// Register module info
		dc.table.modules[normalizeCase(n.Name)] = NewModuleInfo(n.Name, dc.table.CurrentScope())
		return dc

	case *ast.BlockData:
		dc.table.EnterScope(n, ScopeBlock)
		dc.scopeStack = append(dc.scopeStack, n)
		return dc

	// Declaration statements
	case *ast.TypeDeclaration:
		dc.handleTypeDeclaration(n)
		return nil // Don't descend into children

	case *ast.ImplicitStatement:
		dc.handleImplicitStatement(n)
		return nil // Don't descend into children

	case *ast.CommonStmt:
		dc.handleCommonStmt(n)
		return nil // Don't descend into children

	case *ast.ExternalStmt:
		dc.handleExternalStmt(n)
		return nil // Don't descend into children

	case *ast.IntrinsicStmt:
		dc.handleIntrinsicStmt(n)
		return nil // Don't descend into children

	case *ast.DerivedTypeStmt:
		dc.handleDerivedTypeStmt(n)
		return nil // Don't descend into children

	case *ast.UseStatement:
		// TODO: Handle USE statements (Week 4+)
		return nil
	}

	return dc
}

// handleTypeDeclaration processes a type declaration and defines symbols for each entity.
func (dc *DeclarationCollector) handleTypeDeclaration(decl *ast.TypeDeclaration) {
	currentScope := dc.table.CurrentScope()
	for _, entity := range decl.Entities {
		// Determine if this is a PARAMETER
		isParameter := false
		for _, attr := range decl.Attributes {
			if attr == token.PARAMETER {
				isParameter = true
				break
			}
		}

		// Create resolved type
		resolvedType := &ResolvedType{
			BaseType: decl.TypeSpec,
		}

		// Handle KIND parameter
		if decl.KindParam != nil {
			// For now, we just note that KIND exists
			// Full evaluation would require constant expression evaluation
			resolvedType.Kind = 0 // Placeholder
		}

		// Handle CHARACTER length
		if decl.TypeSpec == "CHARACTER" && entity.CharLen != nil {
			// Similar to KIND, full evaluation requires expression evaluation
			resolvedType.CharLen = 0 // Placeholder
		}

		// Check if symbol already exists (e.g., as a parameter from parameter list)
		existingSym := currentScope.Lookup(entity.Name)
		if existingSym != nil {
			// Update existing symbol with more detailed type information
			existingSym.SetType(resolvedType)
			existingSym.SetAttributes(decl.Attributes)
			existingSym.SetArraySpec(entity.ArraySpec)
			existingSym.SetDeclNode(decl)
			existingSym.setImplicit(false)
			continue
		}

		// Create new symbol
		kind := SymVariable
		if isParameter {
			kind = SymParameter
		}

		sym := NewSymbol(entity.Name, kind)
		sym.SetType(resolvedType)
		sym.SetAttributes(decl.Attributes)
		sym.SetArraySpec(entity.ArraySpec)
		sym.SetDeclNode(decl)
		sym.setImplicit(false)

		if err := currentScope.Define(sym); err != nil {
			dc.addError(err)
		}
	}
}

// handleImplicitStatement processes IMPLICIT statements to update typing rules.
func (dc *DeclarationCollector) handleImplicitStatement(stmt *ast.ImplicitStatement) {
	implicit := dc.table.CurrentScope().Implicit()
	if stmt.IsNone {
		// IMPLICIT NONE - disable all implicit typing
		implicit.IsNone = true
		for i := 0; i < 26; i++ {
			implicit.LetterTypes[i] = ""
			implicit.LetterKinds[i] = 0
		}
		return
	}

	// Process each implicit rule
	for _, rule := range stmt.Rules {
		for _, letterRange := range rule.LetterRanges {
			for letter := letterRange.Start; letter <= letterRange.End; letter++ {
				idx := letter - 'A'
				implicit.LetterTypes[idx] = rule.Type
				// KIND evaluation would go here
				implicit.LetterKinds[idx] = 0
			}
		}
	}
}

// handleCommonStmt processes COMMON blocks.
func (dc *DeclarationCollector) handleCommonStmt(stmt *ast.CommonStmt) {
	blockName := stmt.BlockName
	if blockName == "" {
		blockName = "" // Blank common
	}

	normalizedName := normalizeCase(blockName)

	// Get or create common block
	cb := dc.table.CommonBlock(normalizedName)
	if cb == nil {
		cb = NewCommonBlock(blockName)
		dc.table.commonBlocks[normalizedName] = cb
	}

	// Add variables to common block
	currentScope := dc.table.CurrentScope()
	for _, varName := range stmt.Variables {
		cb.AddVariable(varName)

		// Look up or create symbol for this variable
		sym := currentScope.Lookup(varName)
		if sym == nil {
			// Variable not yet declared - will get type from implicit rules
			sym = NewSymbol(varName, SymVariable)
			sym.SetDeclNode(stmt)
			sym.setImplicit(true)
			// Type will be determined by implicit rules or later declaration
			if err := currentScope.Define(sym); err != nil {
				dc.addError(err)
			}
		}

		// Mark variable as being in common
		sym.AddAttribute(token.COMMON)
	}
}

// handleExternalStmt marks procedures as external.
func (dc *DeclarationCollector) handleExternalStmt(stmt *ast.ExternalStmt) {
	currentScope := dc.table.CurrentScope()
	for _, name := range stmt.Names {
		sym := currentScope.Lookup(name)
		if sym == nil {
			// Create symbol for external procedure
			sym = NewSymbol(name, SymExternal)
			sym.SetDeclNode(stmt)
			if err := currentScope.Define(sym); err != nil {
				dc.addError(err)
			}
		} else {
			// Mark existing symbol as external
			// Note: We can't change the Kind after creation with current API
			// This is a limitation we'll need to address if needed
			sym.AddAttribute(token.EXTERNAL)
		}
	}
}

// handleIntrinsicStmt marks procedures as intrinsic.
func (dc *DeclarationCollector) handleIntrinsicStmt(stmt *ast.IntrinsicStmt) {
	currentScope := dc.table.CurrentScope()
	for _, name := range stmt.Names {
		// Check if it's a known intrinsic
		intrinsic := dc.table.Intrinsic(name)
		if intrinsic == nil {
			dc.addError(fmt.Errorf("unknown intrinsic: %s", name))
			continue
		}

		// Create symbol referencing the intrinsic
		sym := NewSymbol(name, SymIntrinsic)
		sym.SetDeclNode(stmt)

		// Set type from intrinsic definition
		if intrinsic.ReturnType() != "" {
			sym.SetType(&ResolvedType{
				BaseType: intrinsic.ReturnType(),
			})
		}

		if err := currentScope.Define(sym); err != nil {
			dc.addError(err)
		}
	}
}

// handleDerivedTypeStmt processes derived type definitions.
func (dc *DeclarationCollector) handleDerivedTypeStmt(stmt *ast.DerivedTypeStmt) {
	currentScope := dc.table.CurrentScope()
	// Define the type itself
	sym := NewSymbol(stmt.Name, SymDerivedType)
	sym.SetDeclNode(stmt)
	if err := currentScope.Define(sym); err != nil {
		dc.addError(err)
	}

	// Components are handled separately if needed
	// (typically not added to symbol table as standalone symbols)
}

// defineParameter creates a symbol for a function/subroutine parameter.
func (dc *DeclarationCollector) defineParameter(param ast.Parameter) {
	currentScope := dc.table.CurrentScope()
	paramType := &ResolvedType{
		BaseType: param.Type,
	}

	sym := NewSymbol(param.Name, SymVariable)
	sym.SetType(paramType)
	sym.SetAttributes(param.Attributes)
	sym.SetArraySpec(param.ArraySpec)

	if err := currentScope.Define(sym); err != nil {
		dc.addError(err)
	}
}

// addError accumulates an error.
func (dc *DeclarationCollector) addError(err error) {
	if err != nil {
		dc.errors = append(dc.errors, err)
	}
}

// CollectFromProgram is a convenience function that creates a collector,
// processes a program, and returns the symbol table and any errors.
func CollectFromProgram(program *ast.Program) (*Table, error) {
	collector := NewDeclarationCollector()
	for i := range program.Units {
		collector.Collect(program.Units[i])
		if len(collector.errors) != 0 {
			return collector.table, errors.Join(collector.errors...)
		}
	}
	return collector.table, nil
}
