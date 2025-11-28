package fortran

import (
	"errors"
	"fmt"
	"go/ast"
	"go/token"
	"maps"
	"math"
	"slices"
	"strconv"
	"strings"

	f90 "github.com/soypat/go-fortran/ast"
	"github.com/soypat/go-fortran/symbol"
	f90token "github.com/soypat/go-fortran/token"
)

type transpileError struct {
	msg string
}

func (te *transpileError) Error() string {
	return te.msg
}
func (te transpileError) String() string {
	return te.msg
}

// sanitizeIdent returns a valid Go identifier by capitalizing the first letter if it's a Go keyword
func sanitizeIdent(name string) string {
	if name == "" {
		return name
	}
	// Check if it's a Go keyword (case-insensitive since Fortran is case-insensitive)
	if token.IsKeyword(strings.ToLower(name)) {
		// Capitalize first letter
		return strings.ToUpper(name[:1]) + name[1:]
	}
	return name
}

// VarInfo tracks all information about a variable in the current scope
type VarInfo struct {
	// Core metadata
	Type  ast.Expr     // Go type expression (e.g., ast.NewIdent("int32"))
	Flags symbol.Flags // Variable flags (IsParameter, IsImplicit, IsAllocatable, etc.)

	// COMMON block membership
	InCommonBlock string // Non-empty if variable is in a COMMON block (stores block name)

	// Array information
	ArraySpec   *f90.ArraySpec // Dimension bounds (nil for non-arrays)
	ElementType ast.Expr       // Element type for arrays (nil for scalars)

	// Cray-style POINTER information
	PointerVar string // For pointees: name of the pointer variable that owns this pointee

	// CHARACTER information
	CharLength int // For CHARACTER(LEN=n), stores n (0 for non-CHARACTER)

	// EQUIVALENCE information
	EquivalencePrimary *VarInfo   // Non-nil if this variable is equivalenced to another (points to the primary variable)
	EquivalenceMembers []*VarInfo // Non-nil if this variable is the primary in an equivalence set (contains all secondary variables)

	// Original name casing (for generating correct Go identifiers)
	OriginalName string // Original name as it appeared in Fortran source (before uppercase normalization)
}

// VarInfo guard methods - provide readable type checks

// isScalarPointerParam checks if variable is a scalar pointer parameter (not array).
// Replaces: v != nil && v.Flags&symbol.FlagPointerParam != 0 && !isArrayType(v.Type)
func (v *VarInfo) isScalarPointerParam() bool {
	return v != nil && v.Flags&symbol.FlagPointerParam != 0 && !isArrayType(v.Type)
}

// isArrayPointerParam checks if variable is an array pointer parameter.
// Replaces: v != nil && v.Flags&symbol.FlagPointerParam != 0 && isArrayType(v.Type)
func (v *VarInfo) isArrayPointerParam() bool {
	return v != nil && v.Flags&symbol.FlagPointerParam != 0 && isArrayType(v.Type)
}

// isInCommonBlock checks if variable is in a COMMON block.
// Replaces: v != nil && v.InCommonBlock != ""
func (v *VarInfo) isInCommonBlock() bool {
	return v != nil && v.InCommonBlock != ""
}

// isPointee checks if variable is a pointee (accessed via Cray pointer).
// Replaces: v != nil && v.Flags&symbol.FlagPointee != 0
func (v *VarInfo) isPointee() bool {
	return v != nil && v.Flags&symbol.FlagPointee != 0
}

// IsUndefined checks if the variable doesn't exist (nil pointer).
// Used for checking if a variable lookup failed.
func (v *VarInfo) IsUndefined() bool {
	return v == nil
}

// IsTypeUnresolved checks if the variable is undefined OR its type hasn't been resolved yet.
// Replaces: v == nil || v.Type == nil
// Used primarily for COMMON block fallback logic: if a variable isn't in local scope
// or hasn't had its type determined, we check COMMON blocks.
func (v *VarInfo) IsTypeUnresolved() bool {
	return v.IsUndefined() || v.Type == nil
}

// TranspileToGo transforms Fortran AST to Go AST
type TranspileToGo struct {
	symTable     *symbol.Table
	imports      []string
	vars         map[string]*VarInfo         // Unified variable tracking
	commonBlocks map[string]*commonBlockInfo // COMMON block name -> info (file-level, not reset per procedure)
	errors       []transpileError
}

// Reset initializes the transpiler with a symbol table
func (tg *TranspileToGo) Reset(symTable *symbol.Table) error {
	// Preserve or initialize commonBlocks (file-level scope)
	if tg.commonBlocks == nil {
		tg.commonBlocks = make(map[string]*commonBlockInfo)
	}
	*tg = TranspileToGo{
		symTable:     symTable,
		imports:      tg.imports[:0],
		commonBlocks: tg.commonBlocks, // Preserve COMMON blocks across procedures
		errors:       tg.errors[:0],
	}
	return nil
}

// commonBlockInfo tracks variables in a COMMON block
type commonBlockInfo struct {
	Name       string                    // COMMON block name (empty for blank COMMON)
	Fields     []*ast.Field              // Go struct fields for each variable
	ArraySpecs map[string]*f90.ArraySpec // Array specs for array variables
}

func (cbi commonBlockInfo) goVarname() string {
	return cbi.Name
}

// findFieldByName finds a field in the COMMON block by variable name (case-insensitive).
// Returns nil if not found.
func (cbi *commonBlockInfo) findFieldByName(name string) *ast.Field {
	upperName := strings.ToUpper(name)
	for _, field := range cbi.Fields {
		if len(field.Names) > 0 && field.Names[0].Name == upperName {
			return field
		}
	}
	return nil
}

// getFieldName safely extracts the field name from an ast.Field.
// Returns empty string if the field has no names.
func getFieldName(field *ast.Field) string {
	if len(field.Names) > 0 {
		return field.Names[0].Name
	}
	return ""
}

func (tg *TranspileToGo) getVar(name string) *VarInfo {
	return tg.vars[strings.ToUpper(name)]
}

// markVarUsed marks a variable as used (to suppress "declared and not used" warnings)
func (tg *TranspileToGo) markVarUsed(name string) {
	if v := tg.getVar(name); v != nil {
		v.Flags |= symbol.FlagUsed
	}
}

// markExpressionsUsed walks a Fortran expression tree and marks all referenced
// variables as used. This is called for RHS expressions to track variable usage.
func (tg *TranspileToGo) markExpressionsUsed(expr f90.Expression) {
	if expr == nil {
		return
	}

	switch e := expr.(type) {
	case *f90.Identifier:
		tg.markVarUsed(e.Value)
	case *f90.ArrayRef:
		tg.markVarUsed(e.Name)
		for _, sub := range e.Subscripts {
			tg.markExpressionsUsed(sub)
		}
	case *f90.FunctionCall:
		// Function calls use all their arguments
		for _, arg := range e.Args {
			tg.markExpressionsUsed(arg)
		}
	case *f90.BinaryExpr:
		tg.markExpressionsUsed(e.Left)
		tg.markExpressionsUsed(e.Right)
	case *f90.UnaryExpr:
		tg.markExpressionsUsed(e.Operand)
	case *f90.ParenExpr:
		tg.markExpressionsUsed(e.Expr)
	// Literals don't reference variables, so nothing to mark
	case *f90.IntegerLiteral, *f90.RealLiteral, *f90.StringLiteral, *f90.LogicalLiteral:
		// No variables to mark
	}
}

// varIdentifier returns the Go identifier for accessing a variable.
// Handles regular variables, COMMON block members, pointees, and equivalenced variables.
func (tg *TranspileToGo) varIdentifier(v *VarInfo, name string) string {
	if v == nil {
		return sanitizeIdent(name)
	}

	// If this variable is equivalenced to another (secondary variable),
	// redirect to the primary variable
	if v.EquivalencePrimary != nil {
		return tg.varIdentifier(v.EquivalencePrimary, v.OriginalName)
	}

	// COMMON block variables accessed via struct
	if v.isInCommonBlock() {
		return v.InCommonBlock + "." + strings.ToUpper(name)
	}

	// Pointee variables accessed via pointer
	if v.isPointee() && v.PointerVar != "" {
		return v.PointerVar
	}

	// Regular variable
	return sanitizeIdent(name)
}

// varIsEquivalencedScalar checks if a variable is a scalar that's been equivalenced to an array.
// Such scalars are stored as Pointer[T] and accessed via .At(1).
func (tg *TranspileToGo) varIsEquivalencedScalar(v *VarInfo) bool {
	if v == nil || v.EquivalencePrimary == nil {
		return false
	}
	// If this variable is equivalenced and is a scalar (no ArraySpec),
	// and the primary is an array, then this is a scalar-to-array equivalence
	return v.ArraySpec == nil && v.EquivalencePrimary.ArraySpec != nil
}

// varAccess returns an expression for accessing a variable's value.
// Handles:
// - Regular scalars: varName
// - Arrays with subscripts: varName.At(idx1, idx2, ...)
// - Arrays without subscripts (whole-array): varName
// - COMMON block members: commonBlock.VARNAME or commonBlock.VARNAME.At(...)
// - Equivalenced scalars stored as arrays: varName.At(1)
// - Equivalenced variables: redirects to primary variable
func (tg *TranspileToGo) varAccess(v *VarInfo, name string, subscripts []ast.Expr) ast.Expr {
	baseIdent := tg.varIdentifier(v, name)

	// If no subscripts and not a special case, just return the identifier
	if len(subscripts) == 0 {
		// Check if this is an equivalenced scalar (needs .At(1))
		if tg.varIsEquivalencedScalar(v) {
			// Return baseIdent.At(1)
			return &ast.CallExpr{
				Fun: &ast.SelectorExpr{
					X:   ast.NewIdent(baseIdent),
					Sel: ast.NewIdent("At"),
				},
				Args: []ast.Expr{&ast.BasicLit{Kind: token.INT, Value: "1"}},
			}
		}
		// Check if this is a scalar pointer parameter (needs .At(1))
		if v != nil && v.isScalarPointerParam() {
			return &ast.CallExpr{
				Fun: &ast.SelectorExpr{
					X:   ast.NewIdent(baseIdent),
					Sel: ast.NewIdent("At"),
				},
				Args: []ast.Expr{&ast.BasicLit{Kind: token.INT, Value: "1"}},
			}
		}
		return ast.NewIdent(baseIdent)
	}

	// Array access: baseIdent.At(subscripts...)
	return &ast.CallExpr{
		Fun: &ast.SelectorExpr{
			X:   ast.NewIdent(baseIdent),
			Sel: ast.NewIdent("At"),
		},
		Args: subscripts,
	}
}

// assignVar returns a statement for assigning to a variable.
// Handles:
// - Regular scalars: varName = rhs
// - Arrays with subscripts: varName.Set(idx1, idx2, ..., rhs)
// - COMMON block members: commonBlock.VARNAME = rhs or commonBlock.VARNAME.Set(...)
// - Equivalenced scalars stored as arrays: varName.Set(1, rhs)
// - Equivalenced variables: redirects to primary variable
func (tg *TranspileToGo) assignVar(v *VarInfo, name string, subscripts []ast.Expr, rhs ast.Expr) ast.Stmt {
	baseIdent := tg.varIdentifier(v, name)

	// Scalar assignment without subscripts
	if len(subscripts) == 0 {
		// Check if this is an equivalenced scalar (needs .Set(1, rhs))
		if tg.varIsEquivalencedScalar(v) {
			// Return baseIdent.Set(1, rhs)
			return &ast.ExprStmt{
				X: &ast.CallExpr{
					Fun: &ast.SelectorExpr{
						X:   ast.NewIdent(baseIdent),
						Sel: ast.NewIdent("Set"),
					},
					Args: []ast.Expr{
						&ast.BasicLit{Kind: token.INT, Value: "1"},
						rhs,
					},
				},
			}
		}

		// Regular scalar assignment
		return &ast.AssignStmt{
			Lhs: []ast.Expr{ast.NewIdent(baseIdent)},
			Tok: token.ASSIGN,
			Rhs: []ast.Expr{rhs},
		}
	}

	// Array element assignment: baseIdent.Set(subscripts..., rhs)
	args := append(subscripts, rhs)
	return &ast.ExprStmt{
		X: &ast.CallExpr{
			Fun: &ast.SelectorExpr{
				X:   ast.NewIdent(baseIdent),
				Sel: ast.NewIdent("Set"),
			},
			Args: args,
		},
	}
}

// getOrMakeVar retrieves or creates VarInfo for a variable (normalized to uppercase for Fortran case-insensitivity)
func (tg *TranspileToGo) getOrMakeVar(name string) *VarInfo {
	key := strings.ToUpper(name)
	if v, ok := tg.vars[key]; ok {
		return v
	}
	// Create new entry
	v := &VarInfo{
		OriginalName: name, // Store original casing
	}
	tg.vars[key] = v
	return v
}

// hasVar checks if variable is tracked (normalized to uppercase)
func (tg *TranspileToGo) hasVar(name string) bool {
	_, ok := tg.vars[strings.ToUpper(name)]
	return ok
}

// getImplicitType returns the Go type for a name based on Fortran IMPLICIT rules
// L prefix → bool (LOGICAL)
// I-N → int32 (INTEGER)
// Others → float64 (REAL)
func getImplicitType(name string) ast.Expr {
	if name == "" {
		return nil
	}
	firstLetter := strings.ToUpper(name[:1])[0]
	if firstLetter == 'L' {
		return ast.NewIdent("bool")
	} else if firstLetter >= 'I' && firstLetter <= 'N' {
		return ast.NewIdent("int32")
	}
	return ast.NewIdent("float64")
}

// trackImplicitVariable tracks a variable that may be implicitly typed
// and adds it to the vars map if it's not already declared
func (tg *TranspileToGo) trackImplicitVariable(name string) {
	// Use unified vars map to check if variable already has a type
	v := tg.getOrMakeVar(name)
	if v.Type != nil {
		return // Already has type (explicitly declared, parameter, or already implicit)
	}

	// Skip function result variables (marked as parameters but without type)
	// In Fortran, assignments to function names are converted to return statements
	if v.Flags&symbol.FlagParameter != 0 {
		return // This is a function result variable, don't generate implicit declaration
	}

	// Infer type from first letter using Fortran IMPLICIT rules
	varType := getImplicitType(name)
	if varType != nil {
		// Track in unified vars map
		v.Type = varType
		v.Flags |= symbol.FlagImplicit
	}
}

// prependImplicitVarDecls generates declarations for implicitly-typed variables
// and prepends them to the statement list
func (tg *TranspileToGo) prependImplicitVarDecls(stmts []ast.Stmt) []ast.Stmt {
	// Collect implicit variables from vars map
	var names []string
	for name, v := range tg.vars {
		if v.Flags&symbol.FlagImplicit != 0 {
			names = append(names, name)
		}
	}

	if len(names) == 0 {
		return stmts
	}

	// Sort names for consistent output
	slices.Sort(names)

	// Create var declarations
	var varDecls []ast.Stmt
	for _, name := range names {
		v := tg.getVar(name)

		// Skip secondary equivalenced variables - they'll be declared in generateEquivalenceDecls
		if v.EquivalencePrimary != nil {
			continue
		}

		// Skip COMMON block variables - they're accessed via the global COMMON struct
		if v.isInCommonBlock() {
			// DEBUG
			if strings.Contains(strings.ToUpper(name), "X") || strings.Contains(strings.ToUpper(name), "Y") || strings.Contains(strings.ToUpper(name), "Z") {
				fmt.Printf("DEBUG prependImplicit: Skipping COMMON var with name=%q\n", name)
			}
			continue
		}

		// DEBUG
		if strings.Contains(strings.ToUpper(name), "X") || strings.Contains(strings.ToUpper(name), "Y") || strings.Contains(strings.ToUpper(name), "Z") {
			fmt.Printf("DEBUG prependImplicit: Processing var name=%q\n", name)
		}

		// Check if this is an array from DIMENSION statement
		if v.ArraySpec != nil {
			// Array needs NewArray call, not just type declaration
			// Determine element type from v.Type or use implicit typing
			elemType := v.Type
			if elemType == nil {
				elemType = getImplicitType(name)
			}

			// Get dimensions (sizes) from array bounds
			var dims []ast.Expr
			for _, bound := range v.ArraySpec.Bounds {
				size := tg.transformExpression(bound.Upper)
				if size == nil {
					// If we can't determine size, skip this array
					continue
				}
				dims = append(dims, size)
			}

			if len(dims) == 0 {
				// Can't create array without dimensions, skip
				continue
			}

			// Generate: var NAME = intrinsic.NewArray[elemType](dims...)
			init := tg.astIntrinsicGeneric("NewArray", elemType, dims...)

			decl := &ast.DeclStmt{
				Decl: &ast.GenDecl{
					Tok: token.VAR,
					Specs: []ast.Spec{
						&ast.ValueSpec{
							Names:  []*ast.Ident{ast.NewIdent(name)},
							Values: []ast.Expr{init},
						},
					},
				},
			}
			varDecls = append(varDecls, decl)
		} else {
			// Regular scalar variable
			decl := &ast.DeclStmt{
				Decl: &ast.GenDecl{
					Tok: token.VAR,
					Specs: []ast.Spec{
						&ast.ValueSpec{
							Names: []*ast.Ident{ast.NewIdent(name)},
							Type:  v.Type,
						},
					},
				},
			}
			varDecls = append(varDecls, decl)
		}
	}

	// Generate EQUIVALENCE variable declarations
	eqDecls := tg.generateEquivalenceDecls()

	// Insert equivalence declarations AFTER type declarations but before other statements
	// This ensures primary variables are declared before secondary equivalenced variables

	// Find the index of the last type declaration in stmts
	lastTypeDeclIdx := -1
	for i, stmt := range stmts {
		if _, ok := stmt.(*ast.DeclStmt); ok {
			lastTypeDeclIdx = i
		}
	}

	// Build the result: varDecls, then stmts up to lastTypeDeclIdx, then eqDecls, then rest of stmts
	var result []ast.Stmt
	result = append(result, varDecls...)
	if lastTypeDeclIdx >= 0 {
		// Insert type declarations first
		result = append(result, stmts[:lastTypeDeclIdx+1]...)
		// Then equivalence declarations
		result = append(result, eqDecls...)
		// Then remaining statements
		result = append(result, stmts[lastTypeDeclIdx+1:]...)
	} else {
		// No type declarations found, just prepend everything
		result = append(result, eqDecls...)
		result = append(result, stmts...)
	}

	return result
}

// generateEquivalenceDecls creates variable declarations for EQUIVALENCE sets.
// For each set, the first variable is the "primary" and other variables are
// created as type-punned views using intrinsic.Equivalence.
func (tg *TranspileToGo) generateEquivalenceDecls() []ast.Stmt {
	var decls []ast.Stmt

	// Iterate through all variables to find primaries with equivalence members
	for primaryName, primaryVar := range tg.vars {
		if len(primaryVar.EquivalenceMembers) == 0 {
			continue // Not a primary variable
		}

		// Get primary type (source type for Equivalence)
		var srcType ast.Expr
		if primaryVar.ElementType != nil {
			srcType = primaryVar.ElementType
		} else if primaryVar.Type != nil {
			srcType = primaryVar.Type
		} else {
			srcType = getImplicitType(primaryName)
		}

		// Generate equivalenced views for each member variable
		for _, eqVar := range primaryVar.EquivalenceMembers {
			// Find the name of this secondary variable by searching tg.vars
			var eqName string
			for name, v := range tg.vars {
				if v == eqVar {
					eqName = name
					break
				}
			}
			if eqName == "" {
				continue // Can't generate without a name
			}

			// Get equivalenced variable type (destination)
			var dstType ast.Expr
			if eqVar.ElementType != nil {
				dstType = eqVar.ElementType
			} else if eqVar.Type != nil {
				dstType = eqVar.Type
			} else {
				dstType = getImplicitType(eqName)
			}

			// Generate equivalence declaration
			// If primary is an array, we need to convert it to a Pointer first
			// Arrays: var EQ_VAR = intrinsic.Equivalence[dstType, srcType](PRIMARY.Pointer())
			// Scalars: var EQ_VAR = intrinsic.Equivalence[dstType, srcType](intrinsic.Ptr(&PRIMARY))
			var srcPtr ast.Expr
			if primaryVar.ArraySpec != nil {
				// Primary is an array, get its underlying Pointer using .Pointer() method
				srcPtr = &ast.CallExpr{
					Fun: &ast.SelectorExpr{
						X:   ast.NewIdent(sanitizeIdent(primaryName)),
						Sel: ast.NewIdent("Pointer"),
					},
				}
			} else {
				// Primary is a scalar, take its address and wrap in Ptr
				primaryRef := &ast.UnaryExpr{
					Op: token.AND,
					X:  ast.NewIdent(sanitizeIdent(primaryName)),
				}
				srcPtr = tg.astIntrinsicGeneric("Ptr", srcType, primaryRef)
			}

			eqCall := tg.astGenericCall2("intrinsic", "Equivalence", dstType, srcType, srcPtr)

			decl := &ast.DeclStmt{
				Decl: &ast.GenDecl{
					Tok: token.VAR,
					Specs: []ast.Spec{
						&ast.ValueSpec{
							Names:  []*ast.Ident{ast.NewIdent(sanitizeIdent(eqName))},
							Values: []ast.Expr{eqCall},
						},
					},
				},
			}
			decls = append(decls, decl)

			// Mark equivalenced variable as declared (not implicit anymore)
			eqVar.Flags &^= symbol.FlagImplicit
		}
	}

	return decls
}

// appendUnusedVarSuppressions adds `_ = var` statements for variables that were
// declared but never used (read from), to suppress Go's "declared and not used" errors.
// Inserts them BEFORE the final return statement if present to avoid unreachable code.
func (tg *TranspileToGo) appendUnusedVarSuppressions(stmts []ast.Stmt) []ast.Stmt {
	var unusedVars []string

	// Find all variables that are not marked as FlagUsed
	for name, v := range tg.vars {
		if v.Flags&symbol.FlagUsed == 0 {
			// Skip parameters - Go doesn't complain about unused parameters
			if v.Flags&symbol.FlagParameter != 0 {
				continue
			}
			// Skip variables in COMMON blocks - they're fields, not local vars
			if v.isInCommonBlock() {
				continue
			}
			// Skip pointees - they don't exist as separate Go variables
			if v.isPointee() {
				continue
			}
			unusedVars = append(unusedVars, name)
		}
	}

	if len(unusedVars) == 0 {
		return stmts
	}

	// Sort for consistent output
	slices.Sort(unusedVars)

	// Generate _ = var statements for each unused variable
	var suppressions []ast.Stmt
	for _, name := range unusedVars {
		v := tg.vars[name] // name is already uppercase key
		// Use the original name casing from the Fortran source
		goName := sanitizeIdent(v.OriginalName)
		stmt := &ast.AssignStmt{
			Lhs: []ast.Expr{ast.NewIdent("_")},
			Tok: token.ASSIGN,
			Rhs: []ast.Expr{ast.NewIdent(goName)},
		}
		suppressions = append(suppressions, stmt)
	}

	// If last statement is a return, insert suppressions BEFORE it to avoid unreachable code
	if len(stmts) > 0 {
		if _, isReturn := stmts[len(stmts)-1].(*ast.ReturnStmt); isReturn {
			// Insert before return
			result := make([]ast.Stmt, 0, len(stmts)+len(suppressions))
			result = append(result, stmts[:len(stmts)-1]...)
			result = append(result, suppressions...)
			result = append(result, stmts[len(stmts)-1])
			return result
		}
	}

	// No return at end, append normally
	return append(stmts, suppressions...)
}

// enterProcedure initializes tracking maps for a function or subroutine
// and marks parameters to avoid redeclaration in the body.
// additionalParams can be used to mark additional names as parameters
// (e.g., function result variable)
func (tg *TranspileToGo) enterProcedure(params []f90.Parameter, body []f90.Statement, additionalParams ...string) {
	// Reset vars tracking for this procedure
	tg.vars = make(map[string]*VarInfo)

	// Pre-scan body for COMMON blocks and EQUIVALENCE statements BEFORE processing statements
	// This ensures variables are properly marked before type declarations are transformed
	if body != nil {
		tg.preScanCommonBlocks(body) // Mark COMMON block variables
		tg.preScanEquivalences(body) // Link equivalenced variables
	}

	// Track all parameters in vars map
	for _, param := range params {
		// Skip alternate return specifiers (legacy Fortran feature: SUBROUTINE FOO(X,Y,*))
		if param.Name == "*" {
			continue
		}

		// Get element type
		tp := tg.fortranTypeToGoWithKind(param.Type)
		if tp == nil {
			// If type is undefined, use implicit typing rules
			// This handles cases like: SUBROUTINE FOO(X,Y) with no type declarations
			tp = getImplicitType(param.Name)
			if tp == nil {
				continue
			}
		}

		// Track in unified vars map
		v := tg.getOrMakeVar(param.Name)
		v.Flags |= symbol.FlagParameter

		// Track array parameters so array references work correctly
		if param.ArraySpec != nil {
			// Array type: *intrinsic.Array[elemType]
			arrayType := &ast.StarExpr{
				X: &ast.IndexExpr{
					X:     _astTypeArray,
					Index: tp, // Type parameter
				},
			}
			v.Type = arrayType
			v.ArraySpec = param.ArraySpec
			v.ElementType = tp
		} else {
			// Scalar parameter
			// Scalar OUT/INOUT parameters are pointers, need dereference in assignments
			if param.Intent == f90.IntentOut || param.Intent == f90.IntentInOut {
				v.Flags |= symbol.FlagPointerParam
				v.Type = &ast.IndexExpr{
					X:     _astTypePointer,
					Index: tp,
				}
			} else {
				// INTENT(IN) or default intent - pass by value
				v.Type = tp
			}
		}
	}

	// Mark additional parameters (e.g., function result variable)
	for _, name := range additionalParams {
		v := tg.getOrMakeVar(name)
		v.Flags |= symbol.FlagParameter
		// Note: Type will be set when the variable is explicitly declared
	}
}

func (tg *TranspileToGo) Errors() []transpileError {
	return append([]transpileError{}, tg.errors...)
}

func (tg *TranspileToGo) Err() error {
	if len(tg.errors) > 0 {
		var str strings.Builder
		for i, err := range tg.errors {
			if i > 5 {
				break
			}
			str.WriteString(err.msg)
			str.WriteByte('\n')
		}
		return errors.New(str.String())
	}
	return nil
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

		// Apply default types to implicitly-typed variables (those with nil Type)
		// Fortran default IMPLICIT rules: I-N = INTEGER, A-H,O-Z = REAL
		// Common convention: L prefix = LOGICAL
		validFields := make([]*ast.Field, 0, len(block.Fields))
		for _, field := range block.Fields {
			if field.Type == nil && len(field.Names) > 0 {
				// Apply Fortran default IMPLICIT type based on first letter
				varName := field.Names[0].Name
				if varName != "" {
					firstLetter := strings.ToUpper(varName[:1])[0]
					// Determine element type: L prefix → bool, I-N → int32, others → float64
					var elemType ast.Expr
					if firstLetter == 'L' {
						elemType = ast.NewIdent("bool")
					} else if firstLetter >= 'I' && firstLetter <= 'N' {
						elemType = ast.NewIdent("int32")
					} else {
						elemType = ast.NewIdent("float64")
					}

					// Check if this variable is an array
					if _, isArray := block.ArraySpecs[varName]; isArray {
						// Generate array type: *intrinsic.Array[elemType]
						field.Type = &ast.StarExpr{
							X: &ast.IndexExpr{
								X:     _astTypeArray,
								Index: elemType,
							},
						}
					} else {
						// Scalar type
						field.Type = elemType
					}
				}
			}
			if field.Type != nil {
				validFields = append(validFields, field)
			}
		}

		// Skip empty COMMON blocks
		if len(validFields) == 0 {
			continue
		}

		structType := &ast.StructType{
			Fields: &ast.FieldList{
				List: validFields,
			},
		}

		// Build composite literal for array initialization
		var compositeLitElts []ast.Expr
		for varName, arraySpec := range block.ArraySpecs {
			// Find element type from field (varName and field.Names are both uppercase)
			var elemType ast.Expr
			for _, field := range validFields {
				if len(field.Names) > 0 && field.Names[0].Name == varName {
					if starExpr, ok := field.Type.(*ast.StarExpr); ok {
						if indexExpr, ok := starExpr.X.(*ast.IndexExpr); ok {
							elemType = indexExpr.Index
						}
					}
					break
				}
			}
			if elemType == nil {
				continue
			}

			// Build dims from arraySpec
			var dims []ast.Expr
			for _, bound := range arraySpec.Bounds {
				size := tg.transformExpression(bound.Upper)
				if size == nil {
					continue
				}
				dims = append(dims, size)
			}

			// Skip if no dimensions could be determined
			if len(dims) == 0 {
				continue
			}

			// Create composite literal element: varName: intrinsic.NewArray[elemType](dims...)
			compositeLitElts = append(compositeLitElts, &ast.KeyValueExpr{
				Key:   ast.NewIdent(varName),
				Value: tg.astIntrinsicGeneric("NewArray", elemType, dims...),
			})
		}

		// Create variable declaration with or without composite literal
		var valueSpec *ast.ValueSpec
		if len(compositeLitElts) > 0 {
			// var BLOCKNAME = struct{...}{field: value, ...}
			valueSpec = &ast.ValueSpec{
				Names: []*ast.Ident{ast.NewIdent(block.goVarname())},
				Values: []ast.Expr{
					&ast.CompositeLit{
						Type: structType,
						Elts: compositeLitElts,
					},
				},
			}
		} else {
			// var BLOCKNAME struct{...} (no initialization needed)
			valueSpec = &ast.ValueSpec{
				Names: []*ast.Ident{ast.NewIdent(block.goVarname())},
				Type:  structType,
			}
		}

		decl := &ast.GenDecl{
			Tok:   token.VAR,
			Specs: []ast.Spec{valueSpec},
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
	tg.enterProcedure(sub.Parameters, sub.Body)

	// Transform parameters
	paramFields := tg.transformParameters(sub.Parameters)

	// Transform body statements
	bodyStmts := tg.transformStatements(sub.Body)

	// Add declarations for implicitly-typed variables
	bodyStmts = tg.prependImplicitVarDecls(bodyStmts)

	// Add blank assignments for unused variables to suppress "declared and not used" errors
	bodyStmts = tg.appendUnusedVarSuppressions(bodyStmts)

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

	return funcDecl, tg.Err()
}

// TransformFunction transforms a Fortran FUNCTION to a Go function declaration
// Functions are like subroutines but return a value
func (tg *TranspileToGo) TransformFunction(fn *f90.Function) (*ast.FuncDecl, error) {
	// Initialize procedure tracking
	// Mark function name as additional parameter (Fortran uses it as result variable)
	tg.enterProcedure(fn.Parameters, fn.Body, fn.Name)

	// Transform parameters
	paramFields := tg.transformParameters(fn.Parameters)

	// Transform body statements
	bodyStmts := tg.transformStatements(fn.Body)

	// Add return statement if function assigns to its name
	// In Fortran: FACTORIAL = result
	// In Go: return result
	bodyStmts = tg.convertFunctionResultToReturn(fn.Name, bodyStmts)

	// Add declarations for implicitly-typed variables
	bodyStmts = tg.prependImplicitVarDecls(bodyStmts)

	// Add blank assignments for unused variables to suppress "declared and not used" errors
	bodyStmts = tg.appendUnusedVarSuppressions(bodyStmts)

	// Map Fortran result type to Go type, considering KIND parameter
	resultType := tg.fortranTypeToGoWithKind(fn.Type)
	if resultType == nil {
		if fn.Type.Token.String() != "<undefined>" {
			return nil, fmt.Errorf("unknown fortran result type: %s", fn.Type.Token)
		}
		// Skip functions with undefined types
		return nil, nil
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

	return funcDecl, tg.Err()
}

// TransformProgram transforms a Fortran PROGRAM block to Go declarations
// Returns a list of declarations: func main() {...} and any contained procedures
func (tg *TranspileToGo) TransformProgram(prog *f90.ProgramBlock) ([]ast.Decl, error) {
	// Initialize procedure tracking for PROGRAM scope (no parameters)
	// Pass prog.Body so prescans run before transforming statements
	tg.enterProcedure(nil, prog.Body)

	// Transform all statements in program body
	bodyStmts := tg.transformStatements(prog.Body)

	// Add declarations for implicitly-typed variables
	bodyStmts = tg.prependImplicitVarDecls(bodyStmts)

	// Add blank assignments for unused variables to suppress "declared and not used" errors
	bodyStmts = tg.appendUnusedVarSuppressions(bodyStmts)

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
		// Skip alternate return specifiers (legacy Fortran feature: SUBROUTINE FOO(X,Y,*))
		if param.Name == "*" {
			continue
		}

		// Get the Go type for this parameter, considering KIND
		goType := tg.fortranTypeToGoWithKind(param.Type)
		if goType == nil {
			tokenStr := param.Type.Token.String()
			if tokenStr != "<undefined>" && tokenStr != "TYPE" {
				tg.addError(fmt.Sprintf("unhandled type parameter %s", param.Type.Token))
				continue
			}
			// If type is undefined, use implicit typing rules
			// This handles cases like: SUBROUTINE FOO(X,Y) with no type declarations
			goType = getImplicitType(param.Name)
			if goType == nil {
				continue
			}
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
				// Use intrinsic.Pointer[T] for scalar OUT/INOUT parameters
				goType = &ast.IndexExpr{
					X:     _astTypePointer,
					Index: goType,
				}
			}
			// INTENT(IN) or default intent use pass by value (no pointer)
		}

		// Create parameter field
		field := &ast.Field{
			Names: []*ast.Ident{ast.NewIdent(sanitizeIdent(param.Name))},
			Type:  goType,
		}
		fields = append(fields, field)
	}

	return fields
}

// transformStatements transforms a slice of Fortran statements to Go statements
// Handles labeled statements by wrapping them in LabeledStmt nodes
func (tg *TranspileToGo) transformStatements(stmts []f90.Statement) (goStmts []ast.Stmt) {
	var err error
	for _, stmt := range stmts {
		var goStmt ast.Stmt
		goStmt, err = tg.transformStatement(stmt)

		// Check if the Fortran statement has a label (e.g., "100" in "100 CONTINUE")
		if label := stmt.GetLabel(); label != "" {
			// Even if goStmt is nil (e.g., FORMAT, COMMON statements), we need to
			// generate a label for goto targets. Use empty statement as placeholder.
			if goStmt == nil {
				goStmt = &ast.EmptyStmt{}
			}
			// Wrap the Go statement with a label: labelN:
			goStmt = &ast.LabeledStmt{
				Label: ast.NewIdent("label" + label),
				Stmt:  goStmt,
			}
			goStmts = append(goStmts, goStmt)
		} else if goStmt != nil {
			// No label, but statement exists
			goStmts = append(goStmts, goStmt)
		}

		if err != nil {
			tg.addError(err.Error())
		}
	}
	return goStmts
}

func (tg *TranspileToGo) addError(msg string) {
	tg.errors = append(tg.errors, transpileError{
		msg: msg,
	})
}

// transformStatement transforms a single Fortran statement to a Go statement
func (tg *TranspileToGo) transformStatement(stmt f90.Statement) (gostmt ast.Stmt, err error) {
	switch s := stmt.(type) {
	case *f90.TypeDeclaration:
		gostmt = tg.transformTypeDeclaration(s)
	case *f90.DerivedTypeStmt:
		gostmt = tg.transformDerivedType(s)
	case *f90.AssignmentStmt:
		gostmt = tg.transformAssignment(s)
	case *f90.PrintStmt:
		gostmt = tg.transformPrint(s)
	case *f90.IfStmt:
		gostmt = tg.transformIfStmt(s)
	case *f90.DoLoop:
		gostmt = tg.transformDoLoop(s)
	case *f90.CallStmt:
		gostmt = tg.transformCallStmt(s)
	case *f90.ReturnStmt:
		// RETURN statement in functions will be handled by convertFunctionResultToReturn
		// For now, just generate empty return (will be filled with result value later)
		gostmt = &ast.ReturnStmt{}
	case *f90.CycleStmt:
		// CYCLE → continue
		gostmt = &ast.BranchStmt{
			Tok: token.CONTINUE,
		}
	case *f90.ExitStmt:
		// EXIT → break
		gostmt = &ast.BranchStmt{
			Tok: token.BREAK,
		}
	case *f90.ContinueStmt:
		// CONTINUE → empty statement (no-op in Go)
		// If there's a label, it will be handled by label processing later
		gostmt = &ast.EmptyStmt{}
	case *f90.GotoStmt:
		// GOTO label → goto labelN
		gostmt = &ast.BranchStmt{
			Tok:   token.GOTO,
			Label: ast.NewIdent("label" + s.Target),
		}
	case *f90.AllocateStmt:
		gostmt = tg.transformAllocateStmt(s)
	case *f90.DeallocateStmt:
		gostmt = tg.transformDeallocateStmt(s)
	case *f90.SelectCaseStmt:
		gostmt = tg.transformSelectCaseStmt(s)
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
		gostmt = tg.transformDataStmt(s)
	case *f90.ArithmeticIfStmt:
		gostmt = tg.transformArithmeticIfStmt(s)
	case *f90.ComputedGotoStmt:
		gostmt = tg.transformComputedGotoStmt(s)
	case *f90.StopStmt:
		gostmt = tg.transformStopStmt(s)
	case *f90.WriteStmt:
		gostmt = tg.transformWriteStmt(s)
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
		err = fmt.Errorf("unsupported transpile statement: %T", s)
	}
	// Note: gostmt can be nil for legitimate reasons (COMMON vars, PARAMETERs, type definitions)
	// Only actual errors are returned via err
	return gostmt, err
}

// transformPrint transforms a Fortran PRINT statement to intrinsic.Formatter.Print call
// Note: Fortran PRINT * uses list-directed I/O formatting
func (tg *TranspileToGo) transformPrint(print *f90.PrintStmt) ast.Stmt {
	// Mark all print arguments as used
	for _, expr := range print.OutputList {
		tg.markExpressionsUsed(expr)
	}

	// Transform output expressions
	args := tg.transformExpressions(print.OutputList)

	// Use intrinsic.Print() for Fortran-compatible formatting
	// This handles: leading space, T/F for LOGICAL, field widths, spacing between items

	return &ast.ExprStmt{
		X: tg.astIntrinsicCall("Print", args...),
	}
}

// transformWriteStmt transforms a Fortran WRITE statement
// For now, implements list-directed WRITE (WRITE(*,*) or WRITE(6,*)) as intrinsic.Print
// TODO: Handle formatted output, file units, error handling
func (tg *TranspileToGo) transformWriteStmt(write *f90.WriteStmt) ast.Stmt {
	// For MVP: treat list-directed WRITE to stdout (unit * or 6) as PRINT
	// Check if this is list-directed format (Format is *)
	isListDirected := false
	if ident, ok := write.Format.(*f90.Identifier); ok && ident.Value == "*" {
		isListDirected = true
	}

	// Check if writing to stdout (unit * or 6)
	isStdout := false
	if ident, ok := write.Unit.(*f90.Identifier); ok && ident.Value == "*" {
		isStdout = true
	}
	if lit, ok := write.Unit.(*f90.IntegerLiteral); ok && lit.Raw == "6" {
		isStdout = true
	}

	// For now, only support list-directed WRITE to stdout
	if isListDirected && isStdout {
		args := tg.transformExpressions(write.OutputList)
		return &ast.ExprStmt{
			X: tg.astIntrinsicCall("Print", args...),
		}
	}

	// Formatted or file WRITE not yet supported - generate comment
	return &ast.ExprStmt{
		X: tg.astIntrinsicCall("Print", &ast.BasicLit{
			Kind:  token.STRING,
			Value: `"[WRITE statement - formatted output not yet supported]"`,
		}),
	}
}

// transformIfStmt transforms a Fortran IF statement to Go if/else statement
func (tg *TranspileToGo) transformIfStmt(ifStmt *f90.IfStmt) ast.Stmt {
	// Mark variables in condition as used
	tg.markExpressionsUsed(ifStmt.Condition)

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

		v := tg.getVar(name)
		if v == nil || v.ElementType == nil {
			continue
		}
		elemType := v.ElementType

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
			Lhs: []ast.Expr{ast.NewIdent(sanitizeIdent(name))},
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
			Lhs: []ast.Expr{ast.NewIdent(sanitizeIdent(name))},
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
		// Mark variables in condition as used
		tg.markExpressionsUsed(loop.Start)

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
	// Track loop variable (may be implicitly typed)
	tg.trackImplicitVariable(loop.Var)

	// Mark loop bounds as used
	tg.markExpressionsUsed(loop.Start)
	tg.markExpressionsUsed(loop.End)
	if loop.Step != nil {
		tg.markExpressionsUsed(loop.Step)
	}

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
			Lhs: []ast.Expr{ast.NewIdent(sanitizeIdent(loop.Var))},
			Tok: token.ASSIGN, // = (not :=)
			Rhs: []ast.Expr{start},
		},
		Cond: &ast.BinaryExpr{
			X:  ast.NewIdent(sanitizeIdent(loop.Var)),
			Op: token.LEQ, // <= for inclusive upper bound (Fortran semantics)
			Y:  end,
		},
		Post: &ast.AssignStmt{
			Lhs: []ast.Expr{ast.NewIdent(sanitizeIdent(loop.Var))},
			Tok: token.ADD_ASSIGN, // +=
			Rhs: []ast.Expr{step},
		},
		Body: &ast.BlockStmt{
			List: body,
		},
	}
}

// isArrayType checks if an AST type expression represents an Array type.
// This is used to distinguish between scalar Pointer[T] and array *intrinsic.Array[T] types.
func isArrayType(typ ast.Expr) bool {
	// Check for intrinsic.Array[T]
	if idx, ok := typ.(*ast.IndexExpr); ok {
		if sel, ok := idx.X.(*ast.SelectorExpr); ok {
			return sel.Sel.Name == "Array"
		}
	}
	// Check for *intrinsic.Array[T] (unwrap pointer)
	if star, ok := typ.(*ast.StarExpr); ok {
		return isArrayType(star.X)
	}
	return false
}

// astExprToString converts an AST expression to a string for comparison.
// Used for comparing type expressions (e.g., checking if two Pointer[T] have same T).
func astExprToString(expr ast.Expr) string {
	switch e := expr.(type) {
	case *ast.Ident:
		return e.Name
	case *ast.SelectorExpr:
		return astExprToString(e.X) + "." + e.Sel.Name
	case *ast.StarExpr:
		return "*" + astExprToString(e.X)
	case *ast.IndexExpr:
		return astExprToString(e.X) + "[" + astExprToString(e.Index) + "]"
	default:
		return fmt.Sprintf("%T", expr)
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
		// Mark variables in arguments as used
		tg.markExpressionsUsed(arg)

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
			// Check if this is an array parameter or scalar parameter
			isArray := i < len(params) && params[i].ArraySpec != nil

			// Check if arg is already a pointer parameter (don't wrap again)
			var alreadyPointer bool
			if ident, ok := arg.(*f90.Identifier); ok {
				v := tg.getVar(ident.Value)
				if v.isScalarPointerParam() {
					alreadyPointer = true
				}
			}

			if alreadyPointer {
				// Already Pointer[T], pass as-is (no wrapping needed)
				// goArg = goArg (no change)
			} else if isArray {
				// Array: use & operator (unchanged)
				goArg = &ast.UnaryExpr{
					Op: token.AND,
					X:  goArg,
				}
			} else {
				// Scalar: wrap with intrinsic.Ptr(&arg)
				goArg = &ast.CallExpr{
					Fun: &ast.SelectorExpr{
						X:   ast.NewIdent("intrinsic"),
						Sel: ast.NewIdent("Ptr"),
					},
					Args: []ast.Expr{
						&ast.UnaryExpr{
							Op: token.AND,
							X:  goArg,
						},
					},
				}
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

// fortranTypeToGoWithKind converts Fortran type to Go type, considering KIND parameter
// KIND mappings:
//
//	INTEGER(KIND=1) → int8, INTEGER(KIND=2) → int16
//	INTEGER(KIND=4) → int32, INTEGER(KIND=8) → int64
//	REAL(KIND=4) → float32, REAL(KIND=8) → float64
func (tg *TranspileToGo) fortranTypeToGoWithKind(ft f90.TypeSpec) (goType ast.Expr) {
	// Extract KIND value if present
	kindValue := tg.extractKindValue(ft.KindOrLen)

	switch ft.Token {
	default:
		// Unknown type - skip with warning rather than fail
		tokenStr := ft.Token.String()
		if tokenStr != "<undefined>" && tokenStr != "TYPE" {
			tg.addError(fmt.Sprintf("unable to resolve token as go type: %s", tokenStr))
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
//
// rewriteFloatLit converts Fortran float literals to Go format
// Converts D/d/Q/q exponents (double/quad precision) to e
// Example: 1.0D0 → 1.0e0, 1.23D+02 → 1.23e+02
func (tg *TranspileToGo) rewriteFloatLit(s string) string {
	// Replace D/d/Q/q exponent notation with e notation
	// Fortran: 1.5D+10, 1.0Q0
	// Go:      1.5e+10, 1.0e0
	result := strings.ReplaceAll(s, "D", "e")
	result = strings.ReplaceAll(result, "d", "e")
	result = strings.ReplaceAll(result, "Q", "e")
	result = strings.ReplaceAll(result, "q", "e")
	return result
}

// fortranConstantToGoExpr converts a Fortran constant expression string to Go AST expression
// This is a simple converter for PARAMETER constants that handles basic cases
func (tg *TranspileToGo) fortranConstantToGoExpr(fortranExpr string) ast.Expr {
	fortranExpr = strings.TrimSpace(fortranExpr)

	// Strip inline comments (! and everything after)
	if idx := strings.Index(fortranExpr, "!"); idx >= 0 {
		fortranExpr = strings.TrimSpace(fortranExpr[:idx])
	}

	// Handle unary + or - prefix
	if strings.HasPrefix(fortranExpr, "+") {
		fortranExpr = strings.TrimSpace(fortranExpr[1:])
	}

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
	// Try to parse as float (after converting D/Q exponents to e)
	rewritten := tg.rewriteFloatLit(fortranExpr)
	if _, err := strconv.ParseFloat(rewritten, 64); err == nil {
		// It's a float literal
		return &ast.BasicLit{Kind: token.FLOAT, Value: rewritten}
	}

	// Handle string literals (quoted strings)
	if strings.HasPrefix(fortranExpr, "'") || strings.HasPrefix(fortranExpr, "\"") {
		// Remove Fortran quotes and add Go quotes
		unquoted := strings.Trim(fortranExpr, "'\"")
		return &ast.BasicLit{Kind: token.STRING, Value: strconv.Quote(unquoted)}
	}

	// Handle function calls like SQRT(3.0D0) or ATAN(1.0)
	// Must check that funcName is a simple identifier (no operators or spaces in it)
	if idx := strings.Index(fortranExpr, "("); idx > 0 {
		funcName := strings.TrimSpace(fortranExpr[:idx])
		// Only treat as function call if funcName is a simple identifier (no operators like *, /, etc.)
		if !strings.ContainsAny(funcName, "*/+-") && strings.HasSuffix(fortranExpr, ")") {
			argStr := fortranExpr[idx+1 : len(fortranExpr)-1]
			// Try to recursively evaluate argument as constant expression
			argExpr := tg.fortranConstantToGoExpr(argStr)

			// Try to evaluate intrinsic at compile time for PARAMETER constants
			if lit, ok := argExpr.(*ast.BasicLit); ok && (lit.Kind == token.FLOAT || lit.Kind == token.INT) {
				// Parse the numeric value
				var argVal float64
				var err error
				if lit.Kind == token.FLOAT {
					argVal, err = strconv.ParseFloat(lit.Value, 64)
				} else {
					var intVal int64
					intVal, err = strconv.ParseInt(lit.Value, 10, 64)
					argVal = float64(intVal)
				}
				if err == nil {
					// Try to evaluate the intrinsic function
					if result, ok := tg.evalNumericIntrinsic(funcName, argVal); ok {
						// Return evaluated constant as a float literal
						return &ast.BasicLit{
							Kind:  token.FLOAT,
							Value: strconv.FormatFloat(result, 'e', -1, 64),
						}
					}
				}
			}

			// If we can't evaluate it, return a call expression (will fail for const but ok for var)
			return &ast.CallExpr{
				Fun:  ast.NewIdent(funcName),
				Args: []ast.Expr{argExpr},
			}
		}
	}

	// Handle binary operations: *, /, +, -
	// Process in order of precedence (lower precedence first, so they become parent nodes)
	// Division and multiplication have same precedence
	if strings.Contains(fortranExpr, "/") {
		parts := strings.SplitN(fortranExpr, "/", 2)
		if len(parts) == 2 {
			left := tg.fortranConstantToGoExpr(strings.TrimSpace(parts[0]))
			right := tg.fortranConstantToGoExpr(strings.TrimSpace(parts[1]))
			// Try to evaluate at compile time if both are numeric literals
			if result, ok := tg.evalBinaryOp(left, token.QUO, right); ok {
				return result
			}
			return &ast.BinaryExpr{
				X:  left,
				Op: token.QUO,
				Y:  right,
			}
		}
	}
	if strings.Contains(fortranExpr, "*") && !strings.Contains(fortranExpr, "**") {
		parts := strings.SplitN(fortranExpr, "*", 2)
		if len(parts) == 2 {
			left := tg.fortranConstantToGoExpr(strings.TrimSpace(parts[0]))
			right := tg.fortranConstantToGoExpr(strings.TrimSpace(parts[1]))
			// Try to evaluate at compile time if both are numeric literals
			if result, ok := tg.evalBinaryOp(left, token.MUL, right); ok {
				return result
			}
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
	goType := tg.fortranTypeToGoWithKind(decl.Type.Type)
	if goType == nil {
		return nil
	}
	// Check for special attributes
	isAllocatable := false
	isParameter := false
	for _, attr := range decl.Type.Attributes {
		if attr.Token == f90token.ALLOCATABLE {
			isAllocatable = true
		}
		if attr.Token == f90token.PARAMETER {
			isParameter = true
		}
	}

	// Create var declarations for each entity
	specs := make([]ast.Spec, 0, len(decl.Entities))
	for i := range decl.Entities {
		entity := &decl.Entities[i]

		// Get or create VarInfo for this entity
		v := tg.getOrMakeVar(entity.Name)

		// If this variable is in a COMMON block, record its type
		if v.isInCommonBlock() {
			// DEBUG
			if entity.Name == "x" || entity.Name == "y" || entity.Name == "z" {
				fmt.Printf("DEBUG Skipping %s (in COMMON block %s)\n", entity.Name, v.InCommonBlock)
			}
			blockName := v.InCommonBlock
			if block, exists := tg.commonBlocks[blockName]; exists {
				// Determine the field type
				var fieldType ast.Expr
				if entity.ArraySpec != nil {
					// Array type: *intrinsic.Array[elemType]
					fieldType = &ast.StarExpr{
						X: &ast.IndexExpr{
							X:     _astTypeArray,
							Index: goType,
						},
					}
					v.ArraySpec = entity.ArraySpec
					v.ElementType = goType
					// Update the block's ArraySpecs map (for init function generation)
					block.ArraySpecs[entity.Name] = entity.ArraySpec
				} else {
					// Scalar type
					fieldType = goType
				}

				// Find the field for this variable and set its type (fields use uppercase names)
				upperName := strings.ToUpper(entity.Name)
				for _, field := range block.Fields {
					if len(field.Names) > 0 && field.Names[0].Name == upperName {
						field.Type = fieldType
						break
					}
				}

				// Track in unified vars map
				v.Type = fieldType
			}

			// v.InCommonBlock was already set by preScanCommonBlocks

			// Variables in COMMON blocks will be accessed via the global struct
			// Don't generate local declarations for them
			continue
		}

		// Skip parameters - they're already declared in function signature
		if v.Flags&symbol.FlagParameter != 0 {
			continue
		}

		// Check if this is an array from either:
		// 1. Array spec in the type declaration (REAL, DIMENSION(10) :: arr)
		// 2. Separate DIMENSION statement (DIMENSION arr(10) before this type decl)
		arraySpec := entity.ArraySpec
		if arraySpec == nil && v.ArraySpec != nil {
			// Use array spec from DIMENSION statement
			arraySpec = v.ArraySpec
		}

		if arraySpec != nil {
			if isAllocatable {
				// Mark as allocatable and generate uninitialized pointer declaration
				v.Flags |= symbol.FlagAllocatable
				// Clear implicit flag if it was set by DIMENSION statement
				v.Flags &^= symbol.FlagImplicit

				// Track in unified vars map (Phase 2 migration)
				// Array type: *intrinsic.Array[elemType]
				arrayType := &ast.StarExpr{
					X: &ast.IndexExpr{
						X:     _astTypeArray,
						Index: goType, // Type parameter
					},
				}
				v.Type = arrayType
				v.ElementType = goType
				// ArraySpec is nil for ALLOCATABLE (unknown bounds)

				spec := tg.transformAllocatableArrayDeclaration(entity.Name, goType)
				specs = append(specs, spec)
			} else {
				// Handle regular array declarations

				// Track in unified vars map (Phase 2 migration)
				// Array type: *intrinsic.Array[elemType]
				arrayType := &ast.StarExpr{
					X: &ast.IndexExpr{
						X:     _astTypeArray,
						Index: goType, // Type parameter
					},
				}
				v.Type = arrayType
				v.ArraySpec = arraySpec
				v.ElementType = goType

				// If array was already marked as implicit by DIMENSION statement,
				// clear that flag since we're explicitly declaring it now
				v.Flags &^= symbol.FlagImplicit

				spec := tg.transformArrayDeclaration(entity.Name, goType, arraySpec)
				specs = append(specs, spec)
			}
			continue
		}

		// DEBUG
		if entity.Name == "x" || entity.Name == "y" || entity.Name == "z" {
			fmt.Printf("DEBUG creating ValueSpec for %s, sanitized=%s\n", entity.Name, sanitizeIdent(entity.Name))
		}

		spec := &ast.ValueSpec{
			Names: []*ast.Ident{ast.NewIdent(sanitizeIdent(entity.Name))},
			Type:  goType,
		}

		// Track scalar variable in unified vars map (Phase 2 migration)
		v.Type = goType
		// Note: v.Flags doesn't set FlagImplicit, so it stays false (default)

		// For PARAMETER constants, convert initializer to Go expression
		if isParameter {
			if entity.Init == nil {
				// Strip = or => prefix
				initStr := strings.TrimSpace(string(entity.Init.AppendString(nil)))
				if strings.HasPrefix(initStr, "=") {
					initStr = strings.TrimSpace(initStr[1:])
				} else if strings.HasPrefix(initStr, "=>") {
					initStr = strings.TrimSpace(initStr[2:])
				}

				// Convert Fortran constant expression to Go expression
				spec.Values = []ast.Expr{tg.fortranConstantToGoExpr(initStr)}
			}
		} else if decl.Type.Type.Token == f90token.CHARACTER && entity.Type.Type.KindOrLen != nil {
			// For CHARACTER variables (not constants), initialize with CharacterArray
			if length := tg.extractIntLiteral(entity.Type.Type.KindOrLen); length > 0 {
				// Track CHARACTER length in unified vars map
				v.CharLength = length
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

// transformDerivedType transforms a Fortran TYPE definition to a Go struct
// Example: TYPE :: person ... END TYPE → type person struct { ... }
func (tg *TranspileToGo) transformDerivedType(deriv *f90.DerivedTypeStmt) ast.Stmt {
	// Create struct field list
	fields := &ast.FieldList{
		List: make([]*ast.Field, 0, len(deriv.Components)),
	}

	// Transform each component declaration to struct fields
	for _, comp := range deriv.Components {
		// Get Go type for this component
		goType := tg.fortranTypeToGoWithKind(comp.Type)
		if goType == nil {
			continue // Skip components we can't translate
		}

		// Process each entity in the component declaration
		for _, entity := range comp.Components {
			field := &ast.Field{
				Names: []*ast.Ident{ast.NewIdent(sanitizeIdent(entity.Name))},
				Type:  goType,
			}

			// Handle array components
			if entity.ArraySpec != nil {
				// For struct fields that are arrays, use *intrinsic.Array[T]
				field.Type = &ast.StarExpr{
					X: &ast.IndexExpr{
						X:     _astTypeArray,
						Index: goType,
					},
				}
			}

			fields.List = append(fields.List, field)
		}
	}

	// Create Go type declaration
	return &ast.DeclStmt{
		Decl: &ast.GenDecl{
			Tok: token.TYPE,
			Specs: []ast.Spec{
				&ast.TypeSpec{
					Name: ast.NewIdent(deriv.Name),
					Type: &ast.StructType{
						Fields: fields,
					},
				},
			},
		},
	}
}

// transformArrayDeclaration creates an intrinsic.Array[T] declaration with constructor call
// Generates: var arr *intrinsic.Array[int32] = intrinsic.NewArray[int32](3, 4)
func (tg *TranspileToGo) transformArrayDeclaration(name string, elemType ast.Expr, arraySpec *f90.ArraySpec) *ast.ValueSpec {
	// Track this array in unified vars map for disambiguation (FunctionCall vs ArrayRef)
	v := tg.getOrMakeVar(name)
	v.ArraySpec = arraySpec
	v.ElementType = elemType

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
		Names:  []*ast.Ident{ast.NewIdent(sanitizeIdent(name))},
		Values: []ast.Expr{constructorCall},
	}
}

// transformAllocatableArrayDeclaration generates an uninitialized pointer declaration for ALLOCATABLE arrays
// These will be initialized later by ALLOCATE statements
func (tg *TranspileToGo) transformAllocatableArrayDeclaration(name string, elemType ast.Expr) *ast.ValueSpec {
	// Track this array in unified vars map for disambiguation (FunctionCall vs ArrayRef)
	v := tg.getOrMakeVar(name)
	v.ElementType = elemType
	// v.ArraySpec is nil for ALLOCATABLE (unknown bounds until ALLOCATE)

	// Build type: *intrinsic.Array[elemType]
	arrayType := &ast.StarExpr{
		X: &ast.IndexExpr{
			X:     _astTypeArray,
			Index: elemType, // Type parameter
		},
	}

	// Return uninitialized pointer (will be set by ALLOCATE statement)
	return &ast.ValueSpec{
		Names: []*ast.Ident{ast.NewIdent(sanitizeIdent(name))},
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
		v := tg.getVar(funcCall.Name)
		// Check if it's an array - check both ArraySpec and ElementType
		// (ALLOCATABLE arrays have ElementType but no ArraySpec until allocated)
		isArray := v != nil && (v.ArraySpec != nil || v.ElementType != nil)
		isChar := v != nil && v.CharLength > 0
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

	// Early detection: check if this is a scalar pointer parameter assignment
	// Need to generate param.Set(1, value) instead of *param = value
	var needsSetMethod bool
	var paramName string
	if ident, ok := assign.Target.(*f90.Identifier); ok {
		v := tg.getVar(ident.Value)
		if v.isScalarPointerParam() {
			needsSetMethod = true
			paramName = ident.Value
		}
	}

	if needsSetMethod {
		// Generate: param.Set(1, rhs)
		rhs := tg.transformExpression(assign.Value)
		return &ast.ExprStmt{
			X: tg.astMethodCall(paramName, "Set", tg.astIntLit(1), rhs),
		}
	}

	// Regular assignment (not to array element)
	lhs := tg.transformExpression(assign.Target)
	rhs := tg.transformExpression(assign.Value)

	// Mark RHS variables as used (LHS variables are not "used" in Go's sense - only assigned)
	tg.markExpressionsUsed(assign.Value)

	if lhs == nil || rhs == nil {
		return nil
	}

	// Type conversion: Fortran allows implicit type conversions in assignments
	// Get LHS type for conversion
	var lhsType ast.Expr
	if ident, ok := assign.Target.(*f90.Identifier); ok {
		if v := tg.getVar(ident.Value); v != nil {
			lhsType = v.Type
		}
	}

	// Infer RHS type (best effort)
	rhsType := tg.inferExpressionType(assign.Value)

	// Wrap RHS with type conversion if needed
	if lhsType != nil && rhsType != nil {
		rhs = tg.wrapWithTypeConversion(lhsType, rhsType, rhs)
	}

	// MALLOC type inference: inject type parameter from LHS
	// Check if RHS is intrinsic.MALLOC call and LHS is a pointer variable
	if callExpr, ok := rhs.(*ast.CallExpr); ok {
		if selExpr, ok := callExpr.Fun.(*ast.SelectorExpr); ok {
			if x, ok := selExpr.X.(*ast.Ident); ok && x.Name == "intrinsic" && selExpr.Sel.Name == "MALLOC" {
				// This is an intrinsic.MALLOC call - inject type parameter from LHS
				if lhsIdent, ok := lhs.(*ast.Ident); ok {
					if v := tg.getVar(lhsIdent.Name); v != nil {
						// Extract element type from Pointer[T]
						if idxExpr, ok := v.Type.(*ast.IndexExpr); ok {
							if selExpr, ok := idxExpr.X.(*ast.SelectorExpr); ok {
								if selExpr.Sel.Name == "Pointer" {
									// Found Pointer[T] - inject T as type parameter
									callExpr.Fun = &ast.IndexExpr{
										X:     callExpr.Fun,
										Index: idxExpr.Index,
									}
								}
							}
						}
					}
				}
			}
		}
	}

	// Check if LHS is a pointer parameter (OUT/INOUT scalar) - need to dereference
	if ident, ok := lhs.(*ast.Ident); ok {
		// Check if this is a CHARACTER assignment - use SetFromString() method
		v := tg.getVar(ident.Name)
		if v != nil && v.CharLength > 0 {
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
		// Check if this is an array pointer parameter - need to dereference for assignment
		// (Scalar pointer params are handled above with .Set() method)
		if v.isArrayPointerParam() {
			lhs = &ast.StarExpr{X: lhs}
		}

		// Track implicitly-typed variables (not in any declaration tracking map)
		tg.trackImplicitVariable(ident.Name)

		// Pointer type conversion: use Equivalence for different typed pointers
		// Check if both LHS and RHS are Pointer types with different element types
		lhsVar := tg.getVar(ident.Name)
		if lhsVar != nil {
			// Check if LHS is Pointer[T1]
			if lhsIdx, ok := lhsVar.Type.(*ast.IndexExpr); ok {
				if lhsSel, ok := lhsIdx.X.(*ast.SelectorExpr); ok && lhsSel.Sel.Name == "Pointer" {
					// LHS is Pointer[T1] - check if RHS is also a Pointer
					if rhsIdent, ok := rhs.(*ast.Ident); ok {
						rhsVar := tg.getVar(rhsIdent.Name)
						if rhsVar != nil {
							if rhsIdx, ok := rhsVar.Type.(*ast.IndexExpr); ok {
								if rhsSel, ok := rhsIdx.X.(*ast.SelectorExpr); ok && rhsSel.Sel.Name == "Pointer" {
									// Both are Pointers - check if element types differ
									// Compare element types by converting to string (simple but effective)
									lhsElemType := astExprToString(lhsIdx.Index)
									rhsElemType := astExprToString(rhsIdx.Index)
									if lhsElemType != rhsElemType {
										// Different element types - use Equivalence
										// Generate: intrinsic.Equivalence[LhsType, RhsType](rhs)
										rhs = tg.astIntrinsicGeneric2("Equivalence", lhsIdx.Index, rhsIdx.Index, rhsIdent)
									}
								}
							}
						}
					}
				}
			}
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

	// Type conversion: Get array element type and wrap RHS if needed
	v := tg.getVar(ref.Name)
	if v != nil {
		// Get array element type
		elemType := v.ElementType
		if elemType == nil {
			if extracted, ok := tg.extractArrayElementType(v.Type); ok {
				elemType = extracted
			}
		}

		// Infer RHS type
		rhsType := tg.inferExpressionType(value)

		// Wrap with conversion if needed
		if elemType != nil && rhsType != nil {
			rhs = tg.wrapWithTypeConversion(elemType, rhsType, rhs)
		}
	}

	// Check if this is a CHARACTER variable with substring assignment
	if v != nil && v.CharLength > 0 && len(ref.Subscripts) == 1 {
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

			// Generate CHARACTER array expression - check if in COMMON block
			var charArrayExpr ast.Expr
			if v.isInCommonBlock() {
				blockName := v.InCommonBlock
				block := tg.commonBlocks[blockName]
				charArrayExpr = &ast.SelectorExpr{
					X:   ast.NewIdent(block.goVarname()),
					Sel: ast.NewIdent(strings.ToUpper(ref.Name)), // Use uppercase for field access
				}
			} else {
				charArrayExpr = ast.NewIdent(ref.Name)
			}

			setRangeCall := &ast.CallExpr{
				Fun: &ast.SelectorExpr{
					X:   charArrayExpr,
					Sel: ast.NewIdent("SetRange"),
				},
				Args: []ast.Expr{
					tg.astIntCast(start),
					tg.astIntCast(end),
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
		indices = append(indices, tg.astIntCast(index))
	}

	// Generate array access expression - check if array is in COMMON block
	var arrayExpr ast.Expr
	if v != nil && v.Flags&symbol.FlagPointee != 0 && v.PointerVar != "" {
		// Pointee assignment - use pointer variable's Set() method
		// Example: AA(10) = value where POINTER (NPAA, AA(1)) → NPAA.Set(10, value)
		// Note: Pointer.Set() uses (index, value) order

		// For now, only support 1D pointee arrays (typical case)
		if len(ref.Subscripts) != 1 {
			// Multi-dimensional pointees would need Array() conversion
			return nil
		}

		// Transform subscript
		index := tg.transformExpression(ref.Subscripts[0])
		if index == nil {
			return nil
		}

		// Generate: pointerVar.Set(int(index), value)
		// Note: Set() takes (index, value) unlike Array.Set(value, indices...)
		setCall := &ast.CallExpr{
			Fun: &ast.SelectorExpr{
				X:   ast.NewIdent(v.PointerVar),
				Sel: ast.NewIdent("Set"),
			},
			Args: []ast.Expr{
				tg.astIntCast(index),
				rhs,
			},
		}

		return &ast.ExprStmt{X: setCall}
	} else if v.isInCommonBlock() {
		// Array is in COMMON block: BLOCKNAME.arrayname.Set(...)
		blockName := v.InCommonBlock
		block := tg.commonBlocks[blockName]
		arrayExpr = &ast.SelectorExpr{
			X:   ast.NewIdent(block.goVarname()),
			Sel: ast.NewIdent(strings.ToUpper(ref.Name)), // Use uppercase for field access
		}
	} else {
		// Regular array: arrayname.Set(...)
		arrayExpr = ast.NewIdent(ref.Name)
	}

	// Generate: arrayExpr.Set(value, int(i), int(j), int(k))
	// Note: Fortran indexing preserved - Array.Set() handles it internally
	setCall := &ast.CallExpr{
		Fun: &ast.SelectorExpr{
			X:   arrayExpr,
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
		v := tg.getVar(ident.Name)
		if v != nil && v.CharLength > 0 {
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
			Value: tg.rewriteFloatLit(e.Raw),
		}
	case *f90.LogicalLiteral:
		// .TRUE. → true, .FALSE. → false
		if e.Value {
			return ast.NewIdent("true")
		}
		return ast.NewIdent("false")
	case *f90.Identifier:
		// Use the unified variable access helper that handles all cases:
		// - COMMON block variables
		// - Equivalenced scalars
		// - Scalar pointer parameters
		// - Regular variables
		v := tg.getVar(e.Value)
		return tg.varAccess(v, e.Value, nil)
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
		v := tg.getVar(e.Name)
		isArray := v != nil && v.ElementType != nil
		isChar := v != nil && v.CharLength > 0
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
	// Check if this is a pointee variable (Cray-style POINTER)
	v := tg.getVar(ref.Name)
	if v != nil && v.Flags&symbol.FlagPointee != 0 && v.PointerVar != "" {
		// This is a pointee - access through the pointer variable
		// Example: AA(10) where POINTER (NPAA, AA(1)) → NPAA.At(10)
		// Note: Pointer has At() method for 1-based indexing (handles 1D arrays)

		// For now, only support 1D pointee arrays (typical case)
		if len(ref.Subscripts) != 1 {
			// Multi-dimensional pointees would need Array() conversion
			// Not implemented yet - fall through to error
			return nil
		}

		// Transform subscript
		index := tg.transformExpression(ref.Subscripts[0])
		if index == nil {
			return nil
		}

		// Generate: pointerVar.At(int(index))
		return tg.astMethodCall(v.PointerVar, "At", tg.astIntCast(index))
	}

	// Check if this is a CHARACTER variable with substring operation
	if v != nil && v.CharLength > 0 && len(ref.Subscripts) == 1 {
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

			// Generate CHARACTER array expression - check if in COMMON block
			var charArrayExpr ast.Expr
			if v.isInCommonBlock() {
				blockName := v.InCommonBlock
				block := tg.commonBlocks[blockName]
				charArrayExpr = &ast.SelectorExpr{
					X:   ast.NewIdent(block.goVarname()),
					Sel: ast.NewIdent(strings.ToUpper(ref.Name)), // Use uppercase for field access
				}
			} else {
				charArrayExpr = ast.NewIdent(ref.Name)
			}

			// Generate: charArrayExpr.View(int(start), int(end)).String()
			return &ast.CallExpr{
				Fun: &ast.SelectorExpr{
					X: &ast.CallExpr{
						Fun: &ast.SelectorExpr{
							X:   charArrayExpr,
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

	// Generate array access expression - check if array is in COMMON block
	var arrayExpr ast.Expr
	if v.isInCommonBlock() {
		// Array is in COMMON block: BLOCKNAME.arrayname.At(...)
		blockName := v.InCommonBlock
		block := tg.commonBlocks[blockName]
		arrayExpr = &ast.SelectorExpr{
			X:   ast.NewIdent(block.goVarname()),
			Sel: ast.NewIdent(sanitizeIdent(ref.Name)),
		}
	} else {
		// Regular array: arrayname.At(...)
		arrayExpr = ast.NewIdent(ref.Name)
	}

	// Generate: arrayExpr.At(int(i), int(j), int(k))
	// Note: No index adjustment needed - Array.At() handles Fortran indexing
	return &ast.CallExpr{
		Fun: &ast.SelectorExpr{
			X:   arrayExpr,
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

	// Special handling: Pointer nil checks (ptr == 0 or ptr != 0)
	// Transform to ptr.Data() == nil or ptr.Data() != nil
	if expr.Op == f90token.EQ || expr.Op == f90token.NE {
		// Check if one side is a Pointer variable and the other is 0
		var ptrExpr ast.Expr
		var zeroSide *ast.BasicLit

		if leftIdent, ok := left.(*ast.Ident); ok {
			if rightLit, ok := right.(*ast.BasicLit); ok && rightLit.Value == "0" {
				// Check if left is a Pointer type
				if v := tg.getVar(leftIdent.Name); v != nil {
					if idxExpr, ok := v.Type.(*ast.IndexExpr); ok {
						if selExpr, ok := idxExpr.X.(*ast.SelectorExpr); ok && selExpr.Sel.Name == "Pointer" {
							ptrExpr = left
							zeroSide = rightLit
						}
					}
				}
			}
		} else if rightIdent, ok := right.(*ast.Ident); ok {
			if leftLit, ok := left.(*ast.BasicLit); ok && leftLit.Value == "0" {
				// Check if right is a Pointer type
				if v := tg.getVar(rightIdent.Name); v != nil {
					if idxExpr, ok := v.Type.(*ast.IndexExpr); ok {
						if selExpr, ok := idxExpr.X.(*ast.SelectorExpr); ok && selExpr.Sel.Name == "Pointer" {
							ptrExpr = right
							zeroSide = leftLit
						}
					}
				}
			}
		}

		if ptrExpr != nil && zeroSide != nil {
			// Generate: ptr.Data() == nil or ptr.Data() != nil
			dataCall := &ast.CallExpr{
				Fun: &ast.SelectorExpr{
					X:   ptrExpr,
					Sel: ast.NewIdent("Data"),
				},
				Args: []ast.Expr{},
			}

			nilIdent := ast.NewIdent("nil")
			op := token.EQL
			if expr.Op == f90token.NE {
				op = token.NEQ
			}

			return &ast.BinaryExpr{
				X:  dataCall,
				Op: op,
				Y:  nilIdent,
			}
		}
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
		// Math intrinsics: SQRT(x) → intrinsic.SQRT[float64](x) or intrinsic.SQRT[float32](x)
		if len(call.Args) != 1 {
			return nil
		}
		// Determine precision from argument type
		floatType := "float32" // default
		if lit, ok := call.Args[0].(*f90.RealLiteral); ok {
			// Check if it's a double precision literal (contains D or Q exponent)
			if strings.ContainsAny(lit.Raw, "DdQq") {
				floatType = "float64"
			}
		}
		arg := tg.transformExpression(call.Args[0])
		if arg == nil {
			return nil
		}
		tg.useImport("github.com/soypat/go-fortran/intrinsic")
		// Call: intrinsic.SQRT[float64](arg) or intrinsic.SQRT[float32](arg)
		return &ast.CallExpr{
			Fun: &ast.IndexListExpr{
				X: &ast.SelectorExpr{
					X:   ast.NewIdent("intrinsic"),
					Sel: ast.NewIdent(funcName),
				},
				Indices: []ast.Expr{ast.NewIdent(floatType)},
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

	case "MALLOC":
		// MALLOC(size) → intrinsic.MALLOC(size)
		// External memory allocation function (typically from C library)
		if len(call.Args) != 1 {
			return nil
		}
		arg := tg.transformExpression(call.Args[0])
		if arg == nil {
			return nil
		}
		tg.useImport("github.com/soypat/go-fortran/intrinsic")
		return &ast.CallExpr{
			Fun: &ast.SelectorExpr{
				X:   ast.NewIdent("intrinsic"),
				Sel: ast.NewIdent("MALLOC"),
			},
			Args: []ast.Expr{arg},
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

// createDataAssignment creates an assignment statement for DATA initialization
// Handles CHARACTER variables specially (uses SetFromString method)
func (tg *TranspileToGo) createDataAssignment(varExpr f90.Expression, valExpr f90.Expression) ast.Stmt {
	// Check if this is a simple identifier that we should skip
	// Skip only if variable is not tracked at all (truly undeclared)
	if ident, ok := varExpr.(*f90.Identifier); ok {
		v := tg.getVar(ident.Value)
		if v.IsTypeUnresolved() {
			// Untracked or implicitly-typed variable - skip DATA initialization
			// (These should have explicit type declarations to use DATA)
			return nil
		}
	}

	// Transform the variable (left-hand side)
	lhs := tg.transformExpression(varExpr)
	if lhs == nil {
		return nil
	}

	// Transform the value (right-hand side)
	rhs := tg.transformExpression(valExpr)
	if rhs == nil {
		return nil
	}

	// Check if LHS is a CHARACTER variable - need to use SetFromString() method
	if ident, ok := lhs.(*ast.Ident); ok {
		v := tg.getVar(ident.Name)
		if v != nil && v.CharLength > 0 {
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
	}

	// Regular assignment
	return &ast.AssignStmt{
		Lhs: []ast.Expr{lhs},
		Tok: token.ASSIGN,
		Rhs: []ast.Expr{rhs},
	}
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
		varExpr := stmt.Variables[i]
		valExpr := stmt.Values[i]

		// Check if this is an array element assignment (need to use .Set() method)
		var assignStmt ast.Stmt
		if arrayRef, ok := varExpr.(*f90.ArrayRef); ok {
			// Array element assignment: arr(i) = val → arr.Set(val, i)
			assignStmt = tg.transformArrayAssignment(arrayRef, valExpr)
		} else if funcCall, ok := varExpr.(*f90.FunctionCall); ok {
			// Check if it's actually an array reference (parser ambiguity)
			v := tg.getVar(funcCall.Name)
			isArray := v != nil && v.ElementType != nil
			isChar := v != nil && v.CharLength > 0
			if isArray || isChar {
				// Convert to ArrayRef and generate .Set() or .SetRange() call
				arrayRef := &f90.ArrayRef{
					Name:       funcCall.Name,
					Subscripts: funcCall.Args,
				}
				assignStmt = tg.transformArrayAssignment(arrayRef, valExpr)
			} else {
				// Not an array, treat as regular assignment
				assignStmt = tg.createDataAssignment(varExpr, valExpr)
			}
		} else {
			// Regular scalar variable assignment
			assignStmt = tg.createDataAssignment(varExpr, valExpr)
		}

		if assignStmt != nil {
			stmts = append(stmts, assignStmt)
		}
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

// preScanCommonBlocks scans statements for COMMON and DIMENSION statements before processing type declarations.
// This allows us to know which variables are in COMMON blocks and array dimensions before we generate declarations.
func (tg *TranspileToGo) preScanCommonBlocks(stmts []f90.Statement) {
	// Apply implicit typing to COMMON variables immediately on return.
	defer tg.applyImplicitTypingToCommonVars()
	// Note: COMMON variable tracking is now done via the unified vars map (VarInfo.InCommonBlock)
	// No need to clear a separate commonVars map
	for _, stmt := range stmts {
		if dimStmt, ok := stmt.(*f90.DimensionStmt); ok {
			// Handle DIMENSION statement
			for i, varName := range dimStmt.Variables {
				var arraySpec *f90.ArraySpec
				if i < len(dimStmt.ArraySpecs) {
					arraySpec = dimStmt.ArraySpecs[i]
				}
				if arraySpec != nil {
					// Track array in unified vars map
					v := tg.getOrMakeVar(varName)
					v.ArraySpec = arraySpec

					// Set element type from IMPLICIT rules if not already set
					if v.ElementType == nil {
						v.ElementType = getImplicitType(varName)
					}

					// Mark as implicit so it gets declared
					// Even though this is an array, we still need v.Type set for detection
					// in transformAssignment (it checks v.Type != nil)
					if v.Type == nil {
						v.Type = getImplicitType(varName) // Element type (for detection)
						v.Flags |= symbol.FlagImplicit
					}
				}
			}
		} else if ptrStmt, ok := stmt.(*f90.PointerCrayStmt); ok {
			// Handle Cray-style POINTER statement
			// POINTER (ptr_var, pointee) declares ptr_var as intrinsic.Pointer[T]
			// and pointee as the variable accessed through that pointer
			for _, pair := range ptrStmt.Pointers {
				// Infer element type from pointee name using IMPLICIT rules
				elemType := getImplicitType(pair.Pointee)

				// Create VarInfo for the pointer variable
				ptrVar := tg.getOrMakeVar(pair.PointerVar)
				ptrVar.Type = &ast.IndexExpr{
					X:     _astTypePointer,
					Index: elemType,
				}
				ptrVar.Flags |= symbol.FlagImplicit

				// Track the pointee - it's not a separate variable, but a name
				// that references memory through the pointer
				// We need to track which pointer variable it corresponds to
				pointeeVar := tg.getOrMakeVar(pair.Pointee)
				pointeeVar.Flags |= symbol.FlagPointee
				pointeeVar.PointerVar = pair.PointerVar // Track the corresponding pointer
				pointeeVar.ArraySpec = pair.ArraySpec   // Store array spec if present
				pointeeVar.ElementType = elemType       // Element type from IMPLICIT rules
				pointeeVar.Type = elemType              // Set Type so v.Type != nil check passes
			}
		} else if commonStmt, ok := stmt.(*f90.CommonStmt); ok {
			blockName := commonStmt.BlockName
			if blockName == "" {
				blockName = "__blank__"
			}

			// Get or create COMMON block info
			block, exists := tg.commonBlocks[blockName]
			if !exists {
				// Create fields for each variable (types will be set later)
				var fields []*ast.Field
				arraySpecs := make(map[string]*f90.ArraySpec)
				for i, varName := range commonStmt.Variables {
					// Track array spec if this variable is an array
					var arraySpec *f90.ArraySpec
					if i < len(commonStmt.ArraySpecs) {
						arraySpec = commonStmt.ArraySpecs[i]
					}
					if arraySpec != nil {
						// Store array spec in the block info (use uppercase key for case-insensitive lookup)
						arraySpecs[strings.ToUpper(varName)] = arraySpec
					}

					// Normalize field name to uppercase for case-insensitive access
					fields = append(fields, &ast.Field{
						Names: []*ast.Ident{ast.NewIdent(strings.ToUpper(varName))},
						Type:  nil, // Will be set when we see the type declaration or by IMPLICIT typing
					})
				}
				block = &commonBlockInfo{
					Name:       blockName,
					Fields:     fields,
					ArraySpecs: arraySpecs,
				}
				tg.commonBlocks[blockName] = block
			}

			// Record each variable in the COMMON block and register arrays
			for _, varName := range commonStmt.Variables {
				// Track in unified vars map
				v := tg.getOrMakeVar(varName)
				v.InCommonBlock = blockName
				v.Flags |= symbol.FlagCommon

				// If this variable is an array, register it in the unified vars map
				if arraySpec, isArray := block.ArraySpecs[varName]; isArray {
					v.ArraySpec = arraySpec
					// Note: ElementType will be set when we see the type declaration
				}
			}
		}
	}
}

// preScanEquivalences scans for EQUIVALENCE statements and records them for later code generation.
// EQUIVALENCE makes multiple variables share the same memory location, requiring special handling.
func (tg *TranspileToGo) preScanEquivalences(stmts []f90.Statement) {
	// First, scan all type declarations to find which variables are arrays
	arrayVars := make(map[string]bool) // uppercase name -> is array
	for _, stmt := range stmts {
		if decl, ok := stmt.(*f90.TypeDeclaration); ok {
			for _, entity := range decl.Entities {
				upperName := strings.ToUpper(entity.Name)
				// Check if this variable has an array spec
				arrayVars[upperName] = entity.ArraySpec != nil
			}
		}
	}

	for _, stmt := range stmts {
		if eqStmt, ok := stmt.(*f90.EquivalenceStmt); ok {
			// Process each equivalence set
			for _, set := range eqStmt.Sets {
				if len(set) < 2 {
					continue // Need at least 2 variables to equivalence
				}

				// Extract variable names and get their VarInfo
				var vars []*VarInfo
				var varNames []string
				for _, expr := range set {
					var name string
					switch e := expr.(type) {
					case *f90.Identifier:
						name = e.Value
					case *f90.ArrayRef:
						name = e.Name
					default:
						// Skip unsupported expression types
						continue
					}
					if name != "" {
						upperName := strings.ToUpper(name)
						varNames = append(varNames, upperName)
						vars = append(vars, tg.getOrMakeVar(upperName))
					}
				}

				if len(vars) < 2 {
					continue
				}

				// Choose primary variable: prefer arrays over scalars
				// Use the arrayVars map since ArraySpec isn't set yet during prescan
				primaryIdx := 0
				for i, name := range varNames {
					if arrayVars[name] {
						primaryIdx = i
						break
					}
				}

				primary := vars[primaryIdx]

				// Link all other variables to the primary
				for i, v := range vars {
					if i == primaryIdx {
						continue
					}
					v.EquivalencePrimary = primary
					primary.EquivalenceMembers = append(primary.EquivalenceMembers, v)
				}
			}
		}
	}
}

// applyImplicitTypingToCommonVars applies Fortran IMPLICIT typing rules to COMMON variables
// that don't have explicit type declarations. Updates BOTH field.Type AND v.Type in tg.vars.
func (tg *TranspileToGo) applyImplicitTypingToCommonVars() {
	for _, block := range tg.commonBlocks {
		for _, field := range block.Fields {
			if field.Type == nil && len(field.Names) > 0 {
				varName := field.Names[0].Name
				elemType := getImplicitType(varName)

				// Check if array
				_, isArray := block.ArraySpecs[varName]
				if isArray {
					field.Type = &ast.StarExpr{
						X: &ast.IndexExpr{
							X:     _astTypeArray,
							Index: elemType,
						},
					}
				} else {
					field.Type = elemType
				}

				// CRITICAL: Update tg.vars as well
				if v := tg.getVar(varName); v != nil {
					v.Type = field.Type
					if isArray {
						v.ElementType = elemType
					}
				}
			}
		}
	}
}

// evalNumericIntrinsic evaluates numeric intrinsic functions at compile time for PARAMETER constants
// Returns [value, ok] where ok indicates if evaluation was successful
func (tg *TranspileToGo) evalNumericIntrinsic(intrinsic string, args ...float64) (float64, bool) {
	if len(args) == 0 {
		return 0, false
	}

	switch strings.ToUpper(intrinsic) {
	case "SQRT":
		if len(args) == 1 && args[0] >= 0 {
			return math.Sqrt(args[0]), true
		}
	case "SIN":
		if len(args) == 1 {
			return math.Sin(args[0]), true
		}
	case "COS":
		if len(args) == 1 {
			return math.Cos(args[0]), true
		}
	case "TAN":
		if len(args) == 1 {
			return math.Tan(args[0]), true
		}
	case "ASIN":
		if len(args) == 1 && args[0] >= -1 && args[0] <= 1 {
			return math.Asin(args[0]), true
		}
	case "ACOS":
		if len(args) == 1 && args[0] >= -1 && args[0] <= 1 {
			return math.Acos(args[0]), true
		}
	case "ATAN":
		if len(args) == 1 {
			return math.Atan(args[0]), true
		}
	case "EXP":
		if len(args) == 1 {
			return math.Exp(args[0]), true
		}
	case "LOG":
		if len(args) == 1 && args[0] > 0 {
			return math.Log(args[0]), true
		}
	case "LOG10":
		if len(args) == 1 && args[0] > 0 {
			return math.Log10(args[0]), true
		}
	case "ABS":
		if len(args) == 1 {
			return math.Abs(args[0]), true
		}
	}
	return 0, false
}

// evalBinaryOp evaluates binary operations at compile time if both operands are numeric literals
// Returns [result expr, ok] where ok indicates if evaluation was successful
func (tg *TranspileToGo) evalBinaryOp(left ast.Expr, op token.Token, right ast.Expr) (ast.Expr, bool) {
	// Extract numeric values from both operands
	leftLit, leftOk := left.(*ast.BasicLit)
	rightLit, rightOk := right.(*ast.BasicLit)
	if !leftOk || !rightOk {
		return nil, false
	}

	// Parse numeric values
	var leftVal, rightVal float64
	var err error
	if leftLit.Kind == token.FLOAT {
		leftVal, err = strconv.ParseFloat(leftLit.Value, 64)
	} else if leftLit.Kind == token.INT {
		var intVal int64
		intVal, err = strconv.ParseInt(leftLit.Value, 10, 64)
		leftVal = float64(intVal)
	} else {
		return nil, false
	}
	if err != nil {
		return nil, false
	}

	if rightLit.Kind == token.FLOAT {
		rightVal, err = strconv.ParseFloat(rightLit.Value, 64)
	} else if rightLit.Kind == token.INT {
		var intVal int64
		intVal, err = strconv.ParseInt(rightLit.Value, 10, 64)
		rightVal = float64(intVal)
	} else {
		return nil, false
	}
	if err != nil {
		return nil, false
	}

	// Evaluate the operation
	var result float64
	switch op {
	case token.ADD:
		result = leftVal + rightVal
	case token.SUB:
		result = leftVal - rightVal
	case token.MUL:
		result = leftVal * rightVal
	case token.QUO:
		if rightVal == 0 {
			return nil, false // Division by zero
		}
		result = leftVal / rightVal
	default:
		return nil, false
	}

	// Return as float literal
	return &ast.BasicLit{
		Kind:  token.FLOAT,
		Value: strconv.FormatFloat(result, 'e', -1, 64),
	}, true
}

// Type extraction helpers - flatten deeply nested type assertions

// extractTypeParam extracts T from generic types like Pointer[T], Array[T], or CharArray[T]
// Returns (elemType, true) if successful, (nil, false) otherwise
func (tg *TranspileToGo) extractTypeParam(typ ast.Expr) (ast.Expr, bool) {
	idx, ok := typ.(*ast.IndexExpr)
	if !ok {
		return nil, false
	}
	return idx.Index, true
}

// extractPointerElementType extracts T from intrinsic.Pointer[T]
// Returns (T, true) if typ is Pointer[T], (nil, false) otherwise
func (tg *TranspileToGo) extractPointerElementType(typ ast.Expr) (ast.Expr, bool) {
	idx, ok := typ.(*ast.IndexExpr)
	if !ok {
		return nil, false
	}
	sel, ok := idx.X.(*ast.SelectorExpr)
	if !ok || sel.Sel.Name != "Pointer" {
		return nil, false
	}
	return idx.Index, true
}

// extractArrayElementType extracts T from intrinsic.Array[T] or *intrinsic.Array[T]
// Returns (T, true) if typ is Array[T] or *Array[T], (nil, false) otherwise
func (tg *TranspileToGo) extractArrayElementType(typ ast.Expr) (ast.Expr, bool) {
	// Handle *intrinsic.Array[T]
	if star, ok := typ.(*ast.StarExpr); ok {
		typ = star.X
	}

	idx, ok := typ.(*ast.IndexExpr)
	if !ok {
		return nil, false
	}
	sel, ok := idx.X.(*ast.SelectorExpr)
	if !ok || sel.Sel.Name != "Array" {
		return nil, false
	}
	return idx.Index, true
}

// isPointerType checks if typ is intrinsic.Pointer[T] for any T
func (tg *TranspileToGo) isPointerType(typ ast.Expr) bool {
	_, ok := tg.extractPointerElementType(typ)
	return ok
}

// Type conversion helpers for implicit Fortran type conversions

// extractTypeName gets the base type name from an ast.Expr type representation.
// Handles Ident, IndexExpr (for generics like Array[T]), and StarExpr (for pointers).
func (tg *TranspileToGo) extractTypeName(typ ast.Expr) string {
	switch t := typ.(type) {
	case *ast.Ident:
		return t.Name
	case *ast.IndexExpr:
		// Handle intrinsic.Array[T], intrinsic.Pointer[T]
		if elem, ok := tg.extractTypeParam(typ); ok {
			return tg.extractTypeName(elem)
		}
	case *ast.StarExpr:
		// Handle *intrinsic.Array[T]
		return tg.extractTypeName(t.X)
	}
	return ""
}

// isIntegerType checks if a type name represents an integer type
func (tg *TranspileToGo) isIntegerType(name string) bool {
	return name == "int8" || name == "int16" || name == "int32" || name == "int64"
}

// isNumericType checks if a type name represents a numeric type (integer or float)
func (tg *TranspileToGo) isNumericType(name string) bool {
	return tg.isIntegerType(name) || name == "float32" || name == "float64"
}

// isComplexType checks if a type is a wrapper type (Pointer, Array) that should not
// undergo type conversion. These types have their own element types and should not be
// converted as if they were primitives.
func (tg *TranspileToGo) isComplexType(typ ast.Expr) bool {
	switch t := typ.(type) {
	case *ast.IndexExpr:
		// Check if this is intrinsic.Pointer[T] or intrinsic.Array[T]
		if sel, ok := t.X.(*ast.SelectorExpr); ok {
			typeName := sel.Sel.Name
			return typeName == "Pointer" || typeName == "Array"
		}
	case *ast.StarExpr:
		// Handle *intrinsic.Array[T] or *intrinsic.Pointer[T]
		return tg.isComplexType(t.X)
	}
	return false
}

// typeNeedsConversion checks if assignment from srcType to dstType requires conversion
func (tg *TranspileToGo) typeNeedsConversion(dstType, srcType ast.Expr) bool {
	// Don't convert complex types (Pointer, Array)
	if tg.isComplexType(dstType) || tg.isComplexType(srcType) {
		return false
	}
	dstName := tg.extractTypeName(dstType)
	srcName := tg.extractTypeName(srcType)
	return dstName != "" && srcName != "" && dstName != srcName
}

// wrapWithTypeConversion wraps rhs expression with type conversion if needed.
// Implements Fortran's implicit type conversion rules.
func (tg *TranspileToGo) wrapWithTypeConversion(dstType, srcType ast.Expr, rhs ast.Expr) ast.Expr {
	if !tg.typeNeedsConversion(dstType, srcType) {
		return rhs
	}

	dstName := tg.extractTypeName(dstType)
	srcName := tg.extractTypeName(srcType)

	// INTEGER ↔ LOGICAL conversions use intrinsic functions
	if dstName == "bool" && tg.isIntegerType(srcName) {
		return tg.astIntrinsicGeneric("Int2Bool", ast.NewIdent(srcName), rhs)
	}
	if srcName == "bool" && tg.isIntegerType(dstName) {
		return tg.astIntrinsicGeneric("Bool2Int", ast.NewIdent(dstName), rhs)
	}

	// Numeric conversions use Go type casts
	if tg.isNumericType(dstName) && tg.isNumericType(srcName) {
		return &ast.CallExpr{
			Fun:  ast.NewIdent(dstName),
			Args: []ast.Expr{rhs},
		}
	}

	return rhs
}

// AST Helper Methods - Create common Go AST patterns concisely

// astMethodCall creates: receiver.methodName(args...)
func (tg *TranspileToGo) astMethodCall(receiver, methodName string, args ...ast.Expr) *ast.CallExpr {
	return &ast.CallExpr{
		Fun: &ast.SelectorExpr{
			X:   ast.NewIdent(sanitizeIdent(receiver)),
			Sel: ast.NewIdent(methodName),
		},
		Args: args,
	}
}

// astPkgCall creates: pkgName.fnName(args...)
func (tg *TranspileToGo) astPkgCall(pkgName, fnName string, args ...ast.Expr) *ast.CallExpr {
	return &ast.CallExpr{
		Fun: &ast.SelectorExpr{
			X:   ast.NewIdent(pkgName),
			Sel: ast.NewIdent(fnName),
		},
		Args: args,
	}
}

// astIntrinsicCall creates: intrinsic.fnName(args...) with auto-import
func (tg *TranspileToGo) astIntrinsicCall(fnName string, args ...ast.Expr) *ast.CallExpr {
	// tg.useImport("github.com/soypat/go-fortran/intrinsic") // intrinsic package imported on TranspileToGo initialization.
	return tg.astPkgCall("intrinsic", fnName, args...)
}

// astGenericCall creates: pkgName.fnName[T](args...)
func (tg *TranspileToGo) astGenericCall(pkgName, fnName string, typeParam ast.Expr, args ...ast.Expr) *ast.CallExpr {
	return &ast.CallExpr{
		Fun: &ast.IndexExpr{
			X: &ast.SelectorExpr{
				X:   ast.NewIdent(pkgName),
				Sel: ast.NewIdent(fnName),
			},
			Index: typeParam,
		},
		Args: args,
	}
}

// astGenericCall2 creates: pkgName.fnName[T1, T2](args...)
func (tg *TranspileToGo) astGenericCall2(pkgName, fnName string, t1, t2 ast.Expr, args ...ast.Expr) *ast.CallExpr {
	return &ast.CallExpr{
		Fun: &ast.IndexListExpr{
			X: &ast.SelectorExpr{
				X:   ast.NewIdent(pkgName),
				Sel: ast.NewIdent(fnName),
			},
			Indices: []ast.Expr{t1, t2},
		},
		Args: args,
	}
}

// astIntLit creates: 42
func (tg *TranspileToGo) astIntLit(value int) *ast.BasicLit {
	return &ast.BasicLit{
		Kind:  token.INT,
		Value: strconv.Itoa(value),
	}
}

// astIntrinsicGeneric creates: intrinsic.fnName[T](args...) with auto-import
func (tg *TranspileToGo) astIntrinsicGeneric(fnName string, typeParam ast.Expr, args ...ast.Expr) *ast.CallExpr {
	// tg.useImport("github.com/soypat/go-fortran/intrinsic") // intrinsic package imported on TranspileToGo initialization.
	return tg.astGenericCall("intrinsic", fnName, typeParam, args...)
}

// astIntrinsicGeneric2 creates: intrinsic.fnName[T1, T2](args...) with auto-import
func (tg *TranspileToGo) astIntrinsicGeneric2(fnName string, t1, t2 ast.Expr, args ...ast.Expr) *ast.CallExpr {
	// tg.useImport("github.com/soypat/go-fortran/intrinsic") // intrinsic package imported on TranspileToGo initialization.
	return tg.astGenericCall2("intrinsic", fnName, t1, t2, args...)
}

// Array method helpers

// astArrayAt creates: arrayExpr.At(indices...)
func (tg *TranspileToGo) astArrayAt(arrayExpr ast.Expr, indices ...ast.Expr) *ast.CallExpr {
	return &ast.CallExpr{
		Fun: &ast.SelectorExpr{
			X:   arrayExpr,
			Sel: ast.NewIdent("At"),
		},
		Args: indices,
	}
}

// astArraySet creates: arrayExpr.Set(value, indices...)
func (tg *TranspileToGo) astArraySet(arrayExpr ast.Expr, value ast.Expr, indices ...ast.Expr) *ast.CallExpr {
	return &ast.CallExpr{
		Fun: &ast.SelectorExpr{
			X:   arrayExpr,
			Sel: ast.NewIdent("Set"),
		},
		Args: append([]ast.Expr{value}, indices...),
	}
}

// Type casting helpers

// astTypeCast creates: typeName(expr)
func (tg *TranspileToGo) astTypeCast(typeName string, expr ast.Expr) *ast.CallExpr {
	return &ast.CallExpr{
		Fun:  ast.NewIdent(typeName),
		Args: []ast.Expr{expr},
	}
}

// astIntCast creates: int(expr)
func (tg *TranspileToGo) astIntCast(expr ast.Expr) *ast.CallExpr {
	return tg.astTypeCast("int", expr)
}

// astInt32Cast creates: int32(expr)
func (tg *TranspileToGo) astInt32Cast(expr ast.Expr) *ast.CallExpr {
	return tg.astTypeCast("int32", expr)
}

// astFloat64Cast creates: float64(expr)
func (tg *TranspileToGo) astFloat64Cast(expr ast.Expr) *ast.CallExpr {
	return tg.astTypeCast("float64", expr)
}

// inferExpressionType attempts to determine the Go type of a Fortran expression.
// Returns nil if type cannot be inferred (caller should fall back to no conversion).
func (tg *TranspileToGo) inferExpressionType(expr f90.Expression) ast.Expr {
	switch e := expr.(type) {
	case *f90.Identifier:
		if v := tg.getVar(e.Value); v != nil {
			return v.Type
		}
	case *f90.IntegerLiteral:
		return ast.NewIdent("int32")
	case *f90.RealLiteral:
		// Check for D exponent (DOUBLE PRECISION literal)
		if strings.Contains(e.Raw, "D") || strings.Contains(e.Raw, "d") {
			return ast.NewIdent("float64")
		}
		return ast.NewIdent("float32")
	case *f90.LogicalLiteral:
		return ast.NewIdent("bool")
	case *f90.BinaryExpr:
		// Infer from operands (use LHS type for type promotion)
		return tg.inferExpressionType(e.Left)
	case *f90.FunctionCall:
		// Check if it's a type conversion intrinsic
		upper := strings.ToUpper(e.Name)
		switch upper {
		case "REAL":
			return ast.NewIdent("float32")
		case "DBLE":
			return ast.NewIdent("float64")
		case "INT", "INT32":
			return ast.NewIdent("int32")
		}
		// TODO: Look up function return type in symbol table when available
	}
	return nil
}

// Pointer-to-generic type helpers

// astPointerToGeneric creates: *baseType[typeParam]
func (tg *TranspileToGo) astPointerToGeneric(baseType, typeParam ast.Expr) *ast.StarExpr {
	return &ast.StarExpr{
		X: &ast.IndexExpr{
			X:     baseType,
			Index: typeParam,
		},
	}
}

// astPointerToArray creates: *intrinsic.Array[elemType]
func (tg *TranspileToGo) astPointerToArray(elemType ast.Expr) *ast.StarExpr {
	return tg.astPointerToGeneric(_astTypeArray, elemType)
}

// Binary expression helpers

// astBinaryExpr creates: left op right
func (tg *TranspileToGo) astBinaryExpr(left ast.Expr, op token.Token, right ast.Expr) *ast.BinaryExpr {
	return &ast.BinaryExpr{
		X:  left,
		Op: op,
		Y:  right,
	}
}

// Unary expression helpers

// astUnaryExpr creates: op expr
func (tg *TranspileToGo) astUnaryExpr(op token.Token, expr ast.Expr) *ast.UnaryExpr {
	return &ast.UnaryExpr{
		Op: op,
		X:  expr,
	}
}

// astAddressOf creates: &expr
func (tg *TranspileToGo) astAddressOf(expr ast.Expr) *ast.UnaryExpr {
	return tg.astUnaryExpr(token.AND, expr)
}

// Expression statement helper

// astExprStmt creates: &ast.ExprStmt{X: expr}
func (tg *TranspileToGo) astExprStmt(expr ast.Expr) *ast.ExprStmt {
	return &ast.ExprStmt{X: expr}
}

// astCommonFieldRef creates a COMMON block field reference: blockVarName.FIELDNAME
// Used for accessing variables stored in COMMON blocks as struct fields.
func (tg *TranspileToGo) astCommonFieldRef(blockVarName, fieldName string) *ast.SelectorExpr {
	return &ast.SelectorExpr{
		X:   ast.NewIdent(blockVarName),
		Sel: ast.NewIdent(strings.ToUpper(fieldName)),
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
