// Package symbol provides symbol table and type resolution infrastructure
// for semantic analysis of Fortran programs.
package symbol

import (
	"fmt"
	"strings"

	"github.com/soypat/go-fortran/ast"
	"github.com/soypat/go-fortran/token"
)

// Flags
type Flags uint64

const (
	FlagImplicit Flags = 1 << iota
	FlagUsed
	FlagPointer
	FlagTarget
)

func (f Flags) HasAny(hasBits Flags) bool { return f&hasBits != 0 }
func (f Flags) HasAll(hasBits Flags) bool { return f&hasBits == hasBits }
func (f Flags) With(mask Flags, setBits bool) Flags {
	if setBits {
		return f | mask
	} else {
		return f &^ mask
	}
}

// Symbol represents a declared entity (variable, function, type, etc.)
type Symbol struct {
	name       string         // Symbol name (case-insensitive in Fortran)
	typ        *ResolvedType  // Fully resolved type information
	kind       SymbolKind     // What kind of symbol this is
	attributes []token.Token  // SAVE, POINTER, TARGET, etc.
	arraySpec  *ast.ArraySpec // Array dimensions (nil if not an array)
	declNode   ast.Node       // Reference to declaration AST node
	scope      *Scope         // Scope where this symbol is defined
	flags      Flags
}

// NewSymbol creates a new symbol with the given name and kind
func NewSymbol(name string, kind SymbolKind) *Symbol {
	return &Symbol{
		name: name,
		kind: kind,
	}
}

// Name returns the symbol name
func (s *Symbol) Name() string {
	return s.name
}

// Type returns the resolved type (may be nil if not yet resolved)
func (s *Symbol) Type() *ResolvedType {
	return s.typ
}

// Kind returns the symbol kind
func (s *Symbol) Kind() SymbolKind {
	return s.kind
}

// Attributes returns the symbol attributes
func (s *Symbol) Attributes() []token.Token {
	return s.attributes
}

// ArraySpec returns the array specification (nil if not an array)
func (s *Symbol) ArraySpec() *ast.ArraySpec {
	return s.arraySpec
}

// DeclNode returns the AST node where this symbol was declared
func (s *Symbol) DeclNode() ast.Node {
	return s.declNode
}

// Scope returns the scope where this symbol is defined
func (s *Symbol) Scope() *Scope {
	return s.scope
}

// Flags returns the symbol [Flags].
func (s *Symbol) Flags() Flags {
	return s.flags
}

// SetType sets the resolved type (used during type resolution)
func (s *Symbol) SetType(typ *ResolvedType) {
	s.typ = typ
}

// SetArraySpec sets the array specification
func (s *Symbol) SetArraySpec(spec *ast.ArraySpec) {
	s.arraySpec = spec
}

// SetAttributes sets the symbol attributes
func (s *Symbol) SetAttributes(attrs []token.Token) {
	s.attributes = attrs
}

// AddAttribute adds an attribute to the symbol
func (s *Symbol) AddAttribute(attr token.Token) {
	s.attributes = append(s.attributes, attr)
}

// SetDeclNode sets the declaration node
func (s *Symbol) SetDeclNode(node ast.Node) {
	s.declNode = node
}

// SetScope sets the scope (used during symbol table building)
func (s *Symbol) SetScope(scope *Scope) {
	s.scope = scope
}

// setImplicit marks whether the type is from implicit rules
func (s *Symbol) setImplicit(implicit bool) {
	s.flags = s.flags.With(FlagImplicit, implicit)
}

// markUsed marks the symbol as used/referenced
func (s *Symbol) markUsed() {
	s.flags = s.flags.With(FlagUsed, true)
}

// ResolvedType represents a fully resolved Fortran type
type ResolvedType struct {
	BaseType string // "INTEGER", "REAL", "LOGICAL", "CHARACTER", "TYPE", "DOUBLE PRECISION"
	Kind     int    // Kind parameter value (0 = default kind)
	CharLen  int    // CHARACTER length (-1 = assumed, -2 = deferred, 0 = default)
	TypeName string // For derived types: TYPE(typename)
	Flags    Flags
}

// SymbolKind classifies what kind of entity a symbol represents
type SymbolKind int

const (
	SymUnknown     SymbolKind = iota
	SymVariable               // Regular variable
	SymParameter              // Compile-time constant (PARAMETER attribute)
	SymFunction               // Function (returns value)
	SymSubroutine             // Subroutine (no return value)
	SymModule                 // Module
	SymProgram                // Main program
	SymCommonBlock            // COMMON block
	SymDerivedType            // User-defined TYPE
	SymIntrinsic              // Intrinsic function
	SymExternal               // External procedure
)

// String returns the string representation of SymbolKind
func (sk SymbolKind) String() string {
	switch sk {
	case SymUnknown:
		return "Unknown"
	case SymVariable:
		return "Variable"
	case SymParameter:
		return "Parameter"
	case SymFunction:
		return "Function"
	case SymSubroutine:
		return "Subroutine"
	case SymModule:
		return "Module"
	case SymProgram:
		return "Program"
	case SymCommonBlock:
		return "CommonBlock"
	case SymDerivedType:
		return "DerivedType"
	case SymIntrinsic:
		return "Intrinsic"
	case SymExternal:
		return "External"
	default:
		return "Unknown"
	}
}

// Scope represents a lexical scope with symbol table
type Scope struct {
	parent      *Scope             // Parent scope (nil for global)
	children    []*Scope           // Nested scopes
	symbols     map[string]*Symbol // Symbol table (case-insensitive keys)
	implicit    *ImplicitRules     // Implicit typing rules for this scope
	programUnit ast.ProgramUnit    // PROGRAM/SUBROUTINE/FUNCTION/MODULE
	scopeType   ScopeType          // Global, Program, Procedure, Block
}

// Parent returns the parent scope (nil for global scope)
func (s *Scope) Parent() *Scope {
	return s.parent
}

// Children returns child scopes
func (s *Scope) Children() []*Scope {
	return s.children
}

// Implicit returns the implicit typing rules for this scope
func (s *Scope) Implicit() *ImplicitRules {
	return s.implicit
}

// ProgramUnit returns the associated program unit
func (s *Scope) ProgramUnit() ast.ProgramUnit {
	return s.programUnit
}

// ScopeType returns the type of this scope
func (s *Scope) Type() ScopeType {
	return s.scopeType
}

// Lookup searches for a symbol in this scope and parent scopes
func (s *Scope) Lookup(name string) *Symbol {
	name = normalizeCase(name) // Fortran is case-insensitive

	// Search current scope
	if sym, ok := s.symbols[name]; ok {
		return sym
	}

	// Search parent scopes
	scope := s.parent
	for scope != nil {
		if sym, ok := scope.symbols[name]; ok {
			return sym
		}
		scope = scope.parent
	}

	return nil
}

// LookupLocal searches for a symbol only in this scope (not parent scopes)
func (s *Scope) LookupLocal(name string) *Symbol {
	name = normalizeCase(name)
	return s.symbols[name]
}

// Symbols returns a copy of the symbol map for iteration (for testing/debugging)
func (s *Scope) Symbols() map[string]*Symbol {
	// Return the actual map - caller should not modify it
	return s.symbols
}

// Define adds a symbol to this scope
func (s *Scope) Define(sym *Symbol) error {
	name := normalizeCase(sym.Name())

	// Check if already defined in current scope
	if _, ok := s.symbols[name]; ok {
		return fmt.Errorf("symbol %s already defined in scope", sym.Name())
	}

	sym.SetScope(s)
	s.symbols[name] = sym
	return nil
}

// ScopeType identifies the type of scope
type ScopeType int

const (
	ScopeGlobal    ScopeType = iota // Global scope (entire file)
	ScopeProgram                    // PROGRAM unit
	ScopeProcedure                  // SUBROUTINE or FUNCTION
	ScopeModule                     // MODULE
	ScopeBlock                      // Block (IF, DO, etc.) - F90+
)

// String returns the string representation of ScopeType
func (st ScopeType) String() string {
	switch st {
	case ScopeGlobal:
		return "Global"
	case ScopeProgram:
		return "Program"
	case ScopeProcedure:
		return "Procedure"
	case ScopeModule:
		return "Module"
	case ScopeBlock:
		return "Block"
	default:
		return "Unknown"
	}
}

// ImplicitRules stores implicit typing rules for a scope
type ImplicitRules struct {
	IsNone      bool       // IMPLICIT NONE specified?
	LetterTypes [26]string // Type for each letter A-Z (empty = no rule)
	LetterKinds [26]int    // Kind for each letter (0 = default)
}

// Copy creates a deep copy of ImplicitRules
func (ir *ImplicitRules) Copy() *ImplicitRules {
	if ir == nil {
		return nil
	}
	newRules := &ImplicitRules{
		IsNone: ir.IsNone,
	}
	copy(newRules.LetterTypes[:], ir.LetterTypes[:])
	copy(newRules.LetterKinds[:], ir.LetterKinds[:])
	return newRules
}

// Table is the root of the symbol table hierarchy
type Table struct {
	globalScope  *Scope                  // Global scope
	currentScope *Scope                  // Current scope during analysis
	commonBlocks map[string]*CommonBlock // COMMON block registry
	modules      map[string]*ModuleInfo  // Module registry
	intrinsics   map[string]*Intrinsic   // Intrinsic function database
}

// GlobalScope returns the global scope
func (st *Table) GlobalScope() *Scope {
	return st.globalScope
}

// CurrentScope returns the current scope during analysis
func (st *Table) CurrentScope() *Scope {
	return st.currentScope
}

// CommonBlock returns a COMMON block by name (nil if not found)
func (st *Table) CommonBlock(name string) *CommonBlock {
	return st.commonBlocks[normalizeCase(name)]
}

// Module returns a module by name (nil if not found)
func (st *Table) Module(name string) *ModuleInfo {
	return st.modules[normalizeCase(name)]
}

// Intrinsic returns an intrinsic function by name (nil if not found)
func (st *Table) Intrinsic(name string) *Intrinsic {
	return st.intrinsics[normalizeCase(name)]
}

// CommonBlock represents a COMMON block shared between program units
type CommonBlock struct {
	name      string   // Empty for blank COMMON
	variables []string // Variable names in order
	sizes     []int    // Byte sizes (for storage sequence)
	totalSize int      // Total bytes
}

// Name returns the common block name
func (cb *CommonBlock) Name() string {
	return cb.name
}

// Variables returns the variable names in order
func (cb *CommonBlock) Variables() []string {
	return cb.variables
}

// Sizes returns the byte sizes for storage sequence
func (cb *CommonBlock) Sizes() []int {
	return cb.sizes
}

// TotalSize returns the total bytes
func (cb *CommonBlock) TotalSize() int {
	return cb.totalSize
}

// AddVariable adds a variable to the common block
func (cb *CommonBlock) AddVariable(name string) {
	cb.variables = append(cb.variables, name)
}

// NewCommonBlock creates a new common block with the given name
func NewCommonBlock(name string) *CommonBlock {
	return &CommonBlock{
		name:      name,
		variables: make([]string, 0),
		sizes:     make([]int, 0),
	}
}

// ModuleInfo represents a MODULE with its exported symbols
type ModuleInfo struct {
	name           string
	publicSymbols  map[string]*Symbol // PUBLIC symbols
	privateSymbols map[string]*Symbol // PRIVATE symbols
	scope          *Scope
}

// Name returns the module name
func (m *ModuleInfo) Name() string {
	return m.name
}

// PublicSymbols returns the map of public symbols
func (m *ModuleInfo) PublicSymbols() map[string]*Symbol {
	return m.publicSymbols
}

// PrivateSymbols returns the map of private symbols
func (m *ModuleInfo) PrivateSymbols() map[string]*Symbol {
	return m.privateSymbols
}

// Scope returns the module's scope
func (m *ModuleInfo) Scope() *Scope {
	return m.scope
}

// NewModuleInfo creates a new module info with the given name and scope
func NewModuleInfo(name string, scope *Scope) *ModuleInfo {
	return &ModuleInfo{
		name:           name,
		publicSymbols:  make(map[string]*Symbol),
		privateSymbols: make(map[string]*Symbol),
		scope:          scope,
	}
}

// Intrinsic represents an intrinsic function or subroutine
type Intrinsic struct {
	name       string
	kind       IntrinsicKind
	returnType string // For functions
}

// Name returns the intrinsic name
func (i *Intrinsic) Name() string {
	return i.name
}

// Kind returns the intrinsic kind
func (i *Intrinsic) Kind() IntrinsicKind {
	return i.kind
}

// ReturnType returns the return type for functions
func (i *Intrinsic) ReturnType() string {
	return i.returnType
}

// NewIntrinsic creates a new intrinsic
func NewIntrinsic(name, returnType string, kind IntrinsicKind) *Intrinsic {
	return &Intrinsic{
		name:       name,
		kind:       kind,
		returnType: returnType,
	}
}

// IntrinsicKind identifies whether an intrinsic is a function or subroutine
type IntrinsicKind int

const (
	IntrinsicFunction IntrinsicKind = iota
	IntrinsicSubroutine
)

// String returns the string representation of IntrinsicKind
func (ik IntrinsicKind) String() string {
	switch ik {
	case IntrinsicFunction:
		return "Function"
	case IntrinsicSubroutine:
		return "Subroutine"
	default:
		return "Unknown"
	}
}

// NewSymbolTable creates a new symbol table with global scope
func NewSymbolTable() *Table {
	st := &Table{
		commonBlocks: make(map[string]*CommonBlock),
		modules:      make(map[string]*ModuleInfo),
		intrinsics:   loadIntrinsics(),
	}
	st.globalScope = &Scope{
		symbols:   make(map[string]*Symbol),
		implicit:  defaultImplicitRules(),
		scopeType: ScopeGlobal,
	}
	st.currentScope = st.globalScope
	return st
}

// EnterScope creates a new scope as child of current scope
func (st *Table) EnterScope(unit ast.ProgramUnit, scopeType ScopeType) *Scope {
	newScope := &Scope{
		parent:      st.currentScope,
		symbols:     make(map[string]*Symbol),
		implicit:    st.currentScope.implicit.Copy(), // Inherit implicit rules
		programUnit: unit,
		scopeType:   scopeType,
	}
	st.currentScope.children = append(st.currentScope.children, newScope)
	st.currentScope = newScope
	return newScope
}

// ExitScope returns to parent scope
func (st *Table) ExitScope() {
	if st.currentScope.parent != nil {
		st.currentScope = st.currentScope.parent
	}
}

// normalizeCase converts a Fortran identifier to normalized form (uppercase)
// Fortran is case-insensitive, so we normalize to uppercase for consistent lookup
func normalizeCase(name string) string {
	return strings.ToUpper(name)
}

// defaultImplicitRules returns the default Fortran 77/90 implicit typing rules:
// - Variables starting with I-N are INTEGER
// - Variables starting with A-H, O-Z are REAL
func defaultImplicitRules() *ImplicitRules {
	rules := &ImplicitRules{}

	// I-N = INTEGER (letters 8-13, 0-indexed)
	for i := 'I'; i <= 'N'; i++ {
		rules.LetterTypes[i-'A'] = "INTEGER"
	}

	// A-H = REAL (letters 0-7)
	for i := 'A'; i <= 'H'; i++ {
		rules.LetterTypes[i-'A'] = "REAL"
	}

	// O-Z = REAL (letters 14-25)
	for i := 'O'; i <= 'Z'; i++ {
		rules.LetterTypes[i-'A'] = "REAL"
	}

	return rules
}

// loadIntrinsics creates the database of Fortran intrinsic functions
func loadIntrinsics() map[string]*Intrinsic {
	intrinsics := make(map[string]*Intrinsic)

	// Math functions
	add := func(name, returnType string) {
		intrinsics[name] = NewIntrinsic(name, returnType, IntrinsicFunction)
	}

	// Trigonometric
	add("SIN", "REAL")
	add("COS", "REAL")
	add("TAN", "REAL")
	add("ASIN", "REAL")
	add("ACOS", "REAL")
	add("ATAN", "REAL")
	add("ATAN2", "REAL")

	// Exponential and logarithmic
	add("EXP", "REAL")
	add("LOG", "REAL")
	add("LOG10", "REAL")
	add("SQRT", "REAL")

	// Type conversion
	add("INT", "INTEGER")
	add("REAL", "REAL")
	add("DBLE", "REAL")
	add("CMPLX", "COMPLEX")

	// Numeric inquiry and manipulation
	add("ABS", "REAL")
	add("MOD", "INTEGER")
	add("SIGN", "REAL")
	add("DIM", "REAL")
	add("MAX", "REAL")
	add("MIN", "REAL")

	// String functions
	add("LEN", "INTEGER")
	add("INDEX", "INTEGER")
	add("TRIM", "CHARACTER")

	// Array functions
	add("SIZE", "INTEGER")
	add("SHAPE", "INTEGER")
	add("LBOUND", "INTEGER")
	add("UBOUND", "INTEGER")

	// Logical array reduction functions
	add("ALL", "LOGICAL")    // .TRUE. if all values are true
	add("ANY", "LOGICAL")    // .TRUE. if any value is true
	add("COUNT", "INTEGER")  // Count of .TRUE. elements

	// Array reduction functions
	add("SUM", "REAL")       // Sum of array elements
	add("PRODUCT", "REAL")   // Product of array elements
	add("MAXVAL", "REAL")    // Maximum value in array
	add("MINVAL", "REAL")    // Minimum value in array
	add("MAXLOC", "INTEGER") // Location of maximum value
	add("MINLOC", "INTEGER") // Location of minimum value

	// Array construction and manipulation
	add("MERGE", "REAL")     // Merge arrays under mask
	add("PACK", "REAL")      // Pack array into vector under mask
	add("UNPACK", "REAL")    // Unpack vector into array under mask
	add("SPREAD", "REAL")    // Replicate array by adding dimension
	add("RESHAPE", "REAL")   // Reshape array to different dimensions
	add("TRANSPOSE", "REAL") // Matrix transpose
	add("EOSHIFT", "REAL")   // End-off shift
	add("CSHIFT", "REAL")    // Circular shift

	// Matrix operations
	add("MATMUL", "REAL")       // Matrix multiplication
	add("DOT_PRODUCT", "REAL")  // Dot product of vectors

	// Character functions
	add("ADJUSTL", "CHARACTER")  // Adjust left (remove leading spaces)
	add("ADJUSTR", "CHARACTER")  // Adjust right (remove trailing spaces)
	add("LEN_TRIM", "INTEGER")   // Length without trailing spaces
	add("SCAN", "INTEGER")       // Scan for character set
	add("VERIFY", "INTEGER")     // Verify character set
	add("REPEAT", "CHARACTER")   // Repeat string
	add("CHAR", "CHARACTER")     // Convert integer to character
	add("ICHAR", "INTEGER")      // Convert character to integer
	add("ACHAR", "CHARACTER")    // ASCII character (F90)
	add("IACHAR", "INTEGER")     // ASCII integer code (F90)

	// Bit manipulation (F90)
	add("IAND", "INTEGER")   // Bitwise AND
	add("IOR", "INTEGER")    // Bitwise inclusive OR
	add("IEOR", "INTEGER")   // Bitwise exclusive OR
	add("NOT", "INTEGER")    // Bitwise complement
	add("BTEST", "LOGICAL")  // Test bit
	add("IBSET", "INTEGER")  // Set bit
	add("IBCLR", "INTEGER")  // Clear bit
	add("IBITS", "INTEGER")  // Extract bit sequence
	add("ISHFT", "INTEGER")  // Logical shift
	add("ISHFTC", "INTEGER") // Circular shift

	// Numeric functions
	add("FLOOR", "INTEGER")     // Greatest integer <= x
	add("CEILING", "INTEGER")   // Least integer >= x
	add("MODULO", "REAL")       // Modulo function
	add("NINT", "INTEGER")      // Nearest integer
	add("ANINT", "REAL")        // Nearest whole number
	add("AINT", "REAL")         // Truncate to whole number
	add("FRACTION", "REAL")     // Fractional part of model representation
	add("EXPONENT", "INTEGER")  // Exponent part of model representation
	add("SCALE", "REAL")        // Scale by power of radix
	add("NEAREST", "REAL")      // Nearest different machine number
	add("SPACING", "REAL")      // Absolute spacing of model numbers
	add("RRSPACING", "REAL")    // Reciprocal of relative spacing
	add("SET_EXPONENT", "REAL") // Set exponent part

	// Hyperbolic functions (F2008, but commonly supported)
	add("SINH", "REAL")  // Hyperbolic sine
	add("COSH", "REAL")  // Hyperbolic cosine
	add("TANH", "REAL")  // Hyperbolic tangent
	add("ASINH", "REAL") // Inverse hyperbolic sine
	add("ACOSH", "REAL") // Inverse hyperbolic cosine
	add("ATANH", "REAL") // Inverse hyperbolic tangent

	// Kind and model inquiry functions
	add("KIND", "INTEGER")              // Kind type parameter value
	add("SELECTED_INT_KIND", "INTEGER") // Integer kind meeting range requirement
	add("SELECTED_REAL_KIND", "INTEGER") // Real kind meeting precision requirement
	add("PRECISION", "INTEGER")         // Decimal precision
	add("RANGE", "INTEGER")             // Decimal exponent range
	add("RADIX", "INTEGER")             // Base of the model
	add("DIGITS", "INTEGER")            // Number of significant digits
	add("MINEXPONENT", "INTEGER")       // Minimum exponent
	add("MAXEXPONENT", "INTEGER")       // Maximum exponent
	add("EPSILON", "REAL")              // Machine epsilon
	add("TINY", "REAL")                 // Smallest positive number
	add("HUGE", "REAL")                 // Largest number

	// Transfer and conversion
	add("TRANSFER", "REAL") // Treat bit pattern as different type

	// Pointer and allocation inquiry
	add("ALLOCATED", "LOGICAL")  // Test allocation status
	add("ASSOCIATED", "LOGICAL") // Test pointer association status
	add("PRESENT", "LOGICAL")    // Test presence of optional argument

	// Miscellaneous
	add("NULL", "POINTER")   // Null pointer
	add("DATE_AND_TIME", "") // Get date and time (subroutine)
	add("SYSTEM_CLOCK", "")  // Get system clock (subroutine)
	add("CPU_TIME", "")      // Get CPU time (subroutine)

	return intrinsics
}
