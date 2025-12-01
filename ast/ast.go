package ast

import (
	"github.com/soypat/go-fortran/token"
)

type Node interface {
	AppendTokenLiteral(dst []byte) []byte
	AppendString(dst []byte) []byte
	// SourcePos returns the position of first character belonging to the node as start
	// and the position of the first character immediately after the node as end in the file.
	SourcePos() Position
}

type Expression interface {
	Node
	expressionNode()
}

type Statement interface {
	Node
	statementNode()
	GetLabel() string
}

// ProgramUnit represents a top-level construct (PROGRAM, SUBROUTINE, FUNCTION, MODULE)
type ProgramUnit interface {
	Statement
	programUnitNode()
}

func Pos(start, end int) Position {
	if end < start {
		panic("end < start")
	}
	return Position{start: start, end: end}
}

type Position struct {
	start int
	end   int
}

func (p Position) Start() int { return p.start }
func (p Position) End() int   { return p.end }

func (sp Position) SourcePos() Position {
	return sp
}

type TypeSpec struct {
	Token token.Token // Intrinsic used to specify type.
	Name  string      // Is non-empty for TYPE specified.
	// Optional kind parameter. Valid for token.INTEGER, token.REAL, token.COMPLEX, token.LOGICAL
	// For CHARACTER type is required and is the LEN attribute.
	KindOrLen  Expression
	Attributes []TypeAttribute
}

func (ts *TypeSpec) Intent() IntentType {
	for i := range ts.Attributes {
		attr := &ts.Attributes[i]
		if tok, ok := attr.Expr.(*TokenExpr); attr.Token == token.INTENT && ok {
			switch tok.Token {
			case token.IN:
				return IntentIn
			case token.OUT:
				return IntentOut
			case token.INOUT:
				return IntentInOut
			default:
				return intentDefault
			}
		}
	}
	return intentDefault
}

func (ts TypeSpec) AppendString(dst []byte) []byte {
	dst = appendTypenameOrTok(dst, ts.Name, ts.Token)
	if len(ts.Attributes) > 0 {
		dst = append(dst, ", "...)
		for i := range ts.Attributes {
			if i > 0 {
				dst = append(dst, ", "...)
			}
			dst = ts.Attributes[i].AppendString(dst)
		}
	}
	return dst
}

func appendTypenameOrTok(dst []byte, typename string, tok token.Token) []byte {
	if typename != "" {
		dst = append(dst, typename...)
	} else {
		if tok != token.TYPE {
			dst = append(dst, "<unexpected token type>"...)
		}
		dst = append(dst, tok.String()...)
	}
	return dst
}

// Program represents the root node of a Fortran source file, containing one or
// more program units. A file may contain multiple independent programs, modules,
// functions, or subroutines.
//
// Example:
//
//	<program-unit>...
//
//	PROGRAM main
//	  ...
//	END PROGRAM main
//
//	MODULE utilities
//	  ...
//	END MODULE utilities
type Program struct {
	Units []ProgramUnit
	Label string
}

func (p *Program) GetLabel() string { return p.Label }

func (p *Program) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "PROGRAM"...)
}

func (p *Program) AppendString(dst []byte) []byte {
	for i, unit := range p.Units {
		if i > 0 {
			dst = append(dst, '\n')
		}
		dst = unit.AppendString(dst)
	}
	return dst
}

func (p *Program) SourcePos() Position {
	if len(p.Units) <= 0 {
		return Position{}
	}
	p0 := p.Units[0].SourcePos()
	pend := p.Units[len(p.Units)-1].SourcePos()
	return Position{start: p0.start, end: pend.end}
}

// ProgramBlock represents the main executable program unit that serves as the
// entry point for program execution. A Fortran program may contain at most one
// PROGRAM block, though it may be omitted for simple programs.
//
// Example:
//
//	PROGRAM <name>
//	  <specification-statements>
//	  <executable-statements>
//	END PROGRAM [<name>]
//
//	PROGRAM hello
//	  PRINT *, 'Hello, World!'
//	END PROGRAM hello
type ProgramBlock struct {
	Name     string
	Body     []Statement   // Specification and executable statements
	Contains []ProgramUnit // Internal procedures (CONTAINS section)
	Label    string
	Position
}

var _ ProgramUnit = (*ProgramBlock)(nil) // compile time check of interface implementation.

func (pb *ProgramBlock) GetLabel() string { return pb.Label }

func (pb *ProgramBlock) statementNode()   {}
func (pb *ProgramBlock) programUnitNode() {}
func (pb *ProgramBlock) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "PROGRAM"...)
}
func (pb *ProgramBlock) AppendString(dst []byte) []byte {
	dst = append(dst, "PROGRAM "...)
	dst = append(dst, pb.Name...)
	return dst
}

// Subroutine represents a callable procedure that performs operations but does
// not return a value. Subroutines are invoked using [CallStmt] and can modify
// arguments, perform I/O, or change program state.
//
// Example:
//
//	SUBROUTINE <name>([<parameter-list>])
//	  <specification-statements>
//	  <executable-statements>
//	END SUBROUTINE [<name>]
//
//	SUBROUTINE swap(a, b)
//	  REAL, INTENT(INOUT) :: a, b
//	  REAL :: temp
//	  temp = a
//	  a = b
//	  b = temp
//	END SUBROUTINE swap
type Subroutine struct {
	Name       string
	Parameters []Parameter   // Function/subroutine parameters with type information
	Attributes []token.Token // RECURSIVE, PURE, etc.
	Body       []Statement   // Specification and executable statements
	Label      string
	Position
}

var _ ProgramUnit = (*Subroutine)(nil) // compile time check of interface implementation.

func (s *Subroutine) GetLabel() string { return s.Label }

func (s *Subroutine) statementNode()   {}
func (s *Subroutine) programUnitNode() {}
func (s *Subroutine) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "SUBROUTINE"...)
}
func (s *Subroutine) AppendString(dst []byte) []byte {
	dst = append(dst, "SUBROUTINE "...)
	dst = append(dst, s.Name...)
	dst = append(dst, '(')
	for i, p := range s.Parameters {
		if i > 0 {
			dst = append(dst, ", "...)
		}
		dst = append(dst, p.Name...)
	}
	dst = append(dst, ')')
	return dst
}

// Function represents a callable procedure that returns a value. Functions can
// be used in expressions and must assign a value to the function name or result
// variable before returning.
//
// Example:
//
//	[<type>] FUNCTION <name>([<parameter-list>]) [RESULT(<var>)]
//	  <specification-statements>
//	  <executable-statements>
//	END FUNCTION [<name>]
//
//	REAL FUNCTION average(arr, n)
//	  REAL :: arr(n)
//	  INTEGER :: n
//	  average = SUM(arr) / n
//	END FUNCTION average
type Function struct {
	Name           string
	Type           TypeSpec      // Result type with optional KIND/LEN
	Parameters     []Parameter   // Function parameters with type information
	ResultVariable string        // For RESULT(var) clause
	Attributes     []token.Token // RECURSIVE, PURE, ELEMENTAL
	Body           []Statement   // Specification and executable statements
	Label          string
	Position
}

var _ ProgramUnit = (*Function)(nil) // compile time check of interface implementation.

func (f *Function) GetLabel() string { return f.Label }

func (f *Function) statementNode()   {}
func (f *Function) programUnitNode() {}
func (f *Function) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "FUNCTION"...)
}
func (f *Function) AppendString(dst []byte) []byte {
	if f.Type.Token != 0 {
		dst = f.Type.AppendString(dst)
	}
	dst = append(dst, "FUNCTION "...)
	dst = append(dst, f.Name...)
	dst = append(dst, '(')
	for i, p := range f.Parameters {
		if i > 0 {
			dst = append(dst, ", "...)
		}
		dst = append(dst, p.Name...)
	}
	dst = append(dst, ')')
	if f.ResultVariable != "" {
		dst = append(dst, " RESULT("...)
		dst = append(dst, f.ResultVariable...)
		dst = append(dst, ')')
	}
	return dst
}

// Module represents a namespace for data, type definitions, and procedures that
// can be shared across program units via [UseStatement]. Modules support
// encapsulation and information hiding through [PublicStmt] and [PrivateStmt].
//
// Example:
//
//	MODULE <name>
//	  <specification-statements>
//	  [CONTAINS
//	    <module-procedures>]
//	END MODULE [<name>]
//
//	MODULE constants
//	  IMPLICIT NONE
//	  REAL, PARAMETER :: PI = 3.14159
//	END MODULE constants
type Module struct {
	Name     string
	Body     []Statement   // Module-level declarations
	Contains []ProgramUnit // Procedures in CONTAINS section
	Label    string
	Position
}

var _ ProgramUnit = (*Module)(nil) // compile time check of interface implementation.

func (m *Module) GetLabel() string { return m.Label }

func (m *Module) statementNode()   {}
func (m *Module) programUnitNode() {}
func (m *Module) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "MODULE"...)
}
func (m *Module) AppendString(dst []byte) []byte {
	dst = append(dst, "MODULE "...)
	dst = append(dst, m.Name...)
	if len(m.Contains) > 0 {
		dst = append(dst, " CONTAINS "...)
	}
	return dst
}

// BlockData represents a named or unnamed BLOCK DATA program unit used in
// Fortran 77 to initialize variables in COMMON blocks. This feature is largely
// obsolete in modern Fortran, replaced by module initialization.
//
// Example:
//
//	BLOCK DATA [<name>]
//	  <specification-statements>
//	END BLOCK DATA [<name>]
//
//	BLOCK DATA init_common
//	  COMMON /shared/ x, y
//	  DATA x, y /1.0, 2.0/
//	END BLOCK DATA init_common
type BlockData struct {
	Name  string
	Body  []Statement
	Label string
	Position
}

var _ ProgramUnit = (*BlockData)(nil) // compile time check of interface implementation.

func (bd *BlockData) GetLabel() string { return bd.Label }

func (bd *BlockData) statementNode()   {}
func (bd *BlockData) programUnitNode() {}
func (bd *BlockData) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "BLOCKDATA"...)
}
func (bd *BlockData) AppendString(dst []byte) []byte {
	dst = append(dst, "BLOCK DATA"...)
	if bd.Name != "" {
		dst = append(dst, ' ')
		dst = append(dst, bd.Name...)
	}
	return dst
}

// TokenTuple represents a stored token for deferred parsing
type TokenTuple struct {
	Tok   token.Token
	Start int
	Lit   []byte
}

// Specification Part Statements (Phase 2)

// LetterRange represents a range of letters (A-Z) for implicit typing rules.
// Used in IMPLICIT statements to specify which variable names get a certain type.
//
// Example: In "IMPLICIT REAL (A-H)", the range is A to H
type LetterRange struct {
	Start byte // 'A' to 'Z'
	End   byte // 'A' to 'Z', Start <= End
}

// ImplicitRule specifies a type for a set of letter ranges.
// Used in IMPLICIT statements to define custom typing rules.
//
// Example: "IMPLICIT REAL(KIND=8) (A-H, O-Z)" creates a rule with
// Type="REAL", Kind=8, and two letter ranges
type ImplicitRule struct {
	Type         TypeSpec      // Type with optional KIND/LEN in TypeSpec.KindOrLen
	LetterRanges []LetterRange // Letter ranges this rule applies to
}

// ImplicitStatement controls implicit typing rules for variables. IMPLICIT NONE
// disables implicit typing, requiring all variables to be explicitly declared.
// Without IMPLICIT NONE, Fortran uses default typing rules (I-N for INTEGER,
// A-H and O-Z for REAL).
//
// Example:
//
//	IMPLICIT NONE
//	IMPLICIT REAL (A-H, O-Z)
//	IMPLICIT INTEGER (I-N)
//	IMPLICIT REAL(KIND=8) (A-C, X-Z)
type ImplicitStatement struct {
	IsNone bool           // true for IMPLICIT NONE
	Rules  []ImplicitRule // Custom type rules (empty if IsNone)
	Label  string
	Position
}

var _ Statement = (*ImplicitStatement)(nil) // compile time check of interface implementation.

func (is *ImplicitStatement) GetLabel() string { return is.Label }

func (is *ImplicitStatement) statementNode() {}
func (is *ImplicitStatement) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "IMPLICIT"...)
}
func (is *ImplicitStatement) AppendString(dst []byte) []byte {
	if is.IsNone {
		return append(dst, "IMPLICIT NONE"...)
	}
	dst = append(dst, "IMPLICIT"...)
	for i, rule := range is.Rules {
		if i > 0 {
			dst = append(dst, ", "...)
		} else {
			dst = append(dst, ' ')
		}
		dst = rule.Type.AppendString(dst)
		if rule.Type.KindOrLen != nil {
			dst = append(dst, '(')
			dst = rule.Type.KindOrLen.AppendString(dst)
			dst = append(dst, ')')
		}
		dst = append(dst, " ("...)
		for j, lr := range rule.LetterRanges {
			if j > 0 {
				dst = append(dst, ", "...)
			}
			dst = append(dst, lr.Start)
			if lr.Start != lr.End {
				dst = append(dst, '-')
				dst = append(dst, lr.End)
			}
		}
		dst = append(dst, ')')
	}
	return dst
}

// UseStatement imports entities from a [Module] into the current scope, making
// module procedures, variables, and types accessible. The ONLY clause restricts
// which entities are imported.
//
// Example:
//
//	USE <module-name> [, ONLY: <entity-list>]
//	USE constants
//	USE utilities, ONLY: sort, search
//	USE iso_fortran_env, ONLY: REAL64
type UseStatement struct {
	ModuleName string
	Only       []string // Empty if not using ONLY clause
	Label      string
	Position
}

var _ Statement = (*UseStatement)(nil) // compile time check of interface implementation.

func (us *UseStatement) GetLabel() string { return us.Label }

func (us *UseStatement) statementNode() {}
func (us *UseStatement) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "USE"...)
}
func (us *UseStatement) AppendString(dst []byte) []byte {
	dst = append(dst, "USE "...)
	dst = append(dst, us.ModuleName...)
	if len(us.Only) > 0 {
		dst = append(dst, ", ONLY: "...)
		for i, name := range us.Only {
			if i > 0 {
				dst = append(dst, ", "...)
			}
			dst = append(dst, name...)
		}
	}
	return dst
}

// CommonStmt declares variables in a COMMON block for shared storage between
// program units. COMMON blocks are a Fortran 77 feature for sharing variables
// without explicit parameter passing.
//
// Example:
//
//	COMMON /block1/ a, b, c
//	COMMON /block2/ x, y, z
//	COMMON // blank_var        ! Blank COMMON (no name)
//	COMMON /block3/ arr(10, 20), scalar
type CommonStmt struct {
	BlockName  string       // Empty string for blank COMMON
	Variables  []string     // Variable names in this block
	ArraySpecs []*ArraySpec // Array specs for each variable (nil for scalars)
	Label      string
	Position
}

var _ Statement = (*CommonStmt)(nil) // compile time check of interface implementation.

func (cs *CommonStmt) GetLabel() string { return cs.Label }

func (cs *CommonStmt) statementNode() {}
func (cs *CommonStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "COMMON"...)
}
func (cs *CommonStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "COMMON"...)
	if cs.BlockName != "" {
		dst = append(dst, " /"...)
		dst = append(dst, cs.BlockName...)
		dst = append(dst, '/')
	} else {
		dst = append(dst, " //"...)
	}
	for i, v := range cs.Variables {
		if i > 0 {
			dst = append(dst, ", "...)
		} else {
			dst = append(dst, ' ')
		}
		dst = append(dst, v...)
	}
	return dst
}

// ExternalStmt declares names as external procedures (user-defined functions/subroutines).
// This helps distinguish user procedures from intrinsic functions.
//
// Example:
//
//	EXTERNAL mysub, myfunc
type ExternalStmt struct {
	Names []string // Procedure names declared EXTERNAL
	Label string
	Position
}

var _ Statement = (*ExternalStmt)(nil) // compile time check of interface implementation.

func (es *ExternalStmt) GetLabel() string { return es.Label }

func (es *ExternalStmt) statementNode() {}
func (es *ExternalStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "EXTERNAL"...)
}
func (es *ExternalStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "EXTERNAL"...)
	for i, name := range es.Names {
		if i > 0 {
			dst = append(dst, ", "...)
		} else {
			dst = append(dst, ' ')
		}
		dst = append(dst, name...)
	}
	return dst
}

// IntrinsicStmt declares names as intrinsic functions (built-in Fortran functions).
// This prevents name conflicts with user-defined procedures.
//
// Example:
//
//	INTRINSIC sin, cos, sqrt
type IntrinsicStmt struct {
	Names []string // Function names declared INTRINSIC
	Label string
	Position
}

var _ Statement = (*IntrinsicStmt)(nil) // compile time check of interface implementation.

func (is *IntrinsicStmt) GetLabel() string { return is.Label }

func (is *IntrinsicStmt) statementNode() {}
func (is *IntrinsicStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "INTRINSIC"...)
}
func (is *IntrinsicStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "INTRINSIC"...)
	for i, name := range is.Names {
		if i > 0 {
			dst = append(dst, ", "...)
		} else {
			dst = append(dst, ' ')
		}
		dst = append(dst, name...)
	}
	return dst
}

// DimensionStmt declares array dimensions for variables using IMPLICIT typing.
// Standalone DIMENSION statements are common in F77 code.
//
// Example:
//
//	DIMENSION AA(100), BB(10,20), CC(5)
type DimensionStmt struct {
	Variables  []string     // Variable names
	ArraySpecs []*ArraySpec // Array dimensions for each variable
	Label      string
	Position
}

var _ Statement = (*DimensionStmt)(nil) // compile time check of interface implementation.

func (ds *DimensionStmt) GetLabel() string { return ds.Label }

func (ds *DimensionStmt) statementNode() {}
func (ds *DimensionStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "DIMENSION"...)
}
func (ds *DimensionStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "DIMENSION"...)
	for i, v := range ds.Variables {
		if i > 0 {
			dst = append(dst, ", "...)
		} else {
			dst = append(dst, ' ')
		}
		dst = append(dst, v...)
		if ds.ArraySpecs[i] != nil {
			dst = append(dst, '(')
			for j, bound := range ds.ArraySpecs[i].Bounds {
				if j > 0 {
					dst = append(dst, ',')
				}
				if bound.Lower != nil {
					dst = bound.Lower.AppendString(dst)
					dst = append(dst, ':')
				}
				if bound.Upper != nil {
					dst = bound.Upper.AppendString(dst)
				}
			}
			dst = append(dst, ')')
		}
	}
	return dst
}

// EquivalenceStmt declares that multiple variables share the same memory location.
// This enables type punning and memory aliasing in Fortran, typically used for:
// - Viewing the same memory as different types (e.g., REAL vs INTEGER)
// - Overlaying arrays to save memory
// - Interfacing with legacy code that uses memory tricks
//
// Syntax:
//
//	EQUIVALENCE (var1, var2, ...), (var3, var4, ...), ...
//
// Each parenthesized group is an "equivalence set" - all variables in the set
// share the same storage location. Array subscripts may appear to specify offsets.
//
// Example:
//
//	DOUBLE PRECISION :: DEFALT
//	INTEGER, DIMENSION(2) :: I_DEFALT
//	EQUIVALENCE (DEFALT, I_DEFALT)
//
// This makes DEFALT and I_DEFALT(1) occupy the same 8 bytes of memory,
// allowing the same memory to be viewed as either a float64 or two int32 values.
type EquivalenceStmt struct {
	Sets  [][]Expression // Each set is a list of variable names/refs that share memory
	Label string
	Position
}

var _ Statement = (*EquivalenceStmt)(nil) // compile time check of interface implementation.

func (es *EquivalenceStmt) GetLabel() string { return es.Label }

func (es *EquivalenceStmt) statementNode() {}
func (es *EquivalenceStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "EQUIVALENCE"...)
}
func (es *EquivalenceStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "EQUIVALENCE"...)
	for i, set := range es.Sets {
		if i > 0 {
			dst = append(dst, ", "...)
		}
		dst = append(dst, " ("...)
		for j, expr := range set {
			if j > 0 {
				dst = append(dst, ", "...)
			}
			dst = expr.AppendString(dst)
		}
		dst = append(dst, ')')
	}
	return dst
}

// PointerCrayStmt declares Cray-style pointers (Fortran 77 extension).
// This is distinct from modern F90+ POINTER attribute declarations.
//
// Cray-style syntax:
//
//	POINTER (pointer_var, pointee), (ptr2, pointee2), ...
//
// where pointer_var is an INTEGER holding a memory address and pointee
// is the variable accessed through that address.
//
// Example:
//
//	POINTER (NPAA,AA(1)), (NPII,II(1)), (NPLL,LL(1))
type PointerCrayStmt struct {
	Pointers []PointerCrayPair
	Label    string
	Position
}

// PointerCrayPair represents a single (pointer_var, pointee) pair.
type PointerCrayPair struct {
	PointerVar string     // e.g., "NPAA" - holds memory address
	Pointee    string     // e.g., "AA" - variable accessed through pointer
	ArraySpec  *ArraySpec // e.g., "(1)" from AA(1), often a placeholder dimension
}

var _ Statement = (*PointerCrayStmt)(nil) // compile time check

func (ps *PointerCrayStmt) GetLabel() string { return ps.Label }

func (ps *PointerCrayStmt) statementNode() {}

func (ps *PointerCrayStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "POINTER"...)
}

func (ps *PointerCrayStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "POINTER"...)
	for i, pair := range ps.Pointers {
		if i > 0 {
			dst = append(dst, ", "...)
		} else {
			dst = append(dst, ' ')
		}
		dst = append(dst, '(')
		dst = append(dst, pair.PointerVar...)
		dst = append(dst, ", "...)
		dst = append(dst, pair.Pointee...)
		if pair.ArraySpec != nil {
			dst = append(dst, '(')
			for j, bound := range pair.ArraySpec.Bounds {
				if j > 0 {
					dst = append(dst, ',')
				}
				if bound.Lower != nil {
					dst = bound.Lower.AppendString(dst)
					dst = append(dst, ':')
				}
				if bound.Upper != nil {
					dst = bound.Upper.AppendString(dst)
				}
			}
			dst = append(dst, ')')
		}
		dst = append(dst, ')')
	}
	return dst
}

// DataStmt initializes variables with specified values in Fortran 77 style.
// DATA statements can use implied DO loops for array initialization.
//
// Example:
//
//	DATA x, y / 1.0, 2.0 /
//	DATA (arr(i), i=1,10) / 10*0.0 /
//	DATA a, b, c / 1, 2, 3 /
type DataStmt struct {
	Variables []Expression // Variable names (identifiers or array refs)
	Values    []Expression // Initialization values
	Label     string
	Position
}

var _ Statement = (*DataStmt)(nil)

func (ds *DataStmt) GetLabel() string { return ds.Label }
func (ds *DataStmt) statementNode()   {}
func (ds *DataStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "DATA"...)
}
func (ds *DataStmt) AppendString(dst []byte) []byte {
	return append(dst, "DATA"...)
}

// TypeDeclaration declares variables with a specific type and optional attributes.
// Fortran 90 syntax uses :: to separate attributes from the entity list. Attributes
// control properties like storage (SAVE, PARAMETER), intent (IN, OUT, INOUT),
// and shape (DIMENSION, ALLOCATABLE).
//
// Example:
//
//	<type> [, <attributes>] :: <entity-list>
//	INTEGER :: i, j, k
//	REAL, DIMENSION(10) :: array
//	REAL, PARAMETER :: PI = 3.14159
//	CHARACTER(LEN=80), INTENT(IN) :: filename
type TypeDeclaration struct {
	Type     TypeSpec
	Entities []DeclEntity // Variables being declared
	Label    string
	Position
}

var _ Statement = (*TypeDeclaration)(nil) // compile time check of interface implementation.

func (td *TypeDeclaration) GetLabel() string { return td.Label }

func (td *TypeDeclaration) statementNode() {}
func (td *TypeDeclaration) AppendTokenLiteral(dst []byte) []byte {
	return td.Type.AppendString(dst)
}
func (td *TypeDeclaration) AppendString(dst []byte) []byte {
	dst = td.Type.AppendString(dst)
	dst = append(dst, " :: "...)
	for i, entity := range td.Entities {
		if i > 0 {
			dst = append(dst, ", "...)
		}
		dst = append(dst, entity.Name...)
	}
	return dst
}

// ArraySpecKind represents the kind of array specification
type ArraySpecKind int

const (
	ArraySpecExplicit    ArraySpecKind = iota // Explicit shape: (1:10, 1:20)
	ArraySpecAssumed                          // Assumed shape: (:, :)
	ArraySpecDeferred                         // Deferred shape: (:) with ALLOCATABLE/POINTER
	ArraySpecAssumedSize                      // Assumed size: (*) - F77 style
)

func (ask ArraySpecKind) String() string {
	switch ask {
	case ArraySpecExplicit:
		return "explicit"
	case ArraySpecAssumed:
		return "assumed"
	case ArraySpecDeferred:
		return "deferred"
	case ArraySpecAssumedSize:
		return "assumed-size"
	default:
		return "unknown"
	}
}

// ArrayBound represents a single dimension's bounds (lower:upper)
// For explicit shape: Lower and/or Upper are Expression nodes
// For assumed shape (:): Lower and Upper are nil
// For assumed size (*): Upper is Identifier("*"), Lower is nil
type ArrayBound struct {
	Lower Expression // Lower bound expression (nil for assumed shape or when omitted)
	Upper Expression // Upper bound expression (nil for assumed shape, Identifier("*") for assumed size)
}

func (ab *ArrayBound) AppendString(dst []byte) []byte {
	if id, ok := ab.Upper.(*Identifier); ok && id.Value == "*" {
		return append(dst, '*')
	}
	if ab.Lower != nil {
		dst = ab.Lower.AppendString(dst)
	}
	dst = append(dst, ':')
	if ab.Upper != nil {
		dst = ab.Upper.AppendString(dst)
	}
	return dst
}

// ArraySpec represents array dimension specification
type ArraySpec struct {
	Kind   ArraySpecKind
	Bounds []ArrayBound // One bound per dimension
}

func (as *ArraySpec) AppendString(dst []byte) []byte {
	dst = append(dst, '(')
	for i := range as.Bounds {
		dst = as.Bounds[i].AppendString(dst)
		if i != len(as.Bounds)-1 {
			dst = append(dst, ',')
		}
	}
	dst = append(dst, ')')
	return dst
}

type TypeAttribute struct {
	Token     token.Token
	Expr      Expression
	Dimension *ArraySpec
}

func (ta *TypeAttribute) AppendString(dst []byte) []byte {
	dst = append(dst, ta.Token.String()...)
	if ta.Expr != nil {
		dst = append(dst, '(')
		dst = ta.Expr.AppendString(dst)
		dst = append(dst, ')')
	} else if ta.Dimension != nil {
		dst = ta.Dimension.AppendString(dst)
	}
	return dst
}

type TokenExpr struct {
	Token token.Token
	Position
}

var _ Expression = (*TokenExpr)(nil) // compile time check of interface implementation.

func (te *TokenExpr) expressionNode() {}
func (te *TokenExpr) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, te.Token.String()...)
}
func (te *TokenExpr) AppendString(dst []byte) []byte {
	return te.AppendString(dst)
}

// DeclEntity represents a single entity in a type declaration.
// Multiple entities can appear in one statement (e.g., "INTEGER :: i=1, j=2").
//
// Parsing edge cases:
//   - F90: REAL, DIMENSION(3) :: field = (/ 0., 1., 2. /)  → ArraySpec + Initializer
//   - F90: INTEGER :: i = 1, j = 2  → Multiple entities, each with own Initializer
//   - F90: CHARACTER(LEN=*) :: str  → CharLen = Identifier("*")
//   - F90: TYPE(t_pair) :: pair = t_pair(1, 0.5)  → Derived type with Initializer
//   - F77: INTEGER I /2/, J /3/  → Multiple entities with DATA-style Initializer
//   - F77: CHARACTER*80 NAME  → CharLen = IntegerLiteral("80")
//
// Note: Initializer is currently a string (becomes Expression in Phase 4).
type DeclEntity struct {
	Name      string
	Type      *TypeSpec
	ArraySpec *ArraySpec // Array dimensions if this is an array
	Init      Expression
}

func (de *DeclEntity) Dimension() *ArraySpec {
	if de.ArraySpec != nil {
		return de.ArraySpec
	}
	for i := range de.Type.Attributes {
		attr := &de.Type.Attributes[i]
		if attr.Token == token.DIMENSION {
			return attr.Dimension
		}
	}
	return nil
}

func (de *DeclEntity) AppendString(dst []byte) []byte {
	dst = append(dst, de.Name...)
	if de.ArraySpec != nil {
		dst = de.ArraySpec.AppendString(dst)
	}
	if de.Init != nil {
		dst = append(dst, ' ')
		dst = de.Init.AppendString(dst)
	}
	return dst
}

// IntentType represents the INTENT attribute direction
type IntentType int

const (
	// IntentInOut
	intentDefault IntentType = iota
	IntentInOut
	IntentIn
	IntentOut
)

func (it IntentType) String() string {
	switch it {
	case IntentIn:
		return "IN"
	case IntentOut:
		return "OUT"
	case IntentInOut:
		return "INOUT"
	default:
		return ""
	}
}

// Parameter represents a formal argument to a [Function] or [Subroutine] with
// complete type information. Parameters may have INTENT attributes controlling
// whether they can be read, written, or both.
//
// Example:
//
//	REAL, INTENT(IN) :: x
//	INTEGER, INTENT(OUT) :: result
//	REAL, DIMENSION(:), INTENT(INOUT) :: array
//	CHARACTER(LEN=*), OPTIONAL :: message
type Parameter struct {
	Name string // Parameter name
	Decl *DeclEntity
}

// Identifier represents a variable name, function name, or other named entity
// in Fortran source code. Fortran identifiers are case-insensitive and can
// contain letters, digits, and underscores (starting with a letter).
//
// Example:
//
//	x
//	temperature
//	max_iterations
//	MyVariable
type Identifier struct {
	Value string
	Position
}

func (i *Identifier) expressionNode() {}
func (i *Identifier) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, i.Value...)
}
func (i *Identifier) AppendString(dst []byte) []byte {
	return append(dst, i.Value...)
}

// RangeExpr represents array slice/range syntax used in array sections and
// subscript triplets. Omitted bounds use defaults: lower bound 1, upper bound
// equal to dimension size, stride 1.
//
// Example:
//
//	[<start>]:[<end>][:<stride>]
//	:
//	1:10
//	::2
//	start:end:step
type RangeExpr struct {
	Start  Expression // nil means implicit start (1)
	End    Expression // nil means implicit end (size)
	Stride Expression // nil means stride of 1 (F90 feature)
	Position
}

var _ Expression = (*RangeExpr)(nil)

func (r *RangeExpr) expressionNode() {}
func (r *RangeExpr) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, ":"...)
}
func (r *RangeExpr) AppendString(dst []byte) []byte {
	if r.Start != nil {
		dst = r.Start.AppendString(dst)
	}
	dst = append(dst, ':')
	if r.End != nil {
		dst = r.End.AppendString(dst)
	}
	if r.Stride != nil {
		dst = append(dst, ':')
		dst = r.Stride.AppendString(dst)
	}
	return dst
}

// IntegerLiteral represents an integer constant in source code. Fortran supports
// decimal, octal, and hexadecimal integer literals, with optional kind parameters.
//
// Example:
//
//	42
//	-123
//	1_8
//	Z'FF'
type IntegerLiteral struct {
	Value int64  // Parsed integer value (0 if not yet parsed)
	Raw   string // Original text representation
	Position
}

var _ Expression = (*IntegerLiteral)(nil) // compile time check of interface implementation.

func (il *IntegerLiteral) expressionNode() {}
func (il *IntegerLiteral) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "INTEGER"...)
}
func (il *IntegerLiteral) AppendString(dst []byte) []byte {
	// Convert int64 to string
	return append(dst, il.Raw...)
}

// RealLiteral represents a floating-point constant in source code. Supports
// decimal notation, scientific notation, and kind parameters for precision control.
//
// Example:
//
//	3.14
//	1.0E-10
//	2.5D0
//	1.23_4
type RealLiteral struct {
	Value float64
	Raw   string // Original text representation
	Position
}

var _ Expression = (*RealLiteral)(nil) // compile time check of interface implementation.

func (rl *RealLiteral) expressionNode() {}
func (rl *RealLiteral) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "REAL"...)
}
func (rl *RealLiteral) AppendString(dst []byte) []byte {
	return append(dst, rl.Raw...)
}

// StringLiteral represents a character string constant enclosed in single quotes,
// double quotes, or Fortran 2003 delimiters. Used for text data and format strings.
//
// Example:
//
//	'Hello, World!'
//	"Fortran 90"
//	'It''s a quote'
type StringLiteral struct {
	Value string
	Position
}

var _ Expression = (*StringLiteral)(nil) // compile time check of interface implementation.

func (sl *StringLiteral) expressionNode() {}
func (sl *StringLiteral) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "STRING"...)
}
func (sl *StringLiteral) AppendString(dst []byte) []byte {
	dst = append(dst, '"')
	dst = append(dst, sl.Value...)
	dst = append(dst, '"')
	return dst
}

// LogicalLiteral represents a boolean constant in Fortran, enclosed in periods.
// Fortran uses .TRUE. and .FALSE. for logical values rather than true/false.
//
// Example:
//
//	.TRUE.
//	.FALSE.
//	.true.
type LogicalLiteral struct {
	Value bool
	Position
}

var _ Expression = (*LogicalLiteral)(nil) // compile time check of interface implementation.

func (ll *LogicalLiteral) expressionNode() {}
func (ll *LogicalLiteral) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "LOGICAL"...)
}
func (ll *LogicalLiteral) AppendString(dst []byte) []byte {
	if ll.Value {
		return append(dst, ".TRUE."...)
	}
	return append(dst, ".FALSE."...)
}

// AlternateReturnArg represents an alternate return argument in a CALL statement.
// These are Fortran 77 constructs that allow subroutines to return to different
// labels in the caller based on error conditions or status.
//
// Example:
//
//	CALL SUB(*100, *200, x, y)
//	! *100 and *200 are alternate return labels
type AlternateReturnArg struct {
	Label string // The label to jump to (without the *)
	Position
}

var _ Expression = (*AlternateReturnArg)(nil) // compile time check of interface implementation.

func (ara *AlternateReturnArg) expressionNode() {}
func (ara *AlternateReturnArg) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "ALTERNATE_RETURN"...)
}
func (ara *AlternateReturnArg) AppendString(dst []byte) []byte {
	dst = append(dst, '*')
	dst = append(dst, ara.Label...)
	return dst
}

// BinaryExpr represents a binary operation with two operands. Fortran supports
// arithmetic operators (+, -, *, /, **), relational operators (.LT., .GT., etc.),
// and logical operators (.AND., .OR., .NOT.).
//
// Example:
//
//	<left> <operator> <right>
//	a + b
//	x * y
//	i .LT. n
//	flag .AND. status
type BinaryExpr struct {
	Op    token.Token // Operator token
	Left  Expression
	Right Expression
	Position
}

var _ Expression = (*BinaryExpr)(nil) // compile time check of interface implementation.

func (be *BinaryExpr) expressionNode() {}
func (be *BinaryExpr) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, be.Op.String()...)
}
func (be *BinaryExpr) AppendString(dst []byte) []byte {
	dst = be.Left.AppendString(dst)
	dst = append(dst, ' ')
	dst = append(dst, be.Op.String()...)
	dst = append(dst, ' ')
	dst = be.Right.AppendString(dst)
	return dst
}

// UnaryExpr represents a unary prefix operation with a single operand. Common
// unary operators include arithmetic negation (-), unary plus (+), and logical
// negation (.NOT.).
//
// Example:
//
//	<operator> <operand>
//	-x
//	+value
//	.NOT. flag
type UnaryExpr struct {
	Op      token.Token // Operator token
	Operand Expression
	Position
}

var _ Expression = (*UnaryExpr)(nil) // compile time check of interface implementation.

func (ue *UnaryExpr) expressionNode() {}
func (ue *UnaryExpr) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, ue.Op.String()...)
}
func (ue *UnaryExpr) AppendString(dst []byte) []byte {
	dst = append(dst, ue.Op.String()...)
	dst = append(dst, ' ')
	dst = ue.Operand.AppendString(dst)
	return dst
}

// FunctionCall represents an invocation of a function that returns a value.
// Functions can be intrinsic (built-in) or user-defined. Unlike [CallStmt]
// for subroutines, function calls appear in expressions.
//
// Example:
//
//	<function-name>([<argument-list>])
//	sqrt(x)
//	max(a, b, c)
//	my_function(i, j)
type FunctionCall struct {
	Name string
	Args []Expression
	Position
}

var _ Expression = (*FunctionCall)(nil) // compile time check of interface implementation.

func (fc *FunctionCall) expressionNode() {}
func (fc *FunctionCall) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "CALL"...)
}
func (fc *FunctionCall) AppendString(dst []byte) []byte {
	dst = append(dst, fc.Name...)
	dst = append(dst, '(')
	for i, arg := range fc.Args {
		if i > 0 {
			dst = append(dst, ", "...)
		}
		dst = arg.AppendString(dst)
	}
	dst = append(dst, ')')
	return dst
}

// ArrayRef represents a reference to a single element of an array using integer
// subscripts. For array sections (ranges), use [ArraySection] instead.
//
// Example:
//
//	<array-name>(<subscript-list>)
//	arr(i)
//	matrix(i, j)
//	cube(x, y, z)
type ArrayRef struct {
	Name       string
	Subscripts []Expression
	Position
}

var _ Expression = (*ArrayRef)(nil) // compile time check of interface implementation.

func (ar *ArrayRef) expressionNode() {}
func (ar *ArrayRef) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "ARRAYREF"...)
}
func (ar *ArrayRef) AppendString(dst []byte) []byte {
	dst = append(dst, ar.Name...)
	dst = append(dst, '(')
	for i, sub := range ar.Subscripts {
		if i > 0 {
			dst = append(dst, ", "...)
		}
		dst = sub.AppendString(dst)
	}
	dst = append(dst, ')')
	return dst
}

// ParenExpr represents an expression enclosed in parentheses for grouping or
// to override operator precedence. The parentheses do not change the value
// but may affect evaluation order.
//
// Example:
//
//	(<expression>)
//	(a + b)
//	(x * y) / z
type ParenExpr struct {
	Expr Expression
	Position
}

var _ Expression = (*ParenExpr)(nil) // compile time check of interface implementation.

func (pe *ParenExpr) expressionNode() {}
func (pe *ParenExpr) AppendTokenLiteral(dst []byte) []byte {
	return pe.Expr.AppendTokenLiteral(dst)
}
func (pe *ParenExpr) AppendString(dst []byte) []byte {
	dst = append(dst, '(')
	dst = pe.Expr.AppendString(dst)
	dst = append(dst, ')')
	return dst
}

// ImpliedDoLoop represents an implied DO loop used in array constructors and
// I/O lists to generate sequences of values without explicit loop statements.
//
// Example:
//
//	(<expression-list>, <loop-var> = <start>, <end> [, <stride>])
//	(i, i=1, 10)
//	(arr(i), i=1, n, 2)
//	(matrix(i,j), i=1, rows)
type ImpliedDoLoop struct {
	Expressions []Expression // The output list before the loop control
	LoopVar     string       // Loop variable name
	Start       Expression   // Start value
	End         Expression   // End value
	Stride      Expression   // Optional stride (nil if not specified)
	Position
}

var _ Expression = (*ImpliedDoLoop)(nil) // compile time check of interface implementation.

func (idl *ImpliedDoLoop) expressionNode() {}
func (idl *ImpliedDoLoop) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "IMPLIED-DO"...)
}
func (idl *ImpliedDoLoop) AppendString(dst []byte) []byte {
	dst = append(dst, '(')
	for i, expr := range idl.Expressions {
		if i > 0 {
			dst = append(dst, ", "...)
		}
		dst = expr.AppendString(dst)
	}
	dst = append(dst, ", "...)
	dst = append(dst, idl.LoopVar...)
	dst = append(dst, " = "...)
	dst = idl.Start.AppendString(dst)
	dst = append(dst, ", "...)
	dst = idl.End.AppendString(dst)
	if idl.Stride != nil {
		dst = append(dst, ", "...)
		dst = idl.Stride.AppendString(dst)
	}
	dst = append(dst, ')')
	return dst
}

// Executable Statements (Phase 5)

// AssignmentStmt assigns the value of an expression to a variable, array element,
// or derived type component. The target must be a valid lvalue (assignable entity).
//
// Example:
//
//	<target> = <expression>
//	x = y + 1
//	arr(i) = 42
//	matrix(i,j) = a * b
//	person%age = 30
type AssignmentStmt struct {
	Target              Expression
	Value               Expression
	Label               string
	IsPointerAssignment bool
	Position
}

var _ Statement = (*AssignmentStmt)(nil) // compile time check of interface implementation.

func (as *AssignmentStmt) GetLabel() string { return as.Label }

func (as *AssignmentStmt) statementNode() {}
func (as *AssignmentStmt) AppendTokenLiteral(dst []byte) []byte {
	if as.IsPointerAssignment {
		return append(dst, "=>"...)
	}
	return append(dst, '=')
}
func (as *AssignmentStmt) AppendString(dst []byte) []byte {
	dst = as.Target.AppendString(dst)
	dst = append(dst, ' ')
	dst = as.AppendTokenLiteral(dst)
	dst = append(dst, ' ')
	dst = as.Value.AppendString(dst)
	return dst
}

// IfStmt represents a block IF construct that conditionally executes different
// code blocks based on logical conditions. It supports multiple branches with
// ELSE IF and a final ELSE clause.
//
// Example:
//
//	IF (<condition>) THEN
//	  <statements>
//	[ELSE IF (<condition>) THEN
//	  <statements>]...
//	[ELSE
//	  <statements>]
//	END IF
//
//	IF (x > 0) THEN
//	  PRINT *, 'Positive'
//	ELSE IF (x < 0) THEN
//	  PRINT *, 'Negative'
//	ELSE
//	  PRINT *, 'Zero'
//	END IF
type IfStmt struct {
	Condition      Expression
	ThenPart       []Statement
	ElseIfParts    []ElseIfClause
	ElsePart       []Statement
	Label          string
	ConstructLabel string
	EndLabel       string // Label on END IF statement (F77)
	Position
}

var _ Statement = (*IfStmt)(nil) // compile time check of interface implementation.

func (is *IfStmt) GetLabel() string { return is.Label }

// ElseIfClause represents a single ELSE IF block
type ElseIfClause struct {
	Condition Expression
	ThenPart  []Statement
	Position
}

func (is *IfStmt) statementNode() {}
func (is *IfStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "IF"...)
}
func (is *IfStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "IF ("...)
	dst = is.Condition.AppendString(dst)
	dst = append(dst, ") THEN"...)
	return dst
}

// ArithmeticIfStmt is a three-way branch statement that transfers control based
// on the sign of an arithmetic expression. This Fortran 77 feature is superseded
// by the more readable block [IfStmt] in modern Fortran.
//
// Example:
//
//	IF (<expression>) <neg-label>, <zero-label>, <pos-label>
//	IF (x - y) 10, 20, 30
//	IF (balance) 100, 200, 300
//
// Branches to the first label if expression < 0, second if == 0, third if > 0.
type ArithmeticIfStmt struct {
	Condition     Expression // Arithmetic expression to evaluate
	NegativeLabel string     // Label to jump to if condition < 0
	ZeroLabel     string     // Label to jump to if condition == 0
	PositiveLabel string     // Label to jump to if condition > 0
	Label         string     // Optional statement label
	Position
}

var _ Statement = (*ArithmeticIfStmt)(nil)

func (ais *ArithmeticIfStmt) GetLabel() string { return ais.Label }
func (ais *ArithmeticIfStmt) statementNode()   {}
func (ais *ArithmeticIfStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "IF"...)
}
func (ais *ArithmeticIfStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "IF ("...)
	dst = ais.Condition.AppendString(dst)
	dst = append(dst, ") "...)
	dst = append(dst, ais.NegativeLabel...)
	dst = append(dst, ", "...)
	dst = append(dst, ais.ZeroLabel...)
	dst = append(dst, ", "...)
	dst = append(dst, ais.PositiveLabel...)
	return dst
}

// DoLoop represents an iterative loop construct that executes a block of
// statements repeatedly. Supports both counted loops (with loop variable)
// and WHILE loops.
//
// Example:
//
//	DO [<label>] [<var> = <start>, <end> [, <step>]]
//	  <statements>
//	END DO [<label>]
//
//	DO i = 1, 10
//	  PRINT *, i
//	END DO
//
//	DO 100 i = 1, n, 2
//	  sum = sum + arr(i)
//	100 CONTINUE
type DoLoop struct {
	Var            string // Loop variable (empty for DO WHILE)
	Start          Expression
	End            Expression
	Step           Expression
	Body           []Statement
	Label          string // Statement label (line prefix, e.g., "10" in "10 DO...")
	TargetLabel    string // DO target label (F77, e.g., "20" in "DO 20 I=1,10")
	EndLabel       string // Label on END DO statement (F77, e.g., "20" in "20 END DO")
	ConstructLabel string
	Position
}

var _ Statement = (*DoLoop)(nil) // compile time check of interface implementation.

func (dl *DoLoop) GetLabel() string { return dl.Label }

func (dl *DoLoop) statementNode() {}
func (dl *DoLoop) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "DO"...)
}
func (dl *DoLoop) AppendString(dst []byte) []byte {
	dst = append(dst, "DO"...)
	if dl.Var != "" {
		dst = append(dst, " "...)
		dst = append(dst, dl.Var...)
		dst = append(dst, " = "...)
		dst = dl.Start.AppendString(dst)
		dst = append(dst, ", "...)
		dst = dl.End.AppendString(dst)
		if dl.Step != nil {
			dst = append(dst, ", "...)
			dst = dl.Step.AppendString(dst)
		}
	}
	return dst
}

// SelectCaseStmt represents a multi-way branch construct that executes different
// code blocks based on the value of an expression. Each case specifies one or
// more matching values, with an optional default case.
//
// Example:
//
//	SELECT CASE (<expression>)
//	CASE (<value-list>)
//	  <statements>
//	[CASE (<value-list>)
//	  <statements>]...
//	[CASE DEFAULT
//	  <statements>]
//	END SELECT
//
//	SELECT CASE (status)
//	CASE (1)
//	  PRINT *, 'Success'
//	CASE (2, 3)
//	  PRINT *, 'Warning'
//	CASE DEFAULT
//	  PRINT *, 'Error'
//	END SELECT
type SelectCaseStmt struct {
	Expression     Expression   // The expression being selected on
	Cases          []CaseClause // List of CASE clauses
	Label          string       // Statement label
	ConstructLabel string
	EndLabel       string // Label on END SELECT

	Position
}

var _ Statement = (*SelectCaseStmt)(nil)

func (s *SelectCaseStmt) GetLabel() string { return s.Label }
func (s *SelectCaseStmt) statementNode()   {}
func (s *SelectCaseStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "SELECT CASE"...)
}
func (s *SelectCaseStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "SELECT CASE ("...)
	if s.Expression != nil {
		dst = s.Expression.AppendString(dst)
	}
	dst = append(dst, ")"...)
	return dst
}

// CaseClause represents a single CASE clause within a SELECT CASE
type CaseClause struct {
	Values    []Expression // Values for this case (nil for CASE DEFAULT)
	Body      []Statement  // Statements in this case
	IsDefault bool         // True if this is CASE DEFAULT
	Position
}

// CallStmt invokes a subroutine with optional arguments. Unlike functions,
// subroutines do not return a value but may modify arguments or perform I/O.
//
// Example:
//
//	CALL <subroutine-name> [(<argument-list>)]
//	CALL initialize
//	CALL compute(x, y, result)
//	CALL output_data(array, n, filename)
type CallStmt struct {
	Name  string
	Args  []Expression
	Label string
	Position
}

var _ Statement = (*CallStmt)(nil) // compile time check of interface implementation.

func (cs *CallStmt) GetLabel() string { return cs.Label }

func (cs *CallStmt) statementNode() {}
func (cs *CallStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "CALL"...)
}
func (cs *CallStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "CALL "...)
	dst = append(dst, cs.Name...)
	if len(cs.Args) > 0 {
		dst = append(dst, '(')
		for i, arg := range cs.Args {
			if i > 0 {
				dst = append(dst, ", "...)
			}
			dst = arg.AppendString(dst)
		}
		dst = append(dst, ')')
	}
	return dst
}

// EntryStmt defines an alternate entry point in a subroutine or function (F77 feature).
// It allows multiple entry points into a single program unit.
//
// Example:
//
//	ENTRY alternate_name(param1, param2)
type EntryStmt struct {
	Name       string
	Parameters []Parameter
	Label      string
	Position
}

var _ Statement = (*EntryStmt)(nil) // compile time check of interface implementation.

func (es *EntryStmt) GetLabel() string { return es.Label }

func (es *EntryStmt) statementNode() {}
func (es *EntryStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "ENTRY"...)
}
func (es *EntryStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "ENTRY "...)
	dst = append(dst, es.Name...)
	if len(es.Parameters) > 0 {
		dst = append(dst, '(')
		for i, param := range es.Parameters {
			if i > 0 {
				dst = append(dst, ", "...)
			}
			dst = append(dst, param.Name...)
		}
		dst = append(dst, ')')
	}
	return dst
}

// ReturnStmt transfers control back to the calling program unit (function or
// subroutine). In functions, the return value must be assigned before returning.
// In Fortran 77, RETURN can take an integer argument for alternate returns.
//
// Example:
//
//	RETURN
//	RETURN 1
type ReturnStmt struct {
	AlternateReturn Expression // Optional: for RETURN <integer> (Fortran 77 alternate returns)
	Label           string
	Position
}

var _ Statement = (*ReturnStmt)(nil) // compile time check of interface implementation.

func (rs *ReturnStmt) GetLabel() string { return rs.Label }

func (rs *ReturnStmt) statementNode() {}
func (rs *ReturnStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "RETURN"...)
}
func (rs *ReturnStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "RETURN"...)
	if rs.AlternateReturn != nil {
		dst = append(dst, ' ')
		dst = rs.AlternateReturn.AppendString(dst)
	}
	return dst
}

// CycleStmt skips the remaining statements in the current iteration of a
// [DoLoop] and proceeds to the next iteration. Similar to 'continue' in C.
//
// Example:
//
//	CYCLE
//
//	DO i = 1, 10
//	  IF (arr(i) < 0) CYCLE
//	  sum = sum + arr(i)
//	END DO
type CycleStmt struct {
	Label         string // Statement label (e.g., "10" in "10 CYCLE")
	ConstructName string // Loop construct name to cycle (e.g., "loop1" in "CYCLE loop1")
	Position
}

var _ Statement = (*CycleStmt)(nil) // compile time check of interface implementation.

func (cs *CycleStmt) GetLabel() string { return cs.Label }

func (cs *CycleStmt) statementNode() {}
func (cs *CycleStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "CYCLE"...)
}
func (cs *CycleStmt) AppendString(dst []byte) []byte {
	return append(dst, "CYCLE"...)
}

// ExitStmt terminates execution of the innermost enclosing [DoLoop], transferring
// control to the statement immediately following the loop's END DO. Unlike
// [CycleStmt] which continues to the next iteration, EXIT leaves the loop entirely.
//
// Example:
//
//	EXIT
//
//	DO i = 1, 100
//	  IF (found) EXIT
//	  CALL search(arr(i), found)
//	END DO
type ExitStmt struct {
	Label         string // Statement label (e.g., "10" in "10 EXIT")
	ConstructName string // Loop construct name to exit (e.g., "loop1" in "EXIT loop1")
	Position
}

var _ Statement = (*ExitStmt)(nil) // compile time check of interface implementation.

func (es *ExitStmt) GetLabel() string { return es.Label }

func (es *ExitStmt) statementNode() {}
func (es *ExitStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "EXIT"...)
}
func (es *ExitStmt) AppendString(dst []byte) []byte {
	return append(dst, "EXIT"...)
}

// ContinueStmt is a no-operation statement primarily used as a labeled target
// for control flow statements like [GotoStmt], [DoLoop] termination labels,
// and branch targets in Fortran 77 code.
//
// Example:
//
//	CONTINUE
//
//	DO 100 i = 1, n
//	  sum = sum + arr(i)
//	100 CONTINUE
type ContinueStmt struct {
	Label string
	Position
}

var _ Statement = (*ContinueStmt)(nil) // compile time check of interface implementation.

func (cs *ContinueStmt) GetLabel() string { return cs.Label }

func (cs *ContinueStmt) statementNode() {}
func (cs *ContinueStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "CONTINUE"...)
}
func (cs *ContinueStmt) AppendString(dst []byte) []byte {
	return append(dst, "CONTINUE"...)
}

// GotoStmt unconditionally transfers control to the statement with the specified
// label. While commonly used in Fortran 77 code, modern Fortran prefers structured
// control flow with [IfStmt], [DoLoop], and [SelectCaseStmt].
//
// Example:
//
//	GO TO <label>
//	GO TO 100
//	IF (error) GO TO 999
//
//	100 PRINT *, 'Target reached'
type GotoStmt struct {
	Target string // The label to jump to
	Label  string // Optional statement label
	Position
}

var _ Statement = (*GotoStmt)(nil) // compile time check of interface implementation.

func (gs *GotoStmt) GetLabel() string { return gs.Label }

func (gs *GotoStmt) statementNode() {}
func (gs *GotoStmt) AppendTokenLiteral(dst []byte) []byte {
	dst = append(dst, "GO TO "...)
	return append(dst, gs.Target...)
}
func (gs *GotoStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "GO TO "...)
	return append(dst, gs.Target...)
}

// AssignStmt assigns a statement label to an integer variable (Fortran 77 feature).
// Used with assigned GOTO statements.
//
// Example:
//
//	ASSIGN <label> TO <variable>
//	ASSIGN 100 TO jump_target
//	ASSIGN 2000 TO IGOTO
type AssignStmt struct {
	LabelValue string // The label being assigned
	Variable   string // The variable to assign to
	Label      string // Optional statement label
	Position
}

var _ Statement = (*AssignStmt)(nil)

func (as *AssignStmt) GetLabel() string { return as.Label }

func (as *AssignStmt) statementNode() {}
func (as *AssignStmt) AppendTokenLiteral(dst []byte) []byte {
	dst = append(dst, "ASSIGN "...)
	dst = append(dst, as.LabelValue...)
	dst = append(dst, " TO "...)
	return append(dst, as.Variable...)
}
func (as *AssignStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "ASSIGN "...)
	dst = append(dst, as.LabelValue...)
	dst = append(dst, " TO "...)
	return append(dst, as.Variable...)
}

// ComputedGotoStmt transfers control to one of several labeled statements based
// on the runtime value of an integer expression. This is a Fortran 77 feature
// largely replaced by [SelectCaseStmt] in modern Fortran.
//
// Example:
//
//	GO TO (<label-list>) <integer-expression>
//	GO TO (10, 20, 30) choice
//	GO TO (100, 200, 300, 400) status
//
// If the expression evaluates to n, control transfers to the nth label in the list.
// Values less than 1 or greater than the list length fall through to the next statement.
type ComputedGotoStmt struct {
	Labels     []string   // List of labels to jump to (stored as strings for consistency)
	Expression Expression // Expression to evaluate (1-based index into Labels)
	Label      string     // Optional statement label
	Position
}

var _ Statement = (*ComputedGotoStmt)(nil)

func (cgs *ComputedGotoStmt) GetLabel() string { return cgs.Label }
func (cgs *ComputedGotoStmt) statementNode()   {}
func (cgs *ComputedGotoStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "GO TO"...)
}
func (cgs *ComputedGotoStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "GO TO ("...)
	for i, label := range cgs.Labels {
		if i > 0 {
			dst = append(dst, ", "...)
		}
		dst = append(dst, label...)
	}
	dst = append(dst, ") "...)
	if cgs.Expression != nil {
		dst = cgs.Expression.AppendString(dst)
	}
	return dst
}

// AssignedGotoStmt transfers control to a label stored in a variable (Fortran 77 feature).
// The variable must have been previously assigned a label using an ASSIGN statement.
// The optional label list restricts which labels the variable can contain.
//
// Example:
//
//	GO TO variable-name [, ( label-list )]
//	GO TO IGOTO
//	GO TO IGOTO, (500, 2000)
type AssignedGotoStmt struct {
	Variable string   // Variable containing the label to jump to
	Labels   []string // Optional list of allowed labels
	Label    string   // Optional statement label
	Position
}

var _ Statement = (*AssignedGotoStmt)(nil)

func (ags *AssignedGotoStmt) GetLabel() string { return ags.Label }
func (ags *AssignedGotoStmt) statementNode()   {}
func (ags *AssignedGotoStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "GO TO"...)
}
func (ags *AssignedGotoStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "GO TO "...)
	dst = append(dst, ags.Variable...)
	if len(ags.Labels) > 0 {
		dst = append(dst, ", ("...)
		for i, label := range ags.Labels {
			if i > 0 {
				dst = append(dst, ", "...)
			}
			dst = append(dst, label...)
		}
		dst = append(dst, ')')
	}
	return dst
}

// InquireStmt queries properties of files or I/O units, such as whether a file
// exists, is open, its name, access method, and other attributes. Results are
// returned through variables specified in the inquiry specifiers.
//
// Example:
//
//	INQUIRE([UNIT=]<unit> | FILE=<filename>, <specifier-list>)
//	INQUIRE(FILE='data.txt', EXIST=lexist)
//	INQUIRE(UNIT=10, OPENED=lopen, NAME=fname)
//	INQUIRE(FILE='output.dat', EXIST=fexist, OPENED=fopen, NUMBER=inum)
type InquireStmt struct {
	Specifiers map[string]Expression // INQUIRE specifiers: UNIT, FILE, EXIST, OPENED, etc.
	OutputList []Expression          // Output items for IOLENGTH form: INQUIRE(IOLENGTH=var) output-list
	Label      string                // Optional statement label
	Position
}

var _ Statement = (*InquireStmt)(nil)

func (is *InquireStmt) GetLabel() string { return is.Label }
func (is *InquireStmt) statementNode()   {}
func (is *InquireStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "INQUIRE"...)
}
func (is *InquireStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "INQUIRE("...)
	first := true
	for key, value := range is.Specifiers {
		if !first {
			dst = append(dst, ", "...)
		}
		first = false
		dst = append(dst, key...)
		dst = append(dst, '=')
		dst = value.AppendString(dst)
	}
	dst = append(dst, ')')
	return dst
}

// OpenStmt establishes a connection between a logical unit number and an
// external file, preparing it for input or output operations with statements
// such as [WriteStmt] and [ReadStmt]. Specifiers control file properties like
// access method, format, and status.
//
// Example:
//
//	OPEN([UNIT=]<unit>, FILE=<filename> [, STATUS=<status>] [, IOSTAT=<var>])
//	OPEN(10, FILE='data.txt', STATUS='OLD')
type OpenStmt struct {
	Specifiers map[string]Expression // OPEN specifiers: UNIT, FILE, STATUS, etc.
	Label      string                // Optional statement label
	Position
}

var _ Statement = (*OpenStmt)(nil) // compile time check of interface implementation.

func (os *OpenStmt) GetLabel() string { return os.Label }

func (os *OpenStmt) statementNode() {}

func (os *OpenStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "OPEN"...)
}

func (os *OpenStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "OPEN("...)
	first := true
	for key, value := range os.Specifiers {
		if !first {
			dst = append(dst, ", "...)
		}
		first = false
		dst = append(dst, key...)
		dst = append(dst, '=')
		dst = value.AppendString(dst)
	}
	dst = append(dst, ')')
	return dst
}

// CloseStmt terminates the connection between a logical unit and an external
// file that was established by [OpenStmt]. The unit becomes available for
// reconnection to another file.
//
// Example:
//
//	CLOSE([UNIT=]<unit> [, STATUS=<status>] [, IOSTAT=<var>])
//	CLOSE(10)
//	CLOSE(UNIT=20, STATUS='KEEP')
type CloseStmt struct {
	Specifiers map[string]Expression // CLOSE specifiers: UNIT, STATUS, IOSTAT, ERR
	Label      string                // Optional statement label
	Position
}

var _ Statement = (*CloseStmt)(nil)

func (cs *CloseStmt) GetLabel() string { return cs.Label }
func (cs *CloseStmt) statementNode()   {}
func (cs *CloseStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "CLOSE"...)
}
func (cs *CloseStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "CLOSE("...)
	first := true
	for key, value := range cs.Specifiers {
		if !first {
			dst = append(dst, ", "...)
		}
		first = false
		dst = append(dst, key...)
		dst = append(dst, '=')
		dst = value.AppendString(dst)
	}
	dst = append(dst, ')')
	return dst
}

// WriteStmt transfers data from internal storage to an external file or
// device specified by a unit number. The format specifier controls how the
// data is formatted.
//
// Example:
//
//	WRITE([UNIT=]<unit>, [FMT=]<format> [, IOSTAT=<var>]) <output-list>
//	WRITE(6, *) 'Hello, World!'
//	WRITE(10, 100) x, y, z
//	WRITE(UNIT=20, FMT='(I5, F10.2)') num, val
type WriteStmt struct {
	Unit       Expression            // Unit specifier (e.g., 91, *, variable)
	Format     Expression            // Format specifier
	Specifiers map[string]Expression // I/O specifiers: END, ERR, IOSTAT, etc.
	OutputList []Expression          // List of expressions to write
	Label      string                // Optional statement label
	Position
}

var _ Statement = (*WriteStmt)(nil) // compile time check of interface implementation.

func (ws *WriteStmt) GetLabel() string { return ws.Label }

func (ws *WriteStmt) statementNode() {}

// ReadStmt transfers data from an external file or device into internal
// storage. The format specifier controls how the data is interpreted.
//
// Example:
//
//	READ([UNIT=]<unit>, [FMT=]<format> [, IOSTAT=<var>] [, END=<label>]) <input-list>
//	READ(*, *) x, y
//	READ(10, 100) name, age
//	READ(UNIT=15, FMT='(I5)', IOSTAT=ios, END=999) num
type ReadStmt struct {
	Unit       Expression            // Unit specifier (e.g., 91, *, variable)
	Format     Expression            // Format specifier
	Specifiers map[string]Expression // I/O specifiers: END, ERR, IOSTAT, etc.
	InputList  []Expression          // List of variables to read into
	Label      string                // Optional statement label
	Position
}

var _ Statement = (*ReadStmt)(nil) // compile time check of interface implementation.

func (rs *ReadStmt) GetLabel() string { return rs.Label }

func (rs *ReadStmt) statementNode() {}

func (rs *ReadStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "READ"...)
}

func (rs *ReadStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "READ(...) "...)
	for i, expr := range rs.InputList {
		if i > 0 {
			dst = append(dst, ", "...)
		}
		dst = expr.AppendString(dst)
	}
	return dst
}

func (ws *WriteStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "WRITE"...)
}
func (ws *WriteStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "WRITE(...) "...)
	for i, expr := range ws.OutputList {
		if i > 0 {
			dst = append(dst, ", "...)
		}
		dst = expr.AppendString(dst)
	}
	return dst
}

// PrintStmt outputs data to the standard output device (typically the terminal).
// It is a simplified form of [WriteStmt] that always writes to unit *.
//
// Example:
//
//	PRINT <format> [, <output-list>]
//	PRINT *, 'Hello, World!'
//	PRINT 100, x, y, z
//	PRINT '(I5, F10.2)', num, val
type PrintStmt struct {
	Format     Expression   // Format specifier (*, label, or character expression)
	OutputList []Expression // List of expressions to print
	Label      string       // Optional statement label
	Position
}

var _ Statement = (*PrintStmt)(nil) // compile time check of interface implementation.

func (ps *PrintStmt) GetLabel() string { return ps.Label }

func (ps *PrintStmt) statementNode() {}

func (ps *PrintStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "PRINT"...)
}

func (ps *PrintStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "PRINT "...)
	if ps.Format != nil {
		dst = ps.Format.AppendString(dst)
	}
	if len(ps.OutputList) > 0 {
		dst = append(dst, ", "...)
		for i, expr := range ps.OutputList {
			if i > 0 {
				dst = append(dst, ", "...)
			}
			dst = expr.AppendString(dst)
		}
	}
	return dst
}

// BackspaceStmt positions a sequential file at the beginning of the preceding
// record, allowing the next read to re-read the previous record.
//
// Example:
//
//	BACKSPACE <unit>
//	BACKSPACE([UNIT=]<unit> [, IOSTAT=<var>] [, ERR=<label>])
//	BACKSPACE 10
//	BACKSPACE(UNIT=15, IOSTAT=ierr)
type BackspaceStmt struct {
	Specifiers map[string]Expression // BACKSPACE specifiers: UNIT, IOSTAT, ERR
	Label      string                // Optional statement label
	Position
}

var _ Statement = (*BackspaceStmt)(nil)

func (bs *BackspaceStmt) GetLabel() string { return bs.Label }
func (bs *BackspaceStmt) statementNode()   {}
func (bs *BackspaceStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "BACKSPACE"...)
}
func (bs *BackspaceStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "BACKSPACE("...)
	first := true
	for key, value := range bs.Specifiers {
		if !first {
			dst = append(dst, ", "...)
		}
		first = false
		dst = append(dst, key...)
		dst = append(dst, '=')
		dst = value.AppendString(dst)
	}
	dst = append(dst, ')')
	return dst
}

// RewindStmt positions a sequential file at its initial point (beginning),
// allowing subsequent reads to start from the first record.
//
// Example:
//
//	REWIND <unit>
//	REWIND([UNIT=]<unit> [, IOSTAT=<var>] [, ERR=<label>])
//	REWIND 25
//	REWIND(UNIT=30, IOSTAT=ierr)
type RewindStmt struct {
	Specifiers map[string]Expression // REWIND specifiers: UNIT, IOSTAT, ERR
	Label      string                // Optional statement label
	Position
}

var _ Statement = (*RewindStmt)(nil)

func (rs *RewindStmt) GetLabel() string { return rs.Label }
func (rs *RewindStmt) statementNode()   {}
func (rs *RewindStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "REWIND"...)
}
func (rs *RewindStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "REWIND("...)
	first := true
	for key, value := range rs.Specifiers {
		if !first {
			dst = append(dst, ", "...)
		}
		first = false
		dst = append(dst, key...)
		dst = append(dst, '=')
		dst = value.AppendString(dst)
	}
	dst = append(dst, ')')
	return dst
}

// EndfileStmt writes an end-of-file record to a sequential file, typically used
// to mark logical divisions in data files or signal the end of usable data.
//
// Example:
//
//	ENDFILE <unit>
//	ENDFILE([UNIT=]<unit> [, IOSTAT=<var>] [, ERR=<label>])
//	ENDFILE 10
//	ENDFILE(UNIT=15, IOSTAT=ierr)
type EndfileStmt struct {
	Specifiers map[string]Expression // ENDFILE specifiers: UNIT, IOSTAT, ERR
	Label      string                // Optional statement label
	Position
}

var _ Statement = (*EndfileStmt)(nil)

func (es *EndfileStmt) GetLabel() string { return es.Label }
func (es *EndfileStmt) statementNode()   {}
func (es *EndfileStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "ENDFILE"...)
}
func (es *EndfileStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "ENDFILE("...)
	first := true
	for key, value := range es.Specifiers {
		if !first {
			dst = append(dst, ", "...)
		}
		first = false
		dst = append(dst, key...)
		dst = append(dst, '=')
		dst = value.AppendString(dst)
	}
	dst = append(dst, ')')
	return dst
}

// StopStmt terminates program execution. An optional code (integer or string)
// can be provided to indicate the reason for termination.
//
// Example:
//
//	STOP [<code>]
//	STOP
//	STOP 123
//	STOP 'Error: Invalid input'
type StopStmt struct {
	Code  Expression // Optional stop code (integer or string)
	Label string     // Optional statement label
	Position
}

var _ Statement = (*StopStmt)(nil)

func (ss *StopStmt) GetLabel() string { return ss.Label }
func (ss *StopStmt) statementNode()   {}
func (ss *StopStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "STOP"...)
}
func (ss *StopStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "STOP"...)
	if ss.Code != nil {
		dst = append(dst, ' ')
		dst = ss.Code.AppendString(dst)
	}
	return dst
}

// FormatStmt defines a reusable format specification that can be referenced by
// label in I/O statements such as [ReadStmt], [WriteStmt], and [PrintStmt].
// The format specification controls how data is formatted during I/O operations.
//
// Example:
//
//	<label> FORMAT(<format-spec>)
//	100 FORMAT(I5)
//	200 FORMAT(I5, F10.2, A)
//	300 FORMAT('Result = ', F8.3)
type FormatStmt struct {
	Spec  string // Format specification (stored as string)
	Label string // Statement label (always present for FORMAT)
	Position
}

var _ Statement = (*FormatStmt)(nil)

func (fs *FormatStmt) GetLabel() string { return fs.Label }
func (fs *FormatStmt) statementNode()   {}
func (fs *FormatStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "FORMAT"...)
}
func (fs *FormatStmt) AppendString(dst []byte) []byte {
	if fs.Label != "" {
		dst = append(dst, fs.Label...)
		dst = append(dst, ' ')
	}
	dst = append(dst, "FORMAT("...)
	dst = append(dst, fs.Spec...)
	dst = append(dst, ')')
	return dst
}

// AllocateStmt dynamically allocates memory for allocatable arrays and pointer
// targets at runtime. The allocated memory persists until explicitly deallocated
// with [DeallocateStmt] or until the program terminates.
//
// Example:
//
//	ALLOCATE(<object-list> [, STAT=<var>] [, ERRMSG=<var>] [, SOURCE=<expr>])
//	ALLOCATE(A(10))
//	ALLOCATE(A(10,20), B(100), STAT=ierr)
//	ALLOCATE(matrix(n,m), STAT=istat, ERRMSG=emsg)
type AllocateStmt struct {
	Objects []Expression          // List of objects to allocate
	Options map[string]Expression // Optional specifiers: STAT, ERRMSG, SOURCE, MOLD
	Label   string                // Optional statement label
	Position
}

var _ Statement = (*AllocateStmt)(nil)

func (as *AllocateStmt) GetLabel() string { return as.Label }
func (as *AllocateStmt) statementNode()   {}
func (as *AllocateStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "ALLOCATE"...)
}
func (as *AllocateStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "ALLOCATE("...)
	for i, obj := range as.Objects {
		if i > 0 {
			dst = append(dst, ", "...)
		}
		dst = obj.AppendString(dst)
	}
	for key, value := range as.Options {
		dst = append(dst, ", "...)
		dst = append(dst, key...)
		dst = append(dst, '=')
		dst = value.AppendString(dst)
	}
	dst = append(dst, ')')
	return dst
}

// DeallocateStmt releases memory that was previously allocated by [AllocateStmt],
// making it available for other uses. After deallocation, the objects become
// undefined and must be reallocated before use.
//
// Example:
//
//	DEALLOCATE(<object-list> [, STAT=<var>] [, ERRMSG=<var>])
//	DEALLOCATE(A)
//	DEALLOCATE(A, B, C, STAT=ierr)
//	DEALLOCATE(matrix, STAT=istat, ERRMSG=emsg)
type DeallocateStmt struct {
	Objects []Expression          // List of objects to deallocate
	Options map[string]Expression // Optional specifiers: STAT, ERRMSG
	Label   string                // Optional statement label
	Position
}

var _ Statement = (*DeallocateStmt)(nil)

func (ds *DeallocateStmt) GetLabel() string { return ds.Label }
func (ds *DeallocateStmt) statementNode()   {}
func (ds *DeallocateStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "DEALLOCATE"...)
}
func (ds *DeallocateStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "DEALLOCATE("...)
	for i, obj := range ds.Objects {
		if i > 0 {
			dst = append(dst, ", "...)
		}
		dst = obj.AppendString(dst)
	}
	for key, value := range ds.Options {
		dst = append(dst, ", "...)
		dst = append(dst, key...)
		dst = append(dst, '=')
		dst = value.AppendString(dst)
	}
	dst = append(dst, ')')
	return dst
}

// Derived Type Statements (Phase 7)

// DerivedTypeStmt defines a user-defined composite data type that groups related
// data of different types together. Similar to structs in C or records in Pascal,
// derived types enable object-oriented-like data structures in Fortran.
//
// Example:
//
//	TYPE <type-name>
//	  <component-declarations>
//	END TYPE [<type-name>]
//
//	TYPE :: Person
//	  CHARACTER(LEN=50) :: name
//	  INTEGER :: age
//	  REAL :: height
//	END TYPE Person
type DerivedTypeStmt struct {
	Name       string
	Components []ComponentDecl
	Label      string
	Attributes []TypeAttribute
	Position
}

var _ Statement = (*DerivedTypeStmt)(nil) // compile time check of interface implementation.

func (dts *DerivedTypeStmt) GetLabel() string { return dts.Label }

func (dts *DerivedTypeStmt) statementNode() {}
func (dts *DerivedTypeStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "TYPE"...)
}
func (dts *DerivedTypeStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "TYPE :: "...)
	dst = append(dst, dts.Name...)
	return dst
}

// ComponentDecl represents a single component (field) declaration within a
// [DerivedTypeStmt]. Each component has a type and may have attributes like
// POINTER, ALLOCATABLE, or DIMENSION.
//
// Example:
//
//	<type> [, <attributes>] :: <component-list>
//	INTEGER :: count
//	REAL, DIMENSION(3) :: velocity
//	TYPE(Date), POINTER :: birth_date
type ComponentDecl struct {
	Type       TypeSpec // Type with optional KIND/LEN
	Attributes []token.Token
	Components []DeclEntity
	Label      string
	Position
}

var _ Statement = (*ComponentDecl)(nil) // compile time check of interface implementation.

func (cd *ComponentDecl) GetLabel() string { return cd.Label }

func (cd *ComponentDecl) statementNode() {}
func (cd *ComponentDecl) AppendTokenLiteral(dst []byte) []byte {
	return cd.Type.AppendString(dst)
}
func (cd *ComponentDecl) AppendString(dst []byte) []byte {
	dst = cd.Type.AppendString(dst)
	if len(cd.Attributes) > 0 {
		dst = append(dst, ", "...)
		for i, attr := range cd.Attributes {
			if i > 0 {
				dst = append(dst, ", "...)
			}
			dst = append(dst, attr.String()...)
		}
	}
	dst = append(dst, " :: "...)
	for i, entity := range cd.Components {
		if i > 0 {
			dst = append(dst, ", "...)
		}
		dst = append(dst, entity.Name...)
	}
	return dst
}

// Interface Blocks (Phase 7)

// InterfaceStmt defines a generic interface that can bind multiple specific
// procedures to a single generic name, enabling function overloading and
// operator customization in Fortran.
//
// Example:
//
//	INTERFACE [<generic-name> | OPERATOR(<op>) | ASSIGNMENT(=)]
//	  <interface-body>
//	END INTERFACE [<generic-name>]
//
//	INTERFACE sort
//	  MODULE PROCEDURE sort_int, sort_real
//	END INTERFACE sort
type InterfaceStmt struct {
	Name  string
	Body  []Statement
	Label string
	Position
}

var _ Statement = (*InterfaceStmt)(nil) // compile time check of interface implementation.

func (is *InterfaceStmt) GetLabel() string { return is.Label }

func (is *InterfaceStmt) statementNode() {}
func (is *InterfaceStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "INTERFACE"...)
}
func (is *InterfaceStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "INTERFACE"...)
	if is.Name != "" {
		dst = append(dst, " "...)
		dst = append(dst, is.Name...)
	}
	return dst
}

// Accessibility Statements (Phase 7)

// PrivateStmt restricts access to module entities, making them invisible outside
// the module. Without a list, it sets the default accessibility to private for
// all entities. With a list, only specified entities become private.
//
// Example:
//
//	PRIVATE
//	PRIVATE :: <entity-list>
//	PRIVATE :: internal_var, helper_func
type PrivateStmt struct {
	Entities []string
	Label    string
	Position
}

var _ Statement = (*PrivateStmt)(nil) // compile time check of interface implementation.

func (ps *PrivateStmt) GetLabel() string { return ps.Label }

func (ps *PrivateStmt) statementNode() {}
func (ps *PrivateStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "PRIVATE"...)
}
func (ps *PrivateStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "PRIVATE"...)
	if len(ps.Entities) > 0 {
		dst = append(dst, " :: "...)
		for i, entity := range ps.Entities {
			if i > 0 {
				dst = append(dst, ", "...)
			}
			dst = append(dst, entity...)
		}
	}
	return dst
}

// PublicStmt declares module entities as publicly accessible from outside the
// module. Without a list, it sets the default accessibility to public. With a
// list, only specified entities become public.
//
// Example:
//
//	PUBLIC
//	PUBLIC :: <entity-list>
//	PUBLIC :: api_function, OPERATOR(+)
type PublicStmt struct {
	Entities []string
	Label    string
	Position
}

var _ Statement = (*PublicStmt)(nil) // compile time check of interface implementation.

func (ps *PublicStmt) GetLabel() string { return ps.Label }

func (ps *PublicStmt) statementNode() {}
func (ps *PublicStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "PUBLIC"...)
}
func (ps *PublicStmt) AppendString(dst []byte) []byte {
	dst = append(dst, "PUBLIC"...)
	if len(ps.Entities) > 0 {
		dst = append(dst, " :: "...)
		for i, entity := range ps.Entities {
			if i > 0 {
				dst = append(dst, ", "...)
			}
			dst = append(dst, entity...)
		}
	}
	return dst
}

// Advanced Expressions (Phase 7)

// ArraySection represents a contiguous or strided subset of an array, specified
// using range notation. Array sections can be used in expressions, assignments,
// and as arguments to procedures.
//
// Example:
//
//	<array-name>(<subscript-list>)
//	arr(1:5)
//	matrix(:, 2)
//	data(1:10:2)
//	cube(i, :, 1:n)
type ArraySection struct {
	Name       string
	Subscripts []Subscript
	Position
}

var _ Expression = (*ArraySection)(nil) // compile time check of interface implementation.

func (as *ArraySection) expressionNode() {}
func (as *ArraySection) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "ARRAY_SECTION"...)
}
func (as *ArraySection) AppendString(dst []byte) []byte {
	dst = append(dst, as.Name...)
	dst = append(dst, '(')
	for i, sub := range as.Subscripts {
		if i > 0 {
			dst = append(dst, ", "...)
		}
		if sub.Lower != nil {
			dst = sub.Lower.AppendString(dst)
		}
		dst = append(dst, ':')
		if sub.Upper != nil {
			dst = sub.Upper.AppendString(dst)
		}
		if sub.Stride != nil {
			dst = append(dst, ':')
			dst = sub.Stride.AppendString(dst)
		}
	}
	dst = append(dst, ')')
	return dst
}

// Subscript represents a subscript in an array section or array reference
type Subscript struct {
	Lower  Expression
	Upper  Expression
	Stride Expression
}

// ArrayConstructor creates array values inline using literal notation. The F90
// syntax uses (/ ... /), while F2003 introduced the bracket syntax [ ... ].
//
// Example:
//
//	(/ <value-list> /)
//	(/ 1, 2, 3, 4, 5 /)
//	(/ (i, i=1,10) /)
//	[1.0, 2.0, 3.0]
type ArrayConstructor struct {
	Values []Expression
	Position
}

var _ Expression = (*ArrayConstructor)(nil) // compile time check of interface implementation.

func (ac *ArrayConstructor) expressionNode() {}
func (ac *ArrayConstructor) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "(/"...)
}
func (ac *ArrayConstructor) AppendString(dst []byte) []byte {
	dst = append(dst, "(/"...)
	for i, val := range ac.Values {
		if i > 0 {
			dst = append(dst, ", "...)
		}
		dst = val.AppendString(dst)
	}
	dst = append(dst, "/)"...)
	return dst
}

// ComponentAccess accesses a component (field) of a derived type variable using
// the percent operator. Multiple levels of access can be chained for nested types.
//
// Example:
//
//	<base>%<component>
//	person%name
//	car%engine%horsepower
//	point%x
type ComponentAccess struct {
	Base      Expression
	Component string
	Position
}

var _ Expression = (*ArrayConstructor)(nil) // compile time check of interface implementation.

func (ca *ComponentAccess) expressionNode() {}
func (ca *ComponentAccess) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "%"...)
}
func (ca *ComponentAccess) AppendString(dst []byte) []byte {
	dst = ca.Base.AppendString(dst)
	dst = append(dst, '%')
	dst = append(dst, ca.Component...)
	return dst
}

// type SpecifierKey int

// const (
// 	SpecUndefined SpecifierKey = iota
// 	SpecUNIT
// 	SpecIOSTAT
// 	SpecERR
// )

// type speckeyval struct {
// 	key   SpecifierKey
// 	value Expression
// }

// type Specifiers struct {
// 	kv []speckeyval
// }

// func (specs *Specifiers) Add(key SpecifierKey, value Expression) {
// 	specs.kv = append(specs.kv, speckeyval{
// 		key: key, value: value,
// 	})
// }

// func (specs *Specifiers) Get(key SpecifierKey) Expression {
// 	for _, kv := range specs.kv {
// 		if kv.key == key {
// 			return kv.value
// 		}
// 	}
// 	return nil
// }
