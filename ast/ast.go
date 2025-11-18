package ast

import (
	"github.com/soypat/go-fortran/token"
)

type Node interface {
	AppendTokenLiteral(dst []byte) []byte
	AppendString(dst []byte) []byte
	Pos() int // position of first character belonging to the node in file.
	End() int // position of first character immediately after the node in file.
}

type Expression interface {
	Node
	expressionNode()
}

type Statement interface {
	Node
	statementNode()
}

// ProgramUnit represents a top-level construct (PROGRAM, SUBROUTINE, FUNCTION, MODULE)
type ProgramUnit interface {
	Statement
	programUnitNode()
}

// Program represents the root node of a Fortran program file
type Program struct {
	Units []ProgramUnit
}

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

func (p *Program) Pos() int {
	if len(p.Units) == 0 {
		return 0
	}
	return p.Units[0].Pos()
}

func (p *Program) End() int {
	if len(p.Units) == 0 {
		return 0
	}
	return p.Units[len(p.Units)-1].End()
}

// ProgramBlock represents a PROGRAM...END PROGRAM block
type ProgramBlock struct {
	Name     string
	Body     []Statement // Specification and executable statements
	StartPos int
	EndPos   int
}

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

func (pb *ProgramBlock) Pos() int { return pb.StartPos }
func (pb *ProgramBlock) End() int { return pb.EndPos }

// Subroutine represents a SUBROUTINE...END SUBROUTINE block
type Subroutine struct {
	Name       string
	Parameters []Parameter   // Function/subroutine parameters with type information
	Attributes []token.Token // RECURSIVE, PURE, etc.
	Body       []Statement   // Specification and executable statements
	StartPos   int
	EndPos     int
}

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

func (s *Subroutine) Pos() int { return s.StartPos }
func (s *Subroutine) End() int { return s.EndPos }

// Function represents a FUNCTION...END FUNCTION block
type Function struct {
	Name           string
	ResultType     string        // e.g., "INTEGER", "REAL"
	Parameters     []Parameter   // Function parameters with type information
	ResultVariable string        // For RESULT(var) clause
	Attributes     []token.Token // RECURSIVE, PURE, ELEMENTAL
	Body           []Statement   // Specification and executable statements
	StartPos       int
	EndPos         int
}

func (f *Function) statementNode()   {}
func (f *Function) programUnitNode() {}
func (f *Function) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "FUNCTION"...)
}
func (f *Function) AppendString(dst []byte) []byte {
	if f.ResultType != "" {
		dst = append(dst, f.ResultType...)
		dst = append(dst, ' ')
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

func (f *Function) Pos() int { return f.StartPos }
func (f *Function) End() int { return f.EndPos }

// Module represents a MODULE...END MODULE block
type Module struct {
	Name     string
	Body     []Statement   // Module-level declarations
	Contains []ProgramUnit // Procedures in CONTAINS section
	StartPos int
	EndPos   int
}

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

func (m *Module) Pos() int { return m.StartPos }
func (m *Module) End() int { return m.EndPos }

// BlockData represents a BLOCK DATA...END BLOCK DATA block (Fortran 77 feature)
type BlockData struct {
	Name     string
	Body     []Statement
	StartPos int
	EndPos   int
}

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

func (bd *BlockData) Pos() int { return bd.StartPos }
func (bd *BlockData) End() int { return bd.EndPos }

// TokenTuple represents a stored token for deferred parsing
type TokenTuple struct {
	Tok   token.Token
	Start int
	Lit   []byte
}

// Specification Part Statements (Phase 2)

// ImplicitStatement represents an IMPLICIT statement
type ImplicitStatement struct {
	IsNone   bool // true for IMPLICIT NONE
	StartPos int
	EndPos   int
}

func (is *ImplicitStatement) statementNode() {}
func (is *ImplicitStatement) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "IMPLICIT"...)
}
func (is *ImplicitStatement) AppendString(dst []byte) []byte {
	if is.IsNone {
		return append(dst, "IMPLICIT NONE"...)
	}
	return append(dst, "IMPLICIT"...)
}
func (is *ImplicitStatement) Pos() int { return is.StartPos }
func (is *ImplicitStatement) End() int { return is.EndPos }

// UseStatement represents a USE statement
type UseStatement struct {
	ModuleName string
	Only       []string // Empty if not using ONLY clause
	StartPos   int
	EndPos     int
}

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
func (us *UseStatement) Pos() int { return us.StartPos }
func (us *UseStatement) End() int { return us.EndPos }

// TypeDeclaration represents a type declaration with attributes
type TypeDeclaration struct {
	TypeSpec   string        // e.g., "INTEGER", "REAL", "CHARACTER"
	Attributes []token.Token // e.g., PARAMETER, SAVE, INTENT, etc.
	Entities   []DeclEntity  // Variables being declared
	StartPos   int
	EndPos     int
}

func (td *TypeDeclaration) statementNode() {}
func (td *TypeDeclaration) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, td.TypeSpec...)
}
func (td *TypeDeclaration) AppendString(dst []byte) []byte {
	dst = append(dst, td.TypeSpec...)
	if len(td.Attributes) > 0 {
		dst = append(dst, ", "...)
		for i, attr := range td.Attributes {
			if i > 0 {
				dst = append(dst, ", "...)
			}
			dst = append(dst, attr.String()...)
		}
	}
	dst = append(dst, " :: "...)
	for i, entity := range td.Entities {
		if i > 0 {
			dst = append(dst, ", "...)
		}
		dst = append(dst, entity.Name...)
	}
	return dst
}
func (td *TypeDeclaration) Pos() int { return td.StartPos }
func (td *TypeDeclaration) End() int { return td.EndPos }

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

// ArraySpec represents array dimension specification
type ArraySpec struct {
	Kind   ArraySpecKind
	Bounds []ArrayBound // One bound per dimension
}

// DeclEntity represents a single entity in a type declaration
type DeclEntity struct {
	Name        string
	ArraySpec   *ArraySpec // Array dimensions if this is an array
	Initializer string     // Initialization expression (will become Expression in Phase 4)
	CharLen     string     // CHARACTER length specification (will become Expression in Phase 4)
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

// Parameter represents a subroutine or function parameter with full type information
type Parameter struct {
	Name       string        // Parameter name
	Type       string        // Type specification (INTEGER, REAL, etc.)
	Intent     IntentType    // INTENT(IN/OUT/INOUT)
	Attributes []token.Token // Other attributes (OPTIONAL, POINTER, TARGET, etc.)
	ArraySpec  *ArraySpec    // Array dimensions if this is an array parameter
	CharLen    string        // CHARACTER length specification (will become Expression in Phase 4)
}

// Identifier represents an identifier
type Identifier struct {
	Value    string
	StartPos int
	EndPos   int
}

func (i *Identifier) expressionNode() {}
func (i *Identifier) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, i.Value...)
}
func (i *Identifier) AppendString(dst []byte) []byte {
	return append(dst, i.Value...)
}

func (i *Identifier) Pos() int { return i.StartPos }
func (i *Identifier) End() int { return i.EndPos }

// IntegerLiteral represents an integer literal
type IntegerLiteral struct {
	Value    int64  // Parsed integer value (0 if not yet parsed)
	Raw      string // Original text representation
	StartPos int
	EndPos   int
}

func (il *IntegerLiteral) expressionNode() {}
func (il *IntegerLiteral) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "INTEGER"...)
}
func (il *IntegerLiteral) AppendString(dst []byte) []byte {
	// Convert int64 to string
	return append(dst, []byte(string(rune(il.Value)))...)
}

func (il *IntegerLiteral) Pos() int { return il.StartPos }
func (il *IntegerLiteral) End() int { return il.EndPos }

// RealLiteral represents a floating-point literal
type RealLiteral struct {
	Value    float64
	Raw      string // Original text representation
	StartPos int
	EndPos   int
}

func (rl *RealLiteral) expressionNode() {}
func (rl *RealLiteral) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "REAL"...)
}
func (rl *RealLiteral) AppendString(dst []byte) []byte {
	return append(dst, rl.Raw...)
}
func (rl *RealLiteral) Pos() int { return rl.StartPos }
func (rl *RealLiteral) End() int { return rl.EndPos }

// StringLiteral represents a string literal
type StringLiteral struct {
	Value    string
	StartPos int
	EndPos   int
}

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
func (sl *StringLiteral) Pos() int { return sl.StartPos }
func (sl *StringLiteral) End() int { return sl.EndPos }

// LogicalLiteral represents .TRUE. or .FALSE.
type LogicalLiteral struct {
	Value    bool
	StartPos int
	EndPos   int
}

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
func (ll *LogicalLiteral) Pos() int { return ll.StartPos }
func (ll *LogicalLiteral) End() int { return ll.EndPos }

// BinaryExpr represents a binary operation (e.g., a + b, x .GT. y)
type BinaryExpr struct {
	Op       token.Token // Operator token
	Left     Expression
	Right    Expression
	StartPos int
	EndPos   int
}

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
func (be *BinaryExpr) Pos() int { return be.StartPos }
func (be *BinaryExpr) End() int { return be.EndPos }

// UnaryExpr represents a unary operation (e.g., -x, +y, .NOT. flag)
type UnaryExpr struct {
	Op      token.Token // Operator token
	Operand Expression
	StartPos int
	EndPos   int
}

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
func (ue *UnaryExpr) Pos() int { return ue.StartPos }
func (ue *UnaryExpr) End() int { return ue.EndPos }

// FunctionCall represents a function call expression (e.g., sqrt(x), max(a, b, c))
type FunctionCall struct {
	Name     string
	Args     []Expression
	StartPos int
	EndPos   int
}

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
func (fc *FunctionCall) Pos() int { return fc.StartPos }
func (fc *FunctionCall) End() int { return fc.EndPos }

// ArrayRef represents an array reference (e.g., arr(i), matrix(i,j))
type ArrayRef struct {
	Name       string
	Subscripts []Expression
	StartPos   int
	EndPos     int
}

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
func (ar *ArrayRef) Pos() int { return ar.StartPos }
func (ar *ArrayRef) End() int { return ar.EndPos }

// ParenExpr represents a parenthesized expression
type ParenExpr struct {
	Expr     Expression
	StartPos int
	EndPos   int
}

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
func (pe *ParenExpr) Pos() int { return pe.StartPos }
func (pe *ParenExpr) End() int { return pe.EndPos }
