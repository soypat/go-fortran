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

// Program represents the root node of a Fortran program file
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

// ProgramBlock represents a PROGRAM...END PROGRAM block
type ProgramBlock struct {
	Name  string
	Body  []Statement // Specification and executable statements
	Label string
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

// Subroutine represents a SUBROUTINE...END SUBROUTINE block
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

// Function represents a FUNCTION...END FUNCTION block
type Function struct {
	Name           string
	ResultType     string        // e.g., "INTEGER", "REAL"
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

// Module represents a MODULE...END MODULE block
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

// BlockData represents a BLOCK DATA...END BLOCK DATA block (Fortran 77 feature)
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

// ImplicitStatement represents an IMPLICIT statement
type ImplicitStatement struct {
	IsNone bool // true for IMPLICIT NONE
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
	return append(dst, "IMPLICIT"...)
}

// UseStatement represents a USE statement
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

// TypeDeclaration represents a type declaration with attributes
type TypeDeclaration struct {
	TypeSpec   string        // e.g., "INTEGER", "REAL", "CHARACTER"
	Attributes []token.Token // e.g., PARAMETER, SAVE, INTENT, etc.
	Entities   []DeclEntity  // Variables being declared
	Label      string
	Position
}

var _ Statement = (*TypeDeclaration)(nil) // compile time check of interface implementation.

func (td *TypeDeclaration) GetLabel() string { return td.Label }

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

// RangeExpr represents array slice/range syntax in Fortran
// Examples: :, start:end, :end, start:, start:end:stride
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

// IntegerLiteral represents an integer literal
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
	return append(dst, []byte(string(rune(il.Value)))...)
}

// RealLiteral represents a floating-point literal
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

// StringLiteral represents a string literal
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

// LogicalLiteral represents .TRUE. or .FALSE.
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

// BinaryExpr represents a binary operation (e.g., a + b, x .GT. y)
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

// UnaryExpr represents a unary prefix operation (e.g., -x, +y, .NOT. flag)
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

// FunctionCall represents a function call expression (e.g., sqrt(x), max(a, b, c))
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

// ArrayRef represents an array reference (e.g., arr(i), matrix(i,j))
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

// ParenExpr represents a parenthesized expression
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

// ImpliedDoLoop represents an implied DO loop in I/O statements
// Syntax: ( expression-list, loop-var = start, end [, stride] )
// Example: (COVSCR(M,1), M=1, 6)
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

// AssignmentStmt represents an assignment statement (e.g., x = y + 1)
type AssignmentStmt struct {
	Target Expression
	Value  Expression
	Label  string
	Position
}

var _ Statement = (*AssignmentStmt)(nil) // compile time check of interface implementation.

func (as *AssignmentStmt) GetLabel() string { return as.Label }

func (as *AssignmentStmt) statementNode() {}
func (as *AssignmentStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "="...)
}
func (as *AssignmentStmt) AppendString(dst []byte) []byte {
	dst = as.Target.AppendString(dst)
	dst = append(dst, " = "...)
	dst = as.Value.AppendString(dst)
	return dst
}

// IfStmt represents an IF...THEN...ELSE IF...ELSE...END IF construct
type IfStmt struct {
	Condition   Expression
	ThenPart    []Statement
	ElseIfParts []ElseIfClause
	ElsePart    []Statement
	Label       string
	EndLabel    string // Label on END IF statement (F77)
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

// ArithmeticIfStmt represents the F77 arithmetic IF statement
// IF (expr) label_negative, label_zero, label_positive
// Branches based on whether the expression is < 0, == 0, or > 0
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

// DoLoop represents a DO loop
type DoLoop struct {
	Var         string // Loop variable (empty for DO WHILE)
	Start       Expression
	End         Expression
	Step        Expression
	Body        []Statement
	Label       string // Statement label (line prefix, e.g., "10" in "10 DO...")
	TargetLabel string // DO target label (F77, e.g., "20" in "DO 20 I=1,10")
	EndLabel    string // Label on END DO statement (F77, e.g., "20" in "20 END DO")
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

// SelectCaseStmt represents a SELECT CASE construct
type SelectCaseStmt struct {
	Expression Expression   // The expression being selected on
	Cases      []CaseClause // List of CASE clauses
	Label      string       // Statement label
	EndLabel   string       // Label on END SELECT
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

// CallStmt represents a CALL statement
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

// ReturnStmt represents a RETURN statement
type ReturnStmt struct {
	Label string
	Position
}

var _ Statement = (*ReturnStmt)(nil) // compile time check of interface implementation.

func (rs *ReturnStmt) GetLabel() string { return rs.Label }

func (rs *ReturnStmt) statementNode() {}
func (rs *ReturnStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "RETURN"...)
}
func (rs *ReturnStmt) AppendString(dst []byte) []byte {
	return append(dst, "RETURN"...)
}

// CycleStmt represents a CYCLE statement
type CycleStmt struct {
	Label string
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

// ExitStmt represents an EXIT statement
type ExitStmt struct {
	Label string
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

// ContinueStmt represents a CONTINUE statement
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

// GotoStmt represents a GOTO or GO TO statement
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

// ComputedGotoStmt represents a computed/indexed GOTO statement (F77)
// GOTO (label1, label2, ..., labeln) expression
// Jumps to one of the labels based on the value of the expression (1-indexed)
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

// InquireStmt represents an INQUIRE statement
type InquireStmt struct {
	Specifiers map[string]Expression // INQUIRE specifiers: UNIT, FILE, EXIST, OPENED, etc.
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

// OpenStmt represents an OPEN statement
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

// CloseStmt represents a CLOSE statement
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

// WriteStmt represents a WRITE statement
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

// ReadStmt represents a READ statement
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

// PrintStmt represents a PRINT statement
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

// BackspaceStmt represents a BACKSPACE statement
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

// RewindStmt represents a REWIND statement
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

// StopStmt represents a STOP statement
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

// FormatStmt represents a FORMAT statement
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

// AllocateStmt represents an ALLOCATE statement
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

// DeallocateStmt represents a DEALLOCATE statement
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

// DerivedTypeStmt represents a TYPE...END TYPE block
type DerivedTypeStmt struct {
	Name       string
	Components []ComponentDecl
	Label      string
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

// ComponentDecl represents a component in a derived type
type ComponentDecl struct {
	Type       string
	Attributes []token.Token
	Components []DeclEntity
	Label      string
	Position
}

var _ Statement = (*ComponentDecl)(nil) // compile time check of interface implementation.

func (cd *ComponentDecl) GetLabel() string { return cd.Label }

func (cd *ComponentDecl) statementNode() {}
func (cd *ComponentDecl) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, cd.Type...)
}
func (cd *ComponentDecl) AppendString(dst []byte) []byte {
	dst = append(dst, cd.Type...)
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

// InterfaceStmt represents an INTERFACE...END INTERFACE block
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

// PrivateStmt represents a PRIVATE statement
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

// PublicStmt represents a PUBLIC statement
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

// ArraySection represents an array section (e.g., arr(1:5), arr(:, 1:10:2))
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

// ArrayConstructor represents an array constructor (e.g., (/ 1, 2, 3 /))
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

// ComponentAccess represents a derived type component access (e.g., person%name)
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

// PointerAssignmentStmt represents a pointer assignment statement (e.g., ptr => target)
type PointerAssignmentStmt struct {
	Target Expression
	Value  Expression
	Label  string
	Position
}

var _ Statement = (*PointerAssignmentStmt)(nil) // compile time check of interface implementation.

func (pa *PointerAssignmentStmt) GetLabel() string { return pa.Label }

func (pa *PointerAssignmentStmt) statementNode() {}
func (pa *PointerAssignmentStmt) AppendTokenLiteral(dst []byte) []byte {
	return append(dst, "=>"...)
}
func (pa *PointerAssignmentStmt) AppendString(dst []byte) []byte {
	dst = pa.Target.AppendString(dst)
	dst = append(dst, " => "...)
	dst = pa.Value.AppendString(dst)
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
