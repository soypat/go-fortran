package ast

import "github.com/soypat/go-fortran/token"

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
	Name       string
	BodyTokens []TokenTuple // Unparsed body for phase 2
	StartPos   int
	EndPos     int
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
	Parameters []string
	Attributes []token.Token // RECURSIVE, PURE, etc.
	BodyTokens []TokenTuple
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
		dst = append(dst, p...)
	}
	dst = append(dst, ')')
	return dst
}

func (s *Subroutine) Pos() int { return s.StartPos }
func (s *Subroutine) End() int { return s.EndPos }

// Function represents a FUNCTION...END FUNCTION block
type Function struct {
	Name           string
	ResultType     string // e.g., "INTEGER", "REAL"
	Parameters     []string
	ResultVariable string        // For RESULT(var) clause
	Attributes     []token.Token // RECURSIVE, PURE, ELEMENTAL
	BodyTokens     []TokenTuple
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
		dst = append(dst, p...)
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
	Name       string
	BodyTokens []TokenTuple  // Module-level declarations
	Contains   []ProgramUnit // Procedures in CONTAINS section
	StartPos   int
	EndPos     int
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
	Name       string
	BodyTokens []TokenTuple
	StartPos   int
	EndPos     int
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
	Value    int64
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
