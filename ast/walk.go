package ast

// A Visitor's Visit method is invoked for each node encountered by Walk.
// If the result visitor w is not nil, Walk visits each of the children
// of node with the visitor w, followed by a call of w.Visit(nil).
type Visitor interface {
	Visit(node Node) (w Visitor)
}

// Walk traverses an AST in depth-first order: It starts by calling
// v.Visit(node); node must not be nil. If the visitor w returned by
// v.Visit(node) is not nil, Walk is invoked recursively with visitor
// w for each of the non-nil children of node, followed by a call of
// w.Visit(nil).
func Walk(v Visitor, node Node) {
	if v = v.Visit(node); v == nil {
		return
	}

	// Walk children based on node type
	switch n := node.(type) {
	// Root node
	case *Program:
		for _, unit := range n.Units {
			Walk(v, unit)
		}

	// Program units
	case *ProgramBlock:
		for _, stmt := range n.Body {
			Walk(v, stmt)
		}

	case *Subroutine:
		for _, stmt := range n.Body {
			Walk(v, stmt)
		}

	case *Function:
		if n.Type.KindOrLen != nil {
			Walk(v, n.Type.KindOrLen)
		}
		for _, stmt := range n.Body {
			Walk(v, stmt)
		}

	case *Module:
		for _, stmt := range n.Body {
			Walk(v, stmt)
		}
		for _, proc := range n.Contains {
			Walk(v, proc)
		}

	case *BlockData:
		for _, stmt := range n.Body {
			Walk(v, stmt)
		}

	// Declaration statements
	case *TypeDeclaration:
		if n.Type.KindOrLen != nil {
			Walk(v, n.Type.KindOrLen)
		}
		for _, entity := range n.Entities {
			if entity.ArraySpec != nil {
				for _, bound := range entity.ArraySpec.Bounds {
					if bound.Lower != nil {
						Walk(v, bound.Lower)
					}
					if bound.Upper != nil {
						Walk(v, bound.Upper)
					}
				}
			}
			if entity.CharLen != nil {
				Walk(v, entity.CharLen)
			}
			// Note: entity.Initializer is a string, not an Expression
		}

	case *ImplicitStatement:
		for _, rule := range n.Rules {
			if rule.Type.KindOrLen != nil {
				Walk(v, rule.Type.KindOrLen)
			}
		}

	case *UseStatement:
		// UseStatement has no child nodes

	case *CommonStmt:
		// CommonStmt.Variables is []string, no child nodes to walk

	case *ExternalStmt:
		// ExternalStmt has no child nodes

	case *IntrinsicStmt:
		// IntrinsicStmt has no child nodes

	case *DataStmt:
		// DataStmt is currently empty, no child nodes

	case *DerivedTypeStmt:
		for i := range n.Components {
			Walk(v, &n.Components[i])
		}

	case *ComponentDecl:
		// Walk DeclEntity components
		for _, entity := range n.Components {
			if entity.ArraySpec != nil {
				for _, bound := range entity.ArraySpec.Bounds {
					if bound.Lower != nil {
						Walk(v, bound.Lower)
					}
					if bound.Upper != nil {
						Walk(v, bound.Upper)
					}
				}
			}
			if entity.CharLen != nil {
				Walk(v, entity.CharLen)
			}
		}

	case *InterfaceStmt:
		for _, stmt := range n.Body {
			Walk(v, stmt)
		}

	case *PrivateStmt:
		// PrivateStmt has no child nodes

	case *PublicStmt:
		// PublicStmt has no child nodes

	// Executable statements
	case *AssignmentStmt:
		Walk(v, n.Target)
		Walk(v, n.Value)

	case *IfStmt:
		Walk(v, n.Condition)
		for _, stmt := range n.ThenPart {
			Walk(v, stmt)
		}
		for _, elif := range n.ElseIfParts {
			Walk(v, elif.Condition)
			for _, stmt := range elif.ThenPart {
				Walk(v, stmt)
			}
		}
		for _, stmt := range n.ElsePart {
			Walk(v, stmt)
		}

	case *ArithmeticIfStmt:
		Walk(v, n.Condition)

	case *DoLoop:
		// n.Var is a string, not a Node
		if n.Start != nil {
			Walk(v, n.Start)
		}
		if n.End != nil {
			Walk(v, n.End)
		}
		if n.Step != nil {
			Walk(v, n.Step)
		}
		for _, stmt := range n.Body {
			Walk(v, stmt)
		}

	case *SelectCaseStmt:
		Walk(v, n.Expression)
		for _, clause := range n.Cases {
			for _, value := range clause.Values {
				Walk(v, value)
			}
			for _, stmt := range clause.Body {
				Walk(v, stmt)
			}
		}

	case *CallStmt:
		for _, arg := range n.Args {
			Walk(v, arg)
		}

	case *EntryStmt:
		// EntryStmt has no child nodes that need walking

	case *ReturnStmt:
		if n.AlternateReturn != nil {
			Walk(v, n.AlternateReturn)
		}

	case *CycleStmt:
		// CycleStmt has no child nodes

	case *ExitStmt:
		// ExitStmt has no child nodes

	case *ContinueStmt:
		// ContinueStmt has no child nodes

	case *GotoStmt:
		// GotoStmt has no child nodes

	case *AssignStmt:
		// AssignStmt has no child nodes

	case *ComputedGotoStmt:
		if n.Expression != nil {
			Walk(v, n.Expression)
		}

	case *AssignedGotoStmt:
		// n.Variable is a string, not a Node

	// I/O statements
	case *InquireStmt:
		for _, spec := range n.Specifiers {
			Walk(v, spec)
		}
		for _, item := range n.OutputList {
			Walk(v, item)
		}

	case *OpenStmt:
		for _, spec := range n.Specifiers {
			Walk(v, spec)
		}

	case *CloseStmt:
		for _, spec := range n.Specifiers {
			Walk(v, spec)
		}

	case *WriteStmt:
		if n.Unit != nil {
			Walk(v, n.Unit)
		}
		if n.Format != nil {
			Walk(v, n.Format)
		}
		for _, spec := range n.Specifiers {
			Walk(v, spec)
		}
		for _, item := range n.OutputList {
			Walk(v, item)
		}

	case *ReadStmt:
		if n.Unit != nil {
			Walk(v, n.Unit)
		}
		if n.Format != nil {
			Walk(v, n.Format)
		}
		for _, spec := range n.Specifiers {
			Walk(v, spec)
		}
		for _, item := range n.InputList {
			Walk(v, item)
		}

	case *PrintStmt:
		if n.Format != nil {
			Walk(v, n.Format)
		}
		for _, item := range n.OutputList {
			Walk(v, item)
		}

	case *BackspaceStmt:
		for _, spec := range n.Specifiers {
			Walk(v, spec)
		}

	case *RewindStmt:
		for _, spec := range n.Specifiers {
			Walk(v, spec)
		}

	case *EndfileStmt:
		for _, spec := range n.Specifiers {
			Walk(v, spec)
		}

	case *StopStmt:
		if n.Code != nil {
			Walk(v, n.Code)
		}

	case *FormatStmt:
		// FormatStmt has string descriptor, no child nodes to walk

	// Memory management
	case *AllocateStmt:
		for _, obj := range n.Objects {
			Walk(v, obj)
		}
		for _, opt := range n.Options {
			Walk(v, opt)
		}

	case *DeallocateStmt:
		for _, obj := range n.Objects {
			Walk(v, obj)
		}
		for _, opt := range n.Options {
			Walk(v, opt)
		}

	// Expressions
	case *Identifier:
		// Identifier is a leaf node

	case *IntegerLiteral:
		// IntegerLiteral is a leaf node

	case *RealLiteral:
		// RealLiteral is a leaf node

	case *StringLiteral:
		// StringLiteral is a leaf node

	case *LogicalLiteral:
		// LogicalLiteral is a leaf node

	case *AlternateReturnArg:
		// AlternateReturnArg is a leaf node

	case *BinaryExpr:
		Walk(v, n.Left)
		Walk(v, n.Right)

	case *UnaryExpr:
		Walk(v, n.Operand)

	case *FunctionCall:
		for _, arg := range n.Args {
			Walk(v, arg)
		}

	case *ArrayRef:
		// n.Name is a string, not a Node
		for _, subscript := range n.Subscripts {
			Walk(v, subscript)
		}

	case *ArraySection:
		// n.Name is a string, not a Node
		for _, subscript := range n.Subscripts {
			if subscript.Lower != nil {
				Walk(v, subscript.Lower)
			}
			if subscript.Upper != nil {
				Walk(v, subscript.Upper)
			}
			if subscript.Stride != nil {
				Walk(v, subscript.Stride)
			}
		}

	case *ParenExpr:
		Walk(v, n.Expr)

	case *ImpliedDoLoop:
		// n.LoopVar is a string, not a Node
		Walk(v, n.Start)
		Walk(v, n.End)
		if n.Stride != nil {
			Walk(v, n.Stride)
		}
		for _, expr := range n.Expressions {
			Walk(v, expr)
		}

	case *RangeExpr:
		if n.Start != nil {
			Walk(v, n.Start)
		}
		if n.End != nil {
			Walk(v, n.End)
		}
		if n.Stride != nil {
			Walk(v, n.Stride)
		}

	case *ArrayConstructor:
		for _, value := range n.Values {
			Walk(v, value)
		}

	case *ComponentAccess:
		Walk(v, n.Base)
		// Field is a string, not a node
	}

	v.Visit(nil)
}

// Inspect traverses an AST in depth-first order: It starts by calling
// f(node); node must not be nil. If f returns true, Inspect invokes f
// recursively for each of the non-nil children of node, followed by a
// call of f(nil).
//
// Inspect is a convenience wrapper around Walk that allows using a
// simple function instead of implementing the Visitor interface.
func Inspect(node Node, f func(Node) bool) {
	Walk(inspector(f), node)
}

type inspector func(Node) bool

func (f inspector) Visit(node Node) Visitor {
	if f(node) {
		return f
	}
	return nil
}
