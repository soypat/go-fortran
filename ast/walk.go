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
	case *ProgramBlock:
		// ProgramBlock has no child nodes, only token tuples

	case *Subroutine:
		// Subroutine has no child nodes in Phase 1, only token tuples

	case *Function:
		// Function has no child nodes in Phase 1, only token tuples

	case *Module:
		// Walk contained procedures
		for _, proc := range n.Contains {
			Walk(v, proc)
		}

	case *BlockData:
		// BlockData has no child nodes, only token tuples

	case *Identifier:
		// Identifier is a leaf node

	case *IntegerLiteral:
		// IntegerLiteral is a leaf node

	// Phase 2 nodes can be added here as they are implemented
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

// walkList walks a list of nodes
func walkList(v Visitor, list []Node) {
	for _, n := range list {
		Walk(v, n)
	}
}
