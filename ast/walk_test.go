package ast

import (
	"testing"

	"github.com/soypat/go-fortran/token"
)

// countVisitor counts how many times Visit is called
type countVisitor struct {
	count int
}

func (v *countVisitor) Visit(node Node) Visitor {
	if node != nil {
		v.count++
	}
	return v
}

func TestWalkProgramBlock(t *testing.T) {
	prog := &Unit{
		Token:    token.PROGRAM,
		Name:     "test",
		Position: Pos(0, 100),
	}
	v := &countVisitor{}
	Walk(v, prog)

	// Should visit the program block once (plus one nil call at end)
	if v.count != 1 {
		t.Errorf("Expected 1 visit, got %d", v.count)
	}
}

func TestWalkModule(t *testing.T) {
	// Create a module with contained procedures
	mod := &Unit{
		Token: token.MODULE,
		Name:  "mymodule",
		Contains: []Unit{
			{Token: token.SUBROUTINE, Name: "sub1"},
			{Token: token.FUNCTION, Name: "func1"},
			{Token: token.SUBROUTINE, Name: "sub2"},
		},
	}

	v := &countVisitor{}
	Walk(v, mod)

	// Should visit: Module + 3 contained procedures = 4
	if v.count != 4 {
		t.Errorf("Expected 4 visits (module + 3 procedures), got %d", v.count)
	}
}

func TestWalkNestedModule(t *testing.T) {
	// Create nested structure (not typical in Fortran, but testing the walker)
	innerMod := Unit{
		Token: token.MODULE,
		Name:  "inner",
		Contains: []Unit{
			{Token: token.SUBROUTINE, Name: "inner_sub"},
		},
	}

	outerMod := &Unit{
		Token:    token.MODULE,
		Name:     "outer",
		Contains: []Unit{innerMod},
	}

	v := &countVisitor{}
	Walk(v, outerMod)

	// Should visit: outer module + inner module + subroutine = 3
	if v.count != 3 {
		t.Errorf("Expected 3 visits, got %d", v.count)
	}
}

// collectVisitor collects all node types visited
type collectVisitor struct {
	nodes []Node
}

func (v *collectVisitor) Visit(node Node) Visitor {
	if node != nil {
		v.nodes = append(v.nodes, node)
	}
	return v
}

func TestWalkCollect(t *testing.T) {
	mod := &Unit{
		Token: token.MODULE,
		Name:  "test",
		Contains: []Unit{
			{Token: token.SUBROUTINE, Name: "sub1"},
			{Token: token.FUNCTION, Name: "func1"},
		},
	}

	v := &collectVisitor{}
	Walk(v, mod)

	if len(v.nodes) != 3 {
		t.Errorf("Expected 3 nodes, got %d", len(v.nodes))
	}

	// Check types - all are *Unit now, so check Token field
	u0, ok := v.nodes[0].(*Unit)
	if !ok || u0.Token != token.MODULE {
		t.Errorf("First node should be Module Unit, got %T", v.nodes[0])
	}
	u1, ok := v.nodes[1].(*Unit)
	if !ok || u1.Token != token.SUBROUTINE {
		t.Errorf("Second node should be Subroutine Unit, got %T", v.nodes[1])
	}
	u2, ok := v.nodes[2].(*Unit)
	if !ok || u2.Token != token.FUNCTION {
		t.Errorf("Third node should be Function Unit, got %T", v.nodes[2])
	}
}

func TestInspect(t *testing.T) {
	mod := &Unit{
		Token: token.MODULE,
		Name:  "test",
		Contains: []Unit{
			{Token: token.SUBROUTINE, Name: "sub1"},
			{Token: token.FUNCTION, Name: "func1"},
			{Token: token.SUBROUTINE, Name: "sub2"},
		},
	}

	// Count subroutines
	subCount := 0
	Inspect(mod, func(n Node) bool {
		if u, ok := n.(*Unit); ok && u.Token == token.SUBROUTINE {
			subCount++
		}
		return true
	})

	if subCount != 2 {
		t.Errorf("Expected 2 subroutines, got %d", subCount)
	}
}

func TestInspectEarlyReturn(t *testing.T) {
	mod := &Unit{
		Token: token.MODULE,
		Name:  "test",
		Contains: []Unit{
			{Token: token.SUBROUTINE, Name: "sub1"},
			{Token: token.FUNCTION, Name: "func1"},
			{Token: token.SUBROUTINE, Name: "sub2"},
		},
	}

	// Count visits but stop after first subroutine
	visitCount := 0
	Inspect(mod, func(n Node) bool {
		visitCount++
		// Stop traversal after first subroutine
		if u, ok := n.(*Unit); ok && u.Token == token.SUBROUTINE {
			return false
		}
		return true
	})

	// Should visit: Module + Subroutine (but not continue into its children)
	// Then continue with Function + second Subroutine
	if visitCount < 2 {
		t.Errorf("Expected at least 2 visits, got %d", visitCount)
	}
}

// Helper to count nodes of a specific token type
func countNodeToken(node Node, tok token.Token) int {
	count := 0
	Inspect(node, func(n Node) bool {
		if u, ok := n.(*Unit); ok && u.Token == tok {
			count++
		}
		return true
	})
	return count
}

func TestCountNodeType(t *testing.T) {
	mod := &Unit{
		Token: token.MODULE,
		Name:  "test",
		Contains: []Unit{
			{Token: token.SUBROUTINE, Name: "sub1", ResultType: TypeSpec{Attributes: []TypeAttribute{{Token: token.RECURSIVE}}}},
			{Token: token.FUNCTION, Name: "func1"},
			{Token: token.SUBROUTINE, Name: "sub2"},
			{Token: token.FUNCTION, Name: "func2"},
		},
	}

	if count := countNodeToken(mod, token.SUBROUTINE); count != 2 {
		t.Errorf("Expected 2 subroutines, got %d", count)
	}

	if count := countNodeToken(mod, token.FUNCTION); count != 2 {
		t.Errorf("Expected 2 functions, got %d", count)
	}

	if count := countNodeToken(mod, token.MODULE); count != 1 {
		t.Errorf("Expected 1 module, got %d", count)
	}
}
