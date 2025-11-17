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
	prog := &ProgramBlock{
		Name:     "test",
		StartPos: 0,
		EndPos:   100,
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
	mod := &Module{
		Name: "mymodule",
		Contains: []ProgramUnit{
			&Subroutine{Name: "sub1"},
			&Function{Name: "func1"},
			&Subroutine{Name: "sub2"},
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
	innerMod := &Module{
		Name: "inner",
		Contains: []ProgramUnit{
			&Subroutine{Name: "inner_sub"},
		},
	}

	outerMod := &Module{
		Name:     "outer",
		Contains: []ProgramUnit{innerMod},
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
	mod := &Module{
		Name: "test",
		Contains: []ProgramUnit{
			&Subroutine{Name: "sub1"},
			&Function{Name: "func1"},
		},
	}

	v := &collectVisitor{}
	Walk(v, mod)

	if len(v.nodes) != 3 {
		t.Errorf("Expected 3 nodes, got %d", len(v.nodes))
	}

	// Check types
	if _, ok := v.nodes[0].(*Module); !ok {
		t.Errorf("First node should be Module, got %T", v.nodes[0])
	}
	if _, ok := v.nodes[1].(*Subroutine); !ok {
		t.Errorf("Second node should be Subroutine, got %T", v.nodes[1])
	}
	if _, ok := v.nodes[2].(*Function); !ok {
		t.Errorf("Third node should be Function, got %T", v.nodes[2])
	}
}

func TestInspect(t *testing.T) {
	mod := &Module{
		Name: "test",
		Contains: []ProgramUnit{
			&Subroutine{Name: "sub1"},
			&Function{Name: "func1"},
			&Subroutine{Name: "sub2"},
		},
	}

	// Count subroutines
	subCount := 0
	Inspect(mod, func(n Node) bool {
		if _, ok := n.(*Subroutine); ok {
			subCount++
		}
		return true
	})

	if subCount != 2 {
		t.Errorf("Expected 2 subroutines, got %d", subCount)
	}
}

func TestInspectEarlyReturn(t *testing.T) {
	mod := &Module{
		Name: "test",
		Contains: []ProgramUnit{
			&Subroutine{Name: "sub1"},
			&Function{Name: "func1"},
			&Subroutine{Name: "sub2"},
		},
	}

	// Count visits but stop after first subroutine
	visitCount := 0
	Inspect(mod, func(n Node) bool {
		visitCount++
		// Stop traversal after first subroutine
		if _, ok := n.(*Subroutine); ok {
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

// Helper to count nodes of a specific type
func countNodeType(node Node, target any) int {
	count := 0
	Inspect(node, func(n Node) bool {
		switch target.(type) {
		case *Subroutine:
			if _, ok := n.(*Subroutine); ok {
				count++
			}
		case *Function:
			if _, ok := n.(*Function); ok {
				count++
			}
		case *Module:
			if _, ok := n.(*Module); ok {
				count++
			}
		}
		return true
	})
	return count
}

func TestCountNodeType(t *testing.T) {
	mod := &Module{
		Name: "test",
		Contains: []ProgramUnit{
			&Subroutine{Name: "sub1", Attributes: []token.Token{token.RECURSIVE}},
			&Function{Name: "func1"},
			&Subroutine{Name: "sub2"},
			&Function{Name: "func2"},
		},
	}

	if count := countNodeType(mod, (*Subroutine)(nil)); count != 2 {
		t.Errorf("Expected 2 subroutines, got %d", count)
	}

	if count := countNodeType(mod, (*Function)(nil)); count != 2 {
		t.Errorf("Expected 2 functions, got %d", count)
	}

	if count := countNodeType(mod, (*Module)(nil)); count != 1 {
		t.Errorf("Expected 1 module, got %d", count)
	}
}
