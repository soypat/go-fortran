package fortran_test

import (
	"strings"
	"testing"

	"github.com/soypat/go-fortran"
	"github.com/soypat/go-fortran/ast"
)

// TestExecutableStatements tests parsing of executable statements in program units
func TestExecutableStatements(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		validate func(t *testing.T, unit ast.ProgramUnit)
	}{
		{
			name: "simple assignment",
			source: `
PROGRAM test
  INTEGER :: x
  x = 10
END PROGRAM test`,
			validate: func(t *testing.T, unit ast.ProgramUnit) {
				prog, ok := unit.(*ast.ProgramBlock)
				if !ok {
					t.Fatalf("Expected ProgramBlock, got %T", unit)
				}
				if len(prog.Body) != 2 {
					t.Fatalf("Expected 2 statements, got %d", len(prog.Body))
				}
				assign, ok := prog.Body[1].(*ast.AssignmentStmt)
				if !ok {
					t.Fatalf("Expected AssignmentStmt, got %T", prog.Body[1])
				}
				if ident, ok := assign.Target.(*ast.Identifier); !ok || ident.Value != "x" {
					t.Errorf("Expected target 'x', got %v", assign.Target)
				}
			},
		},
		{
			name: "if-then-endif",
			source: `
PROGRAM test
  INTEGER :: x
  IF (x > 0) THEN
    x = 1
  END IF
END PROGRAM test`,
			validate: func(t *testing.T, unit ast.ProgramUnit) {
				prog := unit.(*ast.ProgramBlock)
				if len(prog.Body) != 2 {
					t.Fatalf("Expected 2 statements, got %d", len(prog.Body))
				}
				ifStmt, ok := prog.Body[1].(*ast.IfStmt)
				if !ok {
					t.Fatalf("Expected IfStmt, got %T", prog.Body[1])
				}
				if len(ifStmt.ThenPart) != 1 {
					t.Errorf("Expected 1 statement in THEN part, got %d", len(ifStmt.ThenPart))
				}
			},
		},
		{
			name: "if-then-else-endif",
			source: `
PROGRAM test
  INTEGER :: x
  IF (x > 0) THEN
    x = 1
  ELSE
    x = -1
  END IF
END PROGRAM test`,
			validate: func(t *testing.T, unit ast.ProgramUnit) {
				prog := unit.(*ast.ProgramBlock)
				ifStmt := prog.Body[1].(*ast.IfStmt)
				if len(ifStmt.ThenPart) != 1 {
					t.Errorf("Expected 1 statement in THEN part, got %d", len(ifStmt.ThenPart))
				}
				if len(ifStmt.ElsePart) != 1 {
					t.Errorf("Expected 1 statement in ELSE part, got %d", len(ifStmt.ElsePart))
				}
			},
		},
		{
			name: "if-then-elseif-endif",
			source: `
PROGRAM test
  INTEGER :: x
  IF (x > 0) THEN
    x = 1
  ELSE IF (x < 0) THEN
    x = -1
  END IF
END PROGRAM test`,
			validate: func(t *testing.T, unit ast.ProgramUnit) {
				prog := unit.(*ast.ProgramBlock)
				ifStmt := prog.Body[1].(*ast.IfStmt)
				if len(ifStmt.ElseIfParts) != 1 {
					t.Errorf("Expected 1 ELSE IF clause, got %d", len(ifStmt.ElseIfParts))
				}
			},
		},
		{
			name: "do loop with counter",
			source: `
PROGRAM test
  INTEGER :: i
  DO i = 1, 10
    CONTINUE
  END DO
END PROGRAM test`,
			validate: func(t *testing.T, unit ast.ProgramUnit) {
				prog := unit.(*ast.ProgramBlock)
				if len(prog.Body) != 2 {
					t.Fatalf("Expected 2 statements, got %d", len(prog.Body))
				}
				doLoop, ok := prog.Body[1].(*ast.DoLoop)
				if !ok {
					t.Fatalf("Expected DoLoop, got %T", prog.Body[1])
				}
				if doLoop.Var != "i" {
					t.Errorf("Expected loop var 'i', got %q", doLoop.Var)
				}
				if len(doLoop.Body) != 1 {
					t.Errorf("Expected 1 statement in loop body, got %d", len(doLoop.Body))
				}
			},
		},
		{
			name: "do loop with step",
			source: `
PROGRAM test
  INTEGER :: i
  DO i = 1, 10, 2
    CONTINUE
  END DO
END PROGRAM test`,
			validate: func(t *testing.T, unit ast.ProgramUnit) {
				prog := unit.(*ast.ProgramBlock)
				doLoop := prog.Body[1].(*ast.DoLoop)
				if doLoop.Step == nil {
					t.Error("Expected step expression, got nil")
				}
			},
		},
		{
			name: "call statement",
			source: `
PROGRAM test
  CALL sub()
END PROGRAM test`,
			validate: func(t *testing.T, unit ast.ProgramUnit) {
				prog := unit.(*ast.ProgramBlock)
				if len(prog.Body) != 1 {
					t.Fatalf("Expected 1 statement, got %d", len(prog.Body))
				}
				callStmt, ok := prog.Body[0].(*ast.CallStmt)
				if !ok {
					t.Fatalf("Expected CallStmt, got %T", prog.Body[0])
				}
				if callStmt.Name != "sub" {
					t.Errorf("Expected call to 'sub', got %q", callStmt.Name)
				}
			},
		},
		{
			name: "call with arguments",
			source: `
PROGRAM test
  INTEGER :: x, y
  CALL sub(x, y)
END PROGRAM test`,
			validate: func(t *testing.T, unit ast.ProgramUnit) {
				prog := unit.(*ast.ProgramBlock)
				callStmt := prog.Body[1].(*ast.CallStmt)
				if len(callStmt.Args) != 2 {
					t.Errorf("Expected 2 arguments, got %d", len(callStmt.Args))
				}
			},
		},
		{
			name: "return statement",
			source: `
SUBROUTINE test()
  RETURN
END SUBROUTINE test`,
			validate: func(t *testing.T, unit ast.ProgramUnit) {
				sub := unit.(*ast.Subroutine)
				if len(sub.Body) != 1 {
					t.Fatalf("Expected 1 statement, got %d", len(sub.Body))
				}
				if _, ok := sub.Body[0].(*ast.ReturnStmt); !ok {
					t.Fatalf("Expected ReturnStmt, got %T", sub.Body[0])
				}
			},
		},
		{
			name: "nested if statements",
			source: `
PROGRAM test
  INTEGER :: x, y
  IF (x > 0) THEN
    IF (y > 0) THEN
      x = 1
    END IF
  END IF
END PROGRAM test`,
			validate: func(t *testing.T, unit ast.ProgramUnit) {
				prog := unit.(*ast.ProgramBlock)
				outerIf := prog.Body[1].(*ast.IfStmt)
				if len(outerIf.ThenPart) != 1 {
					t.Fatalf("Expected 1 statement in outer THEN, got %d", len(outerIf.ThenPart))
				}
				innerIf, ok := outerIf.ThenPart[0].(*ast.IfStmt)
				if !ok {
					t.Fatalf("Expected nested IfStmt, got %T", outerIf.ThenPart[0])
				}
				if len(innerIf.ThenPart) != 1 {
					t.Errorf("Expected 1 statement in inner THEN, got %d", len(innerIf.ThenPart))
				}
			},
		},
		{
			name: "nested do loops",
			source: `
PROGRAM test
  INTEGER :: i, j
  DO i = 1, 10
    DO j = 1, 5
      CONTINUE
    END DO
  END DO
END PROGRAM test`,
			validate: func(t *testing.T, unit ast.ProgramUnit) {
				prog := unit.(*ast.ProgramBlock)
				outerDo := prog.Body[1].(*ast.DoLoop)
				if len(outerDo.Body) != 1 {
					t.Fatalf("Expected 1 statement in outer loop, got %d", len(outerDo.Body))
				}
				innerDo, ok := outerDo.Body[0].(*ast.DoLoop)
				if !ok {
					t.Fatalf("Expected nested DoLoop, got %T", outerDo.Body[0])
				}
				if innerDo.Var != "j" {
					t.Errorf("Expected inner loop var 'j', got %q", innerDo.Var)
				}
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var parser fortran.Parser90
			err := parser.Reset("test.f90", strings.NewReader(tt.source))
			if err != nil {
				t.Fatalf("Reset failed: %v", err)
			}

			unit := parser.ParseNextProgramUnit()
			if unit == nil {
				t.Fatal("ParseNextProgramUnit returned nil")
			}

			if len(parser.Errors()) > 0 {
				t.Errorf("Parse errors:")
				for _, e := range parser.Errors() {
					t.Errorf("  %v", &e)
				}
				return
			}

			tt.validate(t, unit)
		})
	}
}

// TestExecutableStatementsInSubroutine tests executable statements in subroutines
func TestExecutableStatementsInSubroutine(t *testing.T) {
	source := `
SUBROUTINE compute(x, y, result)
  INTEGER, INTENT(IN) :: x, y
  INTEGER, INTENT(OUT) :: result

  IF (x > y) THEN
    result = x
  ELSE
    result = y
  END IF
END SUBROUTINE compute`

	var parser fortran.Parser90
	err := parser.Reset("test.f90", strings.NewReader(source))
	if err != nil {
		t.Fatalf("Reset failed: %v", err)
	}

	unit := parser.ParseNextProgramUnit()
	if unit == nil {
		t.Fatal("ParseNextProgramUnit returned nil")
	}

	if len(parser.Errors()) > 0 {
		for _, e := range parser.Errors() {
			t.Errorf("Parse error: %v", e)
		}
		return
	}

	sub, ok := unit.(*ast.Subroutine)
	if !ok {
		t.Fatalf("Expected Subroutine, got %T", unit)
	}

	// Should have 2 type declarations (x,y in one; result in another) + 1 if statement
	if len(sub.Body) != 3 {
		t.Errorf("Expected 3 statements, got %d", len(sub.Body))
		for i, stmt := range sub.Body {
			t.Logf("  %d: %T", i, stmt)
		}
	}

	// Verify last statement is an IfStmt
	if _, ok := sub.Body[2].(*ast.IfStmt); !ok {
		t.Errorf("Expected last statement to be IfStmt, got %T", sub.Body[2])
	}
}
