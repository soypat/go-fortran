package fortran

import (
	"strconv"
	"strings"
	"testing"

	"github.com/soypat/go-fortran/ast"
)

// TestStatementParsing verifies that the statement parser correctly constructs
// statement AST nodes for various Fortran statement types.
func TestStatementParsing(t *testing.T) {
	tests := []struct {
		name     string
		src      string
		validate func(t *testing.T, stmt ast.Statement)
	}{
		// ===== Arithmetic IF Statements (F77) =====
		{
			name: "arithmetic IF with simple expression",
			src:  "IF(X-5) 100,200,300",
			validate: func(t *testing.T, stmt ast.Statement) {
				aif, ok := stmt.(*ast.ArithmeticIfStmt)
				if !ok {
					t.Fatalf("Expected *ast.ArithmeticIfStmt, got %T", stmt)
				}

				if aif.NegativeLabel != "100" {
					t.Errorf("Expected negative label '100', got %q", aif.NegativeLabel)
				}
				if aif.ZeroLabel != "200" {
					t.Errorf("Expected zero label '200', got %q", aif.ZeroLabel)
				}
				if aif.PositiveLabel != "300" {
					t.Errorf("Expected positive label '300', got %q", aif.PositiveLabel)
				}

				// Verify condition is a binary expression
				_, ok = aif.Condition.(*ast.BinaryExpr)
				if !ok {
					t.Errorf("Expected condition to be *ast.BinaryExpr, got %T", aif.Condition)
				}
			},
		},
		{
			name: "arithmetic IF with complex expression",
			src:  "IF(ISPACE-2) 500,1000,1500",
			validate: func(t *testing.T, stmt ast.Statement) {
				aif := stmt.(*ast.ArithmeticIfStmt)

				if aif.NegativeLabel != "500" {
					t.Errorf("Expected negative label '500', got %q", aif.NegativeLabel)
				}
				if aif.ZeroLabel != "1000" {
					t.Errorf("Expected zero label '1000', got %q", aif.ZeroLabel)
				}
				if aif.PositiveLabel != "1500" {
					t.Errorf("Expected positive label '1500', got %q", aif.PositiveLabel)
				}
			},
		},
		{
			name: "arithmetic IF with parenthesized expression",
			src:  "IF((A+B)*C) 10,20,30",
			validate: func(t *testing.T, stmt ast.Statement) {
				aif := stmt.(*ast.ArithmeticIfStmt)

				if aif.NegativeLabel != "10" || aif.ZeroLabel != "20" || aif.PositiveLabel != "30" {
					t.Errorf("Expected labels 10,20,30, got %q,%q,%q",
						aif.NegativeLabel, aif.ZeroLabel, aif.PositiveLabel)
				}
			},
		},

		// ===== Inline IF Statements =====
		{
			name: "inline IF with assignment",
			src:  "IF(X.GT.0) Y = 1",
			validate: func(t *testing.T, stmt ast.Statement) {
				ifStmt, ok := stmt.(*ast.IfStmt)
				if !ok {
					t.Fatalf("Expected *ast.IfStmt, got %T", stmt)
				}

				// Should have exactly one statement in ThenPart
				if len(ifStmt.ThenPart) != 1 {
					t.Fatalf("Expected 1 statement in ThenPart, got %d", len(ifStmt.ThenPart))
				}

				// ThenPart should contain an assignment
				_, ok = ifStmt.ThenPart[0].(*ast.AssignmentStmt)
				if !ok {
					t.Errorf("Expected ThenPart[0] to be *ast.AssignmentStmt, got %T", ifStmt.ThenPart[0])
				}

				// Should have no ELSE parts
				if len(ifStmt.ElseIfParts) != 0 {
					t.Errorf("Expected no ELSE IF parts, got %d", len(ifStmt.ElseIfParts))
				}
				if len(ifStmt.ElsePart) != 0 {
					t.Errorf("Expected no ELSE part, got %d statements", len(ifStmt.ElsePart))
				}
			},
		},
		{
			name: "inline IF with CALL statement",
			src:  "IF(LNORMP) CALL TITLE(IOUT15)",
			validate: func(t *testing.T, stmt ast.Statement) {
				ifStmt := stmt.(*ast.IfStmt)

				if len(ifStmt.ThenPart) != 1 {
					t.Fatalf("Expected 1 statement in ThenPart, got %d", len(ifStmt.ThenPart))
				}

				// ThenPart should contain a CALL statement
				callStmt, ok := ifStmt.ThenPart[0].(*ast.CallStmt)
				if !ok {
					t.Fatalf("Expected ThenPart[0] to be *ast.CallStmt, got %T", ifStmt.ThenPart[0])
				}

				if callStmt.Name != "TITLE" {
					t.Errorf("Expected CALL to 'TITLE', got %q", callStmt.Name)
				}
			},
		},

		// ===== GOTO Statements =====
		{
			name: "GOTO statement (single token)",
			src:  "GOTO 100",
			validate: func(t *testing.T, stmt ast.Statement) {
				gotoStmt, ok := stmt.(*ast.GotoStmt)
				if !ok {
					t.Fatalf("Expected *ast.GotoStmt, got %T", stmt)
				}

				if gotoStmt.Target != "100" {
					t.Errorf("Expected target '100', got %q", gotoStmt.Target)
				}
			},
		},
		{
			name: "GO TO statement (two tokens)",
			src:  "GO TO 2000",
			validate: func(t *testing.T, stmt ast.Statement) {
				gotoStmt := stmt.(*ast.GotoStmt)

				if gotoStmt.Target != "2000" {
					t.Errorf("Expected target '2000', got %q", gotoStmt.Target)
				}
			},
		},
		{
			name: "computed GOTO with few labels",
			src:  "GOTO (10,20,30) I",
			validate: func(t *testing.T, stmt ast.Statement) {
				computedGoto, ok := stmt.(*ast.ComputedGotoStmt)
				if !ok {
					t.Fatalf("Expected *ast.ComputedGotoStmt, got %T", stmt)
				}

				expectedLabels := []string{"10", "20", "30"}
				if len(computedGoto.Labels) != len(expectedLabels) {
					t.Fatalf("Expected %d labels, got %d", len(expectedLabels), len(computedGoto.Labels))
				}

				for i, expected := range expectedLabels {
					if computedGoto.Labels[i] != expected {
						t.Errorf("Expected label[%d] = %q, got %q", i, expected, computedGoto.Labels[i])
					}
				}

				// Check expression is an identifier
				ident, ok := computedGoto.Expression.(*ast.Identifier)
				if !ok {
					t.Errorf("Expected expression to be *ast.Identifier, got %T", computedGoto.Expression)
				} else if ident.Value != "I" {
					t.Errorf("Expected expression 'I', got %q", ident.Value)
				}
			},
		},
		{
			name: "computed GOTO with many labels",
			src:  "GOTO(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15) ibeta",
			validate: func(t *testing.T, stmt ast.Statement) {
				computedGoto, ok := stmt.(*ast.ComputedGotoStmt)
				if !ok {
					t.Fatalf("Expected *ast.ComputedGotoStmt, got %T", stmt)
				}

				if len(computedGoto.Labels) != 15 {
					t.Fatalf("Expected 15 labels, got %d", len(computedGoto.Labels))
				}

				// Verify first few labels
				for i := 1; i <= 5; i++ {
					expected := strconv.Itoa(i)
					if computedGoto.Labels[i-1] != expected {
						t.Errorf("Expected label[%d] = %q, got %q", i-1, expected, computedGoto.Labels[i-1])
					}
				}

				// Check expression
				ident, ok := computedGoto.Expression.(*ast.Identifier)
				if !ok {
					t.Errorf("Expected expression to be *ast.Identifier, got %T", computedGoto.Expression)
				} else if ident.Value != "ibeta" {
					t.Errorf("Expected expression 'ibeta', got %q", ident.Value)
				}
			},
		},
		{
			name: "GOTO in inline IF",
			src:  "IF(NPARC.LE.0.AND..NOT.LSTARC) GO TO 2000",
			validate: func(t *testing.T, stmt ast.Statement) {
				ifStmt := stmt.(*ast.IfStmt)

				if len(ifStmt.ThenPart) != 1 {
					t.Fatalf("Expected 1 statement in ThenPart, got %d", len(ifStmt.ThenPart))
				}

				gotoStmt, ok := ifStmt.ThenPart[0].(*ast.GotoStmt)
				if !ok {
					t.Fatalf("Expected *ast.GotoStmt in ThenPart, got %T", ifStmt.ThenPart[0])
				}

				if gotoStmt.Target != "2000" {
					t.Errorf("Expected target '2000', got %q", gotoStmt.Target)
				}
			},
		},

		// ===== WRITE Statements =====
		{
			name: "WRITE statement with simple output",
			src:  "WRITE(6) X",
			validate: func(t *testing.T, stmt ast.Statement) {
				writeStmt, ok := stmt.(*ast.WriteStmt)
				if !ok {
					t.Fatalf("Expected *ast.WriteStmt, got %T", stmt)
				}

				if writeStmt.Unit == nil {
					t.Error("Expected non-nil Unit")
				}

				if len(writeStmt.OutputList) != 1 {
					t.Errorf("Expected 1 output item, got %d", len(writeStmt.OutputList))
				}
			},
		},
		{
			name: "WRITE statement with multiple outputs",
			src:  "WRITE(91) BIASP,BIAS,DYNEQ",
			validate: func(t *testing.T, stmt ast.Statement) {
				writeStmt := stmt.(*ast.WriteStmt)

				if len(writeStmt.OutputList) != 3 {
					t.Fatalf("Expected 3 output items, got %d", len(writeStmt.OutputList))
				}

				// All output items should be identifiers
				for i, item := range writeStmt.OutputList {
					if _, ok := item.(*ast.Identifier); !ok {
						t.Errorf("Expected OutputList[%d] to be *ast.Identifier, got %T", i, item)
					}
				}
			},
		},
		{
			name: "WRITE in inline IF",
			src:  "IF(LSTINR) WRITE(91) BIASP,BIAS,DYNEQ",
			validate: func(t *testing.T, stmt ast.Statement) {
				ifStmt := stmt.(*ast.IfStmt)

				if len(ifStmt.ThenPart) != 1 {
					t.Fatalf("Expected 1 statement in ThenPart, got %d", len(ifStmt.ThenPart))
				}

				writeStmt, ok := ifStmt.ThenPart[0].(*ast.WriteStmt)
				if !ok {
					t.Fatalf("Expected *ast.WriteStmt in ThenPart, got %T", ifStmt.ThenPart[0])
				}

				if len(writeStmt.OutputList) != 3 {
					t.Errorf("Expected 3 output items, got %d", len(writeStmt.OutputList))
				}
			},
		},

		// ===== Block IF with ENDIF (F77 single token) =====
		{
			name: "block IF with ENDIF single token",
			src: `IF(X > 0) THEN
         Y = 1
      ENDIF`,
			validate: func(t *testing.T, stmt ast.Statement) {
				ifStmt, ok := stmt.(*ast.IfStmt)
				if !ok {
					t.Fatalf("Expected *ast.IfStmt, got %T", stmt)
				}

				if len(ifStmt.ThenPart) != 1 {
					t.Fatalf("Expected 1 statement in ThenPart, got %d", len(ifStmt.ThenPart))
				}

				_, ok = ifStmt.ThenPart[0].(*ast.AssignmentStmt)
				if !ok {
					t.Errorf("Expected ThenPart[0] to be *ast.AssignmentStmt, got %T", ifStmt.ThenPart[0])
				}
			},
		},

		// ===== Array Slice Syntax (F90) =====
		{
			name: "array slice with colon only",
			src:  "LEDIT_EXTRA(:) = .FALSE.",
			validate: func(t *testing.T, stmt ast.Statement) {
				assignment, ok := stmt.(*ast.AssignmentStmt)
				if !ok {
					t.Fatalf("Expected *ast.AssignmentStmt, got %T", stmt)
				}

				// Target should be a function call with one argument (the range)
				funcCall, ok := assignment.Target.(*ast.FunctionCall)
				if !ok {
					t.Fatalf("Expected Target to be *ast.FunctionCall, got %T", assignment.Target)
				}

				if funcCall.Name != "LEDIT_EXTRA" {
					t.Errorf("Expected array name 'LEDIT_EXTRA', got %q", funcCall.Name)
				}

				if len(funcCall.Args) != 1 {
					t.Fatalf("Expected 1 argument (slice), got %d", len(funcCall.Args))
				}

				rangeExpr, ok := funcCall.Args[0].(*ast.RangeExpr)
				if !ok {
					t.Fatalf("Expected argument to be *ast.RangeExpr, got %T", funcCall.Args[0])
				}

				if rangeExpr.Start != nil {
					t.Error("Expected Start to be nil for ':'")
				}
				if rangeExpr.End != nil {
					t.Error("Expected End to be nil for ':'")
				}
				if rangeExpr.Stride != nil {
					t.Error("Expected Stride to be nil for ':'")
				}
			},
		},
		{
			name: "array slice with start:end",
			src:  "X = ARR(1:10)",
			validate: func(t *testing.T, stmt ast.Statement) {
				assignment := stmt.(*ast.AssignmentStmt)

				funcCall, ok := assignment.Value.(*ast.FunctionCall)
				if !ok {
					t.Fatalf("Expected Value to be *ast.FunctionCall, got %T", assignment.Value)
				}

				if len(funcCall.Args) != 1 {
					t.Fatalf("Expected 1 argument, got %d", len(funcCall.Args))
				}

				rangeExpr, ok := funcCall.Args[0].(*ast.RangeExpr)
				if !ok {
					t.Fatalf("Expected argument to be *ast.RangeExpr, got %T", funcCall.Args[0])
				}

				if rangeExpr.Start == nil {
					t.Error("Expected Start to be non-nil")
				}
				if rangeExpr.End == nil {
					t.Error("Expected End to be non-nil")
				}
				if rangeExpr.Stride != nil {
					t.Error("Expected Stride to be nil for 'start:end'")
				}
			},
		},
		{
			name: "array slice with start:end:stride",
			src:  "X = ARR(1:10:2)",
			validate: func(t *testing.T, stmt ast.Statement) {
				assignment := stmt.(*ast.AssignmentStmt)

				funcCall := assignment.Value.(*ast.FunctionCall)
				rangeExpr := funcCall.Args[0].(*ast.RangeExpr)

				if rangeExpr.Start == nil {
					t.Error("Expected Start to be non-nil")
				}
				if rangeExpr.End == nil {
					t.Error("Expected End to be non-nil")
				}
				if rangeExpr.Stride == nil {
					t.Error("Expected Stride to be non-nil for 'start:end:stride'")
				}
			},
		},
		{
			name: "array slice with :end",
			src:  "X = ARR(:5)",
			validate: func(t *testing.T, stmt ast.Statement) {
				assignment := stmt.(*ast.AssignmentStmt)

				funcCall := assignment.Value.(*ast.FunctionCall)
				rangeExpr := funcCall.Args[0].(*ast.RangeExpr)

				if rangeExpr.Start != nil {
					t.Error("Expected Start to be nil for ':end'")
				}
				if rangeExpr.End == nil {
					t.Error("Expected End to be non-nil")
				}
			},
		},
		{
			name: "array slice with start:",
			src:  "X = ARR(5:)",
			validate: func(t *testing.T, stmt ast.Statement) {
				assignment := stmt.(*ast.AssignmentStmt)

				funcCall := assignment.Value.(*ast.FunctionCall)
				rangeExpr := funcCall.Args[0].(*ast.RangeExpr)

				if rangeExpr.Start == nil {
					t.Error("Expected Start to be non-nil")
				}
				if rangeExpr.End != nil {
					t.Error("Expected End to be nil for 'start:'")
				}
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Wrap the statement in a minimal PROGRAM structure for parsing
			wrappedSrc := "PROGRAM test\n" + tt.src + "\nEND PROGRAM test"

			var parser Parser90
			err := parser.Reset(tt.name+".f90", strings.NewReader(wrappedSrc))
			if err != nil {
				t.Fatalf("Reset failed: %v", err)
			}

			unit := parser.ParseNextProgramUnit()
			if unit == nil {
				t.Fatal("ParseNextProgramUnit returned nil")
			}

			if len(parser.Errors()) > 0 {
				t.Fatalf("Parse errors: %v", parser.Errors())
			}

			// Extract the statement from the parsed program
			progBlock, ok := unit.(*ast.ProgramBlock)
			if !ok {
				t.Fatalf("Expected *ast.ProgramBlock, got %T", unit)
			}

			// The statement should be in Body
			if len(progBlock.Body) == 0 {
				t.Fatal("No statements found in parsed program")
			}
			stmt := progBlock.Body[0]

			// Run the validation function
			tt.validate(t, stmt)
		})
	}
}
