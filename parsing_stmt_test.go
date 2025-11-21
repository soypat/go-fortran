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
			name: "computed GOTO with comma before variable",
			src:  "GO TO (1000,1300,1700,1900,2100,2300),MCALL",
			validate: func(t *testing.T, stmt ast.Statement) {
				computedGoto, ok := stmt.(*ast.ComputedGotoStmt)
				if !ok {
					t.Fatalf("Expected *ast.ComputedGotoStmt, got %T", stmt)
				}

				expectedLabels := []string{"1000", "1300", "1700", "1900", "2100", "2300"}
				if len(computedGoto.Labels) != len(expectedLabels) {
					t.Fatalf("Expected %d labels, got %d", len(expectedLabels), len(computedGoto.Labels))
				}

				for i, expected := range expectedLabels {
					if computedGoto.Labels[i] != expected {
						t.Errorf("Expected label[%d] = %q, got %q", i, expected, computedGoto.Labels[i])
					}
				}

				// Check expression is MCALL
				ident, ok := computedGoto.Expression.(*ast.Identifier)
				if !ok {
					t.Errorf("Expected expression to be *ast.Identifier, got %T", computedGoto.Expression)
				} else if ident.Value != "MCALL" {
					t.Errorf("Expected expression 'MCALL', got %q", ident.Value)
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
		{
			name: "inline IF with keyword array assignment",
			src:  "IF(1) RESULT(N)=1",
			validate: func(t *testing.T, stmt ast.Statement) {
				ifStmt, ok := stmt.(*ast.IfStmt)
				if !ok {
					t.Fatalf("Expected *ast.IfStmt, got %T", stmt)
				}

				if len(ifStmt.ThenPart) != 1 {
					t.Fatalf("Expected 1 statement in ThenPart, got %d", len(ifStmt.ThenPart))
				}

				assignStmt, ok := ifStmt.ThenPart[0].(*ast.AssignmentStmt)
				if !ok {
					t.Fatalf("Expected *ast.AssignmentStmt in ThenPart, got %T", ifStmt.ThenPart[0])
				}

				// Verify target is a function call (array reference)
				_, ok = assignStmt.Target.(*ast.FunctionCall)
				if !ok {
					t.Errorf("Expected Target to be *ast.FunctionCall (array ref), got %T", assignStmt.Target)
				}
			},
		},

		// ===== Keywords Used as Identifiers =====
		{
			name: "keyword as simple variable assignment",
			src:  "RESULT=1",
			validate: func(t *testing.T, stmt ast.Statement) {
				assignStmt, ok := stmt.(*ast.AssignmentStmt)
				if !ok {
					t.Fatalf("Expected *ast.AssignmentStmt, got %T", stmt)
				}

				ident, ok := assignStmt.Target.(*ast.Identifier)
				if !ok {
					t.Fatalf("Expected Target to be *ast.Identifier, got %T", assignStmt.Target)
				}

				if ident.Value != "RESULT" {
					t.Errorf("Expected identifier 'RESULT', got %q", ident.Value)
				}
			},
		},
		{
			name: "keyword as array variable assignment",
			src:  "RESULT(N)=1",
			validate: func(t *testing.T, stmt ast.Statement) {
				assignStmt, ok := stmt.(*ast.AssignmentStmt)
				if !ok {
					t.Fatalf("Expected *ast.AssignmentStmt, got %T", stmt)
				}

				funcCall, ok := assignStmt.Target.(*ast.FunctionCall)
				if !ok {
					t.Fatalf("Expected Target to be *ast.FunctionCall (array ref), got %T", assignStmt.Target)
				}

				if funcCall.Name != "RESULT" {
					t.Errorf("Expected function name 'RESULT', got %q", funcCall.Name)
				}

				if len(funcCall.Args) != 1 {
					t.Errorf("Expected 1 argument, got %d", len(funcCall.Args))
				}
			},
		},
		{
			name: "STOP keyword as array variable",
			src:  "STOP(I)=5",
			validate: func(t *testing.T, stmt ast.Statement) {
				assignStmt, ok := stmt.(*ast.AssignmentStmt)
				if !ok {
					t.Fatalf("Expected *ast.AssignmentStmt, got %T", stmt)
				}

				funcCall, ok := assignStmt.Target.(*ast.FunctionCall)
				if !ok {
					t.Fatalf("Expected Target to be *ast.FunctionCall (array ref), got %T", assignStmt.Target)
				}

				if funcCall.Name != "STOP" {
					t.Errorf("Expected function name 'STOP', got %q", funcCall.Name)
				}
			},
		},
		{
			name: "STOP keyword as simple variable",
			src:  "STOP=.TRUE.",
			validate: func(t *testing.T, stmt ast.Statement) {
				assignStmt, ok := stmt.(*ast.AssignmentStmt)
				if !ok {
					t.Fatalf("Expected *ast.AssignmentStmt, got %T", stmt)
				}

				ident, ok := assignStmt.Target.(*ast.Identifier)
				if !ok {
					t.Fatalf("Expected Target to be *ast.Identifier, got %T", assignStmt.Target)
				}

				if ident.Value != "STOP" {
					t.Errorf("Expected identifier 'STOP', got %q", ident.Value)
				}
			},
		},

		// ===== Implied DO Loops in I/O Statements =====
		{
			name: "WRITE with implied DO loop - single expression",
			src:  "WRITE(6,10109) (COVSCR(M,1), M=1, 6)",
			validate: func(t *testing.T, stmt ast.Statement) {
				writeStmt, ok := stmt.(*ast.WriteStmt)
				if !ok {
					t.Fatalf("Expected *ast.WriteStmt, got %T", stmt)
				}

				if len(writeStmt.OutputList) != 1 {
					t.Fatalf("Expected 1 output item, got %d", len(writeStmt.OutputList))
				}

				impliedDo, ok := writeStmt.OutputList[0].(*ast.ImpliedDoLoop)
				if !ok {
					t.Fatalf("Expected *ast.ImpliedDoLoop, got %T", writeStmt.OutputList[0])
				}

				if len(impliedDo.Expressions) != 1 {
					t.Errorf("Expected 1 expression in implied DO, got %d", len(impliedDo.Expressions))
				}

				if impliedDo.LoopVar != "M" {
					t.Errorf("Expected loop variable 'M', got %q", impliedDo.LoopVar)
				}

				// Check start value
				startIdent, ok := impliedDo.Start.(*ast.IntegerLiteral)
				if !ok {
					t.Errorf("Expected start to be *ast.IntegerLiteral, got %T", impliedDo.Start)
				} else if startIdent.Raw != "1" {
					t.Errorf("Expected start value '1', got %q", startIdent.Raw)
				}

				// Check end value
				endIdent, ok := impliedDo.End.(*ast.IntegerLiteral)
				if !ok {
					t.Errorf("Expected end to be *ast.IntegerLiteral, got %T", impliedDo.End)
				} else if endIdent.Raw != "6" {
					t.Errorf("Expected end value '6', got %q", endIdent.Raw)
				}

				// Stride should be nil
				if impliedDo.Stride != nil {
					t.Errorf("Expected nil stride, got %T", impliedDo.Stride)
				}
			},
		},
		{
			name: "WRITE with implied DO loop - multiple expressions",
			src:  "WRITE(IOUT6,10109) TOTAL, DEL, (COVSCR(M,1), M=1, 6)",
			validate: func(t *testing.T, stmt ast.Statement) {
				writeStmt, ok := stmt.(*ast.WriteStmt)
				if !ok {
					t.Fatalf("Expected *ast.WriteStmt, got %T", stmt)
				}

				if len(writeStmt.OutputList) != 3 {
					t.Fatalf("Expected 3 output items, got %d", len(writeStmt.OutputList))
				}

				// First two should be identifiers
				for i := 0; i < 2; i++ {
					if _, ok := writeStmt.OutputList[i].(*ast.Identifier); !ok {
						t.Errorf("Expected OutputList[%d] to be *ast.Identifier, got %T", i, writeStmt.OutputList[i])
					}
				}

				// Third should be implied DO loop
				impliedDo, ok := writeStmt.OutputList[2].(*ast.ImpliedDoLoop)
				if !ok {
					t.Fatalf("Expected *ast.ImpliedDoLoop, got %T", writeStmt.OutputList[2])
				}

				if impliedDo.LoopVar != "M" {
					t.Errorf("Expected loop variable 'M', got %q", impliedDo.LoopVar)
				}
			},
		},
		{
			name: "implied DO loop with stride",
			src:  "WRITE(6) (A(I), I=1, 10, 2)",
			validate: func(t *testing.T, stmt ast.Statement) {
				writeStmt, ok := stmt.(*ast.WriteStmt)
				if !ok {
					t.Fatalf("Expected *ast.WriteStmt, got %T", stmt)
				}

				impliedDo, ok := writeStmt.OutputList[0].(*ast.ImpliedDoLoop)
				if !ok {
					t.Fatalf("Expected *ast.ImpliedDoLoop, got %T", writeStmt.OutputList[0])
				}

				if impliedDo.LoopVar != "I" {
					t.Errorf("Expected loop variable 'I', got %q", impliedDo.LoopVar)
				}

				// Check stride is present
				if impliedDo.Stride == nil {
					t.Error("Expected non-nil stride")
				} else {
					strideVal, ok := impliedDo.Stride.(*ast.IntegerLiteral)
					if !ok {
						t.Errorf("Expected stride to be *ast.IntegerLiteral, got %T", impliedDo.Stride)
					} else if strideVal.Raw != "2" {
						t.Errorf("Expected stride value '2', got %q", strideVal.Raw)
					}
				}
			},
		},
		{
			name: "implied DO loop with multiple output expressions",
			src:  "WRITE(6) (A(I), B(I), I=1, N)",
			validate: func(t *testing.T, stmt ast.Statement) {
				writeStmt, ok := stmt.(*ast.WriteStmt)
				if !ok {
					t.Fatalf("Expected *ast.WriteStmt, got %T", stmt)
				}

				impliedDo, ok := writeStmt.OutputList[0].(*ast.ImpliedDoLoop)
				if !ok {
					t.Fatalf("Expected *ast.ImpliedDoLoop, got %T", writeStmt.OutputList[0])
				}

				// Should have 2 expressions before the loop control
				if len(impliedDo.Expressions) != 2 {
					t.Errorf("Expected 2 expressions in implied DO, got %d", len(impliedDo.Expressions))
				}

				if impliedDo.LoopVar != "I" {
					t.Errorf("Expected loop variable 'I', got %q", impliedDo.LoopVar)
				}
			},
		},

		// ===== I/O statements with keyword=value control specs =====
		{
			name: "READ with END= keyword",
			src:  "READ(14,5000,END=500) X",
			validate: func(t *testing.T, stmt ast.Statement) {
				readStmt, ok := stmt.(*ast.ReadStmt)
				if !ok {
					t.Fatalf("Expected *ast.ReadStmt, got %T", stmt)
				}

				// Should have 3 specs: unit, format, END=500
				// (Unit is parsed from first spec)
				if readStmt.Unit == nil {
					t.Error("Expected non-nil Unit")
				}

				// Check we have at least one input
				if len(readStmt.InputList) == 0 {
					t.Error("Expected at least one input")
				}
			},
		},
		{
			name: "READ with IOSTAT= keyword",
			src:  "READ(14,5000,IOSTAT=IOS) X, Y",
			validate: func(t *testing.T, stmt ast.Statement) {
				readStmt, ok := stmt.(*ast.ReadStmt)
				if !ok {
					t.Fatalf("Expected *ast.ReadStmt, got %T", stmt)
				}

				if readStmt.Unit == nil {
					t.Error("Expected non-nil Unit")
				}

				// Check we have two inputs
				if len(readStmt.InputList) != 2 {
					t.Errorf("Expected 2 inputs, got %d", len(readStmt.InputList))
				}
			},
		},
		{
			name: "READ with multiple keyword=value specs",
			src:  "READ(14,81200,IOSTAT=IOS,END=60000) X",
			validate: func(t *testing.T, stmt ast.Statement) {
				readStmt, ok := stmt.(*ast.ReadStmt)
				if !ok {
					t.Fatalf("Expected *ast.ReadStmt, got %T", stmt)
				}

				// This was the failing case - should parse successfully
				if readStmt.Unit == nil {
					t.Error("Expected non-nil Unit")
				}

				if len(readStmt.InputList) != 1 {
					t.Errorf("Expected 1 input, got %d", len(readStmt.InputList))
				}
			},
		},
		{
			name: "WRITE with FMT= keyword",
			src:  "WRITE(6,FMT=100) X, Y",
			validate: func(t *testing.T, stmt ast.Statement) {
				writeStmt, ok := stmt.(*ast.WriteStmt)
				if !ok {
					t.Fatalf("Expected *ast.WriteStmt, got %T", stmt)
				}

				if writeStmt.Unit == nil {
					t.Error("Expected non-nil Unit")
				}

				if len(writeStmt.OutputList) != 2 {
					t.Errorf("Expected 2 outputs, got %d", len(writeStmt.OutputList))
				}
			},
		},
		{
			name: "READ with ERR= keyword",
			src:  "READ(14,5000,ERR=999) A, B, C",
			validate: func(t *testing.T, stmt ast.Statement) {
				readStmt, ok := stmt.(*ast.ReadStmt)
				if !ok {
					t.Fatalf("Expected *ast.ReadStmt, got %T", stmt)
				}

				if readStmt.Unit == nil {
					t.Error("Expected non-nil Unit")
				}

				if len(readStmt.InputList) != 3 {
					t.Errorf("Expected 3 inputs, got %d", len(readStmt.InputList))
				}
			},
		},
		{
			name: "WRITE with UNIT= and FMT= keywords",
			src:  "WRITE(UNIT=6,FMT=100) MESSAGE",
			validate: func(t *testing.T, stmt ast.Statement) {
				writeStmt, ok := stmt.(*ast.WriteStmt)
				if !ok {
					t.Fatalf("Expected *ast.WriteStmt, got %T", stmt)
				}

				// With keyword form, both should be parsed
				if writeStmt.Unit == nil {
					t.Error("Expected non-nil Unit")
				}

				if len(writeStmt.OutputList) != 1 {
					t.Errorf("Expected 1 output, got %d", len(writeStmt.OutputList))
				}
			},
		},

		// ===== PRINT Statements =====
		{
			name: "PRINT with list-directed format",
			src:  "PRINT *, 'Hello World'",
			validate: func(t *testing.T, stmt ast.Statement) {
				printStmt, ok := stmt.(*ast.PrintStmt)
				if !ok {
					t.Fatalf("Expected *ast.PrintStmt, got %T", stmt)
				}

				if printStmt.Format == nil {
					t.Fatal("Expected non-nil Format")
				}

				if len(printStmt.OutputList) != 1 {
					t.Errorf("Expected 1 output item, got %d", len(printStmt.OutputList))
				}
			},
		},
		{
			name: "PRINT with format label and variables",
			src:  "PRINT 100, X, Y, Z",
			validate: func(t *testing.T, stmt ast.Statement) {
				printStmt, ok := stmt.(*ast.PrintStmt)
				if !ok {
					t.Fatalf("Expected *ast.PrintStmt, got %T", stmt)
				}

				if printStmt.Format == nil {
					t.Fatal("Expected non-nil Format")
				}

				if len(printStmt.OutputList) != 3 {
					t.Errorf("Expected 3 output items, got %d", len(printStmt.OutputList))
				}
			},
		},
		{
			name: "PRINT with inline format",
			src:  "PRINT '(I5,F10.2)', N, X",
			validate: func(t *testing.T, stmt ast.Statement) {
				printStmt, ok := stmt.(*ast.PrintStmt)
				if !ok {
					t.Fatalf("Expected *ast.PrintStmt, got %T", stmt)
				}

				if printStmt.Format == nil {
					t.Fatal("Expected non-nil Format")
				}

				if len(printStmt.OutputList) != 2 {
					t.Errorf("Expected 2 output items, got %d", len(printStmt.OutputList))
				}
			},
		},
		{
			name: "PRINT with format only (no output list)",
			src:  "PRINT 10000",
			validate: func(t *testing.T, stmt ast.Statement) {
				printStmt, ok := stmt.(*ast.PrintStmt)
				if !ok {
					t.Fatalf("Expected *ast.PrintStmt, got %T", stmt)
				}

				if printStmt.Format == nil {
					t.Fatal("Expected non-nil Format")
				}

				if len(printStmt.OutputList) != 0 {
					t.Errorf("Expected 0 output items, got %d", len(printStmt.OutputList))
				}
			},
		},

		// ===== OPEN Statements =====
		{
			name: "OPEN with positional unit and FILE",
			src:  "OPEN(10, FILE='data.txt')",
			validate: func(t *testing.T, stmt ast.Statement) {
				openStmt, ok := stmt.(*ast.OpenStmt)
				if !ok {
					t.Fatalf("Expected *ast.OpenStmt, got %T", stmt)
				}

				if len(openStmt.Specifiers) == 0 {
					t.Error("Expected non-empty Specifiers map")
				}

				if openStmt.Specifiers["UNIT"] == nil {
					t.Error("Expected UNIT specifier")
				}

				if openStmt.Specifiers["FILE"] == nil {
					t.Error("Expected FILE specifier")
				}
			},
		},
		{
			name: "OPEN with keyword specifiers",
			src:  "OPEN(UNIT=20, FILE='output.dat', STATUS='NEW', FORM='FORMATTED')",
			validate: func(t *testing.T, stmt ast.Statement) {
				openStmt, ok := stmt.(*ast.OpenStmt)
				if !ok {
					t.Fatalf("Expected *ast.OpenStmt, got %T", stmt)
				}

				requiredSpecs := []string{"UNIT", "FILE", "STATUS", "FORM"}
				for _, spec := range requiredSpecs {
					if openStmt.Specifiers[spec] == nil {
						t.Errorf("Expected %s specifier", spec)
					}
				}
			},
		},
		{
			name: "OPEN with IOSTAT and ERR",
			src:  "OPEN(UNIT=30, FILE=FNAME, IOSTAT=IOS, ERR=999)",
			validate: func(t *testing.T, stmt ast.Statement) {
				openStmt, ok := stmt.(*ast.OpenStmt)
				if !ok {
					t.Fatalf("Expected *ast.OpenStmt, got %T", stmt)
				}

				if openStmt.Specifiers["UNIT"] == nil {
					t.Error("Expected UNIT specifier")
				}

				if openStmt.Specifiers["FILE"] == nil {
					t.Error("Expected FILE specifier")
				}

				if openStmt.Specifiers["IOSTAT"] == nil {
					t.Error("Expected IOSTAT specifier")
				}

				if openStmt.Specifiers["ERR"] == nil {
					t.Error("Expected ERR specifier")
				}
			},
		},
		{
			name: "OPEN with ACCESS and RECL for direct access",
			src:  "OPEN(UNIT=40, FILE='direct.dat', ACCESS='DIRECT', RECL=512)",
			validate: func(t *testing.T, stmt ast.Statement) {
				openStmt, ok := stmt.(*ast.OpenStmt)
				if !ok {
					t.Fatalf("Expected *ast.OpenStmt, got %T", stmt)
				}

				if openStmt.Specifiers["ACCESS"] == nil {
					t.Error("Expected ACCESS specifier")
				}

				if openStmt.Specifiers["RECL"] == nil {
					t.Error("Expected RECL specifier")
				}
			},
		},

		// ===== CLOSE Statements =====
		{
			name: "CLOSE with positional unit",
			src:  "CLOSE(10)",
			validate: func(t *testing.T, stmt ast.Statement) {
				closeStmt, ok := stmt.(*ast.CloseStmt)
				if !ok {
					t.Fatalf("Expected *ast.CloseStmt, got %T", stmt)
				}
				if closeStmt.Specifiers["UNIT"] == nil {
					t.Error("Expected UNIT specifier")
				}
			},
		},
		{
			name: "CLOSE with UNIT and STATUS keywords",
			src:  "CLOSE(UNIT=20, STATUS='KEEP')",
			validate: func(t *testing.T, stmt ast.Statement) {
				closeStmt, ok := stmt.(*ast.CloseStmt)
				if !ok {
					t.Fatalf("Expected *ast.CloseStmt, got %T", stmt)
				}
				if closeStmt.Specifiers["UNIT"] == nil {
					t.Error("Expected UNIT specifier")
				}
				if closeStmt.Specifiers["STATUS"] == nil {
					t.Error("Expected STATUS specifier")
				}
			},
		},

		// ===== BACKSPACE Statements =====
		{
			name: "BACKSPACE with positional unit",
			src:  "BACKSPACE(15)",
			validate: func(t *testing.T, stmt ast.Statement) {
				backspaceStmt, ok := stmt.(*ast.BackspaceStmt)
				if !ok {
					t.Fatalf("Expected *ast.BackspaceStmt, got %T", stmt)
				}
				if backspaceStmt.Specifiers["UNIT"] == nil {
					t.Error("Expected UNIT specifier")
				}
			},
		},
		{
			name: "BACKSPACE with IOSTAT and ERR",
			src:  "BACKSPACE(UNIT=10, IOSTAT=ios, ERR=99)",
			validate: func(t *testing.T, stmt ast.Statement) {
				backspaceStmt, ok := stmt.(*ast.BackspaceStmt)
				if !ok {
					t.Fatalf("Expected *ast.BackspaceStmt, got %T", stmt)
				}
				if backspaceStmt.Specifiers["UNIT"] == nil {
					t.Error("Expected UNIT specifier")
				}
				if backspaceStmt.Specifiers["IOSTAT"] == nil {
					t.Error("Expected IOSTAT specifier")
				}
				if backspaceStmt.Specifiers["ERR"] == nil {
					t.Error("Expected ERR specifier")
				}
			},
		},

		// ===== REWIND Statements =====
		{
			name: "REWIND with positional unit",
			src:  "REWIND(25)",
			validate: func(t *testing.T, stmt ast.Statement) {
				rewindStmt, ok := stmt.(*ast.RewindStmt)
				if !ok {
					t.Fatalf("Expected *ast.RewindStmt, got %T", stmt)
				}
				if rewindStmt.Specifiers["UNIT"] == nil {
					t.Error("Expected UNIT specifier")
				}
			},
		},
		{
			name: "REWIND with IOSTAT",
			src:  "REWIND(UNIT=30, IOSTAT=ierr)",
			validate: func(t *testing.T, stmt ast.Statement) {
				rewindStmt, ok := stmt.(*ast.RewindStmt)
				if !ok {
					t.Fatalf("Expected *ast.RewindStmt, got %T", stmt)
				}
				if rewindStmt.Specifiers["UNIT"] == nil {
					t.Error("Expected UNIT specifier")
				}
				if rewindStmt.Specifiers["IOSTAT"] == nil {
					t.Error("Expected IOSTAT specifier")
				}
			},
		},

		// ===== Substring and chained subscript notation =====
		{
			name: "substring notation with single character",
			src:  "IF(ASAVE(isave)(1:1) .NE. ' ') X = 1",
			validate: func(t *testing.T, stmt ast.Statement) {
				ifStmt, ok := stmt.(*ast.IfStmt)
				if !ok {
					t.Fatalf("Expected *ast.IfStmt, got %T", stmt)
				}

				// The condition should be a binary expression (.NE.)
				binExpr, ok := ifStmt.Condition.(*ast.BinaryExpr)
				if !ok {
					t.Fatalf("Expected condition to be *ast.BinaryExpr, got %T", ifStmt.Condition)
				}

				// Left side should be chained FunctionCall: ASAVE(isave)(1:1)
				chainedCall, ok := binExpr.Left.(*ast.FunctionCall)
				if !ok {
					t.Fatalf("Expected left side to be *ast.FunctionCall, got %T", binExpr.Left)
				}

				// For chained calls, Name should be empty and Args[0] should be the base
				if chainedCall.Name != "" {
					t.Logf("Note: Chained call Name=%q (may be empty for substring)", chainedCall.Name)
				}
			},
		},
		{
			name: "array access with substring",
			src:  "Y = STR(5)(2:4)",
			validate: func(t *testing.T, stmt ast.Statement) {
				assignStmt, ok := stmt.(*ast.AssignmentStmt)
				if !ok {
					t.Fatalf("Expected *ast.AssignmentStmt, got %T", stmt)
				}

				// Value should be a chained FunctionCall
				chainedCall, ok := assignStmt.Value.(*ast.FunctionCall)
				if !ok {
					t.Fatalf("Expected value to be *ast.FunctionCall, got %T", assignStmt.Value)
				}

				// Should have args (the chained structure)
				if len(chainedCall.Args) == 0 {
					t.Error("Expected non-empty Args for chained call")
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
		{
			name: "IF with ELSEIF",
			src: `IF (1) THEN
   ELSEIF (1) THEN
      k=1
   ENDIF`,
			validate: func(t *testing.T, stmt ast.Statement) {
				ifStmt, ok := stmt.(*ast.IfStmt)
				if !ok {
					t.Fatalf("Expected *ast.IfStmt, got %T", stmt)
				}

				// Initial THEN part should be empty
				if len(ifStmt.ThenPart) != 0 {
					t.Errorf("Expected empty ThenPart, got %d statements", len(ifStmt.ThenPart))
				}

				// Should have one ELSEIF part
				if len(ifStmt.ElseIfParts) != 1 {
					t.Fatalf("Expected 1 ELSEIF part, got %d", len(ifStmt.ElseIfParts))
				}

				// ELSEIF part should have one statement
				if len(ifStmt.ElseIfParts[0].ThenPart) != 1 {
					t.Fatalf("Expected 1 statement in ELSEIF ThenPart, got %d", len(ifStmt.ElseIfParts[0].ThenPart))
				}

				// Statement should be an assignment
				_, ok = ifStmt.ElseIfParts[0].ThenPart[0].(*ast.AssignmentStmt)
				if !ok {
					t.Errorf("Expected ELSEIF statement to be *ast.AssignmentStmt, got %T", ifStmt.ElseIfParts[0].ThenPart[0])
				}

				// Should have no ELSE part
				if len(ifStmt.ElsePart) != 0 {
					t.Errorf("Expected empty ElsePart, got %d statements", len(ifStmt.ElsePart))
				}
			},
		},
		{
			name: "IF used as variable name in assignment",
			src:  "IF=0",
			validate: func(t *testing.T, stmt ast.Statement) {
				assignStmt, ok := stmt.(*ast.AssignmentStmt)
				if !ok {
					t.Fatalf("Expected *ast.AssignmentStmt, got %T", stmt)
				}

				// Check LHS is identifier "IF"
				ident, ok := assignStmt.Target.(*ast.Identifier)
				if !ok {
					t.Fatalf("Expected target to be *ast.Identifier, got %T", assignStmt.Target)
				}
				if ident.Value != "IF" {
					t.Errorf("Expected target 'IF', got %q", ident.Value)
				}

				// Check RHS is integer literal 0
				intLit, ok := assignStmt.Value.(*ast.IntegerLiteral)
				if !ok {
					t.Fatalf("Expected value to be *ast.IntegerLiteral, got %T", assignStmt.Value)
				}
				if intLit.Raw != "0" {
					t.Errorf("Expected value '0', got %q", intLit.Raw)
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

		// ===== Line Continuation with Comments =====
		{
			name: "CALL statement with continuation and comment",
			src: `CALL DIRALT(AA, II, &
! comment line
     &           C3)`,
			validate: func(t *testing.T, stmt ast.Statement) {
				callStmt, ok := stmt.(*ast.CallStmt)
				if !ok {
					t.Fatalf("Expected *ast.CallStmt, got %T", stmt)
				}

				if callStmt.Name != "DIRALT" {
					t.Errorf("Expected subroutine name 'DIRALT', got %q", callStmt.Name)
				}

				// Should have 3 arguments: AA, II, C3
				if len(callStmt.Args) != 3 {
					t.Fatalf("Expected 3 arguments, got %d", len(callStmt.Args))
				}

				// Check argument names
				expectedArgs := []string{"AA", "II", "C3"}
				for i, expected := range expectedArgs {
					ident, ok := callStmt.Args[i].(*ast.Identifier)
					if !ok {
						t.Errorf("Expected arg[%d] to be *ast.Identifier, got %T", i, callStmt.Args[i])
					} else if ident.Value != expected {
						t.Errorf("Expected arg[%d] = %q, got %q", i, expected, ident.Value)
					}
				}
			},
		},

		// ===== IMPLICIT Statements =====
		{
			name: "IMPLICIT NONE uppercase",
			src:  "IMPLICIT NONE",
			validate: func(t *testing.T, stmt ast.Statement) {
				implStmt, ok := stmt.(*ast.ImplicitStatement)
				if !ok {
					t.Fatalf("Expected *ast.ImplicitStatement, got %T", stmt)
				}
				if !implStmt.IsNone {
					t.Error("Expected IsNone to be true")
				}
			},
		},
		{
			name: "IMPLICIT NONE lowercase",
			src:  "implicit none",
			validate: func(t *testing.T, stmt ast.Statement) {
				implStmt, ok := stmt.(*ast.ImplicitStatement)
				if !ok {
					t.Fatalf("Expected *ast.ImplicitStatement, got %T", stmt)
				}
				if !implStmt.IsNone {
					t.Error("Expected IsNone to be true")
				}
			},
		},
		{
			name: "IMPLICIT NONE mixed case",
			src:  "ImPlIcIt NoNe",
			validate: func(t *testing.T, stmt ast.Statement) {
				implStmt, ok := stmt.(*ast.ImplicitStatement)
				if !ok {
					t.Fatalf("Expected *ast.ImplicitStatement, got %T", stmt)
				}
				if !implStmt.IsNone {
					t.Error("Expected IsNone to be true")
				}
			},
		},

		// ===== Edge cases for construct-ending keywords =====
		// These tests verify that parsing loops stop at construct-ending keywords
		// and don't consume tokens from parent scopes (bug fix for nested control structures)
		{
			name: "nested IF with WRITE near ENDIF",
			src: `IF(X.GT.0) THEN
	IF(Y.GT.0) THEN
		WRITE(6,*) 'test'
	ENDIF
ENDIF`,
			validate: func(t *testing.T, stmt ast.Statement) {
				ifStmt, ok := stmt.(*ast.IfStmt)
				if !ok {
					t.Fatalf("Expected *ast.IfStmt, got %T", stmt)
				}
				// Verify outer IF has inner IF in THEN part
				if len(ifStmt.ThenPart) != 1 {
					t.Fatalf("Expected 1 statement in outer IF THEN part, got %d", len(ifStmt.ThenPart))
				}
				innerIf, ok := ifStmt.ThenPart[0].(*ast.IfStmt)
				if !ok {
					t.Errorf("Expected inner statement to be *ast.IfStmt, got %T", ifStmt.ThenPart[0])
				}
				// Verify inner IF has WRITE statement
				if len(innerIf.ThenPart) != 1 {
					t.Errorf("Expected 1 statement in inner IF THEN part, got %d", len(innerIf.ThenPart))
				}
			},
		},
		{
			name: "WRITE in ELSE block near ENDIF",
			src: `IF(FLAG) THEN
	X = 1
ELSE
	WRITE(6,*) 'message'
ENDIF`,
			validate: func(t *testing.T, stmt ast.Statement) {
				ifStmt, ok := stmt.(*ast.IfStmt)
				if !ok {
					t.Fatalf("Expected *ast.IfStmt, got %T", stmt)
				}
				// Verify ELSE part has WRITE statement
				if len(ifStmt.ElsePart) != 1 {
					t.Fatalf("Expected 1 statement in ELSE part, got %d", len(ifStmt.ElsePart))
				}
			},
		},
		{
			name: "multiple nested IFs with WRITE statements",
			src: `IF(A) THEN
	IF(B) THEN
		WRITE(6,*) 'b'
	ENDIF
	IF(C) THEN
		WRITE(6,*) 'c'
	ELSE
		WRITE(6,*) 'd'
	ENDIF
ENDIF`,
			validate: func(t *testing.T, stmt ast.Statement) {
				ifStmt, ok := stmt.(*ast.IfStmt)
				if !ok {
					t.Fatalf("Expected *ast.IfStmt, got %T", stmt)
				}
				// Verify outer IF has 2 inner IFs
				if len(ifStmt.ThenPart) != 2 {
					t.Fatalf("Expected 2 statements in outer IF THEN part, got %d", len(ifStmt.ThenPart))
				}
				// Both should be IF statements
				for i, s := range ifStmt.ThenPart {
					if _, ok := s.(*ast.IfStmt); !ok {
						t.Errorf("Expected statement %d to be *ast.IfStmt, got %T", i, s)
					}
				}
			},
		},

		// ===== F77 labeled END DO =====
		{
			name: "F77 labeled END DO",
			src: `DO 3002 IQP=1,10
 3002 END DO`,
			validate: func(t *testing.T, stmt ast.Statement) {
				doLoop, ok := stmt.(*ast.DoLoop)
				if !ok {
					t.Fatalf("Expected *ast.DoLoop, got %T", stmt)
				}
				// Verify loop variable
				if doLoop.Var != "IQP" {
					t.Errorf("Expected loop variable 'IQP', got %q", doLoop.Var)
				}
				// Verify target label was captured
				if doLoop.TargetLabel != "3002" {
					t.Errorf("Expected DO target label '3002', got %q", doLoop.TargetLabel)
				}
				// Verify end label was captured
				if doLoop.EndLabel != "3002" {
					t.Errorf("Expected END label '3002', got %q", doLoop.EndLabel)
				}
			},
		},

		// ===== SELECT CASE =====
		{
			name: "empty SELECT CASE",
			src: `SELECT CASE (N)
END SELECT`,
			validate: func(t *testing.T, stmt ast.Statement) {
				selectStmt, ok := stmt.(*ast.SelectCaseStmt)
				if !ok {
					t.Fatalf("Expected *ast.SelectCaseStmt, got %T", stmt)
				}

				// Verify expression
				ident, ok := selectStmt.Expression.(*ast.Identifier)
				if !ok {
					t.Fatalf("Expected expression to be *ast.Identifier, got %T", selectStmt.Expression)
				}
				if ident.Value != "N" {
					t.Errorf("Expected expression 'N', got %q", ident.Value)
				}

				// Verify no cases
				if len(selectStmt.Cases) != 0 {
					t.Errorf("Expected 0 cases, got %d", len(selectStmt.Cases))
				}
			},
		},
		{
			name: "SELECT CASE with single case",
			src: `SELECT CASE (STATUS)
CASE (1)
  X = 10
END SELECT`,
			validate: func(t *testing.T, stmt ast.Statement) {
				selectStmt, ok := stmt.(*ast.SelectCaseStmt)
				if !ok {
					t.Fatalf("Expected *ast.SelectCaseStmt, got %T", stmt)
				}

				// Verify one case
				if len(selectStmt.Cases) != 1 {
					t.Fatalf("Expected 1 case, got %d", len(selectStmt.Cases))
				}

				// Check case value
				caseClause := selectStmt.Cases[0]
				if len(caseClause.Values) != 1 {
					t.Fatalf("Expected 1 value in case, got %d", len(caseClause.Values))
				}

				// Check case body
				if len(caseClause.Body) != 1 {
					t.Errorf("Expected 1 statement in case body, got %d", len(caseClause.Body))
				}
			},
		},
		{
			name: "SELECT CASE with multiple values in one case",
			src: `SELECT CASE (I)
CASE (1, 2, 3)
  X = 100
END SELECT`,
			validate: func(t *testing.T, stmt ast.Statement) {
				selectStmt, ok := stmt.(*ast.SelectCaseStmt)
				if !ok {
					t.Fatalf("Expected *ast.SelectCaseStmt, got %T", stmt)
				}

				if len(selectStmt.Cases) != 1 {
					t.Fatalf("Expected 1 case, got %d", len(selectStmt.Cases))
				}

				caseClause := selectStmt.Cases[0]
				if len(caseClause.Values) != 3 {
					t.Fatalf("Expected 3 values in case, got %d", len(caseClause.Values))
				}
			},
		},
		{
			name: "SELECT CASE with CASE DEFAULT",
			src: `SELECT CASE (N)
CASE DEFAULT
  X = 0
END SELECT`,
			validate: func(t *testing.T, stmt ast.Statement) {
				selectStmt, ok := stmt.(*ast.SelectCaseStmt)
				if !ok {
					t.Fatalf("Expected *ast.SelectCaseStmt, got %T", stmt)
				}

				if len(selectStmt.Cases) != 1 {
					t.Fatalf("Expected 1 case, got %d", len(selectStmt.Cases))
				}

				caseClause := selectStmt.Cases[0]
				if !caseClause.IsDefault {
					t.Errorf("Expected CASE DEFAULT, got IsDefault=false")
				}

				if len(caseClause.Body) != 1 {
					t.Errorf("Expected 1 statement in default case body, got %d", len(caseClause.Body))
				}
			},
		},
		{
			name: "SELECT CASE with multiple cases",
			src: `SELECT CASE (STATUS)
CASE (1)
  X = 10
CASE (2, 3)
  Y = 20
CASE DEFAULT
  Z = 0
END SELECT`,
			validate: func(t *testing.T, stmt ast.Statement) {
				selectStmt, ok := stmt.(*ast.SelectCaseStmt)
				if !ok {
					t.Fatalf("Expected *ast.SelectCaseStmt, got %T", stmt)
				}

				if len(selectStmt.Cases) != 3 {
					t.Fatalf("Expected 3 cases, got %d", len(selectStmt.Cases))
				}

				// First case: single value
				if len(selectStmt.Cases[0].Values) != 1 {
					t.Errorf("Expected 1 value in first case, got %d", len(selectStmt.Cases[0].Values))
				}

				// Second case: two values
				if len(selectStmt.Cases[1].Values) != 2 {
					t.Errorf("Expected 2 values in second case, got %d", len(selectStmt.Cases[1].Values))
				}

				// Third case: default
				if !selectStmt.Cases[2].IsDefault {
					t.Errorf("Expected third case to be DEFAULT")
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

			helperFatalErrors(t, &parser, "statement:\n"+wrappedSrc)

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
