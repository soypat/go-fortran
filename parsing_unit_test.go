package fortran

import (
	"strings"
	"testing"

	"github.com/soypat/go-fortran/ast"
)

// TestProgramUnitParsing verifies that program units are parsed correctly,
// including proper variable registration in the parser's symbol table.
func TestProgramUnitParsing(t *testing.T) {
	tests := []struct {
		name     string
		src      string
		validate func(t *testing.T, unit ast.ProgramUnit, data *ParserUnitData)
	}{
		{
			name: "standalone DIMENSION statement registers array variable",
			src: `SUBROUTINE test(ICENTR)
      PARAMETER (NUMGRP=4)
      DIMENSION IPTBEG(NUMGRP)
      DATA IPTBEG/1, 9,11,19/
      IBEG=IPTBEG(ICENTR)
END SUBROUTINE`,
			validate: func(t *testing.T, unit ast.ProgramUnit, data *ParserUnitData) {
				sub, ok := unit.(*ast.Subroutine)
				if !ok {
					t.Fatalf("Expected *ast.Subroutine, got %T", unit)
				}

				// CRITICAL: Verify IPTBEG is registered as an array variable
				vi := data.Var("IPTBEG")
				if vi == nil {
					t.Fatalf("BUG: IPTBEG not registered in variable table after DIMENSION statement")
				}

				// Verify it has dimension information
				if vi.Dimensions() == nil {
					t.Errorf("BUG: IPTBEG registered but has no dimension info")
				}

				// Verify VFlagDimension is set
				if !vi.Flags().HasAny(VFlagDimension) {
					t.Errorf("BUG: IPTBEG missing VFlagDimension flag")
				}

				// Find the assignment statement: IBEG=IPTBEG(ICENTR)
				var assignStmt *ast.AssignmentStmt
				for _, stmt := range sub.Body {
					if assign, ok := stmt.(*ast.AssignmentStmt); ok {
						assignStmt = assign
						break
					}
				}
				if assignStmt == nil {
					t.Fatal("Expected assignment statement")
				}

				// CRITICAL: IPTBEG(ICENTR) must be parsed as ArrayRef, not FunctionCall
				arrayRef, ok := assignStmt.Value.(*ast.ArrayRef)
				if !ok {
					t.Errorf("BUG: IPTBEG(ICENTR) parsed as %T, should be *ast.ArrayRef", assignStmt.Value)
					if funcCall, isFuncCall := assignStmt.Value.(*ast.FunctionCall); isFuncCall {
						t.Errorf("Incorrectly parsed as FunctionCall with name=%q", funcCall.Name)
					}
					t.Errorf("This happens because parseDimensionStmt() doesn't call varInit()")
					return
				}

				// Verify it's the right array
				if arrayRef.Name != "IPTBEG" {
					t.Errorf("Expected ArrayRef name 'IPTBEG', got %q", arrayRef.Name)
				}

				// Verify it has subscripts
				if len(arrayRef.Subscripts) != 1 {
					t.Errorf("Expected 1 subscript, got %d", len(arrayRef.Subscripts))
				}
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var parser Parser90
			err := parser.Reset(tt.name+".f90", strings.NewReader(tt.src))
			if err != nil {
				t.Fatalf("Reset failed: %v", err)
			}

			unit := parser.ParseNextProgramUnit()
			if unit == nil {
				t.Fatal("ParseNextProgramUnit returned nil")
			}

			helperFatalErrors(t, &parser, "source:\n"+tt.src)

			// Get the ParserUnitData to check registered variables
			data, ok := unit.UnitData().(*ParserUnitData)
			if !ok {
				t.Fatalf("Expected *ParserUnitData, got %T", unit.UnitData())
			}

			tt.validate(t, unit, data)
		})
	}
}
