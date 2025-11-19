package fortran_test

import (
	"os"
	"strings"
	"testing"

	"github.com/soypat/go-fortran"
)

// TestKnownIssue1_ExpressionAfterTypeDecls tests the bug where arithmetic expressions
// in CALL arguments fail to parse when there are type declarations before them.
// See KNOWN_ISSUES.md for details.
func TestKnownIssue1_ExpressionAfterTypeDecls(t *testing.T) {
	t.Skip("Known issue: n-1 in CALL fails after type declarations")

	src := `SUBROUTINE fact(n, result)
	INTEGER :: n
	INTEGER :: result
	CALL fact(n-1, result)
END SUBROUTINE`

	var parser fortran.Parser90
	err := parser.Reset("test.f90", strings.NewReader(src))
	if err != nil {
		t.Fatalf("Reset failed: %v", err)
	}

	unit := parser.ParseNextProgramUnit()
	if unit == nil {
		t.Fatal("ParseNextProgramUnit returned nil")
	}

	if len(parser.Errors()) > 0 {
		t.Errorf("Parse errors (KNOWN BUG):")
		for _, e := range parser.Errors() {
			t.Logf("  %v", e)
		}
		// This is the expected failure - n-1 doesn't parse
	}
}

// TestKnownIssue3_LabeledDOLoops tests Fortran 77 style labeled DO loops
// which are not yet implemented.
func TestKnownIssue3_LabeledDOLoops(t *testing.T) {
	t.Skip("Known issue: F77 labeled DO loops not implemented")

	src := `SUBROUTINE test()
	INTEGER :: i, sum
	sum = 0
	DO 10 i = 1, 10
		sum = sum + i
10  CONTINUE
END SUBROUTINE`

	var parser fortran.Parser90
	err := parser.Reset("test.f90", strings.NewReader(src))
	if err != nil {
		t.Fatalf("Reset failed: %v", err)
	}

	unit := parser.ParseNextProgramUnit()
	if unit == nil {
		t.Fatal("ParseNextProgramUnit returned nil")
	}

	if len(parser.Errors()) > 0 {
		t.Errorf("Parse errors (KNOWN BUG):")
		for _, e := range parser.Errors() {
			t.Logf("  %v", e)
		}
		// Expected failure - labeled DO loops not implemented
	}
}

// TestKnownIssue3b_FormatStatements tests FORMAT statements with labels
func TestKnownIssue3b_FormatStatements(t *testing.T) {
	t.Skip("Known issue: FORMAT statements not implemented")

	src := `SUBROUTINE test()
	INTEGER :: x
	WRITE(*,10) x
10  FORMAT(I5)
END SUBROUTINE`

	var parser fortran.Parser90
	err := parser.Reset("test.f90", strings.NewReader(src))
	if err != nil {
		t.Fatalf("Reset failed: %v", err)
	}

	unit := parser.ParseNextProgramUnit()
	if unit == nil {
		t.Fatal("ParseNextProgramUnit returned nil")
	}

	if len(parser.Errors()) > 0 {
		t.Errorf("Parse errors (KNOWN BUG):")
		for _, e := range parser.Errors() {
			t.Logf("  %v", e)
		}
		// Expected failure - FORMAT statements not implemented
	}
}

// TestKnownIssue_FromTestdata tests the actual failing cases from testdata files
func TestKnownIssue_FromTestdata(t *testing.T) {
	tests := []struct {
		name string
		file string
		skip bool
	}{
		{
			name: "valid_control-subroutine.f90",
			file: "testdata/valid_control-subroutine.f90",
			skip: true, // Known issue: n-1 after type decls
		},
		{
			name: "valid_programs.f90",
			file: "testdata/valid_programs.f90",
			skip: true, // Known issue: continuation lines
		},
		{
			name: "valid_subroutines.f90",
			file: "testdata/valid_subroutines.f90",
			skip: true, // Known issue: labeled statements
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if tt.skip {
				t.Skipf("Known issue - see KNOWN_ISSUES.md")
			}

			f, err := os.Open(tt.file)
			if err != nil {
				t.Fatalf("Failed to open file: %v", err)
			}
			defer f.Close()

			var parser fortran.Parser90
			err = parser.Reset(tt.file, f)
			if err != nil {
				t.Fatalf("Reset failed: %v", err)
			}

			// Try to parse all program units
			for {
				unit := parser.ParseNextProgramUnit()
				if unit == nil {
					break
				}
			}

			if len(parser.Errors()) > 0 {
				t.Errorf("Parse errors:")
				for _, e := range parser.Errors() {
					t.Logf("  %v", e)
				}
			}
		})
	}
}
