package fortran

import (
	"embed"
	"fmt"
	"io/fs"
	"regexp"
	"strings"
	"testing"

	"github.com/soypat/go-fortran/ast"
)

//go:embed testdata
var testdatadir embed.FS

func TestData_valid(t *testing.T) {
	entries, err := fs.ReadDir(testdatadir, "testdata")
	if err != nil || len(entries) == 0 {
		t.Fatal(err)
	}
	for _, entry := range entries {
		name := entry.Name()
		if entry.IsDir() || !strings.HasPrefix(name, "valid_") {
			continue
		}
		t.Run(entry.Name(), func(t *testing.T) {
			path := "testdata/" + name
			src, err := fs.ReadFile(testdatadir, path)
			if err != nil {
				t.Fatal(err)
			}
			checkErrors(t, path, string(src), false)
		})
	}
}

func TestData_invalid(t *testing.T) {
	entries, err := fs.ReadDir(testdatadir, "testdata")
	if err != nil || len(entries) == 0 {
		t.Fatal(err)
	}
	for _, entry := range entries {
		name := entry.Name()
		if entry.IsDir() || !strings.HasPrefix(name, "invalid_") {
			continue
		}
		t.Run(entry.Name(), func(t *testing.T) {
			srcpath := "testdata/" + name
			src, err := fs.ReadFile(testdatadir, srcpath)
			if err != nil {
				t.Fatal(err)
			}
			checkErrors(t, srcpath, string(src), true)
		})
	}
}

var errCommentRx = regexp.MustCompile(`!\s*ERROR\s+"([^"]*)"`)

// expectedErrors scans the source for error annotations and returns
// a map of line numbers to expected error patterns (as regexes).
func expectedErrors(src string) map[int]string {
	errors := make(map[int]string)
	lines := strings.Split(src, "\n")

	for lineNum, line := range lines {
		if m := errCommentRx.FindStringSubmatch(line); len(m) == 2 {
			// Line numbers are 1-based
			errors[lineNum+1] = m[1]
		}
	}
	return errors
}

// checkErrors is a test helper that parses source code and verifies errors match annotations.
// If expectErrors is false, it verifies that no errors occurred.
func checkErrors(t *testing.T, srcpath, src string, expectErrors bool) {
	t.Helper()

	expected := map[int]string{}
	if expectErrors {
		expected = expectedErrors(src)
	}

	parser := Parser90{}
	err := parser.Reset(srcpath, strings.NewReader(src))
	if err != nil {
		t.Fatalf("Failed to reset parser: %v", err)
	}

	// Parse all units
	for {
		unit := parser.ParseNextProgramUnit()
		if unit == nil {
			break
		}
	}

	actual := parser.Errors()

	// Compare errors
	if err := compareErrors(t, srcpath, expected, actual); err != nil {
		t.Error(err)
	}
}

// compareErrors compares expected errors (from annotations) with actual parser errors.
// It returns an error describing any mismatches.
func compareErrors(t *testing.T, srcpath string, expected map[int]string, actual []ParserError) error {
	t.Helper()
	actualAreExpected := make([]bool, len(actual))
	for line, pattern := range expected {
		sp := sourcePos{
			Source: srcpath,
			Line:   line,
		}
		rx, err := regexp.Compile(pattern)
		if err != nil {
			return fmt.Errorf("%s: invalid regex pattern %q: %v", sp.String(), pattern, err)
		}
		// Check error is contained.
		matched := false
		lineErrFound := ""
		for i := range actual {
			if actual[i].sp.Line == line {
				lineErrFound = actual[i].msg
				if rx.MatchString(actual[i].msg) {
					matched = true
					actualAreExpected[i] = true
					break
				}
			}
		}
		if lineErrFound == "" {
			return fmt.Errorf("%s: expected error matching %q, but no error found", sp.String(), pattern)

		}
		if !matched {
			return fmt.Errorf("%s: expected error matching %q, but got: %v", sp.String(), pattern, lineErrFound)
		}
	}
	var err error
	for i, isExpected := range actualAreExpected {
		if !isExpected {
			err = &actual[i]
			t.Errorf("unexpected error: %v", err)
		}
	}
	return nil
}

func newParser(t *testing.T, code string) *Parser90 {
	p := &Parser90{}
	err := p.Reset("test.f90", strings.NewReader(code))
	if err != nil {
		t.Fatal(err)
	}
	return p
}

// TestArrayRefVsFunctionCall verifies that declared arrays produce ArrayRef nodes
// while undeclared identifiers produce FunctionCall nodes.
func TestArrayRefVsFunctionCall(t *testing.T) {
	src := `SUBROUTINE test()
  INTEGER, DIMENSION(10) :: arr
  INTEGER :: x
  x = arr(5)
  x = UNKNOWN_FUNC(5)
END SUBROUTINE`

	p := newParser(t, src)
	unit := p.ParseNextProgramUnit()
	if unit == nil {
		t.Fatal("expected program unit")
	}
	sub, ok := unit.(*ast.Subroutine)
	if !ok {
		t.Fatalf("expected Subroutine, got %T", unit)
	}

	// Find the assignment statements
	var assignStmts []*ast.AssignmentStmt
	for _, stmt := range sub.Body {
		if assign, ok := stmt.(*ast.AssignmentStmt); ok {
			assignStmts = append(assignStmts, assign)
		}
	}
	if len(assignStmts) != 2 {
		t.Fatalf("expected 2 assignment statements, got %d", len(assignStmts))
	}

	// First assignment: x = arr(5) - arr should be ArrayRef
	arrRef, ok := assignStmts[0].Value.(*ast.ArrayRef)
	if !ok {
		t.Errorf("expected arr(5) to be *ast.ArrayRef, got %T", assignStmts[0].Value)
	} else if arrRef.Name != "arr" {
		t.Errorf("expected ArrayRef name 'arr', got %s", arrRef.Name)
	}

	// Second assignment: x = UNKNOWN_FUNC(5) - should be FunctionCall
	funcCall, ok := assignStmts[1].Value.(*ast.FunctionCall)
	if !ok {
		t.Errorf("expected UNKNOWN_FUNC(5) to be *ast.FunctionCall, got %T", assignStmts[1].Value)
	} else if funcCall.Name != "UNKNOWN_FUNC" {
		t.Errorf("expected FunctionCall name 'UNKNOWN_FUNC', got %s", funcCall.Name)
	}
}
