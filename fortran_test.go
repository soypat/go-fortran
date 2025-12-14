package fortran

import (
	"embed"
	"fmt"
	"io/fs"
	"regexp"
	"strings"
	"testing"

	"github.com/soypat/go-fortran/ast"
	f90 "github.com/soypat/go-fortran/ast"
	"github.com/soypat/go-fortran/token"
)

//go:embed testdata
var testdatadir embed.FS

func TestData_valid(t *testing.T) {
	entries, err := fs.ReadDir(testdatadir, "testdata")
	if err != nil || len(entries) == 0 {
		t.Fatal(err)
	}
	var parser Parser90
	var tg ToGo
	for _, entry := range entries {
		name := entry.Name()
		if entry.IsDir() || !strings.HasPrefix(name, "valid_") {
			continue
		}
		t.Run(entry.Name(), func(t *testing.T) {
			finishedNormally := false
			defer func() {
				if !finishedNormally {
					t.Log(tg.makeErrAtStmt("panic/t.Fatal at statement"))
				}
			}()
			path := "testdata/" + name
			src, err := fs.ReadFile(testdatadir, path)
			if err != nil {
				t.Fatal(err)
			}
			ssrc := string(src)
			units := testParse(t, &parser, path, ssrc, false)
			tg.Reset()
			allowMultiProg := name == "valid_programs.f90"
			testTranspile(t, &tg, units, path, ssrc, allowMultiProg)
			finishedNormally = true
		})
	}
}

func TestData_invalid(t *testing.T) {
	entries, err := fs.ReadDir(testdatadir, "testdata")
	if err != nil || len(entries) == 0 {
		t.Fatal(err)
	}
	var parser Parser90
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
			ssrc := string(src)
			testParse(t, &parser, srcpath, ssrc, true)

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

func testTranspile(t testing.TB, tg *ToGo, pus []f90.Unit, srcPath string, src string, allowMultiProg bool) {
	tg.SetSource(srcPath, strings.NewReader(src))
	var mainProg *f90.Unit
	for i := range pus {
		unit := &pus[i]
		if unit.Token == token.PROGRAM {
			if mainProg != nil && !allowMultiProg {
				t.Errorf("two main programs detected in %s: %s and %s", srcPath, mainProg.Name, unit.Name)
			}
			mainProg = unit
			continue
		}
		err := tg.RegisterUnits(*unit)
		if err != nil {
			t.Fatal(srcPath, err)
		}
	}
	if mainProg != nil {
		_, err := tg.TransformProgram(*mainProg)
		if err != nil {
			t.Fatal(srcPath, err)
		}
		// TODO: add other routines here.
	} else {
		_, err := tg.TransformUnits(nil, pus...)
		if err != nil {
			t.Fatal(srcPath, err)
		}
	}

}

func testParse(t testing.TB, p *Parser90, srcPath string, src string, expectErrors bool) []f90.Unit {
	expected := map[int]string{}
	if expectErrors {
		expected = expectedErrors(src)
	}
	err := p.Reset(srcPath, strings.NewReader(src))
	if err != nil {
		t.Fatalf("Failed to reset parser: %v", err)
	}
	// Parse all units
	var units []f90.Unit
	for !p.IsDone() {
		unit := p.ParseNextProgramUnit()
		if !unit.IsValid() {
			break
		}
		units = append(units, unit)
	}
	actualErrs := p.Errors()
	if !expectErrors {
		helperFatalErrors(t, p, srcPath)
	}
	// Compare errors
	if err := compareErrors(t, srcPath, expected, actualErrs); err != nil {
		t.Error(err)
	}
	return units
}

// compareErrors compares expected errors (from annotations) with actual parser errors.
// It returns an error describing any mismatches.
func compareErrors(t testing.TB, srcpath string, expected map[int]string, actual []ParserError) error {
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

// helperWantNode asserts that v is of type T and returns it.
// Fails the test with a descriptive message if the type assertion fails.
func helperWantNode[T ast.Node](t testing.TB, v ast.Node, context string) T {
	t.Helper()
	var z T
	vt, ok := v.(T)
	if !ok {
		if context != "" {
			t.Fatalf("%s: want %T, got %T", context, z, v)
		} else {
			t.Fatalf("want %T, got %T", z, v)
		}
	}
	return vt
}
