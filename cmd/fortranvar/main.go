// fortranvar prints variable information from Fortran source files.
//
// Usage:
//
//	fortranvar [flags] file.f90 [file2.f90 ...]
//
// Output format:
//
//	UNIT(name) SCOPE(TYPE:varname[dims]): decl=file:line:col [flags]
//
// Example output:
//
//	PROG(main) LOCAL(INTEGER:x): decl=main.f90:4:3
//	PROG(main) LOCAL(INTEGER:arr(10)): decl=main.f90:5:3 ALLOCATABLE
//	PROG(main) COMMON/blk/(REAL:y): decl=main.f90:6:3
package main

import (
	"flag"
	"fmt"
	"os"
	"strings"

	fortran "github.com/soypat/go-fortran"
	"github.com/soypat/go-fortran/ast"
)

var (
	flagVerbose = flag.Bool("v", false, "verbose output (show all flags)")
	flagFilter  = flag.String("filter", "", "filter variables by name (case-insensitive substring)")
	flagType    = flag.String("type", "", "filter by type (INTEGER, REAL, CHARACTER, etc.)")
)

func main() {
	flag.Parse()
	if flag.NArg() == 0 {
		fmt.Fprintln(os.Stderr, "usage: fortranvar [flags] file.f90 [file2.f90 ...]")
		flag.PrintDefaults()
		os.Exit(1)
	}

	for _, filename := range flag.Args() {
		if err := processFile(filename); err != nil {
			fmt.Fprintf(os.Stderr, "error processing %s: %v\n", filename, err)
			os.Exit(1)
		}
	}
}

func processFile(filename string) error {
	file, err := os.Open(filename)
	if err != nil {
		return err
	}
	defer file.Close()

	var parser fortran.Parser90
	if err := parser.Reset(filename, file); err != nil {
		return err
	}

	for {
		unit := parser.ParseNextProgramUnit()
		if unit == nil {
			break
		}
		printUnitVars(unit)
	}

	if errs := parser.Errors(); len(errs) > 0 {
		for _, e := range errs {
			fmt.Fprintf(os.Stderr, "parse error: %s\n", e)
		}
	}
	return nil
}

func printUnitVars(unit ast.ProgramUnit) {
	printUnitVarsRecursive(unit)
}

func printUnitVarsRecursive(unit ast.ProgramUnit) {
	data := unit.UnitData()
	pud, ok := data.(*fortran.ParserUnitData)
	if ok && pud != nil {
		unitName := unit.UnitName()
		unitKind := unitKindString(unit)

		vars := pud.AppendVarinfo(nil)
		for _, vi := range vars {
			name := vi.Identifier()

			// Apply filters
			if *flagFilter != "" && !strings.Contains(strings.ToUpper(name), strings.ToUpper(*flagFilter)) {
				continue
			}
			typeTok := vi.TypeToken()
			if *flagType != "" && !strings.EqualFold(typeTok.String(), *flagType) {
				continue
			}

			// Build type string with dimensions
			typeStr := formatType(&vi)

			// Determine scope (LOCAL, COMMON, PARAM)
			scope := scopeString(&vi)

			// Get declaration position
			source, line, col := vi.DeclPos()
			declStr := fmt.Sprintf("%s:%d:%d", source, line, col)

			// Build flags string
			flagsStr := formatFlags(&vi)

			// Print the line
			fmt.Printf("%s(%s) %s(%s): decl=%s%s\n",
				unitKind, unitName, scope, typeStr, declStr, flagsStr)
		}
	}

	// Process contained procedures
	switch u := unit.(type) {
	case *ast.ProgramBlock:
		for _, contained := range u.Contains {
			printUnitVarsRecursive(contained)
		}
	case *ast.Module:
		for _, contained := range u.Contains {
			printUnitVarsRecursive(contained)
		}
	}
}

func unitKindString(unit ast.ProgramUnit) string {
	switch unit.(type) {
	case *ast.ProgramBlock:
		return "PROG"
	case *ast.Subroutine:
		return "SUB"
	case *ast.Function:
		return "FUNC"
	case *ast.Module:
		return "MOD"
	case *ast.BlockData:
		return "BDATA"
	default:
		return "UNIT"
	}
}

func scopeString(vi *fortran.Varinfo) string {
	if cb := vi.CommonBlock(); cb != "" {
		if cb == " " || cb == "" {
			return "COMMON//" // blank common
		}
		return fmt.Sprintf("COMMON/%s/", cb)
	}
	if vi.IsParameter() {
		return "PARAM"
	}
	return "LOCAL"
}

func formatType(vi *fortran.Varinfo) string {
	typeTok := vi.TypeToken()
	typeStr := typeTok.String()
	name := vi.Identifier()

	// Add dimensions if array
	dims := vi.Dimensions()
	if dims != nil && len(dims.Bounds) > 0 {
		dimStrs := make([]string, len(dims.Bounds))
		for i, b := range dims.Bounds {
			dimStrs[i] = formatBound(b)
		}
		return fmt.Sprintf("%s:%s(%s)", typeStr, name, strings.Join(dimStrs, ","))
	}

	return fmt.Sprintf("%s:%s", typeStr, name)
}

func formatBound(b ast.ArrayBound) string {
	if b.Lower == nil && b.Upper == nil {
		return ":"
	}
	if b.Lower == nil {
		return fmt.Sprintf(":%s", exprString(b.Upper))
	}
	if b.Upper == nil {
		return fmt.Sprintf("%s:", exprString(b.Lower))
	}
	lower := exprString(b.Lower)
	upper := exprString(b.Upper)
	if lower == "1" {
		return upper
	}
	return fmt.Sprintf("%s:%s", lower, upper)
}

func exprString(e ast.Expression) string {
	if e == nil {
		return "?"
	}
	switch v := e.(type) {
	case *ast.IntegerLiteral:
		return fmt.Sprintf("%d", v.Value)
	case *ast.Identifier:
		return v.Value
	case *ast.BinaryExpr:
		return fmt.Sprintf("%s%s%s", exprString(v.Left), v.Op.String(), exprString(v.Right))
	default:
		// Fallback: use AppendString if available
		buf := e.AppendString(nil)
		return string(buf)
	}
}

func formatFlags(vi *fortran.Varinfo) string {
	flags := vi.Flags()
	if !*flagVerbose && flags == 0 {
		return ""
	}

	var parts []string

	if flags.HasAny(fortran.VFlagAllocatable) {
		parts = append(parts, "ALLOCATABLE")
	}
	if flags.HasAny(fortran.VFlagPointer) {
		parts = append(parts, "POINTER")
	}
	if flags.HasAny(fortran.VFlagTarget) {
		parts = append(parts, "TARGET")
	}
	if flags.HasAny(fortran.VFlagPointee) {
		parts = append(parts, "POINTEE")
	}
	if flags.HasAny(fortran.VFlagEquivalenced) {
		parts = append(parts, "EQUIVALENCED")
	}
	if flags.HasAny(fortran.VFlagIntentIn) {
		parts = append(parts, "INTENT(IN)")
	}
	if flags.HasAny(fortran.VFlagIntentOut) {
		parts = append(parts, "INTENT(OUT)")
	}
	if *flagVerbose {
		if flags.HasAny(fortran.VFlagImplicit) {
			parts = append(parts, "IMPLICIT")
		}
		if flags.HasAny(fortran.VFlagDimension) {
			parts = append(parts, "DIMENSION")
		}
	}

	if len(parts) == 0 {
		return ""
	}
	return " " + strings.Join(parts, " ")
}
