package fortran

import (
	"fmt"
	"path/filepath"
	"runtime"
	"strings"
)

// debugGetCallStack returns a formatted string of the current call stack
// Format: "filename:line in TypeName.FunctionName"
// Example: "parser.go:592 in Parser90.parseExecutableStatement"
func debugGetCallStack(skipAdditional int) string {
	var result strings.Builder

	// Get program counters for up to 32 frames
	pcs := make([]uintptr, 32)
	n := runtime.Callers(2+skipAdditional, pcs) // Skip getCallStack and its caller

	if n == 0 {
		return ""
	}

	pcs = pcs[:n]
	frames := runtime.CallersFrames(pcs)

	first := true
	for {
		frame, more := frames.Next()
		if strings.Contains(frame.File, "/go/") {
			// Skip standard library files and test files.
			break
		}

		// filename := frame.File
		// Extract just the filename from the full path
		filename := filepath.Base(frame.File)

		// runtime.Frame.Function is "pkg/path.(*Type).Method" or "pkg/path.Function".
		funcName := frame.Function
		if pkg, method, ok := stringsꞏCutLast(funcName, "."); ok {
			funcName = method
			if _, typeSeg, ok2 := stringsꞏCutLast(pkg, "."); ok2 && strings.Contains(typeSeg, "(") {
				typeSeg = strings.TrimPrefix(typeSeg, "(*")
				typeSeg = strings.TrimSuffix(typeSeg, ")")
				funcName = typeSeg + "." + method
			}
		}

		if !first {
			result.WriteString("\n")
		}
		first = false

		fmt.Fprintf(&result, "%s:%d @%s", filename, frame.Line, funcName)

		if !more {
			break
		}
	}

	return result.String()
}

// CutLast slices s around the last instance of sep,
// returning the text before and after sep.
// The found result reports whether sep appears in s.
// If sep does not appear in s, CutLast returns s, "", false.
func stringsꞏCutLast(s, sep string) (before, after string, found bool) {
	if i := strings.LastIndex(s, sep); i >= 0 {
		return s[:i], s[i+len(sep):], true
	}
	return s, "", false
}
