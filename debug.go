package fortran

import (
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"

	f90 "github.com/soypat/go-fortran/ast"
)

func (tg *ToGo) makeErrAtStmt(msg string) error {
	if tg.currentNode == nil {
		return tg.makeErrWithPos(f90.Position{}, msg)
	}
	return tg.makeErr(tg.currentNode, msg)
}

func (tg *ToGo) makeErr(node f90.Node, msg string) error {
	tok := node.AppendTokenLiteral(nil)
	pos := node.SourcePos()
	return tg.makeErrWithPos(pos, fmt.Sprintf("%T %s: %s", node, tok, msg))
}

func (tg *ToGo) makeErrWithPos(pos f90.Position, msg string) error {
	// If source is available, compute line:column
	src := tg.sourceFile
	if tg.source != "" {
		if src == nil {
			fp, err := os.Open(tg.source)
			if err == nil {
				src = fp
				defer fp.Close()
			}
		}
		callStr := debugGetCallStack(2)
		var err error
		var line, col int
		if src != nil {
			var buf [1024]byte
			line, col, _, err = pos.ToLineCol(src, buf[:])
		}
		if err == nil && line > 0 {
			return fmt.Errorf("%s:%d:%d: %s\n%s", tg.source, line, col, msg, callStr)
		}
	}

	return fmt.Errorf("%s @ %d", msg, pos.Start())
}

func (p *Parser90) addErrorWithPos(pos sourcePos, msg string) {
	if p.died {
		msg = "got error with terminated parser: " + msg
	}
	p.errors = append(p.errors, ParserError{
		sp:  pos,
		msg: msg,
	})
}

func (p *Parser90) strToks() string {
	return fmt.Sprintf("%q %s %q %s %q %s", p.current.lit, p.current.tok,
		p.peek.lit, p.peek.tok, p.uberpeek.lit, p.uberpeek.tok)
}

func (p *Parser90) addError(msg string) {
	p.addErrorWithPos(p.sourcePos(), msg)
}

func (p *Parser90) addErrorFatal(msg string, callstackSkip int) {
	if p.died {
		p.addError(msg)
	} else {
		callstack := debugGetCallStack(callstackSkip)
		p.addError("token state: " + p.strToks() + "\n" + callstack + "\nfatal error encountered, terminating run early: " + msg) // Only one unrecoverable message
	}
	p.died = true
}

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

// warn used to signal a very claudish poorly designed branch/function was hit and used.
func warn(msg string) {
	cs := debugGetCallStack(1)
	fmt.Printf("\033[33m%s\n%s\033[0m\n", msg, cs)
}
