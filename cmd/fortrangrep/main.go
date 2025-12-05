// fortrangrep searches Fortran source files with comment awareness.
//
// Usage:
//
//	fortrangrep [flags] pattern file.f90 [file2.f90 ...]
//
// Flags:
//
//	-i          case-insensitive matching
//	-n          show line numbers (default true)
//	-c          exclude comment lines from search
//	-A num      show num lines after match
//	-B num      show num lines before match
//	-C num      show num lines before and after match
//	-l          only print filenames with matches
//	-v          invert match (show non-matching lines)
//	-s num      start line (inclusive, 1-indexed)
//	-e num      end line (inclusive, 1-indexed)
package main

import (
	"bufio"
	"flag"
	"fmt"
	"os"
	"regexp"

	fortran "github.com/soypat/go-fortran"
	"github.com/soypat/go-fortran/token"
)

var (
	flagIgnoreCase    = flag.Bool("i", false, "case-insensitive matching")
	flagLineNumbers   = flag.Bool("n", true, "show line numbers")
	flagNoComments    = flag.Bool("c", false, "exclude comment lines from search")
	flagAfterContext  = flag.Int("A", 0, "show num lines after match")
	flagBeforeContext = flag.Int("B", 0, "show num lines before match")
	flagContext       = flag.Int("C", 0, "show num lines before and after match (overrides -A and -B)")
	flagFilesOnly     = flag.Bool("l", false, "only print filenames with matches")
	flagInvert        = flag.Bool("v", false, "invert match (show non-matching lines)")
	flagStartLine     = flag.Int("s", 0, "start line (inclusive, 1-indexed)")
	flagEndLine       = flag.Int("e", 0, "end line (inclusive, 1-indexed)")
	flagNoSeparators  = flag.Bool("no-sep", false, "suppress -- separators between non-contiguous matches")
)

func main() {
	flag.Parse()
	if flag.NArg() < 2 {
		fmt.Fprintln(os.Stderr, "usage: fortrangrep [flags] pattern file.f90 [file2.f90 ...]")
		flag.PrintDefaults()
		os.Exit(1)
	}

	pattern := flag.Arg(0)
	if *flagIgnoreCase {
		pattern = "(?i)" + pattern
	}

	re, err := regexp.Compile(pattern)
	if err != nil {
		fmt.Fprintf(os.Stderr, "invalid pattern: %v\n", err)
		os.Exit(1)
	}

	// Handle -C flag
	beforeCtx := *flagBeforeContext
	afterCtx := *flagAfterContext
	if *flagContext > 0 {
		beforeCtx = *flagContext
		afterCtx = *flagContext
	}

	files := flag.Args()[1:]
	multipleFiles := len(files) > 1
	exitCode := 1

	for _, filename := range files {
		matched, err := searchFile(filename, re, beforeCtx, afterCtx, multipleFiles)
		if err != nil {
			fmt.Fprintf(os.Stderr, "error reading %s: %v\n", filename, err)
			continue
		}
		if matched {
			exitCode = 0
		}
	}
	os.Exit(exitCode)
}

// lineInfo holds information about a single line
type lineInfo struct {
	num         int
	text        string
	commentOnly bool // true if line contains only comment (and whitespace)
}

// readLinesWithCommentInfo reads a file and uses the lexer to determine which lines are comment-only
func readLinesWithCommentInfo(filename string) ([]lineInfo, error) {
	// First pass: read raw lines
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var rawLines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		rawLines = append(rawLines, scanner.Text())
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}

	// Initialize result with raw lines
	lines := make([]lineInfo, len(rawLines))
	for i, text := range rawLines {
		lines[i] = lineInfo{
			num:         i + 1,
			text:        text,
			commentOnly: false,
		}
	}

	// Second pass: use lexer to identify comment lines
	file2, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file2.Close()

	var lexer fortran.Lexer90
	if err := lexer.Reset(filename, file2); err != nil {
		return nil, err
	}

	// Track tokens per line
	lineHasCode := make(map[int]bool)    // line has non-comment, non-newline tokens
	lineHasComment := make(map[int]bool) // line has comment token

	for !lexer.IsDone() {
		tok, _, _ := lexer.NextToken()
		if tok == token.EOF || tok == token.Illegal {
			break
		}

		line, _ := lexer.TokenLineCol()
		if line < 1 || line > len(lines) {
			continue
		}

		switch tok {
		case token.LineComment:
			lineHasComment[line] = true
		case token.NewLine:
			// Don't count newlines as code
		default:
			lineHasCode[line] = true
		}
	}

	// Mark comment-only lines
	for i := range lines {
		lineNum := i + 1
		if lineHasComment[lineNum] && !lineHasCode[lineNum] {
			lines[i].commentOnly = true
		}
	}

	return lines, nil
}

func searchFile(filename string, re *regexp.Regexp, beforeCtx, afterCtx int, showFilename bool) (bool, error) {
	lines, err := readLinesWithCommentInfo(filename)
	if err != nil {
		return false, err
	}

	// Find matches
	type match struct {
		lineIdx int
	}
	var matches []match

	for i, ln := range lines {
		// Skip lines outside the line range
		if *flagStartLine > 0 && ln.num < *flagStartLine {
			continue
		}
		if *flagEndLine > 0 && ln.num > *flagEndLine {
			continue
		}
		// Skip comment lines if -c flag is set
		if *flagNoComments && ln.commentOnly {
			continue
		}

		found := re.MatchString(ln.text)
		if *flagInvert {
			found = !found
		}
		if found {
			matches = append(matches, match{lineIdx: i})
		}
	}

	if len(matches) == 0 {
		return false, nil
	}

	// For -l flag, just print filename
	if *flagFilesOnly {
		fmt.Println(filename)
		return true, nil
	}

	// Build set of lines to print (handling context)
	printLines := make(map[int]bool)
	contextLines := make(map[int]bool) // lines that are context, not matches

	for _, m := range matches {
		printLines[m.lineIdx] = true

		// Add before context
		for j := 1; j <= beforeCtx && m.lineIdx-j >= 0; j++ {
			idx := m.lineIdx - j
			if !printLines[idx] {
				contextLines[idx] = true
			}
			printLines[idx] = true
		}

		// Add after context
		for j := 1; j <= afterCtx && m.lineIdx+j < len(lines); j++ {
			idx := m.lineIdx + j
			if !printLines[idx] {
				contextLines[idx] = true
			}
			printLines[idx] = true
		}
	}

	// Print results in order
	lastPrinted := -2
	for i := 0; i < len(lines); i++ {
		if !printLines[i] {
			continue
		}

		// Print separator if there's a gap
		if lastPrinted >= 0 && i > lastPrinted+1 {
			// Check if gap is only due to comment-only lines (when -c is set)
			gapIsOnlyComments := *flagNoComments
			if gapIsOnlyComments {
				for j := lastPrinted + 1; j < i; j++ {
					if !lines[j].commentOnly {
						gapIsOnlyComments = false
						break
					}
				}
			}
			// Print separator unless gap is only due to skipped comments
			if !gapIsOnlyComments && !*flagNoSeparators {
				fmt.Println("--")
			}
		}
		lastPrinted = i

		ln := lines[i]
		prefix := ""
		if showFilename {
			prefix = filename + ":"
		}

		separator := ":"
		if contextLines[i] {
			separator = "-"
		}

		if *flagLineNumbers {
			fmt.Printf("%s%d%s%s\n", prefix, ln.num, separator, ln.text)
		} else {
			fmt.Printf("%s%s\n", prefix, ln.text)
		}
	}

	return true, nil
}
