// fortranpp preprocesses Fortran source files by expanding INCLUDE statements.
//
// Usage:
//
//	fortranpp [flags] input.f90
//
// Flags:
//
//	-I dir         Add directory to include search path (can be repeated)
//	-o file        Write output to file (default: stdout)
//	-ccomments     Convert Fortran 77 fixed-form column-1 C/c/* comments to ! style
//	-crlf          Normalize \r\n line endings to \n
//
// The tool reads a Fortran source file and replaces all INCLUDE statements
// with the contents of the included files. Nested INCLUDEs are handled
// recursively. Circular includes are detected and reported as errors.
package main

import (
	"bytes"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"regexp"
	"strings"
)

type includePaths []string

func (i *includePaths) String() string {
	return strings.Join(*i, ":")
}

func (i *includePaths) Set(value string) error {
	*i = append(*i, value)
	return nil
}

var (
	flagIncludePaths includePaths
	flagOutput       = flag.String("o", "", "write output to file (default: stdout)")
	flagCComments    = flag.Bool("ccomments", false, "convert Fortran 77 C/c/* in column 1 to ! comments")
	flagCRLF         = flag.Bool("crlf", false, "normalize \\r\\n line endings to \\n")
)

// includeRe matches INCLUDE 'filename' or INCLUDE "filename" (case-insensitive)
var includeRe = regexp.MustCompile(`(?i)^\s*INCLUDE\s+['"]([^'"]+)['"]`)

func main() {
	flag.Var(&flagIncludePaths, "I", "add directory to include search path (can be repeated)")
	flag.Parse()

	if flag.NArg() != 1 {
		fmt.Fprintln(os.Stderr, "usage: fortranpp [flags] input.f90")
		flag.PrintDefaults()
		os.Exit(1)
	}

	inputFile := flag.Arg(0)

	// Process the file
	included := make(map[string]bool)
	output, err := processFile(inputFile, included)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error: %v\n", err)
		os.Exit(1)
	}

	// Write output
	var w io.Writer = os.Stdout
	if *flagOutput != "" {
		f, err := os.Create(*flagOutput)
		if err != nil {
			fmt.Fprintf(os.Stderr, "error creating output file: %v\n", err)
			os.Exit(1)
		}
		defer f.Close()
		w = f
	}

	if _, err := w.Write([]byte(output)); err != nil {
		fmt.Fprintf(os.Stderr, "error writing output: %v\n", err)
		os.Exit(1)
	}
}

// processFile expands INCLUDE statements in a file.
// includeStack tracks the current chain of includes to detect cycles.
func processFile(filename string, includeStack map[string]bool) (string, error) {
	absPath, err := filepath.Abs(filename)
	if err != nil {
		return "", fmt.Errorf("resolving path %s: %w", filename, err)
	}

	// Check for circular includes (A includes B includes A)
	if includeStack[absPath] {
		return "", fmt.Errorf("circular include detected: %s", filename)
	}
	includeStack[absPath] = true
	defer delete(includeStack, absPath)

	data, err := os.ReadFile(filename)
	if err != nil {
		return "", err
	}

	if *flagCRLF {
		data = bytes.ReplaceAll(data, []byte("\r\n"), []byte("\n"))
		data = bytes.ReplaceAll(data, []byte("\r"), []byte("\n"))
	}

	baseDir := filepath.Dir(absPath)
	var result strings.Builder

	// Split on \n; each element may still carry a trailing \r for CRLF files.
	rawLines := bytes.Split(data, []byte("\n"))
	for i, rawLine := range rawLines {
		isLast := i == len(rawLines)-1

		// Detect and strip trailing \r so transforms work on clean content.
		hasCR := len(rawLine) > 0 && rawLine[len(rawLine)-1] == '\r'
		line := rawLine
		if hasCR {
			line = rawLine[:len(rawLine)-1]
		}

		// Convert Fortran 77 fixed-form column-1 comments (C/c/*) to ! style.
		if *flagCComments && len(line) > 0 && (line[0] == 'C' || line[0] == 'c' || line[0] == '*') {
			line = append([]byte{'!'}, line[1:]...)
		}

		lineStr := string(line)

		if match := includeRe.FindStringSubmatch(lineStr); match != nil {
			includeFile := match[1]
			includePath, err := findIncludeFile(includeFile, baseDir)
			if err != nil {
				return "", fmt.Errorf("%s:%d: %w", filename, i+1, err)
			}

			// Recursively process the included file
			content, err := processFile(includePath, includeStack)
			if err != nil {
				return "", err
			}
			result.WriteString(content)
		} else {
			result.WriteString(lineStr)
			if !isLast {
				if hasCR && !*flagCRLF {
					result.WriteString("\r\n")
				} else {
					result.WriteString("\n")
				}
			}
		}
	}

	return result.String(), nil
}

func findIncludeFile(name, baseDir string) (string, error) {
	// First, try relative to the source file's directory
	candidate := filepath.Join(baseDir, name)
	if _, err := os.Stat(candidate); err == nil {
		return candidate, nil
	}

	// Then try each -I path
	for _, dir := range flagIncludePaths {
		candidate := filepath.Join(dir, name)
		if _, err := os.Stat(candidate); err == nil {
			return candidate, nil
		}
	}

	return "", fmt.Errorf("include file not found: %s", name)
}
