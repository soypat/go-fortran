// fortranpp preprocesses Fortran source files by expanding INCLUDE statements.
//
// Usage:
//
//	fortranpp [flags] input.f90
//
// Flags:
//
//	-I dir    Add directory to include search path (can be repeated)
//	-o file   Write output to file (default: stdout)
//
// The tool reads a Fortran source file and replaces all INCLUDE statements
// with the contents of the included files. Nested INCLUDEs are handled
// recursively. Circular includes are detected and reported as errors.
package main

import (
	"bufio"
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
	defer delete(includeStack, absPath) // Remove from stack after processing

	file, err := os.Open(filename)
	if err != nil {
		return "", err
	}
	defer file.Close()

	baseDir := filepath.Dir(absPath)
	var result strings.Builder
	scanner := bufio.NewScanner(file)

	lineNum := 0
	for scanner.Scan() {
		lineNum++
		line := scanner.Text()

		if match := includeRe.FindStringSubmatch(line); match != nil {
			includeFile := match[1]
			includePath, err := findIncludeFile(includeFile, baseDir)
			if err != nil {
				return "", fmt.Errorf("%s:%d: %w", filename, lineNum, err)
			}

			// Recursively process the included file
			content, err := processFile(includePath, includeStack)
			if err != nil {
				return "", err
			}
			result.WriteString(content)
		} else {
			result.WriteString(line)
			result.WriteString("\n")
		}
	}

	if err := scanner.Err(); err != nil {
		return "", fmt.Errorf("reading %s: %w", filename, err)
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
