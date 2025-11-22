package ast

import (
	"fmt"
	"io"
	"os"
	"reflect"
	"strings"
)

// A FieldFilter is used to filter fields when printing AST nodes.
// If it returns false, the field is excluded from the output.
type FieldFilter func(name string, value reflect.Value) bool

// NotNilFilter returns true for all fields that are not nil or zero-value.
// This is useful for excluding nil pointers, slices, maps, and false bools from the output.
func NotNilFilter(_ string, v reflect.Value) bool {
	switch v.Kind() {
	case reflect.Chan, reflect.Func, reflect.Interface, reflect.Map, reflect.Pointer, reflect.Slice:
		return !v.IsNil()
	case reflect.Bool:
		// Skip false bools (zero value)
		return v.Bool()
	}
	return true
}

// Fprint prints the AST node x to w in an indented tree format.
// If a non-nil FieldFilter f is provided, only fields for which f returns true are printed.
// Fprint is useful for debugging and testing.
func Fprint(w io.Writer, x any, f FieldFilter) error {
	p := &printer{
		output: w,
		filter: f,
		ptrmap: make(map[any]int),
		indent: 0,
	}
	return p.print(reflect.ValueOf(x))
}

// Print calls Fprint(os.Stdout, x, NotNilFilter) for debugging convenience.
func Print(x any) error {
	return Fprint(os.Stdout, x, NotNilFilter)
}

type printer struct {
	output     io.Writer
	filter     FieldFilter
	ptrmap     map[any]int
	indent     int
	lastWasNil bool
}

func (p *printer) printf(format string, args ...any) {
	fmt.Fprintf(p.output, format, args...)
}

func (p *printer) print(v reflect.Value) error {
	// Handle invalid values
	if !v.IsValid() {
		p.printf("nil")
		return nil
	}

	// Dereference interface values
	if v.Kind() == reflect.Interface && !v.IsNil() {
		v = v.Elem()
	}

	// Handle pointers
	if v.Kind() == reflect.Pointer {
		if v.IsNil() {
			p.printf("nil")
			p.lastWasNil = true
			return nil
		}

		// Check for cycles
		ptr := v.Interface()
		if line, exists := p.ptrmap[ptr]; exists {
			p.printf("(obj @ %d)", line)
			return nil
		}

		// Record this pointer
		p.ptrmap[ptr] = len(p.ptrmap)

		// Dereference and continue
		v = v.Elem()
	}

	// Get type information
	t := v.Type()

	// Special handling for Position type
	if t.Name() == "Position" && t.PkgPath() == "github.com/soypat/go-fortran/ast" {
		// Position has unexported fields, so we need to call the methods
		if posVal, ok := v.Interface().(Position); ok {
			p.printf("Position {\n")
			p.indent++
			p.printIndent()
			p.printf("Start: %d\n", posVal.Start())
			p.printIndent()
			p.printf("End: %d\n", posVal.End())
			p.indent--
			p.printIndent()
			p.printf("}")
			return nil
		}
	}

	// Print based on kind
	switch v.Kind() {
	case reflect.Struct:
		p.printf("%s {", t.Name())
		if v.NumField() > 0 {
			p.printf("\n")
			p.indent++
			for i := 0; i < v.NumField(); i++ {
				field := t.Field(i)
				fv := v.Field(i)

				// Skip unexported fields
				if !field.IsExported() {
					continue
				}

				// Apply filter
				if p.filter != nil && !p.filter(field.Name, fv) {
					continue
				}

				// Print field
				p.printIndent()
				p.printf("%s: ", field.Name)
				p.print(fv)
				p.printf("\n")
			}
			p.indent--
			p.printIndent()
		}
		p.printf("}")

	case reflect.Slice:
		if v.IsNil() {
			p.printf("nil")
			p.lastWasNil = true
			return nil
		}

		p.printf("%s (len=%d) [", t.Elem().String(), v.Len())
		if v.Len() > 0 {
			p.printf("\n")
			p.indent++
			for i := 0; i < v.Len(); i++ {
				p.printIndent()
				p.printf("%d: ", i)
				p.print(v.Index(i))
				p.printf("\n")
			}
			p.indent--
			p.printIndent()
		}
		p.printf("]")

	case reflect.String:
		s := v.String()
		// Escape special characters
		s = strings.ReplaceAll(s, "\n", "\\n")
		s = strings.ReplaceAll(s, "\t", "\\t")
		p.printf("%q", s)

	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		p.printf("%d", v.Int())

	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		p.printf("%d", v.Uint())

	case reflect.Float32, reflect.Float64:
		p.printf("%g", v.Float())

	case reflect.Bool:
		p.printf("%t", v.Bool())

	case reflect.Array:
		p.printf("%s [", t.String())
		if v.Len() > 0 {
			p.printf("\n")
			p.indent++
			for i := 0; i < v.Len(); i++ {
				p.printIndent()
				p.printf("%d: ", i)
				p.print(v.Index(i))
				p.printf("\n")
			}
			p.indent--
			p.printIndent()
		}
		p.printf("]")

	default:
		// For other types, use default formatting
		p.printf("%v", v.Interface())
	}

	return nil
}

func (p *printer) printIndent() {
	for i := 0; i < p.indent; i++ {
		p.printf("  ")
	}
}
