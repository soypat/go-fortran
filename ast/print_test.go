package ast

import (
	"bytes"
	"strings"
	"testing"

	"github.com/soypat/go-fortran/token"
)

func TestPrint(t *testing.T) {
	// Create a simple AST node
	prog := &ProgramBlock{
		Name:     "test",
		Position: Pos(0, 100),
		Body: []Statement{
			&ImplicitStatement{IsNone: true, Position: Pos(10, 22)},
		},
	}

	var buf bytes.Buffer
	err := Fprint(&buf, prog, nil)
	if err != nil {
		t.Fatalf("Fprint failed: %v", err)
	}

	output := buf.String()

	// Check that output contains expected fields
	expected := []string{
		"ProgramBlock",
		"Name: \"test\"",
		"Start: 0",
		"End: 100",
		"Body",
	}

	for _, exp := range expected {
		if !strings.Contains(output, exp) {
			t.Errorf("Output missing expected string %q\nGot:\n%s", exp, output)
		}
	}
}

func TestPrintWithFilter(t *testing.T) {
	prog := &ProgramBlock{
		Name:     "test",
		Position: Pos(0, 100),
		Body:     nil, // nil slice
	}

	var buf bytes.Buffer
	err := Fprint(&buf, prog, NotNilFilter)
	if err != nil {
		t.Fatalf("Fprint failed: %v", err)
	}

	output := buf.String()

	// With NotNilFilter, Body should not appear
	if strings.Contains(output, "Body") {
		t.Errorf("Expected Body to be filtered out (nil), but got:\n%s", output)
	}

	// But Name should still appear
	if !strings.Contains(output, "Name: \"test\"") {
		t.Errorf("Expected Name to appear, but got:\n%s", output)
	}
}

func TestPrintSubroutine(t *testing.T) {
	sub := &Subroutine{
		Name: "factorial",
		Parameters: []Parameter{{
			Name: "n",
		}, {
			Name: "result",
		}},
		Attributes: []token.Token{token.RECURSIVE},
		Position:   Pos(0, 50),
	}

	var buf bytes.Buffer
	err := Fprint(&buf, sub, NotNilFilter)
	if err != nil {
		t.Fatalf("Fprint failed: %v", err)
	}

	output := buf.String()

	// Check output contains expected content
	if !strings.Contains(output, "Subroutine") {
		t.Errorf("Expected type name Subroutine in output:\n%s", output)
	}
	if !strings.Contains(output, "factorial") {
		t.Errorf("Expected function name in output:\n%s", output)
	}
	if !strings.Contains(output, "Parameters") {
		t.Errorf("Expected Parameters field in output:\n%s", output)
	}
}

func TestPrintNil(t *testing.T) {
	var buf bytes.Buffer
	err := Fprint(&buf, nil, nil)
	if err != nil {
		t.Fatalf("Fprint failed: %v", err)
	}

	output := buf.String()
	if output != "nil" {
		t.Errorf("Expected 'nil', got %q", output)
	}
}
