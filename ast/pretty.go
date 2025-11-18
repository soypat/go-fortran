package ast

import (
	"bytes"
	"fmt"
)

// PrettyPrint generates a formatted Fortran source code string from an AST node.
func PrettyPrint(node Node) string {
	var buf bytes.Buffer
	pp(&buf, node, 0)
	return buf.String()
}

func pp(buf *bytes.Buffer, node Node, indent int) {
	if node == nil {
		return
	}

	switch n := node.(type) {
	case *Program:
		for i, unit := range n.Units {
			if i > 0 {
				buf.WriteString("\n\n")
			}
			pp(buf, unit, indent)
		}
	case *Subroutine:
		writeIndent(buf, indent)
		buf.WriteString("SUBROUTINE ")
		buf.WriteString(n.Name)
		if len(n.Parameters) > 0 {
			buf.WriteString("(")
			for i, p := range n.Parameters {
				if i > 0 {
					buf.WriteString(", ")
				}
				buf.WriteString(p.Name)
			}
			buf.WriteString(")")
		}
		buf.WriteString("\n")
		for _, stmt := range n.Body {
			pp(buf, stmt, indent+1)
		}
		writeIndent(buf, indent)
		buf.WriteString("END SUBROUTINE ")
		buf.WriteString(n.Name)
		buf.WriteString("\n")
	// Add cases for other ProgramUnit types (Function, Module, etc.) here

	case *TypeDeclaration:
		writeIndent(buf, indent)
		buf.WriteString(n.TypeSpec)
		if len(n.Attributes) > 0 {
			for _, attr := range n.Attributes {
				buf.WriteString(", ")
				buf.WriteString(attr.String())
			}
		}
		buf.WriteString(" :: ")
		for i, entity := range n.Entities {
			if i > 0 {
				buf.WriteString(", ")
			}
			buf.WriteString(entity.Name)
			// TODO: Add array spec, char len, and initializer
		}
		buf.WriteString("\n")

	case *AssignmentStmt:
		writeIndent(buf, indent)
		pp(buf, n.Target, 0)
		buf.WriteString(" = ")
		pp(buf, n.Value, 0)
		buf.WriteString("\n")

	case *IfStmt:
		writeIndent(buf, indent)
		buf.WriteString("IF (")
		pp(buf, n.Condition, 0)
		buf.WriteString(") THEN\n")
		for _, stmt := range n.ThenPart {
			pp(buf, stmt, indent+1)
		}
		for _, elseIf := range n.ElseIfParts {
			writeIndent(buf, indent)
			buf.WriteString("ELSE IF (")
			pp(buf, elseIf.Condition, 0)
			buf.WriteString(") THEN\n")
			for _, stmt := range elseIf.ThenPart {
				pp(buf, stmt, indent+1)
			}
		}
		if len(n.ElsePart) > 0 {
			writeIndent(buf, indent)
			buf.WriteString("ELSE\n")
			for _, stmt := range n.ElsePart {
				pp(buf, stmt, indent+1)
			}
		}
		writeIndent(buf, indent)
		buf.WriteString("END IF\n")

	case *DoLoop:
		writeIndent(buf, indent)
		buf.WriteString("DO")
		if n.Var != "" {
			fmt.Fprintf(buf, " %s = ", n.Var)
			pp(buf, n.Start, 0)
			buf.WriteString(", ")
			pp(buf, n.End, 0)
			if n.Step != nil {
				buf.WriteString(", ")
				pp(buf, n.Step, 0)
			}
		} else if n.Start != nil { // DO WHILE
			buf.WriteString(" WHILE (")
			pp(buf, n.Start, 0)
			buf.WriteString(")")
		}
		buf.WriteString("\n")
		for _, stmt := range n.Body {
			pp(buf, stmt, indent+1)
		}
		writeIndent(buf, indent)
		buf.WriteString("END DO\n")

	case *CallStmt:
		writeIndent(buf, indent)
		buf.WriteString("CALL ")
		buf.WriteString(n.Name)
		if len(n.Args) > 0 {
			buf.WriteString("(")
			for i, arg := range n.Args {
				if i > 0 {
					buf.WriteString(", ")
				}
				pp(buf, arg, 0)
			}
			buf.WriteString(")")
		}
		buf.WriteString("\n")

	case *ReturnStmt:
		writeIndent(buf, indent)
		buf.WriteString("RETURN\n")

	case *CycleStmt:
		writeIndent(buf, indent)
		buf.WriteString("CYCLE\n")

	case *ExitStmt:
		writeIndent(buf, indent)
		buf.WriteString("EXIT\n")

	// Expression printing
	case *Identifier:
		buf.WriteString(n.Value)
	case *IntegerLiteral:
		buf.WriteString(n.Raw)
	case *RealLiteral:
		buf.WriteString(n.Raw)
	case *StringLiteral:
		fmt.Fprintf(buf, "%q", n.Value)
	case *LogicalLiteral:
		if n.Value {
			buf.WriteString(".TRUE.")
		} else {
			buf.WriteString(".FALSE.")
		}
	case *BinaryExpr:
		pp(buf, n.Left, 0)
		fmt.Fprintf(buf, " %s ", n.Op)
		pp(buf, n.Right, 0)
	case *UnaryExpr:
		buf.WriteString(n.Op.String())
		pp(buf, n.Operand, 0)
	case *ParenExpr:
		buf.WriteString("(")
		pp(buf, n.Expr, 0)
		buf.WriteString(")")

	default:
		// For any unhandled nodes, just print their type
		fmt.Fprintf(buf, "[UNHANDLED: %T]\n", node)
	}
}

func writeIndent(buf *bytes.Buffer, indent int) {
	for i := 0; i < indent; i++ {
		buf.WriteString("  ")
	}
}
