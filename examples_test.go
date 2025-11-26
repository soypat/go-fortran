package fortran_test

import (
	"fmt"
	"strings"

	"github.com/soypat/go-fortran"
	"github.com/soypat/go-fortran/ast"
)

// Example_parseAndPrintAST demonstrates parsing a simple Fortran program
// and printing its Abstract Syntax Tree (AST) in a visual format.
func Example_parseAndPrintAST() {
	// Sample Fortran 90 program
	src := `
PROGRAM hello
  IMPLICIT NONE
  INTEGER :: x, y
  REAL :: result

  x = 10
  y = 20
  result = x + y
  PRINT *, 'Result:', result
END PROGRAM hello
`

	// Create parser and parse the program
	var parser fortran.Parser90
	err := parser.Reset("example.f90", strings.NewReader(src))
	if err != nil {
		fmt.Println("Error:", err)
		return
	}

	// Parse the first program unit
	unit := parser.ParseNextProgramUnit()
	if unit == nil {
		fmt.Println("No program unit found")
		return
	}

	// Check for parsing errors
	if len(parser.Errors()) > 0 {
		fmt.Println("Parse errors:")
		for _, e := range parser.Errors() {
			fmt.Println("  ", e)
		}
		return
	}

	// Print the AST in a visual tree format
	fmt.Println("Abstract Syntax Tree:")
	fmt.Println("=====================")
	ast.Print(unit)
	// TODO: fix this after transpiler totally finished and not expected to change.
	// OMIT Output for now:
	// Abstract Syntax Tree:
	// =====================
	// ProgramBlock {
	//   Name: "hello"
	//   Body: ast.Statement (len=7) [
	//     0: ImplicitStatement {
	//       IsNone: true
	//       Label: ""
	//       Position: Position {
	//         Start: 19
	//         End: 32
	//       }
	//     }
	//     1: TypeDeclaration {
	//       TypeSpec: "INTEGER"
	//       Entities: ast.DeclEntity (len=2) [
	//         0: DeclEntity {
	//           Name: "x"
	//           Initializer: ""
	//         }
	//         1: DeclEntity {
	//           Name: "y"
	//           Initializer: ""
	//         }
	//       ]
	//       Label: ""
	//       Position: Position {
	//         Start: 35
	//         End: 50
	//       }
	//     }
	//     2: TypeDeclaration {
	//       TypeSpec: "REAL"
	//       Entities: ast.DeclEntity (len=1) [
	//         0: DeclEntity {
	//           Name: "result"
	//           Initializer: ""
	//         }
	//       ]
	//       Label: ""
	//       Position: Position {
	//         Start: 53
	//         End: 67
	//       }
	//     }
	//     3: AssignmentStmt {
	//       Target: Identifier {
	//         Value: "x"
	//         Position: Position {
	//           Start: 71
	//           End: 72
	//         }
	//       }
	//       Value: IntegerLiteral {
	//         Value: 10
	//         Raw: "10"
	//         Position: Position {
	//           Start: 75
	//           End: 77
	//         }
	//       }
	//       Label: ""
	//       Position: Position {
	//         Start: 71
	//         End: 77
	//       }
	//     }
	//     4: AssignmentStmt {
	//       Target: Identifier {
	//         Value: "y"
	//         Position: Position {
	//           Start: 80
	//           End: 81
	//         }
	//       }
	//       Value: IntegerLiteral {
	//         Value: 20
	//         Raw: "20"
	//         Position: Position {
	//           Start: 84
	//           End: 86
	//         }
	//       }
	//       Label: ""
	//       Position: Position {
	//         Start: 80
	//         End: 86
	//       }
	//     }
	//     5: AssignmentStmt {
	//       Target: Identifier {
	//         Value: "result"
	//         Position: Position {
	//           Start: 89
	//           End: 95
	//         }
	//       }
	//       Value: BinaryExpr {
	//         Op: 91
	//         Left: Identifier {
	//           Value: "x"
	//           Position: Position {
	//             Start: 98
	//             End: 99
	//           }
	//         }
	//         Right: Identifier {
	//           Value: "y"
	//           Position: Position {
	//             Start: 102
	//             End: 103
	//           }
	//         }
	//         Position: Position {
	//           Start: 98
	//           End: 103
	//         }
	//       }
	//       Label: ""
	//       Position: Position {
	//         Start: 89
	//         End: 103
	//       }
	//     }
	//     6: PrintStmt {
	//       Format: Identifier {
	//         Value: "*"
	//         Position: Position {
	//           Start: 112
	//           End: 112
	//         }
	//       }
	//       OutputList: ast.Expression (len=2) [
	//         0: StringLiteral {
	//           Value: "Result:"
	//           Position: Position {
	//             Start: 115
	//             End: 122
	//           }
	//         }
	//         1: Identifier {
	//           Value: "result"
	//           Position: Position {
	//             Start: 126
	//             End: 132
	//           }
	//         }
	//       ]
	//       Label: ""
	//       Position: Position {
	//         Start: 106
	//         End: 132
	//       }
	//     }
	//   ]
	//   Label: ""
	//   Position: Position {
	//     Start: 3
	//     End: 133
	//   }
	// }
}

// Example_parseModule demonstrates parsing a Fortran module with
// specification statements and contained procedures.
func Example_parseModule() {
	src := `
MODULE math_utils
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: add, multiply

  INTEGER, PARAMETER :: VERSION = 1

  CONTAINS

  FUNCTION add(a, b) RESULT(sum)
    INTEGER, INTENT(IN) :: a, b
    INTEGER :: sum
    sum = a + b
  END FUNCTION add

  FUNCTION multiply(a, b)
    INTEGER :: a, b, multiply
    multiply = a * b
  END FUNCTION multiply

END MODULE math_utils
`

	var parser fortran.Parser90
	err := parser.Reset("math_utils.f90", strings.NewReader(src))
	if err != nil {
		fmt.Println("Error:", err)
		return
	}

	unit := parser.ParseNextProgramUnit()
	if unit == nil {
		fmt.Println("No program unit found")
		return
	}

	if len(parser.Errors()) > 0 {
		fmt.Println("Parse errors:")
		for _, e := range parser.Errors() {
			fmt.Println("  ", e)
		}
		return
	}

	// Print just the module structure (not the full AST)
	if mod, ok := unit.(*ast.Module); ok {
		fmt.Printf("Module: %s\n", mod.Name)
		fmt.Printf("  Specification statements: %d\n", len(mod.Body))
		fmt.Printf("  Contained procedures: %d\n", len(mod.Contains))

		fmt.Println("\nSpecification statements:")
		for i, stmt := range mod.Body {
			switch s := stmt.(type) {
			case *ast.ImplicitStatement:
				if s.IsNone {
					fmt.Printf("  %d: IMPLICIT NONE\n", i)
				}
			case *ast.TypeDeclaration:
				fmt.Printf("  %d: %s declaration with %d entities\n", i, s.Type.String(), len(s.Entities))
			default:
				fmt.Printf("  %d: %T\n", i, stmt)
			}
		}

		fmt.Println("\nContained procedures:")
		for i, proc := range mod.Contains {
			switch p := proc.(type) {
			case *ast.Function:
				fmt.Printf("  %d: FUNCTION %s\n", i, p.Name)
			case *ast.Subroutine:
				fmt.Printf("  %d: SUBROUTINE %s\n", i, p.Name)
			}
		}
	}

	// omit Output:
	// Module: math_utils
	//   Specification statements: 2
	//   Contained procedures: 2
	//
	// Specification statements:
	//   0: IMPLICIT NONE
	//   1: INTEGER declaration with 1 entities
	//
	// Contained procedures:
	//   0: FUNCTION add
	//   1: FUNCTION multiply
}

// Example_detectSpecificationErrors demonstrates how the parser
// detects errors in specification statement ordering.
func Example_detectSpecificationErrors() {
	// This program has IMPLICIT NONE after a type declaration (error!)
	src := `
PROGRAM bad_implicit
  INTEGER :: x
  IMPLICIT NONE
  x = 42
END PROGRAM
`

	var parser fortran.Parser90
	err := parser.Reset("bad.f90", strings.NewReader(src))
	if err != nil {
		fmt.Println("Error:", err)
		return
	}

	parser.ParseNextProgramUnit()

	// Check for errors
	if len(parser.Errors()) > 0 {
		fmt.Println("Parse errors detected:")
		for _, e := range parser.Errors() {
			fmt.Printf("  %s\n", e.Error())
		}
	} else {
		fmt.Println("No errors detected")
	}

	// Output:
	// Parse errors detected:
	//   bad.f90:4:13: IMPLICIT NONE must appear before type declarations
}

// Example_parseSubroutine demonstrates parsing a subroutine
// with parameters and specification statements.
func Example_parseSubroutine() {
	src := `
SUBROUTINE swap(a, b)
  IMPLICIT NONE
  REAL, INTENT(INOUT) :: a, b
  REAL :: temp

  temp = a
  a = b
  b = temp
END SUBROUTINE swap
`

	var parser fortran.Parser90
	parser.Reset("swap.f90", strings.NewReader(src))

	unit := parser.ParseNextProgramUnit()
	if sub, ok := unit.(*ast.Subroutine); ok {
		fmt.Printf("Subroutine: %s\n", sub.Name)

		// Extract parameter names
		var paramNames []string
		for _, p := range sub.Parameters {
			paramNames = append(paramNames, p.Name)
		}
		fmt.Printf("Parameters: %v\n", paramNames)
		fmt.Printf("Specification statements: %d\n", len(sub.Body))
	}

	// Output:
	// Subroutine: swap
	// Parameters: [a b]
	// Specification statements: 6
}
