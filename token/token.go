package token

import "bytes"

type Token int

// Install stringer tool:
//  go install golang.org/x/tools/cmd/stringer@latest

//go:generate stringer -type=Token -linecomment -output stringers.go .

// List of all tokens of the Fortran programming language.
// When adding a new token add it in between blocks since we use comparison functions to check properties of tokens.
const (
	// Not to be used in code. Is to catch uninitialized tokens.
	Undefined Token = iota // <undefined>

	// ==================== KEYWORDS ====================

	// Type declaration keywords
	INTEGER         // INTEGER
	REAL            // REAL
	COMPLEX         // COMPLEX
	LOGICAL         // LOGICAL
	CHARACTER       // CHARACTER
	DOUBLE          // DOUBLE
	PRECISION       // PRECISION
	DOUBLEPRECISION // DOUBLEPRECISION

	// Program structure keywords
	PROGRAM       // PROGRAM
	END           // END
	ENDPROGRAM    // ENDPROGRAM
	SUBROUTINE    // SUBROUTINE
	ENDSUBROUTINE // ENDSUBROUTINE
	FUNCTION      // FUNCTION
	ENDFUNCTION   // ENDFUNCTION
	MODULE        // MODULE
	ENDMODULE     // ENDMODULE
	CONTAINS      // CONTAINS

	// Control flow keywords
	IF        // IF
	THEN      // THEN
	ELSE      // ELSE
	ELSEIF    // ELSEIF
	ENDIF     // ENDIF
	DO        // DO
	ENDDO     // ENDDO
	WHILE     // WHILE
	SELECT    // SELECT
	CASE      // CASE
	DEFAULT   // DEFAULT
	ENDSELECT // ENDSELECT
	CYCLE     // CYCLE
	EXIT      // EXIT
	GOTO      // GOTO
	CONTINUE  // CONTINUE
	RETURN    // RETURN
	STOP      // STOP

	// I/O keywords
	READ      // READ
	WRITE     // WRITE
	PRINT     // PRINT
	OPEN      // OPEN
	CLOSE     // CLOSE
	INQUIRE   // INQUIRE
	BACKSPACE // BACKSPACE
	REWIND    // REWIND
	ENDFILE   // ENDFILE
	FORMAT    // FORMAT
	NAMELIST  // NAMELIST

	// Declaration and specification keywords
	IMPLICIT    // IMPLICIT
	PARAMETER   // PARAMETER
	DIMENSION   // DIMENSION
	DATA        // DATA
	EQUIVALENCE // EQUIVALENCE
	COMMON      // COMMON
	EXTERNAL    // EXTERNAL
	INTRINSIC   // INTRINSIC
	SAVE        // SAVE
	SEQUENCE    // SEQUENCE

	// Interface and type keywords (F90)
	INTERFACE    // INTERFACE
	ENDINTERFACE // ENDINTERFACE
	TYPE         // TYPE
	ENDTYPE      // ENDTYPE

	// Module and visibility keywords (F90)
	USE     // USE
	ONLY    // ONLY
	PRIVATE // PRIVATE
	PUBLIC  // PUBLIC

	// Miscellaneous keywords
	CALL    // CALL
	ASSIGN  // ASSIGN
	INCLUDE // INCLUDE
	DEFINE  // DEFINE

	// Array operations (F90)
	WHERE     // WHERE
	ELSEWHERE // ELSEWHERE
	ENDWHERE  // ENDWHERE

	// ==================== ATTRIBUTES (F90) ====================

	INTENT      // INTENT
	IN          // IN
	OUT         // OUT
	INOUT       // INOUT
	OPTIONAL    // OPTIONAL
	POINTER     // POINTER
	TARGET      // TARGET
	ALLOCATABLE // ALLOCATABLE
	ALLOCATE    // ALLOCATE
	DEALLOCATE  // DEALLOCATE
	NULLIFY     // NULLIFY
	RECURSIVE   // RECURSIVE
	ELEMENTAL   // ELEMENTAL
	PURE        // PURE
	RESULT      // RESULT
	KIND        // KIND
	LEN         // LEN

	// ==================== OPERATORS ====================

	// Arithmetic operators
	Plus       // +
	Minus      // -
	Asterisk   // *
	Slash      // /
	DoubleStar // **

	// Assignment operators
	Equals        // =
	PointerAssign // =>

	// Relational operators (Fortran 77 style)
	EQ // .EQ.
	NE // .NE.
	LT // .LT.
	LE // .LE.
	GT // .GT.
	GE // .GE.

	// Relational operators (Fortran 90 style)
	EqEq      // ==
	NotEquals // /=
	Less      // <
	LessEq    // <=
	Greater   // >
	GreaterEq // >=

	// Logical operators
	AND  // .AND.
	OR   // .OR.
	NOT  // .NOT.
	EQV  // .EQV.
	NEQV // .NEQV.

	// String operator
	StringConcat // //

	// ==================== DELIMITERS / PUNCTUATION ====================

	LParen      // (
	RParen      // )
	Comma       // ,
	Colon       // :
	DoubleColon // ::
	Semicolon   // ;
	LBracket    // [
	RBracket    // ]
	Percent     // %
	Ampersand   // &
	Dollar      // $

	// ==================== LITERALS ====================

	// Logical constants
	TRUE  // .TRUE.
	FALSE // .FALSE.

	// User-defined literals
	Identifier // <identifier>
	IntLit     // <integer>
	FloatLit   // <float>
	StringLit  // <string>
	FormatSpec // <formatspec>

	// ==================== SPECIAL TOKENS ====================

	// Label       // <label> // REMOVE TEMPORARILY UNTIL IMPLEMENTED IN LEXER UNAMBIGUOUSLY.
	LineComment // <linecomment>
	// ENDPARSE is for internal debugging purposes. User may insert ENDPARSE and it shall end parsing immediately.
	EndParse // <EOF_ARTIFICIAL>
	NewLine  // <newline>
	EOF      // <EOF>
	Illegal  // <illegal>
	numToks
)

// IsEndConstruct returns non-zero if the current, next, and next-next tokens form an end construct
// for control structures and blocks contained within a program unit (e.g., IF, DO, SELECT).
// This does NOT include program unit endings like END SUBROUTINE or END PROGRAM.
// Returns 1 if only first token is end construct, 2 if both are part of end construct. 0 if not a end construct.
func IsEndConstruct(current, next, nextnext Token) int {
	if current == IntLit {
		n := isEndConstruct(next, nextnext)
		if n > 0 {
			// Labelled end statement.
			return n + 1
		}
		return 0
	}
	return isEndConstruct(current, next)
}

func isEndConstruct(current, next Token) int {
	switch {
	case current.IsEndConstruct():
		return 1
	case current == END:
		if next.IsConstruct() {
			return 2
		}
	}
	return 0
}

func (next Token) IsConstruct() bool {
	return next == IF || next == DO || next == SELECT || next == WHERE ||
		next == INTERFACE || next == TYPE
}

func (tok Token) IsEndConstruct() bool {
	return tok == ENDIF || tok == ENDDO || tok == ENDSELECT || tok == ENDWHERE ||
		tok == ENDINTERFACE || tok == ENDTYPE
}

// IsKeyword returns true if the token is a Fortran keyword.
func (tok Token) IsKeyword() bool {
	return tok >= INTEGER && tok <= ENDWHERE
}

func (tok Token) IsAttributeKeyword() bool {
	return tok == PURE || tok == RECURSIVE || tok == ELEMENTAL
}

func (tok Token) EndConstructComposite() Token {
	switch tok {
	case IF:
		return ENDIF
	case DO:
		return ENDDO
	default:
		panic(tok.String() + " has no composite")
	}
}

// IsExecutableStatement returns true if the token is a executable
// statement- control structure or built in function.
// The [Identifier] token returns false as it may or may not represent an executable statement.
func (tok Token) IsExecutableStatement() bool {
	switch tok {
	case IF, DO, CALL, RETURN, STOP, EXIT,
		ALLOCATE, DEALLOCATE, READ, WRITE, PRINT,
		GOTO, CONTINUE, CYCLE:
		return true
	}
	return false
}

func (tok Token) IsIllegalOrEOF() bool {
	return tok == EOF || tok == Illegal
}

// IsEndOrElse returns true if the token is a construct-ending keyword.
// These tokens typically mark the end of a control structure or block.
func (tok Token) IsEndOrElse() bool {
	return tok.IsEnd() || tok == ELSE || tok == ELSEIF
}

// IsEnd returns true if the token starts with END. Includes composite ENDs like ENDDO, ENDIF, ENDPROGRAM, etc.
func (tok Token) IsEnd() bool {
	switch tok {
	case END, ENDIF, ENDDO, ENDPROGRAM, ENDSUBROUTINE, ENDFUNCTION,
		ENDMODULE, ENDINTERFACE, ENDTYPE, ENDSELECT, ENDWHERE:
		return true
	}
	return false
}

// IsTypeDeclaration returns true if the token is a type declaration keyword.
func (tok Token) IsTypeDeclaration() bool {
	return tok >= INTEGER && tok <= DOUBLEPRECISION
}

// IsAttribute returns true if the token is a Fortran 90 attribute.
func (tok Token) IsAttribute() bool {
	// F90 attributes that can appear in type declarations
	switch tok {
	case PARAMETER, DIMENSION, SAVE, EXTERNAL, INTRINSIC, PUBLIC, PRIVATE:
		return true
	default:
		return tok >= INTENT && tok <= LEN
	}
}

// IsOperator returns true if the token is an operator.
func (tok Token) IsOperator() bool {
	return tok >= Plus && tok <= StringConcat
}

// IsDelimiter returns true if the token is a delimiter or punctuation.
func (tok Token) IsDelimiter() bool {
	return tok >= LParen && tok <= Dollar
}

// IsLiteral returns true if the token is a literal value (logical constant or user-defined literal).
func (tok Token) IsLiteral() bool {
	return tok >= TRUE && tok <= FormatSpec
}

// LookupKeyword returns [Identifier] or the token for keyword maybeKeyword represents if found.
func LookupKeyword(maybeKeyword []byte) Token {
	// Convert to uppercase for case-insensitive comparison
	upper := bytes.ToUpper(maybeKeyword)
	switch string(upper) {
	default:
		return Identifier
	case "ENDPARSE":
		return EndParse
	case "PROGRAM":
		return PROGRAM
	case "SUBROUTINE":
		return SUBROUTINE
	case "INTEGER":
		return INTEGER
	case "CHARACTER":
		return CHARACTER
	case "COMPLEX":
		return COMPLEX
	case "LOGICAL":
		return LOGICAL
	case "REAL":
		return REAL
	case "DATA":
		return DATA
	case "EXTERNAL":
		return EXTERNAL
	case "IMPLICIT":
		return IMPLICIT
	case "FUNCTION":
		return FUNCTION
	case "END":
		return END
	case "ELSEIF", "ELSE IF":
		return ELSEIF
	case "DO":
		return DO
	case "DIMENSION":
		return DIMENSION
	case "DOUBLE":
		return DOUBLE
	case "DOUBLEPRECISION":
		return DOUBLEPRECISION
	case "CALL":
		return CALL
	case "THEN":
		return THEN
	case "WRITE":
		return WRITE
	case "PRINT":
		return PRINT
	case "WHILE":
		return WHILE
	case "PARAMETER":
		return PARAMETER
	case "PRECISION":
		return PRECISION
	case "INTRINSIC":
		return INTRINSIC
	case "FORMAT":
		return FORMAT
	case "STOP":
		return STOP
	case "SAVE":
		return SAVE
	case "OPEN":
		return OPEN
	case "READ":
		return READ
	case "ASSIGN":
		return ASSIGN
	case "DEFINE":
		return DEFINE
	case "CLOSE":
		return CLOSE
	case "EQUIVALENCE":
		return EQUIVALENCE
	case "COMMON":
		return COMMON
	case "REWIND":
		return REWIND
	case "INCLUDE":
		return INCLUDE
	case "IF":
		return IF
	case "ELSE":
		return ELSE
	case "ENDIF", "END IF":
		return ENDIF
	case "GOTO", "GO TO":
		return GOTO
	case "CONTINUE":
		return CONTINUE
	case "RETURN":
		return RETURN
	case "SELECT":
		return SELECT
	case "CASE":
		return CASE
	case "DEFAULT":
		return DEFAULT
	case "CYCLE":
		return CYCLE
	case "EXIT":
		return EXIT
	case "ENDDO", "END DO":
		return ENDDO
	case "ENDPROGRAM", "END PROGRAM":
		return ENDPROGRAM
	case "ENDSUBROUTINE", "END SUBROUTINE":
		return ENDSUBROUTINE
	case "ENDFUNCTION", "END FUNCTION":
		return ENDFUNCTION
	case "ENDSELECT", "END SELECT":
		return ENDSELECT
	case "MODULE":
		return MODULE
	case "ENDMODULE", "END MODULE":
		return ENDMODULE
	case "USE":
		return USE
	case "ONLY":
		return ONLY
	case "CONTAINS":
		return CONTAINS
	case "INTERFACE":
		return INTERFACE
	case "ENDINTERFACE", "END INTERFACE":
		return ENDINTERFACE
	case "TYPE":
		return TYPE
	case "ENDTYPE", "END TYPE":
		return ENDTYPE
	case "SEQUENCE":
		return SEQUENCE
	case "PRIVATE":
		return PRIVATE
	case "PUBLIC":
		return PUBLIC
	case "INTENT":
		return INTENT
	case "IN":
		return IN
	case "OUT":
		return OUT
	case "INOUT":
		return INOUT
	case "OPTIONAL":
		return OPTIONAL
	case "POINTER":
		return POINTER
	case "TARGET":
		return TARGET
	case "ALLOCATABLE":
		return ALLOCATABLE
	case "RECURSIVE":
		return RECURSIVE
	case "ELEMENTAL":
		return ELEMENTAL
	case "PURE":
		return PURE
	case "RESULT":
		return RESULT
	case "KIND":
		return KIND
	case "LEN":
		return LEN
	case "ALLOCATE":
		return ALLOCATE
	case "DEALLOCATE":
		return DEALLOCATE
	case "NULLIFY":
		return NULLIFY
	case "WHERE":
		return WHERE
	case "ELSEWHERE", "ELSE WHERE":
		return ELSEWHERE
	case "ENDWHERE", "END WHERE":
		return ENDWHERE
	case "INQUIRE":
		return INQUIRE
	case "BACKSPACE":
		return BACKSPACE
	case "ENDFILE":
		return ENDFILE
	case "NAMELIST":
		return NAMELIST
	}
}

// LookupDotOperator checks if the internal characters in a dot operator
// match with a token. Returns [Illegal] if no match found.
func LookupDotOperator(ident []byte) Token {
	// Convert to uppercase for case-insensitive comparison
	upper := bytes.ToUpper(ident)
	switch string(upper) {
	default:
		return Illegal
	case "TRUE":
		return TRUE
	case "FALSE":
		return FALSE
	case "EQ":
		return EQ
	case "NE":
		return NE
	case "LT":
		return LT
	case "LE":
		return LE
	case "GT":
		return GT
	case "GE":
		return GE
	case "AND":
		return AND
	case "OR":
		return OR
	case "NOT":
		return NOT
	case "EQV":
		return EQV
	case "NEQV":
		return NEQV
	}
}
