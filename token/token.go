package token

type Token int

// Install stringer tool:
//  go install golang.org/x/tools/cmd/stringer@latest

//go:generate stringer -type=Token -linecomment -output stringers.go .

// List of all tokens of the Fortran programming language.
const (
	// Not to be used in code. Is to catch uninitialized tokens.
	Undefined Token = iota // <undefined>

	// ==================== KEYWORDS ====================

	// Type declaration keywords
	INTEGER   // INTEGER
	REAL      // REAL
	COMPLEX   // COMPLEX
	LOGICAL   // LOGICAL
	CHARACTER // CHARACTER
	DOUBLE    // DOUBLE
	PRECISION // PRECISION

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
	ENDIF2    // END IF (with space)
	DO        // DO
	ENDDO     // ENDDO
	ENDDO2    // END DO (with space)
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

	LineComment // <linecomment>
	NewLine     // <newline>
	EOF         // <EOF>
	Illegal     // <illegal>
	numToks
)

// IsKeyword returns true if the token is a Fortran keyword.
func (tok Token) IsKeyword() bool {
	return tok >= INTEGER && tok <= ENDWHERE
}

func (tok Token) IsIllegalOrEOF() bool {
	return tok == EOF || tok == Illegal
}

// IsTypeDeclaration returns true if the token is a type declaration keyword.
func (tok Token) IsTypeDeclaration() bool {
	return tok >= INTEGER && tok <= PRECISION
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
