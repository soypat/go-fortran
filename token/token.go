package token

import (
	"bytes"
	"strings"
	"unsafe"
)

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
	keywordBeg      // invalid
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
	ENTRY         // ENTRY

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
	GO        // GO
	TO        // TO
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
	FILE      // FILE
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
	attrStart

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

	keywordEnd // invalid

	// ==================== OPERATORS ====================

	opStart

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

	opEnd

	// ==================== DELIMITERS / PUNCTUATION ====================
	delimStart

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

	delimEnd
	// ==================== LITERALS ====================
	litStart

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
	litEnd
	// ENDPARSE is for internal debugging purposes. User may insert ENDPARSE and it shall end parsing immediately.
	EndParse // <EOF_ARTIFICIAL>
	NewLine  // <newline>
	EOF      // <EOF>
	Illegal  // <illegal>
	numToks
)

const keywordPower = 8

var keywordMap [1 << keywordPower]Token // must be power of 2.

// kwhash is a perfect hash function for keywords.
// It assumes that s has at least length 2.
func kwhash(id string) uint {
	h := uint(toUpper(id[0]))*29 + uint(toUpper(id[1]))*21 + uint(toUpper(id[len(id)-1]))*13 + uint(len(id))*8
	if len(id) > 2 {
		h += uint(toUpper(id[2])) * 10
	}
	if len(id) > 3 {
		h += uint(toUpper(id[3])) * 15
	}
	return h & uint(len(keywordMap)-1)
}

func toUpper(r byte) byte {
	if 'a' <= r && r <= 'z' {
		r -= 'a' - 'A'
	}
	return r
}

func init() {
	// populate keywordMap
	for tok := keywordBeg + 1; tok < keywordEnd; tok++ {
		h := kwhash(tok.String())
		if !tok.IsKeyword() {
			continue // Avoid internal markers.
		}
		if keywordMap[h] != 0 {
			// panic(fmt.Sprintf("imperfect hash at %0x %s collides with %s (%d/%d ok)", h, keywordMap[h].String(), tok.String(), tok-keywordBeg-1, keywordEnd-keywordBeg-1))
		}
		keywordMap[h] = tok
	}
}

func IsAssignment(current, next Token) bool {
	maybeIdent, maybeEqOrLParen := current, next
	if !maybeIdent.CanBeUsedAsIdentifier() {
		return false
	}
	// Ident can be used as identifier by now.
	if maybeEqOrLParen == Equals {
		return true
	} else if maybeEqOrLParen != LParen {
		return false
	}
	// By now we have a identifier followed by left parentheses.
	// Check the identifier is not a keyword that may have parentheses in usage.
	if maybeIdent.IsConstructAdmitsParens() {
		return false
	}
	// Give up trying to prove it is not a assignment, we can be pretty sure it is by now.
	return true
}

func IsExecutableStatement(current, next, _ Token) bool {
	if current.IsExecutableStatement() {
		return true
	} else if current == IntLit && !next.IsEnd() {
		// Statement label followed by executable statement (e.g., "10 READ(...)")
		// But not label followed by END (e.g., "100 END PROGRAM")
		return true
	} else if current == Identifier {
		// Could be assignment or procedure call. We need to lookahead to distinguish.
		// If we see `IDENTIFIER =`, it's an assignment.
		// If we see `IDENTIFIER(...)` it could be an assignment to an array element or a function call.
		// For now, we will treat all identifiers at the start of a statement in the execution part as the start of an executable statement.
		return true
	} else if current == END && next == Equals {
		// Special case: END used as a variable name in assignment (e.g., "END = a(j) + d")
		return true
	} else if current.IsTypeDeclaration() || current == DATA {
		// Type keywords and DATA start specification statements, not executable statements
		return false
	} else if IsAssignment(current, next) {
		// Keywords used as identifiers in assignments (RESULT=1, STOP(I)=5, etc.)
		// But not type keywords which are always declarations
		return true
	}
	return false
}

func IsEndProgramUnit(current, next Token) int {
	switch current {
	case ENDPROGRAM, ENDSUBROUTINE, ENDFUNCTION, ENDMODULE:
		return 1
	}
	if current != END {
		return 0
	}
	switch next {
	case PROGRAM, SUBROUTINE, FUNCTION, MODULE:
		return 2
	case NewLine, EOF, LineComment:
		// Bare END (common in older Fortran)
		return 1
	case Identifier:
		// Could be "END program_name" or "END BLOCK" (for BLOCK DATA)
		return 2
	case Equals:
		// END= is an I/O specifier (e.g., READ(10,END=900)), not end of program unit
		return 0
	}
	return 0
}

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
	return tok > keywordBeg && tok < keywordEnd && tok != attrStart
}

func (tok Token) IsAttributeKeyword() bool {
	return tok > attrStart && tok < keywordEnd
}

// IsOperator returns true if the token is an operator.
func (tok Token) IsOperator() bool {
	return tok > opStart && tok < opEnd
}

// IsDelimiter returns true if the token is a delimiter or punctuation.
func (tok Token) IsDelimiter() bool {
	return tok > delimStart && tok < delimEnd
}

// IsLiteral returns true if the token is a literal value (logical constant or user-defined literal).
func (tok Token) IsLiteral() bool {
	return tok > litStart && tok < litEnd
}

// IsConstructWithParens returns true if the construct has parentheses
// that follow after declaration.
func (tok Token) IsConstructAdmitsParens() bool {
	switch tok {
	case IF, GOTO,
		PARAMETER, EQUIVALENCE, POINTER,
		READ, WRITE, FORMAT, OPEN, CLOSE,
		ALLOCATE, DEALLOCATE, REWIND, BACKSPACE, INQUIRE,
		REAL:
		return true
	}
	return false
}

// CanBeUsedAsIdentifier returns true if this token can be used as an identifier.
// In Fortran, most keywords can be used as variable/function names, but some
// structural keywords (PROGRAM, SUBROUTINE, FUNCTION, MODULE, END, CONTAINS)
// would cause ambiguity and are excluded.
func (tok Token) CanBeUsedAsIdentifier() bool {
	// Explicit identifiers are always OK
	switch tok {
	case Identifier, FormatSpec:
		return true
	case DATA, ENDFILE:
		// Type keywords and DATA start specification statements, not executable statements
		return false
	case PROGRAM, SUBROUTINE, FUNCTION, MODULE, CONTAINS:
		// Exclude structural keywords that would cause ambiguity
		return false
	default:
		// Most other keywords and attributes can be used as identifiers
		return tok.IsKeyword()
	}
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
	case IF, DO, SELECT, CALL, ENTRY, RETURN, STOP, EXIT,
		ALLOCATE, DEALLOCATE, READ, WRITE, OPEN, PRINT,
		CLOSE, BACKSPACE, REWIND, ENDFILE, INQUIRE,
		GOTO, CONTINUE, CYCLE, ASSIGN, GO:
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

// IsTypeIntrinsic returns true if the token is a type instrinsic keyword.
func (tok Token) IsTypeIntrinsic() bool {
	return tok >= INTEGER && tok <= DOUBLEPRECISION
}

// IsTypeDeclaration returns true if the token is a type declaration keyword.
func (tok Token) IsTypeDeclaration() bool {
	return tok.IsTypeIntrinsic() || tok == TYPE
}

// LookupKeyword returns [Identifier] or the token for keyword maybeKeyword represents if found.
func LookupKeyword(maybeKeyword []byte) (tok Token) {
	// Convert to uppercase for case-insensitive comparison
	if len(maybeKeyword) < 2 {
		return Identifier
	}
	s := unsafe.String(&maybeKeyword[0], len(maybeKeyword))
	tok = keywordMap[kwhash(s)]
	if tok != 0 && strings.EqualFold(s, tok.String()) {
		return tok
	}
	return Identifier
}

// LookupDotOperator checks if the internal characters in a dot operator
// match with a token. Returns [Illegal] if no match found.
func LookupDotOperator(ident []byte) (tok Token) {
	// Convert to uppercase for case-insensitive comparison
	upper := bytes.ToUpper(ident)
	switch string(upper) {
	default:
		tok = Illegal
	case "TRUE":
		tok = TRUE
	case "FALSE":
		tok = FALSE
	case "EQ":
		tok = EQ
	case "NE":
		tok = NE
	case "LT":
		tok = LT
	case "LE":
		tok = LE
	case "GT":
		tok = GT
	case "GE":
		tok = GE
	case "AND":
		tok = AND
	case "OR":
		tok = OR
	case "NOT":
		tok = NOT
	case "EQV":
		tok = EQV
	case "NEQV":
		tok = NEQV
	}
	return tok
}
