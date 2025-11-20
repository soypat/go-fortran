package fortran

import (
	"bufio"
	"bytes"
	"errors"
	"io"
	"unicode/utf8"

	"github.com/soypat/go-fortran/token"
)

// This lexer implementation was ripped from https://github.com/soypat/goeda/blob/main/io/dsn/lexer.go
// The above lexer is in turn an implementation of the Lexer as described in "Writing An Interpreter In Go" by Thorsten Ball https://monkeylang.org/

// Lexer90 is a lexer for the Fortran 90 programming language.
type Lexer90 struct {
	input bufio.Reader
	ch    rune // current character (utf8)
	peek  rune // next character (utf8)
	err   error
	idbuf []byte // accumulation buffer.

	// Higher level statistics fields:

	source    string // filename or source name.
	line      int    // file line number
	col       int    // column number in line
	pos       int    // byte position.
	parens    int    // '{','}' braces counter to pick up on unbalanced braces early.
	tokenLine int    // line number where the last token started
	tokenCol  int    // column number where the last token started
}

func (l *Lexer90) IsDone() bool {
	return l.err != nil
}

func (l *Lexer90) isUnitialized() bool {
	return l.source == ""
}

// Reset discards all state and buffered data and begins a new lexing
// procedure on the input r. It performs a single utf8 read to initialize.
func (l *Lexer90) Reset(source string, r io.Reader) error {
	if r == nil {
		return errors.New("nil reader")
	} else if source == "" {
		return errors.New("no source name")
	}
	*l = Lexer90{
		input:  l.input,
		line:   1,
		idbuf:  l.idbuf,
		source: source,
	}
	l.input.Reset(r)
	if l.idbuf == nil {
		l.idbuf = make([]byte, 0, 1024)
	}
	// Fill up peek and current character.
	l.readCharLL()
	l.readCharLL()
	return l.err
}

// Source returns the name the lexer was reset/initialized with. Usually a filename.
func (l *Lexer90) Source() string {
	return l.source
}

// Err returns the lexer error.
func (l *Lexer90) Err() error {
	if l.err == io.EOF {
		return nil
	}
	return l.err
}

// LineCol returns the current line number and column number (utf8 relative).
func (l *Lexer90) LineCol() (line, col int) {
	return l.line, l.col
}

// TokenLineCol returns the line/col where the last returned token started.
func (l *Lexer90) TokenLineCol() (line, col int) {
	return l.tokenLine, l.tokenCol
}

// Pos returns the absolute position of the lexer in bytes from the start of the file.
func (l *Lexer90) Pos() int { return l.pos }

// Parens returns the parentheses/braces depth at the current position.
func (l *Lexer90) Parens() int { return l.parens }

// SkipLines skips next n lines of the input.
func (l *Lexer90) SkipLines(n int) error {
	if n <= 0 {
		return nil
	}
	targetLine := l.line + n
	for l.line != targetLine && !l.IsDone() {
		tok, _, lit := l.NextToken()
		if tok == token.Illegal {
			return errors.New("illegal token: " + string(lit))
		}
	}
	return l.Err()
}

// Next token parses the upcoming token and returns the literal representation
// of the token for identifiers, strings and numbers.
// The returned byte slice is reused between calls to NextToken.
func (l *Lexer90) NextToken() (tok token.Token, startPos int, literal []byte) {
	if l.isUnitialized() {
		l.err = errors.New("lexer unitilialized")
		return token.Illegal, 0, nil
	}

	// Handle labels at the beginning of a line
	if l.col == 1 && isDigit(l.ch) {
		startPos = l.pos
		l.tokenLine, l.tokenCol = l.line, l.col
		literal, ok := l.readNumber()
		if !ok || l.col <= 6 { // Labels are in columns 1-5
			return token.IntLit, startPos, literal
		}
		l.err = errors.New("bad label")
		return token.Illegal, startPos, literal
	}

	l.skipWhitespace()
	startPos = l.pos
	// Capture starting line/col for this token
	l.tokenLine, l.tokenCol = l.line, l.col
	// With lookahead buffer, l.err might be EOF while l.ch still has a valid character
	// Only return EOF when current character is exhausted
	if l.ch == 0 {
		return token.EOF, startPos, nil
	} else if l.err != nil && l.err != io.EOF {
		return token.Illegal, startPos, nil
	}
	ch := l.ch
	// Handle comments - '!' can appear at any column in Fortran 90
	if ch == '!' {
		l.readCharLL() // skip the '!' without continuation handling
		data := l.readCommentContent()
		return token.LineComment, startPos, data
	}
	switch ch {
	case '\n':
		tok = token.NewLine
		l.readChar()
	case '=':
		if l.peekChar() == '>' {
			tok = token.PointerAssign
			l.readChar()
			l.readChar()
		} else if l.peekChar() == '=' {
			tok = token.EqEq
			l.readChar()
			l.readChar()
		} else {
			tok = token.Equals
			l.readChar()
		}
	case '+':
		tok = token.Plus
		l.readChar()
	case '-':
		tok = token.Minus
		l.readChar()
	case '*':
		if l.peekChar() == '*' {
			tok = token.DoubleStar
			l.readChar()
			l.readChar()
		} else {
			tok = token.Asterisk
			l.readChar()
		}
	case '/':
		next := l.peekChar()
		switch next {
		case '=':
			tok = token.NotEquals
			l.readChar()
			l.readChar()
		case '/':
			tok = token.StringConcat
			l.readChar()
			l.readChar()
		default:
			tok = token.Slash
			l.readChar()
		}
	case '<':
		if l.peekChar() == '=' {
			tok = token.LessEq
			l.readChar()
			l.readChar()
		} else {
			tok = token.Less
			l.readChar()
		}
	case '>':
		if l.peekChar() == '=' {
			tok = token.GreaterEq
			l.readChar()
			l.readChar()
		} else {
			tok = token.Greater
			l.readChar()
		}
	case '(':
		tok = token.LParen
		l.parens++
		l.readChar()
	case ')':
		tok = token.RParen
		l.parens--
		l.readChar()
	case ',':
		tok = token.Comma
		l.readChar()
	case ':':
		if l.peekChar() == ':' {
			tok = token.DoubleColon
			l.readChar()
			l.readChar()
		} else {
			tok = token.Colon
			l.readChar()
		}
	case ';':
		tok = token.Semicolon
		l.readChar()
	case '[':
		tok = token.LBracket
		l.readChar()
	case ']':
		tok = token.RBracket
		l.readChar()
	case '%':
		tok = token.Percent
		l.readChar()
	case '&':
		tok = token.Ampersand
		l.readChar()
	case '$':
		tok = token.Dollar
		l.readChar()
	case '\'', '"':
		literal = l.readString(ch)
		tok = token.StringLit
	case '.':
		// Could be a decimal number, a logical operator, or logical constant
		next := l.peekChar()
		if isDigit(next) {
			literal, _ = l.readNumber()
			tok = token.FloatLit
		} else if isIdentifierChar(next) {
			// Read the identifier between dots (e.g., .TRUE., .AND., .EQ.)
			literal = l.readDotOperator()
			tok = l.lookupDotOperator(literal)
		} else {
			tok = token.Illegal
			l.readChar()
		}
	default:
		if isIdentifierChar(ch) {
			literal = l.readIdentifier()
			tok = l.lookupKeyword(literal)
			// Check if this could be a format specifier (e.g., I3, F10, E12, TL5, TR10)
			// Format specs are 1-2 format letters followed immediately by digits/dots
			if tok == token.Identifier && len(literal) > 1 && isFormatLetter(rune(literal[0])) {
				// Check if this matches format spec pattern
				// After initial letter(s), should only be digits and dots
				letterCount := 1
				if len(literal) > 1 && isFormatLetter(rune(literal[1])) {
					letterCount = 2
				}

				// Rest should be digits/dots only (for valid format spec)
				isFormatSpec := false
				if len(literal) > letterCount {
					allDigitsOrDots := true
					hasDigit := false
					for _, b := range literal[letterCount:] {
						r := rune(b)
						if isDigit(r) {
							hasDigit = true
						} else if r != '.' {
							allDigitsOrDots = false
							break
						}
					}
					isFormatSpec = allDigitsOrDots && hasDigit
				}

				if isFormatSpec {
					// Check if there's a dot followed by more digits (e.g., D20.7, F10.2)
					if l.ch == '.' {
						next := l.peekChar()
						if isDigit(next) {
							// Continue reading the format spec with readFormatSpec
							literal = l.readFormatSpec(literal)
						}
					}
					tok = token.FormatSpec
				}
			}
		} else if isDigit(ch) {
			var isFloat bool
			literal, isFloat = l.readNumber()
			// Check if number is followed by a format letter (e.g., 1X, 2H)
			if !isFloat && isFormatLetter(l.ch) {
				literal = l.readFormatSpec(literal)
				tok = token.FormatSpec
			} else if isFloat {
				tok = token.FloatLit
			} else {
				tok = token.IntLit
			}
		} else {
			tok = token.Illegal
			l.readChar()
		}
	}
	return tok, startPos, literal
}

func (l *Lexer90) readUntil(stopchar rune) ([]byte, token.Token) {
	start := l.bufstart()
	var isInteger, isFloat bool = true, true
	for l.err == nil && l.ch != stopchar {
		isLetter := isPrintableASCII(l.ch)
		isDigitOrDec := isDigitOrDecimal(l.ch)
		isSpace := l.ch == ' '
		if !isDigitOrDec && !isLetter && !isSpace {
			break
		}
		isInteger = isInteger && !isLetter && l.ch != '.' && !isSpace
		isFloat = isFloat && !isLetter && !isSpace
		l.idbuf = utf8.AppendRune(l.idbuf, l.ch)
		l.readChar()
	}
	// If we hit EOF but l.ch has the last valid character, append it
	if l.err != nil {
		if l.ch != 0 && l.ch != stopchar {
			isLetter := isPrintableASCII(l.ch)
			isDigitOrDec := isDigitOrDecimal(l.ch)
			isSpace := l.ch == ' '
			if isDigitOrDec || isLetter || isSpace {
				l.idbuf = utf8.AppendRune(l.idbuf, l.ch)
			}
		}
		return l.idbuf[start:], token.Illegal
	}
	tokst := token.Identifier
	literal := l.idbuf[start:]
	if isFloat {
		tokst = token.FloatLit
	} else if isInteger {
		tokst = token.IntLit
	} else {
		literal = bytes.TrimSpace(literal)
	}
	return literal, tokst
}

// readCommentContent reads comment text until newline without processing line continuations.
// In Fortran, '&' inside comments is just regular text, not a continuation character.
func (l *Lexer90) readCommentContent() []byte {
	start := l.bufstart()
	for l.err == nil && l.ch != '\n' && l.ch != 0 {
		l.idbuf = utf8.AppendRune(l.idbuf, l.ch)
		l.readCharLL() // Use low-level read to avoid continuation handling
	}
	// If we hit EOF but l.ch has the last valid character, append it
	if l.err != nil && l.ch != 0 && l.ch != '\n' {
		l.idbuf = utf8.AppendRune(l.idbuf, l.ch)
	}
	return l.idbuf[start:]
}

func (l *Lexer90) readIdentifier() []byte {
	start := l.bufstart()
	for isIdentifierChar(l.ch) || isDigit(l.ch) {
		l.idbuf = utf8.AppendRune(l.idbuf, l.ch)
		l.readChar()
	}
	return l.idbuf[start:]
}

func (l *Lexer90) readString(quote rune) []byte {
	start := l.bufstart()
	l.readChar() // consume opening quote
	for l.ch != quote && l.ch != 0 {
		if l.ch == '\n' {
			// Newline without continuation - unterminated string
			l.err = errors.New("unterminated string literal")
			return l.idbuf[start:]
		}
		l.idbuf = utf8.AppendRune(l.idbuf, l.ch)
		l.readChar()
	}
	if l.ch == quote {
		l.readChar() // consume closing quote
	}
	return l.idbuf[start:]
}

func (l *Lexer90) readDotOperator() []byte {
	start := l.bufstart()
	l.readChar() // consume opening '.'
	for isIdentifierChar(l.ch) {
		l.idbuf = utf8.AppendRune(l.idbuf, l.ch)
		l.readChar()
	}
	if l.ch == '.' {
		l.readChar() // consume closing '.'
	}
	return l.idbuf[start:]
}

func (l *Lexer90) lookupKeyword(ident []byte) token.Token {
	// Convert to uppercase for case-insensitive comparison
	upper := bytes.ToUpper(ident)
	switch string(upper) {
	case "PROGRAM":
		return token.PROGRAM
	case "SUBROUTINE":
		return token.SUBROUTINE
	case "INTEGER":
		return token.INTEGER
	case "CHARACTER":
		return token.CHARACTER
	case "COMPLEX":
		return token.COMPLEX
	case "LOGICAL":
		return token.LOGICAL
	case "REAL":
		return token.REAL
	case "DATA":
		return token.DATA
	case "EXTERNAL":
		return token.EXTERNAL
	case "IMPLICIT":
		return token.IMPLICIT
	case "FUNCTION":
		return token.FUNCTION
	case "END":
		return token.END
	case "ELSEIF", "ELSE IF":
		return token.ELSEIF
	case "DO":
		return token.DO
	case "DIMENSION":
		return token.DIMENSION
	case "DOUBLE":
		return token.DOUBLE
	case "DOUBLEPRECISION":
		return token.DOUBLEPRECISION
	case "CALL":
		return token.CALL
	case "THEN":
		return token.THEN
	case "WRITE":
		return token.WRITE
	case "PRINT":
		return token.PRINT
	case "WHILE":
		return token.WHILE
	case "PARAMETER":
		return token.PARAMETER
	case "PRECISION":
		return token.PRECISION
	case "INTRINSIC":
		return token.INTRINSIC
	case "FORMAT":
		return token.FORMAT
	case "STOP":
		return token.STOP
	case "SAVE":
		return token.SAVE
	case "OPEN":
		return token.OPEN
	case "READ":
		return token.READ
	case "ASSIGN":
		return token.ASSIGN
	case "DEFINE":
		return token.DEFINE
	case "CLOSE":
		return token.CLOSE
	case "EQUIVALENCE":
		return token.EQUIVALENCE
	case "COMMON":
		return token.COMMON
	case "REWIND":
		return token.REWIND
	case "INCLUDE":
		return token.INCLUDE
	case "IF":
		return token.IF
	case "ELSE":
		return token.ELSE
	case "ENDIF", "END IF":
		return token.ENDIF
	case "GOTO", "GO TO":
		return token.GOTO
	case "CONTINUE":
		return token.CONTINUE
	case "RETURN":
		return token.RETURN
	case "SELECT":
		return token.SELECT
	case "CASE":
		return token.CASE
	case "DEFAULT":
		return token.DEFAULT
	case "CYCLE":
		return token.CYCLE
	case "EXIT":
		return token.EXIT
	case "ENDDO", "END DO":
		return token.ENDDO
	case "ENDPROGRAM", "END PROGRAM":
		return token.ENDPROGRAM
	case "ENDSUBROUTINE", "END SUBROUTINE":
		return token.ENDSUBROUTINE
	case "ENDFUNCTION", "END FUNCTION":
		return token.ENDFUNCTION
	case "ENDSELECT", "END SELECT":
		return token.ENDSELECT
	case "MODULE":
		return token.MODULE
	case "ENDMODULE", "END MODULE":
		return token.ENDMODULE
	case "USE":
		return token.USE
	case "ONLY":
		return token.ONLY
	case "CONTAINS":
		return token.CONTAINS
	case "INTERFACE":
		return token.INTERFACE
	case "ENDINTERFACE", "END INTERFACE":
		return token.ENDINTERFACE
	case "TYPE":
		return token.TYPE
	case "ENDTYPE", "END TYPE":
		return token.ENDTYPE
	case "SEQUENCE":
		return token.SEQUENCE
	case "PRIVATE":
		return token.PRIVATE
	case "PUBLIC":
		return token.PUBLIC
	case "INTENT":
		return token.INTENT
	case "IN":
		return token.IN
	case "OUT":
		return token.OUT
	case "INOUT":
		return token.INOUT
	case "OPTIONAL":
		return token.OPTIONAL
	case "POINTER":
		return token.POINTER
	case "TARGET":
		return token.TARGET
	case "ALLOCATABLE":
		return token.ALLOCATABLE
	case "RECURSIVE":
		return token.RECURSIVE
	case "ELEMENTAL":
		return token.ELEMENTAL
	case "PURE":
		return token.PURE
	case "RESULT":
		return token.RESULT
	case "KIND":
		return token.KIND
	case "LEN":
		return token.LEN
	case "ALLOCATE":
		return token.ALLOCATE
	case "DEALLOCATE":
		return token.DEALLOCATE
	case "NULLIFY":
		return token.NULLIFY
	case "WHERE":
		return token.WHERE
	case "ELSEWHERE", "ELSE WHERE":
		return token.ELSEWHERE
	case "ENDWHERE", "END WHERE":
		return token.ENDWHERE
	case "INQUIRE":
		return token.INQUIRE
	case "BACKSPACE":
		return token.BACKSPACE
	case "ENDFILE":
		return token.ENDFILE
	case "NAMELIST":
		return token.NAMELIST
	default:
		return token.Identifier
	}
}

func (l *Lexer90) lookupDotOperator(ident []byte) token.Token {
	// Convert to uppercase for case-insensitive comparison
	upper := bytes.ToUpper(ident)
	switch string(upper) {
	case "TRUE":
		return token.TRUE
	case "FALSE":
		return token.FALSE
	case "EQ":
		return token.EQ
	case "NE":
		return token.NE
	case "LT":
		return token.LT
	case "LE":
		return token.LE
	case "GT":
		return token.GT
	case "GE":
		return token.GE
	case "AND":
		return token.AND
	case "OR":
		return token.OR
	case "NOT":
		return token.NOT
	case "EQV":
		return token.EQV
	case "NEQV":
		return token.NEQV
	default:
		return token.Illegal
	}
}

func (l *Lexer90) readNumber() ([]byte, bool) {
	start := l.bufstart()
	seenDot := false
	if l.ch == '-' {
		// Consume leading negative character.
		l.idbuf = utf8.AppendRune(l.idbuf, l.ch)
		l.readChar()
	}

	// Read mantissa (integer and decimal parts)
	for {
		ch := l.ch
		if !isDigit(ch) {
			if !seenDot && ch == '.' {
				// Peek at what comes after the '.'
				next := l.peekChar()
				// The '.' is part of the number if next is:
				// - a digit (e.g., 1.5)
				// - E/D/Q for exponent (e.g., 1.E5, 100.D0, 1.Q0)
				// - whitespace/operator (e.g., 1. + 2)
				// The '.' is NOT part of the number if next is:
				// - a letter that's not E/D/Q (e.g., 1.OR., 1.AND.)
				if isIdentifierChar(next) && next != 'E' && next != 'e' && next != 'D' && next != 'd' && next != 'Q' && next != 'q' {
					// The '.' starts a dot operator, don't consume it
					break
				}
				// Consume the '.'
				seenDot = true
				l.idbuf = utf8.AppendRune(l.idbuf, l.ch)
				l.readChar()
				continue
			} else if ch == 'E' || ch == 'e' || ch == 'D' || ch == 'd' || ch == 'Q' || ch == 'q' {
				// Handle scientific notation exponent (e.g., 1.5E3, 100.D0, 1.Q0)
				seenDot = true // Numbers with exponents are always floats
				l.idbuf = utf8.AppendRune(l.idbuf, l.ch)
				l.readChar()

				// Check for optional sign in exponent
				if l.ch == '+' || l.ch == '-' {
					l.idbuf = utf8.AppendRune(l.idbuf, l.ch)
					l.readChar()
				}

				// Read exponent digits
				for isDigit(l.ch) {
					l.idbuf = utf8.AppendRune(l.idbuf, l.ch)
					l.readChar()
				}
				break
			} else {
				break
			}
		}
		l.idbuf = utf8.AppendRune(l.idbuf, l.ch)
		l.readChar()
	}
	return l.idbuf[start:], seenDot
}

func (l *Lexer90) bufstart() int {
	const reuseMem = true
	if reuseMem {
		l.idbuf = l.idbuf[:0]
		return 0
	}
	return len(l.idbuf)
}

func (l *Lexer90) skipWhitespace() {
	for isWhitespace(l.ch) {
		l.readChar()
	}
}

// readChar reads the next character with line continuation processing.
// Fortran 90 line continuation: '&' at end of line continues to next line.
//
// IMPORTANT: This should be used for normal code and strings, but NOT for comments.
// - Normal code/strings: '&\n' is a continuation character
// - Comments: '&' is just literal text (use readCharLL instead)
func (l *Lexer90) readChar() {
	if l.err != nil {
		l.ch = 0 // Just in case annihilate char.
		return
	}
	l.readCharLL()

	// Handle Fortran 90 line continuation: '&' followed by '\n'
	// The continuation, along with any surrounding whitespace and optional '&'
	// on the next line, is consumed transparently.
	for l.err == nil && l.ch == '&' && l.peek == '\n' {
		// Consume continuation line commence string "&\n"
		l.readCharLL()
		l.readCharLL()

		// Skip any comment lines between continuations
		// In Fortran, comment lines can appear between continuation lines
		for l.err == nil && l.ch == '!' {
			// Skip entire comment line
			for l.err == nil && l.ch != '\n' && l.ch != 0 {
				l.readCharLL()
			}
			// Consume the newline
			if l.ch == '\n' {
				l.readCharLL()
			}
		}

		// Skip whitespace on continuation line
		for l.err == nil && (l.ch == ' ' || l.ch == '\t') {
			l.readCharLL()
		}
		// Consume continuation line ampersand if found
		if l.err == nil && l.ch == '&' && l.peek != '\n' {
			l.readCharLL()
		}
	}
}

func (l *Lexer90) peekChar() rune {
	return l.peek
}

// readCharLL reads the next character at the lowest level without any processing.
// This is the raw character reader that does NOT handle line continuations.
// Use this directly only when you need to read characters where '&' should be literal
// (e.g., inside comments). For normal code, use readChar() instead.
func (l *Lexer90) readCharLL() {
	// Advance character buffer first, so even on EOF we don't lose the last char
	l.ch = l.peek

	ch, sz, err := l.input.ReadRune()
	if err != nil {
		l.peek = 0
		l.err = err
		return
	}
	if ch == '\n' {
		l.line++
		l.col = 0
	} else {
		l.col++
	}
	l.pos += sz
	l.peek = ch
}

// PositionString returns the "source:line:column" representation of the lexer's current position.
func (l *Lexer90) PositionString() string {
	return string(l.AppendPositionString(make([]byte, 0, len(l.source)+1+3+1+1))) // Enough space for 3 digit line error
}

// AppendPositionString appends [Lexer90.PositionString] to the buffer and returns the result.
func (l *Lexer90) AppendPositionString(b []byte) []byte {
	sp := l.sourcePos()
	return sp.AppendString(b)
}

func (l *Lexer90) sourcePos() sourcePos {
	line, col := l.LineCol()
	return sourcePos{
		Source: l.source,
		Line:   line,
		Col:    col,
		Pos:    l.pos,
	}
}

func isIdentifierChar(ch rune) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

func isDigit(ch rune) bool {
	return '0' <= ch && ch <= '9'
}

func isDigitOrDecimal(ch rune) bool {
	return ch == '.' || isDigit(ch)
}

func isWhitespace(ch rune) bool {
	return ch == ' ' || ch == '\t' || ch == '\r'
}

// isFormatLetter returns true if ch is a letter commonly used in Fortran format specifiers.
// This includes data edit descriptors (I, F, E, D, G, A, L, B, O, Z),
// positioning (X, T, P), and Hollerith (H - deprecated).
func isFormatLetter(ch rune) bool {
	switch ch {
	case 'I', 'i', 'F', 'f', 'E', 'e', 'D', 'd', 'G', 'g',
		'A', 'a', 'L', 'l', 'B', 'b', 'O', 'o', 'Z', 'z',
		'X', 'x', 'T', 't', 'P', 'p', 'H', 'h':
		return true
	default:
		return false
	}
}

// readFormatSpec reads a Fortran format specifier starting from the given prefix.
// Format specifiers can have patterns like:
//   - nX (repeat count + letter): 1X, 5H
//   - Iw[.m]: I3, I5.2
//   - Fw.d: F10.2
//   - Ew.d[Ee]: E12.5, E12.5E3
func (l *Lexer90) readFormatSpec(prefix []byte) []byte {
	start := l.bufstart()
	// Copy prefix into buffer
	l.idbuf = append(l.idbuf[:start], prefix...)

	// Read any letters (for multi-letter format codes like TL, TR)
	for isFormatLetter(l.ch) {
		l.idbuf = utf8.AppendRune(l.idbuf, l.ch)
		l.readChar()
	}

	// Read width (digits)
	for isDigit(l.ch) {
		l.idbuf = utf8.AppendRune(l.idbuf, l.ch)
		l.readChar()
	}

	// Read optional decimal point and precision
	if l.ch == '.' {
		l.idbuf = utf8.AppendRune(l.idbuf, l.ch)
		l.readChar()
		for isDigit(l.ch) {
			l.idbuf = utf8.AppendRune(l.idbuf, l.ch)
			l.readChar()
		}
	}

	// Read optional exponent width (E.g., E12.5E3)
	if l.ch == 'E' || l.ch == 'e' {
		l.idbuf = utf8.AppendRune(l.idbuf, l.ch)
		l.readChar()
		for isDigit(l.ch) {
			l.idbuf = utf8.AppendRune(l.idbuf, l.ch)
			l.readChar()
		}
	}

	return l.idbuf[start:]
}

func isPrintableASCII(ch rune) bool {
	return ch >= 32 && ch <= 126
}
