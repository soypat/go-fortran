package fortran

import (
	"io"
	"strconv"

	"github.com/soypat/go-fortran/ast"
	"github.com/soypat/go-fortran/token"
)

// Pratt parsing functions. a.k.a: Semantic Code.
type (
	prefixParseFn    func() ast.Expression
	infixParseFn     func(ast.Expression) ast.Expression
	statementParseFn func() ast.Statement // For statement-level constructs
)

type ParserError struct {
	sp  sourcePos
	msg string
}

func (pe *ParserError) Error() string {
	var dst []byte
	dst = pe.sp.AppendString(dst)
	dst = append(dst, ':', ' ')
	dst = append(dst, pe.msg...)
	return string(dst)
}

type sourcePos struct {
	Source string
	Line   int
	Col    int
	Pos    int
}

func (l *sourcePos) String() string {
	return string(l.AppendString(nil))
}

func (l *sourcePos) AppendString(b []byte) []byte {
	if b == nil {
		b = make([]byte, 0, len(l.Source)+3+3)
	}
	b = append(b, l.Source...)
	b = append(b, ':')

	b = strconv.AppendInt(b, int64(l.Line), 10)
	if l.Col > 0 {
		b = append(b, ':')
		b = strconv.AppendInt(b, int64(l.Col), 10)
	}
	return b
}

type Parser90 struct {
	l       Lexer90
	current toktuple
	peek    toktuple
	pfxFns  map[token.Token]prefixParseFn
	infxFns map[token.Token]infixParseFn
	stmtFns map[token.Token]statementParseFn // Statement parsers
	errors  []ParserError                    // Collected parsing errors
}

func (p *Parser90) Reset(source string, r io.Reader) error {
	err := p.l.Reset(source, r)
	if err != nil {
		return err
	}
	if p.pfxFns == nil {
		p.pfxFns = make(map[token.Token]prefixParseFn)
		p.infxFns = make(map[token.Token]infixParseFn)
		p.stmtFns = make(map[token.Token]statementParseFn)
	}
	*p = Parser90{
		l: p.l,
		// Reuse memory but clear later.
		pfxFns:  p.pfxFns,
		infxFns: p.infxFns,
		stmtFns: p.stmtFns,
		errors:  p.errors[:0], // Reuse slice, clear contents
	}
	clear(p.pfxFns)
	clear(p.infxFns)
	clear(p.stmtFns)

	// Initialize token stream
	p.nextToken()
	p.nextToken()

	// Register all parsing functions
	p.registerTopLevelParsers()

	return nil
}

func (p *Parser90) nextToken() {
	tok, start, lit := p.l.NextToken()
	// Cycle buffers between current and peek.
	currBuf := p.current.lit // is clobbered by peek, reused by new peek.
	p.current = p.peek

	p.peek.lit = append(currBuf[:0], lit...)
	p.peek.start = start
	p.peek.tok = tok
	if p.current.tok == token.Illegal {

	}
}

func (p *Parser90) registerPrefix(tokenType token.Token, fn prefixParseFn) {
	p.pfxFns[tokenType] = fn
}

func (p *Parser90) registerInfix(tokenType token.Token, fn infixParseFn) {
	p.infxFns[tokenType] = fn
}

func (p *Parser90) registerStatement(tokenType token.Token, fn statementParseFn) {
	p.stmtFns[tokenType] = fn
}

type toktuple struct {
	tok   token.Token
	start int
	lit   []byte
}

// ParseNextProgramUnit parses and returns the next program unit from the input.
// Returns nil when EOF is reached or no more units are available.
// This method can be called repeatedly to incrementally parse a Fortran file.
// Phase 1: Parses only top-level program units (PROGRAM, SUBROUTINE, FUNCTION, MODULE)
func (p *Parser90) ParseNextProgramUnit() ast.ProgramUnit {
	// Skip leading newlines and comments
	p.skipNewlinesAndComments()

	// Check for EOF
	if p.currentTokenIs(token.EOF) {
		return nil
	}

	// Parse and return one program unit
	unit := p.parseTopLevelUnit()

	// Skip trailing newlines after this unit
	p.skipNewlinesAndComments()

	return unit
}

// registerTopLevelParsers registers all statement-level parsing functions
func (p *Parser90) registerTopLevelParsers() {
	// Register top-level keywords
	p.registerStatement(token.PROGRAM, p.parseProgramBlock)
	p.registerStatement(token.SUBROUTINE, p.parseSubroutine)
	p.registerStatement(token.FUNCTION, p.parseFunction)
	p.registerStatement(token.MODULE, p.parseModule)

	// Register type keywords that can prefix FUNCTION
	p.registerStatement(token.INTEGER, p.parseTypePrefixedConstruct)
	p.registerStatement(token.REAL, p.parseTypePrefixedConstruct)
	p.registerStatement(token.LOGICAL, p.parseTypePrefixedConstruct)
	p.registerStatement(token.CHARACTER, p.parseTypePrefixedConstruct)
	p.registerStatement(token.DOUBLE, p.parseTypePrefixedConstruct)
	p.registerStatement(token.COMPLEX, p.parseTypePrefixedConstruct)

	// Register attributes that can prefix procedures
	p.registerStatement(token.RECURSIVE, p.parseProcedureWithAttributes)
	p.registerStatement(token.PURE, p.parseProcedureWithAttributes)
	p.registerStatement(token.ELEMENTAL, p.parseProcedureWithAttributes)
}

// parseTopLevelUnit dispatches to the appropriate registered statement parser
func (p *Parser90) parseTopLevelUnit() ast.ProgramUnit {
	// Special case: BLOCK DATA (BLOCK is an identifier, DATA is a keyword)
	if p.currentTokenIs(token.Identifier) && string(p.current.lit) == "BLOCK" && p.peekTokenIs(token.DATA) {
		return p.parseBlockData()
	}

	stmtFn := p.stmtFns[p.current.tok]
	if stmtFn == nil {
		p.addError("unexpected token at top level: " + p.current.tok.String())
		p.nextToken() // Skip unexpected token
		return nil
	}

	stmt := stmtFn()
	if unit, ok := stmt.(ast.ProgramUnit); ok {
		return unit
	}

	p.addError("statement is not a program unit")
	return nil
}

// Helper methods

func (p *Parser90) currentTokenIs(t token.Token) bool {
	return p.current.tok == t
}

func (p *Parser90) peekTokenIs(t token.Token) bool {
	return p.peek.tok == t
}

func (p *Parser90) expectCurrent(t token.Token) bool {
	if !p.currentTokenIs(t) {
		p.addError("expected " + t.String() + ", got " + p.current.tok.String())
		return false
	}
	return true
}

func (p *Parser90) skipNewlinesAndComments() {
	for p.currentTokenIs(token.NewLine) || p.currentTokenIs(token.LineComment) {
		p.nextToken()
	}
}

func (p *Parser90) addError(msg string) {
	p.errors = append(p.errors, ParserError{
		sp:  p.l.sourcePos(),
		msg: msg,
	})
}

func (p *Parser90) Errors() []ParserError {
	return p.errors
}

func (p *Parser90) isTypeKeyword(t token.Token) bool {
	return t.IsTypeDeclaration()
}

func (p *Parser90) isAttributeKeyword(t token.Token) bool {
	return t == token.RECURSIVE || t == token.PURE || t == token.ELEMENTAL
}

// parseParameterList parses a parameter list like (a, b, c)
// In Fortran, keywords can be used as variable names
func (p *Parser90) parseParameterList() []string {
	params := []string{}

	if !p.expectCurrent(token.LParen) {
		return params
	}
	p.nextToken()

	// Empty parameter list
	if p.currentTokenIs(token.RParen) {
		p.nextToken()
		return params
	}

	// Parse parameters
	for !p.currentTokenIs(token.RParen) && !p.currentTokenIs(token.EOF) {
		// Accept identifiers or keywords as parameter names
		if p.currentTokenIs(token.Identifier) || len(p.current.lit) > 0 {
			params = append(params, string(p.current.lit))
			p.nextToken()

			if p.currentTokenIs(token.Comma) {
				p.nextToken()
				continue
			}

			if p.currentTokenIs(token.RParen) {
				break
			}

			p.addError("expected comma or closing paren in parameter list")
			break
		} else {
			p.addError("expected parameter name")
			break
		}
	}

	if p.expectCurrent(token.RParen) {
		p.nextToken()
	}

	return params
}

// collectUntilEnd collects tokens until one of the specified end tokens is found
// Returns the collected tokens (excluding the end token)
func (p *Parser90) collectUntilEnd(endTokens ...token.Token) []ast.TokenTuple {
	tokens := []ast.TokenTuple{}

	for !p.currentTokenIs(token.EOF) {
		// Check if we've hit an end token
		for _, endTok := range endTokens {
			if p.currentTokenIs(endTok) {
				return tokens
			}
		}

		// Collect the token
		tokens = append(tokens, ast.TokenTuple{
			Tok:   p.current.tok,
			Start: p.current.start,
			Lit:   append([]byte{}, p.current.lit...), // Copy the byte slice
		})

		p.nextToken()
	}

	return tokens
}

// collectBodyUntilEnd collects body tokens for a program unit, handling nested END statements
// endKeyword is the token that should follow END to terminate collection (e.g., PROGRAM, MODULE, SUBROUTINE, FUNCTION)
// If endKeyword is token.Illegal, any END not followed by a keyword will terminate
func (p *Parser90) collectBodyUntilEnd(endKeyword token.Token) []ast.TokenTuple {
	tokens := []ast.TokenTuple{}

	for !p.currentTokenIs(token.EOF) {
		// Check for CONTAINS - for modules/subprograms with contained procedures
		if p.currentTokenIs(token.CONTAINS) {
			return tokens
		}

		// Check for END - could be the unit's END or a nested END
		if p.currentTokenIs(token.END) {
			nextTok := p.peek.tok

			// Check if this is the END we're looking for
			if endKeyword != token.Illegal {
				// Looking for specific END KEYWORD (e.g., END SUBROUTINE)
				if nextTok == endKeyword {
					return tokens // Found exact match
				}
				// Also accept bare END or END followed by identifier (the subprogram name)
				// But NOT if followed by another top-level keyword (PROGRAM, MODULE, SUBROUTINE, FUNCTION)
				if nextTok == token.NewLine || nextTok == token.EOF || nextTok == token.Identifier {
					return tokens // Bare END or END <name>
				}
				// If followed by PROGRAM, MODULE, SUBROUTINE, or FUNCTION, this must be ending
				// a different (parent) construct, so treat current unit as ended
				if nextTok == token.PROGRAM || nextTok == token.MODULE ||
					nextTok == token.SUBROUTINE || nextTok == token.FUNCTION {
					return tokens
				}
			} else {
				// Looking for bare END or END followed by newline/EOF
				if nextTok == token.NewLine || nextTok == token.EOF {
					return tokens
				}
			}

			// This is a nested END (END DO, END IF, END TYPE, etc.)
			// Collect both the END and the following keyword/identifier
			tokens = append(tokens, ast.TokenTuple{
				Tok:   p.current.tok,
				Start: p.current.start,
				Lit:   append([]byte{}, p.current.lit...),
			})
			p.nextToken()

			// Collect the token after END
			if !p.currentTokenIs(token.EOF) && !p.currentTokenIs(token.NewLine) {
				tokens = append(tokens, ast.TokenTuple{
					Tok:   p.current.tok,
					Start: p.current.start,
					Lit:   append([]byte{}, p.current.lit...),
				})
				p.nextToken()
			}
			continue
		}

		// Collect the token
		tokens = append(tokens, ast.TokenTuple{
			Tok:   p.current.tok,
			Start: p.current.start,
			Lit:   append([]byte{}, p.current.lit...),
		})

		p.nextToken()
	}

	return tokens
}

// Semantic parsing functions for top-level constructs

// parseProgramBlock parses a PROGRAM...END PROGRAM block
func (p *Parser90) parseProgramBlock() ast.Statement {
	block := &ast.ProgramBlock{StartPos: p.current.start}

	// Consume PROGRAM keyword
	p.nextToken()

	// Parse program name (can be Identifier or FormatSpec)
	if !p.currentTokenIs(token.Identifier) && !p.currentTokenIs(token.FormatSpec) {
		p.addError("expected program name, got " + p.current.tok.String())
		return nil
	}
	block.Name = string(p.current.lit)
	p.nextToken()

	p.skipNewlinesAndComments()

	// Collect body tokens until END PROGRAM
	block.BodyTokens = p.collectBodyUntilEnd(token.PROGRAM)

	block.EndPos = p.current.start
	p.nextToken() // Move past END

	// Consume optional PROGRAM keyword after END
	if p.currentTokenIs(token.PROGRAM) {
		p.nextToken()
		// Skip optional program name after END PROGRAM
		if p.currentTokenIs(token.Identifier) {
			p.nextToken()
		}
	} else if p.currentTokenIs(token.Identifier) {
		// Bare END followed by program name
		p.nextToken()
	}

	return block
}

// parseSubroutine parses a SUBROUTINE...END SUBROUTINE block
func (p *Parser90) parseSubroutine() ast.Statement {
	sub := &ast.Subroutine{StartPos: p.current.start}

	// Consume SUBROUTINE keyword
	p.nextToken()

	// Parse subroutine name (can be Identifier or FormatSpec)
	if !p.currentTokenIs(token.Identifier) && !p.currentTokenIs(token.FormatSpec) {
		p.addError("expected subroutine name, got " + p.current.tok.String())
		return nil
	}
	sub.Name = string(p.current.lit)
	p.nextToken()

	// Parse parameter list if present
	if p.currentTokenIs(token.LParen) {
		sub.Parameters = p.parseParameterList()
	}

	p.skipNewlinesAndComments()

	// Collect body tokens until END SUBROUTINE
	sub.BodyTokens = p.collectBodyUntilEnd(token.SUBROUTINE)

	sub.EndPos = p.current.start
	p.nextToken() // Move past END

	// Consume optional SUBROUTINE keyword after END
	if p.currentTokenIs(token.SUBROUTINE) {
		p.nextToken()
		// Skip optional subroutine name after END SUBROUTINE
		if p.currentTokenIs(token.Identifier) {
			p.nextToken()
		}
	} else if p.currentTokenIs(token.Identifier) {
		// Bare END followed by subroutine name
		p.nextToken()
	}

	return sub
}

// parseFunction parses a FUNCTION...END FUNCTION block
func (p *Parser90) parseFunction() ast.Statement {
	fn := &ast.Function{StartPos: p.current.start}

	// Consume FUNCTION keyword
	p.nextToken()

	// Parse function name (can be Identifier or FormatSpec token like D71, I3, etc.)
	if !p.currentTokenIs(token.Identifier) && !p.currentTokenIs(token.FormatSpec) {
		p.addError("expected function name, got " + p.current.tok.String())
		return nil
	}
	fn.Name = string(p.current.lit)
	p.nextToken()

	// Parse parameter list
	if p.currentTokenIs(token.LParen) {
		fn.Parameters = p.parseParameterList()
	}

	// Check for RESULT clause
	if p.currentTokenIs(token.RESULT) {
		p.nextToken()
		if p.expectCurrent(token.LParen) {
			p.nextToken()
			if p.expectCurrent(token.Identifier) {
				fn.ResultVariable = string(p.current.lit)
				p.nextToken()
			}
			if p.expectCurrent(token.RParen) {
				p.nextToken()
			}
		}
	}

	p.skipNewlinesAndComments()

	// Collect body tokens until END FUNCTION
	fn.BodyTokens = p.collectBodyUntilEnd(token.FUNCTION)

	fn.EndPos = p.current.start
	p.nextToken() // Move past END

	// Consume optional FUNCTION keyword after END
	if p.currentTokenIs(token.FUNCTION) {
		p.nextToken()
		// Skip optional function name after END FUNCTION
		if p.currentTokenIs(token.Identifier) {
			p.nextToken()
		}
	} else if p.currentTokenIs(token.Identifier) {
		// Bare END followed by function name
		p.nextToken()
	}

	return fn
}

// parseModule parses a MODULE...END MODULE block
func (p *Parser90) parseModule() ast.Statement {
	mod := &ast.Module{StartPos: p.current.start}

	// Consume MODULE keyword
	p.nextToken()

	// Parse module name
	if !p.expectCurrent(token.Identifier) {
		return nil
	}
	mod.Name = string(p.current.lit)
	p.nextToken()

	p.skipNewlinesAndComments()

	// Collect body tokens until CONTAINS or END MODULE
	// Must handle nested END statements (END TYPE, END INTERFACE, etc.)
	mod.BodyTokens = p.collectBodyUntilEnd(token.MODULE)

	// Handle CONTAINS section with recursive parsing
	if p.currentTokenIs(token.CONTAINS) {
		p.nextToken()
		p.skipNewlinesAndComments()

		// Recursively parse contained procedures
		for !p.currentTokenIs(token.END) &&
			!p.currentTokenIs(token.EOF) {

			// Check if this is a procedure keyword
			if p.currentTokenIs(token.SUBROUTINE) ||
				p.currentTokenIs(token.FUNCTION) ||
				p.isAttributeKeyword(p.current.tok) ||
				p.isTypeKeyword(p.current.tok) {

				unit := p.parseTopLevelUnit()
				if unit != nil {
					mod.Contains = append(mod.Contains, unit)
				}
			} else {
				p.nextToken()
			}

			p.skipNewlinesAndComments()
		}
	}

	mod.EndPos = p.current.start
	p.nextToken() // Move past END

	// Consume optional MODULE keyword after END
	if p.currentTokenIs(token.MODULE) {
		p.nextToken()
		// Skip optional module name after END MODULE
		if p.currentTokenIs(token.Identifier) {
			p.nextToken()
		}
	} else if p.currentTokenIs(token.Identifier) {
		// Bare END followed by module name
		p.nextToken()
	}

	return mod
}

// parseBlockData parses a BLOCK DATA...END [BLOCK DATA] block
func (p *Parser90) parseBlockData() ast.ProgramUnit {
	bd := &ast.BlockData{StartPos: p.current.start}

	// Consume BLOCK identifier
	p.nextToken()

	// Consume DATA keyword
	if !p.expectCurrent(token.DATA) {
		return nil
	}
	p.nextToken()

	// Parse optional block data name
	if p.currentTokenIs(token.Identifier) {
		bd.Name = string(p.current.lit)
		p.nextToken()
	}

	p.skipNewlinesAndComments()

	// Collect body tokens until END
	// BLOCK DATA can end with: END, END BLOCK DATA, or END BLOCKDATA
	bd.BodyTokens = p.collectBlockDataBody()

	bd.EndPos = p.current.start
	p.nextToken() // Move past END

	// Consume optional BLOCK DATA after END
	if p.currentTokenIs(token.Identifier) && string(p.current.lit) == "BLOCK" {
		p.nextToken()
		if p.currentTokenIs(token.DATA) {
			p.nextToken()
			// Optional name after END BLOCK DATA
			if p.currentTokenIs(token.Identifier) {
				p.nextToken()
			}
		}
	} else if p.currentTokenIs(token.Identifier) {
		// Bare END followed by block data name
		p.nextToken()
	}

	return bd
}

// collectBlockDataBody collects tokens until END
func (p *Parser90) collectBlockDataBody() []ast.TokenTuple {
	tokens := []ast.TokenTuple{}

	for !p.currentTokenIs(token.EOF) {
		if p.currentTokenIs(token.END) {
			// BLOCK DATA ends with END (with optional BLOCK DATA or name after)
			return tokens
		}

		tokens = append(tokens, ast.TokenTuple{
			Tok:   p.current.tok,
			Start: p.current.start,
			Lit:   append([]byte{}, p.current.lit...),
		})

		p.nextToken()
	}

	return tokens
}

// parseTypePrefixedConstruct handles type-prefixed functions like "INTEGER FUNCTION foo()"
func (p *Parser90) parseTypePrefixedConstruct() ast.Statement {
	// Save the type token
	typeToken := p.current.tok
	typeSpec := typeToken.String()
	p.nextToken()

	// Handle type length/kind specifiers: CHARACTER*4, REAL*8, INTEGER*4, etc.
	if p.currentTokenIs(token.Asterisk) {
		typeSpec += "*"
		p.nextToken()
		// Consume the length/kind value
		if p.currentTokenIs(token.IntLit) || p.currentTokenIs(token.LParen) {
			typeSpec += string(p.current.lit)
			p.nextToken()
			// Handle (kind) notation like INTEGER(4) or CHARACTER(LEN=*)
			if p.currentTokenIs(token.LParen) {
				depth := 1
				typeSpec += "("
				p.nextToken()
				for depth > 0 && !p.currentTokenIs(token.EOF) {
					if p.currentTokenIs(token.LParen) {
						depth++
					} else if p.currentTokenIs(token.RParen) {
						depth--
					}
					if depth > 0 {
						typeSpec += string(p.current.lit)
					}
					p.nextToken()
				}
				typeSpec += ")"
			}
		}
	}

	// Check if this is a function
	if p.currentTokenIs(token.FUNCTION) {
		// Parse as function
		fn := p.parseFunction().(*ast.Function)
		fn.ResultType = typeSpec
		return fn
	}

	// Otherwise, this is a type declaration (Phase 2)
	// For now, skip this line
	p.addError("type declarations not yet supported in Phase 1")
	for !p.currentTokenIs(token.NewLine) && !p.currentTokenIs(token.EOF) {
		p.nextToken()
	}
	return nil
}

// parseProcedureWithAttributes handles procedures with attributes like RECURSIVE, PURE, ELEMENTAL
func (p *Parser90) parseProcedureWithAttributes() ast.Statement {
	// Collect all attributes
	attributes := []token.Token{}

	for p.isAttributeKeyword(p.current.tok) {
		attributes = append(attributes, p.current.tok)
		p.nextToken()
	}

	// Now must be SUBROUTINE or FUNCTION
	var stmt ast.Statement
	if p.currentTokenIs(token.SUBROUTINE) {
		stmt = p.parseSubroutine()
		if sub, ok := stmt.(*ast.Subroutine); ok {
			sub.Attributes = attributes
		}
	} else if p.currentTokenIs(token.FUNCTION) {
		stmt = p.parseFunction()
		if fn, ok := stmt.(*ast.Function); ok {
			fn.Attributes = attributes
		}
	} else if p.isTypeKeyword(p.current.tok) {
		// Type-prefixed function with attributes
		typeToken := p.current.tok
		p.nextToken()
		if p.currentTokenIs(token.FUNCTION) {
			stmt = p.parseFunction()
			if fn, ok := stmt.(*ast.Function); ok {
				fn.Attributes = attributes
				fn.ResultType = typeToken.String()
			}
		} else {
			p.addError("expected FUNCTION after type keyword")
			return nil
		}
	} else {
		p.addError("expected SUBROUTINE or FUNCTION after attributes")
		return nil
	}

	return stmt
}
