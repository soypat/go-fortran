package fortran

import (
	"io"
	"strconv"

	"github.com/soypat/go-fortran/ast"
	"github.com/soypat/go-fortran/token"
)

// Pratt parsing functions. a.k.a: Semantic Code.
type (
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

	stmtFns map[token.Token]statementParseFn // Statement parsers
	errors  []ParserError                    // Collected parsing errors
}

func (p *Parser90) Reset(source string, r io.Reader) error {
	err := p.l.Reset(source, r)
	if err != nil {
		return err
	}
	if p.stmtFns == nil {
		p.stmtFns = make(map[token.Token]statementParseFn)
	}
	*p = Parser90{
		l: p.l,
		// Reuse memory but clear later.

		stmtFns: p.stmtFns,
		errors:  p.errors[:0], // Reuse slice, clear contents
	}
	clear(p.stmtFns)

	// Initialize token stream
	p.nextToken()
	p.nextToken()

	// Register all parsing functions
	p.registerTopLevelParsers()

	return nil
}

func (p *Parser90) nextToken() {
	if p.IsDone() {
		return
	}
	tok, start, lit := p.l.NextToken()
	// Get the line/col where this token started
	line, col := p.l.TokenLineCol()
	// Cycle buffers between current and peek.
	currBuf := p.current.lit // is clobbered by peek, reused by new peek.
	p.current = p.peek

	p.peek.lit = append(currBuf[:0], lit...)
	p.peek.start = start
	p.peek.tok = tok
	p.peek.line = line
	p.peek.col = col
}

func (p *Parser90) IsDone() bool {
	return p.l.IsDone() && p.current.tok.IsIllegalOrEOF()
}

func (p *Parser90) registerStatement(tokenType token.Token, fn statementParseFn) {
	p.stmtFns[tokenType] = fn
}

type toktuple struct {
	tok   token.Token
	start int
	lit   []byte
	line  int // Line number where this token starts
	col   int // Column number where this token starts
}

// ParseNextProgramUnit parses and returns the next program unit from the input.
// Returns nil when EOF is reached or no more units are available.
// This method can be called repeatedly to incrementally parse a Fortran file.
// Phase 1: Parses only top-level program units (PROGRAM, SUBROUTINE, FUNCTION, MODULE)
func (p *Parser90) ParseNextProgramUnit() ast.ProgramUnit {
	for {
		// Skip leading newlines and comments
		p.skipNewlinesAndComments()

		// Check for EOF
		if p.currentTokenIs(token.EOF) {
			return nil
		}

		// Parse one program unit
		unit := p.parseTopLevelUnit()

		// Skip trailing newlines after this unit
		p.skipNewlinesAndComments()

		// If parsing succeeded, return the unit
		if unit != nil {
			return unit
		} else if p.IsDone() {
			return nil
		}
	}
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

	// If stmt is nil, the parser already added an error
	if stmt != nil {
		p.addError("statement is not a program unit")
	}
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
		if p.IsDone() {
			break
		}
	}
}

func (p *Parser90) addError(msg string) {
	p.errors = append(p.errors, ParserError{
		sp: sourcePos{
			Source: p.l.source,
			Line:   p.current.line,
			Col:    p.current.col,
			Pos:    p.current.start,
		},
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

// canUseAsIdentifier returns true if the current token can be used as an identifier.
// In Fortran, keywords can be used as variable/function/subroutine names in many contexts.
func (p *Parser90) canUseAsIdentifier() bool {
	// Explicit identifiers are always OK
	if p.currentTokenIs(token.Identifier) || p.currentTokenIs(token.FormatSpec) {
		return true
	}
	// Allow keywords to be used as identifiers
	// We exclude structural keywords that would cause ambiguity
	switch p.current.tok {
	case token.PROGRAM, token.SUBROUTINE, token.FUNCTION, token.MODULE,
		token.END, token.CONTAINS:
		return false
	default:
		// Most other keywords can be used as identifiers
		return p.current.tok.IsKeyword()
	}
}

// parseParameterList parses a parameter list like (a, b, c)
// In Fortran, keywords can be used as variable names
func (p *Parser90) parseParameterList() []ast.Parameter {
	params := []ast.Parameter{}

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
		// Accept identifiers, keywords, or * (alternate return specifier in F77)
		var paramName string
		if p.currentTokenIs(token.Asterisk) {
			paramName = "*"
			p.nextToken()
		} else if p.currentTokenIs(token.Identifier) || len(p.current.lit) > 0 {
			paramName = string(p.current.lit)
			p.nextToken()
		} else {
			p.addError("expected parameter name")
			break
		}

		// Create Parameter with just the name for now
		// Type information will be filled in by parseBody when it sees type declarations
		params = append(params, ast.Parameter{Name: paramName})

		if p.currentTokenIs(token.Comma) {
			p.nextToken()
			continue
		}

		if p.currentTokenIs(token.RParen) {
			break
		}

		p.addError("expected comma or closing paren in parameter list")
		break
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

// parseBody parses specification statements, then executable statements
// If parameters are provided, it will populate their type information when type declarations are found
func (p *Parser90) parseBody(parameters []ast.Parameter) []ast.Statement {
	var stmts []ast.Statement
	var sawImplicit, sawDecl bool

	// Create a map for quick parameter lookup
	paramMap := make(map[string]*ast.Parameter)
	for i := range parameters {
		paramMap[parameters[i].Name] = &parameters[i]
	}

	// Phase 2: Parse specification statements
	for !p.currentTokenIs(token.CONTAINS) && !p.currentTokenIs(token.END) && !p.currentTokenIs(token.EOF) {
		p.skipNewlinesAndComments()
		if p.currentTokenIs(token.CONTAINS) || p.currentTokenIs(token.END) || p.currentTokenIs(token.EOF) {
			break
		}

		// Check if this is an executable statement (execution part starts here)
		if p.isExecutableStatement() {
			break
		}

		if stmt := p.parseSpecStatement(&sawImplicit, &sawDecl, paramMap); stmt != nil {
			stmts = append(stmts, stmt)
		} else {
			// Not a parseable spec statement - skip the construct
			p.skipToNextStatement()
		}
	}

	// Phase 3: Parse executable statements
	for !p.currentTokenIs(token.CONTAINS) && !p.isEndOfProgramUnit() && !p.currentTokenIs(token.EOF) {
		p.skipNewlinesAndComments()
		if p.currentTokenIs(token.CONTAINS) || p.isEndOfProgramUnit() {
			break
		}

		if stmt := p.parseExecutableStatement(); stmt != nil {
			stmts = append(stmts, stmt)
		} else {
			// Not a parseable executable statement - skip the construct
			p.skipToNextStatement()
		}
	}

	return stmts
}

// parseExecutableStatement parses a single executable statement
func (p *Parser90) parseExecutableStatement() ast.Statement {
	var label string
	if p.currentTokenIs(token.Label) {
		label = string(p.current.lit)
		p.nextToken()
	}

	var stmt ast.Statement
	switch p.current.tok {
	case token.IF:
		stmt = p.parseIfStmt()
	case token.DO:
		stmt = p.parseDoLoop()
	case token.CALL:
		stmt = p.parseCallStmt()
	case token.RETURN:
		stmt = p.parseReturnStmt()
	case token.CYCLE:
		stmt = p.parseCycleStmt()
	case token.EXIT:
		stmt = p.parseExitStmt()
	case token.CONTINUE:
		stmt = p.parseContinueStmt()
	case token.Identifier:
		// This could be an assignment statement or a call to a subroutine without the CALL keyword.
		// For now, we'll assume it's an assignment.
		stmt = p.parseAssignmentStmt()
	default:
		return nil // Unknown statement, caller will skip
	}

	if stmt != nil {
		switch s := stmt.(type) {
		case *ast.IfStmt:
			s.Label = label
		case *ast.DoLoop:
			s.Label = label
		case *ast.CallStmt:
			s.Label = label
		case *ast.ReturnStmt:
			s.Label = label
		case *ast.CycleStmt:
			s.Label = label
		case *ast.ExitStmt:
			s.Label = label
		case *ast.ContinueStmt:
			s.Label = label
		case *ast.AssignmentStmt:
			s.Label = label
		}
	}

	return stmt
}

// parseContinueStmt parses a CONTINUE statement
func (p *Parser90) parseContinueStmt() ast.Statement {
	start := p.current.start
	p.nextToken() // consume CONTINUE
	return &ast.ContinueStmt{
		Position: ast.Pos(start, p.current.start),
	}
}

// parseIfStmt parses an IF construct
func (p *Parser90) parseIfStmt() ast.Statement {
	start := p.current.start
	stmt := &ast.IfStmt{}
	p.nextToken() // consume IF

	if !p.currentTokenIs(token.LParen) {
		p.addError("expected '(' after IF")
		return nil
	}
	p.nextToken() // consume (

	stmt.Condition = p.parseExpression(0)
	if stmt.Condition == nil {
		p.addError("expected condition in IF statement")
		return nil
	}

	if !p.currentTokenIs(token.RParen) {
		p.addError("expected ')' after IF condition")
		return nil
	}
	p.nextToken() // consume )

	if !p.currentTokenIs(token.THEN) {
		p.addError("expected THEN after IF condition")
		return nil
	}
	p.nextToken() // consume THEN
	p.skipNewlinesAndComments()

	// Parse THEN block
	for !p.currentTokenIs(token.ELSE) && !p.currentTokenIs(token.END) && !p.currentTokenIs(token.EOF) {
		if p.peekTokenIs(token.IF) && p.currentTokenIs(token.ELSE) {
			break
		}
		if s := p.parseExecutableStatement(); s != nil {
			stmt.ThenPart = append(stmt.ThenPart, s)
		} else {
			p.skipToNextStatement()
		}
		p.skipNewlinesAndComments()
	}

	// Parse ELSE IF parts
	for p.currentTokenIs(token.ELSE) && p.peekTokenIs(token.IF) {
		p.nextToken() // consume ELSE
		p.nextToken() // consume IF

		clause := ast.ElseIfClause{}
		clauseStart := p.current.start
		if !p.currentTokenIs(token.LParen) {
			p.addError("expected '(' after ELSE IF")
			return nil
		}
		p.nextToken() // consume (

		clause.Condition = p.parseExpression(0)
		if clause.Condition == nil {
			p.addError("expected condition in ELSE IF statement")
			return nil
		}

		if !p.currentTokenIs(token.RParen) {
			p.addError("expected ')' after ELSE IF condition")
			return nil
		}
		p.nextToken() // consume )

		if !p.currentTokenIs(token.THEN) {
			p.addError("expected THEN after ELSE IF condition")
			return nil
		}
		p.nextToken() // consume THEN
		p.skipNewlinesAndComments()

		// Parse ELSE IF block
		for !p.currentTokenIs(token.ELSE) && !p.currentTokenIs(token.END) && !p.currentTokenIs(token.EOF) {
			if p.peekTokenIs(token.IF) && p.currentTokenIs(token.ELSE) {
				break
			}
			if s := p.parseExecutableStatement(); s != nil {
				clause.ThenPart = append(clause.ThenPart, s)
			} else {
				p.skipToNextStatement()
			}
			p.skipNewlinesAndComments()
		}
		clause.Position = ast.Pos(clauseStart, p.current.start)
		stmt.ElseIfParts = append(stmt.ElseIfParts, clause)
	}

	// Parse ELSE part
	if p.currentTokenIs(token.ELSE) {
		p.nextToken() // consume ELSE
		p.skipNewlinesAndComments()

		for !p.currentTokenIs(token.END) && !p.currentTokenIs(token.EOF) {
			if s := p.parseExecutableStatement(); s != nil {
				stmt.ElsePart = append(stmt.ElsePart, s)
			} else {
				p.skipToNextStatement()
			}
			p.skipNewlinesAndComments()
		}
	}

	// Expect END IF
	if p.currentTokenIs(token.END) {
		p.nextToken() // consume END
		if p.currentTokenIs(token.IF) {
			p.nextToken() // consume IF
		} else {
			p.addError("expected IF after END")
		}
	} else {
		p.addError("expected END IF")
	}
	stmt.Position = ast.Pos(start, p.current.start)
	return stmt
}

// parseDoLoop parses a DO loop
func (p *Parser90) parseDoLoop() ast.Statement {
	start := p.current.start
	stmt := &ast.DoLoop{}
	p.nextToken() // consume DO

	var doLabel string
	if p.currentTokenIs(token.IntLit) {
		doLabel = string(p.current.lit)
		p.nextToken()
	}

	// Check for DO WHILE
	if p.currentTokenIs(token.WHILE) {
		p.nextToken() // consume WHILE
		if !p.currentTokenIs(token.LParen) {
			p.addError("expected '(' after DO WHILE")
			return nil
		}
		p.nextToken() // consume (

		stmt.Start = p.parseExpression(0) // Condition stored in Start for DO WHILE
		if stmt.Start == nil {
			p.addError("expected condition in DO WHILE statement")
			return nil
		}

		if !p.currentTokenIs(token.RParen) {
			p.addError("expected ')' after DO WHILE condition")
			return nil
		}
		p.nextToken() // consume )
	} else {
		// Counter-controlled DO loop
		if p.currentTokenIs(token.Identifier) {
			stmt.Var = string(p.current.lit)
			p.nextToken()

			if !p.currentTokenIs(token.Equals) {
				p.addError("expected '=' after loop variable")
				return nil
			}
			p.nextToken() // consume =

			stmt.Start = p.parseExpression(0)
			if stmt.Start == nil {
				p.addError("expected start expression in DO loop")
				return nil
			}

			if !p.currentTokenIs(token.Comma) {
				p.addError("expected ',' after start expression")
				return nil
			}
			p.nextToken() // consume ,

			stmt.End = p.parseExpression(0)
			if stmt.End == nil {
				p.addError("expected end expression in DO loop")
				return nil
			}

			if p.currentTokenIs(token.Comma) {
				p.nextToken() // consume ,
				stmt.Step = p.parseExpression(0)
				if stmt.Step == nil {
					p.addError("expected step expression in DO loop")
					return nil
				}
			}
		}
	}

	p.skipNewlinesAndComments()

	// Parse loop body
	for {
		p.skipNewlinesAndComments()
		if p.currentTokenIs(token.END) && p.peekTokenIs(token.DO) {
			break
		}
		if p.currentTokenIs(token.Label) && string(p.current.lit) == doLabel {
			break
		}
		if p.currentTokenIs(token.EOF) {
			break
		}

		if s := p.parseExecutableStatement(); s != nil {
			stmt.Body = append(stmt.Body, s)
		} else {
			p.skipToNextStatement()
		}
	}

	// Expect END DO or a labeled statement
	if doLabel != "" {
		if p.currentTokenIs(token.Label) && string(p.current.lit) == doLabel {
			p.nextToken() // consume label
			if s := p.parseExecutableStatement(); s != nil {
				stmt.Body = append(stmt.Body, s)
			} else {
				p.addError("expected an executable statement after label " + doLabel)
			}
		} else {
			p.addError("expected statement with label " + doLabel)
		}
	} else {
		if p.currentTokenIs(token.END) {
			p.nextToken() // consume END
			if p.currentTokenIs(token.DO) {
				p.nextToken() // consume DO
			} else {
				p.addError("expected DO after END")
			}
		} else {
			p.addError("expected END DO")
		}
	}
	stmt.Position = ast.Pos(start, p.current.start)
	return stmt
}

// parseCallStmt parses a CALL statement
func (p *Parser90) parseCallStmt() ast.Statement {
	start := p.current.start
	stmt := &ast.CallStmt{}
	p.nextToken() // consume CALL

	if !p.canUseAsIdentifier() {
		p.addError("expected subroutine name after CALL")
		return nil
	}
	stmt.Name = string(p.current.lit)
	p.nextToken()

	if p.currentTokenIs(token.LParen) {
		p.nextToken() // consume (
		var args []ast.Expression
		for !p.currentTokenIs(token.RParen) && !p.currentTokenIs(token.EOF) {
			arg := p.parseExpression(0)
			if arg == nil {
				p.addError("expected expression in argument list")
				break
			}
			args = append(args, arg)

			if p.currentTokenIs(token.Comma) {
				p.nextToken()
			} else if !p.currentTokenIs(token.RParen) {
				p.addError("expected ',' or ')' in argument list")
				break
			}
		}
		stmt.Args = args
		if p.currentTokenIs(token.RParen) {
			p.nextToken() // consume )
		}
	}
	stmt.Position = ast.Pos(start, p.current.start)
	return stmt
}

// parseReturnStmt parses a RETURN statement
func (p *Parser90) parseReturnStmt() ast.Statement {
	start := p.current.start
	stmt := &ast.ReturnStmt{}
	p.nextToken() // consume RETURN
	stmt.Position = ast.Pos(start, p.current.start)
	return stmt
}

// parseCycleStmt parses a CYCLE statement
func (p *Parser90) parseCycleStmt() ast.Statement {
	start := p.current.start
	stmt := &ast.CycleStmt{}
	p.nextToken() // consume CYCLE
	stmt.Position = ast.Pos(start, p.current.start)
	return stmt
}

// parseExitStmt parses an EXIT statement
func (p *Parser90) parseExitStmt() ast.Statement {
	start := p.current.start
	stmt := &ast.ExitStmt{}
	p.nextToken() // consume EXIT
	stmt.Position = ast.Pos(start, p.current.start)
	return stmt
}

// parseAssignmentStmt parses an assignment statement
func (p *Parser90) parseAssignmentStmt() ast.Statement {
	startPos := p.current.start

	// The "target" of an assignment can be a complex expression itself (e.g., array slice, derived type component)
	// We parse it as a general expression.
	target := p.parseExpression(0)
	if target == nil {
		p.addError("invalid target for assignment")
		return nil
	}

	if p.currentTokenIs(token.Equals) {
		p.nextToken() // consume =
		value := p.parseExpression(0)
		if value == nil {
			p.addError("expected expression after '='")
			return nil
		}
		return &ast.AssignmentStmt{
			Target:   target,
			Value:    value,
			Position: ast.Pos(startPos, p.current.start),
		}
	} else if p.currentTokenIs(token.PointerAssign) {
		p.nextToken() // consume =>
		value := p.parseExpression(0)
		if value == nil {
			p.addError("expected expression after '=>'")
			return nil
		}
		return &ast.PointerAssignmentStmt{
			Target:   target,
			Value:    value,
			Position: ast.Pos(startPos, p.current.start),
		}
	}

	// This might be a subroutine call without the CALL keyword.
	// For now, we'll treat it as an error.
	p.addError("expected '=' or '=>' for assignment statement")
	return nil
}

// isExecutableStatement returns true if current token starts an executable statement
func (p *Parser90) isExecutableStatement() bool {
	switch p.current.tok {
	case token.IF, token.DO, token.CALL, token.RETURN, token.STOP, token.EXIT,
		token.ALLOCATE, token.DEALLOCATE, token.READ, token.WRITE, token.PRINT,
		token.GOTO, token.CONTINUE, token.CYCLE:
		return true
	case token.Identifier:
		// Could be assignment or procedure call. We need to lookahead to distinguish.
		// If we see `IDENTIFIER =`, it's an assignment.
		// If we see `IDENTIFIER(...)` it could be an assignment to an array element or a function call.
		// For now, we will treat all identifiers at the start of a statement in the execution part as the start of an executable statement.
		return true
	default:
		return false
	}
}

// skipToNextStatement skips tokens until the next newline
func (p *Parser90) skipToNextStatement() {
	for !p.currentTokenIs(token.NewLine) && !p.currentTokenIs(token.EOF) {
		p.nextToken()
	}
	if p.currentTokenIs(token.NewLine) {
		p.nextToken()
	}
}

// skipTypeDefinition skips from TYPE to END TYPE
func (p *Parser90) skipTypeDefinition() {
	p.nextToken() // Skip TYPE
	depth := 1
	for depth > 0 && !p.currentTokenIs(token.EOF) {
		if p.currentTokenIs(token.TYPE) {
			depth++
			p.nextToken()
		} else if p.currentTokenIs(token.END) {
			p.nextToken()
			if p.currentTokenIs(token.TYPE) {
				depth--
				if depth > 0 {
					p.nextToken()
				}
			}
		} else {
			p.nextToken()
		}
	}
}

// skipInterfaceBlock skips from INTERFACE to END INTERFACE
func (p *Parser90) skipInterfaceBlock() {
	p.nextToken() // Skip INTERFACE
	depth := 1
	for depth > 0 && !p.currentTokenIs(token.EOF) {
		if p.currentTokenIs(token.INTERFACE) {
			depth++
			p.nextToken()
		} else if p.currentTokenIs(token.END) {
			p.nextToken()
			if p.currentTokenIs(token.INTERFACE) {
				depth--
				if depth > 0 {
					p.nextToken()
				}
			}
		} else {
			p.nextToken()
		}
	}
}

// isEndOfProgramUnit checks if current END token ends a program unit
func (p *Parser90) isEndOfProgramUnit() bool {
	if !p.currentTokenIs(token.END) {
		return false
	}
	// Look ahead to see what follows END
	next := p.peek.tok
	switch next {
	case token.PROGRAM, token.SUBROUTINE, token.FUNCTION, token.MODULE:
		return true
	case token.NewLine, token.EOF:
		// Bare END (common in older Fortran)
		return true
	case token.Identifier:
		// Could be "END program_name" or "END BLOCK" (for BLOCK DATA)
		return true
	default:
		return false
	}
}

// skipIfConstruct skips from IF to matching END IF
func (p *Parser90) skipIfConstruct() {
	// For Phase 2, we simply skip to END IF without tracking nesting
	// This handles most common cases correctly
	p.nextToken() // Skip IF
	for !p.currentTokenIs(token.EOF) {
		if p.currentTokenIs(token.END) {
			p.nextToken()
			if p.currentTokenIs(token.IF) {
				p.nextToken() // Skip IF in "END IF"
				return
			}
			// Not END IF, continue (might be END DO inside the IF block, etc.)
		} else {
			p.nextToken()
		}
	}
}

// skipDoConstruct skips from DO to matching END DO
func (p *Parser90) skipDoConstruct() {
	p.nextToken() // Skip DO
	depth := 1
	for depth > 0 && !p.currentTokenIs(token.EOF) {
		if p.currentTokenIs(token.DO) {
			depth++
		} else if p.currentTokenIs(token.END) {
			p.nextToken()
			if p.currentTokenIs(token.DO) {
				depth--
			}
		}
		p.nextToken()
	}
}

// skipSelectConstruct skips from SELECT to matching END SELECT
func (p *Parser90) skipSelectConstruct() {
	p.nextToken() // Skip SELECT
	depth := 1
	for depth > 0 && !p.currentTokenIs(token.EOF) {
		if p.currentTokenIs(token.SELECT) {
			depth++
		} else if p.currentTokenIs(token.END) {
			p.nextToken()
			if p.currentTokenIs(token.SELECT) {
				depth--
			}
		}
		p.nextToken()
	}
}

// parseSpecStatement parses a specification statement
// paramMap is used to populate type information for parameters
func (p *Parser90) parseSpecStatement(sawImplicit, sawDecl *bool, paramMap map[string]*ast.Parameter) ast.Statement {
	switch p.current.tok {
	case token.IMPLICIT:
		return p.parseImplicit(sawImplicit, sawDecl)
	case token.USE:
		return p.parseUse()
	case token.INTEGER, token.REAL, token.DOUBLE, token.COMPLEX, token.LOGICAL, token.CHARACTER:
		*sawDecl = true
		return p.parseTypeDecl(paramMap)
	case token.TYPE:
		// Distinguish between TYPE definition and TYPE(typename) declaration
		if p.peekTokenIs(token.LParen) {
			// TYPE(typename) :: var - treat as type declaration
			*sawDecl = true
			return p.parseTypeDecl(paramMap)
		} else {
			// TYPE :: name ... END TYPE - skip entire block
			p.skipTypeDefinition()
			return &ast.ImplicitStatement{} // Return non-nil to indicate success
		}
	case token.INTERFACE:
		// INTERFACE block - skip entire block
		p.skipInterfaceBlock()
		return &ast.ImplicitStatement{} // Return non-nil to indicate success
	default:
		return nil // Unknown statement, caller will skip
	}
}

// parsePrivateStmt parses a PRIVATE statement
func (p *Parser90) parsePrivateStmt() ast.Statement {
	start := p.current.start
	stmt := &ast.PrivateStmt{}
	p.nextToken() // consume PRIVATE

	if p.currentTokenIs(token.DoubleColon) {
		p.nextToken() // consume ::
		for p.canUseAsIdentifier() {
			stmt.Entities = append(stmt.Entities, string(p.current.lit))
			p.nextToken()
			if !p.currentTokenIs(token.Comma) {
				break
			}
			p.nextToken()
		}
	}
	stmt.Position = ast.Pos(start, p.current.start)
	return stmt
}

// parsePublicStmt parses a PUBLIC statement
func (p *Parser90) parsePublicStmt() ast.Statement {
	start := p.current.start
	stmt := &ast.PublicStmt{}
	p.nextToken() // consume PUBLIC

	if p.currentTokenIs(token.DoubleColon) {
		p.nextToken() // consume ::
		for p.canUseAsIdentifier() {
			stmt.Entities = append(stmt.Entities, string(p.current.lit))
			p.nextToken()
			if !p.currentTokenIs(token.Comma) {
				break
			}
			p.nextToken()
		}
	}
	stmt.Position = ast.Pos(start, p.current.start)
	return stmt
}

// parseInterfaceStmt parses an INTERFACE...END INTERFACE block
func (p *Parser90) parseInterfaceStmt() ast.Statement {
	start := p.current.start
	stmt := &ast.InterfaceStmt{}
	p.nextToken() // consume INTERFACE

	if p.canUseAsIdentifier() {
		stmt.Name = string(p.current.lit)
		p.nextToken()
	}
	p.skipNewlinesAndComments()

	// Parse interface body
	for !p.currentTokenIs(token.END) && !p.currentTokenIs(token.EOF) {
		if p.peekTokenIs(token.INTERFACE) && p.currentTokenIs(token.END) {
			break
		}
		// The body of an interface block can contain procedure headings
		// (subroutines and functions). We can reuse the top-level parsers for this.
		if unit := p.parseTopLevelUnit(); unit != nil {
			stmt.Body = append(stmt.Body, unit)
		} else {
			p.skipToNextStatement()
		}
		p.skipNewlinesAndComments()
	}

	// Expect END INTERFACE
	if p.currentTokenIs(token.END) {
		p.nextToken() // consume END
		if p.currentTokenIs(token.INTERFACE) {
			p.nextToken() // consume INTERFACE
		} else {
			p.addError("expected INTERFACE after END")
		}
	} else {
		p.addError("expected END INTERFACE")
	}
	stmt.Position = ast.Pos(start, p.current.start)
	return stmt
}

// parseDerivedTypeStmt parses a TYPE...END TYPE block
func (p *Parser90) parseDerivedTypeStmt() ast.Statement {
	start := p.current.start
	stmt := &ast.DerivedTypeStmt{}
	p.nextToken() // consume TYPE

	if p.currentTokenIs(token.DoubleColon) {
		p.nextToken() // consume ::
	}

	if !p.canUseAsIdentifier() {
		p.addError("expected derived type name")
		return nil
	}
	stmt.Name = string(p.current.lit)
	p.nextToken()
	p.skipNewlinesAndComments()

	// Parse component declarations
	for !p.currentTokenIs(token.END) && !p.currentTokenIs(token.EOF) {
		if p.peekTokenIs(token.TYPE) && p.currentTokenIs(token.END) {
			break
		}
		if s := p.parseComponentDecl(); s != nil {
			stmt.Components = append(stmt.Components, *s)
		} else {
			p.skipToNextStatement()
		}
		p.skipNewlinesAndComments()
	}

	// Expect END TYPE
	if p.currentTokenIs(token.END) {
		p.nextToken() // consume END
		if p.currentTokenIs(token.TYPE) {
			p.nextToken() // consume TYPE
		} else {
			p.addError("expected TYPE after END")
		}
	} else {
		p.addError("expected END TYPE")
	}

	stmt.Position = ast.Pos(start, p.current.start)
	return stmt
}

// parseComponentDecl parses a component declaration within a derived type
func (p *Parser90) parseComponentDecl() *ast.ComponentDecl {
	start := p.current.start
	stmt := &ast.ComponentDecl{}
	if !p.isTypeKeyword(p.current.tok) && p.current.tok != token.TYPE {
		p.addError("expected type specifier for component declaration")
		return nil
	}
	stmt.Type = p.current.tok.String()
	p.nextToken()

	// Handle DOUBLE PRECISION
	if stmt.Type == "DOUBLE" && p.currentTokenIs(token.PRECISION) {
		stmt.Type = "DOUBLE PRECISION"
		p.nextToken()
	}

	// Handle CHARACTER length specification
	if stmt.Type == "CHARACTER" {
		if p.currentTokenIs(token.LParen) {
			stmt.Type += p.parseCharacterLength()
		} else if p.currentTokenIs(token.Asterisk) {
			p.nextToken() // consume *
			if p.currentTokenIs(token.IntLit) || p.canUseAsIdentifier() {
				stmt.Type += "*" + string(p.current.lit)
				p.nextToken()
			}
		}
	}

	// Handle TYPE(typename) for derived types
	if stmt.Type == "TYPE" && p.currentTokenIs(token.LParen) {
		// Skip (typename)
		depth := 1
		p.nextToken()
		for depth > 0 && !p.currentTokenIs(token.EOF) {
			if p.currentTokenIs(token.LParen) {
				depth++
			} else if p.currentTokenIs(token.RParen) {
				depth--
			}
			p.nextToken()
		}
	}

	// Parse attributes
	if p.currentTokenIs(token.Comma) {
		for p.currentTokenIs(token.Comma) {
			p.nextToken()
			if p.current.tok.IsAttribute() {
				stmt.Attributes = append(stmt.Attributes, p.current.tok)
				p.nextToken()
			}
		}
	}

	if p.currentTokenIs(token.DoubleColon) {
		p.nextToken()
	}

	// Parse component list
	for p.canUseAsIdentifier() {
		entityName := string(p.current.lit)
		entity := ast.DeclEntity{Name: entityName}
		p.nextToken()

		// Check for array declarator
		if p.currentTokenIs(token.LParen) {
			entity.ArraySpec = p.parseArraySpec()
		}

		// Parse initialization expression
		if p.currentTokenIs(token.Equals) || p.currentTokenIs(token.PointerAssign) {
			p.nextToken() // consume = or =>
			// For now, just consume the expression
			for !p.currentTokenIs(token.NewLine) && !p.currentTokenIs(token.EOF) && !p.currentTokenIs(token.Comma) {
				p.nextToken()
			}
		}

		stmt.Components = append(stmt.Components, entity)

		if !p.currentTokenIs(token.Comma) {
			break
		}
		p.nextToken()
	}
	stmt.Position = ast.Pos(start, p.current.start)
	return stmt
}

// parseImplicit parses IMPLICIT [NONE] with validation
func (p *Parser90) parseImplicit(sawImplicit, sawDecl *bool) ast.Statement {
	start := p.current.start
	stmt := &ast.ImplicitStatement{}
	p.nextToken()

	if p.currentTokenIs(token.Identifier) && string(p.current.lit) == "NONE" {
		if *sawImplicit {
			p.addError("duplicate IMPLICIT statement")
		} else if *sawDecl {
			p.addError("IMPLICIT NONE must appear before type declarations")
		}
		*sawImplicit = true
		stmt.IsNone = true
		p.nextToken()
	}
	stmt.Position = ast.Pos(start, p.current.start)
	return stmt
}

// parseUse parses USE module [, ONLY: list]
func (p *Parser90) parseUse() ast.Statement {
	start := p.current.start
	stmt := &ast.UseStatement{}
	p.nextToken()

	if !p.canUseAsIdentifier() {
		p.addError("expected module name after USE")
		return nil
	}
	stmt.ModuleName = string(p.current.lit)
	p.nextToken()

	if p.currentTokenIs(token.Comma) {
		p.nextToken()
		if p.currentTokenIs(token.ONLY) {
			p.nextToken()
			if p.expectCurrent(token.Colon) {
				p.nextToken()
				for p.canUseAsIdentifier() {
					stmt.Only = append(stmt.Only, string(p.current.lit))
					p.nextToken()
					if !p.currentTokenIs(token.Comma) {
						break
					}
					p.nextToken()
				}
			}
		}
	}
	stmt.Position = ast.Pos(start, p.current.start)
	return stmt
}

// parseTypeDecl parses INTEGER :: x, y or REAL, PARAMETER :: pi = 3.14 or TYPE(typename) :: var
// If paramMap is provided, it will populate type information for any parameters found
func (p *Parser90) parseTypeDecl(paramMap map[string]*ast.Parameter) ast.Statement {
	start := p.current.start
	stmt := &ast.TypeDeclaration{TypeSpec: p.current.tok.String()}
	p.nextToken()

	// Handle DOUBLE PRECISION
	if stmt.TypeSpec == "DOUBLE" && p.currentTokenIs(token.PRECISION) {
		stmt.TypeSpec = "DOUBLE PRECISION"
		p.nextToken()
	}

	// Handle CHARACTER length specification: CHARACTER(LEN=80) or CHARACTER*80 or CHARACTER(*)
	var charLen string
	if stmt.TypeSpec == "CHARACTER" {
		if p.currentTokenIs(token.LParen) {
			// CHARACTER(LEN=n) or CHARACTER(n) or CHARACTER(*)
			charLen = p.parseCharacterLength()
		} else if p.currentTokenIs(token.Asterisk) {
			// CHARACTER*n form
			p.nextToken() // consume *
			if p.currentTokenIs(token.IntLit) || p.canUseAsIdentifier() {
				charLen = string(p.current.lit)
				p.nextToken()
			}
		}
	}

	// Handle TYPE(typename) for derived types
	if stmt.TypeSpec == "TYPE" && p.currentTokenIs(token.LParen) {
		// Skip (typename)
		depth := 1
		p.nextToken()
		for depth > 0 && !p.currentTokenIs(token.EOF) {
			if p.currentTokenIs(token.LParen) {
				depth++
			} else if p.currentTokenIs(token.RParen) {
				depth--
			}
			p.nextToken()
		}
	}

	// Parse attributes (PARAMETER, INTENT, etc.)
	var intentType ast.IntentType
	var arraySpec *ast.ArraySpec
	if p.currentTokenIs(token.Comma) {
		for p.currentTokenIs(token.Comma) {
			p.nextToken()
			if p.current.tok.IsAttribute() {
				stmt.Attributes = append(stmt.Attributes, p.current.tok)

				// Special handling for INTENT to extract the direction
				if p.current.tok == token.INTENT {
					p.nextToken()
					if p.currentTokenIs(token.LParen) {
						p.nextToken()
						if p.currentTokenIs(token.Identifier) {
							intentStr := string(p.current.lit)
							switch intentStr {
							case "IN":
								intentType = ast.IntentIn
							case "OUT":
								intentType = ast.IntentOut
							case "INOUT":
								intentType = ast.IntentInOut
							}
						} else if p.currentTokenIs(token.IN) {
							intentType = ast.IntentIn
						} else if p.currentTokenIs(token.OUT) {
							intentType = ast.IntentOut
						} else if p.currentTokenIs(token.INOUT) {
							intentType = ast.IntentInOut
						}
						// Skip to closing paren
						depth := 1
						p.nextToken()
						for depth > 0 && !p.currentTokenIs(token.EOF) {
							if p.currentTokenIs(token.LParen) {
								depth++
							} else if p.currentTokenIs(token.RParen) {
								depth--
							}
							p.nextToken()
						}
						continue
					}
				}

				// Special handling for DIMENSION to extract array bounds
				if p.current.tok == token.DIMENSION {
					p.nextToken()
					if p.currentTokenIs(token.LParen) {
						arraySpec = p.parseArraySpec()
						continue
					}
				}

				p.nextToken()
				// Skip attribute arguments like DIMENSION(...), etc.
				if p.currentTokenIs(token.LParen) {
					depth := 1
					p.nextToken()
					for depth > 0 && !p.currentTokenIs(token.EOF) {
						if p.currentTokenIs(token.LParen) {
							depth++
						} else if p.currentTokenIs(token.RParen) {
							depth--
						}
						p.nextToken()
					}
				}
			}
		}
	}

	// Expect ::
	if p.currentTokenIs(token.DoubleColon) {
		p.nextToken()
	} else if p.currentTokenIs(token.Colon) {
		p.nextToken()
		if p.currentTokenIs(token.Colon) {
			p.nextToken()
		}
	}

	// Parse entity list
	for p.canUseAsIdentifier() {
		entityName := string(p.current.lit)
		entity := ast.DeclEntity{Name: entityName}

		// Set CHARACTER length if this is a CHARACTER type
		if charLen != "" {
			entity.CharLen = charLen
		}

		// Start with arraySpec from DIMENSION attribute if present
		if arraySpec != nil {
			entity.ArraySpec = arraySpec
		}

		p.nextToken()

		// Check for array declarator: arr(10,20)
		var entityArraySpec *ast.ArraySpec
		if p.currentTokenIs(token.LParen) {
			// This could be an array declarator - parse it
			entityArraySpec = p.parseArraySpec()
			// Entity-level array spec takes precedence over DIMENSION attribute
			if entityArraySpec != nil {
				entity.ArraySpec = entityArraySpec
			}
		}

		// Parse initialization expression: = value or => null()
		if p.currentTokenIs(token.Equals) || p.currentTokenIs(token.PointerAssign) {
			isPointerAssign := p.currentTokenIs(token.PointerAssign)
			p.nextToken() // consume = or =>

			var initTokens []byte
			if isPointerAssign {
				initTokens = append(initTokens, []byte("=> ")...)
			}

			depth := 0
			for !p.currentTokenIs(token.NewLine) && !p.currentTokenIs(token.EOF) {
				if p.currentTokenIs(token.LParen) {
					depth++
				} else if p.currentTokenIs(token.RParen) {
					depth--
				} else if p.currentTokenIs(token.Comma) && depth == 0 {
					break
				}
				// Add space before token if needed (but not before first token, and not around parens/slashes)
				if len(initTokens) > 0 {
					lastChar := initTokens[len(initTokens)-1]
					currIsSpecial := p.currentTokenIs(token.LParen) || p.currentTokenIs(token.RParen) || p.currentTokenIs(token.Slash)
					lastIsSpecial := lastChar == '(' || lastChar == ')' || lastChar == '/'
					if !currIsSpecial && !lastIsSpecial {
						initTokens = append(initTokens, ' ')
					}
				}
				initTokens = append(initTokens, p.current.lit...)
				p.nextToken()
			}
			entity.Initializer = string(initTokens)
		}

		// Add entity to statement (now that all fields are populated)
		stmt.Entities = append(stmt.Entities, entity)

		// If this entity is a parameter, populate its type information
		if paramMap != nil {
			if param, exists := paramMap[entityName]; exists {
				param.Type = stmt.TypeSpec
				param.Intent = intentType
				param.Attributes = append(param.Attributes, stmt.Attributes...)
				param.CharLen = entity.CharLen
				// Use entity array spec if present, otherwise DIMENSION attribute spec
				if entityArraySpec != nil {
					param.ArraySpec = entityArraySpec
				} else if arraySpec != nil {
					param.ArraySpec = arraySpec
				}
			}
		}

		if !p.currentTokenIs(token.Comma) {
			break
		}
		p.nextToken()
	}
	stmt.Position = ast.Pos(start, p.current.start)
	return stmt
}

// collectBodyUntilEnd - DEPRECATED, kept for reference
func (p *Parser90) collectBodyUntilEndOLD(endKeyword token.Token) []ast.TokenTuple {
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
				if nextTok == token.NewLine || nextTok == token.EOF || nextTok == token.Identifier {
					return tokens // Bare END or END <name>
				}
				// Note: We do NOT return for END SUBROUTINE/FUNCTION/etc. found while collecting
				// a module body, as these might be nested constructs (e.g., inside INTERFACE blocks)
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
	start := p.current.start
	block := &ast.ProgramBlock{}

	// Consume PROGRAM keyword
	p.nextToken()

	// Parse program name (keywords can be used as program names)
	if !p.canUseAsIdentifier() {
		p.addError("expected program name, got " + p.current.tok.String())
		return nil
	}
	block.Name = string(p.current.lit)
	p.nextToken()

	p.skipNewlinesAndComments()

	// Parse body statements
	block.Body = p.parseBody(nil)

	block.Position = ast.Pos(start, p.current.start)
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
	start := p.current.start
	sub := &ast.Subroutine{}

	// Consume SUBROUTINE keyword
	p.nextToken()

	// Parse subroutine name (keywords can be used as subroutine names)
	if !p.canUseAsIdentifier() {
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

	// Parse body statements
	sub.Body = p.parseBody(sub.Parameters)

	sub.Position = ast.Pos(start, p.current.start)
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
	start := p.current.start
	fn := &ast.Function{}

	// Consume FUNCTION keyword
	p.nextToken()

	// Parse function name (can be Identifier, FormatSpec, or keyword used as identifier)
	if !p.canUseAsIdentifier() {
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

	// Parse body statements
	fn.Body = p.parseBody(fn.Parameters)

	fn.Position = ast.Pos(start, p.current.start)
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
	start := p.current.start
	mod := &ast.Module{}

	// Consume MODULE keyword
	p.nextToken()

	// Parse module name (keywords can be used as module names)
	if !p.canUseAsIdentifier() {
		p.addError("expected module name, got " + p.current.tok.String())
		return nil
	}
	mod.Name = string(p.current.lit)
	p.nextToken()

	p.skipNewlinesAndComments()

	// Parse body statements
	mod.Body = p.parseBody(nil)

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

	mod.Position = ast.Pos(start, p.current.start)
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
	start := p.current.start
	bd := &ast.BlockData{}

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

	// Parse body statements
	bd.Body = p.parseBody(nil)

	bd.Position = ast.Pos(start, p.current.start)
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

// parseArraySpec parses array dimension specification from DIMENSION(...) or entity declarator
// Expects current token to be '(' and consumes up to and including ')'
// Supports: (10), (1:10), (:), (*), (10,20), (1:10,1:20), (:,:), etc.
func (p *Parser90) parseArraySpec() *ast.ArraySpec {
	if !p.currentTokenIs(token.LParen) {
		return nil
	}
	p.nextToken() // consume '('

	spec := &ast.ArraySpec{
		Kind:   ast.ArraySpecExplicit, // Default, may be changed
		Bounds: []ast.ArrayBound{},
	}

	// Track if we see any assumed-shape (:) or assumed-size (*) dimensions
	hasAssumedShape := false
	hasAssumedSize := false
	hasExplicit := false

	for !p.currentTokenIs(token.RParen) && !p.currentTokenIs(token.EOF) {
		var bound ast.ArrayBound

		// Check for assumed-size: *
		if p.currentTokenIs(token.Asterisk) {
			hasAssumedSize = true
			bound.Lower = nil
			// Represent * as an Identifier for consistency
			bound.Upper = &ast.Identifier{
				Value:    "*",
				Position: ast.Pos(p.current.start, p.current.start+1),
			}
			p.nextToken()
		} else if p.currentTokenIs(token.Colon) {
			// Assumed-shape: :
			hasAssumedShape = true
			bound.Lower = nil
			bound.Upper = nil
			p.nextToken()
		} else {
			// Parse explicit bound expression
			// parseExpression will stop at :, ,, or ) since they have precedence 0
			lowerExpr := p.parseExpression(0)
			if lowerExpr == nil {
				// Invalid expression
				break
			}

			if p.currentTokenIs(token.Colon) {
				// This was the lower bound, now get upper bound
				bound.Lower = lowerExpr
				p.nextToken() // consume ':'

				// Parse upper bound expression
				upperExpr := p.parseExpression(0)
				if upperExpr == nil {
					// Invalid expression
					break
				}
				bound.Upper = upperExpr
				hasExplicit = true
			} else {
				// No colon, so this is just upper bound (lower defaults to 1)
				bound.Lower = nil
				bound.Upper = lowerExpr
				hasExplicit = true
			}
		}

		spec.Bounds = append(spec.Bounds, bound)

		// Move past comma if present
		if p.currentTokenIs(token.Comma) {
			p.nextToken()
		}
	}

	// Consume closing paren
	if p.currentTokenIs(token.RParen) {
		p.nextToken()
	}

	// Determine the kind based on what we saw
	if hasAssumedSize {
		spec.Kind = ast.ArraySpecAssumedSize
	} else if hasAssumedShape {
		spec.Kind = ast.ArraySpecAssumed
	} else if hasExplicit {
		spec.Kind = ast.ArraySpecExplicit
	}

	return spec
}

// parseCharacterLength parses CHARACTER length specification from (LEN=n), (n), (*), or (LEN=:)
// Expects current token to be '(' and consumes up to and including ')'
// Returns the length specification as a string
func (p *Parser90) parseCharacterLength() string {
	if !p.currentTokenIs(token.LParen) {
		return ""
	}
	p.nextToken() // consume '('

	var lengthTokens []byte

	// Check for LEN= prefix
	if p.currentTokenIs(token.LEN) {
		p.nextToken() // consume LEN
		if p.currentTokenIs(token.Equals) {
			p.nextToken() // consume =
		}
	}

	// Collect tokens until closing paren
	for !p.currentTokenIs(token.RParen) && !p.currentTokenIs(token.EOF) {
		if len(lengthTokens) > 0 {
			lengthTokens = append(lengthTokens, ' ')
		}
		lengthTokens = append(lengthTokens, p.current.lit...)
		p.nextToken()
	}

	// Consume closing paren
	if p.currentTokenIs(token.RParen) {
		p.nextToken()
	}

	return string(lengthTokens)
}

// getOperatorPrecedence returns the precedence level for operators
// Higher number = higher precedence
// Fortran precedence (highest to lowest):
// 10: % (component access)
// 9: ** (exponentiation, right associative)
// 8: unary +, -, .NOT.
// 7: *, /
// 6: binary +, -
// 5: // (string concatenation)
// 4: relational (.EQ., .NE., .LT., .LE., .GT., .GE., ==, /=, <, <=, >, >=)
// 3: .AND.
// 2: .OR.
// 1: .EQV., .NEQV.
func (p *Parser90) getOperatorPrecedence(op token.Token) int {
	switch op {
	case token.Percent:
		return 10
	case token.DoubleStar: // **
		return 9
	case token.Asterisk, token.Slash: // *, /
		return 7
	case token.Plus, token.Minus: // +, -
		return 6
	case token.StringConcat: // //
		return 5
	case token.EQ, token.NE, token.LT, token.LE, token.GT, token.GE: // .EQ., .NE., etc.
		return 4
	case token.EqEq, token.NotEquals, token.Less, token.LessEq, token.Greater, token.GreaterEq: // ==, /=, <, <=, >, >=
		return 4
	case token.AND: // .AND.
		return 3
	case token.OR: // .OR.
		return 2
	case token.EQV, token.NEQV: // .EQV., .NEQV.
		return 1
	default:
		return 0
	}
}

// isRightAssociative returns true for right-associative operators
func (p *Parser90) isRightAssociative(op token.Token) bool {
	return op == token.DoubleStar // ** is right associative
}

// parseExpression parses a Fortran expression using precedence climbing
// minPrec is the minimum precedence level to parse
func (p *Parser90) parseExpression(minPrec int) ast.Expression {
	// Parse primary expression (literal, identifier, function call, etc.)
	left := p.parsePrimaryExpr()
	if left == nil {
		return nil
	}
	start := left.SourcePos().Start()
	// Precedence climbing
	for {
		// Check if current token is a binary operator
		prec := p.getOperatorPrecedence(p.current.tok)
		if prec == 0 || prec < minPrec {
			break
		}

		op := p.current.tok
		p.nextToken() // consume operator

		if op == token.Percent {
			if !p.canUseAsIdentifier() {
				p.addError("expected component name after '%'")
				return left
			}
			componentName := string(p.current.lit)
			p.nextToken()
			left = &ast.ComponentAccess{
				Base:      left,
				Component: componentName,
				Position:  ast.Pos(start, p.current.start),
			}
			continue
		}

		// Determine the minimum precedence for the right side
		nextMinPrec := prec
		if !p.isRightAssociative(op) {
			nextMinPrec = prec + 1
		}

		// Parse right side
		right := p.parseExpression(nextMinPrec)
		if right == nil {
			p.addError("expected expression after operator")
			return left
		}

		// Create binary expression node
		left = &ast.BinaryExpr{
			Op:       op,
			Left:     left,
			Right:    right,
			Position: ast.Pos(start, right.SourcePos().End()),
		}
	}

	return left
}

// parsePrimaryExpr parses primary expressions: literals, identifiers, function calls,
// array references, and parenthesized expressions
func (p *Parser90) parsePrimaryExpr() ast.Expression {
	startPos := p.current.start

	// Handle unary operators: +, -, .NOT.
	if p.current.tok == token.Plus || p.current.tok == token.Minus || p.current.tok == token.NOT {
		op := p.current.tok
		p.nextToken()
		operand := p.parseExpression(8) // Unary operators have precedence 8
		if operand == nil {
			p.addError("expected expression after unary operator")
			return nil
		}
		return &ast.UnaryExpr{
			Op:       op,
			Operand:  operand,
			Position: ast.Pos(startPos, operand.SourcePos().End()),
		}
	}

	// Handle parenthesized expressions and array constructors
	if p.currentTokenIs(token.LParen) {
		if p.peekTokenIs(token.Slash) {
			return p.parseArrayConstructor()
		}
		p.nextToken() // consume (
		expr := p.parseExpression(0)
		if expr == nil {
			p.addError("expected expression after '('")
			return nil
		}
		if !p.currentTokenIs(token.RParen) {
			p.addError("expected ')' after expression")
			return nil
		}
		endPos := p.current.start
		p.nextToken() // consume )
		return &ast.ParenExpr{
			Expr:     expr,
			Position: ast.Pos(startPos, endPos),
		}
	}
	pos := ast.Pos(startPos, p.current.start+len(p.current.lit))
	// Handle integer literals
	if p.currentTokenIs(token.IntLit) {
		// TODO: Parse actual integer value
		lit := &ast.IntegerLiteral{
			Value:    0,                     // Will need proper parsing
			Raw:      string(p.current.lit), // Store original text
			Position: pos,
		}
		p.nextToken()
		return lit
	}

	// Handle real literals
	if p.currentTokenIs(token.FloatLit) {
		lit := &ast.RealLiteral{
			Value:    0.0, // Will need proper parsing
			Raw:      string(p.current.lit),
			Position: pos,
		}
		p.nextToken()
		return lit
	}

	// Handle string literals
	if p.currentTokenIs(token.StringLit) {
		lit := &ast.StringLiteral{
			Value:    string(p.current.lit),
			Position: pos,
		}
		p.nextToken()
		return lit
	}

	// Handle logical literals
	if p.currentTokenIs(token.TRUE) {
		lit := &ast.LogicalLiteral{
			Value:    true,
			Position: pos,
		}
		p.nextToken()
		return lit
	}
	if p.currentTokenIs(token.FALSE) {
		lit := &ast.LogicalLiteral{
			Value:    false,
			Position: pos,
		}
		p.nextToken()
		return lit
	}

	// Handle identifiers, function calls, and array references
	if p.canUseAsIdentifier() {
		name := string(p.current.lit)
		endPos := p.current.start + len(p.current.lit)
		p.nextToken()

		// Check for function call or array reference/section
		if p.currentTokenIs(token.LParen) {
			p.nextToken() // consume (

			// Parse argument/subscript list
			var args []ast.Expression
			for !p.currentTokenIs(token.RParen) && !p.currentTokenIs(token.EOF) {
				arg := p.parseExpression(0)
				if arg == nil {
					p.addError("expected expression in argument list")
					break
				}
				args = append(args, arg)

				if p.currentTokenIs(token.Comma) {
					p.nextToken()
				} else if !p.currentTokenIs(token.RParen) {
					p.addError("expected ',' or ')' in argument list")
					break
				}
			}

			if p.currentTokenIs(token.RParen) {
				endPos = p.current.start
				p.nextToken() // consume )
			}

			// For now, treat all identifier(...) as function calls
			// In a full implementation, we'd need symbol table to distinguish
			// between function calls and array references
			return &ast.FunctionCall{
				Name:     name,
				Args:     args,
				Position: ast.Pos(startPos, endPos),
			}
		}

		// Just an identifier
		return &ast.Identifier{
			Value:    name,
			Position: ast.Pos(startPos, endPos),
		}
	}

	return nil
}

// parseArrayConstructor parses an array constructor
func (p *Parser90) parseArrayConstructor() ast.Expression {
	start := p.current.start
	stmt := &ast.ArrayConstructor{}
	p.nextToken() // consume (
	p.nextToken() // consume /

	for !p.currentTokenIs(token.Slash) && !p.currentTokenIs(token.EOF) {
		val := p.parseExpression(0)
		if val == nil {
			p.addError("expected expression in array constructor")
			break
		}
		stmt.Values = append(stmt.Values, val)

		if !p.currentTokenIs(token.Comma) {
			break
		}
		p.nextToken()
	}

	if p.currentTokenIs(token.Slash) {
		p.nextToken() // consume /
		if p.currentTokenIs(token.RParen) {
			stmt.Position = ast.Pos(start, p.current.start)
			p.nextToken() // consume )
		} else {
			p.addError("expected ')' after array constructor")
		}
	} else {
		p.addError("expected '/)' at end of array constructor")
	}

	return stmt
}

// parseArraySection parses an array section
func (p *Parser90) parseArraySection(name string, startPos int) ast.Expression {
	start := p.current.start
	stmt := &ast.ArraySection{
		Name: name,
	}
	p.nextToken() // consume (

	for !p.currentTokenIs(token.RParen) && !p.currentTokenIs(token.EOF) {
		sub := ast.Subscript{}
		if !p.currentTokenIs(token.Colon) {
			sub.Lower = p.parseExpression(0)
		}
		if p.currentTokenIs(token.Colon) {
			p.nextToken() // consume :
			if !p.currentTokenIs(token.Comma) && !p.currentTokenIs(token.RParen) {
				sub.Upper = p.parseExpression(0)
			}
		}
		if p.currentTokenIs(token.Colon) {
			p.nextToken() // consume :
			sub.Stride = p.parseExpression(0)
		}
		stmt.Subscripts = append(stmt.Subscripts, sub)

		if !p.currentTokenIs(token.Comma) {
			break
		}
		p.nextToken()
	}

	if p.currentTokenIs(token.RParen) {
		stmt.Position = ast.Pos(start, p.current.start)
		p.nextToken() // consume )
	} else {
		p.addError("expected ')' after array section")
	}

	return stmt
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

	// Handle DOUBLE PRECISION (two-token type specifier)
	if typeToken == token.DOUBLE && p.currentTokenIs(token.PRECISION) {
		typeSpec = "DOUBLE PRECISION"
		p.nextToken()
	}

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
