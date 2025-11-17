package fortran

import (
	"io"

	"github.com/soypat/go-fortran/ast"
	"github.com/soypat/go-fortran/token"
)

// Pratt parsing functions. a.k.a: Semantic Code.
type (
	prefixParseFn func() ast.Expression
	infixParseFn  func(ast.Expression) ast.Expression
)

type Parser90 struct {
	l       Lexer90
	current toktuple
	peek    toktuple
	pfxFns  map[token.Token]prefixParseFn
	infxFns map[token.Token]infixParseFn
}

func (p *Parser90) Reset(source string, r io.Reader) error {
	err := p.l.Reset(source, r)
	if err != nil {
		return err
	}
	if p.pfxFns == nil {
		p.pfxFns = make(map[token.Token]prefixParseFn)
		p.infxFns = make(map[token.Token]infixParseFn)
	}
	*p = Parser90{
		l: p.l,
		// Reuse memory but clear later.
		pfxFns:  p.pfxFns,
		infxFns: p.infxFns,
	}
	clear(p.pfxFns)
	clear(p.infxFns)

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
}

func (p *Parser90) registerPrefix(tokenType token.Token, fn prefixParseFn) {
	p.pfxFns[tokenType] = fn
}

func (p *Parser90) registerInfix(tokenType token.Token, fn infixParseFn) {
	p.infxFns[tokenType] = fn
}

type toktuple struct {
	tok   token.Token
	start int
	lit   []byte
}
