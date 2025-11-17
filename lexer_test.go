package fortran

import (
	"strconv"
	"strings"
	"testing"

	"github.com/soypat/go-fortran/token"
)

type toktuple struct {
	tok     token.Token
	literal string
}

func TestLexer90_tokens(t *testing.T) {
	cases := []struct {
		src    string
		expect []toktuple
	}{
		0: {
			src: "IF(ILY.EQ.1.OR.ID.LT.366) GO TO 5",
			expect: []toktuple{
				{tok: token.IF, literal: "IF"},
				{tok: token.LParen, literal: ""},
				{tok: token.Identifier, literal: "ILY"},
				{tok: token.EQ, literal: "EQ"},
				{tok: token.IntLit, literal: "1"},
				{tok: token.OR, literal: "OR"},
				{tok: token.Identifier, literal: "ID"},
				{tok: token.LT, literal: "LT"},
				{tok: token.IntLit, literal: "366"},
				{tok: token.RParen, literal: ""},
				{tok: token.Identifier, literal: "GO"},
				{tok: token.Identifier, literal: "TO"},
				{tok: token.IntLit, literal: "5"},
			},
		},
		1: {
			src: "BLKIN(1,5,JBLK)= BLKIN(1,5,JBLK)+DBLE(IMTIME)/100.D0",
			expect: []toktuple{
				{tok: token.Identifier, literal: "BLKIN"},
				{tok: token.LParen, literal: ""},
				{tok: token.IntLit, literal: "1"},
				{tok: token.Comma, literal: ""},
				{tok: token.IntLit, literal: "5"},
				{tok: token.Comma, literal: ""},
				{tok: token.Identifier, literal: "JBLK"},
				{tok: token.RParen, literal: ""},
				{tok: token.Equals, literal: ""},
				{tok: token.Identifier, literal: "BLKIN"},
				{tok: token.LParen, literal: ""},
				{tok: token.IntLit, literal: "1"},
				{tok: token.Comma, literal: ""},
				{tok: token.IntLit, literal: "5"},
				{tok: token.Comma, literal: ""},
				{tok: token.Identifier, literal: "JBLK"},
				{tok: token.RParen, literal: ""},
				{tok: token.Plus, literal: ""},
				{tok: token.Identifier, literal: "DBLE"},
				{tok: token.LParen, literal: ""},
				{tok: token.Identifier, literal: "IMTIME"},
				{tok: token.RParen, literal: ""},
				{tok: token.Slash, literal: ""},
				{tok: token.FloatLit, literal: "100.D0"},
			},
		},
		2: {
			src: "      !CHARACTER*1 LINTWD,LTIME",
			expect: []toktuple{
				{tok: token.LineComment, literal: "CHARACTER*1 LINTWD,LTIME"},
			},
		},
		3: {
			src: `80100 FORMAT(1X,'EXECUTION TERMINATING IN TDIF DUE TO INCORRECT TIME SYS&
     &TEM REQUEST.'/1X,'INPUT SYSTEM NUMBER',I3,'OUTPUT SYSTEM NUMBER', &
     & I3)`,
			expect: []toktuple{
				{tok: token.IntLit, literal: "80100"},
				{tok: token.FORMAT, literal: "FORMAT"},
				{tok: token.LParen, literal: ""},
				{tok: token.FormatSpec, literal: "1X"},
				{tok: token.Comma, literal: ""},
				{tok: token.StringLit, literal: "EXECUTION TERMINATING IN TDIF DUE TO INCORRECT TIME SYSTEM REQUEST."},
				{tok: token.Slash, literal: ""},
				{tok: token.FormatSpec, literal: "1X"},
				{tok: token.Comma, literal: ""},
				{tok: token.StringLit, literal: "INPUT SYSTEM NUMBER"},
				{tok: token.Comma, literal: ""},
				{tok: token.FormatSpec, literal: "I3"},
				{tok: token.Comma, literal: ""},
				{tok: token.StringLit, literal: "OUTPUT SYSTEM NUMBER"},
				{tok: token.Comma, literal: ""},
				{tok: token.FormatSpec, literal: "I3"},
				{tok: token.RParen, literal: ""},
			},
		},
		4: {
			src: "      SEC= SEC +IH*3600.D0+IM*60.D0+ DBLE(IS)",
			expect: []toktuple{
				{tok: token.Identifier, literal: "SEC"},
				{tok: token.Equals, literal: ""},
				{tok: token.Identifier, literal: "SEC"},
				{tok: token.Plus, literal: ""},
				{tok: token.Identifier, literal: "IH"},
				{tok: token.Asterisk, literal: ""},
				{tok: token.FloatLit, literal: "3600.D0"},
				{tok: token.Plus, literal: ""},
				{tok: token.Identifier, literal: "IM"},
				{tok: token.Asterisk, literal: ""},
				{tok: token.FloatLit, literal: "60.D0"},
				{tok: token.Plus, literal: ""},
				{tok: token.Identifier, literal: "DBLE"},
				{tok: token.LParen, literal: ""},
				{tok: token.Identifier, literal: "IS"},
				{tok: token.RParen, literal: ""},
			},
		},
		5: {
			src: "INQUIRE(FILE=FILE05,EXIST=LEXIST)",
			expect: []toktuple{
				{tok: token.INQUIRE, literal: "INQUIRE"},
				{tok: token.LParen, literal: ""},
				{tok: token.Identifier, literal: "FILE"},
				{tok: token.Equals, literal: ""},
				{tok: token.Identifier, literal: "FILE05"},
				{tok: token.Comma, literal: ""},
				{tok: token.Identifier, literal: "EXIST"},
				{tok: token.Equals, literal: ""},
				{tok: token.Identifier, literal: "LEXIST"},
				{tok: token.RParen, literal: ""},
			},
		},
		6: {
			src: " 1000 FORMAT(A8,I6,I6,D20.7,I2,I2,I2,I2,I2)",
			expect: []toktuple{
				{tok: token.IntLit, literal: "1000"},
				{tok: token.FORMAT, literal: "FORMAT"},
				{tok: token.LParen, literal: ""},
				{tok: token.FormatSpec, literal: "A8"},
				{tok: token.Comma, literal: ""},
				{tok: token.FormatSpec, literal: "I6"},
				{tok: token.Comma, literal: ""},
				{tok: token.FormatSpec, literal: "I6"},
				{tok: token.Comma, literal: ""},
				{tok: token.FormatSpec, literal: "D20.7"},
				{tok: token.Comma, literal: ""},
				{tok: token.FormatSpec, literal: "I2"},
				{tok: token.Comma, literal: ""},
				{tok: token.FormatSpec, literal: "I2"},
				{tok: token.Comma, literal: ""},
				{tok: token.FormatSpec, literal: "I2"},
				{tok: token.Comma, literal: ""},
				{tok: token.FormatSpec, literal: "I2"},
				{tok: token.Comma, literal: ""},
				{tok: token.FormatSpec, literal: "I2"},
				{tok: token.RParen, literal: ""},
			},
		},
	}
	var l Lexer90
	for i, test := range cases {
		err := l.Reset("TestLexer"+strconv.Itoa(i), strings.NewReader(test.src))
		if err != nil {
			t.Error(err)
			continue
		}
		for i, expect := range test.expect {
			tok, _, literal := l.NextToken()
			if tok == token.EOF {
				t.Errorf("%s tok %d early EOF", l.Source(), i)
				break
			}
			if tok != expect.tok {
				t.Errorf("%s tok %d TokenMismatch %s!=%s", l.Source(), i, tok.String(), expect.tok.String())
			}
			if string(literal) != expect.literal {
				t.Errorf("%s tok %d LiteralMismatch %q!=%q", l.Source(), i, literal, expect.literal)
			}
		}
		if !l.Done() {
			tok, _, lit := l.NextToken()
			t.Errorf("%s expected lexer to be done, got %s (%s)", l.Source(), lit, tok.String())
		}
	}
}
