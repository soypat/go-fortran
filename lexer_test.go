package fortran

import (
	"strconv"
	"strings"
	"testing"

	"github.com/soypat/go-fortran/ast"
	"github.com/soypat/go-fortran/token"
)

type testtoktuple struct {
	tok     token.Token
	literal string
}

func TestLexer90_tokens(t *testing.T) {
	cases := []struct {
		src    string
		expect []testtoktuple
	}{
		0: {
			src: "IF(ILY.EQ.1.OR.ID.LT.366) GO TO 5",
			expect: []testtoktuple{
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
			expect: []testtoktuple{
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
			expect: []testtoktuple{
				{tok: token.LineComment, literal: "CHARACTER*1 LINTWD,LTIME"},
			},
		},
		3: {
			src: `80100 FORMAT(1X,'EXECUTION TERMINATING IN TDIF DUE TO INCORRECT TIME SYS&
     &TEM REQUEST.'/1X,'INPUT SYSTEM NUMBER',I3,'OUTPUT SYSTEM NUMBER', &
     & I3)`,
			expect: []testtoktuple{
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
			expect: []testtoktuple{
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
			expect: []testtoktuple{
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
			expect: []testtoktuple{
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
		7: {
			src: `      IF(EXACOB(K).EQ.-9999999.D0.OR.EXACOB(K+1).EQ.-9999999.D0.OR&
     &.EXACOB(K+2).EQ.-9999999.D0)  THEN`,
			expect: []testtoktuple{
				{tok: token.IF, literal: "IF"},
				{tok: token.LParen, literal: ""},
				{tok: token.Identifier, literal: "EXACOB"},
				{tok: token.LParen, literal: ""},
				{tok: token.Identifier, literal: "K"},
				{tok: token.RParen, literal: ""},
				{tok: token.EQ, literal: "EQ"},
				{tok: token.Minus, literal: ""},
				{tok: token.FloatLit, literal: "9999999.D0"},
				{tok: token.OR, literal: "OR"},
				{tok: token.Identifier, literal: "EXACOB"},
				{tok: token.LParen, literal: ""},
				{tok: token.Identifier, literal: "K"},
				{tok: token.Plus, literal: ""},
				{tok: token.IntLit, literal: "1"},
				{tok: token.RParen, literal: ""},
				{tok: token.EQ, literal: "EQ"},
				{tok: token.Minus, literal: ""},
				{tok: token.FloatLit, literal: "9999999.D0"},
				{tok: token.OR, literal: "OR"},
				{tok: token.Identifier, literal: "EXACOB"},
				{tok: token.LParen, literal: ""},
				{tok: token.Identifier, literal: "K"},
				{tok: token.Plus, literal: ""},
				{tok: token.IntLit, literal: "2"},
				{tok: token.RParen, literal: ""},
				{tok: token.EQ, literal: "EQ"},
				{tok: token.Minus, literal: ""},
				{tok: token.FloatLit, literal: "9999999.D0"},
				{tok: token.RParen, literal: ""},
				{tok: token.THEN, literal: "THEN"},
			},
		},
		8: {
			src: "QFI0MR=QFI0MR*ACOS(-1.Q0)/180.Q0",
			expect: []testtoktuple{
				{tok: token.Identifier, literal: "QFI0MR"},
				{tok: token.Equals, literal: ""},
				{tok: token.Identifier, literal: "QFI0MR"},
				{tok: token.Asterisk, literal: ""},
				{tok: token.Identifier, literal: "ACOS"},
				{tok: token.LParen, literal: ""},
				{tok: token.Minus, literal: ""},
				{tok: token.FloatLit, literal: "1.Q0"},
				{tok: token.RParen, literal: ""},
				{tok: token.Slash, literal: ""},
				{tok: token.FloatLit, literal: "180.Q0"},
			},
		},
		// Test comparison operators without spaces (issue: lexer was treating 1.EQ.1 as float 1.E)
		9: {
			src: "a=1.EQ.1",
			expect: []testtoktuple{
				{tok: token.Identifier, literal: "a"},
				{tok: token.Equals, literal: ""},
				{tok: token.IntLit, literal: "1"},
				{tok: token.EQ, literal: "EQ"},
				{tok: token.IntLit, literal: "1"},
			},
		},
		10: {
			src: "x=2.LT.3.AND.4.GT.1",
			expect: []testtoktuple{
				{tok: token.Identifier, literal: "x"},
				{tok: token.Equals, literal: ""},
				{tok: token.IntLit, literal: "2"},
				{tok: token.LT, literal: "LT"},
				{tok: token.IntLit, literal: "3"},
				{tok: token.AND, literal: "AND"},
				{tok: token.IntLit, literal: "4"},
				{tok: token.GT, literal: "GT"},
				{tok: token.IntLit, literal: "1"},
			},
		},
		11: {
			src: "flag=5.LE.10.OR.x.GE.y",
			expect: []testtoktuple{
				{tok: token.Identifier, literal: "flag"},
				{tok: token.Equals, literal: ""},
				{tok: token.IntLit, literal: "5"},
				{tok: token.LE, literal: "LE"},
				{tok: token.IntLit, literal: "10"},
				{tok: token.OR, literal: "OR"},
				{tok: token.Identifier, literal: "x"},
				{tok: token.GE, literal: "GE"},
				{tok: token.Identifier, literal: "y"},
			},
		},
		// Test that scientific notation still works
		12: {
			src: "val=1.E5+2.E+10-3.E-5",
			expect: []testtoktuple{
				{tok: token.Identifier, literal: "val"},
				{tok: token.Equals, literal: ""},
				{tok: token.FloatLit, literal: "1.E5"},
				{tok: token.Plus, literal: ""},
				{tok: token.FloatLit, literal: "2.E+10"},
				{tok: token.Minus, literal: ""},
				{tok: token.FloatLit, literal: "3.E-5"},
			},
		},
		13: {
			src: "x=100.D0*1.5E3",
			expect: []testtoktuple{
				{tok: token.Identifier, literal: "x"},
				{tok: token.Equals, literal: ""},
				{tok: token.FloatLit, literal: "100.D0"},
				{tok: token.Asterisk, literal: ""},
				{tok: token.FloatLit, literal: "1.5E3"},
			},
		},
		// Test kind specifiers with underscore
		14: {
			src: "i=1_8+2_INT32",
			expect: []testtoktuple{
				{tok: token.Identifier, literal: "i"},
				{tok: token.Equals, literal: ""},
				{tok: token.IntLit, literal: "1_8"},
				{tok: token.Plus, literal: ""},
				{tok: token.IntLit, literal: "2_INT32"},
			},
		},
		15: {
			src: "x=1.5_4*2.0_REAL64",
			expect: []testtoktuple{
				{tok: token.Identifier, literal: "x"},
				{tok: token.Equals, literal: ""},
				{tok: token.FloatLit, literal: "1.5_4"},
				{tok: token.Asterisk, literal: ""},
				{tok: token.FloatLit, literal: "2.0_REAL64"},
			},
		},
		16: {
			src: "CALL DSYSV('L',-1_INT32)",
			expect: []testtoktuple{
				{tok: token.CALL, literal: "CALL"},
				{tok: token.Identifier, literal: "DSYSV"},
				{tok: token.LParen, literal: ""},
				{tok: token.StringLit, literal: "L"},
				{tok: token.Comma, literal: ""},
				{tok: token.Minus, literal: ""},
				{tok: token.IntLit, literal: "1_INT32"},
				{tok: token.RParen, literal: ""},
			},
		},
		17: {
			src: "!\n!\n",
			expect: []testtoktuple{
				{tok: token.LineComment},
				{tok: token.NewLine},
				{tok: token.LineComment},
				{tok: token.NewLine},
			},
		},
		18: {
			src: "PROGRAM G2DYN\n!\n!\n",
			expect: []testtoktuple{
				{tok: token.PROGRAM, literal: "PROGRAM"},
				{tok: token.Identifier, literal: "G2DYN"},
				{tok: token.NewLine},
				{tok: token.LineComment},
				{tok: token.NewLine},
				{tok: token.LineComment},
				{tok: token.NewLine},
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
				t.Errorf("%s tok %d TokenMismatch want %s got %s", l.Source(), i, expect.tok.String(), tok.String())
			}
			if string(literal) != expect.literal {
				t.Errorf("%s tok %d LiteralMismatch want %q got %q", l.Source(), i, expect.literal, literal)
			}
		}
		if !l.IsDone() {
			tok, _, lit := l.NextToken()
			t.Errorf("%s expected lexer to be done, got %s (%s)", l.Source(), lit, tok.String())
		}
	}
}

// TestLexer90_TokenLineCol tests that TokenLineCol returns correct positions for all tokens,
// especially empty comment lines which had a bug where they were reported on the wrong line.
func TestLexer90_TokenLineCol(t *testing.T) {
	type tokpos struct {
		tok  token.Token
		line int
		col  int
	}
	cases := []struct {
		name   string
		src    string
		expect []tokpos
	}{
		{
			name: "empty_comment_lines",
			// Line 1: "!" (empty comment)
			// Line 2: "!" (empty comment)
			// Line 3: "PROGRAM X"
			src: "!\n!\nPROGRAM X\n",
			expect: []tokpos{
				{tok: token.LineComment, line: 1, col: 1},
				{tok: token.NewLine, line: 1, col: 2},
				{tok: token.LineComment, line: 2, col: 1},
				{tok: token.NewLine, line: 2, col: 2},
				{tok: token.PROGRAM, line: 3, col: 1},
				{tok: token.Identifier, line: 3, col: 9},
				{tok: token.NewLine, line: 3, col: 10},
			},
		},
		{
			name: "program_then_empty_comments",
			// Line 1: "PROGRAM G2DYN"
			// Line 2: "!" (empty comment)
			// Line 3: "!" (empty comment)
			src: "PROGRAM G2DYN\n!\n!\n",
			expect: []tokpos{
				{tok: token.PROGRAM, line: 1, col: 1},
				{tok: token.Identifier, line: 1, col: 9},
				{tok: token.NewLine, line: 1, col: 14},
				{tok: token.LineComment, line: 2, col: 1},
				{tok: token.NewLine, line: 2, col: 2},
				{tok: token.LineComment, line: 3, col: 1},
				{tok: token.NewLine, line: 3, col: 2},
			},
		},
		{
			name: "comment_with_content",
			// Line 1: "! comment text"
			// Line 2: "X = 1"
			src: "! comment text\nX = 1\n",
			expect: []tokpos{
				{tok: token.LineComment, line: 1, col: 1},
				{tok: token.NewLine, line: 1, col: 15},
				{tok: token.Identifier, line: 2, col: 1},
				{tok: token.Equals, line: 2, col: 3},
				{tok: token.IntLit, line: 2, col: 5},
				{tok: token.NewLine, line: 2, col: 6},
			},
		},
		{
			name: "openmp_directive_style_comment",
			// Line 1: "!$" (OpenMP-style empty directive, should be comment)
			// Line 2: "PROGRAM TEST"
			src: "!$\nPROGRAM TEST\n",
			expect: []tokpos{
				{tok: token.LineComment, line: 1, col: 1},
				{tok: token.NewLine, line: 1, col: 3},
				{tok: token.PROGRAM, line: 2, col: 1},
				{tok: token.Identifier, line: 2, col: 9},
				{tok: token.NewLine, line: 2, col: 13},
			},
		},
	}

	var l Lexer90
	var buf [256]byte
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			rd := strings.NewReader(tc.src)
			err := l.Reset("test.f90", rd)
			if err != nil {
				t.Fatal(err)
			}
			for i, expect := range tc.expect {
				tok, pos, _ := l.NextToken()
				line, col := l.TokenLineCol()
				if tok != expect.tok {
					t.Errorf("token %d: want %v, got %v", i, expect.tok, tok)
				}
				rd.Reset(tc.src)
				wantLine, wantCol, _, err := ast.Pos(pos, pos).ToLineCol(rd, buf[:])
				if err != nil {
					t.Fatal(err)
				}
				if wantLine != line {
					t.Errorf("token %d (%s) want%d:%d got%d:%d", i, tok.String(), wantLine, wantCol, line, col)
				}
				if line != expect.line || col != expect.col {
					t.Errorf("token %d (%v): want line:col %d:%d, got %d:%d",
						i, tok, expect.line, expect.col, line, col)
				}
			}
		})
	}
}
