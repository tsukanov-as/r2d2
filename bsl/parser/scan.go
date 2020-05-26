package parser

import (
	"unicode/utf8"

	"github.com/tsukanov-as/r2d2/bsl/tokens"
)

func (p *Parser) readRune() {
	p.offset = p.rdOffset
	r, w := rune(p.src[p.offset]), 1
	switch {
	case r == 0:
		p.error("illegal character NUL")
	case r >= 0x80:
		r, w = utf8.DecodeRuneInString(p.src[p.offset:])
		if r == utf8.RuneError && w == 1 {
			p.error("illegal UTF-8 encoding")
		}
	}
	p.rdOffset += w
	p.curpos++
	p.chr = r
}

func (p *Parser) next() {
	if p.rdOffset < len(p.src) {
		if p.chr == '\n' {
			p.curline++
		}
		p.readRune()
	} else {
		p.offset = len(p.src)
		p.eot = true
		p.chr = -1 // eof
	}
}

func isLetter(ch rune) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || 0x410 <= ch && ch <= 0x44F || ch == '_' || ch == 0x401 || ch == 0x451 // ch >= 0x80 && unicode.IsLetter(ch)
}

func isDigit(ch rune) bool {
	return '0' <= ch && ch <= '9' // || ch >= 0x80 && unicode.IsDigit(ch)
}

func isSpace(ch rune) bool {
	return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' || ch == 0xA0 // || ch >= 0x80 && unicode.IsSpace(ch)
}

func (p *Parser) skipWhitespace() {
	for isSpace(p.chr) {
		p.next()
	}
}

func (p *Parser) scanComment() string {
	p.next()
	start := p.offset
	for p.chr != '\n' && !p.eot {
		p.next()
	}
	return p.src[start:p.offset]
}

func (p *Parser) scanIdentifier() string {
	start := p.offset
	p.next()
	for isLetter(p.chr) || isDigit(p.chr) {
		p.next()
	}
	return p.src[start:p.offset]
}

func (p *Parser) scanNumber() string {
	start := p.offset
	p.next()
	for isDigit(p.chr) {
		p.next()
	}
	if p.chr == '.' {
		p.next()
		for isDigit(p.chr) {
			p.next()
		}
	}
	return p.src[start:p.offset]
}

func (p *Parser) scanString() string {
	start := p.offset
	p.chr = '"'
	for p.chr == '"' {
		p.next()
		for p.chr != '"' && p.chr != '\n' && !p.eot {
			p.next()
		}
		if p.chr == '"' {
			p.next()
		}
	}
	return p.src[start:p.offset]
}

func (p *Parser) scanDateTime() string {
	p.next()
	start := p.offset
	for p.chr != '\'' && !p.eot {
		p.next()
	}
	return p.src[start:p.offset]
}

func (p *Parser) scan() tokens.Token {

	p.endpos = p.curpos
	p.endline = p.curline

	if p.lit != "" && p.lit[len(p.lit)-1] == '\n' {
		p.curline++
	}

scan:

	p.skipWhitespace()

	switch ch := p.chr; {
	case isLetter(ch):
		p.lit = p.scanIdentifier()
		p.tok = tokens.Lookup(p.lit)
	case isDigit(ch):
		p.lit = p.scanNumber()
		p.tok = tokens.NUMBER
	default:
		switch ch {
		case '"':
			p.lit = p.scanString()
			if p.lit[len(p.lit)-1] == '"' {
				p.tok = tokens.STRING
			} else {
				p.tok = tokens.STRINGBEG
			}
		case '|':
			p.lit = p.scanString()
			if p.lit[len(p.lit)-1] == '"' {
				p.tok = tokens.STRINGEND
			} else {
				p.tok = tokens.STRINGMID
			}
		case '\'':
			p.lit = p.scanDateTime()
			if p.chr == '\'' {
				p.next()
			} else {
				p.error("error: date not closed")
			}
			p.tok = tokens.DATETIME
		case '=':
			p.tok = tokens.EQL
			p.next()
		case '<':
			p.next()
			if p.chr == '=' {
				p.tok = tokens.LEQ
				p.next()
			} else if p.chr == '>' {
				p.tok = tokens.NEQ
				p.next()
			} else {
				p.tok = tokens.LSS
			}
		case '>':
			p.next()
			if p.chr == '=' {
				p.tok = tokens.GEQ
				p.next()
			} else {
				p.tok = tokens.GTR
			}
		case '+':
			p.tok = tokens.ADD
			p.next()
		case '-':
			p.tok = tokens.SUB
			p.next()
		case '*':
			p.tok = tokens.MUL
			p.next()
		case '/':
			p.next()
			if p.chr == '/' {
				p.comments[p.curline] = p.scanComment()
				p.tok = tokens.COMMENT
				p.next()
				goto scan
			} else {
				p.tok = tokens.DIV
			}
		case '%':
			p.tok = tokens.MOD
			p.next()
		case '(':
			p.tok = tokens.LPAREN
			p.next()
		case ')':
			p.tok = tokens.RPAREN
			p.next()
		case '[':
			p.tok = tokens.LBRACK
			p.next()
		case ']':
			p.tok = tokens.RBRACK
			p.next()
		case '?':
			p.tok = tokens.TERNARY
			p.next()
		case ',':
			p.tok = tokens.COMMA
			p.next()
		case '.':
			p.tok = tokens.PERIOD
			p.next()
		case ':':
			p.tok = tokens.COLON
			p.next()
		case ';':
			p.tok = tokens.SEMICOLON
			p.next()
		case '&':
			p.next()
			if !isLetter(p.chr) {
				p.error("error: expected directive")
			}
			p.lit = p.scanIdentifier()
			p.tok = *tokens.LookupDirective(p.lit)
			if p.tok == tokens.BAD {
				p.error("error: unknown directive")
			}
		case '#':
			p.next()
			p.skipWhitespace()
			if !isLetter(p.chr) {
				p.error("error: expected preprocessor")
			}
			p.lit = p.scanIdentifier()
			p.tok = tokens.LookupPrepInst(p.lit)
			if p.tok == tokens.BAD {
				p.error("error: unknown preprocessor")
			}
		case '~':
			p.next()
			p.skipWhitespace()
			if isLetter(p.chr) || isDigit(p.chr) {
				p.lit = p.scanIdentifier()
			}
			p.tok = tokens.LABEL
		case -1:
			p.tok = tokens.EOF
		}
	}
	//println(p.tok.String())
	return p.tok
}
