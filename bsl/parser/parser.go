package parser

import (
	"fmt"
	"io/ioutil"
	"strings"

	"github.com/tsukanov-as/r2d2/bsl/ast"
	"github.com/tsukanov-as/r2d2/bsl/tokens"
)

// Parser ...
type Parser struct {
	path     string
	src      string
	offset   int
	rdOffset int
	chr      rune
	eot      bool
	err      int

	scope   *ast.Scope
	tok     tokens.Token
	lit     string
	curpos  int
	begpos  int
	endpos  int
	curline int
	endline int

	isFunc    bool
	allowVar  bool
	directive *tokens.Token

	vars      map[string]*ast.Item
	methods   map[string]*ast.Item
	unknown   map[string]*ast.Item
	callSites map[*ast.Item][]*ast.Place
	exported  []ast.Decl
	comments  map[int]string
}

// Init ...
func (p *Parser) Init(path string) {
	src, err := ioutil.ReadFile(path)
	checkError(err, "Unable to read file")
	p.path = path
	p.src = string(src[3:])
	p.scope = ast.NewScope(nil)
	p.curline = 1
	p.comments = make(map[int]string)
	p.offset = 0
	p.rdOffset = 0
	p.vars = make(map[string]*ast.Item)
	p.methods = make(map[string]*ast.Item)
	p.unknown = make(map[string]*ast.Item)
	p.callSites = make(map[*ast.Item][]*ast.Place)
	if len(p.src) > 0 {
		p.readRune()
		p.eot = false
	} else {
		p.eot = true
		p.chr = -1 // eof
	}
}

func checkError(err error, msg string) {
	if err != nil {
		//log.Fatal(msg)
		println(msg)
	}
}

func (p *Parser) error(msg string) {
	print("line: ", p.curline)
	println(msg)
	//panic(msg)
}

func (p *Parser) warning(msg string) {
	//println(msg)
}

func (p *Parser) expect(tok tokens.Token) {
	if p.tok != tok {
		//spew.Dump(p)
		p.error(fmt.Sprintf(" expected '%v'", tok.String()))
		println(p.path)
		println("-----------------------")
	}
}

func (p *Parser) openScope() {
	p.scope = ast.NewScope(p.scope)
	p.vars = p.scope.Objects
}

func (p *Parser) closeScope() {
	p.scope = p.scope.Outer
	p.vars = p.scope.Objects
}

// Parse ...
func (p *Parser) Parse() *ast.Module {
	p.scan()
	decls := p.parseModDecls()
	body := p.parseStatements()
	return &ast.Module{
		Path:      p.path,
		Decls:     decls,
		Auto:      p.scope.Auto,
		Body:      body,
		Interface: p.exported,
		Comments:  p.comments,
	}
}

// @DECL

func (p *Parser) parseModDecls() []ast.Decl {
	var list []ast.Decl
	p.allowVar = true
	for p.tok == tokens.DIRECTIVE {
		p.directive = tokens.LookupDirective(p.lit)
		p.scan()
	}
loop:
	for {
		pos, line := p.begpos, p.curline
		switch p.tok {
		case tokens.VAR:
			if !p.allowVar {
				break loop
			}
			list = append(list, p.parseVarModListDecl())
		case tokens.FUNCTION:
			p.isFunc = true
			list = append(list, p.parseMethodDecl())
			p.isFunc = false
			p.allowVar = false
		case tokens.PROCEDURE:
			list = append(list, p.parseMethodDecl())
			p.allowVar = false
		case tokens.PREGION:
			inst := p.parsePrepRegionInst()
			p.scan()
			inst.Place = ast.Place{
				Pos:     pos,
				Len:     p.endpos - pos,
				BegLine: line,
				EndLine: p.endline,
			}
			list = append(list, inst)
		case tokens.PENDREGION:
			p.scan()
			list = append(list, &ast.PrepEndRegionInst{
				Place: ast.Place{
					Pos:     pos,
					Len:     p.endpos - pos,
					BegLine: line,
					EndLine: p.endline,
				},
			})
		case tokens.PIF:
			inst := p.parsePrepIfInst()
			p.scan()
			inst.Place = ast.Place{
				Pos:     pos,
				Len:     p.endpos - pos,
				BegLine: line,
				EndLine: p.endline,
			}
			list = append(list, inst)
		case tokens.PELSIF:
			inst := p.parsePrepElsIfInst()
			p.scan()
			inst.Place = ast.Place{
				Pos:     pos,
				Len:     p.endpos - pos,
				BegLine: line,
				EndLine: p.endline,
			}
			list = append(list, inst)
		case tokens.PELSE:
			p.scan()
			list = append(list, &ast.PrepElseInst{
				Place: ast.Place{
					Pos:     pos,
					Len:     p.endpos - pos,
					BegLine: line,
					EndLine: p.endline,
				},
			})
		case tokens.PENDIF:
			p.scan()
			list = append(list, &ast.PrepEndIfInst{
				Place: ast.Place{
					Pos:     pos,
					Len:     p.endpos - pos,
					BegLine: line,
					EndLine: p.endline,
				},
			})
		default:
			break loop
		}
		p.directive = nil
		for p.tok == tokens.DIRECTIVE {
			p.directive = tokens.LookupDirective(p.lit)
			p.scan()
		}
	}
	return list
}

func (p *Parser) parseVarModListDecl() ast.Decl {
	pos, line := p.begpos, p.curline
	p.scan()
	var list []*ast.VarModDecl
	list = append(list, p.parseVarModDecl())
	for p.tok == tokens.COMMA {
		p.scan()
		list = append(list, p.parseVarModDecl())
	}
	decl := &ast.VarModListDecl{
		Directive: p.directive,
		List:      list,
		Place: ast.Place{
			Pos:     pos,
			Len:     p.endpos - pos,
			BegLine: line,
			EndLine: p.endline,
		},
	}
	p.expect(tokens.SEMICOLON)
	p.scan()
	for p.tok == tokens.SEMICOLON {
		p.scan()
	}
	return decl
}

func (p *Parser) parseVarModDecl() *ast.VarModDecl {
	pos, line := p.begpos, p.curline
	p.expect(tokens.IDENT)
	name := p.lit
	p.scan()
	export := false
	if p.tok == tokens.EXPORT {
		export = true
		p.scan()
	}
	decl := &ast.VarModDecl{
		Name:      name,
		Directive: p.directive,
		Export:    export,
		Place: ast.Place{
			Pos:     pos,
			Len:     p.endpos - pos,
			BegLine: line,
			EndLine: p.endline,
		},
	}
	if export {
		p.exported = append(p.exported, decl)
	}
	nameLower := strings.ToLower(name)
	if p.vars[nameLower] != nil {
		p.error("Identifier already declared")
	}
	p.vars[nameLower] = &ast.Item{
		Name: name,
		Decl: decl,
	}
	return decl
}

func (p *Parser) parseVars() (list []*ast.VarLocDecl) {
	for p.tok == tokens.VAR {
		p.scan()
		list = append(list, p.parseVarLocDecl())
		for p.tok == tokens.COMMA {
			p.scan()
			list = append(list, p.parseVarLocDecl())
		}
		p.expect(tokens.SEMICOLON)
		p.scan()
	}
	return list
}

func (p *Parser) parseVarLocDecl() *ast.VarLocDecl {
	p.expect(tokens.IDENT)
	name := p.lit
	decl := &ast.VarLocDecl{
		Name: name,
		Place: ast.Place{
			Pos:     p.begpos,
			Len:     p.curpos - p.begpos,
			BegLine: p.curline,
			EndLine: p.endline,
		},
	}
	p.vars[strings.ToLower(name)] = &ast.Item{
		Name: name,
		Decl: decl,
	}
	p.scan()
	return decl
}

func (p *Parser) parseMethodDecl() *ast.MethodDecl {
	pos, line := p.begpos, p.curline
	export := false
	p.scan()
	p.expect(tokens.IDENT)
	name := p.lit
	p.scan()
	p.openScope()
	params := p.parseParams()
	if p.tok == tokens.EXPORT {
		export = true
		p.scan()
	}
	var sign ast.Decl
	if p.isFunc {
		sign = &ast.FuncSign{
			Name:      name,
			Directive: p.directive,
			Params:    params,
			Export:    export,
			Place: ast.Place{
				Pos:     pos,
				Len:     p.endpos - pos,
				BegLine: line,
				EndLine: p.endline,
			},
		}
	} else {
		sign = &ast.ProcSign{
			Name:      name,
			Directive: p.directive,
			Params:    params,
			Export:    export,
			Place: ast.Place{
				Pos:     pos,
				Len:     p.endpos - pos,
				BegLine: line,
				EndLine: p.endline,
			},
		}
	}
	nameLower := strings.ToLower(name)
	object := p.unknown[nameLower]
	if object != nil {
		delete(p.unknown, nameLower)
		object.Decl = sign
	} else {
		object = &ast.Item{
			Name: name,
			Decl: sign,
		}
	}
	if p.methods[nameLower] != nil {
		p.error("Method already declared")
	}
	p.methods[nameLower] = object
	if export {
		p.methods[nameLower] = object
	}
	vars := p.parseVars()
	body := p.parseStatements()
	if p.isFunc {
		p.expect(tokens.ENDFUNCTION)
	} else {
		p.expect(tokens.ENDPROCEDURE)
	}
	var auto []*ast.AutoDecl
	for _, obj := range p.scope.Auto {
		auto = append(auto, obj)
	}
	p.closeScope()
	p.scan()
	return &ast.MethodDecl{
		Sign: sign,
		Vars: vars,
		Auto: auto,
		Body: body,
		Place: ast.Place{
			Pos:     pos,
			Len:     p.endpos - pos,
			BegLine: line,
			EndLine: p.endline,
		},
	}
}

func (p *Parser) parseParams() (list []*ast.ParamDecl) {
	p.expect(tokens.LPAREN)
	p.scan()
	if p.tok != tokens.RPAREN {
		list = append(list, p.parseParamDecl())
		for p.tok == tokens.COMMA {
			p.scan()
			list = append(list, p.parseParamDecl())
		}
	}
	p.expect(tokens.RPAREN)
	p.scan()
	return list
}

func (p *Parser) parseParamDecl() (decl *ast.ParamDecl) {
	pos, line := p.begpos, p.curline
	byval := false
	if p.tok == tokens.VAL {
		byval = true
		p.scan()
	}
	p.expect(tokens.IDENT)
	name := p.lit
	p.scan()
	var expr ast.Expr
	if p.tok == tokens.EQL {
		p.scan()
		expr = p.parseUnaryExpr()
	}
	decl = &ast.ParamDecl{
		Name:  name,
		ByVal: byval,
		Value: expr,
		Place: ast.Place{
			Pos:     pos,
			Len:     p.endpos - pos,
			BegLine: line,
			EndLine: p.endline,
		},
	}
	nameLower := strings.ToLower(name)
	if p.vars[nameLower] != nil {
		p.error("Identifier already declared")
	}
	p.vars[nameLower] = &ast.Item{
		Name: name,
		Decl: decl,
	}
	return decl
}

// @EXPR

func (p *Parser) parseExpression() ast.Expr {
	pos, line := p.begpos, p.curline
	expr := p.parseAndExpr()
	for p.tok == tokens.OR {
		op := p.tok
		p.scan()
		expr = &ast.BinaryExpr{
			Left:     expr,
			Operator: op,
			Right:    p.parseAndExpr(),
			Place: ast.Place{
				Pos:     pos,
				Len:     p.endpos - pos,
				BegLine: line,
				EndLine: p.endline,
			},
		}
	}
	return expr
}

func (p *Parser) parseAndExpr() ast.Expr {
	pos, line := p.begpos, p.curline
	expr := p.parseNotExpr()
	for p.tok == tokens.AND {
		op := p.tok
		p.scan()
		expr = &ast.BinaryExpr{
			Left:     expr,
			Operator: op,
			Right:    p.parseNotExpr(),
			Place: ast.Place{
				Pos:     pos,
				Len:     p.endpos - pos,
				BegLine: line,
				EndLine: p.endline,
			},
		}
	}
	return expr
}

func (p *Parser) parseNotExpr() (expr ast.Expr) {
	pos, line := p.begpos, p.curline
	if p.tok == tokens.NOT {
		p.scan()
		expr = &ast.NotExpr{
			Expr: p.parseRelExpr(),
			Place: ast.Place{
				Pos:     pos,
				Len:     p.endpos - pos,
				BegLine: line,
				EndLine: p.endline,
			},
		}
	} else {
		expr = p.parseRelExpr()
	}
	return expr
}

func (p *Parser) parseRelExpr() ast.Expr {
	pos, line := p.begpos, p.curline
	expr := p.parseAddExpr()
	for p.tok == tokens.EQL ||
		p.tok == tokens.NEQ ||
		p.tok == tokens.LSS ||
		p.tok == tokens.GTR ||
		p.tok == tokens.LEQ ||
		p.tok == tokens.GEQ {
		op := p.tok
		p.scan()
		expr = &ast.BinaryExpr{
			Left:     expr,
			Operator: op,
			Right:    p.parseAddExpr(),
			Place: ast.Place{
				Pos:     pos,
				Len:     p.endpos - pos,
				BegLine: line,
				EndLine: p.endline,
			},
		}
	}
	return expr
}

func (p *Parser) parseAddExpr() ast.Expr {
	pos, line := p.begpos, p.curline
	expr := p.parseMulExpr()
	for p.tok == tokens.ADD ||
		p.tok == tokens.SUB {
		op := p.tok
		p.scan()
		expr = &ast.BinaryExpr{
			Left:     expr,
			Operator: op,
			Right:    p.parseMulExpr(),
			Place: ast.Place{
				Pos:     pos,
				Len:     p.endpos - pos,
				BegLine: line,
				EndLine: p.endline,
			},
		}
	}
	return expr
}

func (p *Parser) parseMulExpr() ast.Expr {
	pos, line := p.begpos, p.curline
	expr := p.parseUnaryExpr()
	for p.tok == tokens.MUL ||
		p.tok == tokens.DIV ||
		p.tok == tokens.MOD {
		op := p.tok
		p.scan()
		expr = &ast.BinaryExpr{
			Left:     expr,
			Operator: op,
			Right:    p.parseUnaryExpr(),
			Place: ast.Place{
				Pos:     pos,
				Len:     p.endpos - pos,
				BegLine: line,
				EndLine: p.endline,
			},
		}
	}
	return expr
}

func (p *Parser) parseUnaryExpr() ast.Expr {
	pos, line := p.begpos, p.curline
	switch op := p.tok; op {
	case tokens.ADD, tokens.SUB:
		p.scan()
		return &ast.UnaryExpr{
			Operator: op,
			Operand:  p.parseOperand(),
			Place: ast.Place{
				Pos:     pos,
				Len:     p.endpos - pos,
				BegLine: line,
				EndLine: p.endline,
			},
		}
	case tokens.EOF:
		return nil
	default:
		return p.parseOperand()
	}
}

func (p *Parser) parseOperand() (expr ast.Expr) {
	switch p.tok {
	case tokens.STRING, tokens.STRINGBEG:
		expr = p.parseStringExpr()
	case tokens.NUMBER, tokens.DATETIME, tokens.TRUE, tokens.FALSE, tokens.UNDEFINED, tokens.NULL:
		expr = &ast.BasicLitExpr{
			Kind:  p.tok,
			Value: nil, // TODO: value
			Place: ast.Place{
				Pos:     p.begpos,
				Len:     p.curpos - p.begpos,
				BegLine: p.curline,
				EndLine: p.endline,
			},
		}
		p.scan()
	case tokens.IDENT:
		expr, _, _ = p.parseIdentExpr(false)
	case tokens.LPAREN:
		expr = p.parseParenExpr()
	case tokens.NEW:
		expr = p.parseNewExpr()
	case tokens.TERNARY:
		expr = p.parseTernaryExpr()
	default:
		p.error("Expected operand")
	}
	return expr
}

func (p *Parser) parseStringExpr() ast.Expr {
	pos, line := p.begpos, p.curline
	var list []*ast.BasicLitExpr
loop:
	for {
		switch p.tok {
		case tokens.STRING:
			for p.tok == tokens.STRING {
				list = p.appendStringPart(list)
			}
		case tokens.STRINGBEG:
			list = p.appendStringPart(list)
			for p.tok == tokens.STRINGMID {
				list = p.appendStringPart(list)
			}
			if p.tok != tokens.STRINGEND {
				p.error("Expected \"")
			}
			list = p.appendStringPart(list)
		default:
			break loop
		}
	}
	return &ast.StringExpr{
		List: list,
		Place: ast.Place{
			Pos:     pos,
			Len:     p.endpos - pos,
			BegLine: line,
			EndLine: p.endline,
		},
	}
}

func (p *Parser) appendStringPart(list []*ast.BasicLitExpr) []*ast.BasicLitExpr {
	list = append(list, &ast.BasicLitExpr{
		Kind:  p.tok,
		Value: nil,
		Place: ast.Place{
			Pos:     p.begpos,
			Len:     p.curpos - p.begpos,
			BegLine: p.curline,
			EndLine: p.endline,
		},
	})
	p.scan()
	return list
}

func (p *Parser) parseNewExpr() ast.Expr {
	pos, line := p.begpos, p.curline
	var name *string
	var args []ast.Expr
	p.scan()
	if p.tok == tokens.IDENT {
		name = &p.lit
		p.scan()
	}
	if p.tok == tokens.LPAREN {
		p.scan()
		if p.tok != tokens.RPAREN {
			args = p.parseArguments()
			p.expect(tokens.RPAREN)
		}
		p.scan()
	}
	if name == nil && len(args) == 0 {
		p.error("Expected constructor")
	}
	return &ast.NewExpr{
		Name: name,
		Args: args,
		Place: ast.Place{
			Pos:     pos,
			Len:     p.endpos - pos,
			BegLine: line,
			EndLine: p.endline,
		},
	}
}

func (p *Parser) parseIdentExpr(allowNewVar bool) (expr *ast.IdentExpr, newvar *ast.Item, call bool) {
	pos, line := p.begpos, p.curline
	name := p.lit
	autoPlace := ast.Place{
		Pos:     p.begpos,
		Len:     p.curpos - p.begpos,
		BegLine: p.curline,
		EndLine: p.endline,
	}
	p.scan()
	var item *ast.Item
	var args []ast.Expr
	var tail []ast.Expr
	if p.tok == tokens.LPAREN {
		if p.scan() == tokens.RPAREN {
			args = []ast.Expr{}
		} else {
			args = p.parseArguments()
		}
		p.expect(tokens.RPAREN)
		p.scan()
		nameLower := strings.ToLower(name)
		if item = p.methods[nameLower]; item == nil {
			if item = p.unknown[nameLower]; item == nil {
				item = &ast.Item{Name: name, Decl: nil}
				p.unknown[nameLower] = item
				callSites := []*ast.Place{}
				callSites = append(callSites, &autoPlace)
				p.callSites[item] = callSites
			}
		}
		call = true
		tail = p.parseTail(&call)
	} else {
		call = false
		tail = p.parseTail(&call)
		if len(tail) > 0 {
			allowNewVar = false
		}
		item = p.scope.Find(name)
		if item == nil {
			if allowNewVar {
				item = &ast.Item{Name: name, Decl: &ast.AutoDecl{Name: name, Place: autoPlace}}
				newvar = item
			} else {
				item = &ast.Item{Name: name, Decl: nil}
				p.warning("Undeclared identifier " + name) // TODO: name
			}
		}
	}
	return &ast.IdentExpr{
		Item: item,
		Tail: tail,
		Args: args,
		Place: ast.Place{
			Pos:     pos,
			Len:     p.endpos - pos,
			BegLine: line,
			EndLine: p.endline,
		},
	}, newvar, call
}

func (p *Parser) parseTail(call *bool) (tail []ast.Expr) {
	pos, line := p.begpos, p.curline
	var args []ast.Expr
loop:
	for {
		switch p.tok {
		case tokens.PERIOD:
			p.scan()
			if p.tok != tokens.IDENT && tokens.Lookup(p.lit) == tokens.IDENT { // TODO: isName()
				p.expect(tokens.IDENT)
			}
			name := p.lit
			if p.scan() == tokens.LPAREN {
				if p.scan() == tokens.RPAREN {
					args = []ast.Expr{}
				} else {
					args = p.parseArguments()
				}
				p.expect(tokens.RPAREN)
				p.scan()
				*call = true
			} else {
				*call = false
			}
			expr := &ast.FieldExpr{
				Name: name,
				Args: args,
				Place: ast.Place{
					Pos:     pos,
					Len:     p.endpos - pos,
					BegLine: line,
					EndLine: p.endline,
				},
			}
			tail = append(tail, expr)
		case tokens.LBRACK:
			*call = false
			if p.scan() == tokens.RBRACK {
				p.error("Expected expression")
			}
			index := p.parseExpression()
			p.expect(tokens.RBRACK)
			p.scan()
			expr := &ast.IndexExpr{
				Expr: index,
				Place: ast.Place{
					Pos:     pos,
					Len:     p.endpos - pos,
					BegLine: line,
					EndLine: p.endline,
				},
			}
			tail = append(tail, expr)
		default:
			break loop
		}
	}
	return tail
}

func (p *Parser) parseArguments() (args []ast.Expr) {
	for {
		if tokens.InitOfExpr(p.tok) {
			args = append(args, p.parseExpression())
		} else {
			args = append(args, nil)
		}
		if p.tok == tokens.COMMA {
			p.scan()
		} else {
			break
		}
	}
	return args
}

func (p *Parser) parseTernaryExpr() ast.Expr {
	pos, line := p.begpos, p.curline
	p.scan()
	p.expect(tokens.LPAREN)
	p.scan()
	cond := p.parseExpression()
	p.expect(tokens.COMMA)
	p.scan()
	thenpart := p.parseExpression()
	p.expect(tokens.COMMA)
	p.scan()
	elsepart := p.parseExpression()
	p.expect(tokens.RPAREN)
	var tail []ast.Expr
	if p.scan() == tokens.PERIOD {
		call := false
		tail = p.parseTail(&call)
	} else {
		tail = []ast.Expr{}
	}
	return &ast.TernaryExpr{
		Cond: cond,
		Then: thenpart,
		Else: elsepart,
		Tail: tail,
		Place: ast.Place{
			Pos:     pos,
			Len:     p.endpos - pos,
			BegLine: line,
			EndLine: p.endline,
		},
	}
}

func (p *Parser) parseParenExpr() ast.Expr {
	pos, line := p.begpos, p.curline
	p.scan()
	expr := p.parseExpression()
	p.expect(tokens.RPAREN)
	p.scan()
	return &ast.ParenExpr{
		Expr: expr,
		Place: ast.Place{
			Pos:     pos,
			Len:     p.endpos - pos,
			BegLine: line,
			EndLine: p.endline,
		},
	}
}

// @STMT

func (p *Parser) parseStatements() (list []ast.Stmt) {
	if stmt := p.parseStmt(); stmt != nil {
		list = append(list, stmt)
	}
loop:
	for {
		switch p.tok {
		case tokens.SEMICOLON:
			p.scan()
		case tokens.PREGION, tokens.PENDREGION, tokens.PIF, tokens.PELSIF, tokens.PELSE, tokens.PENDIF:
		default:
			break loop
		}
		if stmt := p.parseStmt(); stmt != nil {
			list = append(list, stmt)
		}
	}
	return list
}

func (p *Parser) parseStmt() (stmt ast.Stmt) {
	switch p.tok {
	case tokens.IDENT:
		stmt = p.parseAssignOrCallStmt()
	case tokens.IF:
		stmt = p.parseIfStmt()
	case tokens.TRY:
		stmt = p.parseTryStmt()
	case tokens.WHILE:
		stmt = p.parseWhileStmt()
	case tokens.FOR:
		p.scan()
		if p.tok == tokens.EACH {
			stmt = p.parseForEachStmt()
		} else {
			stmt = p.parseForStmt()
		}
	case tokens.RETURN:
		stmt = p.parseReturnStmt()
	case tokens.BREAK:
		p.scan()
		stmt = &ast.BreakStmt{}
	case tokens.CONTINUE:
		p.scan()
		stmt = &ast.ContinueStmt{}
	case tokens.RAISE:
		stmt = p.parseRaiseStmt()
	case tokens.EXECUTE:
		stmt = p.parseExecuteStmt()
	case tokens.GOTO:
		stmt = p.parseGotoStmt()
	case tokens.LABEL:
		stmt = &ast.LabelStmt{
			Label: p.lit,
			Place: ast.Place{
				Pos:     p.begpos,
				Len:     p.curpos - p.begpos,
				BegLine: p.curline,
				EndLine: p.endline,
			}}
		p.scan()
		p.expect(tokens.COLON)
		p.tok = tokens.SEMICOLON // cheat code
	case tokens.PREGION:
		stmt = p.parsePrepRegionInst()
	case tokens.PENDREGION:
		stmt = &ast.PrepEndRegionInst{}
		p.tok = tokens.SEMICOLON // cheat code
	case tokens.PIF:
		stmt = p.parsePrepIfInst()
	case tokens.PELSIF:
		stmt = p.parsePrepElsIfInst()
	case tokens.PELSE:
		stmt = &ast.PrepElseInst{}
		p.tok = tokens.SEMICOLON // cheat code
	case tokens.PENDIF:
		stmt = &ast.PrepEndIfInst{}
		p.tok = tokens.SEMICOLON // cheat code
	case tokens.SEMICOLON:
		// NOP
	}
	return stmt
}

func (p *Parser) parseRaiseStmt() *ast.RaiseStmt {
	pos, line := p.begpos, p.curline
	var expr ast.Expr
	p.scan()
	if tokens.InitOfExpr(p.tok) {
		expr = p.parseExpression()
	}
	return &ast.RaiseStmt{
		Expr: expr,
		Place: ast.Place{
			Pos:     pos,
			Len:     p.endpos - pos,
			BegLine: line,
			EndLine: p.endline,
		},
	}
}

func (p *Parser) parseExecuteStmt() *ast.ExecuteStmt {
	pos, line := p.begpos, p.curline
	p.scan()
	return &ast.ExecuteStmt{
		Expr: p.parseExpression(),
		Place: ast.Place{
			Pos:     pos,
			Len:     p.endpos - pos,
			BegLine: line,
			EndLine: p.endline,
		},
	}
}

func (p *Parser) parseAssignOrCallStmt() (stmt ast.Stmt) {
	pos, line := p.begpos, p.curline
	left, newvar, call := p.parseIdentExpr(true)
	if call {
		stmt = &ast.CallStmt{
			Ident: left,
			Place: ast.Place{
				Pos:     pos,
				Len:     p.endpos - pos,
				BegLine: line,
				EndLine: p.endline,
			},
		}
	} else {
		p.expect(tokens.EQL)
		p.scan()
		right := p.parseExpression()
		if newvar != nil {
			nameLower := strings.ToLower(newvar.Name)
			p.vars[nameLower] = newvar
			if decl, ok := newvar.Decl.(*ast.AutoDecl); ok {
				p.scope.Auto = append(p.scope.Auto, decl)
			}
		}
		stmt = &ast.AssignStmt{
			Left:  left,
			Right: right,
			Place: ast.Place{
				Pos:     pos,
				Len:     p.endpos - pos,
				BegLine: line,
				EndLine: p.endline,
			},
		}
	}
	return stmt
}

func (p *Parser) parseIfStmt() *ast.IfStmt {
	pos, line := p.begpos, p.curline
	p.scan()
	cond := p.parseExpression()
	p.expect(tokens.THEN)
	p.scan()
	thenpart := p.parseStatements()
	var elsifpart []*ast.ElsIfStmt
	if p.tok == tokens.ELSIF {
		elsifpart = []*ast.ElsIfStmt{}
		for p.tok == tokens.ELSIF {
			pos, line := p.begpos, p.curline
			p.scan()
			elsifcond := p.parseExpression()
			p.expect(tokens.THEN)
			p.scan()
			elsifthen := p.parseStatements()
			elsifpart = append(elsifpart, &ast.ElsIfStmt{
				Cond: elsifcond,
				Then: elsifthen,
				Place: ast.Place{
					Pos:     pos,
					Len:     p.endpos - pos,
					BegLine: line,
					EndLine: p.endline,
				},
			})
		}
	}
	var elsepart *ast.ElseStmt
	if p.tok == tokens.ELSE {
		pos, line := p.begpos, p.curline
		p.scan()
		elsepart = &ast.ElseStmt{
			Body: p.parseStatements(),
			Place: ast.Place{
				Pos:     pos,
				Len:     p.endpos - pos,
				BegLine: line,
				EndLine: p.endline,
			},
		}
	}
	p.expect(tokens.ENDIF)
	p.scan()
	return &ast.IfStmt{
		Cond:  cond,
		Then:  thenpart,
		ElsIf: elsifpart,
		Else:  elsepart,
		Place: ast.Place{
			Pos:     pos,
			Len:     p.endpos - pos,
			BegLine: line,
			EndLine: p.endline,
		},
	}
}

func (p *Parser) parseTryStmt() *ast.TryStmt {
	pos, line := p.begpos, p.curline
	p.scan()
	try := p.parseStatements()
	p.expect(tokens.EXCEPT)
	except := p.parseExceptStmt()
	p.expect(tokens.ENDTRY)
	p.scan()
	return &ast.TryStmt{
		Try:    try,
		Except: except,
		Place: ast.Place{
			Pos:     pos,
			Len:     p.endpos - pos,
			BegLine: line,
			EndLine: p.endline,
		},
	}
}

func (p *Parser) parseExceptStmt() ast.ExceptStmt {
	pos, line := p.begpos, p.curline
	p.scan()
	return ast.ExceptStmt{
		Body: p.parseStatements(),
		Place: ast.Place{
			Pos:     pos,
			Len:     p.endpos - pos,
			BegLine: line,
			EndLine: p.endline,
		},
	}
}

func (p *Parser) parseWhileStmt() *ast.WhileStmt {
	pos, line := p.begpos, p.curline
	p.scan()
	cond := p.parseExpression()
	p.expect(tokens.DO)
	p.scan()
	body := p.parseStatements()
	p.expect(tokens.ENDDO)
	p.scan()
	return &ast.WhileStmt{
		Cond: cond,
		Body: body,
		Place: ast.Place{
			Pos:     pos,
			Len:     p.endpos - pos,
			BegLine: line,
			EndLine: p.endline,
		},
	}
}

func (p *Parser) parseForStmt() *ast.ForStmt {
	pos, line := p.begpos, p.curline
	p.expect(tokens.IDENT)
	ident, newvar, call := p.parseIdentExpr(true)
	if call {
		p.error("Expected variable") // TODO: var pos
	}
	p.expect(tokens.EQL)
	p.scan()
	from := p.parseExpression()
	p.expect(tokens.TO)
	p.scan()
	until := p.parseExpression()
	if newvar != nil {
		nameLower := strings.ToLower(newvar.Name)
		p.vars[nameLower] = newvar
		if decl, ok := newvar.Decl.(*ast.AutoDecl); ok {
			p.scope.Auto = append(p.scope.Auto, decl)
		}
	}
	p.expect(tokens.DO)
	p.scan()
	body := p.parseStatements()
	p.expect(tokens.ENDDO)
	p.scan()
	return &ast.ForStmt{
		Ident: ident,
		From:  from,
		To:    until,
		Body:  body,
		Place: ast.Place{
			Pos:     pos,
			Len:     p.endpos - pos,
			BegLine: line,
			EndLine: p.endline,
		},
	}
}

func (p *Parser) parseForEachStmt() *ast.ForEachStmt {
	pos, line := p.begpos, p.curline
	p.scan()
	p.expect(tokens.IDENT)
	ident, newvar, call := p.parseIdentExpr(true)
	if call {
		p.error("Expected variable") // TODO: var pos
	}
	p.expect(tokens.IN)
	p.scan()
	collection := p.parseExpression()
	if newvar != nil {
		nameLower := strings.ToLower(newvar.Name)
		p.vars[nameLower] = newvar
		if decl, ok := newvar.Decl.(*ast.AutoDecl); ok {
			p.scope.Auto = append(p.scope.Auto, decl)
		}
	}
	p.expect(tokens.DO)
	p.scan()
	body := p.parseStatements()
	p.expect(tokens.ENDDO)
	p.scan()
	return &ast.ForEachStmt{
		Ident: ident,
		In:    collection,
		Body:  body,
		Place: ast.Place{
			Pos:     pos,
			Len:     p.endpos - pos,
			BegLine: line,
			EndLine: p.endline,
		},
	}
}

func (p *Parser) parseGotoStmt() *ast.GotoStmt {
	pos, line := p.begpos, p.curline
	p.scan()
	p.expect(tokens.LABEL)
	label := p.lit
	p.scan()
	return &ast.GotoStmt{
		Label: label,
		Place: ast.Place{
			Pos:     pos,
			Len:     p.endpos - pos,
			BegLine: line,
			EndLine: p.endline,
		},
	}
}

func (p *Parser) parseReturnStmt() *ast.ReturnStmt {
	pos, line := p.begpos, p.curline
	p.scan()
	var expr ast.Expr
	if p.isFunc {
		expr = p.parseExpression()
	}
	return &ast.ReturnStmt{
		Expr: expr,
		Place: ast.Place{
			Pos:     pos,
			Len:     p.endpos - pos,
			BegLine: line,
			EndLine: p.endline,
		},
	}
}

// @PREP

func (p *Parser) parsePrepExpression() ast.PrepExpr {
	pos, line := p.begpos, p.curline
	expr := p.parsePrepAndExpr()
	for p.tok == tokens.OR {
		op := p.tok
		p.scan()
		expr = &ast.PrepBinaryExpr{
			Left:     expr,
			Operator: op,
			Right:    p.parsePrepAndExpr(),
			Place: ast.Place{
				Pos:     pos,
				Len:     p.endpos - pos,
				BegLine: line,
				EndLine: p.endline,
			},
		}
	}
	return expr
}

func (p *Parser) parsePrepAndExpr() ast.PrepExpr {
	pos, line := p.begpos, p.curline
	expr := p.parsePrepNotExpr()
	for p.tok == tokens.AND {
		op := p.tok
		p.scan()
		expr = &ast.PrepBinaryExpr{
			Left:     expr,
			Operator: op,
			Right:    p.parsePrepNotExpr(),
			Place: ast.Place{
				Pos:     pos,
				Len:     p.endpos - pos,
				BegLine: line,
				EndLine: p.endline,
			},
		}
	}
	return expr
}

func (p *Parser) parsePrepNotExpr() (expr ast.PrepExpr) {
	pos, line := p.begpos, p.curline
	if p.tok == tokens.NOT {
		p.scan()
		expr = &ast.PrepNotExpr{
			Expr: p.parsePrepSymExpr(),
			Place: ast.Place{
				Pos:     pos,
				Len:     p.endpos - pos,
				BegLine: line,
				EndLine: p.endline,
			},
		}
	} else {
		expr = p.parsePrepSymExpr()
	}
	return expr
}

func (p *Parser) parsePrepSymExpr() (expr ast.PrepExpr) {
	if p.tok == tokens.IDENT {
		exist := tokens.LookupPrepSymbol(p.lit)
		expr = &ast.PrepSymExpr{
			Symbol: p.lit,
			Exist:  exist,
			Place: ast.Place{
				Pos:     p.begpos,
				Len:     p.curpos - p.begpos,
				BegLine: p.curline,
				EndLine: p.endline,
			},
		}
	}
	return expr
}

func (p *Parser) parsePrepIfInst() *ast.PrepIfInst {
	pos, line := p.begpos, p.curline
	p.scan()
	cond := p.parseExpression()
	p.expect(tokens.THEN)
	p.tok = tokens.SEMICOLON
	return &ast.PrepIfInst{
		Cond: cond,
		Place: ast.Place{
			Pos:     pos,
			Len:     p.curpos - pos,
			BegLine: line,
			EndLine: p.endline,
		},
	}
}

func (p *Parser) parsePrepElsIfInst() *ast.PrepElsIfInst {
	pos, line := p.begpos, p.curline
	p.scan()
	cond := p.parseExpression()
	p.expect(tokens.THEN)
	p.tok = tokens.SEMICOLON
	return &ast.PrepElsIfInst{
		Cond: cond,
		Place: ast.Place{
			Pos:     pos,
			Len:     p.curpos - pos,
			BegLine: line,
			EndLine: p.endline,
		},
	}
}

func (p *Parser) parsePrepRegionInst() *ast.PrepRegionInst {
	pos, line := p.begpos, p.curline
	p.scan()
	p.expect(tokens.IDENT)
	name := p.lit
	p.tok = tokens.SEMICOLON
	return &ast.PrepRegionInst{
		Name: name,
		Place: ast.Place{
			Pos:     pos,
			Len:     p.curpos - pos,
			BegLine: line,
			EndLine: p.endline,
		},
	}
}
