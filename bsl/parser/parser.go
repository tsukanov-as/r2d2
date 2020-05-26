package parser

import (
	"fmt"
	"io/ioutil"
	"log"
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
	pos     int
	line    int
	tokpos  int
	linpos  int
	tokInfo *tokens.TokenInfo

	isFunc    bool
	allowVar  bool
	directive *tokens.Token

	vars      map[string]*ast.Item
	methods   map[string]*ast.Item
	unknown   map[string]*ast.Item
	callSites map[*ast.Item][]*tokens.TokenInfo
	exported  []ast.Decl
}

// Init ...
func (p *Parser) Init(path string) {
	src, err := ioutil.ReadFile(path)
	checkError(err, "Unable to read file")
	p.path = path
	p.src = string(src[3:])
	p.scope = ast.NewScope(nil)
	p.line = 1
	p.offset = 0
	p.rdOffset = 0
	p.vars = make(map[string]*ast.Item)
	p.methods = make(map[string]*ast.Item)
	p.unknown = make(map[string]*ast.Item)
	p.callSites = make(map[*ast.Item][]*tokens.TokenInfo)
	p.tokInfo = &tokens.TokenInfo{}
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
		log.Fatal(msg)
	}
}

func (p *Parser) error(msg string) {
	panic(fmt.Sprint(msg, " in ", p.path, ":", p.line))
}

func (p *Parser) warning(msg string) {
	//println(msg)
}

func (p *Parser) expect(tok tokens.Token) {
	if p.tok != tok {
		//spew.Dump(p)
		p.error(fmt.Sprintf("Expected '%v'", tok.String()))
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
	}
}

// @DECL

func (p *Parser) parseModDecls() []ast.Decl {
	var list []ast.Decl
	p.allowVar = true
loop:
	for {
		p.directive = nil
		for p.tok == tokens.DIRECTIVE {
			p.directive = tokens.LookupDirective(p.lit)
			p.scan()
		}
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
			list = append(list, p.parsePrepRegionInst())
			p.scan()
		case tokens.PENDREGION:
			list = append(list, p.parsePrepEndRegionInst())
			p.scan()
		case tokens.PIF:
			list = append(list, p.parsePrepIfInst())
			p.scan()
		case tokens.PELSIF:
			list = append(list, p.parsePrepElsIfInst())
			p.scan()
		case tokens.PELSE:
			list = append(list, p.parsePrepElseInst())
			p.scan()
		case tokens.PENDIF:
			list = append(list, p.parsePrepEndIfInst())
			p.scan()
		default:
			break loop
		}
	}
	return list
}

func (p *Parser) parseVarModListDecl() ast.Decl {
	beg := p.tokInfo
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
			Beg: beg,
			End: p.tokInfo.Prev,
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
	beg := p.tokInfo
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
			Beg: beg,
			End: p.tokInfo.Prev,
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
			Beg: p.tokInfo,
			End: p.tokInfo,
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
	beg := p.tokInfo
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
				Beg: beg,
				End: p.tokInfo.Prev,
			},
		}
	} else {
		sign = &ast.ProcSign{
			Name:      name,
			Directive: p.directive,
			Params:    params,
			Export:    export,
			Place: ast.Place{
				Beg: beg,
				End: p.tokInfo.Prev,
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
			Beg: beg,
			End: p.tokInfo.Prev,
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
	beg := p.tokInfo
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
			Beg: beg,
			End: p.tokInfo.Prev,
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
	beg := p.tokInfo
	expr := p.parseAndExpr()
	for p.tok == tokens.OR {
		op := p.tok
		p.scan()
		expr = &ast.BinaryExpr{
			Left:     expr,
			Operator: op,
			Right:    p.parseAndExpr(),
			Place: ast.Place{
				Beg: beg,
				End: p.tokInfo.Prev,
			},
		}
	}
	return expr
}

func (p *Parser) parseAndExpr() ast.Expr {
	beg := p.tokInfo
	expr := p.parseNotExpr()
	for p.tok == tokens.AND {
		op := p.tok
		p.scan()
		expr = &ast.BinaryExpr{
			Left:     expr,
			Operator: op,
			Right:    p.parseNotExpr(),
			Place: ast.Place{
				Beg: beg,
				End: p.tokInfo.Prev,
			},
		}
	}
	return expr
}

func (p *Parser) parseNotExpr() (expr ast.Expr) {
	beg := p.tokInfo
	if p.tok == tokens.NOT {
		p.scan()
		expr = &ast.NotExpr{
			Expr: p.parseRelExpr(),
			Place: ast.Place{
				Beg: beg,
				End: p.tokInfo.Prev,
			},
		}
	} else {
		expr = p.parseRelExpr()
	}
	return expr
}

func (p *Parser) parseRelExpr() ast.Expr {
	beg := p.tokInfo
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
				Beg: beg,
				End: p.tokInfo.Prev,
			},
		}
	}
	return expr
}

func (p *Parser) parseAddExpr() ast.Expr {
	beg := p.tokInfo
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
				Beg: beg,
				End: p.tokInfo.Prev,
			},
		}
	}
	return expr
}

func (p *Parser) parseMulExpr() ast.Expr {
	beg := p.tokInfo
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
				Beg: beg,
				End: p.tokInfo.Prev,
			},
		}
	}
	return expr
}

func (p *Parser) parseUnaryExpr() ast.Expr {
	beg := p.tokInfo
	switch op := p.tok; op {
	case tokens.ADD, tokens.SUB:
		p.scan()
		return &ast.UnaryExpr{
			Operator: op,
			Operand:  p.parseOperand(),
			Place: ast.Place{
				Beg: beg,
				End: p.tokInfo.Prev,
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
				Beg: p.tokInfo,
				End: p.tokInfo,
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
	beg := p.tokInfo
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
			Beg: beg,
			End: p.tokInfo.Prev,
		},
	}
}

func (p *Parser) appendStringPart(list []*ast.BasicLitExpr) []*ast.BasicLitExpr {
	list = append(list, &ast.BasicLitExpr{
		Kind:  p.tok,
		Value: nil,
		Place: ast.Place{
			Beg: p.tokInfo,
			End: p.tokInfo,
		},
	})
	p.scan()
	return list
}

func (p *Parser) parseNewExpr() ast.Expr {
	beg := p.tokInfo
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
			Beg: beg,
			End: p.tokInfo.Prev,
		},
	}
}

func (p *Parser) parseIdentExpr(allowNewVar bool) (expr *ast.IdentExpr, newvar *ast.Item, call bool) {
	beg := p.tokInfo
	name := p.lit
	autoPlace := p.tokInfo
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
				callSites := []*tokens.TokenInfo{}
				callSites = append(callSites, autoPlace)
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
				decl := &ast.AutoDecl{
					Name: name,
					Place: ast.Place{
						Beg: autoPlace,
						End: autoPlace,
					},
				}
				item = &ast.Item{Name: name, Decl: decl}
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
			Beg: beg,
			End: p.tokInfo.Prev,
		},
	}, newvar, call
}

func (p *Parser) parseTail(call *bool) (tail []ast.Expr) {
	beg := p.tokInfo
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
					Beg: beg,
					End: p.tokInfo.Prev,
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
					Beg: beg,
					End: p.tokInfo.Prev,
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
	beg := p.tokInfo
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
			Beg: beg,
			End: p.tokInfo.Prev,
		},
	}
}

func (p *Parser) parseParenExpr() ast.Expr {
	beg := p.tokInfo
	p.scan()
	expr := p.parseExpression()
	p.expect(tokens.RPAREN)
	p.scan()
	return &ast.ParenExpr{
		Expr: expr,
		Place: ast.Place{
			Beg: beg,
			End: p.tokInfo.Prev,
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
		stmt = p.parseLabelStmt()
	case tokens.PREGION:
		stmt = p.parsePrepRegionInst()
	case tokens.PENDREGION:
		stmt = p.parsePrepEndRegionInst()
	case tokens.PIF:
		stmt = p.parsePrepIfInst()
	case tokens.PELSIF:
		stmt = p.parsePrepElsIfInst()
	case tokens.PELSE:
		stmt = p.parsePrepElseInst()
	case tokens.PENDIF:
		stmt = p.parsePrepEndIfInst()
	case tokens.SEMICOLON:
		// NOP
	}
	return stmt
}

func (p *Parser) parseRaiseStmt() *ast.RaiseStmt {
	beg := p.tokInfo
	var expr ast.Expr
	p.scan()
	if tokens.InitOfExpr(p.tok) {
		expr = p.parseExpression()
	}
	return &ast.RaiseStmt{
		Expr: expr,
		Place: ast.Place{
			Beg: beg,
			End: p.tokInfo.Prev,
		},
	}
}

func (p *Parser) parseExecuteStmt() *ast.ExecuteStmt {
	beg := p.tokInfo
	p.scan()
	return &ast.ExecuteStmt{
		Expr: p.parseExpression(),
		Place: ast.Place{
			Beg: beg,
			End: p.tokInfo.Prev,
		},
	}
}

func (p *Parser) parseAssignOrCallStmt() (stmt ast.Stmt) {
	beg := p.tokInfo
	left, newvar, call := p.parseIdentExpr(true)
	if call {
		stmt = &ast.CallStmt{
			Ident: left,
			Place: ast.Place{
				Beg: beg,
				End: p.tokInfo.Prev,
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
				Beg: beg,
				End: p.tokInfo.Prev,
			},
		}
	}
	return stmt
}

func (p *Parser) parseIfStmt() *ast.IfStmt {
	beg := p.tokInfo
	p.scan()
	cond := p.parseExpression()
	p.expect(tokens.THEN)
	p.scan()
	thenpart := p.parseStatements()
	var elsifpart []*ast.ElsIfStmt
	if p.tok == tokens.ELSIF {
		elsifpart = []*ast.ElsIfStmt{}
		for p.tok == tokens.ELSIF {
			beg := p.tokInfo
			p.scan()
			elsifcond := p.parseExpression()
			p.expect(tokens.THEN)
			p.scan()
			elsifthen := p.parseStatements()
			elsifpart = append(elsifpart, &ast.ElsIfStmt{
				Cond: elsifcond,
				Then: elsifthen,
				Place: ast.Place{
					Beg: beg,
					End: p.tokInfo.Prev,
				},
			})
		}
	}
	var elsepart *ast.ElseStmt
	if p.tok == tokens.ELSE {
		beg := p.tokInfo
		p.scan()
		elsepart = &ast.ElseStmt{
			Body: p.parseStatements(),
			Place: ast.Place{
				Beg: beg,
				End: p.tokInfo.Prev,
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
			Beg: beg,
			End: p.tokInfo.Prev,
		},
	}
}

func (p *Parser) parseTryStmt() *ast.TryStmt {
	beg := p.tokInfo
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
			Beg: beg,
			End: p.tokInfo.Prev,
		},
	}
}

func (p *Parser) parseExceptStmt() ast.ExceptStmt {
	beg := p.tokInfo
	p.scan()
	return ast.ExceptStmt{
		Body: p.parseStatements(),
		Place: ast.Place{
			Beg: beg,
			End: p.tokInfo.Prev,
		},
	}
}

func (p *Parser) parseWhileStmt() *ast.WhileStmt {
	beg := p.tokInfo
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
			Beg: beg,
			End: p.tokInfo.Prev,
		},
	}
}

func (p *Parser) parseForStmt() *ast.ForStmt {
	beg := p.tokInfo
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
			Beg: beg,
			End: p.tokInfo.Prev,
		},
	}
}

func (p *Parser) parseForEachStmt() *ast.ForEachStmt {
	beg := p.tokInfo
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
			Beg: beg,
			End: p.tokInfo.Prev,
		},
	}
}

func (p *Parser) parseGotoStmt() *ast.GotoStmt {
	beg := p.tokInfo
	p.scan()
	p.expect(tokens.LABEL)
	label := p.lit
	p.scan()
	return &ast.GotoStmt{
		Label: label,
		Place: ast.Place{
			Beg: beg,
			End: p.tokInfo.Prev,
		},
	}
}

func (p *Parser) parseLabelStmt() *ast.LabelStmt {
	beg := p.tokInfo
	label := p.lit
	p.scan()
	p.expect(tokens.COLON)
	p.tok = tokens.SEMICOLON // cheat code
	return &ast.LabelStmt{
		Label: label,
		Place: ast.Place{
			Beg: beg,
			End: p.tokInfo.Prev,
		},
	}
}

func (p *Parser) parseReturnStmt() *ast.ReturnStmt {
	beg := p.tokInfo
	p.scan()
	var expr ast.Expr
	if p.isFunc {
		expr = p.parseExpression()
	}
	return &ast.ReturnStmt{
		Expr: expr,
		Place: ast.Place{
			Beg: beg,
			End: p.tokInfo.Prev,
		},
	}
}

// @PREP

func (p *Parser) parsePrepExpression() ast.PrepExpr {
	beg := p.tokInfo
	expr := p.parsePrepAndExpr()
	for p.tok == tokens.OR {
		op := p.tok
		p.scan()
		expr = &ast.PrepBinaryExpr{
			Left:     expr,
			Operator: op,
			Right:    p.parsePrepAndExpr(),
			Place: ast.Place{
				Beg: beg,
				End: p.tokInfo.Prev,
			},
		}
	}
	return expr
}

func (p *Parser) parsePrepAndExpr() ast.PrepExpr {
	beg := p.tokInfo
	expr := p.parsePrepNotExpr()
	for p.tok == tokens.AND {
		op := p.tok
		p.scan()
		expr = &ast.PrepBinaryExpr{
			Left:     expr,
			Operator: op,
			Right:    p.parsePrepNotExpr(),
			Place: ast.Place{
				Beg: beg,
				End: p.tokInfo.Prev,
			},
		}
	}
	return expr
}

func (p *Parser) parsePrepNotExpr() (expr ast.PrepExpr) {
	beg := p.tokInfo
	if p.tok == tokens.NOT {
		p.scan()
		expr = &ast.PrepNotExpr{
			Expr: p.parsePrepSymExpr(),
			Place: ast.Place{
				Beg: beg,
				End: p.tokInfo.Prev,
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
				Beg: p.tokInfo,
				End: p.tokInfo,
			},
		}
	}
	return expr
}

func (p *Parser) parsePrepIfInst() *ast.PrepIfInst {
	beg := p.tokInfo
	p.scan()
	cond := p.parseExpression()
	p.expect(tokens.THEN)
	p.tok = tokens.SEMICOLON // cheatcode
	return &ast.PrepIfInst{
		Cond: cond,
		Place: ast.Place{
			Beg: beg,
			End: p.tokInfo,
		},
	}
}

func (p *Parser) parsePrepElsIfInst() *ast.PrepElsIfInst {
	beg := p.tokInfo
	p.scan()
	cond := p.parseExpression()
	p.expect(tokens.THEN)
	p.tok = tokens.SEMICOLON // cheatcode
	return &ast.PrepElsIfInst{
		Cond: cond,
		Place: ast.Place{
			Beg: beg,
			End: p.tokInfo.Prev,
		},
	}
}

func (p *Parser) parsePrepElseInst() *ast.PrepElseInst {
	beg := p.tokInfo
	p.tok = tokens.SEMICOLON // cheatcode
	return &ast.PrepElseInst{
		Place: ast.Place{
			Beg: beg,
			End: p.tokInfo,
		},
	}
}

func (p *Parser) parsePrepEndIfInst() *ast.PrepEndIfInst {
	beg := p.tokInfo
	p.tok = tokens.SEMICOLON // cheatcode
	return &ast.PrepEndIfInst{
		Place: ast.Place{
			Beg: beg,
			End: p.tokInfo,
		},
	}
}

func (p *Parser) parsePrepRegionInst() *ast.PrepRegionInst {
	beg := p.tokInfo
	p.scan()
	p.expect(tokens.IDENT)
	name := p.lit
	p.tok = tokens.SEMICOLON // cheatcode
	return &ast.PrepRegionInst{
		Name: name,
		Place: ast.Place{
			Beg: beg,
			End: p.tokInfo,
		},
	}
}

func (p *Parser) parsePrepEndRegionInst() *ast.PrepEndRegionInst {
	beg := p.tokInfo
	p.tok = tokens.SEMICOLON // cheatcode
	return &ast.PrepEndRegionInst{
		Place: ast.Place{
			Beg: beg,
			End: p.tokInfo,
		},
	}
}
