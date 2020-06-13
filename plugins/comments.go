package plugins

import (
	"strings"

	"github.com/tsukanov-as/r2d2/bsl/ast"
	"github.com/tsukanov-as/r2d2/bsl/parser"
	"github.com/tsukanov-as/r2d2/bsl/tokens"
)

type pluginWrongComment struct {
	src string
}

func PluginWrongComment(p *parser.Parser) *pluginWrongComment {
	return &pluginWrongComment{src: p.Source()}
}

func (p *pluginWrongComment) VisitMethodDecl(node ast.Node) {
	decl := node.(*ast.MethodDecl)
	nextTokenInfo := decl.End.Next
	if nextTokenInfo.Token == tokens.COMMENT && nextTokenInfo.Line == decl.End.Line {
		comment := strings.TrimRight(p.src[nextTokenInfo.BegOffset:nextTokenInfo.EndOffset], " \t\r\n")
		validComment := " " + decl.Sign.Name + "()"
		if comment != validComment {
			// fmt.Printf("wrong comment: `%v` valid comment: `%v`\n", comment, validComment)
		}
	}
}
