package plugins

import (
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
	if nextTokenInfo.Token == tokens.COMMENT {
		end := p.src[nextTokenInfo.BegOffset:nextTokenInfo.EndOffset]
		_ = end
		// println("---->>> ", end)
	}
}
