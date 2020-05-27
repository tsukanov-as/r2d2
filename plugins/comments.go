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
	next := decl.End.Next
	if next.Token == tokens.COMMENT {
		end := p.src[next.Pos : next.Pos+next.Len]
		_ = end
		// println("---->>> ", end)
	}
}
