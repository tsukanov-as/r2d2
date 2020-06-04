package ast

import (
	"strings"

	"github.com/tsukanov-as/r2d2/bsl/tokens"
)

// Value ...
type Value interface{}

// PrepSymbol ...
type PrepSymbol = string

// Node ...
type Node interface {
	Accept(*Visitor)
	//Place() *Place
}

// Place ...
type Place struct {
	Beg *tokens.TokenInfo
	End *tokens.TokenInfo
}

// Decl ...
type Decl interface {
	Node
}

// Stmt ...
type Stmt interface {
	Node
}

// Expr ...
type Expr interface {
	Node
}

// PrepInst ...
type PrepInst interface {
	Node
}

// PrepExpr ...
type PrepExpr interface {
	Node
}

// Module ...
type Module struct {
	Path      string
	Decls     []Decl
	Auto      []*AutoDecl
	Body      []Stmt
	Interface []Decl // TODO: Item?
}

func (node *Module) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitModule {
		plugin.VisitModule(node)
	}
	for _, decl := range node.Decls {
		decl.Accept(visitor)
	}
	for _, auto := range node.Auto {
		auto.Accept(visitor)
	}
	for _, stmt := range node.Body {
		stmt.Accept(visitor)
	}
	for _, plugin := range visitor.LeaveModule {
		plugin.LeaveModule(node)
	}
}

// Scope ...
type Scope struct {
	Outer   *Scope // optional
	Objects map[string]*Item
	Auto    []*AutoDecl
}

// Insert ...
func (s *Scope) Insert(obj *Item) (alt *Item) {
	if alt = s.Objects[obj.Name]; alt == nil {
		s.Objects[obj.Name] = obj
	}
	return alt
}

// Find ...
func (s *Scope) Find(name string) *Item {
	return s.Objects[strings.ToLower(name)]
}

// NewScope ...
func NewScope(outer *Scope) *Scope {
	const n = 4
	return &Scope{outer, make(map[string]*Item, n), []*AutoDecl{}}
}

// Item ...
type Item struct {
	Name string
	Decl Decl // optional
}

// VarModListDecl ...
type VarModListDecl struct {
	Directive *tokens.Token // optional
	List      []*VarModDecl
	Place
}

func (node *VarModListDecl) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitVarModListDecl {
		plugin.VisitVarModListDecl(node)
	}
	for _, decl := range node.List {
		decl.Accept(visitor)
	}
	for _, plugin := range visitor.LeaveVarModListDecl {
		plugin.LeaveVarModListDecl(node)
	}
}

// VarModDecl ...
type VarModDecl struct {
	Name      string
	Directive *tokens.Token // optional
	Export    bool
	Place
}

func (node *VarModDecl) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitVarModDecl {
		plugin.VisitVarModDecl(node)
	}
}

// VarLocDecl ...
type VarLocDecl struct {
	Name string
	Place
}

func (node *VarLocDecl) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitVarLocDecl {
		plugin.VisitVarLocDecl(node)
	}
}

// AutoDecl ...
type AutoDecl struct {
	Name string
	Place
}

func (node *AutoDecl) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitAutoDecl {
		plugin.VisitAutoDecl(node)
	}
}

// ParamDecl ...
type ParamDecl struct {
	Name  string
	ByVal bool
	Value Expr // optional
	Place
}

func (node *ParamDecl) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitParamDecl {
		plugin.VisitParamDecl(node)
	}
	if node.Value != nil {
		node.Value.Accept(visitor)
	}
	for _, plugin := range visitor.LeaveParamDecl {
		plugin.LeaveParamDecl(node)
	}
}

// MethodDecl ...
type MethodDecl struct {
	Sign *Signature
	Vars []*VarLocDecl
	Auto []*AutoDecl
	Body []Stmt
	Place
}

func (node *MethodDecl) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitMethodDecl {
		plugin.VisitMethodDecl(node)
	}
	for _, decl := range node.Vars {
		decl.Accept(visitor)
	}
	for _, auto := range node.Auto {
		auto.Accept(visitor)
	}
	for _, stmt := range node.Body {
		stmt.Accept(visitor)
	}
	for _, plugin := range visitor.LeaveMethodDecl {
		plugin.LeaveMethodDecl(node)
	}
}

// Signature ...
type Signature struct {
	Name      string
	Function  bool
	Directive *tokens.Token
	Params    []*ParamDecl
	Export    bool
	Place
}

func (node *Signature) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitProcSign {
		plugin.VisitProcSign(node)
	}
	for _, decl := range node.Params {
		decl.Accept(visitor)
	}
	for _, plugin := range visitor.LeaveProcSign {
		plugin.LeaveProcSign(node)
	}
}

// BasicLitExpr ...
type BasicLitExpr struct {
	Kind  tokens.Token
	Value Value
	Place
}

func (node *BasicLitExpr) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitBasicLitExpr {
		plugin.VisitBasicLitExpr(node)
	}

}

// FieldExpr ...
type FieldExpr struct {
	Name string
	Args []Expr // optional
	Place
}

func (node *FieldExpr) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitFieldExpr {
		plugin.VisitFieldExpr(node)
	}
	if node.Args != nil {
		for _, expr := range node.Args {
			if expr != nil {
				expr.Accept(visitor)
			}
		}
	}
	for _, plugin := range visitor.LeaveFieldExpr {
		plugin.LeaveFieldExpr(node)
	}
}

// IndexExpr ...
type IndexExpr struct {
	Expr Expr
	Place
}

func (node *IndexExpr) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitIndexExpr {
		plugin.VisitIndexExpr(node)
	}
	node.Expr.Accept(visitor)
	for _, plugin := range visitor.LeaveIndexExpr {
		plugin.LeaveIndexExpr(node)
	}
}

// // CallExpr ...
// type CallExpr struct {
// 	Args []Expr
// 	Place
// }

// IdentExpr ...
type IdentExpr struct {
	Item *Item
	Args []Expr // optional
	Tail []Expr
	Place
}

func (node *IdentExpr) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitIdentExpr {
		plugin.VisitIdentExpr(node)
	}
	if node.Args != nil {
		for _, expr := range node.Args {
			if expr != nil {
				expr.Accept(visitor)
			}
		}
	}
	for _, item := range node.Tail {
		item.Accept(visitor)
	}
	for _, plugin := range visitor.LeaveIdentExpr {
		plugin.LeaveIdentExpr(node)
	}
}

// UnaryExpr ...
type UnaryExpr struct {
	Operator tokens.Token
	Operand  Expr
	Place
}

func (node *UnaryExpr) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitUnaryExpr {
		plugin.VisitUnaryExpr(node)
	}
	node.Operand.Accept(visitor)
	for _, plugin := range visitor.LeaveUnaryExpr {
		plugin.LeaveUnaryExpr(node)
	}
}

// BinaryExpr ...
type BinaryExpr struct {
	Left     Expr
	Operator tokens.Token
	Right    Expr
	Place
}

func (node *BinaryExpr) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitBinaryExpr {
		plugin.VisitBinaryExpr(node)
	}
	node.Left.Accept(visitor)
	node.Right.Accept(visitor)
	for _, plugin := range visitor.LeaveBinaryExpr {
		plugin.LeaveBinaryExpr(node)
	}
}

// NewExpr ...
type NewExpr struct {
	Name *string // optional
	Args []Expr  // Expr - optional
	Place
}

func (node *NewExpr) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitNewExpr {
		plugin.VisitNewExpr(node)
	}
	if node.Args != nil {
		for _, expr := range node.Args {
			if expr != nil {
				expr.Accept(visitor)
			}
		}
	}
	for _, plugin := range visitor.LeaveNewExpr {
		plugin.LeaveNewExpr(node)
	}
}

// TernaryExpr ...
type TernaryExpr struct {
	Cond Expr
	Then Expr
	Else Expr
	Tail []Expr
	Place
}

func (node *TernaryExpr) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitTernaryExpr {
		plugin.VisitTernaryExpr(node)
	}
	node.Cond.Accept(visitor)
	node.Then.Accept(visitor)
	node.Else.Accept(visitor)
	for _, item := range node.Tail {
		item.Accept(visitor)
	}
	for _, plugin := range visitor.LeaveTernaryExpr {
		plugin.LeaveTernaryExpr(node)
	}
}

// ParenExpr ...
type ParenExpr struct {
	Expr Expr
	Place
}

func (node *ParenExpr) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitParenExpr {
		plugin.VisitParenExpr(node)
	}
	node.Expr.Accept(visitor)
	for _, plugin := range visitor.LeaveParenExpr {
		plugin.LeaveParenExpr(node)
	}
}

// NotExpr ...
type NotExpr struct {
	Expr Expr
	Place
}

func (node *NotExpr) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitNotExpr {
		plugin.VisitNotExpr(node)
	}
	node.Expr.Accept(visitor)
	for _, plugin := range visitor.LeaveNotExpr {
		plugin.LeaveNotExpr(node)
	}
}

// StringExpr ...
type StringExpr struct {
	List []*BasicLitExpr
	Place
}

func (node *StringExpr) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitStringExpr {
		plugin.VisitStringExpr(node)
	}
	for _, item := range node.List {
		item.Accept(visitor)
	}
	for _, plugin := range visitor.LeaveStringExpr {
		plugin.LeaveStringExpr(node)
	}
}

// AssignStmt ...
type AssignStmt struct {
	Left  *IdentExpr
	Right Expr
	Place
}

func (node *AssignStmt) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitAssignStmt {
		plugin.VisitAssignStmt(node)
	}
	node.Left.Accept(visitor)
	node.Right.Accept(visitor)
	for _, plugin := range visitor.LeaveAssignStmt {
		plugin.LeaveAssignStmt(node)
	}
}

// ReturnStmt ...
type ReturnStmt struct {
	Expr Expr // optional
	Place
}

func (node *ReturnStmt) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitReturnStmt {
		plugin.VisitReturnStmt(node)
	}
	if node.Expr != nil {
		node.Expr.Accept(visitor)
	}
	for _, plugin := range visitor.LeaveReturnStmt {
		plugin.LeaveReturnStmt(node)
	}
}

// BreakStmt ...
type BreakStmt struct {
	Place
}

func (node *BreakStmt) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitBreakStmt {
		plugin.VisitBreakStmt(node)
	}
}

// ContinueStmt ...
type ContinueStmt struct {
	Place
}

func (node *ContinueStmt) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitContinueStmt {
		plugin.VisitContinueStmt(node)
	}
}

// RaiseStmt ...
type RaiseStmt struct {
	Expr Expr // optional
	Place
}

func (node *RaiseStmt) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitRaiseStmt {
		plugin.VisitRaiseStmt(node)
	}
	if node.Expr != nil {
		node.Expr.Accept(visitor)
	}
	for _, plugin := range visitor.LeaveRaiseStmt {
		plugin.LeaveRaiseStmt(node)
	}
}

// ExecuteStmt ...
type ExecuteStmt struct {
	Expr Expr // TODO: nil
	Place
}

func (node *ExecuteStmt) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitExecuteStmt {
		plugin.VisitExecuteStmt(node)
	}
	node.Expr.Accept(visitor)
	for _, plugin := range visitor.LeaveExecuteStmt {
		plugin.LeaveExecuteStmt(node)
	}
}

// CallStmt ...
type CallStmt struct {
	Ident *IdentExpr
	Place
}

func (node *CallStmt) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitCallStmt {
		plugin.VisitCallStmt(node)
	}
	node.Ident.Accept(visitor)
	for _, plugin := range visitor.LeaveCallStmt {
		plugin.LeaveCallStmt(node)
	}
}

// IfStmt ...
type IfStmt struct {
	Cond  Expr
	Then  []Stmt
	ElsIf []*ElsIfStmt // optional
	Else  *ElseStmt    // optional
	Place
}

func (node *IfStmt) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitIfStmt {
		plugin.VisitIfStmt(node)
	}
	for _, stmt := range node.Then {
		stmt.Accept(visitor)
	}
	if node.ElsIf != nil {
		for _, stmt := range node.ElsIf {
			stmt.Accept(visitor)
		}
	}
	if node.Else != nil {
		node.Else.Accept(visitor)
	}
	for _, plugin := range visitor.LeaveIfStmt {
		plugin.LeaveIfStmt(node)
	}
}

// ElseStmt ...
type ElseStmt struct {
	Body []Stmt
	Place
}

func (node *ElseStmt) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitElseStmt {
		plugin.VisitElseStmt(node)
	}
	for _, stmt := range node.Body {
		stmt.Accept(visitor)
	}
	for _, plugin := range visitor.LeaveElseStmt {
		plugin.LeaveElseStmt(node)
	}
}

// ElsIfStmt ...
type ElsIfStmt struct {
	Cond Expr
	Then []Stmt
	Place
}

func (node *ElsIfStmt) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitElsIfStmt {
		plugin.VisitElsIfStmt(node)
	}
	node.Cond.Accept(visitor)
	for _, stmt := range node.Then {
		stmt.Accept(visitor)
	}
	for _, plugin := range visitor.LeaveElsIfStmt {
		plugin.LeaveElsIfStmt(node)
	}
}

// WhileStmt ...
type WhileStmt struct {
	Cond Expr
	Body []Stmt
	Place
}

func (node *WhileStmt) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitWhileStmt {
		plugin.VisitWhileStmt(node)
	}
	node.Cond.Accept(visitor)
	for _, stmt := range node.Body {
		stmt.Accept(visitor)
	}
	for _, plugin := range visitor.LeaveWhileStmt {
		plugin.LeaveWhileStmt(node)
	}
}

// ForStmt ...
type ForStmt struct {
	Ident *IdentExpr
	From  Expr
	To    Expr
	Body  []Stmt
	Place
}

func (node *ForStmt) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitForStmt {
		plugin.VisitForStmt(node)
	}
	node.Ident.Accept(visitor)
	node.From.Accept(visitor)
	node.To.Accept(visitor)
	for _, stmt := range node.Body {
		stmt.Accept(visitor)
	}
	for _, plugin := range visitor.LeaveForStmt {
		plugin.LeaveForStmt(node)
	}
}

// ForEachStmt ...
type ForEachStmt struct {
	Ident *IdentExpr
	In    Expr
	Body  []Stmt
	Place
}

func (node *ForEachStmt) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitForEachStmt {
		plugin.VisitForEachStmt(node)
	}
	node.Ident.Accept(visitor)
	node.In.Accept(visitor)
	for _, stmt := range node.Body {
		stmt.Accept(visitor)
	}
	for _, plugin := range visitor.LeaveForEachStmt {
		plugin.LeaveForEachStmt(node)
	}
}

// TryStmt ...
type TryStmt struct {
	Try    []Stmt
	Except ExceptStmt
	Place
}

func (node *TryStmt) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitTryStmt {
		plugin.VisitTryStmt(node)
	}
	for _, stmt := range node.Try {
		stmt.Accept(visitor)
	}
	node.Except.Accept(visitor)
	for _, plugin := range visitor.LeaveTryStmt {
		plugin.LeaveTryStmt(node)
	}
}

// ExceptStmt ...
type ExceptStmt struct {
	Body []Stmt
	Place
}

func (node *ExceptStmt) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitExceptStmt {
		plugin.VisitExceptStmt(node)
	}
	for _, stmt := range node.Body {
		stmt.Accept(visitor)
	}
	for _, plugin := range visitor.LeaveExceptStmt {
		plugin.LeaveExceptStmt(node)
	}
}

// GotoStmt ...
type GotoStmt struct {
	Label string
	Place
}

func (node *GotoStmt) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitGotoStmt {
		plugin.VisitGotoStmt(node)
	}
}

// LabelStmt ...
type LabelStmt struct {
	Label string
	Place
}

func (node *LabelStmt) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitLabelStmt {
		plugin.VisitLabelStmt(node)
	}
}

// PrepIfInst ...
type PrepIfInst struct {
	Cond PrepExpr
	Place
}

func (node *PrepIfInst) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitPrepIfInst {
		plugin.VisitPrepIfInst(node)
	}
	node.Cond.Accept(visitor)
	for _, plugin := range visitor.LeavePrepIfInst {
		plugin.LeavePrepIfInst(node)
	}
}

// PrepElsIfInst ...
type PrepElsIfInst struct {
	Cond PrepExpr
	Place
}

func (node *PrepElsIfInst) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitPrepElsIfInst {
		plugin.VisitPrepElsIfInst(node)
	}
	node.Cond.Accept(visitor)
	for _, plugin := range visitor.LeavePrepElsIfInst {
		plugin.LeavePrepElsIfInst(node)
	}
}

// PrepElseInst ...
type PrepElseInst struct {
	Place
}

func (node *PrepElseInst) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitPrepElseInst {
		plugin.VisitPrepElseInst(node)
	}
}

// PrepEndIfInst ...
type PrepEndIfInst struct {
	Place
}

func (node *PrepEndIfInst) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitPrepEndIfInst {
		plugin.VisitPrepEndIfInst(node)
	}
}

// PrepRegionInst ...
type PrepRegionInst struct {
	Name string
	Place
}

func (node *PrepRegionInst) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitPrepRegionInst {
		plugin.VisitPrepRegionInst(node)
	}
}

// PrepEndRegionInst ...
type PrepEndRegionInst struct {
	Place
}

func (node *PrepEndRegionInst) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitPrepEndRegionInst {
		plugin.VisitPrepEndRegionInst(node)
	}
}

// PrepBinaryExpr ...
type PrepBinaryExpr struct {
	Left     PrepExpr
	Operator tokens.Token
	Right    PrepExpr
	Place
}

func (node *PrepBinaryExpr) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitPrepBinaryExpr {
		plugin.VisitPrepBinaryExpr(node)
	}
	node.Left.Accept(visitor)
	node.Right.Accept(visitor)
	for _, plugin := range visitor.LeavePrepBinaryExpr {
		plugin.LeavePrepBinaryExpr(node)
	}
}

// PrepNotExpr ...
type PrepNotExpr struct {
	Expr PrepExpr
	Place
}

func (node *PrepNotExpr) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitPrepNotExpr {
		plugin.VisitPrepNotExpr(node)
	}
	node.Expr.Accept(visitor)
	for _, plugin := range visitor.LeavePrepNotExpr {
		plugin.LeavePrepNotExpr(node)
	}
}

// PrepSymExpr ...
type PrepSymExpr struct {
	Symbol PrepSymbol
	Exist  bool
	Place
}

func (node *PrepSymExpr) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitPrepSymExpr {
		plugin.VisitPrepSymExpr(node)
	}
}

// PrepSymExpr ...
type PrepParenExpr struct {
	Expr PrepExpr
	Place
}

func (node *PrepParenExpr) Accept(visitor *Visitor) {
	for _, plugin := range visitor.VisitPrepParenExpr {
		plugin.VisitPrepParenExpr(node)
	}
	node.Expr.Accept(visitor)
	for _, plugin := range visitor.LeavePrepParenExpr {
		plugin.LeavePrepParenExpr(node)
	}
}
