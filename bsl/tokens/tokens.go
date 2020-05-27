package tokens

import (
	"strconv"
)

// Token ...
type Token int

type TokenInfo struct {
	Token     Token
	BegOffset int
	EndOffset int
	Line      int
	Column    int
	Next      *TokenInfo
	Prev      *TokenInfo
}

// tokens
const (
	EOF Token = iota
	BAD

	IDENT
	NUMBER
	STRING
	STRINGBEG
	STRINGMID
	STRINGEND
	DATETIME
	COMMENT

	EQL // =
	NEQ // <>
	LSS // <
	GTR // >
	LEQ // <=
	GEQ // >=

	ADD // +
	SUB // -
	MUL // *
	DIV // /
	MOD // %

	LPAREN // (
	RPAREN // )

	LBRACK // [
	RBRACK // ]

	TERNARY   // ?
	COMMA     // ,
	PERIOD    // .
	COLON     // :
	SEMICOLON // ;
	DIRECTIVE // &
	LABEL     // ~

	keywordBeg

	IF
	THEN
	ELSIF
	ELSE
	ENDIF
	FOR
	EACH
	IN
	TO
	WHILE
	DO
	ENDDO
	PROCEDURE
	ENDPROCEDURE
	FUNCTION
	ENDFUNCTION
	VAR
	VAL
	RETURN
	CONTINUE
	BREAK
	AND
	OR
	NOT
	TRY
	EXCEPT
	RAISE
	ENDTRY
	NEW
	EXECUTE
	EXPORT
	GOTO
	TRUE
	FALSE
	UNDEFINED
	NULL

	keywordEnd

	preprocBeg

	PIF
	PELSIF
	PELSE
	PENDIF
	PREGION
	PENDREGION
	PUSE

	preprocEnd
)

var tokens = [...][2]string{
	EOF:       {"eof"},
	BAD:       {"bad"},
	IDENT:     {"ident"},
	NUMBER:    {"number"},
	STRING:    {"string"},
	STRINGBEG: {"stringbeg"},
	STRINGMID: {"stringmid"},
	STRINGEND: {"stringend"},
	DATETIME:  {"datetime"},
	COMMENT:   {"comment"},
	EQL:       {"="},
	NEQ:       {"<>"},
	LSS:       {"<"},
	GTR:       {">"},
	LEQ:       {"<="},
	GEQ:       {">="},
	ADD:       {"+"},
	SUB:       {"-"},
	MUL:       {"*"},
	DIV:       {"/"},
	MOD:       {"%"},
	LPAREN:    {"("},
	RPAREN:    {")"},
	LBRACK:    {"["},
	RBRACK:    {"]"},
	TERNARY:   {"?"},
	COMMA:     {","},
	PERIOD:    {"."},
	COLON:     {":"},
	SEMICOLON: {";"},
	DIRECTIVE: {"&"},
	LABEL:     {"~"},

	IF:           {"If", "Если"},
	THEN:         {"Then", "Тогда"},
	ELSIF:        {"ElsIf", "ИначеЕсли"},
	ELSE:         {"Else", "Иначе"},
	ENDIF:        {"EndIf", "КонецЕсли"},
	FOR:          {"For", "Для"},
	EACH:         {"Each", "Каждого"},
	IN:           {"In", "Из"},
	TO:           {"To", "По"},
	WHILE:        {"While", "Пока"},
	DO:           {"Do", "Цикл"},
	ENDDO:        {"EndDo", "КонецЦикла"},
	PROCEDURE:    {"Procedure", "Процедура"},
	ENDPROCEDURE: {"EndProcedure", "КонецПроцедуры"},
	FUNCTION:     {"Function", "Функция"},
	ENDFUNCTION:  {"EndFunction", "КонецФункции"},
	VAR:          {"Var", "Перем"},
	VAL:          {"Val", "Знач"},
	RETURN:       {"Return", "Возврат"},
	CONTINUE:     {"Continue", "Продолжить"},
	BREAK:        {"Break", "Прервать"},
	AND:          {"And", "И"},
	OR:           {"Or", "Или"},
	NOT:          {"Not", "Не"},
	TRY:          {"Try", "Попытка"},
	EXCEPT:       {"Except", "Исключение"},
	RAISE:        {"Raise", "ВызватьИсключение"},
	ENDTRY:       {"EndTry", "КонецПопытки"},
	NEW:          {"New", "Новый"},
	EXECUTE:      {"Execute", "Выполнить"},
	EXPORT:       {"Export", "Экспорт"},
	GOTO:         {"Goto", "Перейти"},
	TRUE:         {"True", "Истина"},
	FALSE:        {"False", "Ложь"},
	UNDEFINED:    {"Undefined", "Неопределено"},
	NULL:         {"Null", "Null"},

	PIF:        {"#If", "#Если"},
	PELSIF:     {"#ElsIf", "#ИначеЕсли"},
	PELSE:      {"#Else", "#Иначе"},
	PENDIF:     {"#EndIf", "#КонецЕсли"},
	PREGION:    {"#Region", "#Область"},
	PENDREGION: {"#EndRegion", "#КонецОбласти"},
	PUSE:       {"#Use", "#Использовать"},
}

var keywords map[runes32]Token

// Lookup ...
func Lookup(ident string) Token {
	if tok, ok := keywords[toLower(ident)]; ok {
		return tok
	}
	return IDENT
}

func (tok Token) String() string {
	s := ""
	if 0 <= tok && tok < Token(len(tokens)) {
		s = tokens[tok][0]
	}
	if s == "" {
		s = "token(" + strconv.Itoa(int(tok)) + ")"
	}
	return s
}

const maxKeyLen = 30 // НаКлиентеНаСервереБезКонтекста

type runes32 [maxKeyLen + 1]rune // 1 == rune that not null for keys > maxKeyLen

var prepinst map[runes32]Token

// LookupPrepInst ...
func LookupPrepInst(ident string) Token {
	if tok, ok := prepinst[toLower(ident)]; ok {
		return tok
	}
	return BAD
}

var directive map[runes32]Token

// LookupDirective ...
func LookupDirective(ident string) *Token {
	if tok, ok := directive[toLower(ident)]; ok {
		return &tok
	}
	tok := BAD
	return &tok
}

// InitOfExpr ...
func InitOfExpr(tok Token) bool {
	return tok == ADD ||
		tok == SUB ||
		tok == NOT ||
		tok == IDENT ||
		tok == LPAREN ||
		tok == NUMBER ||
		tok == STRING ||
		tok == STRINGBEG ||
		tok == DATETIME ||
		tok == TERNARY ||
		tok == NEW ||
		tok == TRUE ||
		tok == FALSE ||
		tok == UNDEFINED ||
		tok == NULL
}

var prepsymbol map[runes32]bool

// LookupPrepSymbol ...
func LookupPrepSymbol(ident string) bool {
	if val, ok := prepsymbol[toLower(ident)]; ok {
		return val
	}
	return false
}

func init() {

	keywords = make(map[runes32]Token)
	for i := keywordBeg + 1; i < keywordEnd; i++ {
		keywords[toLower(tokens[i][0])] = i
		keywords[toLower(tokens[i][1])] = i
	}

	prepinst = make(map[runes32]Token)
	prepinst[toLower("If")] = PIF
	prepinst[toLower("Если")] = PIF
	prepinst[toLower("Elsif")] = PELSIF
	prepinst[toLower("ИначеЕсли")] = PELSIF
	prepinst[toLower("Else")] = PELSE
	prepinst[toLower("Иначе")] = PELSE
	prepinst[toLower("Endif")] = PENDIF
	prepinst[toLower("КонецЕсли")] = PENDIF
	prepinst[toLower("Region")] = PREGION
	prepinst[toLower("Область")] = PREGION
	prepinst[toLower("EndRegion")] = PENDREGION
	prepinst[toLower("КонецОбласти")] = PENDREGION
	prepinst[toLower("Use")] = PUSE
	prepinst[toLower("Использовать")] = PUSE

	directive = make(map[runes32]Token)
	directive[toLower("AtClient")] = DIRECTIVE
	directive[toLower("НаКлиенте")] = DIRECTIVE
	directive[toLower("AtServer")] = DIRECTIVE
	directive[toLower("НаСервере")] = DIRECTIVE
	directive[toLower("AtServerNoContext")] = DIRECTIVE
	directive[toLower("НаСервереБезКонтекста")] = DIRECTIVE
	directive[toLower("AtClientAtServerNoContext")] = DIRECTIVE
	directive[toLower("НаКлиентеНаСервереБезКонтекста")] = DIRECTIVE
	directive[toLower("AtClientAtServer")] = DIRECTIVE
	directive[toLower("НаКлиентеНаСервере")] = DIRECTIVE

	prepsymbol = make(map[runes32]bool)
	prepsymbol[toLower("Client")] = true
	prepsymbol[toLower("Клиент")] = true
	prepsymbol[toLower("AtClient")] = true
	prepsymbol[toLower("НаКлиенте")] = true
	prepsymbol[toLower("AtServer")] = true
	prepsymbol[toLower("НаСервере")] = true
	prepsymbol[toLower("MobileAppClient")] = true
	prepsymbol[toLower("МобильноеПриложениеКлиент")] = true
	prepsymbol[toLower("MobileAppServer")] = true
	prepsymbol[toLower("МобильноеПриложениеСервер")] = true
	prepsymbol[toLower("ThickClientOrdinaryApplication")] = true
	prepsymbol[toLower("ТолстыйКлиентОбычноеПриложение")] = true
	prepsymbol[toLower("ThickClientManagedApplication")] = true
	prepsymbol[toLower("ТолстыйКлиентУправляемоеПриложение")] = true
	prepsymbol[toLower("Server")] = true
	prepsymbol[toLower("Сервер")] = true
	prepsymbol[toLower("ExternalConnection")] = true
	prepsymbol[toLower("ВнешнееСоединение")] = true
	prepsymbol[toLower("ThinClient")] = true
	prepsymbol[toLower("ТонкийКлиент")] = true
	prepsymbol[toLower("WebClient")] = true
	prepsymbol[toLower("ВебКлиент")] = true
}

func toLower(s string) runes32 {
	lower := runes32{0}
	i := 0
	for _, r := range s {
		if i > maxKeyLen {
			break
		} else if 0x410 <= r && r <= 0x42F { // 'А' .. 'Я'
			r += 0x430 - 0x410 // 'а' - 'А'
		} else if 'A' <= r && r <= 'Z' {
			r += 'a' - 'A'
		}
		lower[i] = r
		i++
	}
	return lower
}
