package conf

import "github.com/tsukanov-as/r2d2/conf/enums"

// Decimal ...
type Decimal = string

// UUID ...
type UUID = string

// DataPath ...
type DataPath = string

// MDObjectRef ...
type MDObjectRef = string

// MDMethodRef ...
type MDMethodRef = string

// FieldRef ...
type FieldRef = string

// IncludeInCommandCategoriesType ...
type IncludeInCommandCategoriesType = string

// QName ...
type QName = string

// LocalStringType ...
type LocalStringType struct {
	Item []LocalStringTypeItem `xml:"item"`
}

// LocalStringTypeItem ...
type LocalStringTypeItem struct {
	Lang    string `xml:"lang"`
	Content string `xml:"content"`
}

// MDListType ...
type MDListType struct {
	Item []MDListTypeItem
}

// MDListTypeItem ...
type MDListTypeItem struct {
	Type  string `xml:"type,attr"`
	Value string `xml:",chardata"`
}

// FieldList ...
type FieldList struct {
	Field FieldListItem
}

// FieldListItem ...
type FieldListItem struct {
	Type  string `xml:"type,attr"`
	Value string `xml:",chardata"`
}

// ChoiceParameterLinks ...
type ChoiceParameterLinks struct {
	Link []*ChoiceParameterLink
}

// ChoiceParameterLink ...
type ChoiceParameterLink struct {
	Name        string
	DataPath    string
	ValueChange enums.LinkedValueChangeMode
}

// ChoiceParameters ...
type ChoiceParameters struct {
	item []*ChoiceParameter
}

// ChoiceParameter ...
type ChoiceParameter struct { // TODO: сделать
	choiceParameter string
	//value ""; // ChoiceParameter()
}

// TypeLink ...
type TypeLink struct {
	DataPath    DataPath
	LinkItem    Decimal
	ValueChange enums.LinkedValueChangeMode
}

// FillValue ...
type FillValue struct {
	Type  string `xml:"type,attr"`
	Value string `xml:",chardata"`
}
