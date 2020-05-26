package conf

import (
	"github.com/tsukanov-as/r2d2/conf/enums"
)

// StandardAttributes ...
type StandardAttributes struct {
	StandardAttribute []*StandardAttribute
}

// StandardAttribute ...
type StandardAttribute struct {
	Name                 string `xml:"name,attr"`
	Synonym              *LocalStringType
	Comment              string
	ToolTip              *LocalStringType
	QuickChoice          enums.UseQuickChoice
	FillChecking         enums.FillChecking
	FillValue            FillValue
	FillFromFillingValue bool
	ChoiceParameterLinks *ChoiceParameterLinks
	LinkByType           *TypeLink
	FullTextSearch       enums.FullTextSearchUsing
	PasswordMode         bool
	DataHistory          enums.DataHistoryUse
	Format               *LocalStringType
	EditFormat           *LocalStringType
	Mask                 string
	MultiLine            bool
	ExtendedEdit         bool
	MarkNegatives        bool
	ChoiceForm           MDObjectRef
	CreateOnInput        enums.CreateOnInput
	ChoiceHistoryOnInput enums.ChoiceHistoryOnInput
	//ChoiceParameters  ;
	//MinValue  ;
	//MaxValue  ;
}

// StandardTabularSections ...
type StandardTabularSections struct {
	StandardTabularSection *StandardTabularSection
}

// StandardTabularSection ...
type StandardTabularSection struct {
	Name               string `xml:"name,attr"`
	Synonym            *LocalStringType
	Comment            string
	ToolTip            *LocalStringType
	FillChecking       enums.FillChecking
	StandardAttributes *StandardAttributes
}

// Characteristics ...
type Characteristics struct {
	Characteristic *Characteristic
}

// Characteristic ...
type Characteristic struct {
	CharacteristicTypes  *CharacteristicTypes
	CharacteristicValues *CharacteristicValues
}

// CharacteristicTypes ...
type CharacteristicTypes struct {
	From             MDObjectRef `xml:"from,attr"`
	KeyField         FieldRef
	TypesFilterField FieldRef
	// TypesFilterValue
}

// CharacteristicValues ...
type CharacteristicValues struct {
	From        MDObjectRef `xml:"from,attr"`
	ObjectField FieldRef
	TypeField   FieldRef
	// ValueField
}
