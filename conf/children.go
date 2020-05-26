package conf

import "github.com/tsukanov-as/r2d2/conf/enums"

// Attribute ...
type Attribute struct {
	MDObjectBase
	Properties *AttributeProperties
}

// AttributeProperties ...
type AttributeProperties struct {
	Name                  string
	Synonym               *LocalStringType
	Comment               string
	Type                  *TypeDescription
	PasswordMode          bool
	Format                *LocalStringType
	EditFormat            *LocalStringType
	ToolTip               *LocalStringType
	MarkNegatives         bool
	Mask                  string
	MultiLine             bool
	ExtendedEdit          bool
	FillFromFillingValue  bool
	FillValue             *FillValue
	FillChecking          enums.FillChecking
	ChoiceFoldersAndItems enums.FoldersAndItemsUse
	ChoiceParameterLinks  *ChoiceParameterLinks
	QuickChoice           enums.UseQuickChoice
	CreateOnInput         enums.CreateOnInput
	ChoiceForm            MDObjectRef
	LinkByType            *TypeLink
	ChoiceHistoryOnInput  enums.ChoiceHistoryOnInput
	Indexing              enums.Indexing
	FullTextSearch        enums.FullTextSearchUsing
	Use                   enums.AttributeUse
	ScheduleLink          MDObjectRef
	DataHistory           enums.DataHistoryUse
	//MinValue  ;
	//MaxValue  ;
	//ChoiceParameters  ;
}

// Dimension ...
type Dimension struct {
	MDObjectBase
	Properties *DimensionProperties
}

// DimensionProperties ...
type DimensionProperties struct {
	Name                  string
	Synonym               *LocalStringType
	Comment               string
	Type                  *TypeDescription
	PasswordMode          bool
	Format                *LocalStringType
	EditFormat            *LocalStringType
	ToolTip               *LocalStringType
	MarkNegatives         bool
	Mask                  string
	MultiLine             bool
	ExtendedEdit          bool
	FillChecking          enums.FillChecking
	ChoiceFoldersAndItems enums.FoldersAndItemsUse
	ChoiceParameterLinks  *ChoiceParameterLinks
	QuickChoice           enums.UseQuickChoice
	CreateOnInput         enums.CreateOnInput
	ChoiceForm            MDObjectRef
	LinkByType            *TypeLink
	ChoiceHistoryOnInput  enums.ChoiceHistoryOnInput
	Balance               bool
	AccountingFlag        MDObjectRef
	DenyIncompleteValues  bool
	Indexing              enums.Indexing
	FullTextSearch        enums.FullTextSearchUsing
	UseInTotals           bool
	RegisterDimension     MDObjectRef
	LeadingRegisterData   *MDListType
	FillFromFillingValue  bool
	FillValue             *FillValue
	Master                bool
	MainFilter            bool
	BaseDimension         bool
	ScheduleLink          MDObjectRef
	DocumentMap           *MDListType
	RegisterRecordsMap    *MDListType
	DataHistory           enums.DataHistoryUse
	//MinValue  ;
	//MaxValue  ;
	//ChoiceParameters  ;
}

// Resource ...
type Resource struct {
	MDObjectBase
	Properties *ResourceProperties
}

// ResourceProperties ...
type ResourceProperties struct {
	Name                       string
	Synonym                    *LocalStringType
	Comment                    string
	Type                       *TypeDescription
	PasswordMode               bool
	Format                     *LocalStringType
	EditFormat                 *LocalStringType
	ToolTip                    *LocalStringType
	MarkNegatives              bool
	Mask                       string
	MultiLine                  bool
	ExtendedEdit               bool
	FillChecking               enums.FillChecking
	ChoiceFoldersAndItems      enums.FoldersAndItemsUse
	ChoiceParameterLinks       *ChoiceParameterLinks
	QuickChoice                enums.UseQuickChoice
	CreateOnInput              enums.CreateOnInput
	ChoiceForm                 MDObjectRef
	LinkByType                 *TypeLink
	ChoiceHistoryOnInput       enums.ChoiceHistoryOnInput
	FullTextSearch             enums.FullTextSearchUsing
	Balance                    bool
	AccountingFlag             MDObjectRef
	ExtDimensionAccountingFlag MDObjectRef
	NameInDataSource           string
	FillFromFillingValue       bool
	FillValue                  *FillValue
	Indexing                   enums.Indexing
	DataHistory                enums.DataHistoryUse
	//MinValue  ;
	//MaxValue  ;
	//ChoiceParameters  ;
}

// Command ...
type Command struct {
	MDObjectBase
	Properties *CommandProperties
}

// CommandProperties ...
type CommandProperties struct {
	Name                 string
	Synonym              *LocalStringType
	Comment              string
	Group                IncludeInCommandCategoriesType
	CommandParameterType *TypeDescription
	ParameterUseMode     enums.CommandParameterUseMode
	ModifiesData         bool
	Representation       enums.ButtonRepresentation
	ToolTip              *LocalStringType
	//Picture  ;
	//Shortcut  ;
}

// AccountingFlag ...
type AccountingFlag struct {
	MDObjectBase
	Properties *AccountingFlagProperties
}

// AccountingFlagProperties ...
type AccountingFlagProperties struct {
	Name                  string
	Synonym               *LocalStringType
	Comment               string
	Type                  *TypeDescription
	PasswordMode          bool
	Format                *LocalStringType
	EditFormat            *LocalStringType
	ToolTip               *LocalStringType
	MarkNegatives         bool
	Mask                  string
	MultiLine             bool
	ExtendedEdit          bool
	FillFromFillingValue  bool
	FillValue             *FillValue
	FillChecking          enums.FillChecking
	ChoiceFoldersAndItems enums.FoldersAndItemsUse
	ChoiceParameterLinks  *ChoiceParameterLinks
	QuickChoice           enums.UseQuickChoice
	CreateOnInput         enums.CreateOnInput
	ChoiceForm            MDObjectRef
	LinkByType            *TypeLink
	ChoiceHistoryOnInput  enums.ChoiceHistoryOnInput
	//MinValue  ;
	//MaxValue  ;
	//ChoiceParameters  ;
}

// ExtDimensionAccountingFlag ...
type ExtDimensionAccountingFlag struct {
	MDObjectBase
	Properties *ExtDimensionAccountingFlagProperties
}

// ExtDimensionAccountingFlagProperties ...
type ExtDimensionAccountingFlagProperties struct {
	Name                  string
	Synonym               *LocalStringType
	Comment               string
	Type                  *TypeDescription
	PasswordMode          bool
	Format                *LocalStringType
	EditFormat            *LocalStringType
	ToolTip               *LocalStringType
	MarkNegatives         bool
	Mask                  string
	MultiLine             bool
	ExtendedEdit          bool
	FillFromFillingValue  bool
	FillValue             *FillValue
	FillChecking          enums.FillChecking
	ChoiceFoldersAndItems enums.FoldersAndItemsUse
	ChoiceParameterLinks  *ChoiceParameterLinks
	QuickChoice           enums.UseQuickChoice
	CreateOnInput         enums.CreateOnInput
	ChoiceForm            MDObjectRef
	LinkByType            *TypeLink
	ChoiceHistoryOnInput  enums.ChoiceHistoryOnInput
	//MinValue  ;
	//MaxValue  ;
	//ChoiceParameters  ;
}

// Column ...
type Column struct {
	MDObjectBase
	Properties *ColumnProperties
}

// ColumnProperties ...
type ColumnProperties struct {
	Name       string
	Synonym    *LocalStringType
	Comment    string
	Indexing   enums.Indexing
	References *MDListType
}

// EnumValue ...
type EnumValue struct {
	MDObjectBase
	Properties *EnumValueProperties
}

// EnumValueProperties ...
type EnumValueProperties struct {
	Name    string
	Synonym *LocalStringType
	Comment string
}

// Form ...
type Form struct {
	MDObjectBase
	Properties *FormProperties
}

// FormProperties ...
type FormProperties struct {
	Name                  string
	Synonym               *LocalStringType
	Comment               string
	FormType              enums.FormType
	IncludeHelpInContents bool
	ExtendedPresentation  *LocalStringType
	//UsePurposes  "FixedArray";
}

// Template ...
type Template struct {
	MDObjectBase
	Name         string
	Synonym      *LocalStringType
	Comment      string
	TemplateType enums.TemplateType
}

// AddressingAttribute ...
type AddressingAttribute struct {
	MDObjectBase
	Properties *AddressingAttributeProperties
}

// AddressingAttributeProperties ...
type AddressingAttributeProperties struct {
	Name                  string
	Synonym               *LocalStringType
	Comment               string
	Type                  *TypeDescription
	PasswordMode          bool
	Format                *LocalStringType
	EditFormat            *LocalStringType
	ToolTip               *LocalStringType
	MarkNegatives         bool
	Mask                  string
	MultiLine             bool
	ExtendedEdit          bool
	FillFromFillingValue  bool
	FillValue             *FillValue
	FillChecking          enums.FillChecking
	ChoiceFoldersAndItems enums.FoldersAndItemsUse
	ChoiceParameterLinks  *ChoiceParameterLinks
	QuickChoice           enums.UseQuickChoice
	CreateOnInput         enums.CreateOnInput
	ChoiceForm            MDObjectRef
	LinkByType            *TypeLink
	ChoiceHistoryOnInput  enums.ChoiceHistoryOnInput
	Indexing              enums.Indexing
	AddressingDimension   MDObjectRef
	FullTextSearch        enums.FullTextSearchUsing
	//MinValue  ;
	//MaxValue  ;
	//ChoiceParameters  ;
}

// TabularSection ...
type TabularSection struct {
	MDObjectBase
	Properties   *TabularSectionProperties
	ChildObjects *TabularSectionChildObjects
}

// TabularSectionProperties ...
type TabularSectionProperties struct {
	Name               string
	Synonym            *LocalStringType
	Comment            string
	ToolTip            *LocalStringType
	FillChecking       enums.FillChecking
	StandardAttributes *StandardAttributes
	Use                enums.AttributeUse
}

// TabularSectionChildObjects ...
type TabularSectionChildObjects struct {
	Attribute []*Attribute
}
