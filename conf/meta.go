package conf

import "github.com/tsukanov-as/r2d2/conf/enums"

// MetaDataObject ...
type MetaDataObject struct {
	Version                    Decimal `xml:"version,attr"`
	AccountingRegister         *AccountingRegister
	AccumulationRegister       *AccumulationRegister
	BusinessProcess            *BusinessProcess
	CalculationRegister        *CalculationRegister
	Catalog                    *Catalog
	ChartOfAccounts            *ChartOfAccounts
	ChartOfCalculationTypes    *ChartOfCalculationTypes
	ChartOfCharacteristicTypes *ChartOfCharacteristicTypes
	CommandGroup               *CommandGroup
	CommonAttribute            *CommonAttribute
	CommonCommand              *CommonCommand
	CommonForm                 *CommonForm
	CommonModule               *CommonModule
	CommonPicture              *CommonPicture
	CommonTemplate             *CommonTemplate
	Configuration              *Configuration
	Constant                   *Constant
	DataProcessor              *DataProcessor
	Document                   *Document
	DocumentJournal            *DocumentJournal
	DocumentNumerator          *DocumentNumerator
	Enum                       *Enum
	EventSubscription          *EventSubscription
	ExchangePlan               *ExchangePlan
	FilterCriterion            *FilterCriterion
	Form                       *Form
	FunctionalOption           *FunctionalOption
	FunctionalOptionsParameter *FunctionalOptionsParameter
	HTTPService                *HTTPService
	InformationRegister        *InformationRegister
	Language                   *Language
	Report                     *Report
	Role                       *Role
	ScheduledJob               *ScheduledJob
	Sequence                   *Sequence
	SessionParameter           *SessionParameter
	SettingsStorage            *SettingsStorage
	Subsystem                  *Subsystem
	Task                       *Task
	Template                   *Template
	WebService                 *WebService
	WSReference                *WSReference
	XDTOPackage                *XDTOPackage
}

// MDObjectBase ...
type MDObjectBase struct {
	UUID UUID `xml:"uuid,attr"`
	//InternalInfo InternalInfo
}

// AccountingRegister ...
type AccountingRegister struct {
	MDObjectBase
	Properties   *AccountingRegisterProperties
	ChildObjects *AccountingRegisterChildObjects
}

// AccountingRegisterProperties ...
type AccountingRegisterProperties struct {
	Name                     string
	Synonym                  *LocalStringType
	Comment                  string
	UseStandardCommands      bool
	IncludeHelpInContents    bool
	ChartOfAccounts          MDObjectRef
	Correspondence           bool
	PeriodAdjustmentLength   Decimal
	DefaultListForm          MDObjectRef
	AuxiliaryListForm        MDObjectRef
	StandardAttributes       *StandardAttributes
	DataLockControlMode      enums.DefaultDataLockControlMode
	EnableTotalsSplitting    bool
	FullTextSearch           enums.FullTextSearchUsing
	ListPresentation         *LocalStringType
	ExtendedListPresentation *LocalStringType
	Explanation              *LocalStringType
}

// AccountingRegisterChildObjects ...
type AccountingRegisterChildObjects struct {
	Dimension []*Dimension
	Resource  []*Resource
	Attribute []*Attribute
	Form      []string
	Template  []string
	Command   []*Command
}

// AccumulationRegister ...
type AccumulationRegister struct {
	MDObjectBase
	Properties   *AccumulationRegisterProperties
	ChildObjects *AccumulationRegisterChildObjects
}

// AccumulationRegisterProperties ...
type AccumulationRegisterProperties struct {
	Name                     string
	Synonym                  *LocalStringType
	Comment                  string
	UseStandardCommands      bool
	DefaultListForm          MDObjectRef
	AuxiliaryListForm        MDObjectRef
	RegisterType             enums.AccumulationRegisterType
	IncludeHelpInContents    bool
	StandardAttributes       *StandardAttributes
	DataLockControlMode      enums.DefaultDataLockControlMode
	FullTextSearch           enums.FullTextSearchUsing
	EnableTotalsSplitting    bool
	ListPresentation         *LocalStringType
	ExtendedListPresentation *LocalStringType
	Explanation              *LocalStringType
}

// AccumulationRegisterChildObjects ...
type AccumulationRegisterChildObjects struct {
	Resource  []*Resource
	Attribute []*Attribute
	Dimension []*Dimension
	Form      []string
	Template  []string
	Command   []*Command
}

// BusinessProcess ...
type BusinessProcess struct {
	MDObjectBase
	Properties   *BusinessProcessProperties
	ChildObjects *BusinessProcessChildObjects
}

// BusinessProcessProperties ...
type BusinessProcessProperties struct {
	Name                             string
	Synonym                          *LocalStringType
	Comment                          string
	UseStandardCommands              bool
	EditType                         enums.EditType
	InputByString                    *FieldList
	CreateOnInput                    enums.CreateOnInput
	SearchStringModeOnInputByString  enums.SearchStringModeOnInputByString
	ChoiceDataGetModeOnInputByString enums.ChoiceDataGetModeOnInputByString
	FullTextSearchOnInputByString    enums.FullTextSearchOnInputByString
	DefaultObjectForm                MDObjectRef
	DefaultListForm                  MDObjectRef
	DefaultChoiceForm                MDObjectRef
	AuxiliaryObjectForm              MDObjectRef
	AuxiliaryListForm                MDObjectRef
	AuxiliaryChoiceForm              MDObjectRef
	ChoiceHistoryOnInput             enums.ChoiceHistoryOnInput
	NumberType                       enums.BusinessProcessNumberType
	NumberLength                     Decimal
	NumberAllowedLength              enums.AllowedLength
	CheckUnique                      bool
	StandardAttributes               *StandardAttributes
	Characteristics                  *Characteristics
	Autonumbering                    bool
	BasedOn                          MDListType
	NumberPeriodicity                enums.BusinessProcessNumberPeriodicity
	Task                             MDObjectRef
	CreateTaskInPrivilegedMode       bool
	DataLockFields                   *FieldList
	DataLockControlMode              enums.DefaultDataLockControlMode
	IncludeHelpInContents            bool
	FullTextSearch                   enums.FullTextSearchUsing
	ObjectPresentation               *LocalStringType
	ExtendedObjectPresentation       *LocalStringType
	ListPresentation                 *LocalStringType
	ExtendedListPresentation         *LocalStringType
	Explanation                      *LocalStringType
}

// BusinessProcessChildObjects ...
type BusinessProcessChildObjects struct {
	Attribute      []*Attribute
	TabularSection []*TabularSection
	Form           []string
	Template       []string
	Command        []*Command
}

// CalculationRegister ...
type CalculationRegister struct {
	MDObjectBase
	Properties   *CalculationRegisterProperties
	ChildObjects *CalculationRegisterChildObjects
}

// CalculationRegisterProperties ...
type CalculationRegisterProperties struct {
	Name                     string
	Synonym                  *LocalStringType
	Comment                  string
	UseStandardCommands      bool
	DefaultListForm          MDObjectRef
	AuxiliaryListForm        MDObjectRef
	Periodicity              enums.CalculationRegisterPeriodicity
	ActionPeriod             bool
	BasePeriod               bool
	Schedule                 MDObjectRef
	ScheduleValue            MDObjectRef
	ScheduleDate             MDObjectRef
	ChartOfCalculationTypes  MDObjectRef
	IncludeHelpInContents    bool
	StandardAttributes       *StandardAttributes
	DataLockControlMode      enums.DefaultDataLockControlMode
	FullTextSearch           enums.FullTextSearchUsing
	ListPresentation         *LocalStringType
	ExtendedListPresentation *LocalStringType
	Explanation              *LocalStringType
}

// CalculationRegisterChildObjects ...
type CalculationRegisterChildObjects struct {
	Resource      []*Resource
	Attribute     []*Attribute
	Dimension     []*Dimension
	Recalculation []string
	Form          []string
	Template      []string
	Command       []*Command
}

// Catalog ...
type Catalog struct {
	MDObjectBase
	Properties   *CatalogProperties
	ChildObjects *CatalogChildObjects
}

// CatalogProperties ...
type CatalogProperties struct {
	Name                             string
	Synonym                          *LocalStringType
	Comment                          string
	Hierarchical                     bool
	HierarchyType                    enums.HierarchyType
	LimitLevelCount                  bool
	LevelCount                       Decimal
	FoldersOnTop                     bool
	UseStandardCommands              bool
	Owners                           *MDListType
	SubordinationUse                 enums.SubordinationUse
	CodeLength                       Decimal
	DescriptionLength                Decimal
	CodeType                         enums.CatalogCodeType
	CodeAllowedLength                enums.AllowedLength
	CodeSeries                       enums.CatalogCodesSeries
	CheckUnique                      bool
	Autonumbering                    bool
	DefaultPresentation              enums.CatalogMainPresentation
	StandardAttributes               *StandardAttributes
	Characteristics                  *Characteristics
	PredefinedDataUpdate             enums.PredefinedDataUpdate
	EditType                         enums.EditType
	QuickChoice                      bool
	ChoiceMode                       enums.ChoiceMode
	InputByString                    *FieldList
	SearchStringModeOnInputByString  enums.SearchStringModeOnInputByString
	FullTextSearchOnInputByString    enums.FullTextSearchOnInputByString
	ChoiceDataGetModeOnInputByString enums.ChoiceDataGetModeOnInputByString
	DefaultObjectForm                MDObjectRef
	DefaultFolderForm                MDObjectRef
	DefaultListForm                  MDObjectRef
	DefaultChoiceForm                MDObjectRef
	DefaultFolderChoiceForm          MDObjectRef
	AuxiliaryObjectForm              MDObjectRef
	AuxiliaryFolderForm              MDObjectRef
	AuxiliaryListForm                MDObjectRef
	AuxiliaryChoiceForm              MDObjectRef
	AuxiliaryFolderChoiceForm        MDObjectRef
	IncludeHelpInContents            bool
	BasedOn                          *MDListType
	DataLockFields                   *FieldList
	DataLockControlMode              enums.DefaultDataLockControlMode
	FullTextSearch                   enums.FullTextSearchUsing
	ObjectPresentation               *LocalStringType
	ExtendedObjectPresentation       *LocalStringType
	ListPresentation                 *LocalStringType
	ExtendedListPresentation         *LocalStringType
	Explanation                      *LocalStringType
	CreateOnInput                    enums.CreateOnInput
	ChoiceHistoryOnInput             enums.ChoiceHistoryOnInput
	DataHistory                      enums.DataHistoryUse
}

// CatalogChildObjects ...
type CatalogChildObjects struct {
	Attribute      []*Attribute
	TabularSection []*TabularSection
	Form           []string
	Template       []string
	Command        []*Command
}

// ChartOfAccounts ...
type ChartOfAccounts struct {
	MDObjectBase
	Properties   *ChartOfAccountsProperties
	ChildObjects *ChartOfAccountsChildObjects
}

// ChartOfAccountsProperties ...
type ChartOfAccountsProperties struct {
	Name                             string
	Synonym                          *LocalStringType
	Comment                          string
	UseStandardCommands              bool
	IncludeHelpInContents            bool
	BasedOn                          *MDListType
	ExtDimensionTypes                MDObjectRef
	MaxExtDimensionCount             Decimal
	CodeMask                         string
	CodeLength                       Decimal
	DescriptionLength                Decimal
	CodeSeries                       enums.CharOfAccountCodeSeries
	CheckUnique                      bool
	DefaultPresentation              enums.AccountMainPresentation
	StandardAttributes               *StandardAttributes
	Characteristics                  *Characteristics
	StandardTabularSections          *StandardTabularSections
	PredefinedDataUpdate             enums.PredefinedDataUpdate
	EditType                         enums.EditType
	QuickChoice                      bool
	ChoiceMode                       enums.ChoiceMode
	InputByString                    *FieldList
	SearchStringModeOnInputByString  enums.SearchStringModeOnInputByString
	FullTextSearchOnInputByString    enums.FullTextSearchOnInputByString
	ChoiceDataGetModeOnInputByString enums.ChoiceDataGetModeOnInputByString
	CreateOnInput                    enums.CreateOnInput
	ChoiceHistoryOnInput             enums.ChoiceHistoryOnInput
	DefaultObjectForm                MDObjectRef
	DefaultListForm                  MDObjectRef
	DefaultChoiceForm                MDObjectRef
	AuxiliaryObjectForm              MDObjectRef
	AuxiliaryListForm                MDObjectRef
	AuxiliaryChoiceForm              MDObjectRef
	AutoOrderByCode                  bool
	OrderLength                      Decimal
	DataLockFields                   *FieldList
	DataLockControlMode              enums.DefaultDataLockControlMode
	FullTextSearch                   enums.FullTextSearchUsing
	ObjectPresentation               *LocalStringType
	ExtendedObjectPresentation       *LocalStringType
	ListPresentation                 *LocalStringType
	ExtendedListPresentation         *LocalStringType
	Explanation                      *LocalStringType
}

// ChartOfAccountsChildObjects ...
type ChartOfAccountsChildObjects struct {
	Attribute                  []*Attribute
	TabularSection             []*TabularSection
	AccountingFlag             []*AccountingFlag
	ExtDimensionAccountingFlag []*ExtDimensionAccountingFlag
	Form                       []string
	Template                   []string
	Command                    []*Command
}

// ChartOfCalculationTypes ...
type ChartOfCalculationTypes struct {
	MDObjectBase
	Properties   *ChartOfCalculationTypesProperties
	ChildObjects *ChartOfCalculationTypesChildObjects
}

// ChartOfCalculationTypesProperties ...
type ChartOfCalculationTypesProperties struct {
	Name                             string
	Synonym                          *LocalStringType
	Comment                          string
	UseStandardCommands              bool
	CodeLength                       Decimal
	DescriptionLength                Decimal
	CodeType                         enums.ChartOfCalculationTypesCodeType
	CodeAllowedLength                enums.AllowedLength
	DefaultPresentation              enums.CalculationTypeMainPresentation
	EditType                         enums.EditType
	QuickChoice                      bool
	ChoiceMode                       enums.ChoiceMode
	InputByString                    *FieldList
	SearchStringModeOnInputByString  enums.SearchStringModeOnInputByString
	FullTextSearchOnInputByString    enums.FullTextSearchOnInputByString
	ChoiceDataGetModeOnInputByString enums.ChoiceDataGetModeOnInputByString
	CreateOnInput                    enums.CreateOnInput
	ChoiceHistoryOnInput             enums.ChoiceHistoryOnInput
	DefaultObjectForm                MDObjectRef
	DefaultListForm                  MDObjectRef
	DefaultChoiceForm                MDObjectRef
	AuxiliaryObjectForm              MDObjectRef
	AuxiliaryListForm                MDObjectRef
	AuxiliaryChoiceForm              MDObjectRef
	BasedOn                          *MDListType
	DependenceOnCalculationTypes     enums.ChartOfCalculationTypesBaseUse
	BaseCalculationTypes             *MDListType
	ActionPeriodUse                  bool
	StandardAttributes               *StandardAttributes
	Characteristics                  *Characteristics
	StandardTabularSections          *StandardTabularSections
	PredefinedDataUpdate             enums.PredefinedDataUpdate
	IncludeHelpInContents            bool
	DataLockFields                   *FieldList
	DataLockControlMode              enums.DefaultDataLockControlMode
	FullTextSearch                   enums.FullTextSearchUsing
	ObjectPresentation               *LocalStringType
	ExtendedObjectPresentation       *LocalStringType
	ListPresentation                 *LocalStringType
	ExtendedListPresentation         *LocalStringType
	Explanation                      *LocalStringType
}

// ChartOfCalculationTypesChildObjects ...
type ChartOfCalculationTypesChildObjects struct {
	Attribute      []*Attribute
	TabularSection []*TabularSection
	Form           []string
	Template       []string
	Command        []*Command
}

// ChartOfCharacteristicTypes ...
type ChartOfCharacteristicTypes struct {
	MDObjectBase
	Properties   *ChartOfCharacteristicTypesProperties
	ChildObjects *ChartOfCharacteristicTypesChildObjects
}

// ChartOfCharacteristicTypesProperties ...
type ChartOfCharacteristicTypesProperties struct {
	Name                             string
	Synonym                          *LocalStringType
	Comment                          string
	UseStandardCommands              bool
	IncludeHelpInContents            bool
	CharacteristicExtValues          MDObjectRef
	Type                             *TypeDescription
	Hierarchical                     bool
	FoldersOnTop                     bool
	CodeLength                       Decimal
	CodeAllowedLength                enums.AllowedLength
	DescriptionLength                Decimal
	CodeSeries                       enums.CharacteristicKindCodesSeries
	CheckUnique                      bool
	Autonumbering                    bool
	DefaultPresentation              enums.CharacteristicTypeMainPresentation
	StandardAttributes               *StandardAttributes
	Characteristics                  *Characteristics
	PredefinedDataUpdate             enums.PredefinedDataUpdate
	EditType                         enums.EditType
	QuickChoice                      bool
	ChoiceMode                       enums.ChoiceMode
	InputByString                    *FieldList
	CreateOnInput                    enums.CreateOnInput
	SearchStringModeOnInputByString  enums.SearchStringModeOnInputByString
	ChoiceDataGetModeOnInputByString enums.ChoiceDataGetModeOnInputByString
	FullTextSearchOnInputByString    enums.FullTextSearchOnInputByString
	ChoiceHistoryOnInput             enums.ChoiceHistoryOnInput
	DefaultObjectForm                MDObjectRef
	DefaultFolderForm                MDObjectRef
	DefaultListForm                  MDObjectRef
	DefaultChoiceForm                MDObjectRef
	DefaultFolderChoiceForm          MDObjectRef
	AuxiliaryObjectForm              MDObjectRef
	AuxiliaryFolderForm              MDObjectRef
	AuxiliaryListForm                MDObjectRef
	AuxiliaryChoiceForm              MDObjectRef
	AuxiliaryFolderChoiceForm        MDObjectRef
	BasedOn                          MDListType
	DataLockFields                   *FieldList
	DataLockControlMode              enums.DefaultDataLockControlMode
	FullTextSearch                   enums.FullTextSearchUsing
	ObjectPresentation               *LocalStringType
	ExtendedObjectPresentation       *LocalStringType
	ListPresentation                 *LocalStringType
	ExtendedListPresentation         *LocalStringType
	Explanation                      *LocalStringType
}

// ChartOfCharacteristicTypesChildObjects ...
type ChartOfCharacteristicTypesChildObjects struct {
	Attribute      []*Attribute
	TabularSection []*TabularSection
	Form           []string
	Template       []string
	Command        []*Command
}

// CommandGroup ...
type CommandGroup struct {
	MDObjectBase
	Properties *CommandGroupProperties
	// ChildObjects CommandGroupChildObjects
}

// CommandGroupProperties ...
type CommandGroupProperties struct {
	Name           string
	Synonym        *LocalStringType
	Comment        string
	Representation enums.ButtonRepresentation
	ToolTip        *LocalStringType
	Category       enums.CommandGroupCategory
	//Picture  ;
}

// CommonAttribute ...
type CommonAttribute struct {
	MDObjectBase
	Properties *CommonAttributeProperties
	// ChildObjects CommonAttributeChildObjects
}

// CommonAttributeProperties ...
type CommonAttributeProperties struct {
	Name                              string
	Synonym                           *LocalStringType
	Comment                           string
	Type                              *TypeDescription
	PasswordMode                      bool
	Format                            *LocalStringType
	EditFormat                        *LocalStringType
	ToolTip                           *LocalStringType
	MarkNegatives                     bool
	Mask                              string
	MultiLine                         bool
	ExtendedEdit                      bool
	FillFromFillingValue              bool
	FillValue                         *FillValue
	FillChecking                      enums.FillChecking
	ChoiceFoldersAndItems             enums.FoldersAndItemsUse
	ChoiceParameterLinks              *ChoiceParameterLinks
	QuickChoice                       enums.UseQuickChoice
	CreateOnInput                     enums.CreateOnInput
	ChoiceForm                        MDObjectRef
	LinkByType                        *TypeLink
	ChoiceHistoryOnInput              enums.ChoiceHistoryOnInput
	AutoUse                           enums.CommonAttributeAutoUse
	DataSeparation                    enums.CommonAttributeDataSeparation
	SeparatedDataUse                  enums.CommonAttributeSeparatedDataUse
	DataSeparationValue               MDObjectRef
	DataSeparationUse                 MDObjectRef
	ConditionalSeparation             MDObjectRef
	UsersSeparation                   enums.CommonAttributeUsersSeparation
	AuthenticationSeparation          enums.CommonAttributeAuthenticationSeparation
	ConfigurationExtensionsSeparation enums.CommonAttributeConfigurationExtensionsSeparation
	Indexing                          enums.Indexing
	FullTextSearch                    enums.FullTextSearchUsing
	DataHistory                       enums.DataHistoryUse
	//MinValue  ;
	//MaxValue  ;
	//ChoiceParameters  ;
	//Content  CommonAttributeContent();
}

// CommonCommand ...
type CommonCommand struct {
	MDObjectBase
	Properties *CommonCommandProperties
	// ChildObjects CommonCommandChildObjects
}

// CommonCommandProperties ...
type CommonCommandProperties struct {
	Name                  string
	Synonym               *LocalStringType
	Comment               string
	Representation        enums.ButtonRepresentation
	ToolTip               *LocalStringType
	IncludeHelpInContents bool
	CommandParameterType  *TypeDescription
	ParameterUseMode      enums.CommandParameterUseMode
	ModifiesData          bool
	//Group  IncludeInCommandCategoriesType;
	//Picture  ;
	//Shortcut  ;
}

// CommonForm ...
type CommonForm struct {
	MDObjectBase
	Properties *CommonFormProperties
	// ChildObjects CommonFormChildObjects
}

// CommonFormProperties ...
type CommonFormProperties struct {
	Name                  string
	Synonym               *LocalStringType
	Comment               string
	FormType              enums.FormType
	IncludeHelpInContents bool
	UseStandardCommands   bool
	ExtendedPresentation  *LocalStringType
	Explanation           *LocalStringType
	//UsePurposes  "FixedArray";
}

// CommonModule ...
type CommonModule struct {
	MDObjectBase
	Properties *CommonModuleProperties
	// ChildObjects CommonModuleChildObjects
}

// CommonModuleProperties ...
type CommonModuleProperties struct {
	Name                      string
	Synonym                   *LocalStringType
	Comment                   string
	Global                    bool
	ClientManagedApplication  bool
	Server                    bool
	ExternalConnection        bool
	ClientOrdinaryApplication bool
	Client                    bool
	ServerCall                bool
	Privileged                bool
	ReturnValuesReuse         enums.ReturnValuesReuse
}

// CommonPicture ...
type CommonPicture struct {
	MDObjectBase
	Properties *CommonPictureProperties
	// ChildObjects CommonPictureChildObjects
}

// CommonPictureProperties ...
type CommonPictureProperties struct {
	Name    string
	Synonym *LocalStringType
	Comment string
}

// CommonTemplate ...
type CommonTemplate struct {
	MDObjectBase
	Properties *CommonTemplateProperties
	// ChildObjects CommonTemplateChildObjects
}

// CommonTemplateProperties ...
type CommonTemplateProperties struct {
	Name         string
	Synonym      *LocalStringType
	Comment      string
	TemplateType enums.TemplateType
}

// Configuration ...
type Configuration struct {
	MDObjectBase
	Properties   *ConfigurationProperties
	ChildObjects *ConfigurationChildObjects
}

// ConfigurationProperties ...
type ConfigurationProperties struct {
	Name                                            string
	Synonym                                         *LocalStringType
	Comment                                         string
	NamePrefix                                      string
	ConfigurationExtensionCompatibilityMode         enums.CompatibilityMode
	DefaultRunMode                                  enums.ClientRunMode
	ScriptVariant                                   enums.ScriptVariant
	DefaultRoles                                    *MDListType
	Vendor                                          string
	Version                                         string
	UpdateCatalogAddress                            string
	IncludeHelpInContents                           bool
	UseManagedFormInOrdinaryApplication             bool
	UseOrdinaryFormInManagedApplication             bool
	AdditionalFullTextSearchDictionaries            *MDListType
	CommonSettingsStorage                           MDObjectRef
	ReportsUserSettingsStorage                      MDObjectRef
	ReportsVariantsStorage                          MDObjectRef
	FormDataSettingsStorage                         MDObjectRef
	DynamicListsUserSettingsStorage                 MDObjectRef
	Content                                         *MDListType
	DefaultReportForm                               MDObjectRef
	DefaultReportVariantForm                        MDObjectRef
	DefaultReportSettingsForm                       MDObjectRef
	DefaultDynamicListSettingsForm                  MDObjectRef
	DefaultSearchForm                               MDObjectRef
	MainClientApplicationWindowMode                 enums.MainClientApplicationWindowMode
	DefaultInterface                                MDObjectRef
	DefaultStyle                                    MDObjectRef
	DefaultLanguage                                 MDObjectRef
	BriefInformation                                *LocalStringType
	DetailedInformation                             *LocalStringType
	Copyright                                       *LocalStringType
	VendorInformationAddress                        *LocalStringType
	ConfigurationInformationAddress                 *LocalStringType
	DataLockControlMode                             enums.DefaultDataLockControlMode
	ObjectAutonumerationMode                        enums.ObjectAutonumerationMode
	ModalityUseMode                                 enums.ModalityUseMode
	SynchronousPlatformExtensionAndAddInCallUseMode enums.SynchronousPlatformExtensionAndAddInCallUseMode
	InterfaceCompatibilityMode                      enums.InterfaceCompatibilityMode
	CompatibilityMode                               enums.CompatibilityMode
	DefaultConstantsForm                            MDObjectRef
	//UsePurposes  "FixedArray";
	//RequiredMobileApplicationPermissions  "FixedMap";
}

// ConfigurationChildObjects ...
type ConfigurationChildObjects struct {
	AccountingRegister         []string
	AccumulationRegister       []string
	BusinessProcess            []string
	CalculationRegister        []string
	Catalog                    []string
	ChartOfAccounts            []string
	ChartOfCalculationTypes    []string
	ChartOfCharacteristicTypes []string
	CommandGroup               []string
	CommonAttribute            []string
	CommonCommand              []string
	CommonForm                 []string
	CommonModule               []string
	CommonPicture              []string
	CommonTemplate             []string
	Constant                   []string
	DataProcessor              []string
	DefinedType                []string
	Document                   []string
	DocumentJournal            []string
	DocumentNumerator          []string
	Enum                       []string
	EventSubscription          []string
	ExchangePlan               []string
	ExternalDataSource         []string
	FilterCriterion            []string
	FunctionalOption           []string
	FunctionalOptionsParameter []string
	HTTPService                []string
	InformationRegister        []string
	Interface                  []string
	Language                   []string
	Report                     []string
	Role                       []string
	ScheduledJob               []string
	Sequence                   []string
	SessionParameter           []string
	SettingsStorage            []string
	Style                      []string
	StyleItem                  []string
	Subsystem                  []string
	Task                       []string
	WebService                 []string
	WSReference                []string
	XDTOPackage                []string
}

// Constant ...
type Constant struct {
	MDObjectBase
	Properties *ConstantProperties
	// ChildObjects ConstantChildObjects
}

// ConstantProperties ...
type ConstantProperties struct {
	Name                  string
	Synonym               *LocalStringType
	Comment               string
	Type                  *TypeDescription
	UseStandardCommands   bool
	DefaultForm           MDObjectRef
	ExtendedPresentation  *LocalStringType
	Explanation           *LocalStringType
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
	ChoiceForm            MDObjectRef
	LinkByType            *TypeLink
	ChoiceHistoryOnInput  enums.ChoiceHistoryOnInput
	DataLockControlMode   enums.DefaultDataLockControlMode
	//MinValue  ;
	//MaxValue  ;
	//ChoiceParameters  ;
}

// DataProcessor ...
type DataProcessor struct {
	MDObjectBase
	Properties   *DataProcessorProperties
	ChildObjects *DataProcessorChildObjects
}

// DataProcessorProperties ...
type DataProcessorProperties struct {
	Name                  string
	Synonym               *LocalStringType
	Comment               string
	UseStandardCommands   bool
	DefaultForm           MDObjectRef
	AuxiliaryForm         MDObjectRef
	IncludeHelpInContents bool
	ExtendedPresentation  *LocalStringType
	Explanation           *LocalStringType
}

// DataProcessorChildObjects ...
type DataProcessorChildObjects struct {
	Attribute      []*Attribute
	TabularSection []*TabularSection
	Form           []string
	Template       []string
	Command        []*Command
}

// Document ...
type Document struct {
	MDObjectBase
	Properties   *DocumentProperties
	ChildObjects *DocumentChildObjects
}

// DocumentProperties ...
type DocumentProperties struct {
	Name                             string
	Synonym                          *LocalStringType
	Comment                          string
	UseStandardCommands              bool
	Numerator                        MDObjectRef
	NumberType                       enums.DocumentNumberType
	NumberLength                     Decimal
	NumberAllowedLength              enums.AllowedLength
	NumberPeriodicity                enums.DocumentNumberPeriodicity
	CheckUnique                      bool
	Autonumbering                    bool
	StandardAttributes               *StandardAttributes
	Characteristics                  *Characteristics
	BasedOn                          *MDListType
	InputByString                    *FieldList
	CreateOnInput                    enums.CreateOnInput
	SearchStringModeOnInputByString  enums.SearchStringModeOnInputByString
	FullTextSearchOnInputByString    enums.FullTextSearchOnInputByString
	ChoiceDataGetModeOnInputByString enums.ChoiceDataGetModeOnInputByString
	DefaultObjectForm                MDObjectRef
	DefaultListForm                  MDObjectRef
	DefaultChoiceForm                MDObjectRef
	AuxiliaryObjectForm              MDObjectRef
	AuxiliaryListForm                MDObjectRef
	AuxiliaryChoiceForm              MDObjectRef
	Posting                          enums.Posting
	RealTimePosting                  enums.RealTimePosting
	RegisterRecordsDeletion          enums.RegisterRecordsDeletion
	RegisterRecordsWritingOnPost     enums.RegisterRecordsWritingOnPost
	SequenceFilling                  enums.SequenceFilling
	RegisterRecords                  *MDListType
	PostInPrivilegedMode             bool
	UnpostInPrivilegedMode           bool
	IncludeHelpInContents            bool
	DataLockFields                   *FieldList
	DataLockControlMode              enums.DefaultDataLockControlMode
	FullTextSearch                   enums.FullTextSearchUsing
	ObjectPresentation               *LocalStringType
	ExtendedObjectPresentation       *LocalStringType
	ListPresentation                 *LocalStringType
	ExtendedListPresentation         *LocalStringType
	Explanation                      *LocalStringType
	ChoiceHistoryOnInput             enums.ChoiceHistoryOnInput
	DataHistory                      enums.DataHistoryUse
}

// DocumentChildObjects ...
type DocumentChildObjects struct {
	Attribute      []*Attribute
	Form           []string
	TabularSection []*TabularSection
	Template       []string
	Command        []*Command
}

// DocumentJournal ...
type DocumentJournal struct {
	MDObjectBase
	Properties   *DocumentJournalProperties
	ChildObjects *DocumentJournalChildObjects
}

// DocumentJournalProperties ...
type DocumentJournalProperties struct {
	Name                     string
	Synonym                  *LocalStringType
	Comment                  string
	DefaultForm              MDObjectRef
	AuxiliaryForm            MDObjectRef
	UseStandardCommands      bool
	RegisteredDocuments      *MDListType
	IncludeHelpInContents    bool
	StandardAttributes       *StandardAttributes
	ListPresentation         *LocalStringType
	ExtendedListPresentation *LocalStringType
	Explanation              *LocalStringType
}

// DocumentJournalChildObjects ...
type DocumentJournalChildObjects struct {
	Column   []*Column
	Form     []string
	Template []string
	Command  []*Command
}

// DocumentNumerator ...
type DocumentNumerator struct {
	MDObjectBase
	Properties *DocumentNumeratorProperties
	// ChildObjects DocumentNumeratorChildObjects
}

// DocumentNumeratorProperties ...
type DocumentNumeratorProperties struct {
	Name                string
	Synonym             *LocalStringType
	Comment             string
	NumberType          enums.DocumentNumberType
	NumberLength        Decimal
	NumberAllowedLength enums.AllowedLength
	NumberPeriodicity   enums.DocumentNumberPeriodicity
	CheckUnique         bool
}

// Enum ...
type Enum struct {
	MDObjectBase
	Properties   *EnumProperties
	ChildObjects *EnumChildObjects
}

// EnumProperties ...
type EnumProperties struct {
	Name                     string
	Synonym                  *LocalStringType
	Comment                  string
	UseStandardCommands      bool
	StandardAttributes       *StandardAttributes
	Characteristics          *Characteristics
	QuickChoice              bool
	ChoiceMode               enums.ChoiceMode
	DefaultListForm          MDObjectRef
	DefaultChoiceForm        MDObjectRef
	AuxiliaryListForm        MDObjectRef
	AuxiliaryChoiceForm      MDObjectRef
	ListPresentation         *LocalStringType
	ExtendedListPresentation *LocalStringType
	Explanation              *LocalStringType
	ChoiceHistoryOnInput     enums.ChoiceHistoryOnInput
}

// EnumChildObjects ...
type EnumChildObjects struct {
	EnumValue []*EnumValue
	Form      []string
	Template  []string
	Command   []*Command
}

// EventSubscription ...
type EventSubscription struct {
	MDObjectBase
	Properties *EventSubscriptionProperties
	// ChildObjects EventSubscriptionChildObjects
}

// EventSubscriptionProperties ...
type EventSubscriptionProperties struct {
	Name    string
	Synonym *LocalStringType
	Comment string
	Source  *TypeDescription
	Handler MDMethodRef
	//Event  AliasedStringType
}

// ExchangePlan ...
type ExchangePlan struct {
	MDObjectBase
	Properties   *ExchangePlanProperties
	ChildObjects *ExchangePlanChildObjects
}

// ExchangePlanProperties ...
type ExchangePlanProperties struct {
	Name                             string
	Synonym                          *LocalStringType
	Comment                          string
	UseStandardCommands              bool
	CodeLength                       Decimal
	CodeAllowedLength                enums.AllowedLength
	DescriptionLength                Decimal
	DefaultPresentation              enums.DataExchangeMainPresentation
	EditType                         enums.EditType
	QuickChoice                      bool
	ChoiceMode                       enums.ChoiceMode
	InputByString                    *FieldList
	SearchStringModeOnInputByString  enums.SearchStringModeOnInputByString
	FullTextSearchOnInputByString    enums.FullTextSearchOnInputByString
	ChoiceDataGetModeOnInputByString enums.ChoiceDataGetModeOnInputByString
	DefaultObjectForm                MDObjectRef
	DefaultListForm                  MDObjectRef
	DefaultChoiceForm                MDObjectRef
	AuxiliaryObjectForm              MDObjectRef
	AuxiliaryListForm                MDObjectRef
	AuxiliaryChoiceForm              MDObjectRef
	StandardAttributes               *StandardAttributes
	Characteristics                  *Characteristics
	BasedOn                          *MDListType
	DistributedInfoBase              bool
	CreateOnInput                    enums.CreateOnInput
	ChoiceHistoryOnInput             enums.ChoiceHistoryOnInput
	IncludeHelpInContents            bool
	DataLockFields                   *FieldList
	DataLockControlMode              enums.DefaultDataLockControlMode
	FullTextSearch                   enums.FullTextSearchUsing
	ObjectPresentation               *LocalStringType
	ExtendedObjectPresentation       *LocalStringType
	ListPresentation                 *LocalStringType
	ExtendedListPresentation         *LocalStringType
	Explanation                      *LocalStringType
}

// ExchangePlanChildObjects ...
type ExchangePlanChildObjects struct {
	Attribute      []*Attribute
	TabularSection []*TabularSection
	Form           []string
	Template       []string
	Command        []*Command
}

// FilterCriterion ...
type FilterCriterion struct {
	MDObjectBase
	Properties   *FilterCriterionProperties
	ChildObjects *FilterCriterionChildObjects
}

// FilterCriterionProperties ...
type FilterCriterionProperties struct {
	Name                     string
	Synonym                  *LocalStringType
	Comment                  string
	Type                     *TypeDescription
	UseStandardCommands      bool
	Content                  *MDListType
	DefaultForm              MDObjectRef
	AuxiliaryForm            MDObjectRef
	ListPresentation         *LocalStringType
	ExtendedListPresentation *LocalStringType
	Explanation              *LocalStringType
}

// FilterCriterionChildObjects ...
type FilterCriterionChildObjects struct {
	Form    []string
	Command []*Command
}

// FunctionalOption ...
type FunctionalOption struct {
	MDObjectBase
	Properties *FunctionalOptionProperties
	// ChildObjects FunctionalOptionChildObjects
}

// FunctionalOptionProperties ...
type FunctionalOptionProperties struct {
	Name              string
	Synonym           *LocalStringType
	Comment           string
	Location          MDObjectRef
	PrivilegedGetMode bool
	//Content  FuncOptionContentType;
}

// FunctionalOptionsParameter ...
type FunctionalOptionsParameter struct {
	MDObjectBase
	Properties *FunctionalOptionsParameterProperties
	// ChildObjects FunctionalOptionsParameterChildObjects
}

// FunctionalOptionsParameterProperties ...
type FunctionalOptionsParameterProperties struct {
	Name    string
	Synonym *LocalStringType
	Comment string
	Use     *MDListType
}

// HTTPService ...
type HTTPService struct {
	MDObjectBase
	Properties   *HTTPServiceProperties
	ChildObjects *HTTPServiceChildObjects
}

// HTTPServiceProperties ...
type HTTPServiceProperties struct {
	Name          string
	Synonym       *LocalStringType
	Comment       string
	RootURL       string
	ReuseSessions enums.SessionReuseMode
	SessionMaxAge Decimal
}

// HTTPServiceChildObjects ...
type HTTPServiceChildObjects struct {
	// URLTemplate []string
}

// InformationRegister ...
type InformationRegister struct {
	MDObjectBase
	Properties   *InformationRegisterProperties
	ChildObjects *InformationRegisterChildObjects
}

// InformationRegisterProperties ...
type InformationRegisterProperties struct {
	Name                           string
	Synonym                        *LocalStringType
	Comment                        string
	UseStandardCommands            bool
	EditType                       enums.EditType
	DefaultRecordForm              MDObjectRef
	DefaultListForm                MDObjectRef
	AuxiliaryRecordForm            MDObjectRef
	AuxiliaryListForm              MDObjectRef
	StandardAttributes             *StandardAttributes
	InformationRegisterPeriodicity enums.InformationRegisterPeriodicity
	WriteMode                      enums.RegisterWriteMode
	MainFilterOnPeriod             bool
	IncludeHelpInContents          bool
	DataLockControlMode            enums.DefaultDataLockControlMode
	FullTextSearch                 enums.FullTextSearchUsing
	EnableTotalsSliceFirst         bool
	EnableTotalsSliceLast          bool
	RecordPresentation             *LocalStringType
	ExtendedRecordPresentation     *LocalStringType
	ListPresentation               *LocalStringType
	ExtendedListPresentation       *LocalStringType
	Explanation                    *LocalStringType
	DataHistory                    enums.DataHistoryUse
}

// InformationRegisterChildObjects ...
type InformationRegisterChildObjects struct {
	Resource  []*Resource
	Attribute []*Attribute
	Dimension []*Dimension
	Form      []string
	Template  []string
	Command   []*Command
}

// Language ...
type Language struct {
	MDObjectBase
	Properties *LanguageProperties
}

// LanguageProperties ...
type LanguageProperties struct {
	Name         string
	Synonym      *LocalStringType
	Comment      string
	LanguageCode string
}

// Report ...
type Report struct {
	MDObjectBase
	Properties   *ReportProperties
	ChildObjects *ReportChildObjects
}

// ReportProperties ...
type ReportProperties struct {
	Name                      string
	Synonym                   *LocalStringType
	Comment                   string
	UseStandardCommands       bool
	DefaultForm               MDObjectRef
	AuxiliaryForm             MDObjectRef
	MainDataCompositionSchema MDObjectRef
	DefaultSettingsForm       MDObjectRef
	AuxiliarySettingsForm     MDObjectRef
	DefaultVariantForm        MDObjectRef
	VariantsStorage           MDObjectRef
	SettingsStorage           MDObjectRef
	IncludeHelpInContents     bool
	ExtendedPresentation      *LocalStringType
	Explanation               *LocalStringType
}

// ReportChildObjects ...
type ReportChildObjects struct {
	Attribute      []*Attribute
	TabularSection []*TabularSection
	Form           []string
	Template       []string
	Command        []*Command
}

// Role ...
type Role struct {
	MDObjectBase
	Properties *RoleProperties
}

// RoleProperties ...
type RoleProperties struct {
	Name    string
	Synonym *LocalStringType
	Comment string
}

// ScheduledJob ...
type ScheduledJob struct {
	MDObjectBase
	Properties *ScheduledJobProperties
}

// ScheduledJobProperties ...
type ScheduledJobProperties struct {
	Name                     string
	Synonym                  *LocalStringType
	Comment                  string
	MethodName               MDMethodRef
	Description              string
	Key                      string
	Use                      bool
	Predefined               bool
	RestartCountOnFailure    Decimal
	RestartIntervalOnFailure Decimal
}

// Sequence ...
type Sequence struct {
	MDObjectBase
	Properties   *SequenceProperties
	ChildObjects *SequenceChildObjects
}

// SequenceProperties ...
type SequenceProperties struct {
	Name                  string
	Synonym               *LocalStringType
	Comment               string
	MoveBoundaryOnPosting enums.MoveBoundaryOnPosting
	Documents             *MDListType
	RegisterRecords       *MDListType
	DataLockControlMode   enums.DefaultDataLockControlMode
}

// SequenceChildObjects ...
type SequenceChildObjects struct {
	Dimension []*Dimension
}

// SessionParameter ...
type SessionParameter struct {
	MDObjectBase
	Properties *SessionParameterProperties
}

// SessionParameterProperties ...
type SessionParameterProperties struct {
	Name    string
	Synonym *LocalStringType
	Comment string
	Type    *TypeDescription
}

// SettingsStorage ...
type SettingsStorage struct {
	MDObjectBase
	Properties   *SettingsStorageProperties
	ChildObjects *SettingsStorageChildObjects
}

// SettingsStorageProperties ...
type SettingsStorageProperties struct {
	Name              string
	Synonym           *LocalStringType
	Comment           string
	DefaultSaveForm   MDObjectRef
	DefaultLoadForm   MDObjectRef
	AuxiliarySaveForm MDObjectRef
	AuxiliaryLoadForm MDObjectRef
}

// SettingsStorageChildObjects ...
type SettingsStorageChildObjects struct {
	Form     []string
	Template []string
}

// Subsystem ...
type Subsystem struct {
	MDObjectBase
	Properties   *SubsystemProperties
	ChildObjects *SubsystemChildObjects
}

// SubsystemProperties ...
type SubsystemProperties struct {
	Name                      string
	Synonym                   *LocalStringType
	Comment                   string
	IncludeHelpInContents     bool
	IncludeInCommandInterface bool
	Explanation               *LocalStringType
	Content                   *MDListType
	//Picture  ;
}

// SubsystemChildObjects ...
type SubsystemChildObjects struct {
	Subsystem []string
}

// Task ...
type Task struct {
	MDObjectBase
	Properties   *TaskProperties
	ChildObjects *TaskChildObjects
}

// TaskProperties ...
type TaskProperties struct {
	Name                             string
	Synonym                          *LocalStringType
	Comment                          string
	UseStandardCommands              bool
	NumberType                       enums.TaskNumberType
	NumberLength                     Decimal
	NumberAllowedLength              enums.AllowedLength
	CheckUnique                      bool
	Autonumbering                    bool
	TaskNumberAutoPrefix             enums.TaskNumberAutoPrefix
	DescriptionLength                Decimal
	Addressing                       MDObjectRef
	MainAddressingAttribute          MDObjectRef
	CurrentPerformer                 MDObjectRef
	BasedOn                          *MDListType
	StandardAttributes               *StandardAttributes
	Characteristics                  *Characteristics
	DefaultPresentation              enums.TaskMainPresentation
	EditType                         enums.EditType
	InputByString                    *FieldList
	SearchStringModeOnInputByString  enums.SearchStringModeOnInputByString
	FullTextSearchOnInputByString    enums.FullTextSearchOnInputByString
	ChoiceDataGetModeOnInputByString enums.ChoiceDataGetModeOnInputByString
	CreateOnInput                    enums.CreateOnInput
	DefaultObjectForm                MDObjectRef
	DefaultListForm                  MDObjectRef
	DefaultChoiceForm                MDObjectRef
	AuxiliaryObjectForm              MDObjectRef
	AuxiliaryListForm                MDObjectRef
	AuxiliaryChoiceForm              MDObjectRef
	ChoiceHistoryOnInput             enums.ChoiceHistoryOnInput
	IncludeHelpInContents            bool
	DataLockFields                   *FieldList
	DataLockControlMode              enums.DefaultDataLockControlMode
	FullTextSearch                   enums.FullTextSearchUsing
	ObjectPresentation               *LocalStringType
	ExtendedObjectPresentation       *LocalStringType
	ListPresentation                 *LocalStringType
	ExtendedListPresentation         *LocalStringType
	Explanation                      *LocalStringType
}

// TaskChildObjects ...
type TaskChildObjects struct {
	Attribute           []*Attribute
	TabularSection      []*TabularSection
	Form                []string
	Template            []string
	AddressingAttribute []*AddressingAttribute
	Command             []*Command
}

// WebService ...
type WebService struct {
	MDObjectBase
	Properties   *WebServiceProperties
	ChildObjects *WebServiceChildObjects
}

// WebServiceProperties ...
type WebServiceProperties struct {
	Name               string
	Synonym            *LocalStringType
	Comment            string
	Namespace          string
	DescriptorFileName string
	ReuseSessions      enums.SessionReuseMode
	SessionMaxAge      Decimal
	//XDTOPackages  ValueList;
}

// WebServiceChildObjects ...
type WebServiceChildObjects struct {
	Operation []*Operation
}

// Operation ...
type Operation struct {
	MDObjectBase
	Properties   *OperationProperties
	ChildObjects *OperationChildObjects
}

// OperationProperties ...
type OperationProperties struct {
	Name                   string
	Synonym                *LocalStringType
	Comment                string
	XDTOReturningValueType QName
	Nillable               bool
	Transactioned          bool
	ProcedureName          string
}

// OperationChildObjects ...
type OperationChildObjects struct {
	Parameter []*Parameter
}

// Parameter ...
type Parameter struct {
	MDObjectBase
	Properties *ParameterProperties
}

// ParameterProperties ...
type ParameterProperties struct {
	Name              string
	Synonym           *LocalStringType
	Comment           string
	XDTOValueType     QName
	Nillable          bool
	TransferDirection enums.TransferDirection
}

// WSReference ...
type WSReference struct {
	MDObjectBase
	Properties *WSReferenceProperties
}

// WSReferenceProperties ...
type WSReferenceProperties struct {
	Name        string
	Synonym     *LocalStringType
	Comment     string
	LocationURL string
}

// XDTOPackage ...
type XDTOPackage struct {
	MDObjectBase
	Properties *XDTOPackageProperties
}

// XDTOPackageProperties ...
type XDTOPackageProperties struct {
	Name      string
	Synonym   *LocalStringType
	Comment   string
	Namespace string
}
