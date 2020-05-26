package conf

import (
	"encoding/xml"
)

//import "github.com/tsukanov-as/r2d2/conf/enums"

// FormEvent ...
// type FormEvent struct {
// 	Name  string `xml:"name,attr"`
// 	Value string `xml:",chardata"`
// }

// #Region Form

type (
	// DateTime ...
	DateTime = string
	// FormItemRef ...
	FormItemRef = string
	// LFEDataPath ...
	LFEDataPath  = string
	base64Binary = string
)

// FormVisualEntity ...
type FormVisualEntity struct {
	PredefinedChildItems
	Events     *FormItemEvents
	ChildItems *ChildItems
}

// PredefinedChildItems ...
type PredefinedChildItems struct {
	ContextMenu           *ContextMenu
	AutoCommandBar        *AutoCommandBar
	ExtendedTooltip       *LabelDecoration
	SearchStringAddition  *SearchStringAddition
	ViewStatusAddition    *ViewStatusAddition
	SearchControlAddition *SearchControlAddition
}

// ManagedForm ...
type ManagedForm struct {
	FormVisualEntity
	Title                       *LocalStringType
	Width                       Decimal
	Height                      Decimal
	WindowOpeningMode           string //Enums.FormWindowOpeningMode;
	EnterKeyBehavior            string //Enums.FormEnterKeyBehavior;
	AutoSaveDataInSettings      string //Enums.AutoSaveFormDataInSettings;
	SaveDataInSettings          string //Enums.SaveFormDataInSettings;
	SettingsStorage             MDObjectRef
	AutoTitle                   bool
	AutoURL                     bool
	Group                       string //Enums.FormChildrenGroup;
	ChildrenAlign               string //Enums.FormChildrenAlign;
	HorizontalSpacing           string //Enums.FormItemSpacing;
	VerticalSpacing             string //Enums.FormItemSpacing;
	HorizontalAlign             string //Enums.ItemHorizontalAlignment;
	VerticalAlign               string //Enums.ItemVerticalAlignment;
	ChildItemsWidth             string //Enums.FormChildrenWidth;
	AutoFillCheck               bool
	Customizable                bool
	Enabled                     bool
	ReadOnly                    bool
	CommandBarLocation          string //Enums.FormElementCommandBarLocation;
	VerticalScroll              string //Enums.LogFormScrollMode
	ScalingMode                 string //Enums.FormBaseFontVariant;
	Scale                       Decimal
	ConversationsRepresentation string //Enums.LogFormShowConversations;
	CommandSet                  *CommandsContent
	ShowTitle                   bool
	ShowCloseButton             bool
	UseForFoldersAndItems       string //Enums.FoldersAndItemsUse;
	GroupList                   FormItemRef
	AutoTime                    string //Enums.AutoTimeMode;
	UsePostingMode              string //Enums.PostingModeUse;
	RepostOnWrite               bool
	ReportResult                LFEDataPath
	DetailsData                 LFEDataPath
	ReportFormType              string //Enums.ReportFormType;
	VariantAppearance           LFEDataPath
	AutoShowState               string //Enums.AutoShowStateMode;
	CustomSettingsFolder        FormItemRef
	Attributes                  *FormAttributes
	Commands                    *FormCommands
	Parameters                  *FormParameters
	CommandInterface            *FormCommandInterface
	//BaseForm Form
}

// FormItemBase ...
type FormItemBase struct {
	FormVisualEntity
	id   Decimal
	name string
}

// GroupBase ...
type GroupBase struct {
	FormItemBase
	Visible               bool
	UserVisible           *AdjustableBoolean
	Enabled               bool
	ReadOnly              bool
	EnableContentChange   bool
	Title                 *LocalStringType
	TitleTextColor        string //Color
	TitleFont             Font
	ToolTip               *LocalStringType
	ToolTipRepresentation string //Enums.TooltipRepresentation;
	Shortcut              string
	Width                 Decimal
	Height                Decimal
	HorizontalStretch     string //Enums.BWAValue;
	VerticalStretch       string //Enums.BWAValue;
	GroupHorizontalAlign  string //Enums.ItemHorizontalAlignment;
	GroupVerticalAlign    string //Enums.ItemVerticalAlignment;
}

// Decoration ...
type Decoration struct {
	FormItemBase
	Visible               bool
	UserVisible           *AdjustableBoolean
	Enabled               bool
	Width                 Decimal
	AutoMaxWidth          bool
	MaxWidth              Decimal
	MinWidth              Decimal
	Height                Decimal
	AutoMaxHeight         bool
	MaxHeight             Decimal
	HorizontalStretch     string //Enums.BWAValue;
	VerticalStretch       string //Enums.BWAValue;
	SkipOnInput           string //Enums.BWAValue;
	TextColor             string //Color
	Font                  Font
	Shortcut              string
	Title                 *FormattedStringType
	ToolTip               *LocalStringType
	ToolTipRepresentation string //Enums.TooltipRepresentation;
	GroupHorizontalAlign  string //Enums.ItemHorizontalAlignment;
	GroupVerticalAlign    string //Enums.ItemVerticalAlignment;
}

// Field ...
type Field struct {
	FormItemBase
	DataPath                    LFEDataPath
	Visible                     bool
	UserVisible                 *AdjustableBoolean
	DefaultItem                 bool
	Enabled                     bool
	ReadOnly                    bool
	SkipOnInput                 string //Enums.BWAValue;
	Title                       *LocalStringType
	TitleTextColor              string //Color
	TitleBackColor              string //Color
	TitleFont                   Font
	TitleLocation               string //Enums.FormElementTitleLocation;
	TitleHeight                 Decimal
	ToolTip                     *LocalStringType
	ToolTipRepresentation       string //Enums.TooltipRepresentation;
	WarningOnEditRepresentation string //Enums.WarningOnEditRepresentation;
	WarningOnEdit               *LocalStringType
	Shortcut                    string
	CommandSet                  *CommandsContent
	HorizontalAlign             string //Enums.ItemHorizontalAlignment;
	VerticalAlign               string //Enums.ItemVerticalAlignment;
	GroupHorizontalAlign        string //Enums.ItemHorizontalAlignment;
	GroupVerticalAlign          string //Enums.ItemVerticalAlignment;
	EditMode                    string //Enums.TableFieldEditMode;
	FixingInTable               string //Enums.FormFixedInTable;
	CellHyperlink               bool
	AutoCellHeight              bool
	ShowInHeader                bool
	HeaderPicture               *Picture
	HeaderHorizontalAlign       string //Enums.ItemHorizontalAlignment;
	ShowInFooter                bool
	FooterDataPath              LFEDataPath
	FooterText                  *LocalStringType
	FooterTextColor             string //Color
	FooterBackColor             string //Color
	FooterFont                  *Font
	FooterPicture               *Picture
	FooterHorizontalAlign       string //Enums.ItemHorizontalAlignment;
}

// CommandsContent ...
type CommandsContent struct {
	ExcludedCommand []string
}

// #Region Events

// FormItemEvents ...
type FormItemEvents struct {
	Event []*FormItemEvent
}

// FormItemEvent ...
type FormItemEvent struct {
	name     string
	callType string //Enums.HandlerCallType;
	_        string
}

// #EndRegion // Events

// #Region Attributes

// FormAttributes ...
type FormAttributes struct {
	Attribute []*FormAttribute
	// Items["ConditionalAppearance"] = "ConditionalAppearance";
}

// FormAttribute ...
type FormAttribute struct {
	name              string
	id                Decimal
	Type              *TypeDescription
	Title             *LocalStringType
	View              *AdjustableBoolean
	Edit              *AdjustableBoolean
	MainAttribute     bool
	SavedData         bool
	FillCheck         string //Enums.FillChecking
	UseAlways         *ContentType
	Save              *ContentType
	FunctionalOptions *FunctionalOptions
	Columns           *FormAttributeColumns
	//Settings ""; // FormAttribute()
}

// ContentType ...
type ContentType struct {
	Field []LFEDataPath
}

// FunctionalOptions ...
type FunctionalOptions struct {
	Item []MDObjectRef
}

// #Region Columns

// FormAttributeColumns ...
type FormAttributeColumns struct {
	Column            []*FormAttributeColumn
	AdditionalColumns []*FormAttributeAdditionalColumns
}

// FormAttributeAdditionalColumns ...
type FormAttributeAdditionalColumns struct {
	Table  LFEDataPath `xml:"table,attr"`
	Column []*FormAttributeColumn
}

// FormAttributeColumn ...
type FormAttributeColumn struct {
	name              string
	id                Decimal
	Title             *LocalStringType
	View              *AdjustableBoolean
	Edit              *AdjustableBoolean
	FillCheck         string //Enums.FillChecking
	FunctionalOptions *FunctionalOptions
}

// #EndRegion // Columns

// #EndRegion // Attributes

// #Region Commands

// FormCommands ...
type FormCommands struct {
	Command []FormCommand
}

// FormCommand ...
type FormCommand struct {
	name                     string
	id                       Decimal
	Title                    *LocalStringType
	ToolTip                  *LocalStringType
	Use                      *AdjustableBoolean
	Shortcut                 string
	Picture                  *Picture
	Action                   *FormCommandAction
	FunctionalOptions        *FunctionalOptions
	Representation           string //Enums.DefaultRepresentation;
	ModifiesSavedData        bool
	CurrentRowUse            string  //Enums.CurrentRowUse;
	AssociatedTableElementID Decimal `xml:"AssociatedTableElementId"`
}

// FormCommandAction ...
type FormCommandAction struct {
	callType string //Enums.HandlerCallType;
	_        string
}

// #EndRegion // Commands

// #Region CommandInterface

// FormCommandInterface ...
type FormCommandInterface struct {
	NavigationPanel *FormCommandInterfaceItems
	CommandBar      *FormCommandInterfaceItems
}

// FormCommandInterfaceItems ...
type FormCommandInterfaceItems struct {
	Item []*FormCommandInterfaceItem
}

// FormCommandInterfaceItem ...
type FormCommandInterfaceItem struct {
	Command        string
	Type           string //Enums.CommandKind;
	Attribute      LFEDataPath
	CommandGroup   string
	Index          Decimal
	DefaultVisible bool
	Visible        *AdjustableBoolean
}

// #EndRegion // CommandInterface

// #Region Parameters

// FormParameters ...
type FormParameters struct {
	Parameter []*FormParameter
}

// FormParameter ...
type FormParameter struct {
	name         string
	Type         *TypeDescription
	KeyParameter bool
}

// #EndRegion // Parameters

// #Region Addition

// Addition ...
type Addition struct {
	FormItemBase
	Source                *AdditionSource
	AdditionSource        *AdditionSource
	Visible               bool
	UserVisible           *AdjustableBoolean
	Enabled               bool
	PlacementArea         string //Enums.MenuElementPlacementArea;
	Title                 *LocalStringType
	ToolTip               *LocalStringType
	ToolTipRepresentation string //Enums.TooltipRepresentation;
	GroupHorizontalAlign  string //Enums.ItemHorizontalAlignment;
	GroupVerticalAlign    string //Enums.ItemVerticalAlignment;
}

// AdditionSource ...
type AdditionSource struct {
	Item string
	Type string //Enums.LogFormElementAdditionKind;
}

// #EndRegion // Addition

// #Region ChildItems

// ChildItems ...
type ChildItems []interface{}

// UnmarshalXML ...
func (md *ChildItems) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	//md.XMLName = start.Name
	// grab any other attrs

	// decode inner elements
	for {
		t, err := d.Token()
		if err != nil {
			return err
		}
		var i interface{}
		switch tt := t.(type) {
		case xml.StartElement:
			switch tt.Name.Local {
			case "AutoCommandBar":
				i = new(AutoCommandBar)
			case "Button":
				i = new(Button)
			case "ButtonGroup":
				i = new(ButtonGroup)
			case "CalendarField":
				i = new(CalendarField)
			case "ChartField":
				i = new(ChartField)
			case "CheckBoxField":
				i = new(CheckBoxField)
			case "ColumnGroup":
				i = new(ColumnGroup)
			case "CommandBar":
				i = new(CommandBar)
			case "ContextMenu":
				i = new(ContextMenu)
			case "DendrogramField":
				i = new(DendrogramField)
			case "FormattedDocumentField":
				i = new(FormattedDocumentField)
			case "GanttChartField":
				i = new(GanttChartField)
			case "GeographicalSchemaField":
				i = new(GeographicalSchemaField)
			case "GraphicalSchemaField":
				i = new(GraphicalSchemaField)
			case "HTMLDocumentField":
				i = new(HTMLDocumentField)
			case "InputField":
				i = new(InputField)
			case "LabelDecoration":
				i = new(LabelDecoration)
			case "LabelField":
				i = new(LabelField)
			case "Page":
				i = new(Page)
			case "Pages":
				i = new(Pages)
			case "PeriodField":
				i = new(PeriodField)
			case "PictureDecoration":
				i = new(PictureDecoration)
			case "PictureField":
				i = new(PictureField)
			case "PlannerField":
				i = new(PlannerField)
			case "Popup":
				i = new(Popup)
			case "ProgressBarField":
				i = new(ProgressBarField)
			case "RadioButtonField":
				i = new(RadioButtonField)
			case "SearchControlAddition":
				i = new(SearchControlAddition)
			case "SearchStringAddition":
				i = new(SearchStringAddition)
			case "SpreadSheetDocumentField":
				i = new(SpreadSheetDocumentField)
			case "Table":
				i = new(Table)
			case "TextDocumentField":
				i = new(TextDocumentField)
			case "TrackBarField":
				i = new(TrackBarField)
			case "UsualGroup":
				i = new(UsualGroup)
			case "ViewStatusAddition":
				i = new(ViewStatusAddition)
			default: // ignored for brevity
			}
			// known child element found, decode it
			if i != nil {
				err = d.DecodeElement(i, &tt)
				if err != nil {
					return err
				}
				*md = append(*md, i)
				i = nil
			}
		case xml.EndElement:
			if tt == start.End() {
				return nil
			}
		}

	}
	//return nil
}

//type ChildItems struct {
// This = Object();
// This.Ordered = True;
// Items = This.Items;
// Items["AutoCommandBar"]           = AutoCommandBar();
// Items["Button"]                   = Button();
// Items["ButtonGroup"]              = ButtonGroup();
// Items["CalendarField"]            = CalendarField();
// Items["ChartField"]               = ChartField();
// Items["CheckBoxField"]            = CheckBoxField();
// Items["ColumnGroup"]              = ColumnGroup();
// Items["CommandBar"]               = CommandBar();
// Items["ContextMenu"]              = ContextMenu();
// Items["DendrogramField"]          = DendrogramField();
// Items["FormattedDocumentField"]   = FormattedDocumentField();
// Items["GanttChartField"]          = GanttChartField();
// Items["GeographicalSchemaField"]  = GeographicalSchemaField();
// Items["GraphicalSchemaField"]     = GraphicalSchemaField();
// Items["HTMLDocumentField"]        = HTMLDocumentField();
// Items["InputField"]               = InputField();
// Items["LabelDecoration"]          = LabelDecoration();
// Items["LabelField"]               = LabelField();
// Items["Page"]                     = Page();
// Items["Pages"]                    = Pages();
// Items["PeriodField"]              = PeriodField();
// Items["PictureDecoration"]        = PictureDecoration();
// Items["PictureField"]             = PictureField();
// Items["PlannerField"]             = PlannerField();
// Items["Popup"]                    = Popup();
// Items["ProgressBarField"]         = ProgressBarField();
// Items["RadioButtonField"]         = RadioButtonField();
// Items["SearchControlAddition"]    = SearchControlAddition();
// Items["SearchStringAddition"]     = SearchStringAddition();
// Items["SpreadSheetDocumentField"] = SpreadSheetDocumentField();
// Items["Table"]                    = Table();
// Items["TextDocumentField"]        = TextDocumentField();
// Items["TrackBarField"]            = TrackBarField();
// Items["UsualGroup"]               = UsualGroup();
// Items["ViewStatusAddition"]       = ViewStatusAddition();
//}

// AutoCommandBar ...
type AutoCommandBar struct {
	GroupBase
	HorizontalAlign string //Enums.ItemHorizontalAlignment;
	Autofill        bool
}

// Button ...
type Button struct {
	FormItemBase
	Type        string //Enums.ManagedFormButtonType;
	DataPath    LFEDataPath
	CommandName string
	//Parameter ""; bool
	UserVisible                 *AdjustableBoolean
	Representation              string //Enums.ButtonRepresentation;
	DefaultButton               bool
	SkipOnInput                 string //Enums.BWAValue;
	Enabled                     bool
	DefaultItem                 bool
	OnlyInAllActions            string //Enums.BWAValue;
	Width                       Decimal
	AutoMaxWidth                bool
	MaxWidth                    Decimal
	MinWidth                    Decimal
	Height                      Decimal
	AutoMaxHeight               bool
	MaxHeight                   Decimal
	HorizontalStretch           bool
	VerticalStretch             bool
	GroupHorizontalAlign        string //Enums.ItemHorizontalAlignment;
	GroupVerticalAlign          string //Enums.ItemVerticalAlignment;
	PlacementArea               string //Enums.MenuElementPlacementArea;
	Check                       bool
	TextColor                   string //Color
	BackColor                   string //Color
	BorderColor                 string //Color
	Font                        *Font
	Shortcut                    string
	Picture                     *Picture
	Title                       *LocalStringType
	TitleHeight                 Decimal
	ToolTipRepresentation       string //Enums.TooltipRepresentation;
	RepresentationInContextMenu string //Enums.RepresentationInContextMenu;
	Shape                       string //Enums.ButtonShape;
	ShapeRepresentation         string //Enums.ButtonShapeRepresentation;
	PictureLocation             string //Enums.FormButtonPictureLocation;
}

// ButtonGroup ...
type ButtonGroup struct {
	GroupBase
	CommandSource  string //CommandSourceName
	PlacementArea  string //Enums.MenuElementPlacementArea;
	Representation string //Enums.ButtonGroupRepresentation;
}

// CalendarField ...
type CalendarField struct {
	Field
	Width                       Decimal
	AutoMaxWidth                bool
	MaxWidth                    Decimal
	MinWidth                    Decimal
	Height                      Decimal
	AutoMaxHeight               bool
	MaxHeight                   Decimal
	HorizontalStretch           bool
	VerticalStretch             bool
	SelectionMode               string //Enums.FormDateSelectionMode;
	ShowCurrentDate             bool
	CalendarNavigation          bool
	BeginOfRepresentationPeriod DateTime
	EndOfRepresentationPeriod   DateTime
	EnableStartDrag             bool
	EnableDrag                  bool
	Font                        *Font
	BorderColor                 string //Color
	Border                      *Border
	ShowMonthsPanel             bool
	WidthInMonths               Decimal
	HeightInMonths              Decimal
}

// ChartField ...
type ChartField struct {
	Field
	Width             Decimal
	AutoMaxWidth      bool
	MaxWidth          Decimal
	MinWidth          Decimal
	Height            Decimal
	AutoMaxHeight     bool
	MaxHeight         Decimal
	HorizontalStretch bool
	VerticalStretch   bool
}

// CheckBoxField ...
type CheckBoxField struct {
	Field
	CheckBoxType    string //Enums.CheckBoxType;
	ThreeState      bool
	BorderColor     string //Color
	BackColor       string //Color
	TextColor       string //Color
	Font            *Font
	EditFormat      *LocalStringType
	ItemTitleHeight Decimal
	ItemWidth       Decimal
	ItemHeight      Decimal
	EqualItemsWidth string //Enums.BWAValue;
}

// ColumnGroup ...
type ColumnGroup struct {
	GroupBase
	Group                 string //Enums.ColumnsGroup;
	ShowTitle             bool
	TitleBackColor        string //Color
	ShowInHeader          bool
	HeaderDataPath        LFEDataPath
	HeaderHorizontalAlign string //Enums.ItemHorizontalAlignment;
	HeaderFormat          *LocalStringType
	HeaderPicture         *Picture
	FixingInTable         string //Enums.FormFixedInTable;
}

// CommandBar ...
type CommandBar struct {
	GroupBase
	HorizontalLocation string //Enums.ItemHorizontalAlignment;
	CommandSource      string //CommandSourceName
}

// ContextMenu ...
type ContextMenu struct {
	GroupBase
	Autofill bool
}

// DendrogramField ...
type DendrogramField struct {
	Field
	Width             Decimal
	AutoMaxWidth      bool
	MaxWidth          Decimal
	MinWidth          Decimal
	Height            Decimal
	AutoMaxHeight     bool
	MaxHeight         Decimal
	HorizontalStretch bool
	VerticalStretch   bool
}

// FormattedDocumentField ...
type FormattedDocumentField struct {
	Field
	Width             Decimal
	AutoMaxWidth      bool
	MaxWidth          Decimal
	MinWidth          Decimal
	Height            Decimal
	AutoMaxHeight     bool
	MaxHeight         Decimal
	HorizontalStretch bool
	VerticalStretch   bool
	Output            string //Enums.UseOutput;
	TextColor         string //Color
	BackColor         string //Color
	BorderColor       string //Color
	Font              *Font
}

// GanttChartField ...
type GanttChartField struct {
	Field
	Width             Decimal
	AutoMaxWidth      bool
	MaxWidth          Decimal
	MinWidth          Decimal
	Height            Decimal
	AutoMaxHeight     bool
	MaxHeight         Decimal
	HorizontalStretch bool
	VerticalStretch   bool
}

// GeographicalSchemaField ...
type GeographicalSchemaField struct {
	Field
	Width             Decimal
	AutoMaxWidth      bool
	MaxWidth          Decimal
	MinWidth          Decimal
	Height            Decimal
	AutoMaxHeight     bool
	MaxHeight         Decimal
	HorizontalStretch bool
	VerticalStretch   bool
	Output            string //Enums.UseOutput;
	BorderColor       string //Color
}

// GraphicalSchemaField ...
type GraphicalSchemaField struct {
	Field
	Width             Decimal
	AutoMaxWidth      bool
	MaxWidth          Decimal
	MinWidth          Decimal
	Height            Decimal
	AutoMaxHeight     bool
	MaxHeight         Decimal
	HorizontalStretch bool
	VerticalStretch   bool
	Output            string //Enums.UseOutput;
	Edit              bool
	BorderColor       string //Color
}

// HTMLDocumentField ...
type HTMLDocumentField struct {
	Field
	Width             Decimal
	AutoMaxWidth      bool
	MaxWidth          Decimal
	MinWidth          Decimal
	Height            Decimal
	AutoMaxHeight     bool
	MaxHeight         Decimal
	HorizontalStretch bool
	VerticalStretch   bool
	Output            string //Enums.UseOutput;
	BorderColor       string //Color
}

// InputField ...
type InputField struct {
	Field
	Width                      Decimal
	AutoMaxWidth               bool
	MaxWidth                   Decimal
	MinWidth                   Decimal
	Height                     Decimal
	AutoMaxHeight              bool
	MaxHeight                  Decimal
	HorizontalStretch          string //Enums.BWAValue;
	VerticalStretch            string //Enums.BWAValue;
	Wrap                       bool
	PasswordMode               string //Enums.BWAValue;
	MultiLine                  string //Enums.BWAValue;
	ExtendedEdit               string //Enums.BWAValue;
	MarkNegatives              string //Enums.BWAValue;
	DropListButton             string //Enums.BWAValue;
	ChoiceButton               string //Enums.BWAValue;
	ChoiceButtonRepresentation string //Enums.ChoiceButtonRepresentation;
	ChoiceButtonPicture        *Picture
	ClearButton                string //Enums.BWAValue;
	SpinButton                 string //Enums.BWAValue;
	OpenButton                 string //Enums.BWAValue;
	CreateButton               string //Enums.BWAValue;
	Mask                       string
	AutoChoiceIncomplete       string //Enums.BWAValue;
	QuickChoice                string //Enums.BWAValue;
	ChoiceFoldersAndItems      string //Enums.FoldersAndItems;
	Format                     *LocalStringType
	EditFormat                 *LocalStringType
	AutoMarkIncomplete         string //Enums.BWAValue;
	ChooseType                 bool
	IncompleteChoiceMode       string //Enums.IncompleteItemChoiceMode;
	TypeDomainEnabled          bool
	TextEdit                   bool
	EditTextUpdate             string //Enums.EditTextUpdate;
	//MinValue "";MaxValue ""; MDObjectRef
	ChoiceParameterLinks          *ChoiceParameterLinks
	ChoiceParameters              *ChoiceParameters
	AvailableTypes                *TypeDescription
	ListChoiceMode                bool
	ChoiceList                    *ValueList
	ChoiceListButton              string //Enums.BWAValue;
	ChoiceListHeight              Decimal
	DropListWidth                 Decimal
	TextColor                     string //Color
	BackColor                     string //Color
	BorderColor                   string //Color
	Font                          *Font
	TypeLink                      *ItemTypeLink
	HeightControlVariant          string //Enums.HeightControlVariant;
	AutoShowClearButtonMode       string //Enums.AutoShowClearButtonMode;
	AutoShowOpenButtonMode        string //Enums.AutoShowOpenButtonMode;
	AutoCorrectionOnTextInput     string //Enums.AutoCorrectionOnTextInput;
	SpellCheckingOnTextInput      string //Enums.SpellCheckingOnTextInput;
	AutoCapitalizationOnTextInput string //Enums.AutoCapitalizationOnTextInput;
	SpecialTextInputMode          string //Enums.SpecialTextInputMode;
	OnScreenKeyboardReturnKeyText string //Enums.OnScreenKeyboardReturnKeyText;
	InputHint                     *LocalStringType
	ChoiceHistoryOnInput          string //Enums.ChoiceHistoryOnInput;
}

// LabelDecoration ...
type LabelDecoration struct {
	Decoration
	Hyperlink       bool
	HorizontalAlign string //Enums.ItemHorizontalAlignment;
	VerticalAlign   string //Enums.ItemVerticalAlignment;
	TitleHeight     Decimal
	BackColor       string //Color
	BorderColor     string //Color
	Border          *Border
}

// LabelField ...
type LabelField struct {
	Field
	Width             Decimal
	AutoMaxWidth      bool
	MaxWidth          Decimal
	MinWidth          Decimal
	Height            Decimal
	AutoMaxHeight     bool
	MaxHeight         Decimal
	HorizontalStretch string //Enums.BWAValue;
	VerticalStretch   string //Enums.BWAValue;
	MarkNegatives     string //Enums.BWAValue;
	Format            *LocalStringType
	Hiperlink         bool
	PasswordMode      string //Enums.BWAValue;
	Border            *Border
	BorderColor       string //Color
	TextColor         string //Color
	BackColor         string //Color
	Font              *Font
}

// Page ...
type Page struct {
	GroupBase
	Picture           *Picture
	Group             string //Enums.FormChildrenGroup;
	ChildrenAlign     string //Enums.FormChildrenAlign;
	HorizontalSpacing string //Enums.FormItemSpacing;
	VerticalSpacing   string //Enums.FormItemSpacing;
	HorizontalAlign   string //Enums.ItemHorizontalAlignment;
	VerticalAlign     string //Enums.ItemVerticalAlignment;
	ChildItemsWidth   string //Enums.FormChildrenWidth;
	Format            *LocalStringType
	ShowTitle         bool
	TitleDataPath     LFEDataPath
	BackColor         string //Color
	ScrollOnCompress  bool
}

// Pages ...
type Pages struct {
	GroupBase
	PagesRepresentation string //Enums.FormPagesRepresentation;
}

// PeriodField ...
type PeriodField struct {
	Field
	Width             Decimal
	AutoMaxWidth      bool
	MaxWidth          Decimal
	MinWidth          Decimal
	Height            Decimal
	AutoMaxHeight     bool
	MaxHeight         Decimal
	HorizontalStretch bool
	VerticalStretch   bool
	Font              *Font
	BorderColor       string //Color
	Border            *Border
}

// PictureDecoration ...
type PictureDecoration struct {
	Decoration
	Picture                *Picture
	PictureSize            string //Enums.PictureSize;
	Hyperlink              bool
	Zoomable               bool
	NonselectedPictureText *LocalStringType
	EnableStartDrag        bool
	EnableDrag             bool
	Border                 *Border
	BorderColor            string //Color
}

// PictureField ...
type PictureField struct {
	Field
	Width                  Decimal
	AutoMaxWidth           bool
	MaxWidth               Decimal
	MinWidth               Decimal
	Height                 Decimal
	AutoMaxHeight          bool
	MaxHeight              Decimal
	HorizontalStretch      bool
	VerticalStretch        bool
	PictureSize            string //Enums.PictureSize;
	Zoomable               bool
	Hyperlink              bool
	NonselectedPictureText *LocalStringType
	EnableStartDrag        bool
	EnableDrag             bool
	ValuesPicture          *Picture
	TextColor              string //Color
	Border                 *Border
	BorderColor            string //Color
	Font                   *Font
}

// PlannerField ...
type PlannerField struct {
	Field
	Width             Decimal
	AutoMaxWidth      bool
	MaxWidth          Decimal
	MinWidth          Decimal
	Height            Decimal
	AutoMaxHeight     bool
	MaxHeight         Decimal
	HorizontalStretch bool
	VerticalStretch   bool
	EnableStartDrag   bool
	EnableDrag        bool
}

// Popup ...
type Popup struct {
	GroupBase
	Picture             *Picture
	CommandSource       string //CommandSourceName
	Representation      string //Enums.ButtonRepresentation;
	PlacementArea       string //Enums.MenuElementPlacementArea;
	Shape               string //Enums.ButtonShape;
	ShapeRepresentation string //Enums.ButtonShapeRepresentation;
	BackColor           string //Color
	BorderColor         string //Color
}

// ProgressBarField ...
type ProgressBarField struct {
	Field
	Width             Decimal
	AutoMaxWidth      bool
	MaxWidth          Decimal
	MinWidth          Decimal
	Height            Decimal
	AutoMaxHeight     bool
	MaxHeight         Decimal
	HorizontalStretch bool
	VerticalStretch   bool
	MinValue          Decimal
	MaxValue          Decimal
	Orientation       string //Enums.FormElementOrientation;
	Representation    string //Enums.FormProgressBarRepresentation;
	ShowPercent       bool
	BorderColor       string //Color
}

// RadioButtonField ...
type RadioButtonField struct {
	Field
	RadioButtonType   string //Enums.RadioButtonType;
	ItemWidth         Decimal
	ItemHeight        Decimal
	ItemTitleHeight   Decimal
	ColumnsCount      Decimal
	EqualColumnsWidth string //Enums.BWAValue;
	ChoiceList        *ValueList
	Font              *Font
	TextColor         string //Color
	BackColor         string //Color
	BorderColor       string //Color
}

// SearchControlAddition ...
type SearchControlAddition struct {
	Addition
	Width             Decimal
	AutoMaxWidth      bool
	MaxWidth          Decimal
	MinWidth          Decimal
	HorizontalStretch string //Enums.BWAValue;
	BackColor         string //Color
	TextColor         string //Color
	BorderColor       string //Color
	Font              *Font
}

// SearchStringAddition ...
type SearchStringAddition struct {
	Addition
	Width             Decimal
	AutoMaxWidth      bool
	MaxWidth          Decimal
	MinWidth          Decimal
	HorizontalStretch string //Enums.BWAValue;
	BackColor         string //Color
	TextColor         string //Color
	BorderColor       string //Color
	Font              *Font
}

// SpreadSheetDocumentField ...
type SpreadSheetDocumentField struct {
	Field
	Width                 Decimal
	AutoMaxWidth          bool
	MaxWidth              Decimal
	MinWidth              Decimal
	Height                Decimal
	AutoMaxHeight         bool
	MaxHeight             Decimal
	HorizontalStretch     bool
	VerticalStretch       bool
	ShowGrid              bool
	ShowHeaders           bool
	VerticalScrollBar     string //SpreadSheetDocumentScrollBarUse
	HorizontalScrollBar   string //SpreadSheetDocumentScrollBarUse
	BlackAndWhiteView     bool
	Protection            bool
	SelectionShowMode     string //Enums.SelectionShowMode;
	Output                string //Enums.UseOutput;
	Edit                  bool
	ShowGroups            bool
	EnableStartDrag       bool
	EnableDrag            bool
	BorderColor           string //Color
	ViewScalingMode       string //Enums.ViewScalingMode;
	ShowCellNames         bool
	ShowRowAndColumnNames bool
	PointerType           string //Enums.SpreadsheetDocumentPointerType;
}

// Table ...
type Table struct {
	FormItemBase
	Representation                          string //Enums.TableRepresentation;
	Visible                                 bool
	UserVisible                             *AdjustableBoolean
	CommandBarLocation                      string //Enums.FormElementCommandBarLocation;
	Autofill                                bool
	Enabled                                 bool
	ReadOnly                                bool
	SkipOnInput                             string //Enums.BWAValue;
	DefaultItem                             bool
	ChangeRowSet                            bool
	ChangeRowOrder                          bool
	Width                                   Decimal
	AutoMaxWidth                            bool
	MaxWidth                                Decimal
	MinWidth                                Decimal
	Height                                  Decimal
	AutoMaxHeight                           bool
	MaxHeight                               Decimal
	HeightInTableRows                       Decimal
	HeightControlVariant                    string //Enums.TableHeightControlVariant;
	AutoMaxRowsCount                        bool
	MaxRowsCount                            Decimal
	ChoiceMode                              bool
	MultipleChoice                          bool
	RowInputMode                            string //Enums.TableRowInputMode;
	SelectionMode                           string //Enums.TableSelectionMode;
	RowSelectionMode                        string //Enums.TableRowSelectionMode;
	Header                                  bool
	HeaderHeight                            Decimal
	Footer                                  bool
	FooterHeight                            Decimal
	HorizontalScrollBar                     string //Enums.TableScrollBarUse;
	VerticalScrollBar                       string //Enums.TableScrollBarUse;
	HorizontalLines                         bool
	VerticalLines                           bool
	FixedLeft                               Decimal
	FixedRight                              Decimal
	UseAlternationRowColor                  bool
	AutoInsertNewRow                        bool
	AutoAddIncomplete                       string //Enums.BWAValue;
	AutoMarkIncomplete                      string //Enums.BWAValue;
	SearchOnInput                           string //Enums.SearchOnInput;
	InitialListView                         string //Enums.TableInitialListView;
	InitialTreeView                         string //Enums.TableInitialTreeView;
	Output                                  string //Enums.UseOutput;
	HorizontalStretch                       bool
	VerticalStretch                         bool
	EnableStartDrag                         bool
	EnableDrag                              bool
	DataPath                                LFEDataPath
	RowPictureDataPath                      LFEDataPath
	RowsPicture                             *Picture
	TextColor                               string //Color
	BackColor                               string //Color
	BorderColor                             string //Color
	Font                                    *Font
	Title                                   *LocalStringType
	TitleHeight                             Decimal
	TitleFont                               Font
	TitleTextColor                          string //Color
	TitleLocation                           string //Enums.FormElementTitleLocation;
	Shortcut                                string
	CommandSet                              *CommandsContent
	ToolTip                                 *LocalStringType
	ToolTipRepresentation                   string //Enums.TooltipRepresentation;
	SearchStringLocation                    string //Enums.SearchStringLocation;
	ViewStatusLocation                      string //Enums.ViewStatusLocation;
	SearchControlLocation                   string //Enums.SearchControlLocation;
	GroupHorizontalAlign                    string //Enums.ItemHorizontalAlignment;
	GroupVerticalAlign                      string //Enums.ItemVerticalAlignment;
	RefreshRequest                          string //Enums.RefreshRequestMethod;
	ViewMode                                string //Enums.DataCompositionSettingsViewMode;
	SettingsNamedItemDetailedRepresentation bool
	AutoRefresh                             bool
	AutoRefreshPeriod                       Decimal
	Period                                  *StandardPeriod
	ChoiceFoldersAndItems                   string //Enums.FoldersAndItemsUse;
	RestoreCurrentRow                       bool
	//TopLevelParent ""; bool
	AllowRootChoice bool
	//RowFilter ""; string //Enums.UpdateOnDataChange;
	UserSettingsGroup string
}

// TextDocumentField ...
type TextDocumentField struct {
	Field
	Width             Decimal
	AutoMaxWidth      bool
	MaxWidth          Decimal
	MinWidth          Decimal
	Height            Decimal
	AutoMaxHeight     bool
	MaxHeight         Decimal
	HorizontalStretch bool
	VerticalStretch   bool
	Output            string //Enums.UseOutput;
	TextColor         string //Color
	BackColor         string //Color
	BorderColor       string //Color
	Font              *Font
}

// TrackBarField ...
type TrackBarField struct {
	Field
	Width             Decimal
	AutoMaxWidth      bool
	MaxWidth          Decimal
	MinWidth          Decimal
	Height            Decimal
	AutoMaxHeight     bool
	MaxHeight         Decimal
	HorizontalStretch bool
	VerticalStretch   bool
	MinValue          Decimal
	MaxValue          Decimal
	Step              Decimal
	LargeStep         Decimal
	MarkingStep       Decimal
	Orientation       string //Enums.FormElementOrientation;
	MarkingAppearance string //Enums.MarkingStyle;
	BorderColor       string //Color
}

// UsualGroup ...
type UsualGroup struct {
	GroupBase
	Group                        string //Enums.FormChildrenGroup;
	ChildrenAlign                string //Enums.FormChildrenAlign;
	HorizontalSpacing            string //Enums.FormItemSpacing;
	VerticalSpacing              string //Enums.FormItemSpacing;
	HorizontalAlign              string //Enums.ItemHorizontalAlignment;
	VerticalAlign                string //Enums.ItemVerticalAlignment;
	Behavior                     string //Enums.UsualGroupBehavior;
	CollapsedRepresentationTitle *LocalStringType
	Collapsed                    bool
	ControlRepresentation        string //Enums.UsualGroupControlRepresentation;
	Representation               string //Enums.UsualGroupControlRepresentation;
	ShowLeftMargin               bool
	United                       bool
	ChildItemsWidth              string //Enums.FormChildrenWidth;
	Format                       *LocalStringType
	ShowTitle                    bool
	TitleDataPath                LFEDataPath
	BackColor                    string //Color
	ThroughAlign                 string //Enums.UsualGroupThroughAlign;
}

// ViewStatusAddition ...
type ViewStatusAddition struct {
	Addition
	Width              Decimal
	AutoMaxWidth       bool
	MaxWidth           Decimal
	MinWidth           Decimal
	HorizontalStretch  string //Enums.BWAValue;
	HorizontalLocation string //Enums.ItemHorizontalAlignment;
	BackColor          string //Color
	ButtonColor        string //Color
	TextColor          string //Color
	TitleTextColor     string //Color
	BorderColor        string //Color
	Font               *Font
	TitleFont          *Font
	Border             *Border
}

// #EndRegion // ChildItems

// #Region Other

// Font ...
type Font struct {
	ref       string // StyleRef
	faceName  string
	height    Decimal
	bold      bool
	italic    bool
	underline bool
	strikeout bool
	kind      string //Enums.FontType;
	scale     Decimal
}

// Border ...
type Border struct {
	ref   string  //StyleRef
	style string  //BorderType
	width Decimal //unsignedInt
}

// StandardPeriod ...
type StandardPeriod struct {
	variant   string //Enums.StandardPeriodVariant;
	startDate DateTime
	endDate   DateTime
}

// ValueList ...
type ValueList struct {
	Item []*ValueListItem
}

// ValueListItem ...
type ValueListItem struct {
	Presentation string
	CheckState   Decimal
	//Value ""; // ValueListItem()
}

// FormattedStringType ...
type FormattedStringType struct {
	LocalStringType
	formatted bool
}

// Picture ...
type Picture struct {
	url string
	ref string //PictureRef
	t   bool
	tx  Decimal
	ty  Decimal
	gx  Decimal
	gy  Decimal
	gw  Decimal
	gh  Decimal
	_   base64Binary
}

// ItemTypeLink ...
type ItemTypeLink struct {
	DataPath *string
	LinkItem Decimal
}

// AdjustableBoolean ...
type AdjustableBoolean struct {
	Common []bool
	Value  []*AdjustableBooleanItemType
}

// AdjustableBooleanItemType ...
type AdjustableBooleanItemType struct {
	name MDObjectRef
	_    bool
}

// #EndRegion // Other

// #EndRegion // Form
