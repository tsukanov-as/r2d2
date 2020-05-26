package conf

import "github.com/tsukanov-as/r2d2/conf/enums"

// TypeDescription ...
type TypeDescription struct {
	Type                 QName
	TypeSet              QName
	TypeID               UUID `xml:"TypeId"`
	NumberQualifiers     NumberQualifiers
	StringQualifiers     StringQualifiers
	DateQualifiers       DateQualifiers
	BinaryDataQualifiers BinaryDataQualifiers
}

// NumberQualifiers ...
type NumberQualifiers struct {
	Digits         Decimal
	FractionDigits Decimal
	AllowedSign    enums.AllowedSign
}

// StringQualifiers ...
type StringQualifiers struct {
	Length        Decimal
	AllowedLength enums.AllowedLength
}

// DateQualifiers ...
type DateQualifiers struct {
	DateFractions enums.DateFractions
}

// BinaryDataQualifiers ...
type BinaryDataQualifiers struct {
	Length        Decimal
	AllowedLength enums.AllowedLength
}
