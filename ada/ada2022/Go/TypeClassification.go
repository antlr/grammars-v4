package parser

type TypeClassification int

const (
	Global_ TypeClassification = iota
	Block_
	TypeName_
	ObjectName_
	SubprogramName_
	PackageName_
	ExceptionName_
	GenericName_
	EnumerationLiteral_
	ComponentName_
	LabelName_
)
