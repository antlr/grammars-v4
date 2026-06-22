package parser

type TypeClassification int

const (
	Global_               TypeClassification = iota
	Block_
	Function_
	Variable_
	TypeSpecifier_
	StorageClassSpecifier_
	TypeQualifier_
	FunctionSpecifier_
	AlignmentSpecifier_
	AtomicTypeSpecifier_
	EnumSpecifier_
)
