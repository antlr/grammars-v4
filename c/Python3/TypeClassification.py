from enum import IntEnum

class TypeClassification(IntEnum):
    Global_ = 0
    Block_ = 1
    Function_ = 2
    Variable_ = 3
    TypeSpecifier_ = 4
    StorageClassSpecifier_ = 5
    TypeQualifier_ = 6
    FunctionSpecifier_ = 7
    AlignmentSpecifier_ = 8
    AtomicTypeSpecifier_ = 9
    EnumSpecifier_ = 10
