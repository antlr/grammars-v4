from enum import Enum, auto

class TypeClassification(Enum):
    Global_ = auto()
    Block_ = auto()
    TypeName_ = auto()
    ObjectName_ = auto()
    SubprogramName_ = auto()
    PackageName_ = auto()
    ExceptionName_ = auto()
    GenericName_ = auto()
    EnumerationLiteral_ = auto()
    ComponentName_ = auto()
    LabelName_ = auto()
