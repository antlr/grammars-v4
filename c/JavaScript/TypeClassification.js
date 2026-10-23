const TypeClassification = Object.freeze({
    Global_: 0,
    Block_: 1,
    Function_: 2,
    Variable_: 3,
    TypeSpecifier_: 4,
    StorageClassSpecifier_: 5,
    TypeQualifier_: 6,
    FunctionSpecifier_: 7,
    AlignmentSpecifier_: 8,
    AtomicTypeSpecifier_: 9,
    EnumSpecifier_: 10
});

// Reverse mapping for toString
const TypeClassificationNames = Object.freeze(
    Object.fromEntries(Object.entries(TypeClassification).map(([k, v]) => [v, k]))
);

export { TypeClassification, TypeClassificationNames };
export default TypeClassification;
