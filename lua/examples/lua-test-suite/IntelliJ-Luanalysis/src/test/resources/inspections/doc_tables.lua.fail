---@alias DocTable1 {a: number}

---@type DocTable1
local docTable1

---@class NotDocTable1
---@field a number
local NotDocTable1

---@type NotDocTable1
local notDocTable1

docTable1 = {a = 1}
notDocTable1 = <error descr="Type mismatch. Required: 'NotDocTable1' Found: 'table'">{a = 1}</error>

docTable1 = notDocTable1
notDocTable1 = <error descr="Type mismatch. Required: 'NotDocTable1' Found: 'DocTable1'">docTable1</error>

docTable1 = {a = <error descr="Type mismatch. Required: 'number' Found: '\"not a number\"'">"not a number"</error>}
docTable1 = {a = 1, extraMember = "whatever"}

---@alias GenericDocTable<T> {a: T}

---@type number
local aNumber

---@type string
local aString

---@type GenericDocTable<number>
local genericNumberDocTable

genericNumberDocTable = {a = aNumber}
genericNumberDocTable = {a = <error descr="Type mismatch. Required: 'number' Found: 'string'">aString</error>}

---@type GenericDocTable<string>
local genericStringDocTable

genericStringDocTable = {a = <error descr="Type mismatch. Required: 'string' Found: 'number'">aNumber</error>}
genericStringDocTable = {a = aString}

---@param docTable GenericDocTable<string>
local function takesADocTable(docTable)
end

takesADocTable({a = aString})
takesADocTable({a = <error descr="Type mismatch. Required: 'string' Found: 'number'">aNumber</error>})

---@type nil | string
local nilOrString

---@type std__Packed<nil | string>
local sparseStringArray = {
    nilOrString,
    n = 1,
}

---@alias NestedDocTable {docTable1: DocTable1, docTable2: {a: number, b: string}}

---@type NestedDocTable
local nestedDocTable = {
    docTable1 = {a = aNumber},
    docTable2 = {a = aNumber, b = aString}
}

nestedDocTable = {
    docTable1 = {a = 1},
    docTable2 = {a = 1, b = "a string"}
}

nestedDocTable = {
    docTable1 = {a = <error descr="Type mismatch. Required: 'number' Found: 'string'">aString</error>},
    docTable2 = <error descr="Type mismatch. Missing member: 'b' of: '{ a: number, b: string }'">{a = aNumber}</error>
}

local notDocTable2 = {a = 1, b = 10}

---@type NestedDocTable
local nestedDocTable3 = {
    docTable1 = {a = <error descr="Type mismatch. Required: 'number' Found: 'string'">aString</error>, b = aString},
    docTable2 = {a = 1, b = <error descr="Type mismatch. Required: 'string' Found: '1'">1</error>}
}

---@return NestedDocTable
local function returnNestedDocTable()
    return {
        docTable1 = {a = <error descr="Type mismatch. Required: 'number' Found: 'table'">{}</error>},
        docTable2 = {a = aNumber, b = <error descr="Type mismatch. Required: 'string' Found: 'number'">aNumber</error>}
    }
end

---@alias DocTableWithOptionalField {requiredField: number, optional: nil | number}

---@type DocTableWithOptionalField
local docTableWithOptionalField = {
    requiredField = 1
}

docTableWithOptionalField = {
    requiredField = 1,
    optional = <error descr="Type mismatch. Required: 'nil | number' Found: 'string'">aString</error>
}

---@alias DocTableArrayValue {a: number}

---@alias DocTableWithArrayValues {values: DocTableArrayValue[]}

---@type DocTableWithArrayValues
local goodLiteral = {
    values = {
        {
            a = 1
        }
    }
}

---@type DocTableWithArrayValues
local badLiteral = {
    values = {
        {
            a = <error descr="Type mismatch. Required: 'number' Found: '\"some string\"'">"some string"</error>
        }
    }
}


---@alias DocTableStringNumberBooleanTuple {string, number, boolean}

---@param tuple DocTableStringNumberBooleanTuple
local function testTuple(tuple) end

testTuple({"hi", 1, true})
testTuple({"hi", 1, <error descr="Type mismatch. Required: 'boolean' Found: '1'">1</error>})

---@type number[]
local numberArray

---@alias DocTableNumberTuple {number, number, number}

---@type DocTableNumberTuple
local numberTuple

numberArray = numberTuple
numberTuple = <error descr="Type mismatch. Required: 'DocTableNumberTuple' Found: 'number[]'">numberArray</error>

numberTuple = {aNumber, aNumber, aNumber}
numberTuple = {[1] = aNumber, [2] = aNumber, [3] = aNumber}
numberTuple = {aNumber, aNumber, [3] = aNumber}

---@alias DocTableNonContiguousNumberTuple {number, number, number, [5] = number}

---@type DocTableNonContiguousNumberTuple
local docTableNonContiguousNumberTuple

numberArray = <error descr="Type mismatch. Required array index: '4' Found non-contiguous index: '5'">docTableNonContiguousNumberTuple</error>

docTableNonContiguousNumberTuple = {aNumber, aNumber, aNumber, [5] = aNumber}


---@alias DocTablePrimitiveAndTable {primitiveField: string, tableField: {}}

---@type DocTablePrimitiveAndTable
local primitiveAndTable = {
    primitiveField = "a string",
    tableField = {}
}

primitiveAndTable = {
    primitiveField = <error descr="Type mismatch. Required: 'string' Found: '1'">1</error>,
    tableField = {}
}

---@alias DocTableUnionMemberA {tag: "String", value: string}

---@alias DocTableUnionMemberB {tag: "Number", value: number}

---@type (DocTableUnionMemberA | DocTableUnionMemberB)[]
local arrayOfUnions = {
    {
        tag = <error descr="Type mismatch. Required: '\"String\"' Found: '\"Number\"', on union candidate DocTableUnionMemberA">"Number"</error>,
        value = <error descr="Type mismatch. Required: 'number' Found: 'table', on union candidate DocTableUnionMemberB"><error descr="Type mismatch. Required: 'string' Found: 'table', on union candidate DocTableUnionMemberA">{}</error></error>
    }
}

---@alias DocTableNilOrArray {value: nil | DocTableNilOrArray[]}

---@type DocTableNilOrArray
local nilOrDocTableArray = {
    value = {
        {
            value = {
                {
                    value = <error descr="Type mismatch. Required: 'DocTableNilOrArray[] | nil' Found: '\"invalid\"'">"invalid"</error>,
                },
                {
                    value = {},
                },
                {
                    value = {
                        <error descr="Type mismatch. Required: 'DocTableNilOrArray[]' Found non-array field 'value'">value = {}</error>,
                    },
                },
            },
        },
    },
    {
        value = nil,
    },
}

---@alias PotentiallyEmptyDocTable {a: nil | number}

---@type PotentiallyEmptyDocTable
local potentiallyEmpty

potentiallyEmpty = {}
potentiallyEmpty = <error descr="Type mismatch. Required: 'PotentiallyEmptyDocTable' Found: '1'">1</error>
potentiallyEmpty = <error descr="Type mismatch. Required: 'PotentiallyEmptyDocTable' Found: '\"invalid\"'">"invalid"</error>


local RecursiveField = {}
RecursiveField.recursiveField = RecursiveField

local CrossRecursiveField1 = {}
local CrossRecursiveField2 = {}

CrossRecursiveField1.recursiveField = CrossRecursiveField2
CrossRecursiveField2.recursiveField = CrossRecursiveField1


---@shape ShapeExtendingDocTable : DocTable1

---@type ShapeExtendingDocTable
local shapeExtendingDocTable

shapeExtendingDocTable.a = 1
shapeExtendingDocTable.a = <error descr="Type mismatch. Required: 'number' Found: 'string'">aString</error>


---@alias AliasedGenericDocTable<A> GenericDocTable<A>

---@shape ShapeExtendingAlias : AliasedGenericDocTable<"hi">

---@type ShapeExtendingAlias
local shapeExtendingAlias

shapeExtendingAlias.a = <error descr="Type mismatch. Required: '\"hi\"' Found: 'string'">aString</error>
shapeExtendingAlias.a = "hi"
