---@type number
local anyNumber

---@type string
local anyString

---@type 1
local one


---@class DocField
---@field value number
---@field method fun(): number
---@field multipleResultsMethod fun(): number, boolean
local DocField = {}

---@class TableExplicitField
local TableExplicitField = {
    ---@type number
    value = one,
    ---@type fun(): number
    method = function() return
    one
    end,
}

---@class TableImplicitField
local TableImplicitField = {
    value = anyNumber,
    method = function()
        return anyNumber
    end,
}

---@class AssignedExplicitField
local AssignedExplicitField = {}

---@type number
AssignedExplicitField.value = 1

---@type fun(): number
AssignedExplicitField.method = function()
    return anyNumber
end

---@class AssignedImplicitField
local AssignedImplicitField = {}

AssignedImplicitField.value = anyNumber
AssignedImplicitField.method = function()
    return anyNumber
end

---@class DeclaredExplicitMethod
local DeclaredExplicitMethod = {}

---@return number
function DeclaredExplicitMethod.method()
    return one
end

---@class DeclaredImplicitMethod
local DeclaredImplicitMethod = {}

function DeclaredImplicitMethod.method()
    return anyNumber
end


---@type DocField
local docField

---@type TableExplicitField
local tableExplicitField

---@type TableImplicitField
local tableImplicitField

---@type AssignedExplicitField
local assignedExplicitField

---@type AssignedImplicitField
local assignedImplicitField

---@type DeclaredExplicitMethod
local declaredExplicitMethod

---@type DeclaredImplicitMethod
local declaredImplicitMethod


docField.value = anyNumber
tableExplicitField.value = anyNumber
tableImplicitField.value = anyNumber
assignedExplicitField.value = anyNumber
assignedImplicitField.value = anyNumber


anyNumber = docField.value
anyNumber = tableExplicitField.value
anyNumber = tableImplicitField.value
anyNumber = assignedExplicitField.value
anyNumber = assignedImplicitField.value

one = <error descr="Type mismatch. Required: '1' Found: 'number'">docField.value</error>
one = <error descr="Type mismatch. Required: '1' Found: 'number'">tableExplicitField.value</error>
one = <error descr="Type mismatch. Required: '1' Found: 'number'">tableImplicitField.value</error>
one = <error descr="Type mismatch. Required: '1' Found: 'number'">assignedExplicitField.value</error>
one = <error descr="Type mismatch. Required: '1' Found: 'number'">assignedImplicitField.value</error>


anyNumber = docField.method()
anyNumber = tableExplicitField.method()
anyNumber = tableImplicitField.method()
anyNumber = assignedExplicitField.method()
anyNumber = assignedImplicitField.method()
anyNumber = declaredExplicitMethod.method()
anyNumber = declaredImplicitMethod.method()

one = <error descr="Type mismatch. Required: '1' Found: 'number'">docField.method()</error>
one = <error descr="Type mismatch. Required: '1' Found: 'number'">tableExplicitField.method()</error>
one = <error descr="Type mismatch. Required: '1' Found: 'number'">tableImplicitField.method()</error>
one = <error descr="Type mismatch. Required: '1' Found: 'number'">assignedExplicitField.method()</error>
one = <error descr="Type mismatch. Required: '1' Found: 'number'">assignedImplicitField.method()</error>
one = <error descr="Type mismatch. Required: '1' Found: 'number'">declaredExplicitMethod.method()</error>
one = <error descr="Type mismatch. Required: '1' Found: 'number'">declaredImplicitMethod.method()</error>



---@class DocFieldChildDocField : DocField
---@field value 1
---@field method fun(): 1

---@class TableExplicitFieldChildDocField : DocField
local TableExplicitFieldChildDocField = {
    ---@type 1
    value = 1,
    ---@type fun(): 1
    method = function()
        return one
    end,
}

---@class TableImplicitFieldChildDocField : DocField
local TableImplicitFieldChildDocField = {
    value = 1,
    method = function()
        return one
    end,
}

---@class AssignedExplicitFieldChildDocField : DocField
local AssignedExplicitFieldChildDocField = {}

---@type 1
AssignedExplicitFieldChildDocField.value = 1

---@type fun(): 1
AssignedExplicitFieldChildDocField.method = function()
    return one
end

---@class AssignedImplicitFieldChildDocField : DocField
local AssignedImplicitFieldChildDocField = {}

AssignedImplicitFieldChildDocField.value = one
AssignedImplicitFieldChildDocField.method = function()
    return one
end

---@class DeclaredExplicitMethodChildDocField : DocField
local DeclaredExplicitMethodChildDocField = {}

---@return 1
function DeclaredExplicitMethodChildDocField.method()
    return one
end

---@class DeclaredImplicitMethodChildDocField : DocField
local DeclaredImplicitMethodChildDocField = {}

function DeclaredImplicitMethodChildDocField.method()
    return one
end


---@type DocFieldChildDocField
local docFieldChildDocField

---@type TableExplicitFieldChildDocField
local tableExplicitFieldChildDocField

---@type TableImplicitFieldChildDocField
local tableImplicitFieldChildDocField

---@type AssignedExplicitFieldChildDocField
local assignedExplicitFieldChildDocField

---@type AssignedImplicitFieldChildDocField
local assignedImplicitFieldChildDocField

---@type DeclaredExplicitMethodChildDocField
local declaredExplicitMethodChildDocField

---@type DeclaredImplicitMethodChildDocField
local declaredImplicitMethodChildDocField


docFieldChildDocField.value = <error descr="Type mismatch. Required: '1' Found: 'number'">anyNumber</error>
tableExplicitFieldChildDocField.value = <error descr="Type mismatch. Required: '1' Found: 'number'">anyNumber</error>
tableImplicitFieldChildDocField.value = anyNumber
assignedExplicitFieldChildDocField.value = <error descr="Type mismatch. Required: '1' Found: 'number'">anyNumber</error>
assignedImplicitFieldChildDocField.value = anyNumber

one = docFieldChildDocField.value
one = tableExplicitFieldChildDocField.value
one = <error descr="Type mismatch. Required: '1' Found: 'number'">tableImplicitFieldChildDocField.value</error>
one = assignedExplicitFieldChildDocField.value
one = <error descr="Type mismatch. Required: '1' Found: 'number'">assignedImplicitFieldChildDocField.value</error>

one = docFieldChildDocField.method()
one = tableExplicitFieldChildDocField.method()
one = <error descr="Type mismatch. Required: '1' Found: 'number'">tableImplicitFieldChildDocField.method()</error>
one = assignedExplicitFieldChildDocField.method()
one = <error descr="Type mismatch. Required: '1' Found: 'number'">assignedImplicitFieldChildDocField.method()</error>
one = declaredExplicitMethodChildDocField.method()
one = <error descr="Type mismatch. Required: '1' Found: 'number'">declaredImplicitMethodChildDocField.method()</error>



---@class DocFieldChildTableExplicitField : TableExplicitField
---@field value 1
---@field method fun(): 1

---@class TableExplicitFieldChildTableExplicitField : TableExplicitField
local TableExplicitFieldChildTableExplicitField = {
    ---@type 1
    value = 1,
    ---@type fun(): 1
    method = function()
        return 1
    end
}

---@class TableImplicitFieldChildTableExplicitField : TableExplicitField
local TableImplicitFieldChildTableExplicitField = {
    value = 1,
    method = function()
        return 1
    end
}

---@class AssignedExplicitFieldChildTableExplicitField : TableExplicitField
local AssignedExplicitFieldChildTableExplicitField = {}

---@type 1
AssignedExplicitFieldChildTableExplicitField.value = 1

---@type fun(): 1
AssignedExplicitFieldChildTableExplicitField.method = function()
    return one
end

---@class AssignedImplicitFieldChildTableExplicitField : TableExplicitField
local AssignedImplicitFieldChildTableExplicitField = {}

AssignedImplicitFieldChildTableExplicitField.value = one
AssignedImplicitFieldChildTableExplicitField.method = function()
    return one
end

---@class DeclaredExplicitMethodChildTableExplicitField : TableExplicitField
local DeclaredExplicitMethodChildTableExplicitField = {}

---@return 1
function DeclaredExplicitMethodChildTableExplicitField.method()
    return one
end

---@class DeclaredImplicitMethodChildTableExplicitField : TableExplicitField
local DeclaredImplicitMethodChildTableExplicitField = {}

function DeclaredImplicitMethodChildTableExplicitField.method()
    return one
end


---@type DocFieldChildTableExplicitField
local docFieldChildTableExplicitField

---@type TableExplicitFieldChildTableExplicitField
local tableExplicitFieldChildTableExplicitField

---@type TableImplicitFieldChildTableExplicitField
local tableImplicitFieldChildTableExplicitField

---@type AssignedExplicitFieldChildTableExplicitField
local assignedExplicitFieldChildTableExplicitField

---@type AssignedImplicitFieldChildTableExplicitField
local assignedImplicitFieldChildTableExplicitField

---@type DeclaredExplicitMethodChildTableExplicitField
local declaredExplicitMethodChildTableExplicitField

---@type DeclaredImplicitMethodChildTableExplicitField
local declaredImplicitMethodChildTableExplicitField


docFieldChildTableExplicitField.value = <error descr="Type mismatch. Required: '1' Found: 'number'">anyNumber</error>
tableExplicitFieldChildTableExplicitField.value = <error descr="Type mismatch. Required: '1' Found: 'number'">anyNumber</error>
tableImplicitFieldChildTableExplicitField.value = anyNumber
assignedExplicitFieldChildTableExplicitField.value = <error descr="Type mismatch. Required: '1' Found: 'number'">anyNumber</error>
assignedImplicitFieldChildTableExplicitField.value = anyNumber

one = docFieldChildTableExplicitField.value
one = tableExplicitFieldChildTableExplicitField.value
one = <error descr="Type mismatch. Required: '1' Found: 'number'">tableImplicitFieldChildTableExplicitField.value</error>
one = assignedExplicitFieldChildTableExplicitField.value
one = <error descr="Type mismatch. Required: '1' Found: 'number'">assignedImplicitFieldChildTableExplicitField.value</error>

one = docFieldChildTableExplicitField.method()
one = tableExplicitFieldChildTableExplicitField.method()
one = <error descr="Type mismatch. Required: '1' Found: 'number'">tableImplicitFieldChildTableExplicitField.method()</error>
one = assignedExplicitFieldChildTableExplicitField.method()
one = <error descr="Type mismatch. Required: '1' Found: 'number'">assignedImplicitFieldChildTableExplicitField.method()</error>
one = declaredExplicitMethodChildTableExplicitField.method()
one = <error descr="Type mismatch. Required: '1' Found: 'number'">declaredImplicitMethodChildTableExplicitField.method()</error>



---@class DocFieldChildTableImplicitField : TableImplicitField
---@field value 1
---@field method fun(): 1

---@class TableExplicitFieldChildTableImplicitField : TableImplicitField
local TableExplicitFieldChildTableImplicitField = {
    ---@type 1
    value = 1,
    ---@type fun(): 1
    method = function()
        return 1
    end
}

---@class TableImplicitFieldChildTableImplicitField : TableImplicitField
local TableImplicitFieldChildTableImplicitField = {
    value = 1,
    method = function()
        return 1
    end
}

---@class AssignedExplicitFieldChildTableImplicitField : TableImplicitField
local AssignedExplicitFieldChildTableImplicitField = {}

---@type 1
AssignedExplicitFieldChildTableImplicitField.value = 1

---@type fun(): 1
AssignedExplicitFieldChildTableImplicitField.method = function()
    return one
end

---@class AssignedImplicitFieldChildTableImplicitField : TableImplicitField
local AssignedImplicitFieldChildTableImplicitField = {}

AssignedImplicitFieldChildTableImplicitField.value = one
AssignedImplicitFieldChildTableImplicitField.method = function()
    return one
end

---@class DeclaredExplicitMethodChildTableImplicitField : TableImplicitField
local DeclaredExplicitMethodChildTableImplicitField = {}

---@return 1
function DeclaredExplicitMethodChildTableImplicitField.method()
    return one
end

---@class DeclaredImplicitMethodChildTableImplicitField : TableImplicitField
local DeclaredImplicitMethodChildTableImplicitField = {}

function DeclaredImplicitMethodChildTableImplicitField.method()
    return one
end


---@type DocFieldChildTableImplicitField
local docFieldChildTableImplicitField

---@type TableExplicitFieldChildTableImplicitField
local tableExplicitFieldChildTableImplicitField

---@type TableImplicitFieldChildTableImplicitField
local tableImplicitFieldChildTableImplicitField

---@type AssignedExplicitFieldChildTableImplicitField
local assignedExplicitFieldChildTableImplicitField

---@type AssignedImplicitFieldChildTableImplicitField
local assignedImplicitFieldChildTableImplicitField

---@type DeclaredExplicitMethodChildTableImplicitField
local declaredExplicitMethodChildTableImplicitField

---@type DeclaredImplicitMethodChildTableImplicitField
local declaredImplicitMethodChildTableImplicitField


docFieldChildTableImplicitField.value = <error descr="Type mismatch. Required: '1' Found: 'number'">anyNumber</error>
tableExplicitFieldChildTableImplicitField.value = <error descr="Type mismatch. Required: '1' Found: 'number'">anyNumber</error>
tableImplicitFieldChildTableImplicitField.value = <error descr="Type mismatch. Required: '1' Found: 'number'">anyNumber</error>
assignedExplicitFieldChildTableImplicitField.value = <error descr="Type mismatch. Required: '1' Found: 'number'">anyNumber</error>
assignedImplicitFieldChildTableImplicitField.value = <error descr="Type mismatch. Required: '1' Found: 'number'">anyNumber</error>

one = docFieldChildTableImplicitField.value
one = tableExplicitFieldChildTableImplicitField.value
one = tableImplicitFieldChildTableImplicitField.value
one = assignedExplicitFieldChildTableImplicitField.value
one = assignedImplicitFieldChildTableImplicitField.value

one = docFieldChildTableImplicitField.method()
one = tableExplicitFieldChildTableImplicitField.method()
one = tableImplicitFieldChildTableImplicitField.method()
one = assignedExplicitFieldChildTableImplicitField.method()
one = assignedImplicitFieldChildTableImplicitField.method()
one = declaredExplicitMethodChildTableImplicitField.method()
one = declaredImplicitMethodChildTableImplicitField.method()



---@class DocFieldChildAssignedExplicitField : AssignedExplicitField
---@field value 1
---@field method fun(): 1

---@class TableExplicitFieldChildAssignedExplicitField : AssignedExplicitField
local TableExplicitFieldChildAssignedExplicitField = {
    ---@type 1
    value = 1,
    ---@type fun(): 1
    method = function()
        return 1
    end
}

---@class TableImplicitFieldChildAssignedExplicitField : AssignedExplicitField
local TableImplicitFieldChildAssignedExplicitField = {
    value = 1,
    method = function()
        return 1
    end
}

---@class AssignedExplicitFieldChildAssignedExplicitField : AssignedExplicitField
local AssignedExplicitFieldChildAssignedExplicitField = {}

---@type 1
AssignedExplicitFieldChildAssignedExplicitField.value = 1

---@type fun(): 1
AssignedExplicitFieldChildAssignedExplicitField.method = function()
    return one
end

---@class AssignedImplicitFieldChildAssignedExplicitField : AssignedExplicitField
local AssignedImplicitFieldChildAssignedExplicitField = {}

AssignedImplicitFieldChildAssignedExplicitField.value = one
AssignedImplicitFieldChildAssignedExplicitField.method = function()
    return one
end

---@class DeclaredExplicitMethodChildAssignedExplicitField : AssignedExplicitField
local DeclaredExplicitMethodChildAssignedExplicitField = {}

---@return 1
function DeclaredExplicitMethodChildAssignedExplicitField.method()
    return one
end

---@class DeclaredImplicitMethodChildAssignedExplicitField : AssignedExplicitField
local DeclaredImplicitMethodChildAssignedExplicitField = {}

function DeclaredImplicitMethodChildAssignedExplicitField.method()
    return one
end


---@type DocFieldChildAssignedExplicitField
local docFieldChildAssignedExplicitField

---@type TableExplicitFieldChildAssignedExplicitField
local tableExplicitFieldChildAssignedExplicitField

---@type TableImplicitFieldChildAssignedExplicitField
local tableImplicitFieldChildAssignedExplicitField

---@type AssignedExplicitFieldChildAssignedExplicitField
local assignedExplicitFieldChildAssignedExplicitField

---@type AssignedImplicitFieldChildAssignedExplicitField
local assignedImplicitFieldChildAssignedExplicitField

---@type DeclaredExplicitMethodChildAssignedExplicitField
local declaredExplicitMethodChildAssignedExplicitField

---@type DeclaredImplicitMethodChildAssignedExplicitField
local declaredImplicitMethodChildAssignedExplicitField


docFieldChildAssignedExplicitField.value = <error descr="Type mismatch. Required: '1' Found: 'number'">anyNumber</error>
tableExplicitFieldChildAssignedExplicitField.value = <error descr="Type mismatch. Required: '1' Found: 'number'">anyNumber</error>
tableImplicitFieldChildAssignedExplicitField.value = anyNumber
assignedExplicitFieldChildAssignedExplicitField.value = <error descr="Type mismatch. Required: '1' Found: 'number'">anyNumber</error>
assignedImplicitFieldChildAssignedExplicitField.value = anyNumber

one = docFieldChildAssignedExplicitField.value
one = tableExplicitFieldChildAssignedExplicitField.value
one = <error descr="Type mismatch. Required: '1' Found: 'number'">tableImplicitFieldChildAssignedExplicitField.value</error>
one = assignedExplicitFieldChildAssignedExplicitField.value
one = <error descr="Type mismatch. Required: '1' Found: 'number'">assignedImplicitFieldChildAssignedExplicitField.value</error>

one = docFieldChildAssignedExplicitField.method()
one = tableExplicitFieldChildAssignedExplicitField.method()
one = <error descr="Type mismatch. Required: '1' Found: 'number'">tableImplicitFieldChildAssignedExplicitField.method()</error>
one = assignedExplicitFieldChildAssignedExplicitField.method()
one = <error descr="Type mismatch. Required: '1' Found: 'number'">assignedImplicitFieldChildAssignedExplicitField.method()</error>
one = declaredExplicitMethodChildAssignedExplicitField.method()
one = <error descr="Type mismatch. Required: '1' Found: 'number'">declaredImplicitMethodChildAssignedExplicitField.method()</error>



---@class DocFieldChildAssignedImplicitField : AssignedImplicitField
---@field value 1
---@field method fun(): 1

---@class TableExplicitFieldChildAssignedImplicitField : AssignedImplicitField
local TableExplicitFieldChildAssignedImplicitField = {
    ---@type 1
    value = 1,
    ---@type fun(): 1
    method = function()
        return 1
    end
}

---@class TableImplicitFieldChildAssignedImplicitField : AssignedImplicitField
local TableImplicitFieldChildAssignedImplicitField = {
    value = 1,
    method = function()
        return 1
    end
}

---@class AssignedExplicitFieldChildAssignedImplicitField : AssignedImplicitField
local AssignedExplicitFieldChildAssignedImplicitField = {}

---@type 1
AssignedExplicitFieldChildAssignedImplicitField.value = 1

---@type fun(): 1
AssignedExplicitFieldChildAssignedImplicitField.method = function()
    return one
end

---@class AssignedImplicitFieldChildAssignedImplicitField : AssignedImplicitField
local AssignedImplicitFieldChildAssignedImplicitField = {}

AssignedImplicitFieldChildAssignedImplicitField.value = one
AssignedImplicitFieldChildAssignedImplicitField.method = function()
    return one
end

---@class DeclaredExplicitMethodChildAssignedImplicitField : AssignedImplicitField
local DeclaredExplicitMethodChildAssignedImplicitField = {}

---@return 1
function DeclaredExplicitMethodChildAssignedImplicitField.method()
    return one
end

---@class DeclaredImplicitMethodChildAssignedImplicitField : AssignedImplicitField
local DeclaredImplicitMethodChildAssignedImplicitField = {}

function DeclaredImplicitMethodChildAssignedImplicitField.method()
    return one
end


---@type DocFieldChildAssignedImplicitField
local docFieldChildAssignedImplicitField

---@type TableExplicitFieldChildAssignedImplicitField
local tableExplicitFieldChildAssignedImplicitField

---@type TableImplicitFieldChildAssignedImplicitField
local tableImplicitFieldChildAssignedImplicitField

---@type AssignedExplicitFieldChildAssignedImplicitField
local assignedExplicitFieldChildAssignedImplicitField

---@type AssignedImplicitFieldChildAssignedImplicitField
local assignedImplicitFieldChildAssignedImplicitField

---@type DeclaredExplicitMethodChildAssignedImplicitField
local declaredExplicitMethodChildAssignedImplicitField

---@type DeclaredImplicitMethodChildAssignedImplicitField
local declaredImplicitMethodChildAssignedImplicitField


docFieldChildAssignedImplicitField.value = <error descr="Type mismatch. Required: '1' Found: 'number'">anyNumber</error>
tableExplicitFieldChildAssignedImplicitField.value = <error descr="Type mismatch. Required: '1' Found: 'number'">anyNumber</error>
tableImplicitFieldChildAssignedImplicitField.value = <error descr="Type mismatch. Required: '1' Found: 'number'">anyNumber</error>
assignedExplicitFieldChildAssignedImplicitField.value = <error descr="Type mismatch. Required: '1' Found: 'number'">anyNumber</error>
assignedImplicitFieldChildAssignedImplicitField.value = <error descr="Type mismatch. Required: '1' Found: 'number'">anyNumber</error>

one = docFieldChildAssignedImplicitField.value
one = tableExplicitFieldChildAssignedImplicitField.value
one = tableImplicitFieldChildAssignedImplicitField.value
one = assignedExplicitFieldChildAssignedImplicitField.value
one = assignedImplicitFieldChildAssignedImplicitField.value

one = docFieldChildAssignedImplicitField.method()
one = tableExplicitFieldChildAssignedImplicitField.method()
one = tableImplicitFieldChildAssignedImplicitField.method()
one = tableImplicitFieldChildAssignedImplicitField.method()
one = assignedImplicitFieldChildAssignedImplicitField.method()
one = declaredExplicitMethodChildAssignedImplicitField.method()
one = declaredImplicitMethodChildAssignedImplicitField.method()



---@class DeclaredExplicitMethodChildDeclaredExplicitMethod : DeclaredExplicitMethod
local DeclaredExplicitMethodChildDeclaredExplicitMethod = {}

---@return 1
function DeclaredExplicitMethodChildDeclaredExplicitMethod.method()
    return one
end

---@class DeclaredImplicitMethodChildDeclaredExplicitMethod : DeclaredExplicitMethod
local DeclaredImplicitMethodChildDeclaredExplicitMethod = {}

function DeclaredImplicitMethodChildDeclaredExplicitMethod.method()
    return one
end


---@type DeclaredExplicitMethodChildDeclaredExplicitMethod
local declaredExplicitMethodChildDeclaredExplicitMethod

---@type DeclaredImplicitMethodChildDeclaredExplicitMethod
local declaredImplicitMethodChildDeclaredExplicitMethod


one = declaredExplicitMethodChildDeclaredExplicitMethod.method()
one = <error descr="Type mismatch. Required: '1' Found: 'number'">declaredImplicitMethodChildDeclaredExplicitMethod.method()</error>



---@class DeclaredExplicitMethodChildDeclaredImplicitMethod : DeclaredImplicitMethod
local DeclaredExplicitMethodChildDeclaredImplicitMethod = {}

---@return 1
function DeclaredExplicitMethodChildDeclaredImplicitMethod.method()
    return one
end

---@class DeclaredImplicitMethodChildDeclaredImplicitMethod : DeclaredImplicitMethod
local DeclaredImplicitMethodChildDeclaredImplicitMethod = {}

function DeclaredImplicitMethodChildDeclaredImplicitMethod.method()
    return one
end


---@type DeclaredExplicitMethodChildDeclaredImplicitMethod
local declaredExplicitMethodChildDeclaredImplicitMethod

---@type DeclaredImplicitMethodChildDeclaredImplicitMethod
local declaredImplicitMethodChildDeclaredImplicitMethod


one = declaredExplicitMethodChildDeclaredImplicitMethod.method()
one = declaredImplicitMethodChildDeclaredImplicitMethod.method()



---@class IllegalDocFieldChildDocField : DocField
---@field value <error descr="Illegal override of \"value\". Type mismatch. Required: 'number' Found: 'string'">string</error>
---@field method <error descr="Illegal override of \"method\". Type mismatch. Required: 'fun(): number' Found: 'fun(): string'">fun(): string</error>

---@class IllegalTableExplicitFieldChildDocField : DocField
local IllegalTableExplicitFieldChildDocField = {
    ---@type <error descr="Illegal override of \"value\". Type mismatch. Required: 'number' Found: 'string'">string</error>
    value = "",
    ---@type <error descr="Illegal override of \"method\". Type mismatch. Required: 'fun(): number' Found: 'fun(): string'">fun(): string</error>
    method = function()
        return anyString
    end,
}

---@class IllegalTableImplicitFieldChildDocField : DocField
local IllegalTableImplicitFieldChildDocField = {
    value = <error descr="Illegal override of \"value\". Type mismatch. Required: 'number' Found: 'string'">anyString</error>,
    method = <error descr="Illegal override of \"method\". Type mismatch. Required: 'fun(): number' Found: 'fun(): string'">function()
        return anyString
    end</error>,
}

---@class IllegalAssignedImplicitFieldChildDocField : DocField
local IllegalAssignedImplicitFieldChildDocField = {}

IllegalAssignedImplicitFieldChildDocField.value = <error descr="Type mismatch. Required: 'number' Found: 'string'">anyString</error>
IllegalAssignedImplicitFieldChildDocField.method = <error descr="Type mismatch. Required: 'fun(): number' Found: 'fun(): string'">function()
    return anyString
end</error>

---@class IllegalDeclaredExplicitMethodChildDocField : DocField
local IllegalDeclaredExplicitMethodChildDocField = {}

<error descr="Illegal override of \"method\". Type mismatch. Required: 'fun(): number' Found: 'fun(): string'">---@return string
function IllegalDeclaredExplicitMethodChildDocField.method()
    return anyString
end</error>

---@class IllegalDeclaredImplicitMethodChildDocField : DocField
local IllegalDeclaredImplicitMethodChildDocField = {}

<error descr="Illegal override of \"method\". Type mismatch. Required: 'fun(): number' Found: 'fun(): string'">function IllegalDeclaredImplicitMethodChildDocField.method()
    return anyString
end</error>


---@class LegalMultipleResultsOverride : DocField
local LegalMultipleResultsOverride = {}

function LegalMultipleResultsOverride.multipleResultsMethod() return 1, true end


---@class IllegalMultipleResultsOverride : DocField
local IllegalMultipleResultsOverride = {}

<error descr="Illegal override of \"multipleResultsMethod\". Type mismatch. Required: 'fun(): number, boolean' Found: 'fun(): true, 1'">function IllegalMultipleResultsOverride.multipleResultsMethod() return true, 1 end</error>
