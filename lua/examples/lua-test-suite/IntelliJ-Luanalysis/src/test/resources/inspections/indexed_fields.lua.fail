---@type number
local aNumber

---@type boolean
local aBoolean

---@class Vector
---@field x number
---@field y number
---@field z number
---@field [1] number
---@field [2] number
---@field [3] number
---@field [number] boolean

---@type Vector
local vector

aNumber = vector.x
aNumber = vector.y
aNumber = vector.z

aBoolean = <error descr="Type mismatch. Required: 'boolean' Found: 'number'">vector.x</error>
aBoolean = <error descr="Type mismatch. Required: 'boolean' Found: 'number'">vector.y</error>
aBoolean = <error descr="Type mismatch. Required: 'boolean' Found: 'number'">vector.z</error>

aNumber = vector['x']
aNumber = vector['y']
aNumber = vector['z']

aBoolean = <error descr="Type mismatch. Required: 'boolean' Found: 'number'">vector['x']</error>
aBoolean = <error descr="Type mismatch. Required: 'boolean' Found: 'number'">vector['y']</error>
aBoolean = <error descr="Type mismatch. Required: 'boolean' Found: 'number'">vector['z']</error>

aNumber = vector[1]
aNumber = vector[2]
aNumber = vector[3]
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'boolean'">vector[4]</error>

aBoolean = <error descr="Type mismatch. Required: 'boolean' Found: 'number'">vector[1]</error>
aBoolean = <error descr="Type mismatch. Required: 'boolean' Found: 'number'">vector[2]</error>
aBoolean = <error descr="Type mismatch. Required: 'boolean' Found: 'number'">vector[3]</error>
aBoolean = vector[4]

aBoolean = <error descr="No such indexer '[true]' found on type 'Vector'">vector[true]</error>

---@type {[number]: boolean}
local tableField

aBoolean = tableField[1]
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'boolean'">tableField[1]</error>

local anonymousClassTable = {}
anonymousClassTable[1] = 1
anonymousClassTable.a = <error descr="Unknown function 'Undeclared'."><warning descr="Undeclared variable 'Undeclared'.">Undeclared</warning>()</error>

aBoolean = <error descr="Type mismatch. Required: 'boolean' Found: '1'">anonymousClassTable[1]</error>
aNumber = anonymousClassTable[1]

local literalTable = {[1] = true, [2] = 1}

aBoolean = literalTable[1]
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'true'">literalTable[1]</error>

aBoolean = <error descr="Type mismatch. Required: 'boolean' Found: '1'">literalTable[2]</error>
aNumber = literalTable[2]

---@alias OneOrTwo 1|2
---@alias OneOrFour 1|4

---@type OneOrTwo
local oneOrTwo

---@type OneOrFour
local oneOrFour

aNumber = vector[oneOrTwo]
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'boolean | number'">vector[oneOrFour]</error>


---@alias AOrB 'a' | 'b'

---@type AOrB
local aOrB

---@shape UnionIndexer
---@field [AOrB] boolean
---@field [OneOrTwo] boolean
---@field [3|4] boolean

---@type UnionIndexer
local unionIndexer

aBoolean = unionIndexer.a
aBoolean = unionIndexer['a']
aBoolean = unionIndexer[aOrB]
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'boolean'">unionIndexer.a</error>
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'boolean'">unionIndexer['a']</error>

aBoolean = unionIndexer[1]
aBoolean = unionIndexer[oneOrTwo]
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'boolean'">unionIndexer[1]</error>
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'boolean'">unionIndexer[oneOrTwo]</error>

---@alias ThreeOrFour 3|4

---@type 3|4
local threeOrFour

---@type ThreeOrFour
local aliasThreeOrFour

aBoolean = unionIndexer[3]
aBoolean = unionIndexer[threeOrFour]
aBoolean = unionIndexer[aliasThreeOrFour]
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'boolean'">unionIndexer[3]</error>
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'boolean'">unionIndexer[threeOrFour]</error>
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'boolean'">unionIndexer[aliasThreeOrFour]</error>
