---@class GenericA<A>
local GenericA

---@class GenericB<B> : GenericA<B>
local GenericB

---@class GenericC<C> : GenericB<C>s
local GenericC

---@type GenericA<string>
local genericA

---@type GenericB<string>
local genericB

---@type GenericC<string>
local genericC

genericA = genericB
genericA = genericC

genericB = <error descr="Type mismatch. Required: 'GenericB<string>' Found: 'GenericA<string>'">genericA</error>
genericB = genericC

genericC = <error descr="Type mismatch. Required: 'GenericC<string>' Found: 'GenericA<string>'">genericA</error>
genericC = <error descr="Type mismatch. Required: 'GenericC<string>' Found: 'GenericB<string>'">genericB</error>


---@class ClassA : GenericA<string>
local ClassA

---@class ClassB : ClassA
local ClassB

---@class ClassC : ClassB
local ClassC

---@type ClassA
local classA

---@type ClassB
local classB

---@type ClassC
local classC

classA = classB
classA = classC

classB = <error descr="Type mismatch. Required: 'ClassB' Found: 'ClassA'">classA</error>
classB = classC

classC = <error descr="Type mismatch. Required: 'ClassC' Found: 'ClassA'">classA</error>
classC = <error descr="Type mismatch. Required: 'ClassC' Found: 'ClassB'">classB</error>

genericA = classA
genericA = classB
genericA = classC


---@class ClassGenA<T>
local ClassGenA

---@class ClassGenB<T> : ClassGenA<string>
local ClassGenB

---@class ClassGenC<T> : ClassGenB<string>
local ClassGenC

---@type ClassGenA<string>
local classGenA

---@type ClassGenB<string>
local classGenB

---@type ClassGenC<string>
local classGenC

classGenA = classGenB
classGenA = classGenC

classGenB = <error descr="Type mismatch. Required: 'ClassGenB<string>' Found: 'ClassGenA<string>'">classGenA</error>
classGenB = classGenC

classGenC = <error descr="Type mismatch. Required: 'ClassGenC<string>' Found: 'ClassGenA<string>'">classGenA</error>
classGenC = <error descr="Type mismatch. Required: 'ClassGenC<string>' Found: 'ClassGenB<string>'">classGenB</error>
