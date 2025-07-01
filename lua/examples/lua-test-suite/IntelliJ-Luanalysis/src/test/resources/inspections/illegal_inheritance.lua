---@<error descr="Illegal self inheritance">class SelfInheritance : SelfInheritance</error>

---@<error descr="Illegal cyclical inheritance from CyclicInheritance2">class CyclicInheritance1 : CyclicInheritance3</error>

---@<error descr="Illegal cyclical inheritance from CyclicInheritance3">class CyclicInheritance2 : CyclicInheritance1</error>

---@<error descr="Illegal cyclical inheritance from CyclicInheritance1">class CyclicInheritance3 : CyclicInheritance2</error>

---@<error descr="Illegal inheritance from primitive type">class PrimitiveInheritance : string</error>

---@<error descr="Illegal inheritance from primitive type">class TableInheritance : table</error>

---@<error descr="Illegal inheritance from primitive type">class GenericTableInheritance : table<string, string></error>

---@<error descr="Illegal inheritance from primitive type">class NumberInheritance : 1</error>

---@alias AliasedNumber 1

---@<error descr="Illegal inheritance from primitive type">class AliasedNumberInheritance : AliasedNumber</error>
