---@class Unconstrained<T>
local Unconstrained = {}

---@class NumberConstrained<T : number>
local NumberConstrained = {}

---@class ChildNumberConstrained<T : number> : NumberConstrained<T>
local ChildNumberConstrained = {}

---@class InvalidNumberConstrained1 : NumberConstrained<<error descr="Type mismatch. Required: 'T : number' Found: 'string'">string</error>>
local InvalidNumberConstrained1 = {}

---@class InvalidNumberConstrained2<T> : NumberConstrained<<error descr="Type mismatch. Required: 'T : number' Found: 'T'">T</error>>
local InvalidNumberConstrained2 = {}

---@type Unconstrained<string>
local a

---@type Unconstrained<number>
local b

---@type NumberConstrained<number>
local c

---@type NumberConstrained<<error descr="Type mismatch. Required: 'T : number' Found: 'string'">string</error>>
local d

---@type ChildNumberConstrained<number>
local e

---@type ChildNumberConstrained<<error descr="Type mismatch. Required: 'T : number' Found: 'string'">string</error>>
local f
