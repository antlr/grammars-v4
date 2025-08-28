-- This file is part of the Roblox luau-polyfill repository and is licensed under MIT License; see LICENSE.txt for details
-- #region Array
-- Array related
local Array = {}
local Object = {}
local Map = {}

type Array<T> = { [number]: T }
type callbackFn<K, V> = (element: V, key: K, map: Map<K, V>) -> ()
type callbackFnWithThisArg<K, V> = (thisArg: Object, value: V, key: K, map: Map<K, V>) -> ()
type Map<K, V> = {
	size: number,
	-- method definitions
	set: (self: Map<K, V>, K, V) -> Map<K, V>,
	get: (self: Map<K, V>, K) -> V | nil,
	clear: (self: Map<K, V>) -> (),
	delete: (self: Map<K, V>, K) -> boolean,
	forEach: (self: Map<K, V>, callback: callbackFn<K, V> | callbackFnWithThisArg<K, V>, thisArg: Object?) -> (),
	has: (self: Map<K, V>, K) -> boolean,
	keys: (self: Map<K, V>) -> Array<K>,
	values: (self: Map<K, V>) -> Array<V>,
	entries: (self: Map<K, V>) -> Array<Tuple<K, V>>,
	ipairs: (self: Map<K, V>) -> any,
	[K]: V,
	_map: { [K]: V },
	_array: { [number]: K },
}
type mapFn<T, U> = (element: T, index: number) -> U
type mapFnWithThisArg<T, U> = (thisArg: any, element: T, index: number) -> U
type Object = { [string]: any }
type Table<T, V> = { [T]: V }
type Tuple<T, V> = Array<T | V>

local Set = {}

-- #region Array
function Array.isArray(value: any): boolean
	if typeof(value) ~= "table" then
		return false
	end
	if next(value) == nil then
		-- an empty table is an empty array
		return true
	end

	local length = #value

	if length == 0 then
		return false
	end

	local count = 0
	local sum = 0
	for key in pairs(value) do
		if typeof(key) ~= "number" then
			return false
		end
		if key % 1 ~= 0 or key < 1 then
			return false
		end
		count += 1
		sum += key
	end

	return sum == (count * (count + 1) / 2)
end

function Array.from<T, U>(
	value: string | Array<T> | Object,
	mapFn: (mapFn<T, U> | mapFnWithThisArg<T, U>)?,
	thisArg: Object?
): Array<U>
	if value == nil then
		error("cannot create array from a nil value")
	end
	local valueType = typeof(value)

	local array = {}

	if valueType == "table" and Array.isArray(value) then
		if mapFn then
			for i = 1, #(value :: Array<T>) do
				if thisArg ~= nil then
					array[i] = (mapFn :: mapFnWithThisArg<T, U>)(thisArg, (value :: Array<T>)[i], i)
				else
					array[i] = (mapFn :: mapFn<T, U>)((value :: Array<T>)[i], i)
				end
			end
		else
			for i = 1, #(value :: Array<T>) do
				array[i] = (value :: Array<any>)[i]
			end
		end
	elseif instanceOf(value, Set) then
		if mapFn then
			for i, v in (value :: any):ipairs() do
				if thisArg ~= nil then
					array[i] = (mapFn :: mapFnWithThisArg<T, U>)(thisArg, v, i)
				else
					array[i] = (mapFn :: mapFn<T, U>)(v, i)
				end
			end
		else
			for i, v in (value :: any):ipairs() do
				array[i] = v
			end
		end
	elseif instanceOf(value, Map) then
		if mapFn then
			for i, v in (value :: any):ipairs() do
				if thisArg ~= nil then
					array[i] = (mapFn :: mapFnWithThisArg<T, U>)(thisArg, v, i)
				else
					array[i] = (mapFn :: mapFn<T, U>)(v, i)
				end
			end
		else
			for i, v in (value :: any):ipairs() do
				array[i] = v
			end
		end
	elseif valueType == "string" then
		if mapFn then
			for i = 1, (value :: string):len() do
				if thisArg ~= nil then
					array[i] = (mapFn :: mapFnWithThisArg<T, U>)(thisArg, (value :: any):sub(i, i), i)
				else
					array[i] = (mapFn :: mapFn<T, U>)((value :: any):sub(i, i), i)
				end
			end
		else
			for i = 1, (value :: string):len() do
				array[i] = (value :: any):sub(i, i)
			end
		end
	end

	return array
end

type callbackFnArrayMap<T, U> = (element: T, index: number, array: Array<T>) -> U
type callbackFnWithThisArgArrayMap<T, U, V> = (thisArg: V, element: T, index: number, array: Array<T>) -> U

-- Implements Javascript's `Array.prototype.map` as defined below
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/map
function Array.map<T, U, V>(
	t: Array<T>,
	callback: callbackFnArrayMap<T, U> | callbackFnWithThisArgArrayMap<T, U, V>,
	thisArg: V?
): Array<U>
	if typeof(t) ~= "table" then
		error(string.format("Array.map called on %s", typeof(t)))
	end
	if typeof(callback) ~= "function" then
		error("callback is not a function")
	end

	local len = #t
	local A = {}
	local k = 1

	while k <= len do
		local kValue = t[k]

		if kValue ~= nil then
			local mappedValue

			if thisArg ~= nil then
				mappedValue = (callback :: callbackFnWithThisArgArrayMap<T, U, V>)(thisArg, kValue, k, t)
			else
				mappedValue = (callback :: callbackFnArrayMap<T, U>)(kValue, k, t)
			end

			A[k] = mappedValue
		end
		k += 1
	end

	return A
end

type Function = (any, any, number, any) -> any

-- Implements Javascript's `Array.prototype.reduce` as defined below
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/reduce
function Array.reduce<T>(array: Array<T>, callback: Function, initialValue: any?): any
	if typeof(array) ~= "table" then
		error(string.format("Array.reduce called on %s", typeof(array)))
	end
	if typeof(callback) ~= "function" then
		error("callback is not a function")
	end

	local length = #array

	local value
	local initial = 1

	if initialValue ~= nil then
		value = initialValue
	else
		initial = 2
		if length == 0 then
			error("reduce of empty array with no initial value")
		end
		value = array[1]
	end

	for i = initial, length do
		value = callback(value, array[i], i, array)
	end

	return value
end

type callbackFnArrayForEach<T> = (element: T, index: number, array: Array<T>) -> ()
type callbackFnWithThisArgArrayForEach<T, U> = (thisArg: U, element: T, index: number, array: Array<T>) -> ()

-- Implements Javascript's `Array.prototype.forEach` as defined below
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/forEach
function Array.forEach<T, U>(
	t: Array<T>,
	callback: callbackFnArrayForEach<T> | callbackFnWithThisArgArrayForEach<T, U>,
	thisArg: U?
): ()
	if typeof(t) ~= "table" then
		error(string.format("Array.forEach called on %s", typeof(t)))
	end
	if typeof(callback) ~= "function" then
		error("callback is not a function")
	end

	local len = #t
	local k = 1

	while k <= len do
		local kValue = t[k]

		if thisArg ~= nil then
			(callback :: callbackFnWithThisArgArrayForEach<T, U>)(thisArg, kValue, k, t)
		else
			(callback :: callbackFnArrayForEach<T>)(kValue, k, t)
		end

		if #t < len then
			-- don't iterate on removed items, don't iterate more than original length
			len = #t
		end
		k += 1
	end
end
-- #endregion

-- #region Set
Set.__index = Set

type callbackFnSet<T> = (value: T, key: T, set: Set<T>) -> ()
type callbackFnWithThisArgSet<T> = (thisArg: Object, value: T, key: T, set: Set<T>) -> ()

export type Set<T> = {
	size: number,
	-- method definitions
	add: (self: Set<T>, T) -> Set<T>,
	clear: (self: Set<T>) -> (),
	delete: (self: Set<T>, T) -> boolean,
	forEach: (self: Set<T>, callback: callbackFnSet<T> | callbackFnWithThisArgSet<T>, thisArg: Object?) -> (),
	has: (self: Set<T>, T) -> boolean,
	ipairs: (self: Set<T>) -> any,
}

type Iterable = { ipairs: (any) -> any }

function Set.new<T>(iterable: Array<T> | Set<T> | Iterable | string | nil): Set<T>
	local array = {}
	local map = {}
	if iterable ~= nil then
		local arrayIterable: Array<any>
		-- ROBLOX TODO: remove type casting from (iterable :: any).ipairs in next release
		if typeof(iterable) == "table" then
			if Array.isArray(iterable) then
				arrayIterable = Array.from(iterable :: Array<any>)
			elseif typeof((iterable :: Iterable).ipairs) == "function" then
				-- handle in loop below
			elseif _G.__DEV__ then
				error("cannot create array from an object-like table")
			end
		elseif typeof(iterable) == "string" then
			arrayIterable = Array.from(iterable :: string)
		else
			error(("cannot create array from value of type `%s`"):format(typeof(iterable)))
		end

		if arrayIterable then
			for _, element in ipairs(arrayIterable) do
				if not map[element] then
					map[element] = true
					table.insert(array, element)
				end
			end
		elseif typeof(iterable) == "table" and typeof((iterable :: Iterable).ipairs) == "function" then
			for _, element in (iterable :: Iterable):ipairs() do
				if not map[element] then
					map[element] = true
					table.insert(array, element)
				end
			end
		end
	end

	return (setmetatable({
		size = #array,
		_map = map,
		_array = array,
	}, Set) :: any) :: Set<T>
end

function Set:add(value)
	if not self._map[value] then
		-- Luau FIXME: analyze should know self is Set<T> which includes size as a number
		self.size = self.size :: number + 1
		self._map[value] = true
		table.insert(self._array, value)
	end
	return self
end

function Set:clear()
	self.size = 0
	table.clear(self._map)
	table.clear(self._array)
end

function Set:delete(value): boolean
	if not self._map[value] then
		return false
	end
	-- Luau FIXME: analyze should know self is Map<K, V> which includes size as a number
	self.size = self.size :: number - 1
	self._map[value] = nil
	local index = table.find(self._array, value)
	if index then
		table.remove(self._array, index)
	end
	return true
end

-- Implements Javascript's `Map.prototype.forEach` as defined below
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/forEach
function Set:forEach<T>(callback: callbackFnSet<T> | callbackFnWithThisArgSet<T>, thisArg: Object?): ()
	if typeof(callback) ~= "function" then
		error("callback is not a function")
	end

	return Array.forEach(self._array, function(value: T)
		if thisArg ~= nil then
			(callback :: callbackFnWithThisArgSet<T>)(thisArg, value, value, self)
		else
			(callback :: callbackFnSet<T>)(value, value, self)
		end
	end)
end

function Set:has(value): boolean
	return self._map[value] ~= nil
end

function Set:ipairs()
	return ipairs(self._array)
end

-- #endregion Set

-- #region Object
function Object.entries(value: string | Object | Array<any>): Array<any>
	assert(value :: any ~= nil, "cannot get entries from a nil value")
	local valueType = typeof(value)

	local entries: Array<Tuple<string, any>> = {}
	if valueType == "table" then
		for key, keyValue in pairs(value :: Object) do
			-- Luau FIXME: Luau should see entries as Array<any>, given object is [string]: any, but it sees it as Array<Array<string>> despite all the manual annotation
			table.insert(entries, { key :: string, keyValue :: any })
		end
	elseif valueType == "string" then
		for i = 1, string.len(value :: string) do
			entries[i] = { tostring(i), string.sub(value :: string, i, i) }
		end
	end

	return entries
end

-- #endregion

-- #region instanceOf

-- ROBLOX note: Typed tbl as any to work with strict type analyze
-- polyfill for https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/instanceof
function instanceOf(tbl: any, class)
	assert(typeof(class) == "table", "Received a non-table as the second argument for instanceof")

	if typeof(tbl) ~= "table" then
		return false
	end

	local ok, hasNew = pcall(function()
		return class.new ~= nil and tbl.new == class.new
	end)
	if ok and hasNew then
		return true
	end

	local seen = { tbl = true }

	while tbl and typeof(tbl) == "table" do
		tbl = getmetatable(tbl)
		if typeof(tbl) == "table" then
			tbl = tbl.__index

			if tbl == class then
				return true
			end
		end

		-- if we still have a valid table then check against seen
		if typeof(tbl) == "table" then
			if seen[tbl] then
				return false
			end
			seen[tbl] = true
		end
	end

	return false
end
-- #endregion

function Map.new<K, V>(iterable: Array<Array<any>>?): Map<K, V>
	local array = {}
	local map = {}
	if iterable ~= nil then
		local arrayFromIterable
		local iterableType = typeof(iterable)
		if iterableType == "table" then
			if #iterable > 0 and typeof(iterable[1]) ~= "table" then
				error("cannot create Map from {K, V} form, it must be { {K, V}... }")
			end

			arrayFromIterable = Array.from(iterable)
		else
			error(("cannot create array from value of type `%s`"):format(iterableType))
		end

		for _, entry in ipairs(arrayFromIterable) do
			local key = entry[1]
			if _G.__DEV__ then
				if key == nil then
					error("cannot create Map from a table that isn't an array.")
				end
			end
			local val = entry[2]
			-- only add to array if new
			if map[key] == nil then
				table.insert(array, key)
			end
			-- always assign
			map[key] = val
		end
	end

	return (setmetatable({
		size = #array,
		_map = map,
		_array = array,
	}, Map) :: any) :: Map<K, V>
end

function Map:set<K, V>(key: K, value: V): Map<K, V>
	-- preserve initial insertion order
	if self._map[key] == nil then
		-- Luau FIXME: analyze should know self is Map<K, V> which includes size as a number
		self.size = self.size :: number + 1
		table.insert(self._array, key)
	end
	-- always update value
	self._map[key] = value
	return self
end

function Map:get(key)
	return self._map[key]
end

function Map:clear()
	local table_: any = table
	self.size = 0
	table_.clear(self._map)
	table_.clear(self._array)
end

function Map:delete(key): boolean
	if self._map[key] == nil then
		return false
	end
	-- Luau FIXME: analyze should know self is Map<K, V> which includes size as a number
	self.size = self.size :: number - 1
	self._map[key] = nil
	local index = table.find(self._array, key)
	if index then
		table.remove(self._array, index)
	end
	return true
end

-- Implements Javascript's `Map.prototype.forEach` as defined below
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map/forEach
function Map:forEach<K, V>(callback: callbackFn<K, V> | callbackFnWithThisArg<K, V>, thisArg: Object?): ()
	if typeof(callback) ~= "function" then
		error("callback is not a function")
	end

	return Array.forEach(self._array, function(key: K)
		local value: V = self._map[key] :: V

		if thisArg ~= nil then
			(callback :: callbackFnWithThisArg<K, V>)(thisArg, value, key, self)
		else
			(callback :: callbackFn<K, V>)(value, key, self)
		end
	end)
end

function Map:has(key): boolean
	return self._map[key] ~= nil
end

function Map:keys()
	return self._array
end

function Map:values()
	return Array.map(self._array, function(key)
		return self._map[key]
	end)
end

function Map:entries()
	return Array.map(self._array, function(key)
		return { key, self._map[key] }
	end)
end

function Map:ipairs()
	return ipairs(self:entries())
end

function Map.__index(self, key)
	local mapProp = rawget(Map, key)
	if mapProp ~= nil then
		return mapProp
	end

	return Map.get(self, key)
end

function Map.__newindex(table_, key, value)
	table_:set(key, value)
end

local function coerceToMap(mapLike: Map<any, any> | Table<any, any>): Map<any, any>
	return instanceOf(mapLike, Map) and mapLike :: Map<any, any> -- ROBLOX: order is preservered
		or Map.new(Object.entries(mapLike)) -- ROBLOX: order is not preserved
end

-- local function coerceToTable(mapLike: Map<any, any> | Table<any, any>): Table<any, any>
-- 	if not instanceOf(mapLike, Map) then
-- 		return mapLike
-- 	end

-- 	-- create table from map
-- 	return Array.reduce(mapLike:entries(), function(tbl, entry)
-- 		tbl[entry[1]] = entry[2]
-- 		return tbl
-- 	end, {})
-- end

-- #region Tests to verify it works as expected
local function it(description: string, fn: () -> ())
	local ok, result = pcall(fn)

	if not ok then
		error("Failed test: " .. description .. "\n" .. result)
	end
end

local AN_ITEM = "bar"
local ANOTHER_ITEM = "baz"

-- #region [Describe] "Map"
-- #region [Child Describe] "constructors"
it("creates an empty array", function()
	local foo = Map.new()
	assert(foo.size == 0)
end)

it("creates a Map from an array", function()
	local foo = Map.new({
		{ AN_ITEM, "foo" },
		{ ANOTHER_ITEM, "val" },
	})
	assert(foo.size == 2)
	assert(foo:has(AN_ITEM) == true)
	assert(foo:has(ANOTHER_ITEM) == true)
end)

it("creates a Map from an array with duplicate keys", function()
	local foo = Map.new({
		{ AN_ITEM, "foo1" },
		{ AN_ITEM, "foo2" },
	})
	assert(foo.size == 1)
	assert(foo:get(AN_ITEM) == "foo2")

	assert(#foo:keys() == 1 and foo:keys()[1] == AN_ITEM)
	assert(#foo:values() == 1 and foo:values()[1] == "foo2")
	assert(#foo:entries() == 1)
	assert(#foo:entries()[1] == 2)

	assert(foo:entries()[1][1] == AN_ITEM)
	assert(foo:entries()[1][2] == "foo2")
end)

it("preserves the order of keys first assignment", function()
	local foo = Map.new({
		{ AN_ITEM, "foo1" },
		{ ANOTHER_ITEM, "bar" },
		{ AN_ITEM, "foo2" },
	})
	assert(foo.size == 2)
	assert(foo:get(AN_ITEM) == "foo2")
	assert(foo:get(ANOTHER_ITEM) == "bar")

	assert(foo:keys()[1] == AN_ITEM)
	assert(foo:keys()[2] == ANOTHER_ITEM)
	assert(foo:values()[1] == "foo2")
	assert(foo:values()[2] == "bar")
	assert(foo:entries()[1][1] == AN_ITEM)
	assert(foo:entries()[1][2] == "foo2")
	assert(foo:entries()[2][1] == ANOTHER_ITEM)
	assert(foo:entries()[2][2] == "bar")
end)
-- #endregion

-- #region [Child Describe] "type"
it("instanceOf return true for an actual Map object", function()
	local foo = Map.new()
	assert(instanceOf(foo, Map) == true)
end)

it("instanceOf return false for an regular plain object", function()
	local foo = {}
	assert(instanceOf(foo, Map) == false)
end)
-- #endregion

-- #region [Child Describe] "set"
it("returns the Map object", function()
	local foo = Map.new()
	assert(foo:set(1, "baz") == foo)
end)

it("increments the size if the element is added for the first time", function()
	local foo = Map.new()
	foo:set(AN_ITEM, "foo")
	assert(foo.size == 1)
end)

it("does not increment the size the second time an element is added", function()
	local foo = Map.new()
	foo:set(AN_ITEM, "foo")
	foo:set(AN_ITEM, "val")
	assert(foo.size == 1)
end)

it("sets values correctly to true/false", function()
	-- Luau FIXME: Luau insists that arrays can't be mixed type
	local foo = Map.new({ { AN_ITEM, false :: any } })
	foo:set(AN_ITEM, false)
	assert(foo.size == 1)
	assert(foo:get(AN_ITEM) == false)

	foo:set(AN_ITEM, true)
	assert(foo.size == 1)
	assert(foo:get(AN_ITEM) == true)

	foo:set(AN_ITEM, false)
	assert(foo.size == 1)
	assert(foo:get(AN_ITEM) == false)
end)

-- #endregion

-- #region [Child Describe] "get"
it("returns value of item from provided key", function()
	local foo = Map.new()
	foo:set(AN_ITEM, "foo")
	assert(foo:get(AN_ITEM) == "foo")
end)

it("returns nil if the item is not in the Map", function()
	local foo = Map.new()
	assert(foo:get(AN_ITEM) == nil)
end)
-- #endregion

-- #region [Child Describe] "clear"
it("sets the size to zero", function()
	local foo = Map.new()
	foo:set(AN_ITEM, "foo")
	foo:clear()
	assert(foo.size == 0)
end)

it("removes the items from the Map", function()
	local foo = Map.new()
	foo:set(AN_ITEM, "foo")
	foo:clear()
	assert(foo:has(AN_ITEM) == false)
end)
-- #endregion

-- #region [Child Describe] "delete"
it("removes the items from the Map", function()
	local foo = Map.new()
	foo:set(AN_ITEM, "foo")
	foo:delete(AN_ITEM)
	assert(foo:has(AN_ITEM) == false)
end)

it("returns true if the item was in the Map", function()
	local foo = Map.new()
	foo:set(AN_ITEM, "foo")
	assert(foo:delete(AN_ITEM) == true)
end)

it("returns false if the item was not in the Map", function()
	local foo = Map.new()
	assert(foo:delete(AN_ITEM) == false)
end)

it("decrements the size if the item was in the Map", function()
	local foo = Map.new()
	foo:set(AN_ITEM, "foo")
	foo:delete(AN_ITEM)
	assert(foo.size == 0)
end)

it("does not decrement the size if the item was not in the Map", function()
	local foo = Map.new()
	foo:set(AN_ITEM, "foo")
	foo:delete(ANOTHER_ITEM)
	assert(foo.size == 1)
end)

it("deletes value set to false", function()
	-- Luau FIXME: Luau insists arrays can't be mixed type
	local foo = Map.new({ { AN_ITEM, false :: any } })

	foo:delete(AN_ITEM)

	assert(foo.size == 0)
	assert(foo:get(AN_ITEM) == nil)
end)
-- #endregion

-- #region [Child Describe] "has"
it("returns true if the item is in the Map", function()
	local foo = Map.new()
	foo:set(AN_ITEM, "foo")
	assert(foo:has(AN_ITEM) == true)
end)

it("returns false if the item is not in the Map", function()
	local foo = Map.new()
	assert(foo:has(AN_ITEM) == false)
end)

it("returns correctly with value set to false", function()
	-- Luau FIXME: Luau insists arrays can't be mixed type
	local foo = Map.new({ { AN_ITEM, false :: any } })

	assert(foo:has(AN_ITEM) == true)
end)
-- #endregion

-- #region [Child Describe] "keys / values / entries"
it("returns array of elements", function()
	local myMap = Map.new()
	myMap:set(AN_ITEM, "foo")
	myMap:set(ANOTHER_ITEM, "val")

	assert(myMap:keys()[1] == AN_ITEM)
	assert(myMap:keys()[2] == ANOTHER_ITEM)

	assert(myMap:values()[1] == "foo")
	assert(myMap:values()[2] == "val")

	assert(myMap:entries()[1][1] == AN_ITEM)
	assert(myMap:entries()[1][2] == "foo")
	assert(myMap:entries()[2][1] == ANOTHER_ITEM)
	assert(myMap:entries()[2][2] == "val")
end)
-- #endregion

-- #region [Child Describe] "__index"
it("can access fields directly without using get", function()
	local typeName = "size"

	local foo = Map.new({
		{ AN_ITEM, "foo" },
		{ ANOTHER_ITEM, "val" },
		{ typeName, "buzz" },
	})

	assert(foo.size == 3)
	assert(foo[AN_ITEM] == "foo")
	assert(foo[ANOTHER_ITEM] == "val")
	assert(foo:get(typeName) == "buzz")
end)
-- #endregion

-- #region [Child Describe] "__newindex"
it("can set fields directly without using set", function()
	local foo = Map.new()

	assert(foo.size == 0)

	foo[AN_ITEM] = "foo"
	foo[ANOTHER_ITEM] = "val"
	foo.fizz = "buzz"

	assert(foo.size == 3)
	assert(foo:get(AN_ITEM) == "foo")
	assert(foo:get(ANOTHER_ITEM) == "val")
	assert(foo:get("fizz") == "buzz")
end)
-- #endregion

-- #region [Child Describe] "ipairs"
local function makeArray(...)
	local array = {}
	for _, item in ... do
		table.insert(array, item)
	end
	return array
end

it("iterates on the elements by their insertion order", function()
	local foo = Map.new()
	foo:set(AN_ITEM, "foo")
	foo:set(ANOTHER_ITEM, "val")
	assert(makeArray(foo:ipairs())[1][1] == AN_ITEM)
	assert(makeArray(foo:ipairs())[1][2] == "foo")
	assert(makeArray(foo:ipairs())[2][1] == ANOTHER_ITEM)
	assert(makeArray(foo:ipairs())[2][2] == "val")
end)

it("does not iterate on removed elements", function()
	local foo = Map.new()
	foo:set(AN_ITEM, "foo")
	foo:set(ANOTHER_ITEM, "val")
	foo:delete(AN_ITEM)
	assert(makeArray(foo:ipairs())[1][1] == ANOTHER_ITEM)
	assert(makeArray(foo:ipairs())[1][2] == "val")
end)

it("iterates on elements if the added back to the Map", function()
	local foo = Map.new()
	foo:set(AN_ITEM, "foo")
	foo:set(ANOTHER_ITEM, "val")
	foo:delete(AN_ITEM)
	foo:set(AN_ITEM, "food")
	assert(makeArray(foo:ipairs())[1][1] == ANOTHER_ITEM)
	assert(makeArray(foo:ipairs())[1][2] == "val")
	assert(makeArray(foo:ipairs())[2][1] == AN_ITEM)
	assert(makeArray(foo:ipairs())[2][2] == "food")
end)
-- #endregion

-- #region [Child Describe] "Integration Tests"
-- it("MDN Examples", function()
-- 	local myMap = Map.new() :: Map<string | Object | Function, string>

-- 	local keyString = "a string"
-- 	local keyObj = {}
-- 	local keyFunc = function() end

-- 	-- setting the values
-- 	myMap:set(keyString, "value associated with 'a string'")
-- 	myMap:set(keyObj, "value associated with keyObj")
-- 	myMap:set(keyFunc, "value associated with keyFunc")

-- 	assert(myMap.size == 3)

-- 	-- getting the values
-- 	assert(myMap:get(keyString) == "value associated with 'a string'")
-- 	assert(myMap:get(keyObj) == "value associated with keyObj")
-- 	assert(myMap:get(keyFunc) == "value associated with keyFunc")

-- 	assert(myMap:get("a string") == "value associated with 'a string'")

-- 	assert(myMap:get({}) == nil) -- nil, because keyObj !== {}
-- 	assert(myMap:get(function() -- nil because keyFunc !== function () {}
-- 	end) == nil)
-- end)

it("handles non-traditional keys", function()
	local myMap = Map.new() :: Map<boolean | number | string, string>

	local falseKey = false
	local trueKey = true
	local negativeKey = -1
	local emptyKey = ""

	myMap:set(falseKey, "apple")
	myMap:set(trueKey, "bear")
	myMap:set(negativeKey, "corgi")
	myMap:set(emptyKey, "doge")

	assert(myMap.size == 4)

	assert(myMap:get(falseKey) == "apple")
	assert(myMap:get(trueKey) == "bear")
	assert(myMap:get(negativeKey) == "corgi")
	assert(myMap:get(emptyKey) == "doge")

	myMap:delete(falseKey)
	myMap:delete(trueKey)
	myMap:delete(negativeKey)
	myMap:delete(emptyKey)

	assert(myMap.size == 0)
end)
-- #endregion

-- #endregion [Describe] "Map"

-- #region [Describe] "coerceToMap"
it("returns the same object if instance of Map", function()
	local map = Map.new()
	assert(coerceToMap(map) == map)

	map = Map.new({})
	assert(coerceToMap(map) == map)

	map = Map.new({ { AN_ITEM, "foo" } })
	assert(coerceToMap(map) == map)
end)
-- #endregion [Describe] "coerceToMap"

-- #endregion Tests to verify it works as expected
