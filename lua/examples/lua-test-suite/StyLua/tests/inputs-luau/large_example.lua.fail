--!strict

type Array<T> = { [number]: T }
type Dictionary<T> = { [string]: T }

local RunService = game:GetService("RunService")

local INVALID_DUMP_VERSION = "API dump is an invalid version `%i` (expected version 1)"
local MODULE_NOT_READY_MESSAGE = "API has not been fetched yet; try using API.isReady() before calling API functions"
local CLASS_NOT_REAL_MESSAGE = "Class `%s` is not a valid Roblox class"
local API_REQUEST_FAILED_MESSAGE = "Could not get API dump: `%s`. Retrying in %i seconds."

local ApiTypes = require(script.ApiTypes)
local FetchApi = require(script.FetchApi)
local Filters = require(script.Filters)
local Util = require(script.Util)

local ReadyBindable = Instance.new("BindableEvent")

local classMap: Dictionary<ApiTypes.Class> = {}
local superClassMap: Dictionary<Array<ApiTypes.Class>> = {}

local dump: ApiTypes.API

local filterSecurity = Util.filterSecurity
local filterTags = Util.filterTags
local lookupify = Util.lookupify
local cloneMember = Util.cloneMember


local function tryAPI(): ()
	if dump then return end
	
	dump = FetchApi()
	
	if dump.Version ~= 1 then
		error(string.format(INVALID_DUMP_VERSION, dump.Version), 2)
	end
	
	for _, class in ipairs(dump.Classes) do
		classMap[class.Name] = class
	end
	
	for className in pairs(classMap) do
		local classTables = {}
		local root = className
		while classMap[root] do
			table.insert(classTables, 1, classMap[root])
			root = classMap[root].Superclass
		end
		superClassMap[className] = classTables
	end
end

local API = {}

API.readyEvent = ReadyBindable.Event
API.filters = Filters

function API.isReady()
	return not not dump
end

function API.getMembers(class: string, tagFilter: Array<string>?, securityFilter: Array<string>?): Dictionary<ApiTypes.Member>
	if not dump then
		error(MODULE_NOT_READY_MESSAGE, 2)
	end
	
	local superClasses: Array<ApiTypes.Class> = superClassMap[class]
	if not superClasses then
		error(string.format(CLASS_NOT_REAL_MESSAGE, class), 2)
	end
	
	local tagLookup: Dictionary<boolean> = lookupify(tagFilter)
	local securityLookup: Dictionary<boolean> = lookupify(securityFilter)
	
	local memberList: Dictionary<ApiTypes.Member> = {}
	for _, class in ipairs(superClasses) do
		for _, v in ipairs(class.Members) do
			if filterSecurity(v.Security, securityLookup) then continue end
			if filterTags(v.Tags, tagLookup) then continue end
			
			memberList[v.Name] = cloneMember(v)
		end
	end
	
	return memberList
end

function API.getProperties(class: string, tagFilter: Array<string>?, securityFilter: Array<string>?): Dictionary<ApiTypes.Property>
	if not dump then
		error(MODULE_NOT_READY_MESSAGE, 2)
	end
	
	local superClasses: Array<ApiTypes.Class> = superClassMap[class]
	if not superClasses then
		error(string.format(CLASS_NOT_REAL_MESSAGE, class), 2)
	end
	
	local tagLookup: Dictionary<boolean> = lookupify(tagFilter)
	local securityLookup: Dictionary<boolean> = lookupify(securityFilter)
	
	local memberList: Dictionary<ApiTypes.Property> = {}
	for _, class in ipairs(superClasses) do
		for _, v in ipairs(class.Members) do
			if v.MemberType ~= "Property" then continue end
			if filterSecurity(v.Security, securityLookup) then continue end
			if filterTags(v.Tags, tagLookup) then continue end
			
			memberList[v.Name] = cloneMember(v)
		end
	end
	
	return memberList
end


function API.getFunctions(class: string, tagFilter: Array<string>?, securityFilter: Array<string>?): Dictionary<ApiTypes.Function>
	if not dump then
		error(MODULE_NOT_READY_MESSAGE, 2)
	end
	
	local superClasses: Array<ApiTypes.Class> = superClassMap[class]
	if not superClasses then
		error(string.format(CLASS_NOT_REAL_MESSAGE, class), 2)
	end
	
	local tagLookup: Dictionary<boolean> = lookupify(tagFilter)
	local securityLookup: Dictionary<boolean> = lookupify(securityFilter)
	
	local memberList: Dictionary<ApiTypes.Function> = {}
	for _, class in ipairs(superClasses) do
		for _, v in ipairs(class.Members) do
			if v.MemberType ~= "Function" then continue end
			if filterSecurity(v.Security, securityLookup) then continue end
			if filterTags(v.Tags, tagLookup) then continue end
			
			memberList[v.Name] = cloneMember(v)
		end
	end
	
	return memberList
end

function API.getEvents(class: string, tagFilter: Array<string>?, securityFilter: Array<string>?): Dictionary<ApiTypes.Event>
	if not dump then
		error(MODULE_NOT_READY_MESSAGE, 2)
	end
	
	local superClasses: Array<ApiTypes.Class> = superClassMap[class]
	if not superClasses then
		error(string.format(CLASS_NOT_REAL_MESSAGE, class), 2)
	end
	
	local tagLookup: Dictionary<boolean> = lookupify(tagFilter)
	local securityLookup: Dictionary<boolean> = lookupify(securityFilter)
	
	local memberList: Dictionary<ApiTypes.Event> = {}
	for _, class in ipairs(superClasses) do
		for _, v in ipairs(class.Members) do
			if v.MemberType ~= "Event" then continue end
			if filterSecurity(v.Security, securityLookup) then continue end
			if filterTags(v.Tags, tagLookup) then continue end
			
			memberList[v.Name] = cloneMember(v)
		end
	end
	
	return memberList
end


function API.getCallbacks(class: string, tagFilter: Array<string>?, securityFilter: Array<string>?): Dictionary<ApiTypes.Callback>
	if not dump then
		error(MODULE_NOT_READY_MESSAGE, 2)
	end
	
	local superClasses: Array<ApiTypes.Class> = superClassMap[class]
	if not superClasses then
		error(string.format(CLASS_NOT_REAL_MESSAGE, class), 2)
	end
	
	local tagLookup: Dictionary<boolean> = lookupify(tagFilter)
	local securityLookup: Dictionary<boolean> = lookupify(securityFilter)
	
	local memberList: Dictionary<ApiTypes.Callback> = {}
	for _, class in ipairs(superClasses) do
		for _, v in ipairs(class.Members) do
			if v.MemberType ~= "Callback" then continue end
			if filterSecurity(v.Security, securityLookup) then continue end
			if filterTags(v.Tags, tagLookup) then continue end
			
			memberList[v.Name] = cloneMember(v)
		end
	end
	
	return memberList
end

function API.getSuperclasses(class: string): Array<string>
	if not dump then
		error(MODULE_NOT_READY_MESSAGE, 2)
	end
	
	local superClasses: Array<ApiTypes.Class> = superClassMap[class]
	if not superClasses then
		error(string.format(CLASS_NOT_REAL_MESSAGE, class), 2)
	end
	
	local list = {}
	for i, class in ipairs(superClasses) do
		list[i] = class.Name
	end
	
	return list
end

function API.isDeprecated(class: string, member: string?): boolean
	if not dump then
		error(MODULE_NOT_READY_MESSAGE, 2)
	end
	
	local classTable: ApiTypes.Class = classMap[class]
	if not classTable then
		error(string.format(CLASS_NOT_REAL_MESSAGE, class), 2)
	end
	
	if member then
		local members = API.getMembers(class, {"Deprecated"})
		if members[member] then
			return true
		else
			return false
		end
	else
		local tags: typeof(classTable.Tags) = classTable.Tags
		if tags then
			if table.find(tags, "Deprecated") then
				return true
			end
		end
	end
	return false
end

function API.isService(class: string): boolean
	if not dump then
		error(MODULE_NOT_READY_MESSAGE, 2)
	end
	
	local classTable: ApiTypes.Class = classMap[class]
	if not classTable then
		error(string.format(CLASS_NOT_REAL_MESSAGE, class), 2)
	end
	
	local tags: typeof(classTable.Tags) = classTable.Tags
	if tags then
		if table.find(tags, "Service") then
			return true
		end
	end
	return false
end

function API.getClasses(filter: Array<string>?): Array<string>
	if not dump then
		error(MODULE_NOT_READY_MESSAGE, 2)
	end
	local classList: Array<string> = {}
	
	local tagLookup: Dictionary<boolean> = lookupify(filter)
	
	local classCount = 1
	
	for _, v in ipairs(dump.Classes) do
		if filterTags(v.Tags, tagLookup) then continue end
		
		classList[classCount] = v.Name
		classCount += 1
	end
	
	return classList
end

function API.getEnums(filter: Array<string>?): Array<string>
	if not dump then
		error(MODULE_NOT_READY_MESSAGE, 2)
	end
	local enumList: Array<string> = {}
	
	local tagLookup: Dictionary<boolean> = lookupify(filter)
	
	local enumCount = 1
	
	for _, v in ipairs(dump.Enums) do
		if filterTags(v.Tags, tagLookup) then continue end
		
		enumList[enumCount] = v.Name
		enumCount += 1
	end
	
	return enumList
end

export type Member = ApiTypes.Member
export type Property = ApiTypes.Property
export type Function = ApiTypes.Function
export type Event = ApiTypes.Event
export type Callback = ApiTypes.Callback

return API
