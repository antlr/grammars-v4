-- Taken from https://raw.githubusercontent.com/Kampfkarren/Roblox/master/Modules/LineOfSight.lua
local ReplicatedStorage = game:GetService("ReplicatedStorage")
local RunService = game:GetService("RunService")

local Raycast = require(ReplicatedStorage.Modules.Raycast)

local DEBUG = true
DEBUG = DEBUG and RunService:IsStudio()

local debug

if DEBUG then
	function debug(...)
		print("[LineOfSight]", ...)
	end
else
	function debug()
	end
end

return function(origin, character, range, ignoreIf, blacklist)
	if typeof(origin) == "Instance" then
		if origin.Position:FuzzyEq(character.PrimaryPart.Position) then
			debug("ORIGIN WAS CHARACTER")
			return origin, origin.Position
		end

		origin = origin.Position
	end

	blacklist = blacklist or {}

	local hit, point do
		while true do
			hit, point = Raycast(Ray.new(origin, (origin - character.PrimaryPart.Position).Unit * -range), blacklist)

			if hit and hit:IsDescendantOf(character) then
				break
			elseif hit and ignoreIf(hit) then
				debug("IGNORING OFF IF", hit:GetFullName())
				blacklist[#blacklist + 1] = hit
			else
				break
			end
		end
	end

	debug("LOS RESULT", hit and hit:GetFullName())

	return hit and hit:IsDescendantOf(character), point
end
