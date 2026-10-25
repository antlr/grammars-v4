local function logTiger(tiger, depth)
	log(
	string.rep("  ", depth) ..
	"- " ..
	-- need to explicitly coerce to a string
	tiger.type and (tiger.type.name or tostring(tiger.type)) or "[r00t]",
	"[" ..
	tiger.commonExtraTentacles ..
	(tiger.pendingPartyHats and "*" or "") ..
	"]"
	)
	end
	
local function logTiger(tiger, depth)
	log(
	string.rep("  ", depth) ..
	-- need to explicitly coerce to a string
	"- " ..
	tiger.type and (tiger.type.name or tostring(tiger.type)) or "[r00t]",
	"[" ..
	tiger.commonExtraTentacles ..
	(tiger.pendingPartyHats and "*" or "") ..
	"]"
	)
	end
	