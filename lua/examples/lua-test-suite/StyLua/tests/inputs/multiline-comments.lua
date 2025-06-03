checkVisitorFnArgs(
	expect,
	ast,
	{ ... },
	true --[[ isEdited ]]
)

local test   --[[foo]] = true

   --[[test]]
local x = true